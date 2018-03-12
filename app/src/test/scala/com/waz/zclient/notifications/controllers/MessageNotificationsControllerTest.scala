/**
 * Wire
 * Copyright (C) 2018 Wire Swiss GmbH
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.waz.zclient.notifications.controllers

import java.util.concurrent.TimeUnit

import android.app.Notification
import com.waz.api.NotificationsHandler.NotificationType
import com.waz.api.NotificationsHandler.NotificationType.CONNECT_REQUEST
import com.waz.content._
import com.waz.service.images.ImageLoader
import com.waz.utils.events.SourceSignal

import scala.concurrent.Await
import com.waz.model._
import com.waz.service.conversation.{ConversationsListStateService, ConversationsService, ConversationsUiService}
import com.waz.service.push.NotificationService.NotificationInfo
import com.waz.service.push.{GlobalNotificationsService, NotificationService}
import com.waz.service.{AccountsService, UiLifeCycle, UserService}
import com.waz.utils.events.Signal
import com.waz.zclient.Module
import com.waz.zclient.common.controllers.SoundController
import com.waz.zclient.common.controllers.global.AccentColorController
import com.waz.zclient.controllers.navigation.INavigationController
import com.waz.zclient.conversation.ConversationController
import com.waz.zclient.messages.controllers.NavigationController
import com.waz.{NotificationManagerWrapper, RobolectricUtils, utils}
import junit.framework.Assert._
import org.junit.runner.RunWith
import org.junit.{Before, Test}
import org.mockito.Mockito._
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import org.scalatest.junit.JUnitSuite
import org.scalatest.{Informer, Informing}
import org.threeten.bp.Instant

import scala.collection.immutable.SortedSet
import scala.concurrent.Future
import scala.concurrent.duration.Duration

@RunWith(classOf[RobolectricTestRunner])
@Config(manifest = "app/src/test/AndroidManifest.xml", resourceDir = "../../build/intermediates/res/merged/dev/debug")
class MessageNotificationsControllerTest extends JUnitSuite with RobolectricUtils with Informing {

  import com.waz.utils.events.EventContext.Implicits.global
  import com.waz.ZLog.ImplicitTag.implicitLogTag

  implicit val timeout = Duration(5000, TimeUnit.MILLISECONDS)

  private val accountId = AccountId()
  private val userId = UserId()
  private val convId = ConvId(userId.str)

  private val displayedNots = new SourceSignal[Map[AccountId, Seq[NotId]]]()

  private val globalNotifications = new GlobalNotificationsService {
    override val groupedNotifications = Signal(Map.empty[AccountId, (Boolean, Seq[NotificationService.NotificationInfo])])

    override def markAsDisplayed(accountId: AccountId, nots: Seq[NotId]): Future[Any] =
      Future.successful(displayedNots.mutate { cur =>
        cur + (accountId -> (cur.getOrElse(accountId, Seq.empty).toSet ++ nots).toSeq)
      })

  }

  // a utility method for sending infos from tests
  def send(info: NotificationInfo, accId: AccountId = accountId) = {
    globalNotifications.groupedNotifications ! Map(accId -> (false, Seq(info)))
  }

  private val notsInManager = new SourceSignal[Map[Int, Notification]]()

  private val notificationManager = new NotificationManagerWrapper {

    override def cancel(id: Int): Unit = {
      notsInManager.mutate { cur => cur.filterKeys(_ != id) }
    }

    override def getActiveNotificationIds: Seq[Int] = {
      notsInManager.currentValue.getOrElse(Map.empty).keys.toSeq
    }

    override def notify(id: Int, notification: Notification): Unit = {
      notsInManager ! Map(id -> notification)
    }
  }

  private lazy val accountsService = mock(classOf[AccountsService])
  private lazy val accountsStorage = mock(classOf[AccountsStorage])
  private lazy val uiLifeCycle     = mock(classOf[UiLifeCycle])
  private lazy val usersStorage    = mock(classOf[UsersStorage])
  private lazy val convStorage     = mock(classOf[ConversationStorage])
  private lazy val convController  = mock(classOf[ConversationController])
  private lazy val assetsStorage   = mock(classOf[AssetsStorage])
  private lazy val imageLoader     = mock(classOf[ImageLoader])
  private lazy val soundController = mock(classOf[SoundController])

  private implicit lazy val module: Module = new Module {
    // `MessageNotificationController` receives `NotificationInfo`s from here
    bind[GlobalNotificationsService] to globalNotifications
    // processed notifications end up here
    bind[NotificationManagerWrapper] to notificationManager

    // mocked global entities
    bind[AccountsStorage] to accountsStorage
    bind[TeamsStorage]    to mock(classOf[TeamsStorage])
    bind[AccountsService] to accountsService
    bind[UiLifeCycle]     to uiLifeCycle

    // mocked services of the current ZMessaging
    bind[Signal[AccountId]]               to Signal.const(accountId)
    bind[Signal[Option[AccountId]]]       to Signal.const(Some(accountId))
    bind[Signal[com.waz.api.AccentColor]] to Signal.const(new com.waz.api.AccentColor { override def getColor: Int = 0 })
    bind[Signal[UsersStorage]]                  to Signal.const(usersStorage)
    bind[Signal[ConversationStorage]]           to Signal.const(convStorage)
    bind[Signal[ConversationsListStateService]] to Signal.const(mock(classOf[ConversationsListStateService]))
    bind[Signal[ConversationsUiService]]        to Signal.const(mock(classOf[ConversationsUiService]))
    bind[Signal[ConversationsService]]          to Signal.const(mock(classOf[ConversationsService]))
    bind[Signal[MembersStorage]]                to Signal.const(mock(classOf[MembersStorage]))
    bind[Signal[UserService]]                   to Signal.const(mock(classOf[UserService]))
    bind[Signal[OtrClientsStorage]]             to Signal.const(mock(classOf[OtrClientsStorage]))
    bind[Signal[AssetsStorage]]                 to Signal.const(assetsStorage)
    bind[Signal[ImageLoader]]                   to Signal.const(imageLoader)

    // mocked controllers
    bind[AccentColorController]  to new AccentColorController()
    bind[NavigationController]   to new NavigationController()
    bind[INavigationController]  to mock(classOf[INavigationController])
    bind[ConversationController] to convController
    bind[SoundController]        to soundController
  }

  @Before
  def setUp(): Unit = {
    utils.isTest = true

    module

    val accountData = AccountData(accountId, userId = Some(userId))
    when(accountsService.loggedInAccounts).thenReturn(Signal.const(Set(accountData)))
    when(usersStorage.signal(userId)).thenReturn(Signal.const(UserData(userId, "TestUser")))
    when(uiLifeCycle.uiActive).thenReturn(Signal.const(true))
    when(convController.currentConvIdOpt).thenReturn(Signal.const(Option.empty[ConvId]))
    when(convStorage.convsSignal).thenReturn(Signal(ConversationsSet(SortedSet.empty)))
    when(accountsStorage.get(accountId)).thenReturn(Future.successful(Some(accountData)))
    when(soundController.soundIntensityNone).thenReturn(true)
    when(soundController.isVibrationEnabled).thenReturn(false)
  }

  @Test
  def displayNotificationForReceivedLike(): Unit = {
    new MessageNotificationsController()
    send( NotificationInfo(NotId(Uid().str), NotificationType.LIKE, Instant.now, "", convId) )

    val results = Await.result(notsInManager.head, timeout)

    assertEquals(1, results.size)
    assertEquals(results.head._1, MessageNotificationsController.toNotificationConvId(accountId, convId))
  }

  @Test
  def receiveConnectRequest(): Unit = {
    new MessageNotificationsController()

    send(
      NotificationInfo(
        NotId(Uid().str), CONNECT_REQUEST, Instant.now(), "", convId, Some("TestUser"),
        Some("TestUser"), userPicture = None, isEphemeral = false, hasBeenDisplayed = false
      )
    )

    val results = Await.result(notsInManager.head, timeout)

    assertEquals(1, results.size)
    assertEquals(results.head._1, MessageNotificationsController.toNotificationConvId(accountId, convId))
  }

  override protected def info: Informer = new Informer {
    override def apply(message: String, payload: Option[Any]): Unit = println(message)
  }
}
