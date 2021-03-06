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
package com.waz.zclient.connect

import android.content.Context
import android.graphics.Color
import android.os.Bundle
import android.view.animation.Animation
import android.view.{LayoutInflater, View, ViewGroup}
import android.widget.ImageView
import com.waz.ZLog.ImplicitTag._
import com.waz.model.UserId
import com.waz.service.ZMessaging
import com.waz.threading.Threading
import com.waz.utils.events.Signal
import com.waz.utils.returning
import com.waz.zclient.common.controllers.UserAccountsController
import com.waz.zclient.common.controllers.global.{AccentColorController, KeyboardController}
import com.waz.zclient.common.views.ImageAssetDrawable
import com.waz.zclient.common.views.ImageAssetDrawable.{RequestBuilder, ScaleType}
import com.waz.zclient.common.views.ImageController.WireImage
import com.waz.zclient.connect.PendingConnectRequestFragment.ArgUserRequester
import com.waz.zclient.conversation.ConversationController
import com.waz.zclient.core.stores.connect.IConnectStore
import com.waz.zclient.messages.UsersController
import com.waz.zclient.pages.BaseFragment
import com.waz.zclient.pages.main.connect.UserProfileContainer
import com.waz.zclient.pages.main.participants.ProfileAnimation
import com.waz.zclient.ui.text.TypefaceTextView
import com.waz.zclient.ui.views.ZetaButton
import com.waz.zclient.utils.ContextUtils._
import com.waz.zclient.utils.{StringUtils, ViewUtils}
import com.waz.zclient.views.menus.{FooterMenu, FooterMenuCallback}
import com.waz.zclient.{FragmentHelper, R}

class SendConnectRequestFragment extends BaseFragment[SendConnectRequestFragment.Container]
  with FragmentHelper {

  import SendConnectRequestFragment._
  import Threading.Implicits.Ui

  implicit def context: Context = getActivity

  private lazy val userToConnectId = UserId(getArguments.getString(ArgumentUserId))
  private lazy val userRequester = IConnectStore.UserRequester.valueOf(getArguments.getString(ArgumentUserRequester))

  private lazy val usersController = inject[UsersController]
  private lazy val userAccountsController = inject[UserAccountsController]
  private lazy val conversationController = inject[ConversationController]
  private lazy val keyboardController = inject[KeyboardController]
  private lazy val accentColorController = inject[AccentColorController]
  private lazy val zms = inject[Signal[ZMessaging]]

  private lazy val user = usersController.user(userToConnectId)

  private lazy val removeConvMemberFeatureEnabled = for {
    convId <- conversationController.currentConvId
    permission <- userAccountsController.hasRemoveConversationMemberPermission(convId)
  } yield permission && userRequester == IConnectStore.UserRequester.PARTICIPANTS

  private lazy val connectButton = returning(view[ZetaButton](R.id.zb__send_connect_request__connect_button)) { vh =>
    accentColorController.accentColor.onUi { color => vh.foreach(_.setAccentColor(color.getColor)) }
    vh.onClick { _ =>
      usersController.connectToUser(userToConnectId).foreach(_.foreach { _ =>
        keyboardController.hideKeyboardIfVisible()
        getContainer.onConnectRequestWasSentToUser()
      })
    }
  }
  private lazy val footerMenu = returning(view[FooterMenu](R.id.fm__footer)) { vh =>
    removeConvMemberFeatureEnabled.map {
      case true => getString(R.string.glyph__minus)
      case false => ""
    }.onUi(text => vh.foreach(_.setRightActionText(text)))
  }
  private lazy val imageViewProfile = view[ImageView](R.id.send_connect)
  private lazy val userNameView = returning(view[TypefaceTextView](R.id.user_name)) { vh =>
    user.map(_.getDisplayName).onUi(t => vh.foreach(_.setText(t)))
  }
  private lazy val userHandleView = returning(view[TypefaceTextView](R.id.user_handle)) { vh =>
    user.map(user => StringUtils.formatHandle(user.handle.map(_.string).getOrElse("")))
      .onUi(t => vh.foreach(_.setText(t)))
  }

  override def onCreateAnimation(transit: Int, enter: Boolean, nextAnim: Int): Animation = {
    def defaultAnimation = super.onCreateAnimation(transit, enter, nextAnim)
    def isConvRequester = {
      val userRequester = IConnectStore.UserRequester.valueOf(getArguments.getString(ArgUserRequester))
      userRequester == IConnectStore.UserRequester.CONVERSATION
    }

    if (isConvRequester || nextAnim != 0) defaultAnimation
    else {
      val centerX = getOrientationIndependentDisplayWidth(getActivity) / 2
      val centerY = getOrientationIndependentDisplayHeight(getActivity) / 2
      val duration =
        if (enter) getInt(R.integer.open_profile__animation_duration)
        else getInt(R.integer.close_profile__animation_duration)
      val delay =
        if (enter) getInt(R.integer.open_profile__delay)
        else 0

      new ProfileAnimation(enter, duration, delay, centerX, centerY)
    }
  }

  override def onCreateView(inflater: LayoutInflater, viewContainer: ViewGroup, savedInstanceState: Bundle): View =
    inflater.inflate(R.layout.fragment_send_connect_request, viewContainer, false)

  override def onViewCreated(view: View, savedInstanceState: Bundle): Unit = {
    userNameView
    userHandleView

    val assetDrawable = new ImageAssetDrawable(
      user.map(_.picture).collect { case Some(p) => WireImage(p) },
      scaleType = ScaleType.CenterInside,
      request = RequestBuilder.Round
    )
    imageViewProfile.foreach(_.setImageDrawable(assetDrawable))

    val backgroundContainer = findById[View](R.id.background_container)
    backgroundContainer.setClickable(true)

    if (userRequester == IConnectStore.UserRequester.PARTICIPANTS) {
      backgroundContainer.setBackgroundColor(Color.TRANSPARENT)
      footerMenu.setVisibility(View.VISIBLE)
      connectButton.setVisibility(View.GONE)
    } else {
      footerMenu.setVisibility(View.GONE)
      connectButton.setVisibility(View.VISIBLE)
    }

    footerMenu.foreach(_.setCallback(new FooterMenuCallback {
      override def onLeftActionClicked(): Unit = showConnectButtonInsteadOfFooterMenu()
      override def onRightActionClicked(): Unit = removeConvMemberFeatureEnabled.head foreach {
        case true => getContainer.showRemoveConfirmation(userToConnectId)
        case _ =>
      }
    }))
  }

  override def onStop(): Unit = {
    keyboardController.hideKeyboardIfVisible()
    super.onStop()
  }

  private def showConnectButtonInsteadOfFooterMenu(): Unit = {
    if (connectButton.getVisibility != View.VISIBLE) {
      footerMenu.foreach(_.setVisibility(View.GONE))
      connectButton.foreach { btn =>
        btn.setAlpha(0)
        btn.setVisibility(View.VISIBLE)
        ViewUtils.fadeInView(btn, getInt(R.integer.framework_animation_duration_long))
      }
    }
  }
}

object SendConnectRequestFragment {
  val Tag: String = classOf[SendConnectRequestFragment].getName
  val ArgumentUserId = "ARGUMENT_USER_ID"
  val ArgumentUserRequester = "ARGUMENT_USER_REQUESTER"

  def newInstance(userId: String, userRequester: IConnectStore.UserRequester): SendConnectRequestFragment =
    returning(new SendConnectRequestFragment)(fragment =>
      fragment.setArguments(returning(new Bundle) { args =>
        args.putString(ArgumentUserId, userId)
        args.putString(ArgumentUserRequester, userRequester.toString)
      })
    )

  trait Container extends UserProfileContainer {
    def onConnectRequestWasSentToUser(): Unit
    override def showRemoveConfirmation(userId: UserId): Unit
  }

}
