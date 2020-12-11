{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.SetIdentityHeadersInNotificationsEnabled
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Given an identity (an email address or a domain), sets whether Amazon SES includes the original email headers in the Amazon Simple Notification Service (Amazon SNS) notifications of a specified type.
--
-- You can execute this operation no more than once per second.
-- For more information about using notifications with Amazon SES, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html Amazon SES Developer Guide> .
module Network.AWS.SES.SetIdentityHeadersInNotificationsEnabled
  ( -- * Creating a request
    SetIdentityHeadersInNotificationsEnabled (..),
    mkSetIdentityHeadersInNotificationsEnabled,

    -- ** Request lenses
    sihineIdentity,
    sihineNotificationType,
    sihineEnabled,

    -- * Destructuring the response
    SetIdentityHeadersInNotificationsEnabledResponse (..),
    mkSetIdentityHeadersInNotificationsEnabledResponse,

    -- ** Response lenses
    sihinersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to set whether Amazon SES includes the original email headers in the Amazon SNS notifications of a specified type. For information about notifications, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications-via-sns.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkSetIdentityHeadersInNotificationsEnabled' smart constructor.
data SetIdentityHeadersInNotificationsEnabled = SetIdentityHeadersInNotificationsEnabled'
  { identity ::
      Lude.Text,
    notificationType ::
      NotificationType,
    enabled ::
      Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetIdentityHeadersInNotificationsEnabled' with the minimum fields required to make a request.
--
-- * 'enabled' - Sets whether Amazon SES includes the original email headers in Amazon SNS notifications of the specified notification type. A value of @true@ specifies that Amazon SES will include headers in notifications, and a value of @false@ specifies that Amazon SES will not include headers in notifications.
--
-- This value can only be set when @NotificationType@ is already set to use a particular Amazon SNS topic.
-- * 'identity' - The identity for which to enable or disable headers in notifications. Examples: @user@example.com@ , @example.com@ .
-- * 'notificationType' - The notification type for which to enable or disable headers in notifications.
mkSetIdentityHeadersInNotificationsEnabled ::
  -- | 'identity'
  Lude.Text ->
  -- | 'notificationType'
  NotificationType ->
  -- | 'enabled'
  Lude.Bool ->
  SetIdentityHeadersInNotificationsEnabled
mkSetIdentityHeadersInNotificationsEnabled
  pIdentity_
  pNotificationType_
  pEnabled_ =
    SetIdentityHeadersInNotificationsEnabled'
      { identity = pIdentity_,
        notificationType = pNotificationType_,
        enabled = pEnabled_
      }

-- | The identity for which to enable or disable headers in notifications. Examples: @user@example.com@ , @example.com@ .
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihineIdentity :: Lens.Lens' SetIdentityHeadersInNotificationsEnabled Lude.Text
sihineIdentity = Lens.lens (identity :: SetIdentityHeadersInNotificationsEnabled -> Lude.Text) (\s a -> s {identity = a} :: SetIdentityHeadersInNotificationsEnabled)
{-# DEPRECATED sihineIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

-- | The notification type for which to enable or disable headers in notifications.
--
-- /Note:/ Consider using 'notificationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihineNotificationType :: Lens.Lens' SetIdentityHeadersInNotificationsEnabled NotificationType
sihineNotificationType = Lens.lens (notificationType :: SetIdentityHeadersInNotificationsEnabled -> NotificationType) (\s a -> s {notificationType = a} :: SetIdentityHeadersInNotificationsEnabled)
{-# DEPRECATED sihineNotificationType "Use generic-lens or generic-optics with 'notificationType' instead." #-}

-- | Sets whether Amazon SES includes the original email headers in Amazon SNS notifications of the specified notification type. A value of @true@ specifies that Amazon SES will include headers in notifications, and a value of @false@ specifies that Amazon SES will not include headers in notifications.
--
-- This value can only be set when @NotificationType@ is already set to use a particular Amazon SNS topic.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihineEnabled :: Lens.Lens' SetIdentityHeadersInNotificationsEnabled Lude.Bool
sihineEnabled = Lens.lens (enabled :: SetIdentityHeadersInNotificationsEnabled -> Lude.Bool) (\s a -> s {enabled = a} :: SetIdentityHeadersInNotificationsEnabled)
{-# DEPRECATED sihineEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.AWSRequest SetIdentityHeadersInNotificationsEnabled where
  type
    Rs SetIdentityHeadersInNotificationsEnabled =
      SetIdentityHeadersInNotificationsEnabledResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "SetIdentityHeadersInNotificationsEnabledResult"
      ( \s h x ->
          SetIdentityHeadersInNotificationsEnabledResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetIdentityHeadersInNotificationsEnabled where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetIdentityHeadersInNotificationsEnabled where
  toPath = Lude.const "/"

instance Lude.ToQuery SetIdentityHeadersInNotificationsEnabled where
  toQuery SetIdentityHeadersInNotificationsEnabled' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("SetIdentityHeadersInNotificationsEnabled" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "Identity" Lude.=: identity,
        "NotificationType" Lude.=: notificationType,
        "Enabled" Lude.=: enabled
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkSetIdentityHeadersInNotificationsEnabledResponse' smart constructor.
newtype SetIdentityHeadersInNotificationsEnabledResponse = SetIdentityHeadersInNotificationsEnabledResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'SetIdentityHeadersInNotificationsEnabledResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkSetIdentityHeadersInNotificationsEnabledResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetIdentityHeadersInNotificationsEnabledResponse
mkSetIdentityHeadersInNotificationsEnabledResponse pResponseStatus_ =
  SetIdentityHeadersInNotificationsEnabledResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihinersResponseStatus :: Lens.Lens' SetIdentityHeadersInNotificationsEnabledResponse Lude.Int
sihinersResponseStatus = Lens.lens (responseStatus :: SetIdentityHeadersInNotificationsEnabledResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetIdentityHeadersInNotificationsEnabledResponse)
{-# DEPRECATED sihinersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
