{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.SetIdentityNotificationTopic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets an Amazon Simple Notification Service (Amazon SNS) topic to use when delivering notifications. When you use this operation, you specify a verified identity, such as an email address or domain. When you send an email that uses the chosen identity in the Source field, Amazon SES sends notifications to the topic you specified. You can send bounce, complaint, or delivery notifications (or any combination of the three) to the Amazon SNS topic that you specify.
--
-- You can execute this operation no more than once per second.
-- For more information about feedback notification, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html Amazon SES Developer Guide> .
module Network.AWS.SES.SetIdentityNotificationTopic
  ( -- * Creating a request
    SetIdentityNotificationTopic (..),
    mkSetIdentityNotificationTopic,

    -- ** Request lenses
    sintSNSTopic,
    sintIdentity,
    sintNotificationType,

    -- * Destructuring the response
    SetIdentityNotificationTopicResponse (..),
    mkSetIdentityNotificationTopicResponse,

    -- ** Response lenses
    sintrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to specify the Amazon SNS topic to which Amazon SES will publish bounce, complaint, or delivery notifications for emails sent with that identity as the Source. For information about Amazon SES notifications, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications-via-sns.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkSetIdentityNotificationTopic' smart constructor.
data SetIdentityNotificationTopic = SetIdentityNotificationTopic'
  { snsTopic ::
      Lude.Maybe Lude.Text,
    identity :: Lude.Text,
    notificationType ::
      NotificationType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetIdentityNotificationTopic' with the minimum fields required to make a request.
--
-- * 'identity' - The identity (email address or domain) that you want to set the Amazon SNS topic for.
--
-- /Important:/ You can only specify a verified identity for this parameter.
-- You can specify an identity by using its name or by using its Amazon Resource Name (ARN). The following examples are all valid identities: @sender@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
-- * 'notificationType' - The type of notifications that will be published to the specified Amazon SNS topic.
-- * 'snsTopic' - The Amazon Resource Name (ARN) of the Amazon SNS topic. If the parameter is omitted from the request or a null value is passed, @SnsTopic@ is cleared and publishing is disabled.
mkSetIdentityNotificationTopic ::
  -- | 'identity'
  Lude.Text ->
  -- | 'notificationType'
  NotificationType ->
  SetIdentityNotificationTopic
mkSetIdentityNotificationTopic pIdentity_ pNotificationType_ =
  SetIdentityNotificationTopic'
    { snsTopic = Lude.Nothing,
      identity = pIdentity_,
      notificationType = pNotificationType_
    }

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic. If the parameter is omitted from the request or a null value is passed, @SnsTopic@ is cleared and publishing is disabled.
--
-- /Note:/ Consider using 'snsTopic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sintSNSTopic :: Lens.Lens' SetIdentityNotificationTopic (Lude.Maybe Lude.Text)
sintSNSTopic = Lens.lens (snsTopic :: SetIdentityNotificationTopic -> Lude.Maybe Lude.Text) (\s a -> s {snsTopic = a} :: SetIdentityNotificationTopic)
{-# DEPRECATED sintSNSTopic "Use generic-lens or generic-optics with 'snsTopic' instead." #-}

-- | The identity (email address or domain) that you want to set the Amazon SNS topic for.
--
-- /Important:/ You can only specify a verified identity for this parameter.
-- You can specify an identity by using its name or by using its Amazon Resource Name (ARN). The following examples are all valid identities: @sender@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sintIdentity :: Lens.Lens' SetIdentityNotificationTopic Lude.Text
sintIdentity = Lens.lens (identity :: SetIdentityNotificationTopic -> Lude.Text) (\s a -> s {identity = a} :: SetIdentityNotificationTopic)
{-# DEPRECATED sintIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

-- | The type of notifications that will be published to the specified Amazon SNS topic.
--
-- /Note:/ Consider using 'notificationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sintNotificationType :: Lens.Lens' SetIdentityNotificationTopic NotificationType
sintNotificationType = Lens.lens (notificationType :: SetIdentityNotificationTopic -> NotificationType) (\s a -> s {notificationType = a} :: SetIdentityNotificationTopic)
{-# DEPRECATED sintNotificationType "Use generic-lens or generic-optics with 'notificationType' instead." #-}

instance Lude.AWSRequest SetIdentityNotificationTopic where
  type
    Rs SetIdentityNotificationTopic =
      SetIdentityNotificationTopicResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "SetIdentityNotificationTopicResult"
      ( \s h x ->
          SetIdentityNotificationTopicResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetIdentityNotificationTopic where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetIdentityNotificationTopic where
  toPath = Lude.const "/"

instance Lude.ToQuery SetIdentityNotificationTopic where
  toQuery SetIdentityNotificationTopic' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("SetIdentityNotificationTopic" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "SnsTopic" Lude.=: snsTopic,
        "Identity" Lude.=: identity,
        "NotificationType" Lude.=: notificationType
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkSetIdentityNotificationTopicResponse' smart constructor.
newtype SetIdentityNotificationTopicResponse = SetIdentityNotificationTopicResponse'
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
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetIdentityNotificationTopicResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkSetIdentityNotificationTopicResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetIdentityNotificationTopicResponse
mkSetIdentityNotificationTopicResponse pResponseStatus_ =
  SetIdentityNotificationTopicResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sintrsResponseStatus :: Lens.Lens' SetIdentityNotificationTopicResponse Lude.Int
sintrsResponseStatus = Lens.lens (responseStatus :: SetIdentityNotificationTopicResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetIdentityNotificationTopicResponse)
{-# DEPRECATED sintrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
