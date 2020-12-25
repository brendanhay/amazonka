{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    sintIdentity,
    sintNotificationType,
    sintSnsTopic,

    -- * Destructuring the response
    SetIdentityNotificationTopicResponse (..),
    mkSetIdentityNotificationTopicResponse,

    -- ** Response lenses
    sintrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to specify the Amazon SNS topic to which Amazon SES will publish bounce, complaint, or delivery notifications for emails sent with that identity as the Source. For information about Amazon SES notifications, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications-via-sns.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkSetIdentityNotificationTopic' smart constructor.
data SetIdentityNotificationTopic = SetIdentityNotificationTopic'
  { -- | The identity (email address or domain) that you want to set the Amazon SNS topic for.
    --
    -- /Important:/ You can only specify a verified identity for this parameter.
    -- You can specify an identity by using its name or by using its Amazon Resource Name (ARN). The following examples are all valid identities: @sender@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
    identity :: Types.Identity,
    -- | The type of notifications that will be published to the specified Amazon SNS topic.
    notificationType :: Types.NotificationType,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic. If the parameter is omitted from the request or a null value is passed, @SnsTopic@ is cleared and publishing is disabled.
    snsTopic :: Core.Maybe Types.SnsTopic
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetIdentityNotificationTopic' value with any optional fields omitted.
mkSetIdentityNotificationTopic ::
  -- | 'identity'
  Types.Identity ->
  -- | 'notificationType'
  Types.NotificationType ->
  SetIdentityNotificationTopic
mkSetIdentityNotificationTopic identity notificationType =
  SetIdentityNotificationTopic'
    { identity,
      notificationType,
      snsTopic = Core.Nothing
    }

-- | The identity (email address or domain) that you want to set the Amazon SNS topic for.
--
-- /Important:/ You can only specify a verified identity for this parameter.
-- You can specify an identity by using its name or by using its Amazon Resource Name (ARN). The following examples are all valid identities: @sender@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sintIdentity :: Lens.Lens' SetIdentityNotificationTopic Types.Identity
sintIdentity = Lens.field @"identity"
{-# DEPRECATED sintIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

-- | The type of notifications that will be published to the specified Amazon SNS topic.
--
-- /Note:/ Consider using 'notificationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sintNotificationType :: Lens.Lens' SetIdentityNotificationTopic Types.NotificationType
sintNotificationType = Lens.field @"notificationType"
{-# DEPRECATED sintNotificationType "Use generic-lens or generic-optics with 'notificationType' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic. If the parameter is omitted from the request or a null value is passed, @SnsTopic@ is cleared and publishing is disabled.
--
-- /Note:/ Consider using 'snsTopic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sintSnsTopic :: Lens.Lens' SetIdentityNotificationTopic (Core.Maybe Types.SnsTopic)
sintSnsTopic = Lens.field @"snsTopic"
{-# DEPRECATED sintSnsTopic "Use generic-lens or generic-optics with 'snsTopic' instead." #-}

instance Core.AWSRequest SetIdentityNotificationTopic where
  type
    Rs SetIdentityNotificationTopic =
      SetIdentityNotificationTopicResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "SetIdentityNotificationTopic")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "Identity" identity)
                Core.<> (Core.toQueryValue "NotificationType" notificationType)
                Core.<> (Core.toQueryValue "SnsTopic" Core.<$> snsTopic)
            )
      }
  response =
    Response.receiveXMLWrapper
      "SetIdentityNotificationTopicResult"
      ( \s h x ->
          SetIdentityNotificationTopicResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkSetIdentityNotificationTopicResponse' smart constructor.
newtype SetIdentityNotificationTopicResponse = SetIdentityNotificationTopicResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetIdentityNotificationTopicResponse' value with any optional fields omitted.
mkSetIdentityNotificationTopicResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SetIdentityNotificationTopicResponse
mkSetIdentityNotificationTopicResponse responseStatus =
  SetIdentityNotificationTopicResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sintrrsResponseStatus :: Lens.Lens' SetIdentityNotificationTopicResponse Core.Int
sintrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sintrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
