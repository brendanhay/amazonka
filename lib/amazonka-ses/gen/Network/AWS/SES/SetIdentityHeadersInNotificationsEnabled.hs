{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    sihinerrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to set whether Amazon SES includes the original email headers in the Amazon SNS notifications of a specified type. For information about notifications, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications-via-sns.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkSetIdentityHeadersInNotificationsEnabled' smart constructor.
data SetIdentityHeadersInNotificationsEnabled = SetIdentityHeadersInNotificationsEnabled'
  { -- | The identity for which to enable or disable headers in notifications. Examples: @user@example.com@ , @example.com@ .
    identity :: Types.Identity,
    -- | The notification type for which to enable or disable headers in notifications.
    notificationType :: Types.NotificationType,
    -- | Sets whether Amazon SES includes the original email headers in Amazon SNS notifications of the specified notification type. A value of @true@ specifies that Amazon SES will include headers in notifications, and a value of @false@ specifies that Amazon SES will not include headers in notifications.
    --
    -- This value can only be set when @NotificationType@ is already set to use a particular Amazon SNS topic.
    enabled :: Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetIdentityHeadersInNotificationsEnabled' value with any optional fields omitted.
mkSetIdentityHeadersInNotificationsEnabled ::
  -- | 'identity'
  Types.Identity ->
  -- | 'notificationType'
  Types.NotificationType ->
  -- | 'enabled'
  Core.Bool ->
  SetIdentityHeadersInNotificationsEnabled
mkSetIdentityHeadersInNotificationsEnabled
  identity
  notificationType
  enabled =
    SetIdentityHeadersInNotificationsEnabled'
      { identity,
        notificationType,
        enabled
      }

-- | The identity for which to enable or disable headers in notifications. Examples: @user@example.com@ , @example.com@ .
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihineIdentity :: Lens.Lens' SetIdentityHeadersInNotificationsEnabled Types.Identity
sihineIdentity = Lens.field @"identity"
{-# DEPRECATED sihineIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

-- | The notification type for which to enable or disable headers in notifications.
--
-- /Note:/ Consider using 'notificationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihineNotificationType :: Lens.Lens' SetIdentityHeadersInNotificationsEnabled Types.NotificationType
sihineNotificationType = Lens.field @"notificationType"
{-# DEPRECATED sihineNotificationType "Use generic-lens or generic-optics with 'notificationType' instead." #-}

-- | Sets whether Amazon SES includes the original email headers in Amazon SNS notifications of the specified notification type. A value of @true@ specifies that Amazon SES will include headers in notifications, and a value of @false@ specifies that Amazon SES will not include headers in notifications.
--
-- This value can only be set when @NotificationType@ is already set to use a particular Amazon SNS topic.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihineEnabled :: Lens.Lens' SetIdentityHeadersInNotificationsEnabled Core.Bool
sihineEnabled = Lens.field @"enabled"
{-# DEPRECATED sihineEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Core.AWSRequest SetIdentityHeadersInNotificationsEnabled where
  type
    Rs SetIdentityHeadersInNotificationsEnabled =
      SetIdentityHeadersInNotificationsEnabledResponse
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
            ( Core.pure ("Action", "SetIdentityHeadersInNotificationsEnabled")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "Identity" identity)
                Core.<> (Core.toQueryValue "NotificationType" notificationType)
                Core.<> (Core.toQueryValue "Enabled" enabled)
            )
      }
  response =
    Response.receiveXMLWrapper
      "SetIdentityHeadersInNotificationsEnabledResult"
      ( \s h x ->
          SetIdentityHeadersInNotificationsEnabledResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkSetIdentityHeadersInNotificationsEnabledResponse' smart constructor.
newtype SetIdentityHeadersInNotificationsEnabledResponse = SetIdentityHeadersInNotificationsEnabledResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetIdentityHeadersInNotificationsEnabledResponse' value with any optional fields omitted.
mkSetIdentityHeadersInNotificationsEnabledResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SetIdentityHeadersInNotificationsEnabledResponse
mkSetIdentityHeadersInNotificationsEnabledResponse responseStatus =
  SetIdentityHeadersInNotificationsEnabledResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihinerrsResponseStatus :: Lens.Lens' SetIdentityHeadersInNotificationsEnabledResponse Core.Int
sihinerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sihinerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
