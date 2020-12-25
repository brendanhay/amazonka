{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.SetIdentityFeedbackForwardingEnabled
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Given an identity (an email address or a domain), enables or disables whether Amazon SES forwards bounce and complaint notifications as email. Feedback forwarding can only be disabled when Amazon Simple Notification Service (Amazon SNS) topics are specified for both bounces and complaints.
--
-- You can execute this operation no more than once per second.
-- For more information about using notifications with Amazon SES, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html Amazon SES Developer Guide> .
module Network.AWS.SES.SetIdentityFeedbackForwardingEnabled
  ( -- * Creating a request
    SetIdentityFeedbackForwardingEnabled (..),
    mkSetIdentityFeedbackForwardingEnabled,

    -- ** Request lenses
    siffeIdentity,
    siffeForwardingEnabled,

    -- * Destructuring the response
    SetIdentityFeedbackForwardingEnabledResponse (..),
    mkSetIdentityFeedbackForwardingEnabledResponse,

    -- ** Response lenses
    sifferrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to enable or disable whether Amazon SES forwards you bounce and complaint notifications through email. For information about email feedback forwarding, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications-via-email.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkSetIdentityFeedbackForwardingEnabled' smart constructor.
data SetIdentityFeedbackForwardingEnabled = SetIdentityFeedbackForwardingEnabled'
  { -- | The identity for which to set bounce and complaint notification forwarding. Examples: @user@example.com@ , @example.com@ .
    identity :: Types.Identity,
    -- | Sets whether Amazon SES will forward bounce and complaint notifications as email. @true@ specifies that Amazon SES will forward bounce and complaint notifications as email, in addition to any Amazon SNS topic publishing otherwise specified. @false@ specifies that Amazon SES will publish bounce and complaint notifications only through Amazon SNS. This value can only be set to @false@ when Amazon SNS topics are set for both @Bounce@ and @Complaint@ notification types.
    forwardingEnabled :: Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetIdentityFeedbackForwardingEnabled' value with any optional fields omitted.
mkSetIdentityFeedbackForwardingEnabled ::
  -- | 'identity'
  Types.Identity ->
  -- | 'forwardingEnabled'
  Core.Bool ->
  SetIdentityFeedbackForwardingEnabled
mkSetIdentityFeedbackForwardingEnabled identity forwardingEnabled =
  SetIdentityFeedbackForwardingEnabled'
    { identity,
      forwardingEnabled
    }

-- | The identity for which to set bounce and complaint notification forwarding. Examples: @user@example.com@ , @example.com@ .
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siffeIdentity :: Lens.Lens' SetIdentityFeedbackForwardingEnabled Types.Identity
siffeIdentity = Lens.field @"identity"
{-# DEPRECATED siffeIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

-- | Sets whether Amazon SES will forward bounce and complaint notifications as email. @true@ specifies that Amazon SES will forward bounce and complaint notifications as email, in addition to any Amazon SNS topic publishing otherwise specified. @false@ specifies that Amazon SES will publish bounce and complaint notifications only through Amazon SNS. This value can only be set to @false@ when Amazon SNS topics are set for both @Bounce@ and @Complaint@ notification types.
--
-- /Note:/ Consider using 'forwardingEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siffeForwardingEnabled :: Lens.Lens' SetIdentityFeedbackForwardingEnabled Core.Bool
siffeForwardingEnabled = Lens.field @"forwardingEnabled"
{-# DEPRECATED siffeForwardingEnabled "Use generic-lens or generic-optics with 'forwardingEnabled' instead." #-}

instance Core.AWSRequest SetIdentityFeedbackForwardingEnabled where
  type
    Rs SetIdentityFeedbackForwardingEnabled =
      SetIdentityFeedbackForwardingEnabledResponse
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
            ( Core.pure ("Action", "SetIdentityFeedbackForwardingEnabled")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "Identity" identity)
                Core.<> (Core.toQueryValue "ForwardingEnabled" forwardingEnabled)
            )
      }
  response =
    Response.receiveXMLWrapper
      "SetIdentityFeedbackForwardingEnabledResult"
      ( \s h x ->
          SetIdentityFeedbackForwardingEnabledResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkSetIdentityFeedbackForwardingEnabledResponse' smart constructor.
newtype SetIdentityFeedbackForwardingEnabledResponse = SetIdentityFeedbackForwardingEnabledResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetIdentityFeedbackForwardingEnabledResponse' value with any optional fields omitted.
mkSetIdentityFeedbackForwardingEnabledResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SetIdentityFeedbackForwardingEnabledResponse
mkSetIdentityFeedbackForwardingEnabledResponse responseStatus =
  SetIdentityFeedbackForwardingEnabledResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sifferrsResponseStatus :: Lens.Lens' SetIdentityFeedbackForwardingEnabledResponse Core.Int
sifferrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sifferrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
