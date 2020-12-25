{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.ConfirmSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Verifies an endpoint owner's intent to receive messages by validating the token sent to the endpoint by an earlier @Subscribe@ action. If the token is valid, the action creates a new subscription and returns its Amazon Resource Name (ARN). This call requires an AWS signature only when the @AuthenticateOnUnsubscribe@ flag is set to "true".
module Network.AWS.SNS.ConfirmSubscription
  ( -- * Creating a request
    ConfirmSubscription (..),
    mkConfirmSubscription,

    -- ** Request lenses
    csTopicArn,
    csToken,
    csAuthenticateOnUnsubscribe,

    -- * Destructuring the response
    ConfirmSubscriptionResponse (..),
    mkConfirmSubscriptionResponse,

    -- ** Response lenses
    csrrsSubscriptionArn,
    csrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | Input for ConfirmSubscription action.
--
-- /See:/ 'mkConfirmSubscription' smart constructor.
data ConfirmSubscription = ConfirmSubscription'
  { -- | The ARN of the topic for which you wish to confirm a subscription.
    topicArn :: Types.TopicARN,
    -- | Short-lived token sent to an endpoint during the @Subscribe@ action.
    token :: Types.Token,
    -- | Disallows unauthenticated unsubscribes of the subscription. If the value of this parameter is @true@ and the request has an AWS signature, then only the topic owner and the subscription owner can unsubscribe the endpoint. The unsubscribe action requires AWS authentication.
    authenticateOnUnsubscribe :: Core.Maybe Types.AuthenticateOnUnsubscribe
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConfirmSubscription' value with any optional fields omitted.
mkConfirmSubscription ::
  -- | 'topicArn'
  Types.TopicARN ->
  -- | 'token'
  Types.Token ->
  ConfirmSubscription
mkConfirmSubscription topicArn token =
  ConfirmSubscription'
    { topicArn,
      token,
      authenticateOnUnsubscribe = Core.Nothing
    }

-- | The ARN of the topic for which you wish to confirm a subscription.
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTopicArn :: Lens.Lens' ConfirmSubscription Types.TopicARN
csTopicArn = Lens.field @"topicArn"
{-# DEPRECATED csTopicArn "Use generic-lens or generic-optics with 'topicArn' instead." #-}

-- | Short-lived token sent to an endpoint during the @Subscribe@ action.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csToken :: Lens.Lens' ConfirmSubscription Types.Token
csToken = Lens.field @"token"
{-# DEPRECATED csToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | Disallows unauthenticated unsubscribes of the subscription. If the value of this parameter is @true@ and the request has an AWS signature, then only the topic owner and the subscription owner can unsubscribe the endpoint. The unsubscribe action requires AWS authentication.
--
-- /Note:/ Consider using 'authenticateOnUnsubscribe' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csAuthenticateOnUnsubscribe :: Lens.Lens' ConfirmSubscription (Core.Maybe Types.AuthenticateOnUnsubscribe)
csAuthenticateOnUnsubscribe = Lens.field @"authenticateOnUnsubscribe"
{-# DEPRECATED csAuthenticateOnUnsubscribe "Use generic-lens or generic-optics with 'authenticateOnUnsubscribe' instead." #-}

instance Core.AWSRequest ConfirmSubscription where
  type Rs ConfirmSubscription = ConfirmSubscriptionResponse
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
            ( Core.pure ("Action", "ConfirmSubscription")
                Core.<> (Core.pure ("Version", "2010-03-31"))
                Core.<> (Core.toQueryValue "TopicArn" topicArn)
                Core.<> (Core.toQueryValue "Token" token)
                Core.<> ( Core.toQueryValue "AuthenticateOnUnsubscribe"
                            Core.<$> authenticateOnUnsubscribe
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ConfirmSubscriptionResult"
      ( \s h x ->
          ConfirmSubscriptionResponse'
            Core.<$> (x Core..@? "SubscriptionArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Response for ConfirmSubscriptions action.
--
-- /See:/ 'mkConfirmSubscriptionResponse' smart constructor.
data ConfirmSubscriptionResponse = ConfirmSubscriptionResponse'
  { -- | The ARN of the created subscription.
    subscriptionArn :: Core.Maybe Types.SubscriptionArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConfirmSubscriptionResponse' value with any optional fields omitted.
mkConfirmSubscriptionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ConfirmSubscriptionResponse
mkConfirmSubscriptionResponse responseStatus =
  ConfirmSubscriptionResponse'
    { subscriptionArn = Core.Nothing,
      responseStatus
    }

-- | The ARN of the created subscription.
--
-- /Note:/ Consider using 'subscriptionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsSubscriptionArn :: Lens.Lens' ConfirmSubscriptionResponse (Core.Maybe Types.SubscriptionArn)
csrrsSubscriptionArn = Lens.field @"subscriptionArn"
{-# DEPRECATED csrrsSubscriptionArn "Use generic-lens or generic-optics with 'subscriptionArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' ConfirmSubscriptionResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
