{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Unsubscribe
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subscription. If the subscription requires authentication for deletion, only the owner of the subscription or the topic's owner can unsubscribe, and an AWS signature is required. If the @Unsubscribe@ call does not require authentication and the requester is not the subscription owner, a final cancellation message is delivered to the endpoint, so that the endpoint owner can easily resubscribe to the topic if the @Unsubscribe@ request was unintended.
--
-- This action is throttled at 100 transactions per second (TPS).
module Network.AWS.SNS.Unsubscribe
  ( -- * Creating a request
    Unsubscribe (..),
    mkUnsubscribe,

    -- ** Request lenses
    uSubscriptionArn,

    -- * Destructuring the response
    UnsubscribeResponse (..),
    mkUnsubscribeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | Input for Unsubscribe action.
--
-- /See:/ 'mkUnsubscribe' smart constructor.
newtype Unsubscribe = Unsubscribe'
  { -- | The ARN of the subscription to be deleted.
    subscriptionArn :: Types.SubscriptionArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Unsubscribe' value with any optional fields omitted.
mkUnsubscribe ::
  -- | 'subscriptionArn'
  Types.SubscriptionArn ->
  Unsubscribe
mkUnsubscribe subscriptionArn = Unsubscribe' {subscriptionArn}

-- | The ARN of the subscription to be deleted.
--
-- /Note:/ Consider using 'subscriptionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uSubscriptionArn :: Lens.Lens' Unsubscribe Types.SubscriptionArn
uSubscriptionArn = Lens.field @"subscriptionArn"
{-# DEPRECATED uSubscriptionArn "Use generic-lens or generic-optics with 'subscriptionArn' instead." #-}

instance Core.AWSRequest Unsubscribe where
  type Rs Unsubscribe = UnsubscribeResponse
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
            ( Core.pure ("Action", "Unsubscribe")
                Core.<> (Core.pure ("Version", "2010-03-31"))
                Core.<> (Core.toQueryValue "SubscriptionArn" subscriptionArn)
            )
      }
  response = Response.receiveNull UnsubscribeResponse'

-- | /See:/ 'mkUnsubscribeResponse' smart constructor.
data UnsubscribeResponse = UnsubscribeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnsubscribeResponse' value with any optional fields omitted.
mkUnsubscribeResponse ::
  UnsubscribeResponse
mkUnsubscribeResponse = UnsubscribeResponse'
