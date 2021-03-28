{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      Unsubscribe (..)
    , mkUnsubscribe
    -- ** Request lenses
    , uSubscriptionArn

    -- * Destructuring the response
    , UnsubscribeResponse (..)
    , mkUnsubscribeResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | Input for Unsubscribe action.
--
-- /See:/ 'mkUnsubscribe' smart constructor.
newtype Unsubscribe = Unsubscribe'
  { subscriptionArn :: Types.SubscriptionArn
    -- ^ The ARN of the subscription to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Unsubscribe' value with any optional fields omitted.
mkUnsubscribe
    :: Types.SubscriptionArn -- ^ 'subscriptionArn'
    -> Unsubscribe
mkUnsubscribe subscriptionArn = Unsubscribe'{subscriptionArn}

-- | The ARN of the subscription to be deleted.
--
-- /Note:/ Consider using 'subscriptionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uSubscriptionArn :: Lens.Lens' Unsubscribe Types.SubscriptionArn
uSubscriptionArn = Lens.field @"subscriptionArn"
{-# INLINEABLE uSubscriptionArn #-}
{-# DEPRECATED subscriptionArn "Use generic-lens or generic-optics with 'subscriptionArn' instead"  #-}

instance Core.ToQuery Unsubscribe where
        toQuery Unsubscribe{..}
          = Core.toQueryPair "Action" ("Unsubscribe" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-03-31" :: Core.Text)
              Core.<> Core.toQueryPair "SubscriptionArn" subscriptionArn

instance Core.ToHeaders Unsubscribe where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest Unsubscribe where
        type Rs Unsubscribe = UnsubscribeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull UnsubscribeResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUnsubscribeResponse' smart constructor.
data UnsubscribeResponse = UnsubscribeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnsubscribeResponse' value with any optional fields omitted.
mkUnsubscribeResponse
    :: UnsubscribeResponse
mkUnsubscribeResponse = UnsubscribeResponse'
