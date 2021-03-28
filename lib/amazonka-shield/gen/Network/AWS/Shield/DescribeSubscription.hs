{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DescribeSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details about the AWS Shield Advanced subscription for an account.
module Network.AWS.Shield.DescribeSubscription
    (
    -- * Creating a request
      DescribeSubscription (..)
    , mkDescribeSubscription

    -- * Destructuring the response
    , DescribeSubscriptionResponse (..)
    , mkDescribeSubscriptionResponse
    -- ** Response lenses
    , dsrrsSubscription
    , dsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkDescribeSubscription' smart constructor.
data DescribeSubscription = DescribeSubscription'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSubscription' value with any optional fields omitted.
mkDescribeSubscription
    :: DescribeSubscription
mkDescribeSubscription = DescribeSubscription'

instance Core.ToQuery DescribeSubscription where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeSubscription where
        toHeaders DescribeSubscription{..}
          = Core.pure
              ("X-Amz-Target", "AWSShield_20160616.DescribeSubscription")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeSubscription where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DescribeSubscription where
        type Rs DescribeSubscription = DescribeSubscriptionResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeSubscriptionResponse' Core.<$>
                   (x Core..:? "Subscription") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeSubscriptionResponse' smart constructor.
data DescribeSubscriptionResponse = DescribeSubscriptionResponse'
  { subscription :: Core.Maybe Types.Subscription
    -- ^ The AWS Shield Advanced subscription details for an account.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeSubscriptionResponse' value with any optional fields omitted.
mkDescribeSubscriptionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSubscriptionResponse
mkDescribeSubscriptionResponse responseStatus
  = DescribeSubscriptionResponse'{subscription = Core.Nothing,
                                  responseStatus}

-- | The AWS Shield Advanced subscription details for an account.
--
-- /Note:/ Consider using 'subscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsSubscription :: Lens.Lens' DescribeSubscriptionResponse (Core.Maybe Types.Subscription)
dsrrsSubscription = Lens.field @"subscription"
{-# INLINEABLE dsrrsSubscription #-}
{-# DEPRECATED subscription "Use generic-lens or generic-optics with 'subscription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DescribeSubscriptionResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
