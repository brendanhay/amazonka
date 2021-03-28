{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.CreateAnomalySubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a subscription to a cost anomaly detection monitor. You can use each subscription to define subscribers with email or SNS notifications. Email subscribers can set a dollar threshold and a time frequency for receiving notifications. 
module Network.AWS.CostExplorer.CreateAnomalySubscription
    (
    -- * Creating a request
      CreateAnomalySubscription (..)
    , mkCreateAnomalySubscription
    -- ** Request lenses
    , casAnomalySubscription

    -- * Destructuring the response
    , CreateAnomalySubscriptionResponse (..)
    , mkCreateAnomalySubscriptionResponse
    -- ** Response lenses
    , casrrsSubscriptionArn
    , casrrsResponseStatus
    ) where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateAnomalySubscription' smart constructor.
newtype CreateAnomalySubscription = CreateAnomalySubscription'
  { anomalySubscription :: Types.AnomalySubscription
    -- ^ The cost anomaly subscription object that you want to create. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAnomalySubscription' value with any optional fields omitted.
mkCreateAnomalySubscription
    :: Types.AnomalySubscription -- ^ 'anomalySubscription'
    -> CreateAnomalySubscription
mkCreateAnomalySubscription anomalySubscription
  = CreateAnomalySubscription'{anomalySubscription}

-- | The cost anomaly subscription object that you want to create. 
--
-- /Note:/ Consider using 'anomalySubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casAnomalySubscription :: Lens.Lens' CreateAnomalySubscription Types.AnomalySubscription
casAnomalySubscription = Lens.field @"anomalySubscription"
{-# INLINEABLE casAnomalySubscription #-}
{-# DEPRECATED anomalySubscription "Use generic-lens or generic-optics with 'anomalySubscription' instead"  #-}

instance Core.ToQuery CreateAnomalySubscription where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateAnomalySubscription where
        toHeaders CreateAnomalySubscription{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSInsightsIndexService.CreateAnomalySubscription")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateAnomalySubscription where
        toJSON CreateAnomalySubscription{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AnomalySubscription" Core..= anomalySubscription)])

instance Core.AWSRequest CreateAnomalySubscription where
        type Rs CreateAnomalySubscription =
             CreateAnomalySubscriptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateAnomalySubscriptionResponse' Core.<$>
                   (x Core..: "SubscriptionArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateAnomalySubscriptionResponse' smart constructor.
data CreateAnomalySubscriptionResponse = CreateAnomalySubscriptionResponse'
  { subscriptionArn :: Types.SubscriptionArn
    -- ^ The unique identifier of your newly created cost anomaly subscription. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAnomalySubscriptionResponse' value with any optional fields omitted.
mkCreateAnomalySubscriptionResponse
    :: Types.SubscriptionArn -- ^ 'subscriptionArn'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateAnomalySubscriptionResponse
mkCreateAnomalySubscriptionResponse subscriptionArn responseStatus
  = CreateAnomalySubscriptionResponse'{subscriptionArn,
                                       responseStatus}

-- | The unique identifier of your newly created cost anomaly subscription. 
--
-- /Note:/ Consider using 'subscriptionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casrrsSubscriptionArn :: Lens.Lens' CreateAnomalySubscriptionResponse Types.SubscriptionArn
casrrsSubscriptionArn = Lens.field @"subscriptionArn"
{-# INLINEABLE casrrsSubscriptionArn #-}
{-# DEPRECATED subscriptionArn "Use generic-lens or generic-optics with 'subscriptionArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casrrsResponseStatus :: Lens.Lens' CreateAnomalySubscriptionResponse Core.Int
casrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE casrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
