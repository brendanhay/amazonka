{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.UpdateAnomalySubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing cost anomaly monitor subscription. 
module Network.AWS.CostExplorer.UpdateAnomalySubscription
    (
    -- * Creating a request
      UpdateAnomalySubscription (..)
    , mkUpdateAnomalySubscription
    -- ** Request lenses
    , uasSubscriptionArn
    , uasFrequency
    , uasMonitorArnList
    , uasSubscribers
    , uasSubscriptionName
    , uasThreshold

    -- * Destructuring the response
    , UpdateAnomalySubscriptionResponse (..)
    , mkUpdateAnomalySubscriptionResponse
    -- ** Response lenses
    , uasrrsSubscriptionArn
    , uasrrsResponseStatus
    ) where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateAnomalySubscription' smart constructor.
data UpdateAnomalySubscription = UpdateAnomalySubscription'
  { subscriptionArn :: Types.GenericString
    -- ^ A cost anomaly subscription Amazon Resource Name (ARN). 
  , frequency :: Core.Maybe Types.AnomalySubscriptionFrequency
    -- ^ The update to the frequency value at which subscribers will receive notifications. 
  , monitorArnList :: Core.Maybe [Types.Value]
    -- ^ A list of cost anomaly subscription ARNs. 
  , subscribers :: Core.Maybe [Types.Subscriber]
    -- ^ The update to the subscriber list. 
  , subscriptionName :: Core.Maybe Types.GenericString
    -- ^ The subscription's new name. 
  , threshold :: Core.Maybe Core.Double
    -- ^ The update to the threshold value for receiving notifications. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAnomalySubscription' value with any optional fields omitted.
mkUpdateAnomalySubscription
    :: Types.GenericString -- ^ 'subscriptionArn'
    -> UpdateAnomalySubscription
mkUpdateAnomalySubscription subscriptionArn
  = UpdateAnomalySubscription'{subscriptionArn,
                               frequency = Core.Nothing, monitorArnList = Core.Nothing,
                               subscribers = Core.Nothing, subscriptionName = Core.Nothing,
                               threshold = Core.Nothing}

-- | A cost anomaly subscription Amazon Resource Name (ARN). 
--
-- /Note:/ Consider using 'subscriptionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasSubscriptionArn :: Lens.Lens' UpdateAnomalySubscription Types.GenericString
uasSubscriptionArn = Lens.field @"subscriptionArn"
{-# INLINEABLE uasSubscriptionArn #-}
{-# DEPRECATED subscriptionArn "Use generic-lens or generic-optics with 'subscriptionArn' instead"  #-}

-- | The update to the frequency value at which subscribers will receive notifications. 
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasFrequency :: Lens.Lens' UpdateAnomalySubscription (Core.Maybe Types.AnomalySubscriptionFrequency)
uasFrequency = Lens.field @"frequency"
{-# INLINEABLE uasFrequency #-}
{-# DEPRECATED frequency "Use generic-lens or generic-optics with 'frequency' instead"  #-}

-- | A list of cost anomaly subscription ARNs. 
--
-- /Note:/ Consider using 'monitorArnList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasMonitorArnList :: Lens.Lens' UpdateAnomalySubscription (Core.Maybe [Types.Value])
uasMonitorArnList = Lens.field @"monitorArnList"
{-# INLINEABLE uasMonitorArnList #-}
{-# DEPRECATED monitorArnList "Use generic-lens or generic-optics with 'monitorArnList' instead"  #-}

-- | The update to the subscriber list. 
--
-- /Note:/ Consider using 'subscribers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasSubscribers :: Lens.Lens' UpdateAnomalySubscription (Core.Maybe [Types.Subscriber])
uasSubscribers = Lens.field @"subscribers"
{-# INLINEABLE uasSubscribers #-}
{-# DEPRECATED subscribers "Use generic-lens or generic-optics with 'subscribers' instead"  #-}

-- | The subscription's new name. 
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasSubscriptionName :: Lens.Lens' UpdateAnomalySubscription (Core.Maybe Types.GenericString)
uasSubscriptionName = Lens.field @"subscriptionName"
{-# INLINEABLE uasSubscriptionName #-}
{-# DEPRECATED subscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead"  #-}

-- | The update to the threshold value for receiving notifications. 
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasThreshold :: Lens.Lens' UpdateAnomalySubscription (Core.Maybe Core.Double)
uasThreshold = Lens.field @"threshold"
{-# INLINEABLE uasThreshold #-}
{-# DEPRECATED threshold "Use generic-lens or generic-optics with 'threshold' instead"  #-}

instance Core.ToQuery UpdateAnomalySubscription where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateAnomalySubscription where
        toHeaders UpdateAnomalySubscription{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSInsightsIndexService.UpdateAnomalySubscription")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateAnomalySubscription where
        toJSON UpdateAnomalySubscription{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SubscriptionArn" Core..= subscriptionArn),
                  ("Frequency" Core..=) Core.<$> frequency,
                  ("MonitorArnList" Core..=) Core.<$> monitorArnList,
                  ("Subscribers" Core..=) Core.<$> subscribers,
                  ("SubscriptionName" Core..=) Core.<$> subscriptionName,
                  ("Threshold" Core..=) Core.<$> threshold])

instance Core.AWSRequest UpdateAnomalySubscription where
        type Rs UpdateAnomalySubscription =
             UpdateAnomalySubscriptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateAnomalySubscriptionResponse' Core.<$>
                   (x Core..: "SubscriptionArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateAnomalySubscriptionResponse' smart constructor.
data UpdateAnomalySubscriptionResponse = UpdateAnomalySubscriptionResponse'
  { subscriptionArn :: Types.GenericString
    -- ^ A cost anomaly subscription ARN. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAnomalySubscriptionResponse' value with any optional fields omitted.
mkUpdateAnomalySubscriptionResponse
    :: Types.GenericString -- ^ 'subscriptionArn'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateAnomalySubscriptionResponse
mkUpdateAnomalySubscriptionResponse subscriptionArn responseStatus
  = UpdateAnomalySubscriptionResponse'{subscriptionArn,
                                       responseStatus}

-- | A cost anomaly subscription ARN. 
--
-- /Note:/ Consider using 'subscriptionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrrsSubscriptionArn :: Lens.Lens' UpdateAnomalySubscriptionResponse Types.GenericString
uasrrsSubscriptionArn = Lens.field @"subscriptionArn"
{-# INLINEABLE uasrrsSubscriptionArn #-}
{-# DEPRECATED subscriptionArn "Use generic-lens or generic-optics with 'subscriptionArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrrsResponseStatus :: Lens.Lens' UpdateAnomalySubscriptionResponse Core.Int
uasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
