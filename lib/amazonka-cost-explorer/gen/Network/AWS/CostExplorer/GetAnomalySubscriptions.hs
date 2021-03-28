{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetAnomalySubscriptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the cost anomaly subscription objects for your account. You can filter using a list of cost anomaly monitor Amazon Resource Names (ARNs). 
module Network.AWS.CostExplorer.GetAnomalySubscriptions
    (
    -- * Creating a request
      GetAnomalySubscriptions (..)
    , mkGetAnomalySubscriptions
    -- ** Request lenses
    , gasMaxResults
    , gasMonitorArn
    , gasNextPageToken
    , gasSubscriptionArnList

    -- * Destructuring the response
    , GetAnomalySubscriptionsResponse (..)
    , mkGetAnomalySubscriptionsResponse
    -- ** Response lenses
    , gasrrsAnomalySubscriptions
    , gasrrsNextPageToken
    , gasrrsResponseStatus
    ) where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAnomalySubscriptions' smart constructor.
data GetAnomalySubscriptions = GetAnomalySubscriptions'
  { maxResults :: Core.Maybe Core.Int
    -- ^ The number of entries a paginated response contains. 
  , monitorArn :: Core.Maybe Types.MonitorArn
    -- ^ Cost anomaly monitor ARNs. 
  , nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size. 
  , subscriptionArnList :: Core.Maybe [Types.Value]
    -- ^ A list of cost anomaly subscription ARNs. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAnomalySubscriptions' value with any optional fields omitted.
mkGetAnomalySubscriptions
    :: GetAnomalySubscriptions
mkGetAnomalySubscriptions
  = GetAnomalySubscriptions'{maxResults = Core.Nothing,
                             monitorArn = Core.Nothing, nextPageToken = Core.Nothing,
                             subscriptionArnList = Core.Nothing}

-- | The number of entries a paginated response contains. 
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasMaxResults :: Lens.Lens' GetAnomalySubscriptions (Core.Maybe Core.Int)
gasMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gasMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Cost anomaly monitor ARNs. 
--
-- /Note:/ Consider using 'monitorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasMonitorArn :: Lens.Lens' GetAnomalySubscriptions (Core.Maybe Types.MonitorArn)
gasMonitorArn = Lens.field @"monitorArn"
{-# INLINEABLE gasMonitorArn #-}
{-# DEPRECATED monitorArn "Use generic-lens or generic-optics with 'monitorArn' instead"  #-}

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size. 
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasNextPageToken :: Lens.Lens' GetAnomalySubscriptions (Core.Maybe Types.NextPageToken)
gasNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE gasNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | A list of cost anomaly subscription ARNs. 
--
-- /Note:/ Consider using 'subscriptionArnList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasSubscriptionArnList :: Lens.Lens' GetAnomalySubscriptions (Core.Maybe [Types.Value])
gasSubscriptionArnList = Lens.field @"subscriptionArnList"
{-# INLINEABLE gasSubscriptionArnList #-}
{-# DEPRECATED subscriptionArnList "Use generic-lens or generic-optics with 'subscriptionArnList' instead"  #-}

instance Core.ToQuery GetAnomalySubscriptions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetAnomalySubscriptions where
        toHeaders GetAnomalySubscriptions{..}
          = Core.pure
              ("X-Amz-Target", "AWSInsightsIndexService.GetAnomalySubscriptions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetAnomalySubscriptions where
        toJSON GetAnomalySubscriptions{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("MonitorArn" Core..=) Core.<$> monitorArn,
                  ("NextPageToken" Core..=) Core.<$> nextPageToken,
                  ("SubscriptionArnList" Core..=) Core.<$> subscriptionArnList])

instance Core.AWSRequest GetAnomalySubscriptions where
        type Rs GetAnomalySubscriptions = GetAnomalySubscriptionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAnomalySubscriptionsResponse' Core.<$>
                   (x Core..:? "AnomalySubscriptions" Core..!= Core.mempty) Core.<*>
                     x Core..:? "NextPageToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAnomalySubscriptionsResponse' smart constructor.
data GetAnomalySubscriptionsResponse = GetAnomalySubscriptionsResponse'
  { anomalySubscriptions :: [Types.AnomalySubscription]
    -- ^ A list of cost anomaly subscriptions that includes the detailed metadata for each one. 
  , nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAnomalySubscriptionsResponse' value with any optional fields omitted.
mkGetAnomalySubscriptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAnomalySubscriptionsResponse
mkGetAnomalySubscriptionsResponse responseStatus
  = GetAnomalySubscriptionsResponse'{anomalySubscriptions =
                                       Core.mempty,
                                     nextPageToken = Core.Nothing, responseStatus}

-- | A list of cost anomaly subscriptions that includes the detailed metadata for each one. 
--
-- /Note:/ Consider using 'anomalySubscriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsAnomalySubscriptions :: Lens.Lens' GetAnomalySubscriptionsResponse [Types.AnomalySubscription]
gasrrsAnomalySubscriptions = Lens.field @"anomalySubscriptions"
{-# INLINEABLE gasrrsAnomalySubscriptions #-}
{-# DEPRECATED anomalySubscriptions "Use generic-lens or generic-optics with 'anomalySubscriptions' instead"  #-}

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size. 
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsNextPageToken :: Lens.Lens' GetAnomalySubscriptionsResponse (Core.Maybe Types.NextPageToken)
gasrrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE gasrrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsResponseStatus :: Lens.Lens' GetAnomalySubscriptionsResponse Core.Int
gasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
