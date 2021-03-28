{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetAnomalyMonitors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the cost anomaly monitor definitions for your account. You can filter using a list of cost anomaly monitor Amazon Resource Names (ARNs). 
module Network.AWS.CostExplorer.GetAnomalyMonitors
    (
    -- * Creating a request
      GetAnomalyMonitors (..)
    , mkGetAnomalyMonitors
    -- ** Request lenses
    , gamMaxResults
    , gamMonitorArnList
    , gamNextPageToken

    -- * Destructuring the response
    , GetAnomalyMonitorsResponse (..)
    , mkGetAnomalyMonitorsResponse
    -- ** Response lenses
    , gamrrsAnomalyMonitors
    , gamrrsNextPageToken
    , gamrrsResponseStatus
    ) where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAnomalyMonitors' smart constructor.
data GetAnomalyMonitors = GetAnomalyMonitors'
  { maxResults :: Core.Maybe Core.Int
    -- ^ The number of entries a paginated response contains. 
  , monitorArnList :: Core.Maybe [Types.Value]
    -- ^ A list of cost anomaly monitor ARNs. 
  , nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAnomalyMonitors' value with any optional fields omitted.
mkGetAnomalyMonitors
    :: GetAnomalyMonitors
mkGetAnomalyMonitors
  = GetAnomalyMonitors'{maxResults = Core.Nothing,
                        monitorArnList = Core.Nothing, nextPageToken = Core.Nothing}

-- | The number of entries a paginated response contains. 
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gamMaxResults :: Lens.Lens' GetAnomalyMonitors (Core.Maybe Core.Int)
gamMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gamMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A list of cost anomaly monitor ARNs. 
--
-- /Note:/ Consider using 'monitorArnList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gamMonitorArnList :: Lens.Lens' GetAnomalyMonitors (Core.Maybe [Types.Value])
gamMonitorArnList = Lens.field @"monitorArnList"
{-# INLINEABLE gamMonitorArnList #-}
{-# DEPRECATED monitorArnList "Use generic-lens or generic-optics with 'monitorArnList' instead"  #-}

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size. 
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gamNextPageToken :: Lens.Lens' GetAnomalyMonitors (Core.Maybe Types.NextPageToken)
gamNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE gamNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

instance Core.ToQuery GetAnomalyMonitors where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetAnomalyMonitors where
        toHeaders GetAnomalyMonitors{..}
          = Core.pure
              ("X-Amz-Target", "AWSInsightsIndexService.GetAnomalyMonitors")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetAnomalyMonitors where
        toJSON GetAnomalyMonitors{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("MonitorArnList" Core..=) Core.<$> monitorArnList,
                  ("NextPageToken" Core..=) Core.<$> nextPageToken])

instance Core.AWSRequest GetAnomalyMonitors where
        type Rs GetAnomalyMonitors = GetAnomalyMonitorsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAnomalyMonitorsResponse' Core.<$>
                   (x Core..:? "AnomalyMonitors" Core..!= Core.mempty) Core.<*>
                     x Core..:? "NextPageToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAnomalyMonitorsResponse' smart constructor.
data GetAnomalyMonitorsResponse = GetAnomalyMonitorsResponse'
  { anomalyMonitors :: [Types.AnomalyMonitor]
    -- ^ A list of cost anomaly monitors that includes the detailed metadata for each monitor. 
  , nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAnomalyMonitorsResponse' value with any optional fields omitted.
mkGetAnomalyMonitorsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAnomalyMonitorsResponse
mkGetAnomalyMonitorsResponse responseStatus
  = GetAnomalyMonitorsResponse'{anomalyMonitors = Core.mempty,
                                nextPageToken = Core.Nothing, responseStatus}

-- | A list of cost anomaly monitors that includes the detailed metadata for each monitor. 
--
-- /Note:/ Consider using 'anomalyMonitors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gamrrsAnomalyMonitors :: Lens.Lens' GetAnomalyMonitorsResponse [Types.AnomalyMonitor]
gamrrsAnomalyMonitors = Lens.field @"anomalyMonitors"
{-# INLINEABLE gamrrsAnomalyMonitors #-}
{-# DEPRECATED anomalyMonitors "Use generic-lens or generic-optics with 'anomalyMonitors' instead"  #-}

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size. 
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gamrrsNextPageToken :: Lens.Lens' GetAnomalyMonitorsResponse (Core.Maybe Types.NextPageToken)
gamrrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE gamrrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gamrrsResponseStatus :: Lens.Lens' GetAnomalyMonitorsResponse Core.Int
gamrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gamrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
