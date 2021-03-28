{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetAnomalies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all of the cost anomalies detected on your account, during the time period specified by the @DateInterval@ object. 
module Network.AWS.CostExplorer.GetAnomalies
    (
    -- * Creating a request
      GetAnomalies (..)
    , mkGetAnomalies
    -- ** Request lenses
    , gaDateInterval
    , gaFeedback
    , gaMaxResults
    , gaMonitorArn
    , gaNextPageToken
    , gaTotalImpact

    -- * Destructuring the response
    , GetAnomaliesResponse (..)
    , mkGetAnomaliesResponse
    -- ** Response lenses
    , garrsAnomalies
    , garrsNextPageToken
    , garrsResponseStatus
    ) where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAnomalies' smart constructor.
data GetAnomalies = GetAnomalies'
  { dateInterval :: Types.AnomalyDateInterval
    -- ^ Assigns the start and end dates for retrieving cost anomalies. The returned anomaly object will have an @AnomalyEndDate@ in the specified time range. 
  , feedback :: Core.Maybe Types.AnomalyFeedbackType
    -- ^ Filters anomaly results by the feedback field on the anomaly object. 
  , maxResults :: Core.Maybe Core.Int
    -- ^ The number of entries a paginated response contains. 
  , monitorArn :: Core.Maybe Types.MonitorArn
    -- ^ Retrieves all of the cost anomalies detected for a specific cost anomaly monitor Amazon Resource Name (ARN). 
  , nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size. 
  , totalImpact :: Core.Maybe Types.TotalImpactFilter
    -- ^ Filters anomaly results by the total impact field on the anomaly object. For example, you can filter anomalies @GREATER_THAN 200.00@ to retrieve anomalies, with an estimated dollar impact greater than 200. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAnomalies' value with any optional fields omitted.
mkGetAnomalies
    :: Types.AnomalyDateInterval -- ^ 'dateInterval'
    -> GetAnomalies
mkGetAnomalies dateInterval
  = GetAnomalies'{dateInterval, feedback = Core.Nothing,
                  maxResults = Core.Nothing, monitorArn = Core.Nothing,
                  nextPageToken = Core.Nothing, totalImpact = Core.Nothing}

-- | Assigns the start and end dates for retrieving cost anomalies. The returned anomaly object will have an @AnomalyEndDate@ in the specified time range. 
--
-- /Note:/ Consider using 'dateInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaDateInterval :: Lens.Lens' GetAnomalies Types.AnomalyDateInterval
gaDateInterval = Lens.field @"dateInterval"
{-# INLINEABLE gaDateInterval #-}
{-# DEPRECATED dateInterval "Use generic-lens or generic-optics with 'dateInterval' instead"  #-}

-- | Filters anomaly results by the feedback field on the anomaly object. 
--
-- /Note:/ Consider using 'feedback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaFeedback :: Lens.Lens' GetAnomalies (Core.Maybe Types.AnomalyFeedbackType)
gaFeedback = Lens.field @"feedback"
{-# INLINEABLE gaFeedback #-}
{-# DEPRECATED feedback "Use generic-lens or generic-optics with 'feedback' instead"  #-}

-- | The number of entries a paginated response contains. 
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaMaxResults :: Lens.Lens' GetAnomalies (Core.Maybe Core.Int)
gaMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gaMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Retrieves all of the cost anomalies detected for a specific cost anomaly monitor Amazon Resource Name (ARN). 
--
-- /Note:/ Consider using 'monitorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaMonitorArn :: Lens.Lens' GetAnomalies (Core.Maybe Types.MonitorArn)
gaMonitorArn = Lens.field @"monitorArn"
{-# INLINEABLE gaMonitorArn #-}
{-# DEPRECATED monitorArn "Use generic-lens or generic-optics with 'monitorArn' instead"  #-}

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size. 
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaNextPageToken :: Lens.Lens' GetAnomalies (Core.Maybe Types.NextPageToken)
gaNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE gaNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | Filters anomaly results by the total impact field on the anomaly object. For example, you can filter anomalies @GREATER_THAN 200.00@ to retrieve anomalies, with an estimated dollar impact greater than 200. 
--
-- /Note:/ Consider using 'totalImpact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaTotalImpact :: Lens.Lens' GetAnomalies (Core.Maybe Types.TotalImpactFilter)
gaTotalImpact = Lens.field @"totalImpact"
{-# INLINEABLE gaTotalImpact #-}
{-# DEPRECATED totalImpact "Use generic-lens or generic-optics with 'totalImpact' instead"  #-}

instance Core.ToQuery GetAnomalies where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetAnomalies where
        toHeaders GetAnomalies{..}
          = Core.pure
              ("X-Amz-Target", "AWSInsightsIndexService.GetAnomalies")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetAnomalies where
        toJSON GetAnomalies{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DateInterval" Core..= dateInterval),
                  ("Feedback" Core..=) Core.<$> feedback,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("MonitorArn" Core..=) Core.<$> monitorArn,
                  ("NextPageToken" Core..=) Core.<$> nextPageToken,
                  ("TotalImpact" Core..=) Core.<$> totalImpact])

instance Core.AWSRequest GetAnomalies where
        type Rs GetAnomalies = GetAnomaliesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAnomaliesResponse' Core.<$>
                   (x Core..:? "Anomalies" Core..!= Core.mempty) Core.<*>
                     x Core..:? "NextPageToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAnomaliesResponse' smart constructor.
data GetAnomaliesResponse = GetAnomaliesResponse'
  { anomalies :: [Types.Anomaly]
    -- ^ A list of cost anomalies. 
  , nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAnomaliesResponse' value with any optional fields omitted.
mkGetAnomaliesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAnomaliesResponse
mkGetAnomaliesResponse responseStatus
  = GetAnomaliesResponse'{anomalies = Core.mempty,
                          nextPageToken = Core.Nothing, responseStatus}

-- | A list of cost anomalies. 
--
-- /Note:/ Consider using 'anomalies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsAnomalies :: Lens.Lens' GetAnomaliesResponse [Types.Anomaly]
garrsAnomalies = Lens.field @"anomalies"
{-# INLINEABLE garrsAnomalies #-}
{-# DEPRECATED anomalies "Use generic-lens or generic-optics with 'anomalies' instead"  #-}

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size. 
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsNextPageToken :: Lens.Lens' GetAnomaliesResponse (Core.Maybe Types.NextPageToken)
garrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE garrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsResponseStatus :: Lens.Lens' GetAnomaliesResponse Core.Int
garrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE garrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
