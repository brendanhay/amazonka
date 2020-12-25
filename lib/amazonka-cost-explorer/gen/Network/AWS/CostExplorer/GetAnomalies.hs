{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetAnomalies (..),
    mkGetAnomalies,

    -- ** Request lenses
    gaDateInterval,
    gaFeedback,
    gaMaxResults,
    gaMonitorArn,
    gaNextPageToken,
    gaTotalImpact,

    -- * Destructuring the response
    GetAnomaliesResponse (..),
    mkGetAnomaliesResponse,

    -- ** Response lenses
    garrsAnomalies,
    garrsNextPageToken,
    garrsResponseStatus,
  )
where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAnomalies' smart constructor.
data GetAnomalies = GetAnomalies'
  { -- | Assigns the start and end dates for retrieving cost anomalies. The returned anomaly object will have an @AnomalyEndDate@ in the specified time range.
    dateInterval :: Types.AnomalyDateInterval,
    -- | Filters anomaly results by the feedback field on the anomaly object.
    feedback :: Core.Maybe Types.AnomalyFeedbackType,
    -- | The number of entries a paginated response contains.
    maxResults :: Core.Maybe Core.Int,
    -- | Retrieves all of the cost anomalies detected for a specific cost anomaly monitor Amazon Resource Name (ARN).
    monitorArn :: Core.Maybe Types.MonitorArn,
    -- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | Filters anomaly results by the total impact field on the anomaly object. For example, you can filter anomalies @GREATER_THAN 200.00@ to retrieve anomalies, with an estimated dollar impact greater than 200.
    totalImpact :: Core.Maybe Types.TotalImpactFilter
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAnomalies' value with any optional fields omitted.
mkGetAnomalies ::
  -- | 'dateInterval'
  Types.AnomalyDateInterval ->
  GetAnomalies
mkGetAnomalies dateInterval =
  GetAnomalies'
    { dateInterval,
      feedback = Core.Nothing,
      maxResults = Core.Nothing,
      monitorArn = Core.Nothing,
      nextPageToken = Core.Nothing,
      totalImpact = Core.Nothing
    }

-- | Assigns the start and end dates for retrieving cost anomalies. The returned anomaly object will have an @AnomalyEndDate@ in the specified time range.
--
-- /Note:/ Consider using 'dateInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaDateInterval :: Lens.Lens' GetAnomalies Types.AnomalyDateInterval
gaDateInterval = Lens.field @"dateInterval"
{-# DEPRECATED gaDateInterval "Use generic-lens or generic-optics with 'dateInterval' instead." #-}

-- | Filters anomaly results by the feedback field on the anomaly object.
--
-- /Note:/ Consider using 'feedback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaFeedback :: Lens.Lens' GetAnomalies (Core.Maybe Types.AnomalyFeedbackType)
gaFeedback = Lens.field @"feedback"
{-# DEPRECATED gaFeedback "Use generic-lens or generic-optics with 'feedback' instead." #-}

-- | The number of entries a paginated response contains.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaMaxResults :: Lens.Lens' GetAnomalies (Core.Maybe Core.Int)
gaMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Retrieves all of the cost anomalies detected for a specific cost anomaly monitor Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'monitorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaMonitorArn :: Lens.Lens' GetAnomalies (Core.Maybe Types.MonitorArn)
gaMonitorArn = Lens.field @"monitorArn"
{-# DEPRECATED gaMonitorArn "Use generic-lens or generic-optics with 'monitorArn' instead." #-}

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaNextPageToken :: Lens.Lens' GetAnomalies (Core.Maybe Types.NextPageToken)
gaNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED gaNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Filters anomaly results by the total impact field on the anomaly object. For example, you can filter anomalies @GREATER_THAN 200.00@ to retrieve anomalies, with an estimated dollar impact greater than 200.
--
-- /Note:/ Consider using 'totalImpact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaTotalImpact :: Lens.Lens' GetAnomalies (Core.Maybe Types.TotalImpactFilter)
gaTotalImpact = Lens.field @"totalImpact"
{-# DEPRECATED gaTotalImpact "Use generic-lens or generic-optics with 'totalImpact' instead." #-}

instance Core.FromJSON GetAnomalies where
  toJSON GetAnomalies {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DateInterval" Core..= dateInterval),
            ("Feedback" Core..=) Core.<$> feedback,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("MonitorArn" Core..=) Core.<$> monitorArn,
            ("NextPageToken" Core..=) Core.<$> nextPageToken,
            ("TotalImpact" Core..=) Core.<$> totalImpact
          ]
      )

instance Core.AWSRequest GetAnomalies where
  type Rs GetAnomalies = GetAnomaliesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSInsightsIndexService.GetAnomalies")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAnomaliesResponse'
            Core.<$> (x Core..:? "Anomalies" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetAnomaliesResponse' smart constructor.
data GetAnomaliesResponse = GetAnomaliesResponse'
  { -- | A list of cost anomalies.
    anomalies :: [Types.Anomaly],
    -- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAnomaliesResponse' value with any optional fields omitted.
mkGetAnomaliesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetAnomaliesResponse
mkGetAnomaliesResponse responseStatus =
  GetAnomaliesResponse'
    { anomalies = Core.mempty,
      nextPageToken = Core.Nothing,
      responseStatus
    }

-- | A list of cost anomalies.
--
-- /Note:/ Consider using 'anomalies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsAnomalies :: Lens.Lens' GetAnomaliesResponse [Types.Anomaly]
garrsAnomalies = Lens.field @"anomalies"
{-# DEPRECATED garrsAnomalies "Use generic-lens or generic-optics with 'anomalies' instead." #-}

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsNextPageToken :: Lens.Lens' GetAnomaliesResponse (Core.Maybe Types.NextPageToken)
garrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED garrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsResponseStatus :: Lens.Lens' GetAnomaliesResponse Core.Int
garrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED garrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
