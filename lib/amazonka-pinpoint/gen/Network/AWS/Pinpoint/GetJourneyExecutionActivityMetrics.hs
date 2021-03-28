{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetJourneyExecutionActivityMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) pre-aggregated data for a standard execution metric that applies to a journey activity.
module Network.AWS.Pinpoint.GetJourneyExecutionActivityMetrics
    (
    -- * Creating a request
      GetJourneyExecutionActivityMetrics (..)
    , mkGetJourneyExecutionActivityMetrics
    -- ** Request lenses
    , gjeamJourneyActivityId
    , gjeamApplicationId
    , gjeamJourneyId
    , gjeamNextToken
    , gjeamPageSize

    -- * Destructuring the response
    , GetJourneyExecutionActivityMetricsResponse (..)
    , mkGetJourneyExecutionActivityMetricsResponse
    -- ** Response lenses
    , gjeamrrsJourneyExecutionActivityMetricsResponse
    , gjeamrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetJourneyExecutionActivityMetrics' smart constructor.
data GetJourneyExecutionActivityMetrics = GetJourneyExecutionActivityMetrics'
  { journeyActivityId :: Core.Text
    -- ^ The unique identifier for the journey activity.
  , applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , journeyId :: Core.Text
    -- ^ The unique identifier for the journey.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
  , pageSize :: Core.Maybe Core.Text
    -- ^ The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetJourneyExecutionActivityMetrics' value with any optional fields omitted.
mkGetJourneyExecutionActivityMetrics
    :: Core.Text -- ^ 'journeyActivityId'
    -> Core.Text -- ^ 'applicationId'
    -> Core.Text -- ^ 'journeyId'
    -> GetJourneyExecutionActivityMetrics
mkGetJourneyExecutionActivityMetrics journeyActivityId
  applicationId journeyId
  = GetJourneyExecutionActivityMetrics'{journeyActivityId,
                                        applicationId, journeyId, nextToken = Core.Nothing,
                                        pageSize = Core.Nothing}

-- | The unique identifier for the journey activity.
--
-- /Note:/ Consider using 'journeyActivityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjeamJourneyActivityId :: Lens.Lens' GetJourneyExecutionActivityMetrics Core.Text
gjeamJourneyActivityId = Lens.field @"journeyActivityId"
{-# INLINEABLE gjeamJourneyActivityId #-}
{-# DEPRECATED journeyActivityId "Use generic-lens or generic-optics with 'journeyActivityId' instead"  #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjeamApplicationId :: Lens.Lens' GetJourneyExecutionActivityMetrics Core.Text
gjeamApplicationId = Lens.field @"applicationId"
{-# INLINEABLE gjeamApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The unique identifier for the journey.
--
-- /Note:/ Consider using 'journeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjeamJourneyId :: Lens.Lens' GetJourneyExecutionActivityMetrics Core.Text
gjeamJourneyId = Lens.field @"journeyId"
{-# INLINEABLE gjeamJourneyId #-}
{-# DEPRECATED journeyId "Use generic-lens or generic-optics with 'journeyId' instead"  #-}

-- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjeamNextToken :: Lens.Lens' GetJourneyExecutionActivityMetrics (Core.Maybe Core.Text)
gjeamNextToken = Lens.field @"nextToken"
{-# INLINEABLE gjeamNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjeamPageSize :: Lens.Lens' GetJourneyExecutionActivityMetrics (Core.Maybe Core.Text)
gjeamPageSize = Lens.field @"pageSize"
{-# INLINEABLE gjeamPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

instance Core.ToQuery GetJourneyExecutionActivityMetrics where
        toQuery GetJourneyExecutionActivityMetrics{..}
          = Core.maybe Core.mempty (Core.toQueryPair "next-token") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "page-size") pageSize

instance Core.ToHeaders GetJourneyExecutionActivityMetrics where
        toHeaders GetJourneyExecutionActivityMetrics{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetJourneyExecutionActivityMetrics where
        type Rs GetJourneyExecutionActivityMetrics =
             GetJourneyExecutionActivityMetricsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<> "/journeys/"
                             Core.<> Core.toText journeyId
                             Core.<> "/activities/"
                             Core.<> Core.toText journeyActivityId
                             Core.<> "/execution-metrics",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetJourneyExecutionActivityMetricsResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetJourneyExecutionActivityMetricsResponse' smart constructor.
data GetJourneyExecutionActivityMetricsResponse = GetJourneyExecutionActivityMetricsResponse'
  { journeyExecutionActivityMetricsResponse :: Types.JourneyExecutionActivityMetricsResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetJourneyExecutionActivityMetricsResponse' value with any optional fields omitted.
mkGetJourneyExecutionActivityMetricsResponse
    :: Types.JourneyExecutionActivityMetricsResponse -- ^ 'journeyExecutionActivityMetricsResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> GetJourneyExecutionActivityMetricsResponse
mkGetJourneyExecutionActivityMetricsResponse
  journeyExecutionActivityMetricsResponse responseStatus
  = GetJourneyExecutionActivityMetricsResponse'{journeyExecutionActivityMetricsResponse,
                                                responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'journeyExecutionActivityMetricsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjeamrrsJourneyExecutionActivityMetricsResponse :: Lens.Lens' GetJourneyExecutionActivityMetricsResponse Types.JourneyExecutionActivityMetricsResponse
gjeamrrsJourneyExecutionActivityMetricsResponse = Lens.field @"journeyExecutionActivityMetricsResponse"
{-# INLINEABLE gjeamrrsJourneyExecutionActivityMetricsResponse #-}
{-# DEPRECATED journeyExecutionActivityMetricsResponse "Use generic-lens or generic-optics with 'journeyExecutionActivityMetricsResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjeamrrsResponseStatus :: Lens.Lens' GetJourneyExecutionActivityMetricsResponse Core.Int
gjeamrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gjeamrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
