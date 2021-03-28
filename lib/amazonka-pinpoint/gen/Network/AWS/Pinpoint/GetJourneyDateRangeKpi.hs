{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetJourneyDateRangeKpi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) pre-aggregated data for a standard engagement metric that applies to a journey.
module Network.AWS.Pinpoint.GetJourneyDateRangeKpi
    (
    -- * Creating a request
      GetJourneyDateRangeKpi (..)
    , mkGetJourneyDateRangeKpi
    -- ** Request lenses
    , gjdrkJourneyId
    , gjdrkApplicationId
    , gjdrkKpiName
    , gjdrkEndTime
    , gjdrkNextToken
    , gjdrkPageSize
    , gjdrkStartTime

    -- * Destructuring the response
    , GetJourneyDateRangeKpiResponse (..)
    , mkGetJourneyDateRangeKpiResponse
    -- ** Response lenses
    , gjdrkrrsJourneyDateRangeKpiResponse
    , gjdrkrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetJourneyDateRangeKpi' smart constructor.
data GetJourneyDateRangeKpi = GetJourneyDateRangeKpi'
  { journeyId :: Core.Text
    -- ^ The unique identifier for the journey.
  , applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , kpiName :: Core.Text
    -- ^ The name of the metric, also referred to as a /key performance indicator (KPI)/ , to retrieve data for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. Examples are email-open-rate and successful-delivery-rate. For a list of valid values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
  , endTime :: Core.Maybe Core.UTCTime
    -- ^ The last date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
  , pageSize :: Core.Maybe Core.Text
    -- ^ The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
  , startTime :: Core.Maybe Core.UTCTime
    -- ^ The first date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value should also be fewer than 90 days from the current day.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetJourneyDateRangeKpi' value with any optional fields omitted.
mkGetJourneyDateRangeKpi
    :: Core.Text -- ^ 'journeyId'
    -> Core.Text -- ^ 'applicationId'
    -> Core.Text -- ^ 'kpiName'
    -> GetJourneyDateRangeKpi
mkGetJourneyDateRangeKpi journeyId applicationId kpiName
  = GetJourneyDateRangeKpi'{journeyId, applicationId, kpiName,
                            endTime = Core.Nothing, nextToken = Core.Nothing,
                            pageSize = Core.Nothing, startTime = Core.Nothing}

-- | The unique identifier for the journey.
--
-- /Note:/ Consider using 'journeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrkJourneyId :: Lens.Lens' GetJourneyDateRangeKpi Core.Text
gjdrkJourneyId = Lens.field @"journeyId"
{-# INLINEABLE gjdrkJourneyId #-}
{-# DEPRECATED journeyId "Use generic-lens or generic-optics with 'journeyId' instead"  #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrkApplicationId :: Lens.Lens' GetJourneyDateRangeKpi Core.Text
gjdrkApplicationId = Lens.field @"applicationId"
{-# INLINEABLE gjdrkApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The name of the metric, also referred to as a /key performance indicator (KPI)/ , to retrieve data for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. Examples are email-open-rate and successful-delivery-rate. For a list of valid values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
--
-- /Note:/ Consider using 'kpiName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrkKpiName :: Lens.Lens' GetJourneyDateRangeKpi Core.Text
gjdrkKpiName = Lens.field @"kpiName"
{-# INLINEABLE gjdrkKpiName #-}
{-# DEPRECATED kpiName "Use generic-lens or generic-optics with 'kpiName' instead"  #-}

-- | The last date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrkEndTime :: Lens.Lens' GetJourneyDateRangeKpi (Core.Maybe Core.UTCTime)
gjdrkEndTime = Lens.field @"endTime"
{-# INLINEABLE gjdrkEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrkNextToken :: Lens.Lens' GetJourneyDateRangeKpi (Core.Maybe Core.Text)
gjdrkNextToken = Lens.field @"nextToken"
{-# INLINEABLE gjdrkNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrkPageSize :: Lens.Lens' GetJourneyDateRangeKpi (Core.Maybe Core.Text)
gjdrkPageSize = Lens.field @"pageSize"
{-# INLINEABLE gjdrkPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The first date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value should also be fewer than 90 days from the current day.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrkStartTime :: Lens.Lens' GetJourneyDateRangeKpi (Core.Maybe Core.UTCTime)
gjdrkStartTime = Lens.field @"startTime"
{-# INLINEABLE gjdrkStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

instance Core.ToQuery GetJourneyDateRangeKpi where
        toQuery GetJourneyDateRangeKpi{..}
          = Core.maybe Core.mempty (Core.toQueryPair "end-time") endTime
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "next-token") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "page-size") pageSize
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "start-time") startTime

instance Core.ToHeaders GetJourneyDateRangeKpi where
        toHeaders GetJourneyDateRangeKpi{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetJourneyDateRangeKpi where
        type Rs GetJourneyDateRangeKpi = GetJourneyDateRangeKpiResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<> "/journeys/"
                             Core.<> Core.toText journeyId
                             Core.<> "/kpis/daterange/"
                             Core.<> Core.toText kpiName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetJourneyDateRangeKpiResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetJourneyDateRangeKpiResponse' smart constructor.
data GetJourneyDateRangeKpiResponse = GetJourneyDateRangeKpiResponse'
  { journeyDateRangeKpiResponse :: Types.JourneyDateRangeKpiResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetJourneyDateRangeKpiResponse' value with any optional fields omitted.
mkGetJourneyDateRangeKpiResponse
    :: Types.JourneyDateRangeKpiResponse -- ^ 'journeyDateRangeKpiResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> GetJourneyDateRangeKpiResponse
mkGetJourneyDateRangeKpiResponse journeyDateRangeKpiResponse
  responseStatus
  = GetJourneyDateRangeKpiResponse'{journeyDateRangeKpiResponse,
                                    responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'journeyDateRangeKpiResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrkrrsJourneyDateRangeKpiResponse :: Lens.Lens' GetJourneyDateRangeKpiResponse Types.JourneyDateRangeKpiResponse
gjdrkrrsJourneyDateRangeKpiResponse = Lens.field @"journeyDateRangeKpiResponse"
{-# INLINEABLE gjdrkrrsJourneyDateRangeKpiResponse #-}
{-# DEPRECATED journeyDateRangeKpiResponse "Use generic-lens or generic-optics with 'journeyDateRangeKpiResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrkrrsResponseStatus :: Lens.Lens' GetJourneyDateRangeKpiResponse Core.Int
gjdrkrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gjdrkrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
