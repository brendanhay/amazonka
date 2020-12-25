{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetApplicationDateRangeKpi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) pre-aggregated data for a standard metric that applies to an application.
module Network.AWS.Pinpoint.GetApplicationDateRangeKpi
  ( -- * Creating a request
    GetApplicationDateRangeKpi (..),
    mkGetApplicationDateRangeKpi,

    -- ** Request lenses
    gadrkApplicationId,
    gadrkKpiName,
    gadrkEndTime,
    gadrkNextToken,
    gadrkPageSize,
    gadrkStartTime,

    -- * Destructuring the response
    GetApplicationDateRangeKpiResponse (..),
    mkGetApplicationDateRangeKpiResponse,

    -- ** Response lenses
    gadrkrrsApplicationDateRangeKpiResponse,
    gadrkrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetApplicationDateRangeKpi' smart constructor.
data GetApplicationDateRangeKpi = GetApplicationDateRangeKpi'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    -- | The name of the metric, also referred to as a /key performance indicator (KPI)/ , to retrieve data for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. Examples are email-open-rate and successful-delivery-rate. For a list of valid values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
    kpiName :: Core.Text,
    -- | The last date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
    endTime :: Core.Maybe Core.UTCTime,
    -- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    pageSize :: Core.Maybe Core.Text,
    -- | The first date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value should also be fewer than 90 days from the current day.
    startTime :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetApplicationDateRangeKpi' value with any optional fields omitted.
mkGetApplicationDateRangeKpi ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'kpiName'
  Core.Text ->
  GetApplicationDateRangeKpi
mkGetApplicationDateRangeKpi applicationId kpiName =
  GetApplicationDateRangeKpi'
    { applicationId,
      kpiName,
      endTime = Core.Nothing,
      nextToken = Core.Nothing,
      pageSize = Core.Nothing,
      startTime = Core.Nothing
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrkApplicationId :: Lens.Lens' GetApplicationDateRangeKpi Core.Text
gadrkApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gadrkApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The name of the metric, also referred to as a /key performance indicator (KPI)/ , to retrieve data for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. Examples are email-open-rate and successful-delivery-rate. For a list of valid values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
--
-- /Note:/ Consider using 'kpiName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrkKpiName :: Lens.Lens' GetApplicationDateRangeKpi Core.Text
gadrkKpiName = Lens.field @"kpiName"
{-# DEPRECATED gadrkKpiName "Use generic-lens or generic-optics with 'kpiName' instead." #-}

-- | The last date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrkEndTime :: Lens.Lens' GetApplicationDateRangeKpi (Core.Maybe Core.UTCTime)
gadrkEndTime = Lens.field @"endTime"
{-# DEPRECATED gadrkEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrkNextToken :: Lens.Lens' GetApplicationDateRangeKpi (Core.Maybe Core.Text)
gadrkNextToken = Lens.field @"nextToken"
{-# DEPRECATED gadrkNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrkPageSize :: Lens.Lens' GetApplicationDateRangeKpi (Core.Maybe Core.Text)
gadrkPageSize = Lens.field @"pageSize"
{-# DEPRECATED gadrkPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The first date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value should also be fewer than 90 days from the current day.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrkStartTime :: Lens.Lens' GetApplicationDateRangeKpi (Core.Maybe Core.UTCTime)
gadrkStartTime = Lens.field @"startTime"
{-# DEPRECATED gadrkStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

instance Core.AWSRequest GetApplicationDateRangeKpi where
  type
    Rs GetApplicationDateRangeKpi =
      GetApplicationDateRangeKpiResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/kpis/daterange/")
                Core.<> (Core.toText kpiName)
            ),
        Core._rqQuery =
          Core.toQueryValue "end-time" Core.<$> endTime
            Core.<> (Core.toQueryValue "next-token" Core.<$> nextToken)
            Core.<> (Core.toQueryValue "page-size" Core.<$> pageSize)
            Core.<> (Core.toQueryValue "start-time" Core.<$> startTime),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApplicationDateRangeKpiResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetApplicationDateRangeKpiResponse' smart constructor.
data GetApplicationDateRangeKpiResponse = GetApplicationDateRangeKpiResponse'
  { applicationDateRangeKpiResponse :: Types.ApplicationDateRangeKpiResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetApplicationDateRangeKpiResponse' value with any optional fields omitted.
mkGetApplicationDateRangeKpiResponse ::
  -- | 'applicationDateRangeKpiResponse'
  Types.ApplicationDateRangeKpiResponse ->
  -- | 'responseStatus'
  Core.Int ->
  GetApplicationDateRangeKpiResponse
mkGetApplicationDateRangeKpiResponse
  applicationDateRangeKpiResponse
  responseStatus =
    GetApplicationDateRangeKpiResponse'
      { applicationDateRangeKpiResponse,
        responseStatus
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'applicationDateRangeKpiResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrkrrsApplicationDateRangeKpiResponse :: Lens.Lens' GetApplicationDateRangeKpiResponse Types.ApplicationDateRangeKpiResponse
gadrkrrsApplicationDateRangeKpiResponse = Lens.field @"applicationDateRangeKpiResponse"
{-# DEPRECATED gadrkrrsApplicationDateRangeKpiResponse "Use generic-lens or generic-optics with 'applicationDateRangeKpiResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrkrrsResponseStatus :: Lens.Lens' GetApplicationDateRangeKpiResponse Core.Int
gadrkrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gadrkrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
