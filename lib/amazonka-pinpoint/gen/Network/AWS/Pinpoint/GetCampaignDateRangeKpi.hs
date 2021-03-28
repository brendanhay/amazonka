{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetCampaignDateRangeKpi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) pre-aggregated data for a standard metric that applies to a campaign.
module Network.AWS.Pinpoint.GetCampaignDateRangeKpi
    (
    -- * Creating a request
      GetCampaignDateRangeKpi (..)
    , mkGetCampaignDateRangeKpi
    -- ** Request lenses
    , gcdrkApplicationId
    , gcdrkKpiName
    , gcdrkCampaignId
    , gcdrkEndTime
    , gcdrkNextToken
    , gcdrkPageSize
    , gcdrkStartTime

    -- * Destructuring the response
    , GetCampaignDateRangeKpiResponse (..)
    , mkGetCampaignDateRangeKpiResponse
    -- ** Response lenses
    , gcdrkrrsCampaignDateRangeKpiResponse
    , gcdrkrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCampaignDateRangeKpi' smart constructor.
data GetCampaignDateRangeKpi = GetCampaignDateRangeKpi'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , kpiName :: Core.Text
    -- ^ The name of the metric, also referred to as a /key performance indicator (KPI)/ , to retrieve data for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. Examples are email-open-rate and successful-delivery-rate. For a list of valid values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
  , campaignId :: Core.Text
    -- ^ The unique identifier for the campaign.
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

-- | Creates a 'GetCampaignDateRangeKpi' value with any optional fields omitted.
mkGetCampaignDateRangeKpi
    :: Core.Text -- ^ 'applicationId'
    -> Core.Text -- ^ 'kpiName'
    -> Core.Text -- ^ 'campaignId'
    -> GetCampaignDateRangeKpi
mkGetCampaignDateRangeKpi applicationId kpiName campaignId
  = GetCampaignDateRangeKpi'{applicationId, kpiName, campaignId,
                             endTime = Core.Nothing, nextToken = Core.Nothing,
                             pageSize = Core.Nothing, startTime = Core.Nothing}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrkApplicationId :: Lens.Lens' GetCampaignDateRangeKpi Core.Text
gcdrkApplicationId = Lens.field @"applicationId"
{-# INLINEABLE gcdrkApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The name of the metric, also referred to as a /key performance indicator (KPI)/ , to retrieve data for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. Examples are email-open-rate and successful-delivery-rate. For a list of valid values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
--
-- /Note:/ Consider using 'kpiName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrkKpiName :: Lens.Lens' GetCampaignDateRangeKpi Core.Text
gcdrkKpiName = Lens.field @"kpiName"
{-# INLINEABLE gcdrkKpiName #-}
{-# DEPRECATED kpiName "Use generic-lens or generic-optics with 'kpiName' instead"  #-}

-- | The unique identifier for the campaign.
--
-- /Note:/ Consider using 'campaignId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrkCampaignId :: Lens.Lens' GetCampaignDateRangeKpi Core.Text
gcdrkCampaignId = Lens.field @"campaignId"
{-# INLINEABLE gcdrkCampaignId #-}
{-# DEPRECATED campaignId "Use generic-lens or generic-optics with 'campaignId' instead"  #-}

-- | The last date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrkEndTime :: Lens.Lens' GetCampaignDateRangeKpi (Core.Maybe Core.UTCTime)
gcdrkEndTime = Lens.field @"endTime"
{-# INLINEABLE gcdrkEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrkNextToken :: Lens.Lens' GetCampaignDateRangeKpi (Core.Maybe Core.Text)
gcdrkNextToken = Lens.field @"nextToken"
{-# INLINEABLE gcdrkNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrkPageSize :: Lens.Lens' GetCampaignDateRangeKpi (Core.Maybe Core.Text)
gcdrkPageSize = Lens.field @"pageSize"
{-# INLINEABLE gcdrkPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The first date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value should also be fewer than 90 days from the current day.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrkStartTime :: Lens.Lens' GetCampaignDateRangeKpi (Core.Maybe Core.UTCTime)
gcdrkStartTime = Lens.field @"startTime"
{-# INLINEABLE gcdrkStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

instance Core.ToQuery GetCampaignDateRangeKpi where
        toQuery GetCampaignDateRangeKpi{..}
          = Core.maybe Core.mempty (Core.toQueryPair "end-time") endTime
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "next-token") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "page-size") pageSize
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "start-time") startTime

instance Core.ToHeaders GetCampaignDateRangeKpi where
        toHeaders GetCampaignDateRangeKpi{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetCampaignDateRangeKpi where
        type Rs GetCampaignDateRangeKpi = GetCampaignDateRangeKpiResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<> "/campaigns/"
                             Core.<> Core.toText campaignId
                             Core.<> "/kpis/daterange/"
                             Core.<> Core.toText kpiName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetCampaignDateRangeKpiResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetCampaignDateRangeKpiResponse' smart constructor.
data GetCampaignDateRangeKpiResponse = GetCampaignDateRangeKpiResponse'
  { campaignDateRangeKpiResponse :: Types.CampaignDateRangeKpiResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetCampaignDateRangeKpiResponse' value with any optional fields omitted.
mkGetCampaignDateRangeKpiResponse
    :: Types.CampaignDateRangeKpiResponse -- ^ 'campaignDateRangeKpiResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> GetCampaignDateRangeKpiResponse
mkGetCampaignDateRangeKpiResponse campaignDateRangeKpiResponse
  responseStatus
  = GetCampaignDateRangeKpiResponse'{campaignDateRangeKpiResponse,
                                     responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'campaignDateRangeKpiResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrkrrsCampaignDateRangeKpiResponse :: Lens.Lens' GetCampaignDateRangeKpiResponse Types.CampaignDateRangeKpiResponse
gcdrkrrsCampaignDateRangeKpiResponse = Lens.field @"campaignDateRangeKpiResponse"
{-# INLINEABLE gcdrkrrsCampaignDateRangeKpiResponse #-}
{-# DEPRECATED campaignDateRangeKpiResponse "Use generic-lens or generic-optics with 'campaignDateRangeKpiResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrkrrsResponseStatus :: Lens.Lens' GetCampaignDateRangeKpiResponse Core.Int
gcdrkrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcdrkrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
