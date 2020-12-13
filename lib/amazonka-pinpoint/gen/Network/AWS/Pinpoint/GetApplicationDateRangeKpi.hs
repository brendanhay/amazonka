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
    gadrkKpiName,
    gadrkStartTime,
    gadrkNextToken,
    gadrkApplicationId,
    gadrkEndTime,
    gadrkPageSize,

    -- * Destructuring the response
    GetApplicationDateRangeKpiResponse (..),
    mkGetApplicationDateRangeKpiResponse,

    -- ** Response lenses
    gadrkrsApplicationDateRangeKpiResponse,
    gadrkrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetApplicationDateRangeKpi' smart constructor.
data GetApplicationDateRangeKpi = GetApplicationDateRangeKpi'
  { -- | The name of the metric, also referred to as a /key performance indicator (KPI)/ , to retrieve data for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. Examples are email-open-rate and successful-delivery-rate. For a list of valid values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
    kpiName :: Lude.Text,
    -- | The first date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value should also be fewer than 90 days from the current day.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text,
    -- | The last date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
    endTime :: Lude.Maybe Lude.Timestamp,
    -- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    pageSize :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetApplicationDateRangeKpi' with the minimum fields required to make a request.
--
-- * 'kpiName' - The name of the metric, also referred to as a /key performance indicator (KPI)/ , to retrieve data for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. Examples are email-open-rate and successful-delivery-rate. For a list of valid values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
-- * 'startTime' - The first date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value should also be fewer than 90 days from the current day.
-- * 'nextToken' - The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'endTime' - The last date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
-- * 'pageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
mkGetApplicationDateRangeKpi ::
  -- | 'kpiName'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  GetApplicationDateRangeKpi
mkGetApplicationDateRangeKpi pKpiName_ pApplicationId_ =
  GetApplicationDateRangeKpi'
    { kpiName = pKpiName_,
      startTime = Lude.Nothing,
      nextToken = Lude.Nothing,
      applicationId = pApplicationId_,
      endTime = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | The name of the metric, also referred to as a /key performance indicator (KPI)/ , to retrieve data for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. Examples are email-open-rate and successful-delivery-rate. For a list of valid values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
--
-- /Note:/ Consider using 'kpiName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrkKpiName :: Lens.Lens' GetApplicationDateRangeKpi Lude.Text
gadrkKpiName = Lens.lens (kpiName :: GetApplicationDateRangeKpi -> Lude.Text) (\s a -> s {kpiName = a} :: GetApplicationDateRangeKpi)
{-# DEPRECATED gadrkKpiName "Use generic-lens or generic-optics with 'kpiName' instead." #-}

-- | The first date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value should also be fewer than 90 days from the current day.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrkStartTime :: Lens.Lens' GetApplicationDateRangeKpi (Lude.Maybe Lude.Timestamp)
gadrkStartTime = Lens.lens (startTime :: GetApplicationDateRangeKpi -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: GetApplicationDateRangeKpi)
{-# DEPRECATED gadrkStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrkNextToken :: Lens.Lens' GetApplicationDateRangeKpi (Lude.Maybe Lude.Text)
gadrkNextToken = Lens.lens (nextToken :: GetApplicationDateRangeKpi -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetApplicationDateRangeKpi)
{-# DEPRECATED gadrkNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrkApplicationId :: Lens.Lens' GetApplicationDateRangeKpi Lude.Text
gadrkApplicationId = Lens.lens (applicationId :: GetApplicationDateRangeKpi -> Lude.Text) (\s a -> s {applicationId = a} :: GetApplicationDateRangeKpi)
{-# DEPRECATED gadrkApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The last date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrkEndTime :: Lens.Lens' GetApplicationDateRangeKpi (Lude.Maybe Lude.Timestamp)
gadrkEndTime = Lens.lens (endTime :: GetApplicationDateRangeKpi -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: GetApplicationDateRangeKpi)
{-# DEPRECATED gadrkEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrkPageSize :: Lens.Lens' GetApplicationDateRangeKpi (Lude.Maybe Lude.Text)
gadrkPageSize = Lens.lens (pageSize :: GetApplicationDateRangeKpi -> Lude.Maybe Lude.Text) (\s a -> s {pageSize = a} :: GetApplicationDateRangeKpi)
{-# DEPRECATED gadrkPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Lude.AWSRequest GetApplicationDateRangeKpi where
  type
    Rs GetApplicationDateRangeKpi =
      GetApplicationDateRangeKpiResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetApplicationDateRangeKpiResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetApplicationDateRangeKpi where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetApplicationDateRangeKpi where
  toPath GetApplicationDateRangeKpi' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/kpis/daterange/",
        Lude.toBS kpiName
      ]

instance Lude.ToQuery GetApplicationDateRangeKpi where
  toQuery GetApplicationDateRangeKpi' {..} =
    Lude.mconcat
      [ "start-time" Lude.=: startTime,
        "next-token" Lude.=: nextToken,
        "end-time" Lude.=: endTime,
        "page-size" Lude.=: pageSize
      ]

-- | /See:/ 'mkGetApplicationDateRangeKpiResponse' smart constructor.
data GetApplicationDateRangeKpiResponse = GetApplicationDateRangeKpiResponse'
  { applicationDateRangeKpiResponse :: ApplicationDateRangeKpiResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetApplicationDateRangeKpiResponse' with the minimum fields required to make a request.
--
-- * 'applicationDateRangeKpiResponse' -
-- * 'responseStatus' - The response status code.
mkGetApplicationDateRangeKpiResponse ::
  -- | 'applicationDateRangeKpiResponse'
  ApplicationDateRangeKpiResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetApplicationDateRangeKpiResponse
mkGetApplicationDateRangeKpiResponse
  pApplicationDateRangeKpiResponse_
  pResponseStatus_ =
    GetApplicationDateRangeKpiResponse'
      { applicationDateRangeKpiResponse =
          pApplicationDateRangeKpiResponse_,
        responseStatus = pResponseStatus_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'applicationDateRangeKpiResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrkrsApplicationDateRangeKpiResponse :: Lens.Lens' GetApplicationDateRangeKpiResponse ApplicationDateRangeKpiResponse
gadrkrsApplicationDateRangeKpiResponse = Lens.lens (applicationDateRangeKpiResponse :: GetApplicationDateRangeKpiResponse -> ApplicationDateRangeKpiResponse) (\s a -> s {applicationDateRangeKpiResponse = a} :: GetApplicationDateRangeKpiResponse)
{-# DEPRECATED gadrkrsApplicationDateRangeKpiResponse "Use generic-lens or generic-optics with 'applicationDateRangeKpiResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrkrsResponseStatus :: Lens.Lens' GetApplicationDateRangeKpiResponse Lude.Int
gadrkrsResponseStatus = Lens.lens (responseStatus :: GetApplicationDateRangeKpiResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetApplicationDateRangeKpiResponse)
{-# DEPRECATED gadrkrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
