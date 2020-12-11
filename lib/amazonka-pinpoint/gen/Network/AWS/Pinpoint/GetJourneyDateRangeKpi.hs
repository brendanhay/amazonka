{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetJourneyDateRangeKpi (..),
    mkGetJourneyDateRangeKpi,

    -- ** Request lenses
    gjdrkStartTime,
    gjdrkNextToken,
    gjdrkEndTime,
    gjdrkPageSize,
    gjdrkJourneyId,
    gjdrkApplicationId,
    gjdrkKpiName,

    -- * Destructuring the response
    GetJourneyDateRangeKpiResponse (..),
    mkGetJourneyDateRangeKpiResponse,

    -- ** Response lenses
    gjdrkrsResponseStatus,
    gjdrkrsJourneyDateRangeKpiResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetJourneyDateRangeKpi' smart constructor.
data GetJourneyDateRangeKpi = GetJourneyDateRangeKpi'
  { startTime ::
      Lude.Maybe Lude.Timestamp,
    nextToken :: Lude.Maybe Lude.Text,
    endTime :: Lude.Maybe Lude.Timestamp,
    pageSize :: Lude.Maybe Lude.Text,
    journeyId :: Lude.Text,
    applicationId :: Lude.Text,
    kpiName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJourneyDateRangeKpi' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'endTime' - The last date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
-- * 'journeyId' - The unique identifier for the journey.
-- * 'kpiName' - The name of the metric, also referred to as a /key performance indicator (KPI)/ , to retrieve data for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. Examples are email-open-rate and successful-delivery-rate. For a list of valid values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
-- * 'nextToken' - The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
-- * 'pageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
-- * 'startTime' - The first date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value should also be fewer than 90 days from the current day.
mkGetJourneyDateRangeKpi ::
  -- | 'journeyId'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  -- | 'kpiName'
  Lude.Text ->
  GetJourneyDateRangeKpi
mkGetJourneyDateRangeKpi pJourneyId_ pApplicationId_ pKpiName_ =
  GetJourneyDateRangeKpi'
    { startTime = Lude.Nothing,
      nextToken = Lude.Nothing,
      endTime = Lude.Nothing,
      pageSize = Lude.Nothing,
      journeyId = pJourneyId_,
      applicationId = pApplicationId_,
      kpiName = pKpiName_
    }

-- | The first date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value should also be fewer than 90 days from the current day.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrkStartTime :: Lens.Lens' GetJourneyDateRangeKpi (Lude.Maybe Lude.Timestamp)
gjdrkStartTime = Lens.lens (startTime :: GetJourneyDateRangeKpi -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: GetJourneyDateRangeKpi)
{-# DEPRECATED gjdrkStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrkNextToken :: Lens.Lens' GetJourneyDateRangeKpi (Lude.Maybe Lude.Text)
gjdrkNextToken = Lens.lens (nextToken :: GetJourneyDateRangeKpi -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetJourneyDateRangeKpi)
{-# DEPRECATED gjdrkNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The last date and time to retrieve data for, as part of an inclusive date range that filters the query results. This value should be in extended ISO 8601 format and use Coordinated Universal Time (UTC), for example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrkEndTime :: Lens.Lens' GetJourneyDateRangeKpi (Lude.Maybe Lude.Timestamp)
gjdrkEndTime = Lens.lens (endTime :: GetJourneyDateRangeKpi -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: GetJourneyDateRangeKpi)
{-# DEPRECATED gjdrkEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrkPageSize :: Lens.Lens' GetJourneyDateRangeKpi (Lude.Maybe Lude.Text)
gjdrkPageSize = Lens.lens (pageSize :: GetJourneyDateRangeKpi -> Lude.Maybe Lude.Text) (\s a -> s {pageSize = a} :: GetJourneyDateRangeKpi)
{-# DEPRECATED gjdrkPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The unique identifier for the journey.
--
-- /Note:/ Consider using 'journeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrkJourneyId :: Lens.Lens' GetJourneyDateRangeKpi Lude.Text
gjdrkJourneyId = Lens.lens (journeyId :: GetJourneyDateRangeKpi -> Lude.Text) (\s a -> s {journeyId = a} :: GetJourneyDateRangeKpi)
{-# DEPRECATED gjdrkJourneyId "Use generic-lens or generic-optics with 'journeyId' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrkApplicationId :: Lens.Lens' GetJourneyDateRangeKpi Lude.Text
gjdrkApplicationId = Lens.lens (applicationId :: GetJourneyDateRangeKpi -> Lude.Text) (\s a -> s {applicationId = a} :: GetJourneyDateRangeKpi)
{-# DEPRECATED gjdrkApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The name of the metric, also referred to as a /key performance indicator (KPI)/ , to retrieve data for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. Examples are email-open-rate and successful-delivery-rate. For a list of valid values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
--
-- /Note:/ Consider using 'kpiName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrkKpiName :: Lens.Lens' GetJourneyDateRangeKpi Lude.Text
gjdrkKpiName = Lens.lens (kpiName :: GetJourneyDateRangeKpi -> Lude.Text) (\s a -> s {kpiName = a} :: GetJourneyDateRangeKpi)
{-# DEPRECATED gjdrkKpiName "Use generic-lens or generic-optics with 'kpiName' instead." #-}

instance Lude.AWSRequest GetJourneyDateRangeKpi where
  type Rs GetJourneyDateRangeKpi = GetJourneyDateRangeKpiResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetJourneyDateRangeKpiResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders GetJourneyDateRangeKpi where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetJourneyDateRangeKpi where
  toPath GetJourneyDateRangeKpi' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/journeys/",
        Lude.toBS journeyId,
        "/kpis/daterange/",
        Lude.toBS kpiName
      ]

instance Lude.ToQuery GetJourneyDateRangeKpi where
  toQuery GetJourneyDateRangeKpi' {..} =
    Lude.mconcat
      [ "start-time" Lude.=: startTime,
        "next-token" Lude.=: nextToken,
        "end-time" Lude.=: endTime,
        "page-size" Lude.=: pageSize
      ]

-- | /See:/ 'mkGetJourneyDateRangeKpiResponse' smart constructor.
data GetJourneyDateRangeKpiResponse = GetJourneyDateRangeKpiResponse'
  { responseStatus ::
      Lude.Int,
    journeyDateRangeKpiResponse ::
      JourneyDateRangeKpiResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJourneyDateRangeKpiResponse' with the minimum fields required to make a request.
--
-- * 'journeyDateRangeKpiResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkGetJourneyDateRangeKpiResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'journeyDateRangeKpiResponse'
  JourneyDateRangeKpiResponse ->
  GetJourneyDateRangeKpiResponse
mkGetJourneyDateRangeKpiResponse
  pResponseStatus_
  pJourneyDateRangeKpiResponse_ =
    GetJourneyDateRangeKpiResponse'
      { responseStatus =
          pResponseStatus_,
        journeyDateRangeKpiResponse = pJourneyDateRangeKpiResponse_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrkrsResponseStatus :: Lens.Lens' GetJourneyDateRangeKpiResponse Lude.Int
gjdrkrsResponseStatus = Lens.lens (responseStatus :: GetJourneyDateRangeKpiResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetJourneyDateRangeKpiResponse)
{-# DEPRECATED gjdrkrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'journeyDateRangeKpiResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrkrsJourneyDateRangeKpiResponse :: Lens.Lens' GetJourneyDateRangeKpiResponse JourneyDateRangeKpiResponse
gjdrkrsJourneyDateRangeKpiResponse = Lens.lens (journeyDateRangeKpiResponse :: GetJourneyDateRangeKpiResponse -> JourneyDateRangeKpiResponse) (\s a -> s {journeyDateRangeKpiResponse = a} :: GetJourneyDateRangeKpiResponse)
{-# DEPRECATED gjdrkrsJourneyDateRangeKpiResponse "Use generic-lens or generic-optics with 'journeyDateRangeKpiResponse' instead." #-}
