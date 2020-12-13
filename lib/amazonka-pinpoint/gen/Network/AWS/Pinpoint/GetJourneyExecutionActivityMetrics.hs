{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetJourneyExecutionActivityMetrics (..),
    mkGetJourneyExecutionActivityMetrics,

    -- ** Request lenses
    gjeamJourneyActivityId,
    gjeamNextToken,
    gjeamApplicationId,
    gjeamJourneyId,
    gjeamPageSize,

    -- * Destructuring the response
    GetJourneyExecutionActivityMetricsResponse (..),
    mkGetJourneyExecutionActivityMetricsResponse,

    -- ** Response lenses
    gjeamrsJourneyExecutionActivityMetricsResponse,
    gjeamrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetJourneyExecutionActivityMetrics' smart constructor.
data GetJourneyExecutionActivityMetrics = GetJourneyExecutionActivityMetrics'
  { -- | The unique identifier for the journey activity.
    journeyActivityId :: Lude.Text,
    -- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text,
    -- | The unique identifier for the journey.
    journeyId :: Lude.Text,
    -- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    pageSize :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJourneyExecutionActivityMetrics' with the minimum fields required to make a request.
--
-- * 'journeyActivityId' - The unique identifier for the journey activity.
-- * 'nextToken' - The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'journeyId' - The unique identifier for the journey.
-- * 'pageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
mkGetJourneyExecutionActivityMetrics ::
  -- | 'journeyActivityId'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  -- | 'journeyId'
  Lude.Text ->
  GetJourneyExecutionActivityMetrics
mkGetJourneyExecutionActivityMetrics
  pJourneyActivityId_
  pApplicationId_
  pJourneyId_ =
    GetJourneyExecutionActivityMetrics'
      { journeyActivityId =
          pJourneyActivityId_,
        nextToken = Lude.Nothing,
        applicationId = pApplicationId_,
        journeyId = pJourneyId_,
        pageSize = Lude.Nothing
      }

-- | The unique identifier for the journey activity.
--
-- /Note:/ Consider using 'journeyActivityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjeamJourneyActivityId :: Lens.Lens' GetJourneyExecutionActivityMetrics Lude.Text
gjeamJourneyActivityId = Lens.lens (journeyActivityId :: GetJourneyExecutionActivityMetrics -> Lude.Text) (\s a -> s {journeyActivityId = a} :: GetJourneyExecutionActivityMetrics)
{-# DEPRECATED gjeamJourneyActivityId "Use generic-lens or generic-optics with 'journeyActivityId' instead." #-}

-- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjeamNextToken :: Lens.Lens' GetJourneyExecutionActivityMetrics (Lude.Maybe Lude.Text)
gjeamNextToken = Lens.lens (nextToken :: GetJourneyExecutionActivityMetrics -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetJourneyExecutionActivityMetrics)
{-# DEPRECATED gjeamNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjeamApplicationId :: Lens.Lens' GetJourneyExecutionActivityMetrics Lude.Text
gjeamApplicationId = Lens.lens (applicationId :: GetJourneyExecutionActivityMetrics -> Lude.Text) (\s a -> s {applicationId = a} :: GetJourneyExecutionActivityMetrics)
{-# DEPRECATED gjeamApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique identifier for the journey.
--
-- /Note:/ Consider using 'journeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjeamJourneyId :: Lens.Lens' GetJourneyExecutionActivityMetrics Lude.Text
gjeamJourneyId = Lens.lens (journeyId :: GetJourneyExecutionActivityMetrics -> Lude.Text) (\s a -> s {journeyId = a} :: GetJourneyExecutionActivityMetrics)
{-# DEPRECATED gjeamJourneyId "Use generic-lens or generic-optics with 'journeyId' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjeamPageSize :: Lens.Lens' GetJourneyExecutionActivityMetrics (Lude.Maybe Lude.Text)
gjeamPageSize = Lens.lens (pageSize :: GetJourneyExecutionActivityMetrics -> Lude.Maybe Lude.Text) (\s a -> s {pageSize = a} :: GetJourneyExecutionActivityMetrics)
{-# DEPRECATED gjeamPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Lude.AWSRequest GetJourneyExecutionActivityMetrics where
  type
    Rs GetJourneyExecutionActivityMetrics =
      GetJourneyExecutionActivityMetricsResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetJourneyExecutionActivityMetricsResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetJourneyExecutionActivityMetrics where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetJourneyExecutionActivityMetrics where
  toPath GetJourneyExecutionActivityMetrics' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/journeys/",
        Lude.toBS journeyId,
        "/activities/",
        Lude.toBS journeyActivityId,
        "/execution-metrics"
      ]

instance Lude.ToQuery GetJourneyExecutionActivityMetrics where
  toQuery GetJourneyExecutionActivityMetrics' {..} =
    Lude.mconcat
      ["next-token" Lude.=: nextToken, "page-size" Lude.=: pageSize]

-- | /See:/ 'mkGetJourneyExecutionActivityMetricsResponse' smart constructor.
data GetJourneyExecutionActivityMetricsResponse = GetJourneyExecutionActivityMetricsResponse'
  { journeyExecutionActivityMetricsResponse :: JourneyExecutionActivityMetricsResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJourneyExecutionActivityMetricsResponse' with the minimum fields required to make a request.
--
-- * 'journeyExecutionActivityMetricsResponse' -
-- * 'responseStatus' - The response status code.
mkGetJourneyExecutionActivityMetricsResponse ::
  -- | 'journeyExecutionActivityMetricsResponse'
  JourneyExecutionActivityMetricsResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetJourneyExecutionActivityMetricsResponse
mkGetJourneyExecutionActivityMetricsResponse
  pJourneyExecutionActivityMetricsResponse_
  pResponseStatus_ =
    GetJourneyExecutionActivityMetricsResponse'
      { journeyExecutionActivityMetricsResponse =
          pJourneyExecutionActivityMetricsResponse_,
        responseStatus = pResponseStatus_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'journeyExecutionActivityMetricsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjeamrsJourneyExecutionActivityMetricsResponse :: Lens.Lens' GetJourneyExecutionActivityMetricsResponse JourneyExecutionActivityMetricsResponse
gjeamrsJourneyExecutionActivityMetricsResponse = Lens.lens (journeyExecutionActivityMetricsResponse :: GetJourneyExecutionActivityMetricsResponse -> JourneyExecutionActivityMetricsResponse) (\s a -> s {journeyExecutionActivityMetricsResponse = a} :: GetJourneyExecutionActivityMetricsResponse)
{-# DEPRECATED gjeamrsJourneyExecutionActivityMetricsResponse "Use generic-lens or generic-optics with 'journeyExecutionActivityMetricsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjeamrsResponseStatus :: Lens.Lens' GetJourneyExecutionActivityMetricsResponse Lude.Int
gjeamrsResponseStatus = Lens.lens (responseStatus :: GetJourneyExecutionActivityMetricsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetJourneyExecutionActivityMetricsResponse)
{-# DEPRECATED gjeamrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
