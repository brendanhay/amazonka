{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetJourneyExecutionMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) pre-aggregated data for a standard execution metric that applies to a journey.
module Network.AWS.Pinpoint.GetJourneyExecutionMetrics
  ( -- * Creating a request
    GetJourneyExecutionMetrics (..),
    mkGetJourneyExecutionMetrics,

    -- ** Request lenses
    gjemNextToken,
    gjemApplicationId,
    gjemJourneyId,
    gjemPageSize,

    -- * Destructuring the response
    GetJourneyExecutionMetricsResponse (..),
    mkGetJourneyExecutionMetricsResponse,

    -- ** Response lenses
    gjemrsJourneyExecutionMetricsResponse,
    gjemrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetJourneyExecutionMetrics' smart constructor.
data GetJourneyExecutionMetrics = GetJourneyExecutionMetrics'
  { -- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
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

-- | Creates a value of 'GetJourneyExecutionMetrics' with the minimum fields required to make a request.
--
-- * 'nextToken' - The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'journeyId' - The unique identifier for the journey.
-- * 'pageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
mkGetJourneyExecutionMetrics ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'journeyId'
  Lude.Text ->
  GetJourneyExecutionMetrics
mkGetJourneyExecutionMetrics pApplicationId_ pJourneyId_ =
  GetJourneyExecutionMetrics'
    { nextToken = Lude.Nothing,
      applicationId = pApplicationId_,
      journeyId = pJourneyId_,
      pageSize = Lude.Nothing
    }

-- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjemNextToken :: Lens.Lens' GetJourneyExecutionMetrics (Lude.Maybe Lude.Text)
gjemNextToken = Lens.lens (nextToken :: GetJourneyExecutionMetrics -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetJourneyExecutionMetrics)
{-# DEPRECATED gjemNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjemApplicationId :: Lens.Lens' GetJourneyExecutionMetrics Lude.Text
gjemApplicationId = Lens.lens (applicationId :: GetJourneyExecutionMetrics -> Lude.Text) (\s a -> s {applicationId = a} :: GetJourneyExecutionMetrics)
{-# DEPRECATED gjemApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique identifier for the journey.
--
-- /Note:/ Consider using 'journeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjemJourneyId :: Lens.Lens' GetJourneyExecutionMetrics Lude.Text
gjemJourneyId = Lens.lens (journeyId :: GetJourneyExecutionMetrics -> Lude.Text) (\s a -> s {journeyId = a} :: GetJourneyExecutionMetrics)
{-# DEPRECATED gjemJourneyId "Use generic-lens or generic-optics with 'journeyId' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjemPageSize :: Lens.Lens' GetJourneyExecutionMetrics (Lude.Maybe Lude.Text)
gjemPageSize = Lens.lens (pageSize :: GetJourneyExecutionMetrics -> Lude.Maybe Lude.Text) (\s a -> s {pageSize = a} :: GetJourneyExecutionMetrics)
{-# DEPRECATED gjemPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Lude.AWSRequest GetJourneyExecutionMetrics where
  type
    Rs GetJourneyExecutionMetrics =
      GetJourneyExecutionMetricsResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetJourneyExecutionMetricsResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetJourneyExecutionMetrics where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetJourneyExecutionMetrics where
  toPath GetJourneyExecutionMetrics' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/journeys/",
        Lude.toBS journeyId,
        "/execution-metrics"
      ]

instance Lude.ToQuery GetJourneyExecutionMetrics where
  toQuery GetJourneyExecutionMetrics' {..} =
    Lude.mconcat
      ["next-token" Lude.=: nextToken, "page-size" Lude.=: pageSize]

-- | /See:/ 'mkGetJourneyExecutionMetricsResponse' smart constructor.
data GetJourneyExecutionMetricsResponse = GetJourneyExecutionMetricsResponse'
  { journeyExecutionMetricsResponse :: JourneyExecutionMetricsResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJourneyExecutionMetricsResponse' with the minimum fields required to make a request.
--
-- * 'journeyExecutionMetricsResponse' -
-- * 'responseStatus' - The response status code.
mkGetJourneyExecutionMetricsResponse ::
  -- | 'journeyExecutionMetricsResponse'
  JourneyExecutionMetricsResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetJourneyExecutionMetricsResponse
mkGetJourneyExecutionMetricsResponse
  pJourneyExecutionMetricsResponse_
  pResponseStatus_ =
    GetJourneyExecutionMetricsResponse'
      { journeyExecutionMetricsResponse =
          pJourneyExecutionMetricsResponse_,
        responseStatus = pResponseStatus_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'journeyExecutionMetricsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjemrsJourneyExecutionMetricsResponse :: Lens.Lens' GetJourneyExecutionMetricsResponse JourneyExecutionMetricsResponse
gjemrsJourneyExecutionMetricsResponse = Lens.lens (journeyExecutionMetricsResponse :: GetJourneyExecutionMetricsResponse -> JourneyExecutionMetricsResponse) (\s a -> s {journeyExecutionMetricsResponse = a} :: GetJourneyExecutionMetricsResponse)
{-# DEPRECATED gjemrsJourneyExecutionMetricsResponse "Use generic-lens or generic-optics with 'journeyExecutionMetricsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjemrsResponseStatus :: Lens.Lens' GetJourneyExecutionMetricsResponse Lude.Int
gjemrsResponseStatus = Lens.lens (responseStatus :: GetJourneyExecutionMetricsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetJourneyExecutionMetricsResponse)
{-# DEPRECATED gjemrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
