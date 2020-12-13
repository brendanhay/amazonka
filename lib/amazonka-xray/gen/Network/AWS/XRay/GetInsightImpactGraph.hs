{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetInsightImpactGraph
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a service graph structure filtered by the specified insight. The service graph is limited to only structural information. For a complete service graph, use this API with the GetServiceGraph API.
module Network.AWS.XRay.GetInsightImpactGraph
  ( -- * Creating a request
    GetInsightImpactGraph (..),
    mkGetInsightImpactGraph,

    -- ** Request lenses
    giigStartTime,
    giigInsightId,
    giigNextToken,
    giigEndTime,

    -- * Destructuring the response
    GetInsightImpactGraphResponse (..),
    mkGetInsightImpactGraphResponse,

    -- ** Response lenses
    giigrsServiceGraphStartTime,
    giigrsStartTime,
    giigrsInsightId,
    giigrsNextToken,
    giigrsEndTime,
    giigrsServiceGraphEndTime,
    giigrsServices,
    giigrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkGetInsightImpactGraph' smart constructor.
data GetInsightImpactGraph = GetInsightImpactGraph'
  { -- | The estimated start time of the insight, in Unix time seconds. The StartTime is inclusive of the value provided and can't be more than 30 days old.
    startTime :: Lude.Timestamp,
    -- | The insight's unique identifier. Use the GetInsightSummaries action to retrieve an InsightId.
    insightId :: Lude.Text,
    -- | Specify the pagination token returned by a previous request to retrieve the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The estimated end time of the insight, in Unix time seconds. The EndTime is exclusive of the value provided. The time range between the start time and end time can't be more than six hours.
    endTime :: Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInsightImpactGraph' with the minimum fields required to make a request.
--
-- * 'startTime' - The estimated start time of the insight, in Unix time seconds. The StartTime is inclusive of the value provided and can't be more than 30 days old.
-- * 'insightId' - The insight's unique identifier. Use the GetInsightSummaries action to retrieve an InsightId.
-- * 'nextToken' - Specify the pagination token returned by a previous request to retrieve the next page of results.
-- * 'endTime' - The estimated end time of the insight, in Unix time seconds. The EndTime is exclusive of the value provided. The time range between the start time and end time can't be more than six hours.
mkGetInsightImpactGraph ::
  -- | 'startTime'
  Lude.Timestamp ->
  -- | 'insightId'
  Lude.Text ->
  -- | 'endTime'
  Lude.Timestamp ->
  GetInsightImpactGraph
mkGetInsightImpactGraph pStartTime_ pInsightId_ pEndTime_ =
  GetInsightImpactGraph'
    { startTime = pStartTime_,
      insightId = pInsightId_,
      nextToken = Lude.Nothing,
      endTime = pEndTime_
    }

-- | The estimated start time of the insight, in Unix time seconds. The StartTime is inclusive of the value provided and can't be more than 30 days old.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigStartTime :: Lens.Lens' GetInsightImpactGraph Lude.Timestamp
giigStartTime = Lens.lens (startTime :: GetInsightImpactGraph -> Lude.Timestamp) (\s a -> s {startTime = a} :: GetInsightImpactGraph)
{-# DEPRECATED giigStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The insight's unique identifier. Use the GetInsightSummaries action to retrieve an InsightId.
--
-- /Note:/ Consider using 'insightId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigInsightId :: Lens.Lens' GetInsightImpactGraph Lude.Text
giigInsightId = Lens.lens (insightId :: GetInsightImpactGraph -> Lude.Text) (\s a -> s {insightId = a} :: GetInsightImpactGraph)
{-# DEPRECATED giigInsightId "Use generic-lens or generic-optics with 'insightId' instead." #-}

-- | Specify the pagination token returned by a previous request to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigNextToken :: Lens.Lens' GetInsightImpactGraph (Lude.Maybe Lude.Text)
giigNextToken = Lens.lens (nextToken :: GetInsightImpactGraph -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetInsightImpactGraph)
{-# DEPRECATED giigNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The estimated end time of the insight, in Unix time seconds. The EndTime is exclusive of the value provided. The time range between the start time and end time can't be more than six hours.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigEndTime :: Lens.Lens' GetInsightImpactGraph Lude.Timestamp
giigEndTime = Lens.lens (endTime :: GetInsightImpactGraph -> Lude.Timestamp) (\s a -> s {endTime = a} :: GetInsightImpactGraph)
{-# DEPRECATED giigEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

instance Lude.AWSRequest GetInsightImpactGraph where
  type Rs GetInsightImpactGraph = GetInsightImpactGraphResponse
  request = Req.postJSON xRayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetInsightImpactGraphResponse'
            Lude.<$> (x Lude..?> "ServiceGraphStartTime")
            Lude.<*> (x Lude..?> "StartTime")
            Lude.<*> (x Lude..?> "InsightId")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "EndTime")
            Lude.<*> (x Lude..?> "ServiceGraphEndTime")
            Lude.<*> (x Lude..?> "Services" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetInsightImpactGraph where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetInsightImpactGraph where
  toJSON GetInsightImpactGraph' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("StartTime" Lude..= startTime),
            Lude.Just ("InsightId" Lude..= insightId),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("EndTime" Lude..= endTime)
          ]
      )

instance Lude.ToPath GetInsightImpactGraph where
  toPath = Lude.const "/InsightImpactGraph"

instance Lude.ToQuery GetInsightImpactGraph where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetInsightImpactGraphResponse' smart constructor.
data GetInsightImpactGraphResponse = GetInsightImpactGraphResponse'
  { -- | The time, in Unix seconds, at which the service graph started.
    serviceGraphStartTime :: Lude.Maybe Lude.Timestamp,
    -- | The provided start time.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | The insight's unique identifier.
    insightId :: Lude.Maybe Lude.Text,
    -- | Pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The provided end time.
    endTime :: Lude.Maybe Lude.Timestamp,
    -- | The time, in Unix seconds, at which the service graph ended.
    serviceGraphEndTime :: Lude.Maybe Lude.Timestamp,
    -- | The AWS instrumented services related to the insight.
    services :: Lude.Maybe [InsightImpactGraphService],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInsightImpactGraphResponse' with the minimum fields required to make a request.
--
-- * 'serviceGraphStartTime' - The time, in Unix seconds, at which the service graph started.
-- * 'startTime' - The provided start time.
-- * 'insightId' - The insight's unique identifier.
-- * 'nextToken' - Pagination token.
-- * 'endTime' - The provided end time.
-- * 'serviceGraphEndTime' - The time, in Unix seconds, at which the service graph ended.
-- * 'services' - The AWS instrumented services related to the insight.
-- * 'responseStatus' - The response status code.
mkGetInsightImpactGraphResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetInsightImpactGraphResponse
mkGetInsightImpactGraphResponse pResponseStatus_ =
  GetInsightImpactGraphResponse'
    { serviceGraphStartTime =
        Lude.Nothing,
      startTime = Lude.Nothing,
      insightId = Lude.Nothing,
      nextToken = Lude.Nothing,
      endTime = Lude.Nothing,
      serviceGraphEndTime = Lude.Nothing,
      services = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The time, in Unix seconds, at which the service graph started.
--
-- /Note:/ Consider using 'serviceGraphStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigrsServiceGraphStartTime :: Lens.Lens' GetInsightImpactGraphResponse (Lude.Maybe Lude.Timestamp)
giigrsServiceGraphStartTime = Lens.lens (serviceGraphStartTime :: GetInsightImpactGraphResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {serviceGraphStartTime = a} :: GetInsightImpactGraphResponse)
{-# DEPRECATED giigrsServiceGraphStartTime "Use generic-lens or generic-optics with 'serviceGraphStartTime' instead." #-}

-- | The provided start time.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigrsStartTime :: Lens.Lens' GetInsightImpactGraphResponse (Lude.Maybe Lude.Timestamp)
giigrsStartTime = Lens.lens (startTime :: GetInsightImpactGraphResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: GetInsightImpactGraphResponse)
{-# DEPRECATED giigrsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The insight's unique identifier.
--
-- /Note:/ Consider using 'insightId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigrsInsightId :: Lens.Lens' GetInsightImpactGraphResponse (Lude.Maybe Lude.Text)
giigrsInsightId = Lens.lens (insightId :: GetInsightImpactGraphResponse -> Lude.Maybe Lude.Text) (\s a -> s {insightId = a} :: GetInsightImpactGraphResponse)
{-# DEPRECATED giigrsInsightId "Use generic-lens or generic-optics with 'insightId' instead." #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigrsNextToken :: Lens.Lens' GetInsightImpactGraphResponse (Lude.Maybe Lude.Text)
giigrsNextToken = Lens.lens (nextToken :: GetInsightImpactGraphResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetInsightImpactGraphResponse)
{-# DEPRECATED giigrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The provided end time.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigrsEndTime :: Lens.Lens' GetInsightImpactGraphResponse (Lude.Maybe Lude.Timestamp)
giigrsEndTime = Lens.lens (endTime :: GetInsightImpactGraphResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: GetInsightImpactGraphResponse)
{-# DEPRECATED giigrsEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The time, in Unix seconds, at which the service graph ended.
--
-- /Note:/ Consider using 'serviceGraphEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigrsServiceGraphEndTime :: Lens.Lens' GetInsightImpactGraphResponse (Lude.Maybe Lude.Timestamp)
giigrsServiceGraphEndTime = Lens.lens (serviceGraphEndTime :: GetInsightImpactGraphResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {serviceGraphEndTime = a} :: GetInsightImpactGraphResponse)
{-# DEPRECATED giigrsServiceGraphEndTime "Use generic-lens or generic-optics with 'serviceGraphEndTime' instead." #-}

-- | The AWS instrumented services related to the insight.
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigrsServices :: Lens.Lens' GetInsightImpactGraphResponse (Lude.Maybe [InsightImpactGraphService])
giigrsServices = Lens.lens (services :: GetInsightImpactGraphResponse -> Lude.Maybe [InsightImpactGraphService]) (\s a -> s {services = a} :: GetInsightImpactGraphResponse)
{-# DEPRECATED giigrsServices "Use generic-lens or generic-optics with 'services' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigrsResponseStatus :: Lens.Lens' GetInsightImpactGraphResponse Lude.Int
giigrsResponseStatus = Lens.lens (responseStatus :: GetInsightImpactGraphResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetInsightImpactGraphResponse)
{-# DEPRECATED giigrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
