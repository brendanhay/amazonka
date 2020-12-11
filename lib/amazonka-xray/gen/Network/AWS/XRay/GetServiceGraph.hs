{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetServiceGraph
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a document that describes services that process incoming requests, and downstream services that they call as a result. Root services process incoming requests and make calls to downstream services. Root services are applications that use the <https://docs.aws.amazon.com/xray/index.html AWS X-Ray SDK> . Downstream services can be other applications, AWS resources, HTTP web APIs, or SQL databases.
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetServiceGraph
  ( -- * Creating a request
    GetServiceGraph (..),
    mkGetServiceGraph,

    -- ** Request lenses
    gsgNextToken,
    gsgGroupARN,
    gsgGroupName,
    gsgStartTime,
    gsgEndTime,

    -- * Destructuring the response
    GetServiceGraphResponse (..),
    mkGetServiceGraphResponse,

    -- ** Response lenses
    gsgrsContainsOldGroupVersions,
    gsgrsStartTime,
    gsgrsNextToken,
    gsgrsEndTime,
    gsgrsServices,
    gsgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkGetServiceGraph' smart constructor.
data GetServiceGraph = GetServiceGraph'
  { nextToken ::
      Lude.Maybe Lude.Text,
    groupARN :: Lude.Maybe Lude.Text,
    groupName :: Lude.Maybe Lude.Text,
    startTime :: Lude.Timestamp,
    endTime :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetServiceGraph' with the minimum fields required to make a request.
--
-- * 'endTime' - The end of the timeframe for which to generate a graph.
-- * 'groupARN' - The Amazon Resource Name (ARN) of a group based on which you want to generate a graph.
-- * 'groupName' - The name of a group based on which you want to generate a graph.
-- * 'nextToken' - Pagination token.
-- * 'startTime' - The start of the time frame for which to generate a graph.
mkGetServiceGraph ::
  -- | 'startTime'
  Lude.Timestamp ->
  -- | 'endTime'
  Lude.Timestamp ->
  GetServiceGraph
mkGetServiceGraph pStartTime_ pEndTime_ =
  GetServiceGraph'
    { nextToken = Lude.Nothing,
      groupARN = Lude.Nothing,
      groupName = Lude.Nothing,
      startTime = pStartTime_,
      endTime = pEndTime_
    }

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgNextToken :: Lens.Lens' GetServiceGraph (Lude.Maybe Lude.Text)
gsgNextToken = Lens.lens (nextToken :: GetServiceGraph -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetServiceGraph)
{-# DEPRECATED gsgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The Amazon Resource Name (ARN) of a group based on which you want to generate a graph.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgGroupARN :: Lens.Lens' GetServiceGraph (Lude.Maybe Lude.Text)
gsgGroupARN = Lens.lens (groupARN :: GetServiceGraph -> Lude.Maybe Lude.Text) (\s a -> s {groupARN = a} :: GetServiceGraph)
{-# DEPRECATED gsgGroupARN "Use generic-lens or generic-optics with 'groupARN' instead." #-}

-- | The name of a group based on which you want to generate a graph.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgGroupName :: Lens.Lens' GetServiceGraph (Lude.Maybe Lude.Text)
gsgGroupName = Lens.lens (groupName :: GetServiceGraph -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: GetServiceGraph)
{-# DEPRECATED gsgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The start of the time frame for which to generate a graph.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgStartTime :: Lens.Lens' GetServiceGraph Lude.Timestamp
gsgStartTime = Lens.lens (startTime :: GetServiceGraph -> Lude.Timestamp) (\s a -> s {startTime = a} :: GetServiceGraph)
{-# DEPRECATED gsgStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The end of the timeframe for which to generate a graph.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgEndTime :: Lens.Lens' GetServiceGraph Lude.Timestamp
gsgEndTime = Lens.lens (endTime :: GetServiceGraph -> Lude.Timestamp) (\s a -> s {endTime = a} :: GetServiceGraph)
{-# DEPRECATED gsgEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

instance Page.AWSPager GetServiceGraph where
  page rq rs
    | Page.stop (rs Lens.^. gsgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gsgrsServices) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gsgNextToken Lens..~ rs Lens.^. gsgrsNextToken

instance Lude.AWSRequest GetServiceGraph where
  type Rs GetServiceGraph = GetServiceGraphResponse
  request = Req.postJSON xRayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetServiceGraphResponse'
            Lude.<$> (x Lude..?> "ContainsOldGroupVersions")
            Lude.<*> (x Lude..?> "StartTime")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "EndTime")
            Lude.<*> (x Lude..?> "Services" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetServiceGraph where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetServiceGraph where
  toJSON GetServiceGraph' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("GroupARN" Lude..=) Lude.<$> groupARN,
            ("GroupName" Lude..=) Lude.<$> groupName,
            Lude.Just ("StartTime" Lude..= startTime),
            Lude.Just ("EndTime" Lude..= endTime)
          ]
      )

instance Lude.ToPath GetServiceGraph where
  toPath = Lude.const "/ServiceGraph"

instance Lude.ToQuery GetServiceGraph where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetServiceGraphResponse' smart constructor.
data GetServiceGraphResponse = GetServiceGraphResponse'
  { containsOldGroupVersions ::
      Lude.Maybe Lude.Bool,
    startTime :: Lude.Maybe Lude.Timestamp,
    nextToken :: Lude.Maybe Lude.Text,
    endTime :: Lude.Maybe Lude.Timestamp,
    services :: Lude.Maybe [ServiceInfo],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetServiceGraphResponse' with the minimum fields required to make a request.
--
-- * 'containsOldGroupVersions' - A flag indicating whether the group's filter expression has been consistent, or if the returned service graph may show traces from an older version of the group's filter expression.
-- * 'endTime' - The end of the time frame for which the graph was generated.
-- * 'nextToken' - Pagination token.
-- * 'responseStatus' - The response status code.
-- * 'services' - The services that have processed a traced request during the specified time frame.
-- * 'startTime' - The start of the time frame for which the graph was generated.
mkGetServiceGraphResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetServiceGraphResponse
mkGetServiceGraphResponse pResponseStatus_ =
  GetServiceGraphResponse'
    { containsOldGroupVersions = Lude.Nothing,
      startTime = Lude.Nothing,
      nextToken = Lude.Nothing,
      endTime = Lude.Nothing,
      services = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A flag indicating whether the group's filter expression has been consistent, or if the returned service graph may show traces from an older version of the group's filter expression.
--
-- /Note:/ Consider using 'containsOldGroupVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgrsContainsOldGroupVersions :: Lens.Lens' GetServiceGraphResponse (Lude.Maybe Lude.Bool)
gsgrsContainsOldGroupVersions = Lens.lens (containsOldGroupVersions :: GetServiceGraphResponse -> Lude.Maybe Lude.Bool) (\s a -> s {containsOldGroupVersions = a} :: GetServiceGraphResponse)
{-# DEPRECATED gsgrsContainsOldGroupVersions "Use generic-lens or generic-optics with 'containsOldGroupVersions' instead." #-}

-- | The start of the time frame for which the graph was generated.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgrsStartTime :: Lens.Lens' GetServiceGraphResponse (Lude.Maybe Lude.Timestamp)
gsgrsStartTime = Lens.lens (startTime :: GetServiceGraphResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: GetServiceGraphResponse)
{-# DEPRECATED gsgrsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgrsNextToken :: Lens.Lens' GetServiceGraphResponse (Lude.Maybe Lude.Text)
gsgrsNextToken = Lens.lens (nextToken :: GetServiceGraphResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetServiceGraphResponse)
{-# DEPRECATED gsgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The end of the time frame for which the graph was generated.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgrsEndTime :: Lens.Lens' GetServiceGraphResponse (Lude.Maybe Lude.Timestamp)
gsgrsEndTime = Lens.lens (endTime :: GetServiceGraphResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: GetServiceGraphResponse)
{-# DEPRECATED gsgrsEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The services that have processed a traced request during the specified time frame.
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgrsServices :: Lens.Lens' GetServiceGraphResponse (Lude.Maybe [ServiceInfo])
gsgrsServices = Lens.lens (services :: GetServiceGraphResponse -> Lude.Maybe [ServiceInfo]) (\s a -> s {services = a} :: GetServiceGraphResponse)
{-# DEPRECATED gsgrsServices "Use generic-lens or generic-optics with 'services' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgrsResponseStatus :: Lens.Lens' GetServiceGraphResponse Lude.Int
gsgrsResponseStatus = Lens.lens (responseStatus :: GetServiceGraphResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetServiceGraphResponse)
{-# DEPRECATED gsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
