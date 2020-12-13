{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetTraceGraph
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a service graph for one or more specific trace IDs.
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetTraceGraph
  ( -- * Creating a request
    GetTraceGraph (..),
    mkGetTraceGraph,

    -- ** Request lenses
    gtgNextToken,
    gtgTraceIds,

    -- * Destructuring the response
    GetTraceGraphResponse (..),
    mkGetTraceGraphResponse,

    -- ** Response lenses
    gtgrsNextToken,
    gtgrsServices,
    gtgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkGetTraceGraph' smart constructor.
data GetTraceGraph = GetTraceGraph'
  { -- | Pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Trace IDs of requests for which to generate a service graph.
    traceIds :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTraceGraph' with the minimum fields required to make a request.
--
-- * 'nextToken' - Pagination token.
-- * 'traceIds' - Trace IDs of requests for which to generate a service graph.
mkGetTraceGraph ::
  GetTraceGraph
mkGetTraceGraph =
  GetTraceGraph' {nextToken = Lude.Nothing, traceIds = Lude.mempty}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgNextToken :: Lens.Lens' GetTraceGraph (Lude.Maybe Lude.Text)
gtgNextToken = Lens.lens (nextToken :: GetTraceGraph -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTraceGraph)
{-# DEPRECATED gtgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Trace IDs of requests for which to generate a service graph.
--
-- /Note:/ Consider using 'traceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgTraceIds :: Lens.Lens' GetTraceGraph [Lude.Text]
gtgTraceIds = Lens.lens (traceIds :: GetTraceGraph -> [Lude.Text]) (\s a -> s {traceIds = a} :: GetTraceGraph)
{-# DEPRECATED gtgTraceIds "Use generic-lens or generic-optics with 'traceIds' instead." #-}

instance Page.AWSPager GetTraceGraph where
  page rq rs
    | Page.stop (rs Lens.^. gtgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gtgrsServices) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gtgNextToken Lens..~ rs Lens.^. gtgrsNextToken

instance Lude.AWSRequest GetTraceGraph where
  type Rs GetTraceGraph = GetTraceGraphResponse
  request = Req.postJSON xRayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTraceGraphResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Services" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTraceGraph where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetTraceGraph where
  toJSON GetTraceGraph' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("TraceIds" Lude..= traceIds)
          ]
      )

instance Lude.ToPath GetTraceGraph where
  toPath = Lude.const "/TraceGraph"

instance Lude.ToQuery GetTraceGraph where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTraceGraphResponse' smart constructor.
data GetTraceGraphResponse = GetTraceGraphResponse'
  { -- | Pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The services that have processed one of the specified requests.
    services :: Lude.Maybe [ServiceInfo],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTraceGraphResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Pagination token.
-- * 'services' - The services that have processed one of the specified requests.
-- * 'responseStatus' - The response status code.
mkGetTraceGraphResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTraceGraphResponse
mkGetTraceGraphResponse pResponseStatus_ =
  GetTraceGraphResponse'
    { nextToken = Lude.Nothing,
      services = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrsNextToken :: Lens.Lens' GetTraceGraphResponse (Lude.Maybe Lude.Text)
gtgrsNextToken = Lens.lens (nextToken :: GetTraceGraphResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTraceGraphResponse)
{-# DEPRECATED gtgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The services that have processed one of the specified requests.
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrsServices :: Lens.Lens' GetTraceGraphResponse (Lude.Maybe [ServiceInfo])
gtgrsServices = Lens.lens (services :: GetTraceGraphResponse -> Lude.Maybe [ServiceInfo]) (\s a -> s {services = a} :: GetTraceGraphResponse)
{-# DEPRECATED gtgrsServices "Use generic-lens or generic-optics with 'services' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrsResponseStatus :: Lens.Lens' GetTraceGraphResponse Lude.Int
gtgrsResponseStatus = Lens.lens (responseStatus :: GetTraceGraphResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTraceGraphResponse)
{-# DEPRECATED gtgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
