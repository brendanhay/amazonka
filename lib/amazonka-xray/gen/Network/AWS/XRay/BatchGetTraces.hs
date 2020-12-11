{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.BatchGetTraces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of traces specified by ID. Each trace is a collection of segment documents that originates from a single request. Use @GetTraceSummaries@ to get a list of trace IDs.
--
-- This operation returns paginated results.
module Network.AWS.XRay.BatchGetTraces
  ( -- * Creating a request
    BatchGetTraces (..),
    mkBatchGetTraces,

    -- ** Request lenses
    bgtNextToken,
    bgtTraceIds,

    -- * Destructuring the response
    BatchGetTracesResponse (..),
    mkBatchGetTracesResponse,

    -- ** Response lenses
    bgtrsNextToken,
    bgtrsTraces,
    bgtrsUnprocessedTraceIds,
    bgtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkBatchGetTraces' smart constructor.
data BatchGetTraces = BatchGetTraces'
  { nextToken ::
      Lude.Maybe Lude.Text,
    traceIds :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetTraces' with the minimum fields required to make a request.
--
-- * 'nextToken' - Pagination token.
-- * 'traceIds' - Specify the trace IDs of requests for which to retrieve segments.
mkBatchGetTraces ::
  BatchGetTraces
mkBatchGetTraces =
  BatchGetTraces' {nextToken = Lude.Nothing, traceIds = Lude.mempty}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgtNextToken :: Lens.Lens' BatchGetTraces (Lude.Maybe Lude.Text)
bgtNextToken = Lens.lens (nextToken :: BatchGetTraces -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchGetTraces)
{-# DEPRECATED bgtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specify the trace IDs of requests for which to retrieve segments.
--
-- /Note:/ Consider using 'traceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgtTraceIds :: Lens.Lens' BatchGetTraces [Lude.Text]
bgtTraceIds = Lens.lens (traceIds :: BatchGetTraces -> [Lude.Text]) (\s a -> s {traceIds = a} :: BatchGetTraces)
{-# DEPRECATED bgtTraceIds "Use generic-lens or generic-optics with 'traceIds' instead." #-}

instance Page.AWSPager BatchGetTraces where
  page rq rs
    | Page.stop (rs Lens.^. bgtrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. bgtrsTraces) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& bgtNextToken Lens..~ rs Lens.^. bgtrsNextToken

instance Lude.AWSRequest BatchGetTraces where
  type Rs BatchGetTraces = BatchGetTracesResponse
  request = Req.postJSON xRayService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetTracesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Traces" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "UnprocessedTraceIds" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetTraces where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON BatchGetTraces where
  toJSON BatchGetTraces' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("TraceIds" Lude..= traceIds)
          ]
      )

instance Lude.ToPath BatchGetTraces where
  toPath = Lude.const "/Traces"

instance Lude.ToQuery BatchGetTraces where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchGetTracesResponse' smart constructor.
data BatchGetTracesResponse = BatchGetTracesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    traces :: Lude.Maybe [Trace],
    unprocessedTraceIds :: Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'BatchGetTracesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Pagination token.
-- * 'responseStatus' - The response status code.
-- * 'traces' - Full traces for the specified requests.
-- * 'unprocessedTraceIds' - Trace IDs of requests that haven't been processed.
mkBatchGetTracesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetTracesResponse
mkBatchGetTracesResponse pResponseStatus_ =
  BatchGetTracesResponse'
    { nextToken = Lude.Nothing,
      traces = Lude.Nothing,
      unprocessedTraceIds = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgtrsNextToken :: Lens.Lens' BatchGetTracesResponse (Lude.Maybe Lude.Text)
bgtrsNextToken = Lens.lens (nextToken :: BatchGetTracesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchGetTracesResponse)
{-# DEPRECATED bgtrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Full traces for the specified requests.
--
-- /Note:/ Consider using 'traces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgtrsTraces :: Lens.Lens' BatchGetTracesResponse (Lude.Maybe [Trace])
bgtrsTraces = Lens.lens (traces :: BatchGetTracesResponse -> Lude.Maybe [Trace]) (\s a -> s {traces = a} :: BatchGetTracesResponse)
{-# DEPRECATED bgtrsTraces "Use generic-lens or generic-optics with 'traces' instead." #-}

-- | Trace IDs of requests that haven't been processed.
--
-- /Note:/ Consider using 'unprocessedTraceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgtrsUnprocessedTraceIds :: Lens.Lens' BatchGetTracesResponse (Lude.Maybe [Lude.Text])
bgtrsUnprocessedTraceIds = Lens.lens (unprocessedTraceIds :: BatchGetTracesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {unprocessedTraceIds = a} :: BatchGetTracesResponse)
{-# DEPRECATED bgtrsUnprocessedTraceIds "Use generic-lens or generic-optics with 'unprocessedTraceIds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgtrsResponseStatus :: Lens.Lens' BatchGetTracesResponse Lude.Int
bgtrsResponseStatus = Lens.lens (responseStatus :: BatchGetTracesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetTracesResponse)
{-# DEPRECATED bgtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
