{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      BatchGetTraces (..)
    , mkBatchGetTraces
    -- ** Request lenses
    , bgtTraceIds
    , bgtNextToken

    -- * Destructuring the response
    , BatchGetTracesResponse (..)
    , mkBatchGetTracesResponse
    -- ** Response lenses
    , bgtrrsNextToken
    , bgtrrsTraces
    , bgtrrsUnprocessedTraceIds
    , bgtrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkBatchGetTraces' smart constructor.
data BatchGetTraces = BatchGetTraces'
  { traceIds :: [Types.TraceId]
    -- ^ Specify the trace IDs of requests for which to retrieve segments.
  , nextToken :: Core.Maybe Core.Text
    -- ^ Pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetTraces' value with any optional fields omitted.
mkBatchGetTraces
    :: BatchGetTraces
mkBatchGetTraces
  = BatchGetTraces'{traceIds = Core.mempty, nextToken = Core.Nothing}

-- | Specify the trace IDs of requests for which to retrieve segments.
--
-- /Note:/ Consider using 'traceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgtTraceIds :: Lens.Lens' BatchGetTraces [Types.TraceId]
bgtTraceIds = Lens.field @"traceIds"
{-# INLINEABLE bgtTraceIds #-}
{-# DEPRECATED traceIds "Use generic-lens or generic-optics with 'traceIds' instead"  #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgtNextToken :: Lens.Lens' BatchGetTraces (Core.Maybe Core.Text)
bgtNextToken = Lens.field @"nextToken"
{-# INLINEABLE bgtNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery BatchGetTraces where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchGetTraces where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON BatchGetTraces where
        toJSON BatchGetTraces{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TraceIds" Core..= traceIds),
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest BatchGetTraces where
        type Rs BatchGetTraces = BatchGetTracesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/Traces",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchGetTracesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Traces" Core.<*>
                     x Core..:? "UnprocessedTraceIds"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager BatchGetTraces where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"traces" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkBatchGetTracesResponse' smart constructor.
data BatchGetTracesResponse = BatchGetTracesResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ Pagination token.
  , traces :: Core.Maybe [Types.Trace]
    -- ^ Full traces for the specified requests.
  , unprocessedTraceIds :: Core.Maybe [Types.TraceId]
    -- ^ Trace IDs of requests that haven't been processed.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetTracesResponse' value with any optional fields omitted.
mkBatchGetTracesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchGetTracesResponse
mkBatchGetTracesResponse responseStatus
  = BatchGetTracesResponse'{nextToken = Core.Nothing,
                            traces = Core.Nothing, unprocessedTraceIds = Core.Nothing,
                            responseStatus}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgtrrsNextToken :: Lens.Lens' BatchGetTracesResponse (Core.Maybe Core.Text)
bgtrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE bgtrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Full traces for the specified requests.
--
-- /Note:/ Consider using 'traces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgtrrsTraces :: Lens.Lens' BatchGetTracesResponse (Core.Maybe [Types.Trace])
bgtrrsTraces = Lens.field @"traces"
{-# INLINEABLE bgtrrsTraces #-}
{-# DEPRECATED traces "Use generic-lens or generic-optics with 'traces' instead"  #-}

-- | Trace IDs of requests that haven't been processed.
--
-- /Note:/ Consider using 'unprocessedTraceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgtrrsUnprocessedTraceIds :: Lens.Lens' BatchGetTracesResponse (Core.Maybe [Types.TraceId])
bgtrrsUnprocessedTraceIds = Lens.field @"unprocessedTraceIds"
{-# INLINEABLE bgtrrsUnprocessedTraceIds #-}
{-# DEPRECATED unprocessedTraceIds "Use generic-lens or generic-optics with 'unprocessedTraceIds' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgtrrsResponseStatus :: Lens.Lens' BatchGetTracesResponse Core.Int
bgtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bgtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
