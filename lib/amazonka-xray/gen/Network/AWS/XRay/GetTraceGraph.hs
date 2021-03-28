{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetTraceGraph (..)
    , mkGetTraceGraph
    -- ** Request lenses
    , gtgTraceIds
    , gtgNextToken

    -- * Destructuring the response
    , GetTraceGraphResponse (..)
    , mkGetTraceGraphResponse
    -- ** Response lenses
    , gtgrrsNextToken
    , gtgrrsServices
    , gtgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkGetTraceGraph' smart constructor.
data GetTraceGraph = GetTraceGraph'
  { traceIds :: [Types.TraceId]
    -- ^ Trace IDs of requests for which to generate a service graph.
  , nextToken :: Core.Maybe Core.Text
    -- ^ Pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTraceGraph' value with any optional fields omitted.
mkGetTraceGraph
    :: GetTraceGraph
mkGetTraceGraph
  = GetTraceGraph'{traceIds = Core.mempty, nextToken = Core.Nothing}

-- | Trace IDs of requests for which to generate a service graph.
--
-- /Note:/ Consider using 'traceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgTraceIds :: Lens.Lens' GetTraceGraph [Types.TraceId]
gtgTraceIds = Lens.field @"traceIds"
{-# INLINEABLE gtgTraceIds #-}
{-# DEPRECATED traceIds "Use generic-lens or generic-optics with 'traceIds' instead"  #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgNextToken :: Lens.Lens' GetTraceGraph (Core.Maybe Core.Text)
gtgNextToken = Lens.field @"nextToken"
{-# INLINEABLE gtgNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetTraceGraph where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetTraceGraph where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON GetTraceGraph where
        toJSON GetTraceGraph{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TraceIds" Core..= traceIds),
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetTraceGraph where
        type Rs GetTraceGraph = GetTraceGraphResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/TraceGraph",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetTraceGraphResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Services" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetTraceGraph where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"services" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetTraceGraphResponse' smart constructor.
data GetTraceGraphResponse = GetTraceGraphResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ Pagination token.
  , services :: Core.Maybe [Types.ServiceInfo]
    -- ^ The services that have processed one of the specified requests.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetTraceGraphResponse' value with any optional fields omitted.
mkGetTraceGraphResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTraceGraphResponse
mkGetTraceGraphResponse responseStatus
  = GetTraceGraphResponse'{nextToken = Core.Nothing,
                           services = Core.Nothing, responseStatus}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrrsNextToken :: Lens.Lens' GetTraceGraphResponse (Core.Maybe Core.Text)
gtgrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gtgrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The services that have processed one of the specified requests.
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrrsServices :: Lens.Lens' GetTraceGraphResponse (Core.Maybe [Types.ServiceInfo])
gtgrrsServices = Lens.field @"services"
{-# INLINEABLE gtgrrsServices #-}
{-# DEPRECATED services "Use generic-lens or generic-optics with 'services' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrrsResponseStatus :: Lens.Lens' GetTraceGraphResponse Core.Int
gtgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
