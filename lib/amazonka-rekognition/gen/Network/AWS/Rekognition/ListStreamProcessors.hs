{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.ListStreamProcessors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of stream processors that you have created with 'CreateStreamProcessor' . 
--
-- This operation returns paginated results.
module Network.AWS.Rekognition.ListStreamProcessors
    (
    -- * Creating a request
      ListStreamProcessors (..)
    , mkListStreamProcessors
    -- ** Request lenses
    , lspMaxResults
    , lspNextToken

    -- * Destructuring the response
    , ListStreamProcessorsResponse (..)
    , mkListStreamProcessorsResponse
    -- ** Response lenses
    , lsprrsNextToken
    , lsprrsStreamProcessors
    , lsprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListStreamProcessors' smart constructor.
data ListStreamProcessors = ListStreamProcessors'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ Maximum number of stream processors you want Amazon Rekognition Video to return in the response. The default is 1000. 
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If the previous response was incomplete (because there are more stream processors to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of stream processors. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListStreamProcessors' value with any optional fields omitted.
mkListStreamProcessors
    :: ListStreamProcessors
mkListStreamProcessors
  = ListStreamProcessors'{maxResults = Core.Nothing,
                          nextToken = Core.Nothing}

-- | Maximum number of stream processors you want Amazon Rekognition Video to return in the response. The default is 1000. 
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspMaxResults :: Lens.Lens' ListStreamProcessors (Core.Maybe Core.Natural)
lspMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lspMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If the previous response was incomplete (because there are more stream processors to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of stream processors. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspNextToken :: Lens.Lens' ListStreamProcessors (Core.Maybe Types.PaginationToken)
lspNextToken = Lens.field @"nextToken"
{-# INLINEABLE lspNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListStreamProcessors where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListStreamProcessors where
        toHeaders ListStreamProcessors{..}
          = Core.pure
              ("X-Amz-Target", "RekognitionService.ListStreamProcessors")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListStreamProcessors where
        toJSON ListStreamProcessors{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListStreamProcessors where
        type Rs ListStreamProcessors = ListStreamProcessorsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListStreamProcessorsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "StreamProcessors"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListStreamProcessors where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"streamProcessors" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListStreamProcessorsResponse' smart constructor.
data ListStreamProcessorsResponse = ListStreamProcessorsResponse'
  { nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of stream processors. 
  , streamProcessors :: Core.Maybe [Types.StreamProcessor]
    -- ^ List of stream processors that you have created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListStreamProcessorsResponse' value with any optional fields omitted.
mkListStreamProcessorsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListStreamProcessorsResponse
mkListStreamProcessorsResponse responseStatus
  = ListStreamProcessorsResponse'{nextToken = Core.Nothing,
                                  streamProcessors = Core.Nothing, responseStatus}

-- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of stream processors. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsprrsNextToken :: Lens.Lens' ListStreamProcessorsResponse (Core.Maybe Types.PaginationToken)
lsprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | List of stream processors that you have created.
--
-- /Note:/ Consider using 'streamProcessors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsprrsStreamProcessors :: Lens.Lens' ListStreamProcessorsResponse (Core.Maybe [Types.StreamProcessor])
lsprrsStreamProcessors = Lens.field @"streamProcessors"
{-# INLINEABLE lsprrsStreamProcessors #-}
{-# DEPRECATED streamProcessors "Use generic-lens or generic-optics with 'streamProcessors' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsprrsResponseStatus :: Lens.Lens' ListStreamProcessorsResponse Core.Int
lsprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
