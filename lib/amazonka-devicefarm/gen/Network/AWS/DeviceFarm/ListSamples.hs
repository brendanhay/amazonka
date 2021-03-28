{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListSamples
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about samples, given an AWS Device Farm job ARN.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListSamples
    (
    -- * Creating a request
      ListSamples (..)
    , mkListSamples
    -- ** Request lenses
    , lsArn
    , lsNextToken

    -- * Destructuring the response
    , ListSamplesResponse (..)
    , mkListSamplesResponse
    -- ** Response lenses
    , lsrrsNextToken
    , lsrrsSamples
    , lsrrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the list samples operation.
--
-- /See:/ 'mkListSamples' smart constructor.
data ListSamples = ListSamples'
  { arn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the job used to list samples.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSamples' value with any optional fields omitted.
mkListSamples
    :: Types.Arn -- ^ 'arn'
    -> ListSamples
mkListSamples arn = ListSamples'{arn, nextToken = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the job used to list samples.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsArn :: Lens.Lens' ListSamples Types.Arn
lsArn = Lens.field @"arn"
{-# INLINEABLE lsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsNextToken :: Lens.Lens' ListSamples (Core.Maybe Types.PaginationToken)
lsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListSamples where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListSamples where
        toHeaders ListSamples{..}
          = Core.pure ("X-Amz-Target", "DeviceFarm_20150623.ListSamples")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListSamples where
        toJSON ListSamples{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("arn" Core..= arn),
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListSamples where
        type Rs ListSamples = ListSamplesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListSamplesResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "samples" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListSamples where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"samples" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Represents the result of a list samples request.
--
-- /See:/ 'mkListSamplesResponse' smart constructor.
data ListSamplesResponse = ListSamplesResponse'
  { nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
  , samples :: Core.Maybe [Types.Sample]
    -- ^ Information about the samples.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSamplesResponse' value with any optional fields omitted.
mkListSamplesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListSamplesResponse
mkListSamplesResponse responseStatus
  = ListSamplesResponse'{nextToken = Core.Nothing,
                         samples = Core.Nothing, responseStatus}

-- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsNextToken :: Lens.Lens' ListSamplesResponse (Core.Maybe Types.PaginationToken)
lsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the samples.
--
-- /Note:/ Consider using 'samples' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsSamples :: Lens.Lens' ListSamplesResponse (Core.Maybe [Types.Sample])
lsrrsSamples = Lens.field @"samples"
{-# INLINEABLE lsrrsSamples #-}
{-# DEPRECATED samples "Use generic-lens or generic-optics with 'samples' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsResponseStatus :: Lens.Lens' ListSamplesResponse Core.Int
lsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
