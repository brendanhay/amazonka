{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListSuites
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about test suites for a given job.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListSuites
    (
    -- * Creating a request
      ListSuites (..)
    , mkListSuites
    -- ** Request lenses
    , lssArn
    , lssNextToken

    -- * Destructuring the response
    , ListSuitesResponse (..)
    , mkListSuitesResponse
    -- ** Response lenses
    , lsrfrsNextToken
    , lsrfrsSuites
    , lsrfrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the list suites operation.
--
-- /See:/ 'mkListSuites' smart constructor.
data ListSuites = ListSuites'
  { arn :: Types.Arn
    -- ^ The job's Amazon Resource Name (ARN).
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSuites' value with any optional fields omitted.
mkListSuites
    :: Types.Arn -- ^ 'arn'
    -> ListSuites
mkListSuites arn = ListSuites'{arn, nextToken = Core.Nothing}

-- | The job's Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssArn :: Lens.Lens' ListSuites Types.Arn
lssArn = Lens.field @"arn"
{-# INLINEABLE lssArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssNextToken :: Lens.Lens' ListSuites (Core.Maybe Types.PaginationToken)
lssNextToken = Lens.field @"nextToken"
{-# INLINEABLE lssNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListSuites where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListSuites where
        toHeaders ListSuites{..}
          = Core.pure ("X-Amz-Target", "DeviceFarm_20150623.ListSuites")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListSuites where
        toJSON ListSuites{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("arn" Core..= arn),
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListSuites where
        type Rs ListSuites = ListSuitesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListSuitesResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "suites" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListSuites where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"suites" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Represents the result of a list suites request.
--
-- /See:/ 'mkListSuitesResponse' smart constructor.
data ListSuitesResponse = ListSuitesResponse'
  { nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
  , suites :: Core.Maybe [Types.Suite]
    -- ^ Information about the suites.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListSuitesResponse' value with any optional fields omitted.
mkListSuitesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListSuitesResponse
mkListSuitesResponse responseStatus
  = ListSuitesResponse'{nextToken = Core.Nothing,
                        suites = Core.Nothing, responseStatus}

-- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrfrsNextToken :: Lens.Lens' ListSuitesResponse (Core.Maybe Types.PaginationToken)
lsrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the suites.
--
-- /Note:/ Consider using 'suites' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrfrsSuites :: Lens.Lens' ListSuitesResponse (Core.Maybe [Types.Suite])
lsrfrsSuites = Lens.field @"suites"
{-# INLINEABLE lsrfrsSuites #-}
{-# DEPRECATED suites "Use generic-lens or generic-optics with 'suites' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrfrsResponseStatus :: Lens.Lens' ListSuitesResponse Core.Int
lsrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
