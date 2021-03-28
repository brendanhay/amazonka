{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.ListStateMachines
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the existing state machines.
--
-- If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- This operation returns paginated results.
module Network.AWS.StepFunctions.ListStateMachines
    (
    -- * Creating a request
      ListStateMachines (..)
    , mkListStateMachines
    -- ** Request lenses
    , lsmMaxResults
    , lsmNextToken

    -- * Destructuring the response
    , ListStateMachinesResponse (..)
    , mkListStateMachinesResponse
    -- ** Response lenses
    , lsmrrsStateMachines
    , lsmrrsNextToken
    , lsmrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StepFunctions.Types as Types

-- | /See:/ 'mkListStateMachines' smart constructor.
data ListStateMachines = ListStateMachines'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListStateMachines' value with any optional fields omitted.
mkListStateMachines
    :: ListStateMachines
mkListStateMachines
  = ListStateMachines'{maxResults = Core.Nothing,
                       nextToken = Core.Nothing}

-- | The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmMaxResults :: Lens.Lens' ListStateMachines (Core.Maybe Core.Natural)
lsmMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lsmMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmNextToken :: Lens.Lens' ListStateMachines (Core.Maybe Types.NextToken)
lsmNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsmNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListStateMachines where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListStateMachines where
        toHeaders ListStateMachines{..}
          = Core.pure ("X-Amz-Target", "AWSStepFunctions.ListStateMachines")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON ListStateMachines where
        toJSON ListStateMachines{..}
          = Core.object
              (Core.catMaybes
                 [("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListStateMachines where
        type Rs ListStateMachines = ListStateMachinesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListStateMachinesResponse' Core.<$>
                   (x Core..:? "stateMachines" Core..!= Core.mempty) Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListStateMachines where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"stateMachines") =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListStateMachinesResponse' smart constructor.
data ListStateMachinesResponse = ListStateMachinesResponse'
  { stateMachines :: [Types.StateMachineListItem]
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListStateMachinesResponse' value with any optional fields omitted.
mkListStateMachinesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListStateMachinesResponse
mkListStateMachinesResponse responseStatus
  = ListStateMachinesResponse'{stateMachines = Core.mempty,
                               nextToken = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'stateMachines' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmrrsStateMachines :: Lens.Lens' ListStateMachinesResponse [Types.StateMachineListItem]
lsmrrsStateMachines = Lens.field @"stateMachines"
{-# INLINEABLE lsmrrsStateMachines #-}
{-# DEPRECATED stateMachines "Use generic-lens or generic-optics with 'stateMachines' instead"  #-}

-- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmrrsNextToken :: Lens.Lens' ListStateMachinesResponse (Core.Maybe Types.NextToken)
lsmrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsmrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmrrsResponseStatus :: Lens.Lens' ListStateMachinesResponse Core.Int
lsmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
