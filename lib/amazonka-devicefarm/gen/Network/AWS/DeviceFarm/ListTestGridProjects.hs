{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListTestGridProjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all Selenium testing projects in your account.
module Network.AWS.DeviceFarm.ListTestGridProjects
    (
    -- * Creating a request
      ListTestGridProjects (..)
    , mkListTestGridProjects
    -- ** Request lenses
    , ltgpMaxResult
    , ltgpNextToken

    -- * Destructuring the response
    , ListTestGridProjectsResponse (..)
    , mkListTestGridProjectsResponse
    -- ** Response lenses
    , ltgprrsNextToken
    , ltgprrsTestGridProjects
    , ltgprrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTestGridProjects' smart constructor.
data ListTestGridProjects = ListTestGridProjects'
  { maxResult :: Core.Maybe Core.Natural
    -- ^ Return no more than this number of results.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ From a response, used to continue a paginated listing. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTestGridProjects' value with any optional fields omitted.
mkListTestGridProjects
    :: ListTestGridProjects
mkListTestGridProjects
  = ListTestGridProjects'{maxResult = Core.Nothing,
                          nextToken = Core.Nothing}

-- | Return no more than this number of results.
--
-- /Note:/ Consider using 'maxResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgpMaxResult :: Lens.Lens' ListTestGridProjects (Core.Maybe Core.Natural)
ltgpMaxResult = Lens.field @"maxResult"
{-# INLINEABLE ltgpMaxResult #-}
{-# DEPRECATED maxResult "Use generic-lens or generic-optics with 'maxResult' instead"  #-}

-- | From a response, used to continue a paginated listing. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgpNextToken :: Lens.Lens' ListTestGridProjects (Core.Maybe Types.PaginationToken)
ltgpNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltgpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListTestGridProjects where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTestGridProjects where
        toHeaders ListTestGridProjects{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.ListTestGridProjects")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTestGridProjects where
        toJSON ListTestGridProjects{..}
          = Core.object
              (Core.catMaybes
                 [("maxResult" Core..=) Core.<$> maxResult,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListTestGridProjects where
        type Rs ListTestGridProjects = ListTestGridProjectsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTestGridProjectsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "testGridProjects"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListTestGridProjectsResponse' smart constructor.
data ListTestGridProjectsResponse = ListTestGridProjectsResponse'
  { nextToken :: Core.Maybe Types.PaginationToken
    -- ^ Used for pagination. Pass into 'ListTestGridProjects' to get more results in a paginated request.
  , testGridProjects :: Core.Maybe [Types.TestGridProject]
    -- ^ The list of TestGridProjects, based on a 'ListTestGridProjectsRequest' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListTestGridProjectsResponse' value with any optional fields omitted.
mkListTestGridProjectsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTestGridProjectsResponse
mkListTestGridProjectsResponse responseStatus
  = ListTestGridProjectsResponse'{nextToken = Core.Nothing,
                                  testGridProjects = Core.Nothing, responseStatus}

-- | Used for pagination. Pass into 'ListTestGridProjects' to get more results in a paginated request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgprrsNextToken :: Lens.Lens' ListTestGridProjectsResponse (Core.Maybe Types.PaginationToken)
ltgprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltgprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The list of TestGridProjects, based on a 'ListTestGridProjectsRequest' .
--
-- /Note:/ Consider using 'testGridProjects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgprrsTestGridProjects :: Lens.Lens' ListTestGridProjectsResponse (Core.Maybe [Types.TestGridProject])
ltgprrsTestGridProjects = Lens.field @"testGridProjects"
{-# INLINEABLE ltgprrsTestGridProjects #-}
{-# DEPRECATED testGridProjects "Use generic-lens or generic-optics with 'testGridProjects' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgprrsResponseStatus :: Lens.Lens' ListTestGridProjectsResponse Core.Int
ltgprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltgprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
