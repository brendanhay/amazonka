{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.ListAssociatedStacks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the name of the stack with which the specified fleet is associated.
--
-- This operation returns paginated results.
module Network.AWS.AppStream.ListAssociatedStacks
    (
    -- * Creating a request
      ListAssociatedStacks (..)
    , mkListAssociatedStacks
    -- ** Request lenses
    , lasFleetName
    , lasNextToken

    -- * Destructuring the response
    , ListAssociatedStacksResponse (..)
    , mkListAssociatedStacksResponse
    -- ** Response lenses
    , lasrrsNames
    , lasrrsNextToken
    , lasrrsResponseStatus
    ) where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAssociatedStacks' smart constructor.
data ListAssociatedStacks = ListAssociatedStacks'
  { fleetName :: Core.Text
    -- ^ The name of the fleet.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAssociatedStacks' value with any optional fields omitted.
mkListAssociatedStacks
    :: Core.Text -- ^ 'fleetName'
    -> ListAssociatedStacks
mkListAssociatedStacks fleetName
  = ListAssociatedStacks'{fleetName, nextToken = Core.Nothing}

-- | The name of the fleet.
--
-- /Note:/ Consider using 'fleetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasFleetName :: Lens.Lens' ListAssociatedStacks Core.Text
lasFleetName = Lens.field @"fleetName"
{-# INLINEABLE lasFleetName #-}
{-# DEPRECATED fleetName "Use generic-lens or generic-optics with 'fleetName' instead"  #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasNextToken :: Lens.Lens' ListAssociatedStacks (Core.Maybe Core.Text)
lasNextToken = Lens.field @"nextToken"
{-# INLINEABLE lasNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListAssociatedStacks where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListAssociatedStacks where
        toHeaders ListAssociatedStacks{..}
          = Core.pure
              ("X-Amz-Target", "PhotonAdminProxyService.ListAssociatedStacks")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListAssociatedStacks where
        toJSON ListAssociatedStacks{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("FleetName" Core..= fleetName),
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListAssociatedStacks where
        type Rs ListAssociatedStacks = ListAssociatedStacksResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListAssociatedStacksResponse' Core.<$>
                   (x Core..:? "Names") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListAssociatedStacks where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"names" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListAssociatedStacksResponse' smart constructor.
data ListAssociatedStacksResponse = ListAssociatedStacksResponse'
  { names :: Core.Maybe [Core.Text]
    -- ^ The name of the stack.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAssociatedStacksResponse' value with any optional fields omitted.
mkListAssociatedStacksResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAssociatedStacksResponse
mkListAssociatedStacksResponse responseStatus
  = ListAssociatedStacksResponse'{names = Core.Nothing,
                                  nextToken = Core.Nothing, responseStatus}

-- | The name of the stack.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrrsNames :: Lens.Lens' ListAssociatedStacksResponse (Core.Maybe [Core.Text])
lasrrsNames = Lens.field @"names"
{-# INLINEABLE lasrrsNames #-}
{-# DEPRECATED names "Use generic-lens or generic-optics with 'names' instead"  #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrrsNextToken :: Lens.Lens' ListAssociatedStacksResponse (Core.Maybe Core.Text)
lasrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lasrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrrsResponseStatus :: Lens.Lens' ListAssociatedStacksResponse Core.Int
lasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
