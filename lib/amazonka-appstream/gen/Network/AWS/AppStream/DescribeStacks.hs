{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DescribeStacks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified stacks, if the stack names are provided. Otherwise, all stacks in the account are described.
--
-- This operation returns paginated results.
module Network.AWS.AppStream.DescribeStacks
    (
    -- * Creating a request
      DescribeStacks (..)
    , mkDescribeStacks
    -- ** Request lenses
    , dNames
    , dNextToken

    -- * Destructuring the response
    , DescribeStacksResponse (..)
    , mkDescribeStacksResponse
    -- ** Response lenses
    , dsrgrsNextToken
    , dsrgrsStacks
    , dsrgrsResponseStatus
    ) where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeStacks' smart constructor.
data DescribeStacks = DescribeStacks'
  { names :: Core.Maybe [Core.Text]
    -- ^ The names of the stacks to describe.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStacks' value with any optional fields omitted.
mkDescribeStacks
    :: DescribeStacks
mkDescribeStacks
  = DescribeStacks'{names = Core.Nothing, nextToken = Core.Nothing}

-- | The names of the stacks to describe.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNames :: Lens.Lens' DescribeStacks (Core.Maybe [Core.Text])
dNames = Lens.field @"names"
{-# INLINEABLE dNames #-}
{-# DEPRECATED names "Use generic-lens or generic-optics with 'names' instead"  #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeStacks (Core.Maybe Core.Text)
dNextToken = Lens.field @"nextToken"
{-# INLINEABLE dNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeStacks where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeStacks where
        toHeaders DescribeStacks{..}
          = Core.pure
              ("X-Amz-Target", "PhotonAdminProxyService.DescribeStacks")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeStacks where
        toJSON DescribeStacks{..}
          = Core.object
              (Core.catMaybes
                 [("Names" Core..=) Core.<$> names,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeStacks where
        type Rs DescribeStacks = DescribeStacksResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeStacksResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Stacks" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeStacks where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"stacks" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeStacksResponse' smart constructor.
data DescribeStacksResponse = DescribeStacksResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
  , stacks :: Core.Maybe [Types.Stack]
    -- ^ Information about the stacks.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeStacksResponse' value with any optional fields omitted.
mkDescribeStacksResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeStacksResponse
mkDescribeStacksResponse responseStatus
  = DescribeStacksResponse'{nextToken = Core.Nothing,
                            stacks = Core.Nothing, responseStatus}

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrgrsNextToken :: Lens.Lens' DescribeStacksResponse (Core.Maybe Core.Text)
dsrgrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsrgrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the stacks.
--
-- /Note:/ Consider using 'stacks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrgrsStacks :: Lens.Lens' DescribeStacksResponse (Core.Maybe [Types.Stack])
dsrgrsStacks = Lens.field @"stacks"
{-# INLINEABLE dsrgrsStacks #-}
{-# DEPRECATED stacks "Use generic-lens or generic-optics with 'stacks' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrgrsResponseStatus :: Lens.Lens' DescribeStacksResponse Core.Int
dsrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
