{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DescribeFleets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified fleets, if the fleet names are provided. Otherwise, all fleets in the account are described.
--
-- This operation returns paginated results.
module Network.AWS.AppStream.DescribeFleets
    (
    -- * Creating a request
      DescribeFleets (..)
    , mkDescribeFleets
    -- ** Request lenses
    , dfNames
    , dfNextToken

    -- * Destructuring the response
    , DescribeFleetsResponse (..)
    , mkDescribeFleetsResponse
    -- ** Response lenses
    , dfrfrsFleets
    , dfrfrsNextToken
    , dfrfrsResponseStatus
    ) where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeFleets' smart constructor.
data DescribeFleets = DescribeFleets'
  { names :: Core.Maybe [Core.Text]
    -- ^ The names of the fleets to describe.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeFleets' value with any optional fields omitted.
mkDescribeFleets
    :: DescribeFleets
mkDescribeFleets
  = DescribeFleets'{names = Core.Nothing, nextToken = Core.Nothing}

-- | The names of the fleets to describe.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfNames :: Lens.Lens' DescribeFleets (Core.Maybe [Core.Text])
dfNames = Lens.field @"names"
{-# INLINEABLE dfNames #-}
{-# DEPRECATED names "Use generic-lens or generic-optics with 'names' instead"  #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfNextToken :: Lens.Lens' DescribeFleets (Core.Maybe Core.Text)
dfNextToken = Lens.field @"nextToken"
{-# INLINEABLE dfNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeFleets where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeFleets where
        toHeaders DescribeFleets{..}
          = Core.pure
              ("X-Amz-Target", "PhotonAdminProxyService.DescribeFleets")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeFleets where
        toJSON DescribeFleets{..}
          = Core.object
              (Core.catMaybes
                 [("Names" Core..=) Core.<$> names,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeFleets where
        type Rs DescribeFleets = DescribeFleetsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeFleetsResponse' Core.<$>
                   (x Core..:? "Fleets") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeFleets where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"fleets" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeFleetsResponse' smart constructor.
data DescribeFleetsResponse = DescribeFleetsResponse'
  { fleets :: Core.Maybe [Types.Fleet]
    -- ^ Information about the fleets.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeFleetsResponse' value with any optional fields omitted.
mkDescribeFleetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeFleetsResponse
mkDescribeFleetsResponse responseStatus
  = DescribeFleetsResponse'{fleets = Core.Nothing,
                            nextToken = Core.Nothing, responseStatus}

-- | Information about the fleets.
--
-- /Note:/ Consider using 'fleets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrfrsFleets :: Lens.Lens' DescribeFleetsResponse (Core.Maybe [Types.Fleet])
dfrfrsFleets = Lens.field @"fleets"
{-# INLINEABLE dfrfrsFleets #-}
{-# DEPRECATED fleets "Use generic-lens or generic-optics with 'fleets' instead"  #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrfrsNextToken :: Lens.Lens' DescribeFleetsResponse (Core.Maybe Core.Text)
dfrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dfrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrfrsResponseStatus :: Lens.Lens' DescribeFleetsResponse Core.Int
dfrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dfrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
