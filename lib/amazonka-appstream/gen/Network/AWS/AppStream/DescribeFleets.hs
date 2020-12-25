{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeFleets (..),
    mkDescribeFleets,

    -- ** Request lenses
    dfNames,
    dfNextToken,

    -- * Destructuring the response
    DescribeFleetsResponse (..),
    mkDescribeFleetsResponse,

    -- ** Response lenses
    dfrfrsFleets,
    dfrfrsNextToken,
    dfrfrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeFleets' smart constructor.
data DescribeFleets = DescribeFleets'
  { -- | The names of the fleets to describe.
    names :: Core.Maybe [Types.String],
    -- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeFleets' value with any optional fields omitted.
mkDescribeFleets ::
  DescribeFleets
mkDescribeFleets =
  DescribeFleets' {names = Core.Nothing, nextToken = Core.Nothing}

-- | The names of the fleets to describe.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfNames :: Lens.Lens' DescribeFleets (Core.Maybe [Types.String])
dfNames = Lens.field @"names"
{-# DEPRECATED dfNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfNextToken :: Lens.Lens' DescribeFleets (Core.Maybe Types.String)
dfNextToken = Lens.field @"nextToken"
{-# DEPRECATED dfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeFleets where
  toJSON DescribeFleets {..} =
    Core.object
      ( Core.catMaybes
          [ ("Names" Core..=) Core.<$> names,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeFleets where
  type Rs DescribeFleets = DescribeFleetsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "PhotonAdminProxyService.DescribeFleets")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetsResponse'
            Core.<$> (x Core..:? "Fleets")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeFleets where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"fleets" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeFleetsResponse' smart constructor.
data DescribeFleetsResponse = DescribeFleetsResponse'
  { -- | Information about the fleets.
    fleets :: Core.Maybe [Types.Fleet],
    -- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeFleetsResponse' value with any optional fields omitted.
mkDescribeFleetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeFleetsResponse
mkDescribeFleetsResponse responseStatus =
  DescribeFleetsResponse'
    { fleets = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the fleets.
--
-- /Note:/ Consider using 'fleets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrfrsFleets :: Lens.Lens' DescribeFleetsResponse (Core.Maybe [Types.Fleet])
dfrfrsFleets = Lens.field @"fleets"
{-# DEPRECATED dfrfrsFleets "Use generic-lens or generic-optics with 'fleets' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrfrsNextToken :: Lens.Lens' DescribeFleetsResponse (Core.Maybe Types.String)
dfrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dfrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrfrsResponseStatus :: Lens.Lens' DescribeFleetsResponse Core.Int
dfrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dfrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
