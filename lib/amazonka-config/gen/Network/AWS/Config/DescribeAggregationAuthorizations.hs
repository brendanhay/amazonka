{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeAggregationAuthorizations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of authorizations granted to various aggregator accounts and regions.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeAggregationAuthorizations
  ( -- * Creating a request
    DescribeAggregationAuthorizations (..),
    mkDescribeAggregationAuthorizations,

    -- ** Request lenses
    daaLimit,
    daaNextToken,

    -- * Destructuring the response
    DescribeAggregationAuthorizationsResponse (..),
    mkDescribeAggregationAuthorizationsResponse,

    -- ** Response lenses
    daarrsAggregationAuthorizations,
    daarrsNextToken,
    daarrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAggregationAuthorizations' smart constructor.
data DescribeAggregationAuthorizations = DescribeAggregationAuthorizations'
  { -- | The maximum number of AggregationAuthorizations returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
    limit :: Core.Maybe Core.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAggregationAuthorizations' value with any optional fields omitted.
mkDescribeAggregationAuthorizations ::
  DescribeAggregationAuthorizations
mkDescribeAggregationAuthorizations =
  DescribeAggregationAuthorizations'
    { limit = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of AggregationAuthorizations returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaLimit :: Lens.Lens' DescribeAggregationAuthorizations (Core.Maybe Core.Natural)
daaLimit = Lens.field @"limit"
{-# DEPRECATED daaLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaNextToken :: Lens.Lens' DescribeAggregationAuthorizations (Core.Maybe Types.String)
daaNextToken = Lens.field @"nextToken"
{-# DEPRECATED daaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeAggregationAuthorizations where
  toJSON DescribeAggregationAuthorizations {..} =
    Core.object
      ( Core.catMaybes
          [ ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeAggregationAuthorizations where
  type
    Rs DescribeAggregationAuthorizations =
      DescribeAggregationAuthorizationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.DescribeAggregationAuthorizations"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAggregationAuthorizationsResponse'
            Core.<$> (x Core..:? "AggregationAuthorizations")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeAggregationAuthorizations where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"aggregationAuthorizations" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeAggregationAuthorizationsResponse' smart constructor.
data DescribeAggregationAuthorizationsResponse = DescribeAggregationAuthorizationsResponse'
  { -- | Returns a list of authorizations granted to various aggregator accounts and regions.
    aggregationAuthorizations :: Core.Maybe [Types.AggregationAuthorization],
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAggregationAuthorizationsResponse' value with any optional fields omitted.
mkDescribeAggregationAuthorizationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAggregationAuthorizationsResponse
mkDescribeAggregationAuthorizationsResponse responseStatus =
  DescribeAggregationAuthorizationsResponse'
    { aggregationAuthorizations =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Returns a list of authorizations granted to various aggregator accounts and regions.
--
-- /Note:/ Consider using 'aggregationAuthorizations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarrsAggregationAuthorizations :: Lens.Lens' DescribeAggregationAuthorizationsResponse (Core.Maybe [Types.AggregationAuthorization])
daarrsAggregationAuthorizations = Lens.field @"aggregationAuthorizations"
{-# DEPRECATED daarrsAggregationAuthorizations "Use generic-lens or generic-optics with 'aggregationAuthorizations' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarrsNextToken :: Lens.Lens' DescribeAggregationAuthorizationsResponse (Core.Maybe Types.String)
daarrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED daarrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarrsResponseStatus :: Lens.Lens' DescribeAggregationAuthorizationsResponse Core.Int
daarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED daarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
