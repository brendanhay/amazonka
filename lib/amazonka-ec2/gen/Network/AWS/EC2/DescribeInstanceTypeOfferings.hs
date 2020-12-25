{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeInstanceTypeOfferings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all instance types offered. The results can be filtered by location (Region or Availability Zone). If no location is specified, the instance types offered in the current Region are returned.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeInstanceTypeOfferings
  ( -- * Creating a request
    DescribeInstanceTypeOfferings (..),
    mkDescribeInstanceTypeOfferings,

    -- ** Request lenses
    ditoDryRun,
    ditoFilters,
    ditoLocationType,
    ditoMaxResults,
    ditoNextToken,

    -- * Destructuring the response
    DescribeInstanceTypeOfferingsResponse (..),
    mkDescribeInstanceTypeOfferingsResponse,

    -- ** Response lenses
    ditorrsInstanceTypeOfferings,
    ditorrsNextToken,
    ditorrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeInstanceTypeOfferings' smart constructor.
data DescribeInstanceTypeOfferings = DescribeInstanceTypeOfferings'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters. Filter names and values are case-sensitive.
    --
    --
    --     * @location@ - This depends on the location type. For example, if the location type is @region@ (default), the location is the Region code (for example, @us-east-2@ .)
    --
    --
    --     * @instance-type@ - The instance type. For example, @c5.2xlarge@ .
    filters :: Core.Maybe [Types.Filter],
    -- | The location type.
    locationType :: Core.Maybe Types.LocationType,
    -- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the next token value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token to retrieve the next page of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstanceTypeOfferings' value with any optional fields omitted.
mkDescribeInstanceTypeOfferings ::
  DescribeInstanceTypeOfferings
mkDescribeInstanceTypeOfferings =
  DescribeInstanceTypeOfferings'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      locationType = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditoDryRun :: Lens.Lens' DescribeInstanceTypeOfferings (Core.Maybe Core.Bool)
ditoDryRun = Lens.field @"dryRun"
{-# DEPRECATED ditoDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more filters. Filter names and values are case-sensitive.
--
--
--     * @location@ - This depends on the location type. For example, if the location type is @region@ (default), the location is the Region code (for example, @us-east-2@ .)
--
--
--     * @instance-type@ - The instance type. For example, @c5.2xlarge@ .
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditoFilters :: Lens.Lens' DescribeInstanceTypeOfferings (Core.Maybe [Types.Filter])
ditoFilters = Lens.field @"filters"
{-# DEPRECATED ditoFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The location type.
--
-- /Note:/ Consider using 'locationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditoLocationType :: Lens.Lens' DescribeInstanceTypeOfferings (Core.Maybe Types.LocationType)
ditoLocationType = Lens.field @"locationType"
{-# DEPRECATED ditoLocationType "Use generic-lens or generic-optics with 'locationType' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the next token value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditoMaxResults :: Lens.Lens' DescribeInstanceTypeOfferings (Core.Maybe Core.Natural)
ditoMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ditoMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditoNextToken :: Lens.Lens' DescribeInstanceTypeOfferings (Core.Maybe Types.NextToken)
ditoNextToken = Lens.field @"nextToken"
{-# DEPRECATED ditoNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeInstanceTypeOfferings where
  type
    Rs DescribeInstanceTypeOfferings =
      DescribeInstanceTypeOfferingsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeInstanceTypeOfferings")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "LocationType" Core.<$> locationType)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeInstanceTypeOfferingsResponse'
            Core.<$> ( x Core..@? "instanceTypeOfferingSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeInstanceTypeOfferings where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"instanceTypeOfferings" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeInstanceTypeOfferingsResponse' smart constructor.
data DescribeInstanceTypeOfferingsResponse = DescribeInstanceTypeOfferingsResponse'
  { -- | The instance types offered.
    instanceTypeOfferings :: Core.Maybe [Types.InstanceTypeOffering],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstanceTypeOfferingsResponse' value with any optional fields omitted.
mkDescribeInstanceTypeOfferingsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeInstanceTypeOfferingsResponse
mkDescribeInstanceTypeOfferingsResponse responseStatus =
  DescribeInstanceTypeOfferingsResponse'
    { instanceTypeOfferings =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The instance types offered.
--
-- /Note:/ Consider using 'instanceTypeOfferings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditorrsInstanceTypeOfferings :: Lens.Lens' DescribeInstanceTypeOfferingsResponse (Core.Maybe [Types.InstanceTypeOffering])
ditorrsInstanceTypeOfferings = Lens.field @"instanceTypeOfferings"
{-# DEPRECATED ditorrsInstanceTypeOfferings "Use generic-lens or generic-optics with 'instanceTypeOfferings' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditorrsNextToken :: Lens.Lens' DescribeInstanceTypeOfferingsResponse (Core.Maybe Types.NextToken)
ditorrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ditorrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditorrsResponseStatus :: Lens.Lens' DescribeInstanceTypeOfferingsResponse Core.Int
ditorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ditorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
