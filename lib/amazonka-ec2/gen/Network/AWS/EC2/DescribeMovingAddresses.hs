{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeMovingAddresses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your Elastic IP addresses that are being moved to the EC2-VPC platform, or that are being restored to the EC2-Classic platform. This request does not return information about any other Elastic IP addresses in your account.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeMovingAddresses
  ( -- * Creating a request
    DescribeMovingAddresses (..),
    mkDescribeMovingAddresses,

    -- ** Request lenses
    dmaDryRun,
    dmaFilters,
    dmaMaxResults,
    dmaNextToken,
    dmaPublicIps,

    -- * Destructuring the response
    DescribeMovingAddressesResponse (..),
    mkDescribeMovingAddressesResponse,

    -- ** Response lenses
    dmarrsMovingAddressStatuses,
    dmarrsNextToken,
    dmarrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeMovingAddresses' smart constructor.
data DescribeMovingAddresses = DescribeMovingAddresses'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters.
    --
    --
    --     * @moving-status@ - The status of the Elastic IP address (@MovingToVpc@ | @RestoringToClassic@ ).
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value outside of this range, an error is returned.
    --
    -- Default: If no value is provided, the default is 1000.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.String,
    -- | One or more Elastic IP addresses.
    publicIps :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMovingAddresses' value with any optional fields omitted.
mkDescribeMovingAddresses ::
  DescribeMovingAddresses
mkDescribeMovingAddresses =
  DescribeMovingAddresses'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      publicIps = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmaDryRun :: Lens.Lens' DescribeMovingAddresses (Core.Maybe Core.Bool)
dmaDryRun = Lens.field @"dryRun"
{-# DEPRECATED dmaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more filters.
--
--
--     * @moving-status@ - The status of the Elastic IP address (@MovingToVpc@ | @RestoringToClassic@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmaFilters :: Lens.Lens' DescribeMovingAddresses (Core.Maybe [Types.Filter])
dmaFilters = Lens.field @"filters"
{-# DEPRECATED dmaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value outside of this range, an error is returned.
--
-- Default: If no value is provided, the default is 1000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmaMaxResults :: Lens.Lens' DescribeMovingAddresses (Core.Maybe Core.Natural)
dmaMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dmaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmaNextToken :: Lens.Lens' DescribeMovingAddresses (Core.Maybe Types.String)
dmaNextToken = Lens.field @"nextToken"
{-# DEPRECATED dmaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One or more Elastic IP addresses.
--
-- /Note:/ Consider using 'publicIps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmaPublicIps :: Lens.Lens' DescribeMovingAddresses (Core.Maybe [Types.String])
dmaPublicIps = Lens.field @"publicIps"
{-# DEPRECATED dmaPublicIps "Use generic-lens or generic-optics with 'publicIps' instead." #-}

instance Core.AWSRequest DescribeMovingAddresses where
  type Rs DescribeMovingAddresses = DescribeMovingAddressesResponse
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
            ( Core.pure ("Action", "DescribeMovingAddresses")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> (Core.toQueryList "PublicIp" Core.<$> publicIps)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeMovingAddressesResponse'
            Core.<$> ( x Core..@? "movingAddressStatusSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeMovingAddresses where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"movingAddressStatuses" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeMovingAddressesResponse' smart constructor.
data DescribeMovingAddressesResponse = DescribeMovingAddressesResponse'
  { -- | The status for each Elastic IP address.
    movingAddressStatuses :: Core.Maybe [Types.MovingAddressStatus],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMovingAddressesResponse' value with any optional fields omitted.
mkDescribeMovingAddressesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeMovingAddressesResponse
mkDescribeMovingAddressesResponse responseStatus =
  DescribeMovingAddressesResponse'
    { movingAddressStatuses =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The status for each Elastic IP address.
--
-- /Note:/ Consider using 'movingAddressStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmarrsMovingAddressStatuses :: Lens.Lens' DescribeMovingAddressesResponse (Core.Maybe [Types.MovingAddressStatus])
dmarrsMovingAddressStatuses = Lens.field @"movingAddressStatuses"
{-# DEPRECATED dmarrsMovingAddressStatuses "Use generic-lens or generic-optics with 'movingAddressStatuses' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmarrsNextToken :: Lens.Lens' DescribeMovingAddressesResponse (Core.Maybe Types.String)
dmarrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dmarrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmarrsResponseStatus :: Lens.Lens' DescribeMovingAddressesResponse Core.Int
dmarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
