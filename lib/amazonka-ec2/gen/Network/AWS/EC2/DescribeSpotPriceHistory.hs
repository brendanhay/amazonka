{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSpotPriceHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Spot price history. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-spot-instances-history.html Spot Instance pricing history> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- When you specify a start and end time, this operation returns the prices of the instance types within the time range that you specified and the time when the price changed. The price is valid within the time period that you specified; the response merely indicates the last time that the price changed.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeSpotPriceHistory
  ( -- * Creating a request
    DescribeSpotPriceHistory (..),
    mkDescribeSpotPriceHistory,

    -- ** Request lenses
    dsphAvailabilityZone,
    dsphDryRun,
    dsphEndTime,
    dsphFilters,
    dsphInstanceTypes,
    dsphMaxResults,
    dsphNextToken,
    dsphProductDescriptions,
    dsphStartTime,

    -- * Destructuring the response
    DescribeSpotPriceHistoryResponse (..),
    mkDescribeSpotPriceHistoryResponse,

    -- ** Response lenses
    dsphrrsNextToken,
    dsphrrsSpotPriceHistory,
    dsphrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeSpotPriceHistory.
--
-- /See:/ 'mkDescribeSpotPriceHistory' smart constructor.
data DescribeSpotPriceHistory = DescribeSpotPriceHistory'
  { -- | Filters the results by the specified Availability Zone.
    availabilityZone :: Core.Maybe Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The date and time, up to the current date, from which to stop retrieving the price history data, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
    endTime :: Core.Maybe Core.UTCTime,
    -- | One or more filters.
    --
    --
    --     * @availability-zone@ - The Availability Zone for which prices should be returned.
    --
    --
    --     * @instance-type@ - The type of instance (for example, @m3.medium@ ).
    --
    --
    --     * @product-description@ - The product description for the Spot price (@Linux/UNIX@ | @Red Hat Enterprise Linux@ | @SUSE Linux@ | @Windows@ | @Linux/UNIX (Amazon VPC)@ | @Red Hat Enterprise Linux (Amazon VPC)@ | @SUSE Linux (Amazon VPC)@ | @Windows (Amazon VPC)@ ).
    --
    --
    --     * @spot-price@ - The Spot price. The value must match exactly (or use wildcards; greater than or less than comparison is not supported).
    --
    --
    --     * @timestamp@ - The time stamp of the Spot price history, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). You can use wildcards (* and ?). Greater than or less than comparison is not supported.
    filters :: Core.Maybe [Types.Filter],
    -- | Filters the results by the specified instance types.
    instanceTypes :: Core.Maybe [Types.InstanceType],
    -- | The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
    maxResults :: Core.Maybe Core.Int,
    -- | The token for the next set of results.
    nextToken :: Core.Maybe Types.String,
    -- | Filters the results by the specified basic product descriptions.
    productDescriptions :: Core.Maybe [Types.String],
    -- | The date and time, up to the past 90 days, from which to start retrieving the price history data, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
    startTime :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeSpotPriceHistory' value with any optional fields omitted.
mkDescribeSpotPriceHistory ::
  DescribeSpotPriceHistory
mkDescribeSpotPriceHistory =
  DescribeSpotPriceHistory'
    { availabilityZone = Core.Nothing,
      dryRun = Core.Nothing,
      endTime = Core.Nothing,
      filters = Core.Nothing,
      instanceTypes = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      productDescriptions = Core.Nothing,
      startTime = Core.Nothing
    }

-- | Filters the results by the specified Availability Zone.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsphAvailabilityZone :: Lens.Lens' DescribeSpotPriceHistory (Core.Maybe Types.String)
dsphAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED dsphAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsphDryRun :: Lens.Lens' DescribeSpotPriceHistory (Core.Maybe Core.Bool)
dsphDryRun = Lens.field @"dryRun"
{-# DEPRECATED dsphDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The date and time, up to the current date, from which to stop retrieving the price history data, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsphEndTime :: Lens.Lens' DescribeSpotPriceHistory (Core.Maybe Core.UTCTime)
dsphEndTime = Lens.field @"endTime"
{-# DEPRECATED dsphEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | One or more filters.
--
--
--     * @availability-zone@ - The Availability Zone for which prices should be returned.
--
--
--     * @instance-type@ - The type of instance (for example, @m3.medium@ ).
--
--
--     * @product-description@ - The product description for the Spot price (@Linux/UNIX@ | @Red Hat Enterprise Linux@ | @SUSE Linux@ | @Windows@ | @Linux/UNIX (Amazon VPC)@ | @Red Hat Enterprise Linux (Amazon VPC)@ | @SUSE Linux (Amazon VPC)@ | @Windows (Amazon VPC)@ ).
--
--
--     * @spot-price@ - The Spot price. The value must match exactly (or use wildcards; greater than or less than comparison is not supported).
--
--
--     * @timestamp@ - The time stamp of the Spot price history, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). You can use wildcards (* and ?). Greater than or less than comparison is not supported.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsphFilters :: Lens.Lens' DescribeSpotPriceHistory (Core.Maybe [Types.Filter])
dsphFilters = Lens.field @"filters"
{-# DEPRECATED dsphFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | Filters the results by the specified instance types.
--
-- /Note:/ Consider using 'instanceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsphInstanceTypes :: Lens.Lens' DescribeSpotPriceHistory (Core.Maybe [Types.InstanceType])
dsphInstanceTypes = Lens.field @"instanceTypes"
{-# DEPRECATED dsphInstanceTypes "Use generic-lens or generic-optics with 'instanceTypes' instead." #-}

-- | The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsphMaxResults :: Lens.Lens' DescribeSpotPriceHistory (Core.Maybe Core.Int)
dsphMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dsphMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsphNextToken :: Lens.Lens' DescribeSpotPriceHistory (Core.Maybe Types.String)
dsphNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsphNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filters the results by the specified basic product descriptions.
--
-- /Note:/ Consider using 'productDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsphProductDescriptions :: Lens.Lens' DescribeSpotPriceHistory (Core.Maybe [Types.String])
dsphProductDescriptions = Lens.field @"productDescriptions"
{-# DEPRECATED dsphProductDescriptions "Use generic-lens or generic-optics with 'productDescriptions' instead." #-}

-- | The date and time, up to the past 90 days, from which to start retrieving the price history data, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsphStartTime :: Lens.Lens' DescribeSpotPriceHistory (Core.Maybe Core.UTCTime)
dsphStartTime = Lens.field @"startTime"
{-# DEPRECATED dsphStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

instance Core.AWSRequest DescribeSpotPriceHistory where
  type Rs DescribeSpotPriceHistory = DescribeSpotPriceHistoryResponse
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
            ( Core.pure ("Action", "DescribeSpotPriceHistory")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "AvailabilityZone" Core.<$> availabilityZone)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "EndTime" Core.<$> endTime)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryList "InstanceType" Core.<$> instanceTypes)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> ( Core.toQueryList "ProductDescription"
                            Core.<$> productDescriptions
                        )
                Core.<> (Core.toQueryValue "StartTime" Core.<$> startTime)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSpotPriceHistoryResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "spotPriceHistorySet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeSpotPriceHistory where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"spotPriceHistory" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Contains the output of DescribeSpotPriceHistory.
--
-- /See:/ 'mkDescribeSpotPriceHistoryResponse' smart constructor.
data DescribeSpotPriceHistoryResponse = DescribeSpotPriceHistoryResponse'
  { -- | The token required to retrieve the next set of results. This value is null or an empty string when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The historical Spot prices.
    spotPriceHistory :: Core.Maybe [Types.SpotPrice],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeSpotPriceHistoryResponse' value with any optional fields omitted.
mkDescribeSpotPriceHistoryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeSpotPriceHistoryResponse
mkDescribeSpotPriceHistoryResponse responseStatus =
  DescribeSpotPriceHistoryResponse'
    { nextToken = Core.Nothing,
      spotPriceHistory = Core.Nothing,
      responseStatus
    }

-- | The token required to retrieve the next set of results. This value is null or an empty string when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsphrrsNextToken :: Lens.Lens' DescribeSpotPriceHistoryResponse (Core.Maybe Types.String)
dsphrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsphrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The historical Spot prices.
--
-- /Note:/ Consider using 'spotPriceHistory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsphrrsSpotPriceHistory :: Lens.Lens' DescribeSpotPriceHistoryResponse (Core.Maybe [Types.SpotPrice])
dsphrrsSpotPriceHistory = Lens.field @"spotPriceHistory"
{-# DEPRECATED dsphrrsSpotPriceHistory "Use generic-lens or generic-optics with 'spotPriceHistory' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsphrrsResponseStatus :: Lens.Lens' DescribeSpotPriceHistoryResponse Core.Int
dsphrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsphrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
