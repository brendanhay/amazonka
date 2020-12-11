{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dsphInstanceTypes,
    dsphStartTime,
    dsphFilters,
    dsphNextToken,
    dsphAvailabilityZone,
    dsphEndTime,
    dsphProductDescriptions,
    dsphDryRun,
    dsphMaxResults,

    -- * Destructuring the response
    DescribeSpotPriceHistoryResponse (..),
    mkDescribeSpotPriceHistoryResponse,

    -- ** Response lenses
    dsphrsNextToken,
    dsphrsSpotPriceHistory,
    dsphrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeSpotPriceHistory.
--
-- /See:/ 'mkDescribeSpotPriceHistory' smart constructor.
data DescribeSpotPriceHistory = DescribeSpotPriceHistory'
  { instanceTypes ::
      Lude.Maybe [InstanceType],
    startTime :: Lude.Maybe Lude.ISO8601,
    filters :: Lude.Maybe [Filter],
    nextToken :: Lude.Maybe Lude.Text,
    availabilityZone :: Lude.Maybe Lude.Text,
    endTime :: Lude.Maybe Lude.ISO8601,
    productDescriptions ::
      Lude.Maybe [Lude.Text],
    dryRun :: Lude.Maybe Lude.Bool,
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSpotPriceHistory' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - Filters the results by the specified Availability Zone.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'endTime' - The date and time, up to the current date, from which to stop retrieving the price history data, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
-- * 'filters' - One or more filters.
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
-- * 'instanceTypes' - Filters the results by the specified instance types.
-- * 'maxResults' - The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
-- * 'nextToken' - The token for the next set of results.
-- * 'productDescriptions' - Filters the results by the specified basic product descriptions.
-- * 'startTime' - The date and time, up to the past 90 days, from which to start retrieving the price history data, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
mkDescribeSpotPriceHistory ::
  DescribeSpotPriceHistory
mkDescribeSpotPriceHistory =
  DescribeSpotPriceHistory'
    { instanceTypes = Lude.Nothing,
      startTime = Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      endTime = Lude.Nothing,
      productDescriptions = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Filters the results by the specified instance types.
--
-- /Note:/ Consider using 'instanceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsphInstanceTypes :: Lens.Lens' DescribeSpotPriceHistory (Lude.Maybe [InstanceType])
dsphInstanceTypes = Lens.lens (instanceTypes :: DescribeSpotPriceHistory -> Lude.Maybe [InstanceType]) (\s a -> s {instanceTypes = a} :: DescribeSpotPriceHistory)
{-# DEPRECATED dsphInstanceTypes "Use generic-lens or generic-optics with 'instanceTypes' instead." #-}

-- | The date and time, up to the past 90 days, from which to start retrieving the price history data, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsphStartTime :: Lens.Lens' DescribeSpotPriceHistory (Lude.Maybe Lude.ISO8601)
dsphStartTime = Lens.lens (startTime :: DescribeSpotPriceHistory -> Lude.Maybe Lude.ISO8601) (\s a -> s {startTime = a} :: DescribeSpotPriceHistory)
{-# DEPRECATED dsphStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

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
dsphFilters :: Lens.Lens' DescribeSpotPriceHistory (Lude.Maybe [Filter])
dsphFilters = Lens.lens (filters :: DescribeSpotPriceHistory -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeSpotPriceHistory)
{-# DEPRECATED dsphFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsphNextToken :: Lens.Lens' DescribeSpotPriceHistory (Lude.Maybe Lude.Text)
dsphNextToken = Lens.lens (nextToken :: DescribeSpotPriceHistory -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSpotPriceHistory)
{-# DEPRECATED dsphNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filters the results by the specified Availability Zone.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsphAvailabilityZone :: Lens.Lens' DescribeSpotPriceHistory (Lude.Maybe Lude.Text)
dsphAvailabilityZone = Lens.lens (availabilityZone :: DescribeSpotPriceHistory -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: DescribeSpotPriceHistory)
{-# DEPRECATED dsphAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The date and time, up to the current date, from which to stop retrieving the price history data, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsphEndTime :: Lens.Lens' DescribeSpotPriceHistory (Lude.Maybe Lude.ISO8601)
dsphEndTime = Lens.lens (endTime :: DescribeSpotPriceHistory -> Lude.Maybe Lude.ISO8601) (\s a -> s {endTime = a} :: DescribeSpotPriceHistory)
{-# DEPRECATED dsphEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | Filters the results by the specified basic product descriptions.
--
-- /Note:/ Consider using 'productDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsphProductDescriptions :: Lens.Lens' DescribeSpotPriceHistory (Lude.Maybe [Lude.Text])
dsphProductDescriptions = Lens.lens (productDescriptions :: DescribeSpotPriceHistory -> Lude.Maybe [Lude.Text]) (\s a -> s {productDescriptions = a} :: DescribeSpotPriceHistory)
{-# DEPRECATED dsphProductDescriptions "Use generic-lens or generic-optics with 'productDescriptions' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsphDryRun :: Lens.Lens' DescribeSpotPriceHistory (Lude.Maybe Lude.Bool)
dsphDryRun = Lens.lens (dryRun :: DescribeSpotPriceHistory -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeSpotPriceHistory)
{-# DEPRECATED dsphDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsphMaxResults :: Lens.Lens' DescribeSpotPriceHistory (Lude.Maybe Lude.Int)
dsphMaxResults = Lens.lens (maxResults :: DescribeSpotPriceHistory -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeSpotPriceHistory)
{-# DEPRECATED dsphMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeSpotPriceHistory where
  page rq rs
    | Page.stop (rs Lens.^. dsphrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dsphrsSpotPriceHistory) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dsphNextToken Lens..~ rs Lens.^. dsphrsNextToken

instance Lude.AWSRequest DescribeSpotPriceHistory where
  type Rs DescribeSpotPriceHistory = DescribeSpotPriceHistoryResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeSpotPriceHistoryResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "spotPriceHistorySet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSpotPriceHistory where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeSpotPriceHistory where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSpotPriceHistory where
  toQuery DescribeSpotPriceHistory' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeSpotPriceHistory" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "InstanceType" Lude.<$> instanceTypes),
        "StartTime" Lude.=: startTime,
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "AvailabilityZone" Lude.=: availabilityZone,
        "EndTime" Lude.=: endTime,
        Lude.toQuery
          ( Lude.toQueryList "ProductDescription"
              Lude.<$> productDescriptions
          ),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | Contains the output of DescribeSpotPriceHistory.
--
-- /See:/ 'mkDescribeSpotPriceHistoryResponse' smart constructor.
data DescribeSpotPriceHistoryResponse = DescribeSpotPriceHistoryResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    spotPriceHistory ::
      Lude.Maybe [SpotPrice],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSpotPriceHistoryResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token required to retrieve the next set of results. This value is null or an empty string when there are no more results to return.
-- * 'responseStatus' - The response status code.
-- * 'spotPriceHistory' - The historical Spot prices.
mkDescribeSpotPriceHistoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSpotPriceHistoryResponse
mkDescribeSpotPriceHistoryResponse pResponseStatus_ =
  DescribeSpotPriceHistoryResponse'
    { nextToken = Lude.Nothing,
      spotPriceHistory = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token required to retrieve the next set of results. This value is null or an empty string when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsphrsNextToken :: Lens.Lens' DescribeSpotPriceHistoryResponse (Lude.Maybe Lude.Text)
dsphrsNextToken = Lens.lens (nextToken :: DescribeSpotPriceHistoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSpotPriceHistoryResponse)
{-# DEPRECATED dsphrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The historical Spot prices.
--
-- /Note:/ Consider using 'spotPriceHistory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsphrsSpotPriceHistory :: Lens.Lens' DescribeSpotPriceHistoryResponse (Lude.Maybe [SpotPrice])
dsphrsSpotPriceHistory = Lens.lens (spotPriceHistory :: DescribeSpotPriceHistoryResponse -> Lude.Maybe [SpotPrice]) (\s a -> s {spotPriceHistory = a} :: DescribeSpotPriceHistoryResponse)
{-# DEPRECATED dsphrsSpotPriceHistory "Use generic-lens or generic-optics with 'spotPriceHistory' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsphrsResponseStatus :: Lens.Lens' DescribeSpotPriceHistoryResponse Lude.Int
dsphrsResponseStatus = Lens.lens (responseStatus :: DescribeSpotPriceHistoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSpotPriceHistoryResponse)
{-# DEPRECATED dsphrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
