{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeReservedInstancesOfferings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes Reserved Instance offerings that are available for purchase. With Reserved Instances, you purchase the right to launch instances for a period of time. During that time period, you do not receive insufficient capacity errors, and you pay a lower usage rate than the rate charged for On-Demand instances for the actual time used.
--
-- If you have listed your own Reserved Instances for sale in the Reserved Instance Marketplace, they will be excluded from these results. This is to ensure that you do not purchase your own Reserved Instances.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved Instance Marketplace> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeReservedInstancesOfferings
  ( -- * Creating a request
    DescribeReservedInstancesOfferings (..),
    mkDescribeReservedInstancesOfferings,

    -- ** Request lenses
    drioMaxDuration,
    drioProductDescription,
    drioFilters,
    drioIncludeMarketplace,
    drioInstanceType,
    drioNextToken,
    drioMinDuration,
    drioAvailabilityZone,
    drioOfferingType,
    drioReservedInstancesOfferingIds,
    drioInstanceTenancy,
    drioOfferingClass,
    drioMaxInstanceCount,
    drioDryRun,
    drioMaxResults,

    -- * Destructuring the response
    DescribeReservedInstancesOfferingsResponse (..),
    mkDescribeReservedInstancesOfferingsResponse,

    -- ** Response lenses
    driorsNextToken,
    driorsReservedInstancesOfferings,
    driorsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeReservedInstancesOfferings.
--
-- /See:/ 'mkDescribeReservedInstancesOfferings' smart constructor.
data DescribeReservedInstancesOfferings = DescribeReservedInstancesOfferings'
  { maxDuration ::
      Lude.Maybe
        Lude.Integer,
    productDescription ::
      Lude.Maybe
        RIProductDescription,
    filters ::
      Lude.Maybe [Filter],
    includeMarketplace ::
      Lude.Maybe Lude.Bool,
    instanceType ::
      Lude.Maybe
        InstanceType,
    nextToken ::
      Lude.Maybe Lude.Text,
    minDuration ::
      Lude.Maybe
        Lude.Integer,
    availabilityZone ::
      Lude.Maybe Lude.Text,
    offeringType ::
      Lude.Maybe
        OfferingTypeValues,
    reservedInstancesOfferingIds ::
      Lude.Maybe
        [Lude.Text],
    instanceTenancy ::
      Lude.Maybe Tenancy,
    offeringClass ::
      Lude.Maybe
        OfferingClassType,
    maxInstanceCount ::
      Lude.Maybe Lude.Int,
    dryRun ::
      Lude.Maybe Lude.Bool,
    maxResults ::
      Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReservedInstancesOfferings' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone in which the Reserved Instance can be used.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters.
--
--
--     * @availability-zone@ - The Availability Zone where the Reserved Instance can be used.
--
--
--     * @duration@ - The duration of the Reserved Instance (for example, one year or three years), in seconds (@31536000@ | @94608000@ ).
--
--
--     * @fixed-price@ - The purchase price of the Reserved Instance (for example, 9800.0).
--
--
--     * @instance-type@ - The instance type that is covered by the reservation.
--
--
--     * @marketplace@ - Set to @true@ to show only Reserved Instance Marketplace offerings. When this filter is not used, which is the default behavior, all offerings from both AWS and the Reserved Instance Marketplace are listed.
--
--
--     * @product-description@ - The Reserved Instance product platform description. Instances that include @(Amazon VPC)@ in the product platform description will only be displayed to EC2-Classic account holders and are for use with Amazon VPC. (@Linux/UNIX@ | @Linux/UNIX (Amazon VPC)@ | @SUSE Linux@ | @SUSE Linux (Amazon VPC)@ | @Red Hat Enterprise Linux@ | @Red Hat Enterprise Linux (Amazon VPC)@ | @Windows@ | @Windows (Amazon VPC)@ | @Windows with SQL Server Standard@ | @Windows with SQL Server Standard (Amazon VPC)@ | @Windows with SQL Server Web@ | @Windows with SQL Server Web (Amazon VPC)@ | @Windows with SQL Server Enterprise@ | @Windows with SQL Server Enterprise (Amazon VPC)@ )
--
--
--     * @reserved-instances-offering-id@ - The Reserved Instances offering ID.
--
--
--     * @scope@ - The scope of the Reserved Instance (@Availability Zone@ or @Region@ ).
--
--
--     * @usage-price@ - The usage price of the Reserved Instance, per hour (for example, 0.84).
--
--
-- * 'includeMarketplace' - Include Reserved Instance Marketplace offerings in the response.
-- * 'instanceTenancy' - The tenancy of the instances covered by the reservation. A Reserved Instance with a tenancy of @dedicated@ is applied to instances that run in a VPC on single-tenant hardware (i.e., Dedicated Instances).
--
-- __Important:__ The @host@ value cannot be used with this parameter. Use the @default@ or @dedicated@ values only.
-- Default: @default@
-- * 'instanceType' - The instance type that the reservation will cover (for example, @m1.small@ ). For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'maxDuration' - The maximum duration (in seconds) to filter when searching for offerings.
--
-- Default: 94608000 (3 years)
-- * 'maxInstanceCount' - The maximum number of instances to filter when searching for offerings.
--
-- Default: 20
-- * 'maxResults' - The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. The maximum is 100.
--
-- Default: 100
-- * 'minDuration' - The minimum duration (in seconds) to filter when searching for offerings.
--
-- Default: 2592000 (1 month)
-- * 'nextToken' - The token to retrieve the next page of results.
-- * 'offeringClass' - The offering class of the Reserved Instance. Can be @standard@ or @convertible@ .
-- * 'offeringType' - The Reserved Instance offering type. If you are using tools that predate the 2011-11-01 API version, you only have access to the @Medium Utilization@ Reserved Instance offering type.
-- * 'productDescription' - The Reserved Instance product platform description. Instances that include @(Amazon VPC)@ in the description are for use with Amazon VPC.
-- * 'reservedInstancesOfferingIds' - One or more Reserved Instances offering IDs.
mkDescribeReservedInstancesOfferings ::
  DescribeReservedInstancesOfferings
mkDescribeReservedInstancesOfferings =
  DescribeReservedInstancesOfferings'
    { maxDuration = Lude.Nothing,
      productDescription = Lude.Nothing,
      filters = Lude.Nothing,
      includeMarketplace = Lude.Nothing,
      instanceType = Lude.Nothing,
      nextToken = Lude.Nothing,
      minDuration = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      offeringType = Lude.Nothing,
      reservedInstancesOfferingIds = Lude.Nothing,
      instanceTenancy = Lude.Nothing,
      offeringClass = Lude.Nothing,
      maxInstanceCount = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The maximum duration (in seconds) to filter when searching for offerings.
--
-- Default: 94608000 (3 years)
--
-- /Note:/ Consider using 'maxDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drioMaxDuration :: Lens.Lens' DescribeReservedInstancesOfferings (Lude.Maybe Lude.Integer)
drioMaxDuration = Lens.lens (maxDuration :: DescribeReservedInstancesOfferings -> Lude.Maybe Lude.Integer) (\s a -> s {maxDuration = a} :: DescribeReservedInstancesOfferings)
{-# DEPRECATED drioMaxDuration "Use generic-lens or generic-optics with 'maxDuration' instead." #-}

-- | The Reserved Instance product platform description. Instances that include @(Amazon VPC)@ in the description are for use with Amazon VPC.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drioProductDescription :: Lens.Lens' DescribeReservedInstancesOfferings (Lude.Maybe RIProductDescription)
drioProductDescription = Lens.lens (productDescription :: DescribeReservedInstancesOfferings -> Lude.Maybe RIProductDescription) (\s a -> s {productDescription = a} :: DescribeReservedInstancesOfferings)
{-# DEPRECATED drioProductDescription "Use generic-lens or generic-optics with 'productDescription' instead." #-}

-- | One or more filters.
--
--
--     * @availability-zone@ - The Availability Zone where the Reserved Instance can be used.
--
--
--     * @duration@ - The duration of the Reserved Instance (for example, one year or three years), in seconds (@31536000@ | @94608000@ ).
--
--
--     * @fixed-price@ - The purchase price of the Reserved Instance (for example, 9800.0).
--
--
--     * @instance-type@ - The instance type that is covered by the reservation.
--
--
--     * @marketplace@ - Set to @true@ to show only Reserved Instance Marketplace offerings. When this filter is not used, which is the default behavior, all offerings from both AWS and the Reserved Instance Marketplace are listed.
--
--
--     * @product-description@ - The Reserved Instance product platform description. Instances that include @(Amazon VPC)@ in the product platform description will only be displayed to EC2-Classic account holders and are for use with Amazon VPC. (@Linux/UNIX@ | @Linux/UNIX (Amazon VPC)@ | @SUSE Linux@ | @SUSE Linux (Amazon VPC)@ | @Red Hat Enterprise Linux@ | @Red Hat Enterprise Linux (Amazon VPC)@ | @Windows@ | @Windows (Amazon VPC)@ | @Windows with SQL Server Standard@ | @Windows with SQL Server Standard (Amazon VPC)@ | @Windows with SQL Server Web@ | @Windows with SQL Server Web (Amazon VPC)@ | @Windows with SQL Server Enterprise@ | @Windows with SQL Server Enterprise (Amazon VPC)@ )
--
--
--     * @reserved-instances-offering-id@ - The Reserved Instances offering ID.
--
--
--     * @scope@ - The scope of the Reserved Instance (@Availability Zone@ or @Region@ ).
--
--
--     * @usage-price@ - The usage price of the Reserved Instance, per hour (for example, 0.84).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drioFilters :: Lens.Lens' DescribeReservedInstancesOfferings (Lude.Maybe [Filter])
drioFilters = Lens.lens (filters :: DescribeReservedInstancesOfferings -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeReservedInstancesOfferings)
{-# DEPRECATED drioFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | Include Reserved Instance Marketplace offerings in the response.
--
-- /Note:/ Consider using 'includeMarketplace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drioIncludeMarketplace :: Lens.Lens' DescribeReservedInstancesOfferings (Lude.Maybe Lude.Bool)
drioIncludeMarketplace = Lens.lens (includeMarketplace :: DescribeReservedInstancesOfferings -> Lude.Maybe Lude.Bool) (\s a -> s {includeMarketplace = a} :: DescribeReservedInstancesOfferings)
{-# DEPRECATED drioIncludeMarketplace "Use generic-lens or generic-optics with 'includeMarketplace' instead." #-}

-- | The instance type that the reservation will cover (for example, @m1.small@ ). For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drioInstanceType :: Lens.Lens' DescribeReservedInstancesOfferings (Lude.Maybe InstanceType)
drioInstanceType = Lens.lens (instanceType :: DescribeReservedInstancesOfferings -> Lude.Maybe InstanceType) (\s a -> s {instanceType = a} :: DescribeReservedInstancesOfferings)
{-# DEPRECATED drioInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drioNextToken :: Lens.Lens' DescribeReservedInstancesOfferings (Lude.Maybe Lude.Text)
drioNextToken = Lens.lens (nextToken :: DescribeReservedInstancesOfferings -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeReservedInstancesOfferings)
{-# DEPRECATED drioNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The minimum duration (in seconds) to filter when searching for offerings.
--
-- Default: 2592000 (1 month)
--
-- /Note:/ Consider using 'minDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drioMinDuration :: Lens.Lens' DescribeReservedInstancesOfferings (Lude.Maybe Lude.Integer)
drioMinDuration = Lens.lens (minDuration :: DescribeReservedInstancesOfferings -> Lude.Maybe Lude.Integer) (\s a -> s {minDuration = a} :: DescribeReservedInstancesOfferings)
{-# DEPRECATED drioMinDuration "Use generic-lens or generic-optics with 'minDuration' instead." #-}

-- | The Availability Zone in which the Reserved Instance can be used.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drioAvailabilityZone :: Lens.Lens' DescribeReservedInstancesOfferings (Lude.Maybe Lude.Text)
drioAvailabilityZone = Lens.lens (availabilityZone :: DescribeReservedInstancesOfferings -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: DescribeReservedInstancesOfferings)
{-# DEPRECATED drioAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The Reserved Instance offering type. If you are using tools that predate the 2011-11-01 API version, you only have access to the @Medium Utilization@ Reserved Instance offering type.
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drioOfferingType :: Lens.Lens' DescribeReservedInstancesOfferings (Lude.Maybe OfferingTypeValues)
drioOfferingType = Lens.lens (offeringType :: DescribeReservedInstancesOfferings -> Lude.Maybe OfferingTypeValues) (\s a -> s {offeringType = a} :: DescribeReservedInstancesOfferings)
{-# DEPRECATED drioOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | One or more Reserved Instances offering IDs.
--
-- /Note:/ Consider using 'reservedInstancesOfferingIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drioReservedInstancesOfferingIds :: Lens.Lens' DescribeReservedInstancesOfferings (Lude.Maybe [Lude.Text])
drioReservedInstancesOfferingIds = Lens.lens (reservedInstancesOfferingIds :: DescribeReservedInstancesOfferings -> Lude.Maybe [Lude.Text]) (\s a -> s {reservedInstancesOfferingIds = a} :: DescribeReservedInstancesOfferings)
{-# DEPRECATED drioReservedInstancesOfferingIds "Use generic-lens or generic-optics with 'reservedInstancesOfferingIds' instead." #-}

-- | The tenancy of the instances covered by the reservation. A Reserved Instance with a tenancy of @dedicated@ is applied to instances that run in a VPC on single-tenant hardware (i.e., Dedicated Instances).
--
-- __Important:__ The @host@ value cannot be used with this parameter. Use the @default@ or @dedicated@ values only.
-- Default: @default@
--
-- /Note:/ Consider using 'instanceTenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drioInstanceTenancy :: Lens.Lens' DescribeReservedInstancesOfferings (Lude.Maybe Tenancy)
drioInstanceTenancy = Lens.lens (instanceTenancy :: DescribeReservedInstancesOfferings -> Lude.Maybe Tenancy) (\s a -> s {instanceTenancy = a} :: DescribeReservedInstancesOfferings)
{-# DEPRECATED drioInstanceTenancy "Use generic-lens or generic-optics with 'instanceTenancy' instead." #-}

-- | The offering class of the Reserved Instance. Can be @standard@ or @convertible@ .
--
-- /Note:/ Consider using 'offeringClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drioOfferingClass :: Lens.Lens' DescribeReservedInstancesOfferings (Lude.Maybe OfferingClassType)
drioOfferingClass = Lens.lens (offeringClass :: DescribeReservedInstancesOfferings -> Lude.Maybe OfferingClassType) (\s a -> s {offeringClass = a} :: DescribeReservedInstancesOfferings)
{-# DEPRECATED drioOfferingClass "Use generic-lens or generic-optics with 'offeringClass' instead." #-}

-- | The maximum number of instances to filter when searching for offerings.
--
-- Default: 20
--
-- /Note:/ Consider using 'maxInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drioMaxInstanceCount :: Lens.Lens' DescribeReservedInstancesOfferings (Lude.Maybe Lude.Int)
drioMaxInstanceCount = Lens.lens (maxInstanceCount :: DescribeReservedInstancesOfferings -> Lude.Maybe Lude.Int) (\s a -> s {maxInstanceCount = a} :: DescribeReservedInstancesOfferings)
{-# DEPRECATED drioMaxInstanceCount "Use generic-lens or generic-optics with 'maxInstanceCount' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drioDryRun :: Lens.Lens' DescribeReservedInstancesOfferings (Lude.Maybe Lude.Bool)
drioDryRun = Lens.lens (dryRun :: DescribeReservedInstancesOfferings -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeReservedInstancesOfferings)
{-# DEPRECATED drioDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. The maximum is 100.
--
-- Default: 100
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drioMaxResults :: Lens.Lens' DescribeReservedInstancesOfferings (Lude.Maybe Lude.Int)
drioMaxResults = Lens.lens (maxResults :: DescribeReservedInstancesOfferings -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeReservedInstancesOfferings)
{-# DEPRECATED drioMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeReservedInstancesOfferings where
  page rq rs
    | Page.stop (rs Lens.^. driorsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. driorsReservedInstancesOfferings) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& drioNextToken Lens..~ rs Lens.^. driorsNextToken

instance Lude.AWSRequest DescribeReservedInstancesOfferings where
  type
    Rs DescribeReservedInstancesOfferings =
      DescribeReservedInstancesOfferingsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeReservedInstancesOfferingsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "reservedInstancesOfferingsSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReservedInstancesOfferings where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeReservedInstancesOfferings where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReservedInstancesOfferings where
  toQuery DescribeReservedInstancesOfferings' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeReservedInstancesOfferings" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "MaxDuration" Lude.=: maxDuration,
        "ProductDescription" Lude.=: productDescription,
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "IncludeMarketplace" Lude.=: includeMarketplace,
        "InstanceType" Lude.=: instanceType,
        "NextToken" Lude.=: nextToken,
        "MinDuration" Lude.=: minDuration,
        "AvailabilityZone" Lude.=: availabilityZone,
        "OfferingType" Lude.=: offeringType,
        Lude.toQuery
          ( Lude.toQueryList "ReservedInstancesOfferingId"
              Lude.<$> reservedInstancesOfferingIds
          ),
        "InstanceTenancy" Lude.=: instanceTenancy,
        "OfferingClass" Lude.=: offeringClass,
        "MaxInstanceCount" Lude.=: maxInstanceCount,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | Contains the output of DescribeReservedInstancesOfferings.
--
-- /See:/ 'mkDescribeReservedInstancesOfferingsResponse' smart constructor.
data DescribeReservedInstancesOfferingsResponse = DescribeReservedInstancesOfferingsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    reservedInstancesOfferings ::
      Lude.Maybe
        [ReservedInstancesOffering],
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

-- | Creates a value of 'DescribeReservedInstancesOfferingsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'reservedInstancesOfferings' - A list of Reserved Instances offerings.
-- * 'responseStatus' - The response status code.
mkDescribeReservedInstancesOfferingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReservedInstancesOfferingsResponse
mkDescribeReservedInstancesOfferingsResponse pResponseStatus_ =
  DescribeReservedInstancesOfferingsResponse'
    { nextToken =
        Lude.Nothing,
      reservedInstancesOfferings = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
driorsNextToken :: Lens.Lens' DescribeReservedInstancesOfferingsResponse (Lude.Maybe Lude.Text)
driorsNextToken = Lens.lens (nextToken :: DescribeReservedInstancesOfferingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeReservedInstancesOfferingsResponse)
{-# DEPRECATED driorsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of Reserved Instances offerings.
--
-- /Note:/ Consider using 'reservedInstancesOfferings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
driorsReservedInstancesOfferings :: Lens.Lens' DescribeReservedInstancesOfferingsResponse (Lude.Maybe [ReservedInstancesOffering])
driorsReservedInstancesOfferings = Lens.lens (reservedInstancesOfferings :: DescribeReservedInstancesOfferingsResponse -> Lude.Maybe [ReservedInstancesOffering]) (\s a -> s {reservedInstancesOfferings = a} :: DescribeReservedInstancesOfferingsResponse)
{-# DEPRECATED driorsReservedInstancesOfferings "Use generic-lens or generic-optics with 'reservedInstancesOfferings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
driorsResponseStatus :: Lens.Lens' DescribeReservedInstancesOfferingsResponse Lude.Int
driorsResponseStatus = Lens.lens (responseStatus :: DescribeReservedInstancesOfferingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReservedInstancesOfferingsResponse)
{-# DEPRECATED driorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
