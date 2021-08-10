{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeReservedInstancesOfferings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes Reserved Instance offerings that are available for purchase.
-- With Reserved Instances, you purchase the right to launch instances for
-- a period of time. During that time period, you do not receive
-- insufficient capacity errors, and you pay a lower usage rate than the
-- rate charged for On-Demand instances for the actual time used.
--
-- If you have listed your own Reserved Instances for sale in the Reserved
-- Instance Marketplace, they will be excluded from these results. This is
-- to ensure that you do not purchase your own Reserved Instances.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved Instance Marketplace>
-- in the /Amazon EC2 User Guide/.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeReservedInstancesOfferings
  ( -- * Creating a Request
    DescribeReservedInstancesOfferings (..),
    newDescribeReservedInstancesOfferings,

    -- * Request Lenses
    describeReservedInstancesOfferings_minDuration,
    describeReservedInstancesOfferings_nextToken,
    describeReservedInstancesOfferings_instanceType,
    describeReservedInstancesOfferings_dryRun,
    describeReservedInstancesOfferings_maxInstanceCount,
    describeReservedInstancesOfferings_maxResults,
    describeReservedInstancesOfferings_includeMarketplace,
    describeReservedInstancesOfferings_instanceTenancy,
    describeReservedInstancesOfferings_availabilityZone,
    describeReservedInstancesOfferings_offeringClass,
    describeReservedInstancesOfferings_filters,
    describeReservedInstancesOfferings_offeringType,
    describeReservedInstancesOfferings_reservedInstancesOfferingIds,
    describeReservedInstancesOfferings_productDescription,
    describeReservedInstancesOfferings_maxDuration,

    -- * Destructuring the Response
    DescribeReservedInstancesOfferingsResponse (..),
    newDescribeReservedInstancesOfferingsResponse,

    -- * Response Lenses
    describeReservedInstancesOfferingsResponse_nextToken,
    describeReservedInstancesOfferingsResponse_reservedInstancesOfferings,
    describeReservedInstancesOfferingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeReservedInstancesOfferings.
--
-- /See:/ 'newDescribeReservedInstancesOfferings' smart constructor.
data DescribeReservedInstancesOfferings = DescribeReservedInstancesOfferings'
  { -- | The minimum duration (in seconds) to filter when searching for
    -- offerings.
    --
    -- Default: 2592000 (1 month)
    minDuration :: Prelude.Maybe Prelude.Integer,
    -- | The token to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The instance type that the reservation will cover (for example,
    -- @m1.small@). For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
    -- in the /Amazon EC2 User Guide/.
    instanceType :: Prelude.Maybe InstanceType,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of instances to filter when searching for offerings.
    --
    -- Default: 20
    maxInstanceCount :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results of the initial request can be seen by
    -- sending another request with the returned @NextToken@ value. The maximum
    -- is 100.
    --
    -- Default: 100
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Include Reserved Instance Marketplace offerings in the response.
    includeMarketplace :: Prelude.Maybe Prelude.Bool,
    -- | The tenancy of the instances covered by the reservation. A Reserved
    -- Instance with a tenancy of @dedicated@ is applied to instances that run
    -- in a VPC on single-tenant hardware (i.e., Dedicated Instances).
    --
    -- __Important:__ The @host@ value cannot be used with this parameter. Use
    -- the @default@ or @dedicated@ values only.
    --
    -- Default: @default@
    instanceTenancy :: Prelude.Maybe Tenancy,
    -- | The Availability Zone in which the Reserved Instance can be used.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The offering class of the Reserved Instance. Can be @standard@ or
    -- @convertible@.
    offeringClass :: Prelude.Maybe OfferingClassType,
    -- | One or more filters.
    --
    -- -   @availability-zone@ - The Availability Zone where the Reserved
    --     Instance can be used.
    --
    -- -   @duration@ - The duration of the Reserved Instance (for example, one
    --     year or three years), in seconds (@31536000@ | @94608000@).
    --
    -- -   @fixed-price@ - The purchase price of the Reserved Instance (for
    --     example, 9800.0).
    --
    -- -   @instance-type@ - The instance type that is covered by the
    --     reservation.
    --
    -- -   @marketplace@ - Set to @true@ to show only Reserved Instance
    --     Marketplace offerings. When this filter is not used, which is the
    --     default behavior, all offerings from both AWS and the Reserved
    --     Instance Marketplace are listed.
    --
    -- -   @product-description@ - The Reserved Instance product platform
    --     description. Instances that include @(Amazon VPC)@ in the product
    --     platform description will only be displayed to EC2-Classic account
    --     holders and are for use with Amazon VPC. (@Linux\/UNIX@ |
    --     @Linux\/UNIX (Amazon VPC)@ | @SUSE Linux@ |
    --     @SUSE Linux (Amazon VPC)@ | @Red Hat Enterprise Linux@ |
    --     @Red Hat Enterprise Linux (Amazon VPC)@ | @Windows@ |
    --     @Windows (Amazon VPC)@ | @Windows with SQL Server Standard@ |
    --     @Windows with SQL Server Standard (Amazon VPC)@ |
    --     @Windows with SQL Server Web@ |
    --     @ Windows with SQL Server Web (Amazon VPC)@ |
    --     @Windows with SQL Server Enterprise@ |
    --     @Windows with SQL Server Enterprise (Amazon VPC)@)
    --
    -- -   @reserved-instances-offering-id@ - The Reserved Instances offering
    --     ID.
    --
    -- -   @scope@ - The scope of the Reserved Instance (@Availability Zone@ or
    --     @Region@).
    --
    -- -   @usage-price@ - The usage price of the Reserved Instance, per hour
    --     (for example, 0.84).
    filters :: Prelude.Maybe [Filter],
    -- | The Reserved Instance offering type. If you are using tools that predate
    -- the 2011-11-01 API version, you only have access to the
    -- @Medium Utilization@ Reserved Instance offering type.
    offeringType :: Prelude.Maybe OfferingTypeValues,
    -- | One or more Reserved Instances offering IDs.
    reservedInstancesOfferingIds :: Prelude.Maybe [Prelude.Text],
    -- | The Reserved Instance product platform description. Instances that
    -- include @(Amazon VPC)@ in the description are for use with Amazon VPC.
    productDescription :: Prelude.Maybe RIProductDescription,
    -- | The maximum duration (in seconds) to filter when searching for
    -- offerings.
    --
    -- Default: 94608000 (3 years)
    maxDuration :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedInstancesOfferings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minDuration', 'describeReservedInstancesOfferings_minDuration' - The minimum duration (in seconds) to filter when searching for
-- offerings.
--
-- Default: 2592000 (1 month)
--
-- 'nextToken', 'describeReservedInstancesOfferings_nextToken' - The token to retrieve the next page of results.
--
-- 'instanceType', 'describeReservedInstancesOfferings_instanceType' - The instance type that the reservation will cover (for example,
-- @m1.small@). For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/.
--
-- 'dryRun', 'describeReservedInstancesOfferings_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxInstanceCount', 'describeReservedInstancesOfferings_maxInstanceCount' - The maximum number of instances to filter when searching for offerings.
--
-- Default: 20
--
-- 'maxResults', 'describeReservedInstancesOfferings_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. The maximum
-- is 100.
--
-- Default: 100
--
-- 'includeMarketplace', 'describeReservedInstancesOfferings_includeMarketplace' - Include Reserved Instance Marketplace offerings in the response.
--
-- 'instanceTenancy', 'describeReservedInstancesOfferings_instanceTenancy' - The tenancy of the instances covered by the reservation. A Reserved
-- Instance with a tenancy of @dedicated@ is applied to instances that run
-- in a VPC on single-tenant hardware (i.e., Dedicated Instances).
--
-- __Important:__ The @host@ value cannot be used with this parameter. Use
-- the @default@ or @dedicated@ values only.
--
-- Default: @default@
--
-- 'availabilityZone', 'describeReservedInstancesOfferings_availabilityZone' - The Availability Zone in which the Reserved Instance can be used.
--
-- 'offeringClass', 'describeReservedInstancesOfferings_offeringClass' - The offering class of the Reserved Instance. Can be @standard@ or
-- @convertible@.
--
-- 'filters', 'describeReservedInstancesOfferings_filters' - One or more filters.
--
-- -   @availability-zone@ - The Availability Zone where the Reserved
--     Instance can be used.
--
-- -   @duration@ - The duration of the Reserved Instance (for example, one
--     year or three years), in seconds (@31536000@ | @94608000@).
--
-- -   @fixed-price@ - The purchase price of the Reserved Instance (for
--     example, 9800.0).
--
-- -   @instance-type@ - The instance type that is covered by the
--     reservation.
--
-- -   @marketplace@ - Set to @true@ to show only Reserved Instance
--     Marketplace offerings. When this filter is not used, which is the
--     default behavior, all offerings from both AWS and the Reserved
--     Instance Marketplace are listed.
--
-- -   @product-description@ - The Reserved Instance product platform
--     description. Instances that include @(Amazon VPC)@ in the product
--     platform description will only be displayed to EC2-Classic account
--     holders and are for use with Amazon VPC. (@Linux\/UNIX@ |
--     @Linux\/UNIX (Amazon VPC)@ | @SUSE Linux@ |
--     @SUSE Linux (Amazon VPC)@ | @Red Hat Enterprise Linux@ |
--     @Red Hat Enterprise Linux (Amazon VPC)@ | @Windows@ |
--     @Windows (Amazon VPC)@ | @Windows with SQL Server Standard@ |
--     @Windows with SQL Server Standard (Amazon VPC)@ |
--     @Windows with SQL Server Web@ |
--     @ Windows with SQL Server Web (Amazon VPC)@ |
--     @Windows with SQL Server Enterprise@ |
--     @Windows with SQL Server Enterprise (Amazon VPC)@)
--
-- -   @reserved-instances-offering-id@ - The Reserved Instances offering
--     ID.
--
-- -   @scope@ - The scope of the Reserved Instance (@Availability Zone@ or
--     @Region@).
--
-- -   @usage-price@ - The usage price of the Reserved Instance, per hour
--     (for example, 0.84).
--
-- 'offeringType', 'describeReservedInstancesOfferings_offeringType' - The Reserved Instance offering type. If you are using tools that predate
-- the 2011-11-01 API version, you only have access to the
-- @Medium Utilization@ Reserved Instance offering type.
--
-- 'reservedInstancesOfferingIds', 'describeReservedInstancesOfferings_reservedInstancesOfferingIds' - One or more Reserved Instances offering IDs.
--
-- 'productDescription', 'describeReservedInstancesOfferings_productDescription' - The Reserved Instance product platform description. Instances that
-- include @(Amazon VPC)@ in the description are for use with Amazon VPC.
--
-- 'maxDuration', 'describeReservedInstancesOfferings_maxDuration' - The maximum duration (in seconds) to filter when searching for
-- offerings.
--
-- Default: 94608000 (3 years)
newDescribeReservedInstancesOfferings ::
  DescribeReservedInstancesOfferings
newDescribeReservedInstancesOfferings =
  DescribeReservedInstancesOfferings'
    { minDuration =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxInstanceCount = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      includeMarketplace = Prelude.Nothing,
      instanceTenancy = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      offeringClass = Prelude.Nothing,
      filters = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      reservedInstancesOfferingIds =
        Prelude.Nothing,
      productDescription = Prelude.Nothing,
      maxDuration = Prelude.Nothing
    }

-- | The minimum duration (in seconds) to filter when searching for
-- offerings.
--
-- Default: 2592000 (1 month)
describeReservedInstancesOfferings_minDuration :: Lens.Lens' DescribeReservedInstancesOfferings (Prelude.Maybe Prelude.Integer)
describeReservedInstancesOfferings_minDuration = Lens.lens (\DescribeReservedInstancesOfferings' {minDuration} -> minDuration) (\s@DescribeReservedInstancesOfferings' {} a -> s {minDuration = a} :: DescribeReservedInstancesOfferings)

-- | The token to retrieve the next page of results.
describeReservedInstancesOfferings_nextToken :: Lens.Lens' DescribeReservedInstancesOfferings (Prelude.Maybe Prelude.Text)
describeReservedInstancesOfferings_nextToken = Lens.lens (\DescribeReservedInstancesOfferings' {nextToken} -> nextToken) (\s@DescribeReservedInstancesOfferings' {} a -> s {nextToken = a} :: DescribeReservedInstancesOfferings)

-- | The instance type that the reservation will cover (for example,
-- @m1.small@). For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/.
describeReservedInstancesOfferings_instanceType :: Lens.Lens' DescribeReservedInstancesOfferings (Prelude.Maybe InstanceType)
describeReservedInstancesOfferings_instanceType = Lens.lens (\DescribeReservedInstancesOfferings' {instanceType} -> instanceType) (\s@DescribeReservedInstancesOfferings' {} a -> s {instanceType = a} :: DescribeReservedInstancesOfferings)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeReservedInstancesOfferings_dryRun :: Lens.Lens' DescribeReservedInstancesOfferings (Prelude.Maybe Prelude.Bool)
describeReservedInstancesOfferings_dryRun = Lens.lens (\DescribeReservedInstancesOfferings' {dryRun} -> dryRun) (\s@DescribeReservedInstancesOfferings' {} a -> s {dryRun = a} :: DescribeReservedInstancesOfferings)

-- | The maximum number of instances to filter when searching for offerings.
--
-- Default: 20
describeReservedInstancesOfferings_maxInstanceCount :: Lens.Lens' DescribeReservedInstancesOfferings (Prelude.Maybe Prelude.Int)
describeReservedInstancesOfferings_maxInstanceCount = Lens.lens (\DescribeReservedInstancesOfferings' {maxInstanceCount} -> maxInstanceCount) (\s@DescribeReservedInstancesOfferings' {} a -> s {maxInstanceCount = a} :: DescribeReservedInstancesOfferings)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. The maximum
-- is 100.
--
-- Default: 100
describeReservedInstancesOfferings_maxResults :: Lens.Lens' DescribeReservedInstancesOfferings (Prelude.Maybe Prelude.Int)
describeReservedInstancesOfferings_maxResults = Lens.lens (\DescribeReservedInstancesOfferings' {maxResults} -> maxResults) (\s@DescribeReservedInstancesOfferings' {} a -> s {maxResults = a} :: DescribeReservedInstancesOfferings)

-- | Include Reserved Instance Marketplace offerings in the response.
describeReservedInstancesOfferings_includeMarketplace :: Lens.Lens' DescribeReservedInstancesOfferings (Prelude.Maybe Prelude.Bool)
describeReservedInstancesOfferings_includeMarketplace = Lens.lens (\DescribeReservedInstancesOfferings' {includeMarketplace} -> includeMarketplace) (\s@DescribeReservedInstancesOfferings' {} a -> s {includeMarketplace = a} :: DescribeReservedInstancesOfferings)

-- | The tenancy of the instances covered by the reservation. A Reserved
-- Instance with a tenancy of @dedicated@ is applied to instances that run
-- in a VPC on single-tenant hardware (i.e., Dedicated Instances).
--
-- __Important:__ The @host@ value cannot be used with this parameter. Use
-- the @default@ or @dedicated@ values only.
--
-- Default: @default@
describeReservedInstancesOfferings_instanceTenancy :: Lens.Lens' DescribeReservedInstancesOfferings (Prelude.Maybe Tenancy)
describeReservedInstancesOfferings_instanceTenancy = Lens.lens (\DescribeReservedInstancesOfferings' {instanceTenancy} -> instanceTenancy) (\s@DescribeReservedInstancesOfferings' {} a -> s {instanceTenancy = a} :: DescribeReservedInstancesOfferings)

-- | The Availability Zone in which the Reserved Instance can be used.
describeReservedInstancesOfferings_availabilityZone :: Lens.Lens' DescribeReservedInstancesOfferings (Prelude.Maybe Prelude.Text)
describeReservedInstancesOfferings_availabilityZone = Lens.lens (\DescribeReservedInstancesOfferings' {availabilityZone} -> availabilityZone) (\s@DescribeReservedInstancesOfferings' {} a -> s {availabilityZone = a} :: DescribeReservedInstancesOfferings)

-- | The offering class of the Reserved Instance. Can be @standard@ or
-- @convertible@.
describeReservedInstancesOfferings_offeringClass :: Lens.Lens' DescribeReservedInstancesOfferings (Prelude.Maybe OfferingClassType)
describeReservedInstancesOfferings_offeringClass = Lens.lens (\DescribeReservedInstancesOfferings' {offeringClass} -> offeringClass) (\s@DescribeReservedInstancesOfferings' {} a -> s {offeringClass = a} :: DescribeReservedInstancesOfferings)

-- | One or more filters.
--
-- -   @availability-zone@ - The Availability Zone where the Reserved
--     Instance can be used.
--
-- -   @duration@ - The duration of the Reserved Instance (for example, one
--     year or three years), in seconds (@31536000@ | @94608000@).
--
-- -   @fixed-price@ - The purchase price of the Reserved Instance (for
--     example, 9800.0).
--
-- -   @instance-type@ - The instance type that is covered by the
--     reservation.
--
-- -   @marketplace@ - Set to @true@ to show only Reserved Instance
--     Marketplace offerings. When this filter is not used, which is the
--     default behavior, all offerings from both AWS and the Reserved
--     Instance Marketplace are listed.
--
-- -   @product-description@ - The Reserved Instance product platform
--     description. Instances that include @(Amazon VPC)@ in the product
--     platform description will only be displayed to EC2-Classic account
--     holders and are for use with Amazon VPC. (@Linux\/UNIX@ |
--     @Linux\/UNIX (Amazon VPC)@ | @SUSE Linux@ |
--     @SUSE Linux (Amazon VPC)@ | @Red Hat Enterprise Linux@ |
--     @Red Hat Enterprise Linux (Amazon VPC)@ | @Windows@ |
--     @Windows (Amazon VPC)@ | @Windows with SQL Server Standard@ |
--     @Windows with SQL Server Standard (Amazon VPC)@ |
--     @Windows with SQL Server Web@ |
--     @ Windows with SQL Server Web (Amazon VPC)@ |
--     @Windows with SQL Server Enterprise@ |
--     @Windows with SQL Server Enterprise (Amazon VPC)@)
--
-- -   @reserved-instances-offering-id@ - The Reserved Instances offering
--     ID.
--
-- -   @scope@ - The scope of the Reserved Instance (@Availability Zone@ or
--     @Region@).
--
-- -   @usage-price@ - The usage price of the Reserved Instance, per hour
--     (for example, 0.84).
describeReservedInstancesOfferings_filters :: Lens.Lens' DescribeReservedInstancesOfferings (Prelude.Maybe [Filter])
describeReservedInstancesOfferings_filters = Lens.lens (\DescribeReservedInstancesOfferings' {filters} -> filters) (\s@DescribeReservedInstancesOfferings' {} a -> s {filters = a} :: DescribeReservedInstancesOfferings) Prelude.. Lens.mapping Lens._Coerce

-- | The Reserved Instance offering type. If you are using tools that predate
-- the 2011-11-01 API version, you only have access to the
-- @Medium Utilization@ Reserved Instance offering type.
describeReservedInstancesOfferings_offeringType :: Lens.Lens' DescribeReservedInstancesOfferings (Prelude.Maybe OfferingTypeValues)
describeReservedInstancesOfferings_offeringType = Lens.lens (\DescribeReservedInstancesOfferings' {offeringType} -> offeringType) (\s@DescribeReservedInstancesOfferings' {} a -> s {offeringType = a} :: DescribeReservedInstancesOfferings)

-- | One or more Reserved Instances offering IDs.
describeReservedInstancesOfferings_reservedInstancesOfferingIds :: Lens.Lens' DescribeReservedInstancesOfferings (Prelude.Maybe [Prelude.Text])
describeReservedInstancesOfferings_reservedInstancesOfferingIds = Lens.lens (\DescribeReservedInstancesOfferings' {reservedInstancesOfferingIds} -> reservedInstancesOfferingIds) (\s@DescribeReservedInstancesOfferings' {} a -> s {reservedInstancesOfferingIds = a} :: DescribeReservedInstancesOfferings) Prelude.. Lens.mapping Lens._Coerce

-- | The Reserved Instance product platform description. Instances that
-- include @(Amazon VPC)@ in the description are for use with Amazon VPC.
describeReservedInstancesOfferings_productDescription :: Lens.Lens' DescribeReservedInstancesOfferings (Prelude.Maybe RIProductDescription)
describeReservedInstancesOfferings_productDescription = Lens.lens (\DescribeReservedInstancesOfferings' {productDescription} -> productDescription) (\s@DescribeReservedInstancesOfferings' {} a -> s {productDescription = a} :: DescribeReservedInstancesOfferings)

-- | The maximum duration (in seconds) to filter when searching for
-- offerings.
--
-- Default: 94608000 (3 years)
describeReservedInstancesOfferings_maxDuration :: Lens.Lens' DescribeReservedInstancesOfferings (Prelude.Maybe Prelude.Integer)
describeReservedInstancesOfferings_maxDuration = Lens.lens (\DescribeReservedInstancesOfferings' {maxDuration} -> maxDuration) (\s@DescribeReservedInstancesOfferings' {} a -> s {maxDuration = a} :: DescribeReservedInstancesOfferings)

instance
  Core.AWSPager
    DescribeReservedInstancesOfferings
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReservedInstancesOfferingsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReservedInstancesOfferingsResponse_reservedInstancesOfferings
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeReservedInstancesOfferings_nextToken
          Lens..~ rs
          Lens.^? describeReservedInstancesOfferingsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeReservedInstancesOfferings
  where
  type
    AWSResponse DescribeReservedInstancesOfferings =
      DescribeReservedInstancesOfferingsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeReservedInstancesOfferingsResponse'
            Prelude.<$> (x Core..@? "nextToken")
              Prelude.<*> ( x Core..@? "reservedInstancesOfferingsSet"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Core.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReservedInstancesOfferings

instance
  Prelude.NFData
    DescribeReservedInstancesOfferings

instance
  Core.ToHeaders
    DescribeReservedInstancesOfferings
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DescribeReservedInstancesOfferings
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeReservedInstancesOfferings
  where
  toQuery DescribeReservedInstancesOfferings' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeReservedInstancesOfferings" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "MinDuration" Core.=: minDuration,
        "NextToken" Core.=: nextToken,
        "InstanceType" Core.=: instanceType,
        "DryRun" Core.=: dryRun,
        "MaxInstanceCount" Core.=: maxInstanceCount,
        "MaxResults" Core.=: maxResults,
        "IncludeMarketplace" Core.=: includeMarketplace,
        "InstanceTenancy" Core.=: instanceTenancy,
        "AvailabilityZone" Core.=: availabilityZone,
        "OfferingClass" Core.=: offeringClass,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "OfferingType" Core.=: offeringType,
        Core.toQuery
          ( Core.toQueryList "ReservedInstancesOfferingId"
              Prelude.<$> reservedInstancesOfferingIds
          ),
        "ProductDescription" Core.=: productDescription,
        "MaxDuration" Core.=: maxDuration
      ]

-- | Contains the output of DescribeReservedInstancesOfferings.
--
-- /See:/ 'newDescribeReservedInstancesOfferingsResponse' smart constructor.
data DescribeReservedInstancesOfferingsResponse = DescribeReservedInstancesOfferingsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of Reserved Instances offerings.
    reservedInstancesOfferings :: Prelude.Maybe [ReservedInstancesOffering],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedInstancesOfferingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeReservedInstancesOfferingsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'reservedInstancesOfferings', 'describeReservedInstancesOfferingsResponse_reservedInstancesOfferings' - A list of Reserved Instances offerings.
--
-- 'httpStatus', 'describeReservedInstancesOfferingsResponse_httpStatus' - The response's http status code.
newDescribeReservedInstancesOfferingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReservedInstancesOfferingsResponse
newDescribeReservedInstancesOfferingsResponse
  pHttpStatus_ =
    DescribeReservedInstancesOfferingsResponse'
      { nextToken =
          Prelude.Nothing,
        reservedInstancesOfferings =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeReservedInstancesOfferingsResponse_nextToken :: Lens.Lens' DescribeReservedInstancesOfferingsResponse (Prelude.Maybe Prelude.Text)
describeReservedInstancesOfferingsResponse_nextToken = Lens.lens (\DescribeReservedInstancesOfferingsResponse' {nextToken} -> nextToken) (\s@DescribeReservedInstancesOfferingsResponse' {} a -> s {nextToken = a} :: DescribeReservedInstancesOfferingsResponse)

-- | A list of Reserved Instances offerings.
describeReservedInstancesOfferingsResponse_reservedInstancesOfferings :: Lens.Lens' DescribeReservedInstancesOfferingsResponse (Prelude.Maybe [ReservedInstancesOffering])
describeReservedInstancesOfferingsResponse_reservedInstancesOfferings = Lens.lens (\DescribeReservedInstancesOfferingsResponse' {reservedInstancesOfferings} -> reservedInstancesOfferings) (\s@DescribeReservedInstancesOfferingsResponse' {} a -> s {reservedInstancesOfferings = a} :: DescribeReservedInstancesOfferingsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeReservedInstancesOfferingsResponse_httpStatus :: Lens.Lens' DescribeReservedInstancesOfferingsResponse Prelude.Int
describeReservedInstancesOfferingsResponse_httpStatus = Lens.lens (\DescribeReservedInstancesOfferingsResponse' {httpStatus} -> httpStatus) (\s@DescribeReservedInstancesOfferingsResponse' {} a -> s {httpStatus = a} :: DescribeReservedInstancesOfferingsResponse)

instance
  Prelude.NFData
    DescribeReservedInstancesOfferingsResponse
