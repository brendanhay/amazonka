{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.DescribeReservedInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of the Reserved Instances that you purchased.
--
-- For more information about Reserved Instances, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/concepts-on-demand-reserved-instances.html Reserved Instances>
-- in the /Amazon EC2 User Guide/.
module Network.AWS.EC2.DescribeReservedInstances
  ( -- * Creating a Request
    DescribeReservedInstances (..),
    newDescribeReservedInstances,

    -- * Request Lenses
    describeReservedInstances_dryRun,
    describeReservedInstances_offeringClass,
    describeReservedInstances_filters,
    describeReservedInstances_offeringType,
    describeReservedInstances_reservedInstancesIds,

    -- * Destructuring the Response
    DescribeReservedInstancesResponse (..),
    newDescribeReservedInstancesResponse,

    -- * Response Lenses
    describeReservedInstancesResponse_reservedInstances,
    describeReservedInstancesResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeReservedInstances.
--
-- /See:/ 'newDescribeReservedInstances' smart constructor.
data DescribeReservedInstances = DescribeReservedInstances'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Describes whether the Reserved Instance is Standard or Convertible.
    offeringClass :: Prelude.Maybe OfferingClassType,
    -- | One or more filters.
    --
    -- -   @availability-zone@ - The Availability Zone where the Reserved
    --     Instance can be used.
    --
    -- -   @duration@ - The duration of the Reserved Instance (one year or
    --     three years), in seconds (@31536000@ | @94608000@).
    --
    -- -   @end@ - The time when the Reserved Instance expires (for example,
    --     2015-08-07T11:54:42.000Z).
    --
    -- -   @fixed-price@ - The purchase price of the Reserved Instance (for
    --     example, 9800.0).
    --
    -- -   @instance-type@ - The instance type that is covered by the
    --     reservation.
    --
    -- -   @scope@ - The scope of the Reserved Instance (@Region@ or
    --     @Availability Zone@).
    --
    -- -   @product-description@ - The Reserved Instance product platform
    --     description. Instances that include @(Amazon VPC)@ in the product
    --     platform description will only be displayed to EC2-Classic account
    --     holders and are for use with Amazon VPC (@Linux\/UNIX@ |
    --     @Linux\/UNIX (Amazon VPC)@ | @SUSE Linux@ |
    --     @SUSE Linux (Amazon VPC)@ | @Red Hat Enterprise Linux@ |
    --     @Red Hat Enterprise Linux (Amazon VPC)@ | @Windows@ |
    --     @Windows (Amazon VPC)@ | @Windows with SQL Server Standard@ |
    --     @Windows with SQL Server Standard (Amazon VPC)@ |
    --     @Windows with SQL Server Web@ |
    --     @Windows with SQL Server Web (Amazon VPC)@ |
    --     @Windows with SQL Server Enterprise@ |
    --     @Windows with SQL Server Enterprise (Amazon VPC)@).
    --
    -- -   @reserved-instances-id@ - The ID of the Reserved Instance.
    --
    -- -   @start@ - The time at which the Reserved Instance purchase request
    --     was placed (for example, 2014-08-07T11:54:42.000Z).
    --
    -- -   @state@ - The state of the Reserved Instance (@payment-pending@ |
    --     @active@ | @payment-failed@ | @retired@).
    --
    -- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
    --     resource. Use the tag key in the filter name and the tag value as
    --     the filter value. For example, to find all resources that have a tag
    --     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
    --     the filter name and @TeamA@ for the filter value.
    --
    -- -   @tag-key@ - The key of a tag assigned to the resource. Use this
    --     filter to find all resources assigned a tag with a specific key,
    --     regardless of the tag value.
    --
    -- -   @usage-price@ - The usage price of the Reserved Instance, per hour
    --     (for example, 0.84).
    filters :: Prelude.Maybe [Filter],
    -- | The Reserved Instance offering type. If you are using tools that predate
    -- the 2011-11-01 API version, you only have access to the
    -- @Medium Utilization@ Reserved Instance offering type.
    offeringType :: Prelude.Maybe OfferingTypeValues,
    -- | One or more Reserved Instance IDs.
    --
    -- Default: Describes all your Reserved Instances, or only those otherwise
    -- specified.
    reservedInstancesIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeReservedInstances_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'offeringClass', 'describeReservedInstances_offeringClass' - Describes whether the Reserved Instance is Standard or Convertible.
--
-- 'filters', 'describeReservedInstances_filters' - One or more filters.
--
-- -   @availability-zone@ - The Availability Zone where the Reserved
--     Instance can be used.
--
-- -   @duration@ - The duration of the Reserved Instance (one year or
--     three years), in seconds (@31536000@ | @94608000@).
--
-- -   @end@ - The time when the Reserved Instance expires (for example,
--     2015-08-07T11:54:42.000Z).
--
-- -   @fixed-price@ - The purchase price of the Reserved Instance (for
--     example, 9800.0).
--
-- -   @instance-type@ - The instance type that is covered by the
--     reservation.
--
-- -   @scope@ - The scope of the Reserved Instance (@Region@ or
--     @Availability Zone@).
--
-- -   @product-description@ - The Reserved Instance product platform
--     description. Instances that include @(Amazon VPC)@ in the product
--     platform description will only be displayed to EC2-Classic account
--     holders and are for use with Amazon VPC (@Linux\/UNIX@ |
--     @Linux\/UNIX (Amazon VPC)@ | @SUSE Linux@ |
--     @SUSE Linux (Amazon VPC)@ | @Red Hat Enterprise Linux@ |
--     @Red Hat Enterprise Linux (Amazon VPC)@ | @Windows@ |
--     @Windows (Amazon VPC)@ | @Windows with SQL Server Standard@ |
--     @Windows with SQL Server Standard (Amazon VPC)@ |
--     @Windows with SQL Server Web@ |
--     @Windows with SQL Server Web (Amazon VPC)@ |
--     @Windows with SQL Server Enterprise@ |
--     @Windows with SQL Server Enterprise (Amazon VPC)@).
--
-- -   @reserved-instances-id@ - The ID of the Reserved Instance.
--
-- -   @start@ - The time at which the Reserved Instance purchase request
--     was placed (for example, 2014-08-07T11:54:42.000Z).
--
-- -   @state@ - The state of the Reserved Instance (@payment-pending@ |
--     @active@ | @payment-failed@ | @retired@).
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @usage-price@ - The usage price of the Reserved Instance, per hour
--     (for example, 0.84).
--
-- 'offeringType', 'describeReservedInstances_offeringType' - The Reserved Instance offering type. If you are using tools that predate
-- the 2011-11-01 API version, you only have access to the
-- @Medium Utilization@ Reserved Instance offering type.
--
-- 'reservedInstancesIds', 'describeReservedInstances_reservedInstancesIds' - One or more Reserved Instance IDs.
--
-- Default: Describes all your Reserved Instances, or only those otherwise
-- specified.
newDescribeReservedInstances ::
  DescribeReservedInstances
newDescribeReservedInstances =
  DescribeReservedInstances'
    { dryRun =
        Prelude.Nothing,
      offeringClass = Prelude.Nothing,
      filters = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      reservedInstancesIds = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeReservedInstances_dryRun :: Lens.Lens' DescribeReservedInstances (Prelude.Maybe Prelude.Bool)
describeReservedInstances_dryRun = Lens.lens (\DescribeReservedInstances' {dryRun} -> dryRun) (\s@DescribeReservedInstances' {} a -> s {dryRun = a} :: DescribeReservedInstances)

-- | Describes whether the Reserved Instance is Standard or Convertible.
describeReservedInstances_offeringClass :: Lens.Lens' DescribeReservedInstances (Prelude.Maybe OfferingClassType)
describeReservedInstances_offeringClass = Lens.lens (\DescribeReservedInstances' {offeringClass} -> offeringClass) (\s@DescribeReservedInstances' {} a -> s {offeringClass = a} :: DescribeReservedInstances)

-- | One or more filters.
--
-- -   @availability-zone@ - The Availability Zone where the Reserved
--     Instance can be used.
--
-- -   @duration@ - The duration of the Reserved Instance (one year or
--     three years), in seconds (@31536000@ | @94608000@).
--
-- -   @end@ - The time when the Reserved Instance expires (for example,
--     2015-08-07T11:54:42.000Z).
--
-- -   @fixed-price@ - The purchase price of the Reserved Instance (for
--     example, 9800.0).
--
-- -   @instance-type@ - The instance type that is covered by the
--     reservation.
--
-- -   @scope@ - The scope of the Reserved Instance (@Region@ or
--     @Availability Zone@).
--
-- -   @product-description@ - The Reserved Instance product platform
--     description. Instances that include @(Amazon VPC)@ in the product
--     platform description will only be displayed to EC2-Classic account
--     holders and are for use with Amazon VPC (@Linux\/UNIX@ |
--     @Linux\/UNIX (Amazon VPC)@ | @SUSE Linux@ |
--     @SUSE Linux (Amazon VPC)@ | @Red Hat Enterprise Linux@ |
--     @Red Hat Enterprise Linux (Amazon VPC)@ | @Windows@ |
--     @Windows (Amazon VPC)@ | @Windows with SQL Server Standard@ |
--     @Windows with SQL Server Standard (Amazon VPC)@ |
--     @Windows with SQL Server Web@ |
--     @Windows with SQL Server Web (Amazon VPC)@ |
--     @Windows with SQL Server Enterprise@ |
--     @Windows with SQL Server Enterprise (Amazon VPC)@).
--
-- -   @reserved-instances-id@ - The ID of the Reserved Instance.
--
-- -   @start@ - The time at which the Reserved Instance purchase request
--     was placed (for example, 2014-08-07T11:54:42.000Z).
--
-- -   @state@ - The state of the Reserved Instance (@payment-pending@ |
--     @active@ | @payment-failed@ | @retired@).
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @usage-price@ - The usage price of the Reserved Instance, per hour
--     (for example, 0.84).
describeReservedInstances_filters :: Lens.Lens' DescribeReservedInstances (Prelude.Maybe [Filter])
describeReservedInstances_filters = Lens.lens (\DescribeReservedInstances' {filters} -> filters) (\s@DescribeReservedInstances' {} a -> s {filters = a} :: DescribeReservedInstances) Prelude.. Lens.mapping Prelude._Coerce

-- | The Reserved Instance offering type. If you are using tools that predate
-- the 2011-11-01 API version, you only have access to the
-- @Medium Utilization@ Reserved Instance offering type.
describeReservedInstances_offeringType :: Lens.Lens' DescribeReservedInstances (Prelude.Maybe OfferingTypeValues)
describeReservedInstances_offeringType = Lens.lens (\DescribeReservedInstances' {offeringType} -> offeringType) (\s@DescribeReservedInstances' {} a -> s {offeringType = a} :: DescribeReservedInstances)

-- | One or more Reserved Instance IDs.
--
-- Default: Describes all your Reserved Instances, or only those otherwise
-- specified.
describeReservedInstances_reservedInstancesIds :: Lens.Lens' DescribeReservedInstances (Prelude.Maybe [Prelude.Text])
describeReservedInstances_reservedInstancesIds = Lens.lens (\DescribeReservedInstances' {reservedInstancesIds} -> reservedInstancesIds) (\s@DescribeReservedInstances' {} a -> s {reservedInstancesIds = a} :: DescribeReservedInstances) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.AWSRequest DescribeReservedInstances where
  type
    Rs DescribeReservedInstances =
      DescribeReservedInstancesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeReservedInstancesResponse'
            Prelude.<$> ( x Prelude..@? "reservedInstancesSet"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeReservedInstances

instance Prelude.NFData DescribeReservedInstances

instance Prelude.ToHeaders DescribeReservedInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeReservedInstances where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeReservedInstances where
  toQuery DescribeReservedInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeReservedInstances" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "OfferingClass" Prelude.=: offeringClass,
        Prelude.toQuery
          (Prelude.toQueryList "Filter" Prelude.<$> filters),
        "OfferingType" Prelude.=: offeringType,
        Prelude.toQuery
          ( Prelude.toQueryList "ReservedInstancesId"
              Prelude.<$> reservedInstancesIds
          )
      ]

-- | Contains the output for DescribeReservedInstances.
--
-- /See:/ 'newDescribeReservedInstancesResponse' smart constructor.
data DescribeReservedInstancesResponse = DescribeReservedInstancesResponse'
  { -- | A list of Reserved Instances.
    reservedInstances :: Prelude.Maybe [ReservedInstances],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedInstances', 'describeReservedInstancesResponse_reservedInstances' - A list of Reserved Instances.
--
-- 'httpStatus', 'describeReservedInstancesResponse_httpStatus' - The response's http status code.
newDescribeReservedInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReservedInstancesResponse
newDescribeReservedInstancesResponse pHttpStatus_ =
  DescribeReservedInstancesResponse'
    { reservedInstances =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of Reserved Instances.
describeReservedInstancesResponse_reservedInstances :: Lens.Lens' DescribeReservedInstancesResponse (Prelude.Maybe [ReservedInstances])
describeReservedInstancesResponse_reservedInstances = Lens.lens (\DescribeReservedInstancesResponse' {reservedInstances} -> reservedInstances) (\s@DescribeReservedInstancesResponse' {} a -> s {reservedInstances = a} :: DescribeReservedInstancesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeReservedInstancesResponse_httpStatus :: Lens.Lens' DescribeReservedInstancesResponse Prelude.Int
describeReservedInstancesResponse_httpStatus = Lens.lens (\DescribeReservedInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeReservedInstancesResponse' {} a -> s {httpStatus = a} :: DescribeReservedInstancesResponse)

instance
  Prelude.NFData
    DescribeReservedInstancesResponse
