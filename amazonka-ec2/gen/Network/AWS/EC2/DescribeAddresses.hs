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
-- Module      : Network.AWS.EC2.DescribeAddresses
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Elastic IP addresses or all of your Elastic IP
-- addresses.
--
-- An Elastic IP address is for use in either the EC2-Classic platform or
-- in a VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.DescribeAddresses
  ( -- * Creating a Request
    DescribeAddresses (..),
    newDescribeAddresses,

    -- * Request Lenses
    describeAddresses_dryRun,
    describeAddresses_allocationIds,
    describeAddresses_publicIps,
    describeAddresses_filters,

    -- * Destructuring the Response
    DescribeAddressesResponse (..),
    newDescribeAddressesResponse,

    -- * Response Lenses
    describeAddressesResponse_addresses,
    describeAddressesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAddresses' smart constructor.
data DescribeAddresses = DescribeAddresses'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | [EC2-VPC] Information about the allocation IDs.
    allocationIds :: Core.Maybe [Core.Text],
    -- | One or more Elastic IP addresses.
    --
    -- Default: Describes all your Elastic IP addresses.
    publicIps :: Core.Maybe [Core.Text],
    -- | One or more filters. Filter names and values are case-sensitive.
    --
    -- -   @allocation-id@ - [EC2-VPC] The allocation ID for the address.
    --
    -- -   @association-id@ - [EC2-VPC] The association ID for the address.
    --
    -- -   @domain@ - Indicates whether the address is for use in EC2-Classic
    --     (@standard@) or in a VPC (@vpc@).
    --
    -- -   @instance-id@ - The ID of the instance the address is associated
    --     with, if any.
    --
    -- -   @network-border-group@ - A unique set of Availability Zones, Local
    --     Zones, or Wavelength Zones from where AWS advertises IP addresses.
    --
    -- -   @network-interface-id@ - [EC2-VPC] The ID of the network interface
    --     that the address is associated with, if any.
    --
    -- -   @network-interface-owner-id@ - The AWS account ID of the owner.
    --
    -- -   @private-ip-address@ - [EC2-VPC] The private IP address associated
    --     with the Elastic IP address.
    --
    -- -   @public-ip@ - The Elastic IP address, or the carrier IP address.
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
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAddresses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeAddresses_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'allocationIds', 'describeAddresses_allocationIds' - [EC2-VPC] Information about the allocation IDs.
--
-- 'publicIps', 'describeAddresses_publicIps' - One or more Elastic IP addresses.
--
-- Default: Describes all your Elastic IP addresses.
--
-- 'filters', 'describeAddresses_filters' - One or more filters. Filter names and values are case-sensitive.
--
-- -   @allocation-id@ - [EC2-VPC] The allocation ID for the address.
--
-- -   @association-id@ - [EC2-VPC] The association ID for the address.
--
-- -   @domain@ - Indicates whether the address is for use in EC2-Classic
--     (@standard@) or in a VPC (@vpc@).
--
-- -   @instance-id@ - The ID of the instance the address is associated
--     with, if any.
--
-- -   @network-border-group@ - A unique set of Availability Zones, Local
--     Zones, or Wavelength Zones from where AWS advertises IP addresses.
--
-- -   @network-interface-id@ - [EC2-VPC] The ID of the network interface
--     that the address is associated with, if any.
--
-- -   @network-interface-owner-id@ - The AWS account ID of the owner.
--
-- -   @private-ip-address@ - [EC2-VPC] The private IP address associated
--     with the Elastic IP address.
--
-- -   @public-ip@ - The Elastic IP address, or the carrier IP address.
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
newDescribeAddresses ::
  DescribeAddresses
newDescribeAddresses =
  DescribeAddresses'
    { dryRun = Core.Nothing,
      allocationIds = Core.Nothing,
      publicIps = Core.Nothing,
      filters = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeAddresses_dryRun :: Lens.Lens' DescribeAddresses (Core.Maybe Core.Bool)
describeAddresses_dryRun = Lens.lens (\DescribeAddresses' {dryRun} -> dryRun) (\s@DescribeAddresses' {} a -> s {dryRun = a} :: DescribeAddresses)

-- | [EC2-VPC] Information about the allocation IDs.
describeAddresses_allocationIds :: Lens.Lens' DescribeAddresses (Core.Maybe [Core.Text])
describeAddresses_allocationIds = Lens.lens (\DescribeAddresses' {allocationIds} -> allocationIds) (\s@DescribeAddresses' {} a -> s {allocationIds = a} :: DescribeAddresses) Core.. Lens.mapping Lens._Coerce

-- | One or more Elastic IP addresses.
--
-- Default: Describes all your Elastic IP addresses.
describeAddresses_publicIps :: Lens.Lens' DescribeAddresses (Core.Maybe [Core.Text])
describeAddresses_publicIps = Lens.lens (\DescribeAddresses' {publicIps} -> publicIps) (\s@DescribeAddresses' {} a -> s {publicIps = a} :: DescribeAddresses) Core.. Lens.mapping Lens._Coerce

-- | One or more filters. Filter names and values are case-sensitive.
--
-- -   @allocation-id@ - [EC2-VPC] The allocation ID for the address.
--
-- -   @association-id@ - [EC2-VPC] The association ID for the address.
--
-- -   @domain@ - Indicates whether the address is for use in EC2-Classic
--     (@standard@) or in a VPC (@vpc@).
--
-- -   @instance-id@ - The ID of the instance the address is associated
--     with, if any.
--
-- -   @network-border-group@ - A unique set of Availability Zones, Local
--     Zones, or Wavelength Zones from where AWS advertises IP addresses.
--
-- -   @network-interface-id@ - [EC2-VPC] The ID of the network interface
--     that the address is associated with, if any.
--
-- -   @network-interface-owner-id@ - The AWS account ID of the owner.
--
-- -   @private-ip-address@ - [EC2-VPC] The private IP address associated
--     with the Elastic IP address.
--
-- -   @public-ip@ - The Elastic IP address, or the carrier IP address.
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
describeAddresses_filters :: Lens.Lens' DescribeAddresses (Core.Maybe [Filter])
describeAddresses_filters = Lens.lens (\DescribeAddresses' {filters} -> filters) (\s@DescribeAddresses' {} a -> s {filters = a} :: DescribeAddresses) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeAddresses where
  type
    AWSResponse DescribeAddresses =
      DescribeAddressesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeAddressesResponse'
            Core.<$> ( x Core..@? "addressesSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAddresses

instance Core.NFData DescribeAddresses

instance Core.ToHeaders DescribeAddresses where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeAddresses where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAddresses where
  toQuery DescribeAddresses' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeAddresses" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "AllocationId"
              Core.<$> allocationIds
          ),
        Core.toQuery
          (Core.toQueryList "PublicIp" Core.<$> publicIps),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeAddressesResponse' smart constructor.
data DescribeAddressesResponse = DescribeAddressesResponse'
  { -- | Information about the Elastic IP addresses.
    addresses :: Core.Maybe [Address],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAddressesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addresses', 'describeAddressesResponse_addresses' - Information about the Elastic IP addresses.
--
-- 'httpStatus', 'describeAddressesResponse_httpStatus' - The response's http status code.
newDescribeAddressesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeAddressesResponse
newDescribeAddressesResponse pHttpStatus_ =
  DescribeAddressesResponse'
    { addresses =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Elastic IP addresses.
describeAddressesResponse_addresses :: Lens.Lens' DescribeAddressesResponse (Core.Maybe [Address])
describeAddressesResponse_addresses = Lens.lens (\DescribeAddressesResponse' {addresses} -> addresses) (\s@DescribeAddressesResponse' {} a -> s {addresses = a} :: DescribeAddressesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAddressesResponse_httpStatus :: Lens.Lens' DescribeAddressesResponse Core.Int
describeAddressesResponse_httpStatus = Lens.lens (\DescribeAddressesResponse' {httpStatus} -> httpStatus) (\s@DescribeAddressesResponse' {} a -> s {httpStatus = a} :: DescribeAddressesResponse)

instance Core.NFData DescribeAddressesResponse
