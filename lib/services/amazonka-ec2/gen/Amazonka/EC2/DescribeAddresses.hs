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
-- Module      : Amazonka.EC2.DescribeAddresses
-- Copyright   : (c) 2013-2022 Brendan Hay
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
--
-- We are retiring EC2-Classic. We recommend that you migrate from
-- EC2-Classic to a VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.DescribeAddresses
  ( -- * Creating a Request
    DescribeAddresses (..),
    newDescribeAddresses,

    -- * Request Lenses
    describeAddresses_allocationIds,
    describeAddresses_filters,
    describeAddresses_publicIps,
    describeAddresses_dryRun,

    -- * Destructuring the Response
    DescribeAddressesResponse (..),
    newDescribeAddressesResponse,

    -- * Response Lenses
    describeAddressesResponse_addresses,
    describeAddressesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAddresses' smart constructor.
data DescribeAddresses = DescribeAddresses'
  { -- | [EC2-VPC] Information about the allocation IDs.
    allocationIds :: Prelude.Maybe [Prelude.Text],
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
    --     Zones, or Wavelength Zones from where Amazon Web Services advertises
    --     IP addresses.
    --
    -- -   @network-interface-id@ - [EC2-VPC] The ID of the network interface
    --     that the address is associated with, if any.
    --
    -- -   @network-interface-owner-id@ - The Amazon Web Services account ID of
    --     the owner.
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
    filters :: Prelude.Maybe [Filter],
    -- | One or more Elastic IP addresses.
    --
    -- Default: Describes all your Elastic IP addresses.
    publicIps :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAddresses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocationIds', 'describeAddresses_allocationIds' - [EC2-VPC] Information about the allocation IDs.
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
--     Zones, or Wavelength Zones from where Amazon Web Services advertises
--     IP addresses.
--
-- -   @network-interface-id@ - [EC2-VPC] The ID of the network interface
--     that the address is associated with, if any.
--
-- -   @network-interface-owner-id@ - The Amazon Web Services account ID of
--     the owner.
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
--
-- 'publicIps', 'describeAddresses_publicIps' - One or more Elastic IP addresses.
--
-- Default: Describes all your Elastic IP addresses.
--
-- 'dryRun', 'describeAddresses_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newDescribeAddresses ::
  DescribeAddresses
newDescribeAddresses =
  DescribeAddresses'
    { allocationIds = Prelude.Nothing,
      filters = Prelude.Nothing,
      publicIps = Prelude.Nothing,
      dryRun = Prelude.Nothing
    }

-- | [EC2-VPC] Information about the allocation IDs.
describeAddresses_allocationIds :: Lens.Lens' DescribeAddresses (Prelude.Maybe [Prelude.Text])
describeAddresses_allocationIds = Lens.lens (\DescribeAddresses' {allocationIds} -> allocationIds) (\s@DescribeAddresses' {} a -> s {allocationIds = a} :: DescribeAddresses) Prelude.. Lens.mapping Lens.coerced

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
--     Zones, or Wavelength Zones from where Amazon Web Services advertises
--     IP addresses.
--
-- -   @network-interface-id@ - [EC2-VPC] The ID of the network interface
--     that the address is associated with, if any.
--
-- -   @network-interface-owner-id@ - The Amazon Web Services account ID of
--     the owner.
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
describeAddresses_filters :: Lens.Lens' DescribeAddresses (Prelude.Maybe [Filter])
describeAddresses_filters = Lens.lens (\DescribeAddresses' {filters} -> filters) (\s@DescribeAddresses' {} a -> s {filters = a} :: DescribeAddresses) Prelude.. Lens.mapping Lens.coerced

-- | One or more Elastic IP addresses.
--
-- Default: Describes all your Elastic IP addresses.
describeAddresses_publicIps :: Lens.Lens' DescribeAddresses (Prelude.Maybe [Prelude.Text])
describeAddresses_publicIps = Lens.lens (\DescribeAddresses' {publicIps} -> publicIps) (\s@DescribeAddresses' {} a -> s {publicIps = a} :: DescribeAddresses) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeAddresses_dryRun :: Lens.Lens' DescribeAddresses (Prelude.Maybe Prelude.Bool)
describeAddresses_dryRun = Lens.lens (\DescribeAddresses' {dryRun} -> dryRun) (\s@DescribeAddresses' {} a -> s {dryRun = a} :: DescribeAddresses)

instance Core.AWSRequest DescribeAddresses where
  type
    AWSResponse DescribeAddresses =
      DescribeAddressesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeAddressesResponse'
            Prelude.<$> ( x Data..@? "addressesSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAddresses where
  hashWithSalt _salt DescribeAddresses' {..} =
    _salt `Prelude.hashWithSalt` allocationIds
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` publicIps
      `Prelude.hashWithSalt` dryRun

instance Prelude.NFData DescribeAddresses where
  rnf DescribeAddresses' {..} =
    Prelude.rnf allocationIds
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf publicIps
      `Prelude.seq` Prelude.rnf dryRun

instance Data.ToHeaders DescribeAddresses where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeAddresses where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAddresses where
  toQuery DescribeAddresses' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeAddresses" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryList "AllocationId"
              Prelude.<$> allocationIds
          ),
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        Data.toQuery
          (Data.toQueryList "PublicIp" Prelude.<$> publicIps),
        "DryRun" Data.=: dryRun
      ]

-- | /See:/ 'newDescribeAddressesResponse' smart constructor.
data DescribeAddressesResponse = DescribeAddressesResponse'
  { -- | Information about the Elastic IP addresses.
    addresses :: Prelude.Maybe [Address],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeAddressesResponse
newDescribeAddressesResponse pHttpStatus_ =
  DescribeAddressesResponse'
    { addresses =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Elastic IP addresses.
describeAddressesResponse_addresses :: Lens.Lens' DescribeAddressesResponse (Prelude.Maybe [Address])
describeAddressesResponse_addresses = Lens.lens (\DescribeAddressesResponse' {addresses} -> addresses) (\s@DescribeAddressesResponse' {} a -> s {addresses = a} :: DescribeAddressesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAddressesResponse_httpStatus :: Lens.Lens' DescribeAddressesResponse Prelude.Int
describeAddressesResponse_httpStatus = Lens.lens (\DescribeAddressesResponse' {httpStatus} -> httpStatus) (\s@DescribeAddressesResponse' {} a -> s {httpStatus = a} :: DescribeAddressesResponse)

instance Prelude.NFData DescribeAddressesResponse where
  rnf DescribeAddressesResponse' {..} =
    Prelude.rnf addresses
      `Prelude.seq` Prelude.rnf httpStatus
