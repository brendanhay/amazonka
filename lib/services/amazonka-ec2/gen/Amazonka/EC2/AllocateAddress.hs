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
-- Module      : Amazonka.EC2.AllocateAddress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allocates an Elastic IP address to your Amazon Web Services account.
-- After you allocate the Elastic IP address you can associate it with an
-- instance or network interface. After you release an Elastic IP address,
-- it is released to the IP address pool and can be allocated to a
-- different Amazon Web Services account.
--
-- You can allocate an Elastic IP address from an address pool owned by
-- Amazon Web Services or from an address pool created from a public IPv4
-- address range that you have brought to Amazon Web Services for use with
-- your Amazon Web Services resources using bring your own IP addresses
-- (BYOIP). For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-byoip.html Bring Your Own IP Addresses (BYOIP)>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- [EC2-VPC] If you release an Elastic IP address, you might be able to
-- recover it. You cannot recover an Elastic IP address that you released
-- after it is allocated to another Amazon Web Services account. You cannot
-- recover an Elastic IP address for EC2-Classic. To attempt to recover an
-- Elastic IP address that you released, specify it in this operation.
--
-- An Elastic IP address is for use either in the EC2-Classic platform or
-- in a VPC. By default, you can allocate 5 Elastic IP addresses for
-- EC2-Classic per Region and 5 Elastic IP addresses for EC2-VPC per
-- Region.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- You can allocate a carrier IP address which is a public IP address from
-- a telecommunication carrier, to a network interface which resides in a
-- subnet in a Wavelength Zone (for example an EC2 instance).
--
-- We are retiring EC2-Classic. We recommend that you migrate from
-- EC2-Classic to a VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.AllocateAddress
  ( -- * Creating a Request
    AllocateAddress (..),
    newAllocateAddress,

    -- * Request Lenses
    allocateAddress_address,
    allocateAddress_customerOwnedIpv4Pool,
    allocateAddress_domain,
    allocateAddress_dryRun,
    allocateAddress_networkBorderGroup,
    allocateAddress_publicIpv4Pool,
    allocateAddress_tagSpecifications,

    -- * Destructuring the Response
    AllocateAddressResponse (..),
    newAllocateAddressResponse,

    -- * Response Lenses
    allocateAddressResponse_allocationId,
    allocateAddressResponse_carrierIp,
    allocateAddressResponse_customerOwnedIp,
    allocateAddressResponse_customerOwnedIpv4Pool,
    allocateAddressResponse_domain,
    allocateAddressResponse_networkBorderGroup,
    allocateAddressResponse_publicIp,
    allocateAddressResponse_publicIpv4Pool,
    allocateAddressResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAllocateAddress' smart constructor.
data AllocateAddress = AllocateAddress'
  { -- | [EC2-VPC] The Elastic IP address to recover or an IPv4 address from an
    -- address pool.
    address :: Prelude.Maybe Prelude.Text,
    -- | The ID of a customer-owned address pool. Use this parameter to let
    -- Amazon EC2 select an address from the address pool. Alternatively,
    -- specify a specific address from the address pool.
    customerOwnedIpv4Pool :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the Elastic IP address is for use with instances in a
    -- VPC or instances in EC2-Classic.
    --
    -- Default: If the Region supports EC2-Classic, the default is @standard@.
    -- Otherwise, the default is @vpc@.
    domain :: Prelude.Maybe DomainType,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | A unique set of Availability Zones, Local Zones, or Wavelength Zones
    -- from which Amazon Web Services advertises IP addresses. Use this
    -- parameter to limit the IP address to this location. IP addresses cannot
    -- move between network border groups.
    --
    -- Use
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeAvailabilityZones.html DescribeAvailabilityZones>
    -- to view the network border groups.
    --
    -- You cannot use a network border group with EC2 Classic. If you attempt
    -- this operation on EC2 Classic, you receive an
    -- @InvalidParameterCombination@ error.
    networkBorderGroup :: Prelude.Maybe Prelude.Text,
    -- | The ID of an address pool that you own. Use this parameter to let Amazon
    -- EC2 select an address from the address pool. To specify a specific
    -- address from the address pool, use the @Address@ parameter instead.
    publicIpv4Pool :: Prelude.Maybe Prelude.Text,
    -- | The tags to assign to the Elastic IP address.
    tagSpecifications :: Prelude.Maybe [TagSpecification]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AllocateAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'allocateAddress_address' - [EC2-VPC] The Elastic IP address to recover or an IPv4 address from an
-- address pool.
--
-- 'customerOwnedIpv4Pool', 'allocateAddress_customerOwnedIpv4Pool' - The ID of a customer-owned address pool. Use this parameter to let
-- Amazon EC2 select an address from the address pool. Alternatively,
-- specify a specific address from the address pool.
--
-- 'domain', 'allocateAddress_domain' - Indicates whether the Elastic IP address is for use with instances in a
-- VPC or instances in EC2-Classic.
--
-- Default: If the Region supports EC2-Classic, the default is @standard@.
-- Otherwise, the default is @vpc@.
--
-- 'dryRun', 'allocateAddress_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'networkBorderGroup', 'allocateAddress_networkBorderGroup' - A unique set of Availability Zones, Local Zones, or Wavelength Zones
-- from which Amazon Web Services advertises IP addresses. Use this
-- parameter to limit the IP address to this location. IP addresses cannot
-- move between network border groups.
--
-- Use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeAvailabilityZones.html DescribeAvailabilityZones>
-- to view the network border groups.
--
-- You cannot use a network border group with EC2 Classic. If you attempt
-- this operation on EC2 Classic, you receive an
-- @InvalidParameterCombination@ error.
--
-- 'publicIpv4Pool', 'allocateAddress_publicIpv4Pool' - The ID of an address pool that you own. Use this parameter to let Amazon
-- EC2 select an address from the address pool. To specify a specific
-- address from the address pool, use the @Address@ parameter instead.
--
-- 'tagSpecifications', 'allocateAddress_tagSpecifications' - The tags to assign to the Elastic IP address.
newAllocateAddress ::
  AllocateAddress
newAllocateAddress =
  AllocateAddress'
    { address = Prelude.Nothing,
      customerOwnedIpv4Pool = Prelude.Nothing,
      domain = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      networkBorderGroup = Prelude.Nothing,
      publicIpv4Pool = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing
    }

-- | [EC2-VPC] The Elastic IP address to recover or an IPv4 address from an
-- address pool.
allocateAddress_address :: Lens.Lens' AllocateAddress (Prelude.Maybe Prelude.Text)
allocateAddress_address = Lens.lens (\AllocateAddress' {address} -> address) (\s@AllocateAddress' {} a -> s {address = a} :: AllocateAddress)

-- | The ID of a customer-owned address pool. Use this parameter to let
-- Amazon EC2 select an address from the address pool. Alternatively,
-- specify a specific address from the address pool.
allocateAddress_customerOwnedIpv4Pool :: Lens.Lens' AllocateAddress (Prelude.Maybe Prelude.Text)
allocateAddress_customerOwnedIpv4Pool = Lens.lens (\AllocateAddress' {customerOwnedIpv4Pool} -> customerOwnedIpv4Pool) (\s@AllocateAddress' {} a -> s {customerOwnedIpv4Pool = a} :: AllocateAddress)

-- | Indicates whether the Elastic IP address is for use with instances in a
-- VPC or instances in EC2-Classic.
--
-- Default: If the Region supports EC2-Classic, the default is @standard@.
-- Otherwise, the default is @vpc@.
allocateAddress_domain :: Lens.Lens' AllocateAddress (Prelude.Maybe DomainType)
allocateAddress_domain = Lens.lens (\AllocateAddress' {domain} -> domain) (\s@AllocateAddress' {} a -> s {domain = a} :: AllocateAddress)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
allocateAddress_dryRun :: Lens.Lens' AllocateAddress (Prelude.Maybe Prelude.Bool)
allocateAddress_dryRun = Lens.lens (\AllocateAddress' {dryRun} -> dryRun) (\s@AllocateAddress' {} a -> s {dryRun = a} :: AllocateAddress)

-- | A unique set of Availability Zones, Local Zones, or Wavelength Zones
-- from which Amazon Web Services advertises IP addresses. Use this
-- parameter to limit the IP address to this location. IP addresses cannot
-- move between network border groups.
--
-- Use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeAvailabilityZones.html DescribeAvailabilityZones>
-- to view the network border groups.
--
-- You cannot use a network border group with EC2 Classic. If you attempt
-- this operation on EC2 Classic, you receive an
-- @InvalidParameterCombination@ error.
allocateAddress_networkBorderGroup :: Lens.Lens' AllocateAddress (Prelude.Maybe Prelude.Text)
allocateAddress_networkBorderGroup = Lens.lens (\AllocateAddress' {networkBorderGroup} -> networkBorderGroup) (\s@AllocateAddress' {} a -> s {networkBorderGroup = a} :: AllocateAddress)

-- | The ID of an address pool that you own. Use this parameter to let Amazon
-- EC2 select an address from the address pool. To specify a specific
-- address from the address pool, use the @Address@ parameter instead.
allocateAddress_publicIpv4Pool :: Lens.Lens' AllocateAddress (Prelude.Maybe Prelude.Text)
allocateAddress_publicIpv4Pool = Lens.lens (\AllocateAddress' {publicIpv4Pool} -> publicIpv4Pool) (\s@AllocateAddress' {} a -> s {publicIpv4Pool = a} :: AllocateAddress)

-- | The tags to assign to the Elastic IP address.
allocateAddress_tagSpecifications :: Lens.Lens' AllocateAddress (Prelude.Maybe [TagSpecification])
allocateAddress_tagSpecifications = Lens.lens (\AllocateAddress' {tagSpecifications} -> tagSpecifications) (\s@AllocateAddress' {} a -> s {tagSpecifications = a} :: AllocateAddress) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest AllocateAddress where
  type
    AWSResponse AllocateAddress =
      AllocateAddressResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AllocateAddressResponse'
            Prelude.<$> (x Data..@? "allocationId")
            Prelude.<*> (x Data..@? "carrierIp")
            Prelude.<*> (x Data..@? "customerOwnedIp")
            Prelude.<*> (x Data..@? "customerOwnedIpv4Pool")
            Prelude.<*> (x Data..@? "domain")
            Prelude.<*> (x Data..@? "networkBorderGroup")
            Prelude.<*> (x Data..@? "publicIp")
            Prelude.<*> (x Data..@? "publicIpv4Pool")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AllocateAddress where
  hashWithSalt _salt AllocateAddress' {..} =
    _salt
      `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` customerOwnedIpv4Pool
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` networkBorderGroup
      `Prelude.hashWithSalt` publicIpv4Pool
      `Prelude.hashWithSalt` tagSpecifications

instance Prelude.NFData AllocateAddress where
  rnf AllocateAddress' {..} =
    Prelude.rnf address
      `Prelude.seq` Prelude.rnf customerOwnedIpv4Pool
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf networkBorderGroup
      `Prelude.seq` Prelude.rnf publicIpv4Pool
      `Prelude.seq` Prelude.rnf tagSpecifications

instance Data.ToHeaders AllocateAddress where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AllocateAddress where
  toPath = Prelude.const "/"

instance Data.ToQuery AllocateAddress where
  toQuery AllocateAddress' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AllocateAddress" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Address" Data.=: address,
        "CustomerOwnedIpv4Pool"
          Data.=: customerOwnedIpv4Pool,
        "Domain" Data.=: domain,
        "DryRun" Data.=: dryRun,
        "NetworkBorderGroup" Data.=: networkBorderGroup,
        "PublicIpv4Pool" Data.=: publicIpv4Pool,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          )
      ]

-- | /See:/ 'newAllocateAddressResponse' smart constructor.
data AllocateAddressResponse = AllocateAddressResponse'
  { -- | [EC2-VPC] The ID that Amazon Web Services assigns to represent the
    -- allocation of the Elastic IP address for use with instances in a VPC.
    allocationId :: Prelude.Maybe Prelude.Text,
    -- | The carrier IP address. This option is only available for network
    -- interfaces which reside in a subnet in a Wavelength Zone (for example an
    -- EC2 instance).
    carrierIp :: Prelude.Maybe Prelude.Text,
    -- | The customer-owned IP address.
    customerOwnedIp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the customer-owned address pool.
    customerOwnedIpv4Pool :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the Elastic IP address is for use with instances in a
    -- VPC (@vpc@) or instances in EC2-Classic (@standard@).
    domain :: Prelude.Maybe DomainType,
    -- | The set of Availability Zones, Local Zones, or Wavelength Zones from
    -- which Amazon Web Services advertises IP addresses.
    networkBorderGroup :: Prelude.Maybe Prelude.Text,
    -- | The Elastic IP address.
    publicIp :: Prelude.Maybe Prelude.Text,
    -- | The ID of an address pool.
    publicIpv4Pool :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AllocateAddressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocationId', 'allocateAddressResponse_allocationId' - [EC2-VPC] The ID that Amazon Web Services assigns to represent the
-- allocation of the Elastic IP address for use with instances in a VPC.
--
-- 'carrierIp', 'allocateAddressResponse_carrierIp' - The carrier IP address. This option is only available for network
-- interfaces which reside in a subnet in a Wavelength Zone (for example an
-- EC2 instance).
--
-- 'customerOwnedIp', 'allocateAddressResponse_customerOwnedIp' - The customer-owned IP address.
--
-- 'customerOwnedIpv4Pool', 'allocateAddressResponse_customerOwnedIpv4Pool' - The ID of the customer-owned address pool.
--
-- 'domain', 'allocateAddressResponse_domain' - Indicates whether the Elastic IP address is for use with instances in a
-- VPC (@vpc@) or instances in EC2-Classic (@standard@).
--
-- 'networkBorderGroup', 'allocateAddressResponse_networkBorderGroup' - The set of Availability Zones, Local Zones, or Wavelength Zones from
-- which Amazon Web Services advertises IP addresses.
--
-- 'publicIp', 'allocateAddressResponse_publicIp' - The Elastic IP address.
--
-- 'publicIpv4Pool', 'allocateAddressResponse_publicIpv4Pool' - The ID of an address pool.
--
-- 'httpStatus', 'allocateAddressResponse_httpStatus' - The response's http status code.
newAllocateAddressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AllocateAddressResponse
newAllocateAddressResponse pHttpStatus_ =
  AllocateAddressResponse'
    { allocationId =
        Prelude.Nothing,
      carrierIp = Prelude.Nothing,
      customerOwnedIp = Prelude.Nothing,
      customerOwnedIpv4Pool = Prelude.Nothing,
      domain = Prelude.Nothing,
      networkBorderGroup = Prelude.Nothing,
      publicIp = Prelude.Nothing,
      publicIpv4Pool = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | [EC2-VPC] The ID that Amazon Web Services assigns to represent the
-- allocation of the Elastic IP address for use with instances in a VPC.
allocateAddressResponse_allocationId :: Lens.Lens' AllocateAddressResponse (Prelude.Maybe Prelude.Text)
allocateAddressResponse_allocationId = Lens.lens (\AllocateAddressResponse' {allocationId} -> allocationId) (\s@AllocateAddressResponse' {} a -> s {allocationId = a} :: AllocateAddressResponse)

-- | The carrier IP address. This option is only available for network
-- interfaces which reside in a subnet in a Wavelength Zone (for example an
-- EC2 instance).
allocateAddressResponse_carrierIp :: Lens.Lens' AllocateAddressResponse (Prelude.Maybe Prelude.Text)
allocateAddressResponse_carrierIp = Lens.lens (\AllocateAddressResponse' {carrierIp} -> carrierIp) (\s@AllocateAddressResponse' {} a -> s {carrierIp = a} :: AllocateAddressResponse)

-- | The customer-owned IP address.
allocateAddressResponse_customerOwnedIp :: Lens.Lens' AllocateAddressResponse (Prelude.Maybe Prelude.Text)
allocateAddressResponse_customerOwnedIp = Lens.lens (\AllocateAddressResponse' {customerOwnedIp} -> customerOwnedIp) (\s@AllocateAddressResponse' {} a -> s {customerOwnedIp = a} :: AllocateAddressResponse)

-- | The ID of the customer-owned address pool.
allocateAddressResponse_customerOwnedIpv4Pool :: Lens.Lens' AllocateAddressResponse (Prelude.Maybe Prelude.Text)
allocateAddressResponse_customerOwnedIpv4Pool = Lens.lens (\AllocateAddressResponse' {customerOwnedIpv4Pool} -> customerOwnedIpv4Pool) (\s@AllocateAddressResponse' {} a -> s {customerOwnedIpv4Pool = a} :: AllocateAddressResponse)

-- | Indicates whether the Elastic IP address is for use with instances in a
-- VPC (@vpc@) or instances in EC2-Classic (@standard@).
allocateAddressResponse_domain :: Lens.Lens' AllocateAddressResponse (Prelude.Maybe DomainType)
allocateAddressResponse_domain = Lens.lens (\AllocateAddressResponse' {domain} -> domain) (\s@AllocateAddressResponse' {} a -> s {domain = a} :: AllocateAddressResponse)

-- | The set of Availability Zones, Local Zones, or Wavelength Zones from
-- which Amazon Web Services advertises IP addresses.
allocateAddressResponse_networkBorderGroup :: Lens.Lens' AllocateAddressResponse (Prelude.Maybe Prelude.Text)
allocateAddressResponse_networkBorderGroup = Lens.lens (\AllocateAddressResponse' {networkBorderGroup} -> networkBorderGroup) (\s@AllocateAddressResponse' {} a -> s {networkBorderGroup = a} :: AllocateAddressResponse)

-- | The Elastic IP address.
allocateAddressResponse_publicIp :: Lens.Lens' AllocateAddressResponse (Prelude.Maybe Prelude.Text)
allocateAddressResponse_publicIp = Lens.lens (\AllocateAddressResponse' {publicIp} -> publicIp) (\s@AllocateAddressResponse' {} a -> s {publicIp = a} :: AllocateAddressResponse)

-- | The ID of an address pool.
allocateAddressResponse_publicIpv4Pool :: Lens.Lens' AllocateAddressResponse (Prelude.Maybe Prelude.Text)
allocateAddressResponse_publicIpv4Pool = Lens.lens (\AllocateAddressResponse' {publicIpv4Pool} -> publicIpv4Pool) (\s@AllocateAddressResponse' {} a -> s {publicIpv4Pool = a} :: AllocateAddressResponse)

-- | The response's http status code.
allocateAddressResponse_httpStatus :: Lens.Lens' AllocateAddressResponse Prelude.Int
allocateAddressResponse_httpStatus = Lens.lens (\AllocateAddressResponse' {httpStatus} -> httpStatus) (\s@AllocateAddressResponse' {} a -> s {httpStatus = a} :: AllocateAddressResponse)

instance Prelude.NFData AllocateAddressResponse where
  rnf AllocateAddressResponse' {..} =
    Prelude.rnf allocationId
      `Prelude.seq` Prelude.rnf carrierIp
      `Prelude.seq` Prelude.rnf customerOwnedIp
      `Prelude.seq` Prelude.rnf customerOwnedIpv4Pool
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf networkBorderGroup
      `Prelude.seq` Prelude.rnf publicIp
      `Prelude.seq` Prelude.rnf publicIpv4Pool
      `Prelude.seq` Prelude.rnf httpStatus
