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
-- Module      : Network.AWS.EC2.AllocateAddress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allocates an Elastic IP address to your AWS account. After you allocate
-- the Elastic IP address you can associate it with an instance or network
-- interface. After you release an Elastic IP address, it is released to
-- the IP address pool and can be allocated to a different AWS account.
--
-- You can allocate an Elastic IP address from an address pool owned by AWS
-- or from an address pool created from a public IPv4 address range that
-- you have brought to AWS for use with your AWS resources using bring your
-- own IP addresses (BYOIP). For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-byoip.html Bring Your Own IP Addresses (BYOIP)>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- [EC2-VPC] If you release an Elastic IP address, you might be able to
-- recover it. You cannot recover an Elastic IP address that you released
-- after it is allocated to another AWS account. You cannot recover an
-- Elastic IP address for EC2-Classic. To attempt to recover an Elastic IP
-- address that you released, specify it in this operation.
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
module Network.AWS.EC2.AllocateAddress
  ( -- * Creating a Request
    AllocateAddress (..),
    newAllocateAddress,

    -- * Request Lenses
    allocateAddress_tagSpecifications,
    allocateAddress_customerOwnedIpv4Pool,
    allocateAddress_dryRun,
    allocateAddress_address,
    allocateAddress_domain,
    allocateAddress_publicIpv4Pool,
    allocateAddress_networkBorderGroup,

    -- * Destructuring the Response
    AllocateAddressResponse (..),
    newAllocateAddressResponse,

    -- * Response Lenses
    allocateAddressResponse_customerOwnedIpv4Pool,
    allocateAddressResponse_domain,
    allocateAddressResponse_carrierIp,
    allocateAddressResponse_customerOwnedIp,
    allocateAddressResponse_publicIpv4Pool,
    allocateAddressResponse_publicIp,
    allocateAddressResponse_allocationId,
    allocateAddressResponse_networkBorderGroup,
    allocateAddressResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAllocateAddress' smart constructor.
data AllocateAddress = AllocateAddress'
  { -- | The tags to assign to the Elastic IP address.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The ID of a customer-owned address pool. Use this parameter to let
    -- Amazon EC2 select an address from the address pool. Alternatively,
    -- specify a specific address from the address pool.
    customerOwnedIpv4Pool :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | [EC2-VPC] The Elastic IP address to recover or an IPv4 address from an
    -- address pool.
    address :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the Elastic IP address is for use with instances in a
    -- VPC or instances in EC2-Classic.
    --
    -- Default: If the Region supports EC2-Classic, the default is @standard@.
    -- Otherwise, the default is @vpc@.
    domain :: Prelude.Maybe DomainType,
    -- | The ID of an address pool that you own. Use this parameter to let Amazon
    -- EC2 select an address from the address pool. To specify a specific
    -- address from the address pool, use the @Address@ parameter instead.
    publicIpv4Pool :: Prelude.Maybe Prelude.Text,
    -- | A unique set of Availability Zones, Local Zones, or Wavelength Zones
    -- from which AWS advertises IP addresses. Use this parameter to limit the
    -- IP address to this location. IP addresses cannot move between network
    -- border groups.
    --
    -- Use
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeAvailabilityZones.html DescribeAvailabilityZones>
    -- to view the network border groups.
    --
    -- You cannot use a network border group with EC2 Classic. If you attempt
    -- this operation on EC2 classic, you will receive an
    -- @InvalidParameterCombination@ error. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error Codes>.
    networkBorderGroup :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AllocateAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'allocateAddress_tagSpecifications' - The tags to assign to the Elastic IP address.
--
-- 'customerOwnedIpv4Pool', 'allocateAddress_customerOwnedIpv4Pool' - The ID of a customer-owned address pool. Use this parameter to let
-- Amazon EC2 select an address from the address pool. Alternatively,
-- specify a specific address from the address pool.
--
-- 'dryRun', 'allocateAddress_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'address', 'allocateAddress_address' - [EC2-VPC] The Elastic IP address to recover or an IPv4 address from an
-- address pool.
--
-- 'domain', 'allocateAddress_domain' - Indicates whether the Elastic IP address is for use with instances in a
-- VPC or instances in EC2-Classic.
--
-- Default: If the Region supports EC2-Classic, the default is @standard@.
-- Otherwise, the default is @vpc@.
--
-- 'publicIpv4Pool', 'allocateAddress_publicIpv4Pool' - The ID of an address pool that you own. Use this parameter to let Amazon
-- EC2 select an address from the address pool. To specify a specific
-- address from the address pool, use the @Address@ parameter instead.
--
-- 'networkBorderGroup', 'allocateAddress_networkBorderGroup' - A unique set of Availability Zones, Local Zones, or Wavelength Zones
-- from which AWS advertises IP addresses. Use this parameter to limit the
-- IP address to this location. IP addresses cannot move between network
-- border groups.
--
-- Use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeAvailabilityZones.html DescribeAvailabilityZones>
-- to view the network border groups.
--
-- You cannot use a network border group with EC2 Classic. If you attempt
-- this operation on EC2 classic, you will receive an
-- @InvalidParameterCombination@ error. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error Codes>.
newAllocateAddress ::
  AllocateAddress
newAllocateAddress =
  AllocateAddress'
    { tagSpecifications =
        Prelude.Nothing,
      customerOwnedIpv4Pool = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      address = Prelude.Nothing,
      domain = Prelude.Nothing,
      publicIpv4Pool = Prelude.Nothing,
      networkBorderGroup = Prelude.Nothing
    }

-- | The tags to assign to the Elastic IP address.
allocateAddress_tagSpecifications :: Lens.Lens' AllocateAddress (Prelude.Maybe [TagSpecification])
allocateAddress_tagSpecifications = Lens.lens (\AllocateAddress' {tagSpecifications} -> tagSpecifications) (\s@AllocateAddress' {} a -> s {tagSpecifications = a} :: AllocateAddress) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of a customer-owned address pool. Use this parameter to let
-- Amazon EC2 select an address from the address pool. Alternatively,
-- specify a specific address from the address pool.
allocateAddress_customerOwnedIpv4Pool :: Lens.Lens' AllocateAddress (Prelude.Maybe Prelude.Text)
allocateAddress_customerOwnedIpv4Pool = Lens.lens (\AllocateAddress' {customerOwnedIpv4Pool} -> customerOwnedIpv4Pool) (\s@AllocateAddress' {} a -> s {customerOwnedIpv4Pool = a} :: AllocateAddress)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
allocateAddress_dryRun :: Lens.Lens' AllocateAddress (Prelude.Maybe Prelude.Bool)
allocateAddress_dryRun = Lens.lens (\AllocateAddress' {dryRun} -> dryRun) (\s@AllocateAddress' {} a -> s {dryRun = a} :: AllocateAddress)

-- | [EC2-VPC] The Elastic IP address to recover or an IPv4 address from an
-- address pool.
allocateAddress_address :: Lens.Lens' AllocateAddress (Prelude.Maybe Prelude.Text)
allocateAddress_address = Lens.lens (\AllocateAddress' {address} -> address) (\s@AllocateAddress' {} a -> s {address = a} :: AllocateAddress)

-- | Indicates whether the Elastic IP address is for use with instances in a
-- VPC or instances in EC2-Classic.
--
-- Default: If the Region supports EC2-Classic, the default is @standard@.
-- Otherwise, the default is @vpc@.
allocateAddress_domain :: Lens.Lens' AllocateAddress (Prelude.Maybe DomainType)
allocateAddress_domain = Lens.lens (\AllocateAddress' {domain} -> domain) (\s@AllocateAddress' {} a -> s {domain = a} :: AllocateAddress)

-- | The ID of an address pool that you own. Use this parameter to let Amazon
-- EC2 select an address from the address pool. To specify a specific
-- address from the address pool, use the @Address@ parameter instead.
allocateAddress_publicIpv4Pool :: Lens.Lens' AllocateAddress (Prelude.Maybe Prelude.Text)
allocateAddress_publicIpv4Pool = Lens.lens (\AllocateAddress' {publicIpv4Pool} -> publicIpv4Pool) (\s@AllocateAddress' {} a -> s {publicIpv4Pool = a} :: AllocateAddress)

-- | A unique set of Availability Zones, Local Zones, or Wavelength Zones
-- from which AWS advertises IP addresses. Use this parameter to limit the
-- IP address to this location. IP addresses cannot move between network
-- border groups.
--
-- Use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeAvailabilityZones.html DescribeAvailabilityZones>
-- to view the network border groups.
--
-- You cannot use a network border group with EC2 Classic. If you attempt
-- this operation on EC2 classic, you will receive an
-- @InvalidParameterCombination@ error. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error Codes>.
allocateAddress_networkBorderGroup :: Lens.Lens' AllocateAddress (Prelude.Maybe Prelude.Text)
allocateAddress_networkBorderGroup = Lens.lens (\AllocateAddress' {networkBorderGroup} -> networkBorderGroup) (\s@AllocateAddress' {} a -> s {networkBorderGroup = a} :: AllocateAddress)

instance Prelude.AWSRequest AllocateAddress where
  type Rs AllocateAddress = AllocateAddressResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AllocateAddressResponse'
            Prelude.<$> (x Prelude..@? "customerOwnedIpv4Pool")
            Prelude.<*> (x Prelude..@? "domain")
            Prelude.<*> (x Prelude..@? "carrierIp")
            Prelude.<*> (x Prelude..@? "customerOwnedIp")
            Prelude.<*> (x Prelude..@? "publicIpv4Pool")
            Prelude.<*> (x Prelude..@? "publicIp")
            Prelude.<*> (x Prelude..@? "allocationId")
            Prelude.<*> (x Prelude..@? "networkBorderGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AllocateAddress

instance Prelude.NFData AllocateAddress

instance Prelude.ToHeaders AllocateAddress where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath AllocateAddress where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AllocateAddress where
  toQuery AllocateAddress' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("AllocateAddress" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        Prelude.toQuery
          ( Prelude.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "CustomerOwnedIpv4Pool"
          Prelude.=: customerOwnedIpv4Pool,
        "DryRun" Prelude.=: dryRun,
        "Address" Prelude.=: address,
        "Domain" Prelude.=: domain,
        "PublicIpv4Pool" Prelude.=: publicIpv4Pool,
        "NetworkBorderGroup" Prelude.=: networkBorderGroup
      ]

-- | /See:/ 'newAllocateAddressResponse' smart constructor.
data AllocateAddressResponse = AllocateAddressResponse'
  { -- | The ID of the customer-owned address pool.
    customerOwnedIpv4Pool :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the Elastic IP address is for use with instances in a
    -- VPC (@vpc@) or instances in EC2-Classic (@standard@).
    domain :: Prelude.Maybe DomainType,
    -- | The carrier IP address. This option is only available for network
    -- interfaces which reside in a subnet in a Wavelength Zone (for example an
    -- EC2 instance).
    carrierIp :: Prelude.Maybe Prelude.Text,
    -- | The customer-owned IP address.
    customerOwnedIp :: Prelude.Maybe Prelude.Text,
    -- | The ID of an address pool.
    publicIpv4Pool :: Prelude.Maybe Prelude.Text,
    -- | The Elastic IP address.
    publicIp :: Prelude.Maybe Prelude.Text,
    -- | [EC2-VPC] The ID that AWS assigns to represent the allocation of the
    -- Elastic IP address for use with instances in a VPC.
    allocationId :: Prelude.Maybe Prelude.Text,
    -- | The set of Availability Zones, Local Zones, or Wavelength Zones from
    -- which AWS advertises IP addresses.
    networkBorderGroup :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AllocateAddressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerOwnedIpv4Pool', 'allocateAddressResponse_customerOwnedIpv4Pool' - The ID of the customer-owned address pool.
--
-- 'domain', 'allocateAddressResponse_domain' - Indicates whether the Elastic IP address is for use with instances in a
-- VPC (@vpc@) or instances in EC2-Classic (@standard@).
--
-- 'carrierIp', 'allocateAddressResponse_carrierIp' - The carrier IP address. This option is only available for network
-- interfaces which reside in a subnet in a Wavelength Zone (for example an
-- EC2 instance).
--
-- 'customerOwnedIp', 'allocateAddressResponse_customerOwnedIp' - The customer-owned IP address.
--
-- 'publicIpv4Pool', 'allocateAddressResponse_publicIpv4Pool' - The ID of an address pool.
--
-- 'publicIp', 'allocateAddressResponse_publicIp' - The Elastic IP address.
--
-- 'allocationId', 'allocateAddressResponse_allocationId' - [EC2-VPC] The ID that AWS assigns to represent the allocation of the
-- Elastic IP address for use with instances in a VPC.
--
-- 'networkBorderGroup', 'allocateAddressResponse_networkBorderGroup' - The set of Availability Zones, Local Zones, or Wavelength Zones from
-- which AWS advertises IP addresses.
--
-- 'httpStatus', 'allocateAddressResponse_httpStatus' - The response's http status code.
newAllocateAddressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AllocateAddressResponse
newAllocateAddressResponse pHttpStatus_ =
  AllocateAddressResponse'
    { customerOwnedIpv4Pool =
        Prelude.Nothing,
      domain = Prelude.Nothing,
      carrierIp = Prelude.Nothing,
      customerOwnedIp = Prelude.Nothing,
      publicIpv4Pool = Prelude.Nothing,
      publicIp = Prelude.Nothing,
      allocationId = Prelude.Nothing,
      networkBorderGroup = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the customer-owned address pool.
allocateAddressResponse_customerOwnedIpv4Pool :: Lens.Lens' AllocateAddressResponse (Prelude.Maybe Prelude.Text)
allocateAddressResponse_customerOwnedIpv4Pool = Lens.lens (\AllocateAddressResponse' {customerOwnedIpv4Pool} -> customerOwnedIpv4Pool) (\s@AllocateAddressResponse' {} a -> s {customerOwnedIpv4Pool = a} :: AllocateAddressResponse)

-- | Indicates whether the Elastic IP address is for use with instances in a
-- VPC (@vpc@) or instances in EC2-Classic (@standard@).
allocateAddressResponse_domain :: Lens.Lens' AllocateAddressResponse (Prelude.Maybe DomainType)
allocateAddressResponse_domain = Lens.lens (\AllocateAddressResponse' {domain} -> domain) (\s@AllocateAddressResponse' {} a -> s {domain = a} :: AllocateAddressResponse)

-- | The carrier IP address. This option is only available for network
-- interfaces which reside in a subnet in a Wavelength Zone (for example an
-- EC2 instance).
allocateAddressResponse_carrierIp :: Lens.Lens' AllocateAddressResponse (Prelude.Maybe Prelude.Text)
allocateAddressResponse_carrierIp = Lens.lens (\AllocateAddressResponse' {carrierIp} -> carrierIp) (\s@AllocateAddressResponse' {} a -> s {carrierIp = a} :: AllocateAddressResponse)

-- | The customer-owned IP address.
allocateAddressResponse_customerOwnedIp :: Lens.Lens' AllocateAddressResponse (Prelude.Maybe Prelude.Text)
allocateAddressResponse_customerOwnedIp = Lens.lens (\AllocateAddressResponse' {customerOwnedIp} -> customerOwnedIp) (\s@AllocateAddressResponse' {} a -> s {customerOwnedIp = a} :: AllocateAddressResponse)

-- | The ID of an address pool.
allocateAddressResponse_publicIpv4Pool :: Lens.Lens' AllocateAddressResponse (Prelude.Maybe Prelude.Text)
allocateAddressResponse_publicIpv4Pool = Lens.lens (\AllocateAddressResponse' {publicIpv4Pool} -> publicIpv4Pool) (\s@AllocateAddressResponse' {} a -> s {publicIpv4Pool = a} :: AllocateAddressResponse)

-- | The Elastic IP address.
allocateAddressResponse_publicIp :: Lens.Lens' AllocateAddressResponse (Prelude.Maybe Prelude.Text)
allocateAddressResponse_publicIp = Lens.lens (\AllocateAddressResponse' {publicIp} -> publicIp) (\s@AllocateAddressResponse' {} a -> s {publicIp = a} :: AllocateAddressResponse)

-- | [EC2-VPC] The ID that AWS assigns to represent the allocation of the
-- Elastic IP address for use with instances in a VPC.
allocateAddressResponse_allocationId :: Lens.Lens' AllocateAddressResponse (Prelude.Maybe Prelude.Text)
allocateAddressResponse_allocationId = Lens.lens (\AllocateAddressResponse' {allocationId} -> allocationId) (\s@AllocateAddressResponse' {} a -> s {allocationId = a} :: AllocateAddressResponse)

-- | The set of Availability Zones, Local Zones, or Wavelength Zones from
-- which AWS advertises IP addresses.
allocateAddressResponse_networkBorderGroup :: Lens.Lens' AllocateAddressResponse (Prelude.Maybe Prelude.Text)
allocateAddressResponse_networkBorderGroup = Lens.lens (\AllocateAddressResponse' {networkBorderGroup} -> networkBorderGroup) (\s@AllocateAddressResponse' {} a -> s {networkBorderGroup = a} :: AllocateAddressResponse)

-- | The response's http status code.
allocateAddressResponse_httpStatus :: Lens.Lens' AllocateAddressResponse Prelude.Int
allocateAddressResponse_httpStatus = Lens.lens (\AllocateAddressResponse' {httpStatus} -> httpStatus) (\s@AllocateAddressResponse' {} a -> s {httpStatus = a} :: AllocateAddressResponse)

instance Prelude.NFData AllocateAddressResponse
