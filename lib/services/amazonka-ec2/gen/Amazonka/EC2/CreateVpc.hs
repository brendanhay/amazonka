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
-- Module      : Amazonka.EC2.CreateVpc
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPC with the specified CIDR blocks. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/configure-your-vpc.html#vpc-cidr-blocks VPC CIDR blocks>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- You can optionally request an IPv6 CIDR block for the VPC. You can
-- request an Amazon-provided IPv6 CIDR block from Amazon\'s pool of IPv6
-- addresses, or an IPv6 CIDR block from an IPv6 address pool that you
-- provisioned through bring your own IP addresses
-- (<https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-byoip.html BYOIP>).
--
-- By default, each instance that you launch in the VPC has the default
-- DHCP options, which include only a default DNS server that we provide
-- (AmazonProvidedDNS). For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_DHCP_Options.html DHCP option sets>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- You can specify the instance tenancy value for the VPC when you create
-- it. You can\'t change this value for the VPC after you create it. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-instance.html Dedicated Instances>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.CreateVpc
  ( -- * Creating a Request
    CreateVpc (..),
    newCreateVpc,

    -- * Request Lenses
    createVpc_amazonProvidedIpv6CidrBlock,
    createVpc_cidrBlock,
    createVpc_dryRun,
    createVpc_instanceTenancy,
    createVpc_ipv4IpamPoolId,
    createVpc_ipv4NetmaskLength,
    createVpc_ipv6CidrBlock,
    createVpc_ipv6CidrBlockNetworkBorderGroup,
    createVpc_ipv6IpamPoolId,
    createVpc_ipv6NetmaskLength,
    createVpc_ipv6Pool,
    createVpc_tagSpecifications,

    -- * Destructuring the Response
    CreateVpcResponse (..),
    newCreateVpcResponse,

    -- * Response Lenses
    createVpcResponse_vpc,
    createVpcResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVpc' smart constructor.
data CreateVpc = CreateVpc'
  { -- | Requests an Amazon-provided IPv6 CIDR block with a \/56 prefix length
    -- for the VPC. You cannot specify the range of IP addresses, or the size
    -- of the CIDR block.
    amazonProvidedIpv6CidrBlock :: Prelude.Maybe Prelude.Bool,
    -- | The IPv4 network range for the VPC, in CIDR notation. For example,
    -- @10.0.0.0\/16@. We modify the specified CIDR block to its canonical
    -- form; for example, if you specify @100.68.0.18\/18@, we modify it to
    -- @100.68.0.0\/18@.
    cidrBlock :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tenancy options for instances launched into the VPC. For @default@,
    -- instances are launched with shared tenancy by default. You can launch
    -- instances with any tenancy into a shared tenancy VPC. For @dedicated@,
    -- instances are launched as dedicated tenancy instances by default. You
    -- can only launch instances with a tenancy of @dedicated@ or @host@ into a
    -- dedicated tenancy VPC.
    --
    -- __Important:__ The @host@ value cannot be used with this parameter. Use
    -- the @default@ or @dedicated@ values only.
    --
    -- Default: @default@
    instanceTenancy :: Prelude.Maybe Tenancy,
    -- | The ID of an IPv4 IPAM pool you want to use for allocating this VPC\'s
    -- CIDR. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
    -- in the /Amazon VPC IPAM User Guide/.
    ipv4IpamPoolId :: Prelude.Maybe Prelude.Text,
    -- | The netmask length of the IPv4 CIDR you want to allocate to this VPC
    -- from an Amazon VPC IP Address Manager (IPAM) pool. For more information
    -- about IPAM, see
    -- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
    -- in the /Amazon VPC IPAM User Guide/.
    ipv4NetmaskLength :: Prelude.Maybe Prelude.Int,
    -- | The IPv6 CIDR block from the IPv6 address pool. You must also specify
    -- @Ipv6Pool@ in the request.
    --
    -- To let Amazon choose the IPv6 CIDR block for you, omit this parameter.
    ipv6CidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The name of the location from which we advertise the IPV6 CIDR block.
    -- Use this parameter to limit the address to this location.
    --
    -- You must set @AmazonProvidedIpv6CidrBlock@ to @true@ to use this
    -- parameter.
    ipv6CidrBlockNetworkBorderGroup :: Prelude.Maybe Prelude.Text,
    -- | The ID of an IPv6 IPAM pool which will be used to allocate this VPC an
    -- IPv6 CIDR. IPAM is a VPC feature that you can use to automate your IP
    -- address management workflows including assigning, tracking,
    -- troubleshooting, and auditing IP addresses across Amazon Web Services
    -- Regions and accounts throughout your Amazon Web Services Organization.
    -- For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
    -- in the /Amazon VPC IPAM User Guide/.
    ipv6IpamPoolId :: Prelude.Maybe Prelude.Text,
    -- | The netmask length of the IPv6 CIDR you want to allocate to this VPC
    -- from an Amazon VPC IP Address Manager (IPAM) pool. For more information
    -- about IPAM, see
    -- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
    -- in the /Amazon VPC IPAM User Guide/.
    ipv6NetmaskLength :: Prelude.Maybe Prelude.Int,
    -- | The ID of an IPv6 address pool from which to allocate the IPv6 CIDR
    -- block.
    ipv6Pool :: Prelude.Maybe Prelude.Text,
    -- | The tags to assign to the VPC.
    tagSpecifications :: Prelude.Maybe [TagSpecification]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpc' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amazonProvidedIpv6CidrBlock', 'createVpc_amazonProvidedIpv6CidrBlock' - Requests an Amazon-provided IPv6 CIDR block with a \/56 prefix length
-- for the VPC. You cannot specify the range of IP addresses, or the size
-- of the CIDR block.
--
-- 'cidrBlock', 'createVpc_cidrBlock' - The IPv4 network range for the VPC, in CIDR notation. For example,
-- @10.0.0.0\/16@. We modify the specified CIDR block to its canonical
-- form; for example, if you specify @100.68.0.18\/18@, we modify it to
-- @100.68.0.0\/18@.
--
-- 'dryRun', 'createVpc_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceTenancy', 'createVpc_instanceTenancy' - The tenancy options for instances launched into the VPC. For @default@,
-- instances are launched with shared tenancy by default. You can launch
-- instances with any tenancy into a shared tenancy VPC. For @dedicated@,
-- instances are launched as dedicated tenancy instances by default. You
-- can only launch instances with a tenancy of @dedicated@ or @host@ into a
-- dedicated tenancy VPC.
--
-- __Important:__ The @host@ value cannot be used with this parameter. Use
-- the @default@ or @dedicated@ values only.
--
-- Default: @default@
--
-- 'ipv4IpamPoolId', 'createVpc_ipv4IpamPoolId' - The ID of an IPv4 IPAM pool you want to use for allocating this VPC\'s
-- CIDR. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
-- in the /Amazon VPC IPAM User Guide/.
--
-- 'ipv4NetmaskLength', 'createVpc_ipv4NetmaskLength' - The netmask length of the IPv4 CIDR you want to allocate to this VPC
-- from an Amazon VPC IP Address Manager (IPAM) pool. For more information
-- about IPAM, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
-- in the /Amazon VPC IPAM User Guide/.
--
-- 'ipv6CidrBlock', 'createVpc_ipv6CidrBlock' - The IPv6 CIDR block from the IPv6 address pool. You must also specify
-- @Ipv6Pool@ in the request.
--
-- To let Amazon choose the IPv6 CIDR block for you, omit this parameter.
--
-- 'ipv6CidrBlockNetworkBorderGroup', 'createVpc_ipv6CidrBlockNetworkBorderGroup' - The name of the location from which we advertise the IPV6 CIDR block.
-- Use this parameter to limit the address to this location.
--
-- You must set @AmazonProvidedIpv6CidrBlock@ to @true@ to use this
-- parameter.
--
-- 'ipv6IpamPoolId', 'createVpc_ipv6IpamPoolId' - The ID of an IPv6 IPAM pool which will be used to allocate this VPC an
-- IPv6 CIDR. IPAM is a VPC feature that you can use to automate your IP
-- address management workflows including assigning, tracking,
-- troubleshooting, and auditing IP addresses across Amazon Web Services
-- Regions and accounts throughout your Amazon Web Services Organization.
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
-- in the /Amazon VPC IPAM User Guide/.
--
-- 'ipv6NetmaskLength', 'createVpc_ipv6NetmaskLength' - The netmask length of the IPv6 CIDR you want to allocate to this VPC
-- from an Amazon VPC IP Address Manager (IPAM) pool. For more information
-- about IPAM, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
-- in the /Amazon VPC IPAM User Guide/.
--
-- 'ipv6Pool', 'createVpc_ipv6Pool' - The ID of an IPv6 address pool from which to allocate the IPv6 CIDR
-- block.
--
-- 'tagSpecifications', 'createVpc_tagSpecifications' - The tags to assign to the VPC.
newCreateVpc ::
  CreateVpc
newCreateVpc =
  CreateVpc'
    { amazonProvidedIpv6CidrBlock =
        Prelude.Nothing,
      cidrBlock = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      instanceTenancy = Prelude.Nothing,
      ipv4IpamPoolId = Prelude.Nothing,
      ipv4NetmaskLength = Prelude.Nothing,
      ipv6CidrBlock = Prelude.Nothing,
      ipv6CidrBlockNetworkBorderGroup = Prelude.Nothing,
      ipv6IpamPoolId = Prelude.Nothing,
      ipv6NetmaskLength = Prelude.Nothing,
      ipv6Pool = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing
    }

-- | Requests an Amazon-provided IPv6 CIDR block with a \/56 prefix length
-- for the VPC. You cannot specify the range of IP addresses, or the size
-- of the CIDR block.
createVpc_amazonProvidedIpv6CidrBlock :: Lens.Lens' CreateVpc (Prelude.Maybe Prelude.Bool)
createVpc_amazonProvidedIpv6CidrBlock = Lens.lens (\CreateVpc' {amazonProvidedIpv6CidrBlock} -> amazonProvidedIpv6CidrBlock) (\s@CreateVpc' {} a -> s {amazonProvidedIpv6CidrBlock = a} :: CreateVpc)

-- | The IPv4 network range for the VPC, in CIDR notation. For example,
-- @10.0.0.0\/16@. We modify the specified CIDR block to its canonical
-- form; for example, if you specify @100.68.0.18\/18@, we modify it to
-- @100.68.0.0\/18@.
createVpc_cidrBlock :: Lens.Lens' CreateVpc (Prelude.Maybe Prelude.Text)
createVpc_cidrBlock = Lens.lens (\CreateVpc' {cidrBlock} -> cidrBlock) (\s@CreateVpc' {} a -> s {cidrBlock = a} :: CreateVpc)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createVpc_dryRun :: Lens.Lens' CreateVpc (Prelude.Maybe Prelude.Bool)
createVpc_dryRun = Lens.lens (\CreateVpc' {dryRun} -> dryRun) (\s@CreateVpc' {} a -> s {dryRun = a} :: CreateVpc)

-- | The tenancy options for instances launched into the VPC. For @default@,
-- instances are launched with shared tenancy by default. You can launch
-- instances with any tenancy into a shared tenancy VPC. For @dedicated@,
-- instances are launched as dedicated tenancy instances by default. You
-- can only launch instances with a tenancy of @dedicated@ or @host@ into a
-- dedicated tenancy VPC.
--
-- __Important:__ The @host@ value cannot be used with this parameter. Use
-- the @default@ or @dedicated@ values only.
--
-- Default: @default@
createVpc_instanceTenancy :: Lens.Lens' CreateVpc (Prelude.Maybe Tenancy)
createVpc_instanceTenancy = Lens.lens (\CreateVpc' {instanceTenancy} -> instanceTenancy) (\s@CreateVpc' {} a -> s {instanceTenancy = a} :: CreateVpc)

-- | The ID of an IPv4 IPAM pool you want to use for allocating this VPC\'s
-- CIDR. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
-- in the /Amazon VPC IPAM User Guide/.
createVpc_ipv4IpamPoolId :: Lens.Lens' CreateVpc (Prelude.Maybe Prelude.Text)
createVpc_ipv4IpamPoolId = Lens.lens (\CreateVpc' {ipv4IpamPoolId} -> ipv4IpamPoolId) (\s@CreateVpc' {} a -> s {ipv4IpamPoolId = a} :: CreateVpc)

-- | The netmask length of the IPv4 CIDR you want to allocate to this VPC
-- from an Amazon VPC IP Address Manager (IPAM) pool. For more information
-- about IPAM, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
-- in the /Amazon VPC IPAM User Guide/.
createVpc_ipv4NetmaskLength :: Lens.Lens' CreateVpc (Prelude.Maybe Prelude.Int)
createVpc_ipv4NetmaskLength = Lens.lens (\CreateVpc' {ipv4NetmaskLength} -> ipv4NetmaskLength) (\s@CreateVpc' {} a -> s {ipv4NetmaskLength = a} :: CreateVpc)

-- | The IPv6 CIDR block from the IPv6 address pool. You must also specify
-- @Ipv6Pool@ in the request.
--
-- To let Amazon choose the IPv6 CIDR block for you, omit this parameter.
createVpc_ipv6CidrBlock :: Lens.Lens' CreateVpc (Prelude.Maybe Prelude.Text)
createVpc_ipv6CidrBlock = Lens.lens (\CreateVpc' {ipv6CidrBlock} -> ipv6CidrBlock) (\s@CreateVpc' {} a -> s {ipv6CidrBlock = a} :: CreateVpc)

-- | The name of the location from which we advertise the IPV6 CIDR block.
-- Use this parameter to limit the address to this location.
--
-- You must set @AmazonProvidedIpv6CidrBlock@ to @true@ to use this
-- parameter.
createVpc_ipv6CidrBlockNetworkBorderGroup :: Lens.Lens' CreateVpc (Prelude.Maybe Prelude.Text)
createVpc_ipv6CidrBlockNetworkBorderGroup = Lens.lens (\CreateVpc' {ipv6CidrBlockNetworkBorderGroup} -> ipv6CidrBlockNetworkBorderGroup) (\s@CreateVpc' {} a -> s {ipv6CidrBlockNetworkBorderGroup = a} :: CreateVpc)

-- | The ID of an IPv6 IPAM pool which will be used to allocate this VPC an
-- IPv6 CIDR. IPAM is a VPC feature that you can use to automate your IP
-- address management workflows including assigning, tracking,
-- troubleshooting, and auditing IP addresses across Amazon Web Services
-- Regions and accounts throughout your Amazon Web Services Organization.
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
-- in the /Amazon VPC IPAM User Guide/.
createVpc_ipv6IpamPoolId :: Lens.Lens' CreateVpc (Prelude.Maybe Prelude.Text)
createVpc_ipv6IpamPoolId = Lens.lens (\CreateVpc' {ipv6IpamPoolId} -> ipv6IpamPoolId) (\s@CreateVpc' {} a -> s {ipv6IpamPoolId = a} :: CreateVpc)

-- | The netmask length of the IPv6 CIDR you want to allocate to this VPC
-- from an Amazon VPC IP Address Manager (IPAM) pool. For more information
-- about IPAM, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
-- in the /Amazon VPC IPAM User Guide/.
createVpc_ipv6NetmaskLength :: Lens.Lens' CreateVpc (Prelude.Maybe Prelude.Int)
createVpc_ipv6NetmaskLength = Lens.lens (\CreateVpc' {ipv6NetmaskLength} -> ipv6NetmaskLength) (\s@CreateVpc' {} a -> s {ipv6NetmaskLength = a} :: CreateVpc)

-- | The ID of an IPv6 address pool from which to allocate the IPv6 CIDR
-- block.
createVpc_ipv6Pool :: Lens.Lens' CreateVpc (Prelude.Maybe Prelude.Text)
createVpc_ipv6Pool = Lens.lens (\CreateVpc' {ipv6Pool} -> ipv6Pool) (\s@CreateVpc' {} a -> s {ipv6Pool = a} :: CreateVpc)

-- | The tags to assign to the VPC.
createVpc_tagSpecifications :: Lens.Lens' CreateVpc (Prelude.Maybe [TagSpecification])
createVpc_tagSpecifications = Lens.lens (\CreateVpc' {tagSpecifications} -> tagSpecifications) (\s@CreateVpc' {} a -> s {tagSpecifications = a} :: CreateVpc) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreateVpc where
  type AWSResponse CreateVpc = CreateVpcResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateVpcResponse'
            Prelude.<$> (x Data..@? "vpc")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVpc where
  hashWithSalt _salt CreateVpc' {..} =
    _salt
      `Prelude.hashWithSalt` amazonProvidedIpv6CidrBlock
      `Prelude.hashWithSalt` cidrBlock
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` instanceTenancy
      `Prelude.hashWithSalt` ipv4IpamPoolId
      `Prelude.hashWithSalt` ipv4NetmaskLength
      `Prelude.hashWithSalt` ipv6CidrBlock
      `Prelude.hashWithSalt` ipv6CidrBlockNetworkBorderGroup
      `Prelude.hashWithSalt` ipv6IpamPoolId
      `Prelude.hashWithSalt` ipv6NetmaskLength
      `Prelude.hashWithSalt` ipv6Pool
      `Prelude.hashWithSalt` tagSpecifications

instance Prelude.NFData CreateVpc where
  rnf CreateVpc' {..} =
    Prelude.rnf amazonProvidedIpv6CidrBlock
      `Prelude.seq` Prelude.rnf cidrBlock
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf instanceTenancy
      `Prelude.seq` Prelude.rnf ipv4IpamPoolId
      `Prelude.seq` Prelude.rnf ipv4NetmaskLength
      `Prelude.seq` Prelude.rnf ipv6CidrBlock
      `Prelude.seq` Prelude.rnf ipv6CidrBlockNetworkBorderGroup
      `Prelude.seq` Prelude.rnf ipv6IpamPoolId
      `Prelude.seq` Prelude.rnf ipv6NetmaskLength
      `Prelude.seq` Prelude.rnf ipv6Pool
      `Prelude.seq` Prelude.rnf tagSpecifications

instance Data.ToHeaders CreateVpc where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateVpc where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateVpc where
  toQuery CreateVpc' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateVpc" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "AmazonProvidedIpv6CidrBlock"
          Data.=: amazonProvidedIpv6CidrBlock,
        "CidrBlock" Data.=: cidrBlock,
        "DryRun" Data.=: dryRun,
        "InstanceTenancy" Data.=: instanceTenancy,
        "Ipv4IpamPoolId" Data.=: ipv4IpamPoolId,
        "Ipv4NetmaskLength" Data.=: ipv4NetmaskLength,
        "Ipv6CidrBlock" Data.=: ipv6CidrBlock,
        "Ipv6CidrBlockNetworkBorderGroup"
          Data.=: ipv6CidrBlockNetworkBorderGroup,
        "Ipv6IpamPoolId" Data.=: ipv6IpamPoolId,
        "Ipv6NetmaskLength" Data.=: ipv6NetmaskLength,
        "Ipv6Pool" Data.=: ipv6Pool,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          )
      ]

-- | /See:/ 'newCreateVpcResponse' smart constructor.
data CreateVpcResponse = CreateVpcResponse'
  { -- | Information about the VPC.
    vpc :: Prelude.Maybe Vpc,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpc', 'createVpcResponse_vpc' - Information about the VPC.
--
-- 'httpStatus', 'createVpcResponse_httpStatus' - The response's http status code.
newCreateVpcResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVpcResponse
newCreateVpcResponse pHttpStatus_ =
  CreateVpcResponse'
    { vpc = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the VPC.
createVpcResponse_vpc :: Lens.Lens' CreateVpcResponse (Prelude.Maybe Vpc)
createVpcResponse_vpc = Lens.lens (\CreateVpcResponse' {vpc} -> vpc) (\s@CreateVpcResponse' {} a -> s {vpc = a} :: CreateVpcResponse)

-- | The response's http status code.
createVpcResponse_httpStatus :: Lens.Lens' CreateVpcResponse Prelude.Int
createVpcResponse_httpStatus = Lens.lens (\CreateVpcResponse' {httpStatus} -> httpStatus) (\s@CreateVpcResponse' {} a -> s {httpStatus = a} :: CreateVpcResponse)

instance Prelude.NFData CreateVpcResponse where
  rnf CreateVpcResponse' {..} =
    Prelude.rnf vpc
      `Prelude.seq` Prelude.rnf httpStatus
