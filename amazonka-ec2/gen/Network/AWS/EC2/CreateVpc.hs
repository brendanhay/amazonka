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
-- Module      : Network.AWS.EC2.CreateVpc
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPC with the specified IPv4 CIDR block. The smallest VPC you
-- can create uses a \/28 netmask (16 IPv4 addresses), and the largest uses
-- a \/16 netmask (65,536 IPv4 addresses). For more information about how
-- large to make your VPC, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html Your VPC and Subnets>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- You can optionally request an IPv6 CIDR block for the VPC. You can
-- request an Amazon-provided IPv6 CIDR block from Amazon\'s pool of IPv6
-- addresses, or an IPv6 CIDR block from an IPv6 address pool that you
-- provisioned through bring your own IP addresses
-- (<https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-byoip.html BYOIP>).
--
-- By default, each instance you launch in the VPC has the default DHCP
-- options, which include only a default DNS server that we provide
-- (AmazonProvidedDNS). For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_DHCP_Options.html DHCP Options Sets>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- You can specify the instance tenancy value for the VPC when you create
-- it. You can\'t change this value for the VPC after you create it. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-instance.html Dedicated Instances>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.CreateVpc
  ( -- * Creating a Request
    CreateVpc (..),
    newCreateVpc,

    -- * Request Lenses
    createVpc_tagSpecifications,
    createVpc_dryRun,
    createVpc_instanceTenancy,
    createVpc_ipv6Pool,
    createVpc_ipv6CidrBlock,
    createVpc_ipv6CidrBlockNetworkBorderGroup,
    createVpc_amazonProvidedIpv6CidrBlock,
    createVpc_cidrBlock,

    -- * Destructuring the Response
    CreateVpcResponse (..),
    newCreateVpcResponse,

    -- * Response Lenses
    createVpcResponse_vpc,
    createVpcResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateVpc' smart constructor.
data CreateVpc = CreateVpc'
  { -- | The tags to assign to the VPC.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
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
    -- | The ID of an IPv6 address pool from which to allocate the IPv6 CIDR
    -- block.
    ipv6Pool :: Prelude.Maybe Prelude.Text,
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
    -- | Requests an Amazon-provided IPv6 CIDR block with a \/56 prefix length
    -- for the VPC. You cannot specify the range of IP addresses, or the size
    -- of the CIDR block.
    amazonProvidedIpv6CidrBlock :: Prelude.Maybe Prelude.Bool,
    -- | The IPv4 network range for the VPC, in CIDR notation. For example,
    -- @10.0.0.0\/16@. We modify the specified CIDR block to its canonical
    -- form; for example, if you specify @100.68.0.18\/18@, we modify it to
    -- @100.68.0.0\/18@.
    cidrBlock :: Prelude.Text
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
-- 'tagSpecifications', 'createVpc_tagSpecifications' - The tags to assign to the VPC.
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
-- 'ipv6Pool', 'createVpc_ipv6Pool' - The ID of an IPv6 address pool from which to allocate the IPv6 CIDR
-- block.
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
-- 'amazonProvidedIpv6CidrBlock', 'createVpc_amazonProvidedIpv6CidrBlock' - Requests an Amazon-provided IPv6 CIDR block with a \/56 prefix length
-- for the VPC. You cannot specify the range of IP addresses, or the size
-- of the CIDR block.
--
-- 'cidrBlock', 'createVpc_cidrBlock' - The IPv4 network range for the VPC, in CIDR notation. For example,
-- @10.0.0.0\/16@. We modify the specified CIDR block to its canonical
-- form; for example, if you specify @100.68.0.18\/18@, we modify it to
-- @100.68.0.0\/18@.
newCreateVpc ::
  -- | 'cidrBlock'
  Prelude.Text ->
  CreateVpc
newCreateVpc pCidrBlock_ =
  CreateVpc'
    { tagSpecifications = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      instanceTenancy = Prelude.Nothing,
      ipv6Pool = Prelude.Nothing,
      ipv6CidrBlock = Prelude.Nothing,
      ipv6CidrBlockNetworkBorderGroup = Prelude.Nothing,
      amazonProvidedIpv6CidrBlock = Prelude.Nothing,
      cidrBlock = pCidrBlock_
    }

-- | The tags to assign to the VPC.
createVpc_tagSpecifications :: Lens.Lens' CreateVpc (Prelude.Maybe [TagSpecification])
createVpc_tagSpecifications = Lens.lens (\CreateVpc' {tagSpecifications} -> tagSpecifications) (\s@CreateVpc' {} a -> s {tagSpecifications = a} :: CreateVpc) Prelude.. Lens.mapping Lens._Coerce

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

-- | The ID of an IPv6 address pool from which to allocate the IPv6 CIDR
-- block.
createVpc_ipv6Pool :: Lens.Lens' CreateVpc (Prelude.Maybe Prelude.Text)
createVpc_ipv6Pool = Lens.lens (\CreateVpc' {ipv6Pool} -> ipv6Pool) (\s@CreateVpc' {} a -> s {ipv6Pool = a} :: CreateVpc)

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

-- | Requests an Amazon-provided IPv6 CIDR block with a \/56 prefix length
-- for the VPC. You cannot specify the range of IP addresses, or the size
-- of the CIDR block.
createVpc_amazonProvidedIpv6CidrBlock :: Lens.Lens' CreateVpc (Prelude.Maybe Prelude.Bool)
createVpc_amazonProvidedIpv6CidrBlock = Lens.lens (\CreateVpc' {amazonProvidedIpv6CidrBlock} -> amazonProvidedIpv6CidrBlock) (\s@CreateVpc' {} a -> s {amazonProvidedIpv6CidrBlock = a} :: CreateVpc)

-- | The IPv4 network range for the VPC, in CIDR notation. For example,
-- @10.0.0.0\/16@. We modify the specified CIDR block to its canonical
-- form; for example, if you specify @100.68.0.18\/18@, we modify it to
-- @100.68.0.0\/18@.
createVpc_cidrBlock :: Lens.Lens' CreateVpc Prelude.Text
createVpc_cidrBlock = Lens.lens (\CreateVpc' {cidrBlock} -> cidrBlock) (\s@CreateVpc' {} a -> s {cidrBlock = a} :: CreateVpc)

instance Core.AWSRequest CreateVpc where
  type AWSResponse CreateVpc = CreateVpcResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateVpcResponse'
            Prelude.<$> (x Core..@? "vpc")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVpc

instance Prelude.NFData CreateVpc

instance Core.ToHeaders CreateVpc where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateVpc where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateVpc where
  toQuery CreateVpc' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateVpc" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "InstanceTenancy" Core.=: instanceTenancy,
        "Ipv6Pool" Core.=: ipv6Pool,
        "Ipv6CidrBlock" Core.=: ipv6CidrBlock,
        "Ipv6CidrBlockNetworkBorderGroup"
          Core.=: ipv6CidrBlockNetworkBorderGroup,
        "AmazonProvidedIpv6CidrBlock"
          Core.=: amazonProvidedIpv6CidrBlock,
        "CidrBlock" Core.=: cidrBlock
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

instance Prelude.NFData CreateVpcResponse
