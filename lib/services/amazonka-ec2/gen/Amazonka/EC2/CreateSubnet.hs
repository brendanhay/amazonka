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
-- Module      : Amazonka.EC2.CreateSubnet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a subnet in a specified VPC.
--
-- You must specify an IPv4 CIDR block for the subnet. After you create a
-- subnet, you can\'t change its CIDR block. The allowed block size is
-- between a \/16 netmask (65,536 IP addresses) and \/28 netmask (16 IP
-- addresses). The CIDR block must not overlap with the CIDR block of an
-- existing subnet in the VPC.
--
-- If you\'ve associated an IPv6 CIDR block with your VPC, you can create a
-- subnet with an IPv6 CIDR block that uses a \/64 prefix length.
--
-- Amazon Web Services reserves both the first four and the last IPv4
-- address in each subnet\'s CIDR block. They\'re not available for use.
--
-- If you add more than one subnet to a VPC, they\'re set up in a star
-- topology with a logical router in the middle.
--
-- When you stop an instance in a subnet, it retains its private IPv4
-- address. It\'s therefore possible to have a subnet with no running
-- instances (they\'re all stopped), but no remaining IP addresses
-- available.
--
-- For more information about subnets, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html Your VPC and subnets>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Amazonka.EC2.CreateSubnet
  ( -- * Creating a Request
    CreateSubnet (..),
    newCreateSubnet,

    -- * Request Lenses
    createSubnet_outpostArn,
    createSubnet_ipv6Native,
    createSubnet_availabilityZone,
    createSubnet_dryRun,
    createSubnet_cidrBlock,
    createSubnet_tagSpecifications,
    createSubnet_ipv6CidrBlock,
    createSubnet_availabilityZoneId,
    createSubnet_vpcId,

    -- * Destructuring the Response
    CreateSubnetResponse (..),
    newCreateSubnetResponse,

    -- * Response Lenses
    createSubnetResponse_subnet,
    createSubnetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSubnet' smart constructor.
data CreateSubnet = CreateSubnet'
  { -- | The Amazon Resource Name (ARN) of the Outpost. If you specify an Outpost
    -- ARN, you must also specify the Availability Zone of the Outpost subnet.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to create an IPv6 only subnet.
    ipv6Native :: Prelude.Maybe Prelude.Bool,
    -- | The Availability Zone or Local Zone for the subnet.
    --
    -- Default: Amazon Web Services selects one for you. If you create more
    -- than one subnet in your VPC, we do not necessarily select a different
    -- zone for each subnet.
    --
    -- To create a subnet in a Local Zone, set this value to the Local Zone ID,
    -- for example @us-west-2-lax-1a@. For information about the Regions that
    -- support Local Zones, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html#concepts-available-regions Available Regions>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    --
    -- To create a subnet in an Outpost, set this value to the Availability
    -- Zone for the Outpost and specify the Outpost ARN.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IPv4 network range for the subnet, in CIDR notation. For example,
    -- @10.0.0.0\/24@. We modify the specified CIDR block to its canonical
    -- form; for example, if you specify @100.68.0.18\/18@, we modify it to
    -- @100.68.0.0\/18@.
    --
    -- This parameter is not supported for an IPv6 only subnet.
    cidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The tags to assign to the subnet.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The IPv6 network range for the subnet, in CIDR notation. The subnet size
    -- must use a \/64 prefix length.
    --
    -- This parameter is required for an IPv6 only subnet.
    ipv6CidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The AZ ID or the Local Zone ID of the subnet.
    availabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSubnet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outpostArn', 'createSubnet_outpostArn' - The Amazon Resource Name (ARN) of the Outpost. If you specify an Outpost
-- ARN, you must also specify the Availability Zone of the Outpost subnet.
--
-- 'ipv6Native', 'createSubnet_ipv6Native' - Indicates whether to create an IPv6 only subnet.
--
-- 'availabilityZone', 'createSubnet_availabilityZone' - The Availability Zone or Local Zone for the subnet.
--
-- Default: Amazon Web Services selects one for you. If you create more
-- than one subnet in your VPC, we do not necessarily select a different
-- zone for each subnet.
--
-- To create a subnet in a Local Zone, set this value to the Local Zone ID,
-- for example @us-west-2-lax-1a@. For information about the Regions that
-- support Local Zones, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html#concepts-available-regions Available Regions>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- To create a subnet in an Outpost, set this value to the Availability
-- Zone for the Outpost and specify the Outpost ARN.
--
-- 'dryRun', 'createSubnet_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'cidrBlock', 'createSubnet_cidrBlock' - The IPv4 network range for the subnet, in CIDR notation. For example,
-- @10.0.0.0\/24@. We modify the specified CIDR block to its canonical
-- form; for example, if you specify @100.68.0.18\/18@, we modify it to
-- @100.68.0.0\/18@.
--
-- This parameter is not supported for an IPv6 only subnet.
--
-- 'tagSpecifications', 'createSubnet_tagSpecifications' - The tags to assign to the subnet.
--
-- 'ipv6CidrBlock', 'createSubnet_ipv6CidrBlock' - The IPv6 network range for the subnet, in CIDR notation. The subnet size
-- must use a \/64 prefix length.
--
-- This parameter is required for an IPv6 only subnet.
--
-- 'availabilityZoneId', 'createSubnet_availabilityZoneId' - The AZ ID or the Local Zone ID of the subnet.
--
-- 'vpcId', 'createSubnet_vpcId' - The ID of the VPC.
newCreateSubnet ::
  -- | 'vpcId'
  Prelude.Text ->
  CreateSubnet
newCreateSubnet pVpcId_ =
  CreateSubnet'
    { outpostArn = Prelude.Nothing,
      ipv6Native = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      cidrBlock = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      ipv6CidrBlock = Prelude.Nothing,
      availabilityZoneId = Prelude.Nothing,
      vpcId = pVpcId_
    }

-- | The Amazon Resource Name (ARN) of the Outpost. If you specify an Outpost
-- ARN, you must also specify the Availability Zone of the Outpost subnet.
createSubnet_outpostArn :: Lens.Lens' CreateSubnet (Prelude.Maybe Prelude.Text)
createSubnet_outpostArn = Lens.lens (\CreateSubnet' {outpostArn} -> outpostArn) (\s@CreateSubnet' {} a -> s {outpostArn = a} :: CreateSubnet)

-- | Indicates whether to create an IPv6 only subnet.
createSubnet_ipv6Native :: Lens.Lens' CreateSubnet (Prelude.Maybe Prelude.Bool)
createSubnet_ipv6Native = Lens.lens (\CreateSubnet' {ipv6Native} -> ipv6Native) (\s@CreateSubnet' {} a -> s {ipv6Native = a} :: CreateSubnet)

-- | The Availability Zone or Local Zone for the subnet.
--
-- Default: Amazon Web Services selects one for you. If you create more
-- than one subnet in your VPC, we do not necessarily select a different
-- zone for each subnet.
--
-- To create a subnet in a Local Zone, set this value to the Local Zone ID,
-- for example @us-west-2-lax-1a@. For information about the Regions that
-- support Local Zones, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html#concepts-available-regions Available Regions>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- To create a subnet in an Outpost, set this value to the Availability
-- Zone for the Outpost and specify the Outpost ARN.
createSubnet_availabilityZone :: Lens.Lens' CreateSubnet (Prelude.Maybe Prelude.Text)
createSubnet_availabilityZone = Lens.lens (\CreateSubnet' {availabilityZone} -> availabilityZone) (\s@CreateSubnet' {} a -> s {availabilityZone = a} :: CreateSubnet)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createSubnet_dryRun :: Lens.Lens' CreateSubnet (Prelude.Maybe Prelude.Bool)
createSubnet_dryRun = Lens.lens (\CreateSubnet' {dryRun} -> dryRun) (\s@CreateSubnet' {} a -> s {dryRun = a} :: CreateSubnet)

-- | The IPv4 network range for the subnet, in CIDR notation. For example,
-- @10.0.0.0\/24@. We modify the specified CIDR block to its canonical
-- form; for example, if you specify @100.68.0.18\/18@, we modify it to
-- @100.68.0.0\/18@.
--
-- This parameter is not supported for an IPv6 only subnet.
createSubnet_cidrBlock :: Lens.Lens' CreateSubnet (Prelude.Maybe Prelude.Text)
createSubnet_cidrBlock = Lens.lens (\CreateSubnet' {cidrBlock} -> cidrBlock) (\s@CreateSubnet' {} a -> s {cidrBlock = a} :: CreateSubnet)

-- | The tags to assign to the subnet.
createSubnet_tagSpecifications :: Lens.Lens' CreateSubnet (Prelude.Maybe [TagSpecification])
createSubnet_tagSpecifications = Lens.lens (\CreateSubnet' {tagSpecifications} -> tagSpecifications) (\s@CreateSubnet' {} a -> s {tagSpecifications = a} :: CreateSubnet) Prelude.. Lens.mapping Lens.coerced

-- | The IPv6 network range for the subnet, in CIDR notation. The subnet size
-- must use a \/64 prefix length.
--
-- This parameter is required for an IPv6 only subnet.
createSubnet_ipv6CidrBlock :: Lens.Lens' CreateSubnet (Prelude.Maybe Prelude.Text)
createSubnet_ipv6CidrBlock = Lens.lens (\CreateSubnet' {ipv6CidrBlock} -> ipv6CidrBlock) (\s@CreateSubnet' {} a -> s {ipv6CidrBlock = a} :: CreateSubnet)

-- | The AZ ID or the Local Zone ID of the subnet.
createSubnet_availabilityZoneId :: Lens.Lens' CreateSubnet (Prelude.Maybe Prelude.Text)
createSubnet_availabilityZoneId = Lens.lens (\CreateSubnet' {availabilityZoneId} -> availabilityZoneId) (\s@CreateSubnet' {} a -> s {availabilityZoneId = a} :: CreateSubnet)

-- | The ID of the VPC.
createSubnet_vpcId :: Lens.Lens' CreateSubnet Prelude.Text
createSubnet_vpcId = Lens.lens (\CreateSubnet' {vpcId} -> vpcId) (\s@CreateSubnet' {} a -> s {vpcId = a} :: CreateSubnet)

instance Core.AWSRequest CreateSubnet where
  type AWSResponse CreateSubnet = CreateSubnetResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateSubnetResponse'
            Prelude.<$> (x Core..@? "subnet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSubnet where
  hashWithSalt _salt CreateSubnet' {..} =
    _salt `Prelude.hashWithSalt` outpostArn
      `Prelude.hashWithSalt` ipv6Native
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` cidrBlock
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` ipv6CidrBlock
      `Prelude.hashWithSalt` availabilityZoneId
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData CreateSubnet where
  rnf CreateSubnet' {..} =
    Prelude.rnf outpostArn
      `Prelude.seq` Prelude.rnf ipv6Native
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf cidrBlock
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf ipv6CidrBlock
      `Prelude.seq` Prelude.rnf availabilityZoneId
      `Prelude.seq` Prelude.rnf vpcId

instance Core.ToHeaders CreateSubnet where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateSubnet where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateSubnet where
  toQuery CreateSubnet' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateSubnet" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "OutpostArn" Core.=: outpostArn,
        "Ipv6Native" Core.=: ipv6Native,
        "AvailabilityZone" Core.=: availabilityZone,
        "DryRun" Core.=: dryRun,
        "CidrBlock" Core.=: cidrBlock,
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "Ipv6CidrBlock" Core.=: ipv6CidrBlock,
        "AvailabilityZoneId" Core.=: availabilityZoneId,
        "VpcId" Core.=: vpcId
      ]

-- | /See:/ 'newCreateSubnetResponse' smart constructor.
data CreateSubnetResponse = CreateSubnetResponse'
  { -- | Information about the subnet.
    subnet :: Prelude.Maybe Subnet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSubnetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnet', 'createSubnetResponse_subnet' - Information about the subnet.
--
-- 'httpStatus', 'createSubnetResponse_httpStatus' - The response's http status code.
newCreateSubnetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSubnetResponse
newCreateSubnetResponse pHttpStatus_ =
  CreateSubnetResponse'
    { subnet = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the subnet.
createSubnetResponse_subnet :: Lens.Lens' CreateSubnetResponse (Prelude.Maybe Subnet)
createSubnetResponse_subnet = Lens.lens (\CreateSubnetResponse' {subnet} -> subnet) (\s@CreateSubnetResponse' {} a -> s {subnet = a} :: CreateSubnetResponse)

-- | The response's http status code.
createSubnetResponse_httpStatus :: Lens.Lens' CreateSubnetResponse Prelude.Int
createSubnetResponse_httpStatus = Lens.lens (\CreateSubnetResponse' {httpStatus} -> httpStatus) (\s@CreateSubnetResponse' {} a -> s {httpStatus = a} :: CreateSubnetResponse)

instance Prelude.NFData CreateSubnetResponse where
  rnf CreateSubnetResponse' {..} =
    Prelude.rnf subnet
      `Prelude.seq` Prelude.rnf httpStatus
