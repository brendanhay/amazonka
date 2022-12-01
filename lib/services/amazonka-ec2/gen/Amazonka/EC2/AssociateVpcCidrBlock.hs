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
-- Module      : Amazonka.EC2.AssociateVpcCidrBlock
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a CIDR block with your VPC. You can associate a secondary
-- IPv4 CIDR block, an Amazon-provided IPv6 CIDR block, or an IPv6 CIDR
-- block from an IPv6 address pool that you provisioned through bring your
-- own IP addresses
-- (<https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-byoip.html BYOIP>).
-- The IPv6 CIDR block size is fixed at \/56.
--
-- You must specify one of the following in the request: an IPv4 CIDR
-- block, an IPv6 pool, or an Amazon-provided IPv6 CIDR block.
--
-- For more information about associating CIDR blocks with your VPC and
-- applicable restrictions, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html#VPC_Sizing VPC and subnet sizing>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Amazonka.EC2.AssociateVpcCidrBlock
  ( -- * Creating a Request
    AssociateVpcCidrBlock (..),
    newAssociateVpcCidrBlock,

    -- * Request Lenses
    associateVpcCidrBlock_ipv6CidrBlockNetworkBorderGroup,
    associateVpcCidrBlock_ipv4IpamPoolId,
    associateVpcCidrBlock_ipv6Pool,
    associateVpcCidrBlock_ipv4NetmaskLength,
    associateVpcCidrBlock_ipv6IpamPoolId,
    associateVpcCidrBlock_amazonProvidedIpv6CidrBlock,
    associateVpcCidrBlock_ipv6NetmaskLength,
    associateVpcCidrBlock_cidrBlock,
    associateVpcCidrBlock_ipv6CidrBlock,
    associateVpcCidrBlock_vpcId,

    -- * Destructuring the Response
    AssociateVpcCidrBlockResponse (..),
    newAssociateVpcCidrBlockResponse,

    -- * Response Lenses
    associateVpcCidrBlockResponse_cidrBlockAssociation,
    associateVpcCidrBlockResponse_ipv6CidrBlockAssociation,
    associateVpcCidrBlockResponse_vpcId,
    associateVpcCidrBlockResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateVpcCidrBlock' smart constructor.
data AssociateVpcCidrBlock = AssociateVpcCidrBlock'
  { -- | The name of the location from which we advertise the IPV6 CIDR block.
    -- Use this parameter to limit the CIDR block to this location.
    --
    -- You must set @AmazonProvidedIpv6CidrBlock@ to @true@ to use this
    -- parameter.
    --
    -- You can have one IPv6 CIDR block association per network border group.
    ipv6CidrBlockNetworkBorderGroup :: Prelude.Maybe Prelude.Text,
    -- | Associate a CIDR allocated from an IPv4 IPAM pool to a VPC. For more
    -- information about Amazon VPC IP Address Manager (IPAM), see
    -- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
    -- in the /Amazon VPC IPAM User Guide/.
    ipv4IpamPoolId :: Prelude.Maybe Prelude.Text,
    -- | The ID of an IPv6 address pool from which to allocate the IPv6 CIDR
    -- block.
    ipv6Pool :: Prelude.Maybe Prelude.Text,
    -- | The netmask length of the IPv4 CIDR you would like to associate from an
    -- Amazon VPC IP Address Manager (IPAM) pool. For more information about
    -- IPAM, see
    -- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
    -- in the /Amazon VPC IPAM User Guide/.
    ipv4NetmaskLength :: Prelude.Maybe Prelude.Int,
    -- | Associates a CIDR allocated from an IPv6 IPAM pool to a VPC. For more
    -- information about Amazon VPC IP Address Manager (IPAM), see
    -- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
    -- in the /Amazon VPC IPAM User Guide/.
    ipv6IpamPoolId :: Prelude.Maybe Prelude.Text,
    -- | Requests an Amazon-provided IPv6 CIDR block with a \/56 prefix length
    -- for the VPC. You cannot specify the range of IPv6 addresses, or the size
    -- of the CIDR block.
    amazonProvidedIpv6CidrBlock :: Prelude.Maybe Prelude.Bool,
    -- | The netmask length of the IPv6 CIDR you would like to associate from an
    -- Amazon VPC IP Address Manager (IPAM) pool. For more information about
    -- IPAM, see
    -- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
    -- in the /Amazon VPC IPAM User Guide/.
    ipv6NetmaskLength :: Prelude.Maybe Prelude.Int,
    -- | An IPv4 CIDR block to associate with the VPC.
    cidrBlock :: Prelude.Maybe Prelude.Text,
    -- | An IPv6 CIDR block from the IPv6 address pool. You must also specify
    -- @Ipv6Pool@ in the request.
    --
    -- To let Amazon choose the IPv6 CIDR block for you, omit this parameter.
    ipv6CidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateVpcCidrBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6CidrBlockNetworkBorderGroup', 'associateVpcCidrBlock_ipv6CidrBlockNetworkBorderGroup' - The name of the location from which we advertise the IPV6 CIDR block.
-- Use this parameter to limit the CIDR block to this location.
--
-- You must set @AmazonProvidedIpv6CidrBlock@ to @true@ to use this
-- parameter.
--
-- You can have one IPv6 CIDR block association per network border group.
--
-- 'ipv4IpamPoolId', 'associateVpcCidrBlock_ipv4IpamPoolId' - Associate a CIDR allocated from an IPv4 IPAM pool to a VPC. For more
-- information about Amazon VPC IP Address Manager (IPAM), see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
-- in the /Amazon VPC IPAM User Guide/.
--
-- 'ipv6Pool', 'associateVpcCidrBlock_ipv6Pool' - The ID of an IPv6 address pool from which to allocate the IPv6 CIDR
-- block.
--
-- 'ipv4NetmaskLength', 'associateVpcCidrBlock_ipv4NetmaskLength' - The netmask length of the IPv4 CIDR you would like to associate from an
-- Amazon VPC IP Address Manager (IPAM) pool. For more information about
-- IPAM, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
-- in the /Amazon VPC IPAM User Guide/.
--
-- 'ipv6IpamPoolId', 'associateVpcCidrBlock_ipv6IpamPoolId' - Associates a CIDR allocated from an IPv6 IPAM pool to a VPC. For more
-- information about Amazon VPC IP Address Manager (IPAM), see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
-- in the /Amazon VPC IPAM User Guide/.
--
-- 'amazonProvidedIpv6CidrBlock', 'associateVpcCidrBlock_amazonProvidedIpv6CidrBlock' - Requests an Amazon-provided IPv6 CIDR block with a \/56 prefix length
-- for the VPC. You cannot specify the range of IPv6 addresses, or the size
-- of the CIDR block.
--
-- 'ipv6NetmaskLength', 'associateVpcCidrBlock_ipv6NetmaskLength' - The netmask length of the IPv6 CIDR you would like to associate from an
-- Amazon VPC IP Address Manager (IPAM) pool. For more information about
-- IPAM, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
-- in the /Amazon VPC IPAM User Guide/.
--
-- 'cidrBlock', 'associateVpcCidrBlock_cidrBlock' - An IPv4 CIDR block to associate with the VPC.
--
-- 'ipv6CidrBlock', 'associateVpcCidrBlock_ipv6CidrBlock' - An IPv6 CIDR block from the IPv6 address pool. You must also specify
-- @Ipv6Pool@ in the request.
--
-- To let Amazon choose the IPv6 CIDR block for you, omit this parameter.
--
-- 'vpcId', 'associateVpcCidrBlock_vpcId' - The ID of the VPC.
newAssociateVpcCidrBlock ::
  -- | 'vpcId'
  Prelude.Text ->
  AssociateVpcCidrBlock
newAssociateVpcCidrBlock pVpcId_ =
  AssociateVpcCidrBlock'
    { ipv6CidrBlockNetworkBorderGroup =
        Prelude.Nothing,
      ipv4IpamPoolId = Prelude.Nothing,
      ipv6Pool = Prelude.Nothing,
      ipv4NetmaskLength = Prelude.Nothing,
      ipv6IpamPoolId = Prelude.Nothing,
      amazonProvidedIpv6CidrBlock = Prelude.Nothing,
      ipv6NetmaskLength = Prelude.Nothing,
      cidrBlock = Prelude.Nothing,
      ipv6CidrBlock = Prelude.Nothing,
      vpcId = pVpcId_
    }

-- | The name of the location from which we advertise the IPV6 CIDR block.
-- Use this parameter to limit the CIDR block to this location.
--
-- You must set @AmazonProvidedIpv6CidrBlock@ to @true@ to use this
-- parameter.
--
-- You can have one IPv6 CIDR block association per network border group.
associateVpcCidrBlock_ipv6CidrBlockNetworkBorderGroup :: Lens.Lens' AssociateVpcCidrBlock (Prelude.Maybe Prelude.Text)
associateVpcCidrBlock_ipv6CidrBlockNetworkBorderGroup = Lens.lens (\AssociateVpcCidrBlock' {ipv6CidrBlockNetworkBorderGroup} -> ipv6CidrBlockNetworkBorderGroup) (\s@AssociateVpcCidrBlock' {} a -> s {ipv6CidrBlockNetworkBorderGroup = a} :: AssociateVpcCidrBlock)

-- | Associate a CIDR allocated from an IPv4 IPAM pool to a VPC. For more
-- information about Amazon VPC IP Address Manager (IPAM), see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
-- in the /Amazon VPC IPAM User Guide/.
associateVpcCidrBlock_ipv4IpamPoolId :: Lens.Lens' AssociateVpcCidrBlock (Prelude.Maybe Prelude.Text)
associateVpcCidrBlock_ipv4IpamPoolId = Lens.lens (\AssociateVpcCidrBlock' {ipv4IpamPoolId} -> ipv4IpamPoolId) (\s@AssociateVpcCidrBlock' {} a -> s {ipv4IpamPoolId = a} :: AssociateVpcCidrBlock)

-- | The ID of an IPv6 address pool from which to allocate the IPv6 CIDR
-- block.
associateVpcCidrBlock_ipv6Pool :: Lens.Lens' AssociateVpcCidrBlock (Prelude.Maybe Prelude.Text)
associateVpcCidrBlock_ipv6Pool = Lens.lens (\AssociateVpcCidrBlock' {ipv6Pool} -> ipv6Pool) (\s@AssociateVpcCidrBlock' {} a -> s {ipv6Pool = a} :: AssociateVpcCidrBlock)

-- | The netmask length of the IPv4 CIDR you would like to associate from an
-- Amazon VPC IP Address Manager (IPAM) pool. For more information about
-- IPAM, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
-- in the /Amazon VPC IPAM User Guide/.
associateVpcCidrBlock_ipv4NetmaskLength :: Lens.Lens' AssociateVpcCidrBlock (Prelude.Maybe Prelude.Int)
associateVpcCidrBlock_ipv4NetmaskLength = Lens.lens (\AssociateVpcCidrBlock' {ipv4NetmaskLength} -> ipv4NetmaskLength) (\s@AssociateVpcCidrBlock' {} a -> s {ipv4NetmaskLength = a} :: AssociateVpcCidrBlock)

-- | Associates a CIDR allocated from an IPv6 IPAM pool to a VPC. For more
-- information about Amazon VPC IP Address Manager (IPAM), see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
-- in the /Amazon VPC IPAM User Guide/.
associateVpcCidrBlock_ipv6IpamPoolId :: Lens.Lens' AssociateVpcCidrBlock (Prelude.Maybe Prelude.Text)
associateVpcCidrBlock_ipv6IpamPoolId = Lens.lens (\AssociateVpcCidrBlock' {ipv6IpamPoolId} -> ipv6IpamPoolId) (\s@AssociateVpcCidrBlock' {} a -> s {ipv6IpamPoolId = a} :: AssociateVpcCidrBlock)

-- | Requests an Amazon-provided IPv6 CIDR block with a \/56 prefix length
-- for the VPC. You cannot specify the range of IPv6 addresses, or the size
-- of the CIDR block.
associateVpcCidrBlock_amazonProvidedIpv6CidrBlock :: Lens.Lens' AssociateVpcCidrBlock (Prelude.Maybe Prelude.Bool)
associateVpcCidrBlock_amazonProvidedIpv6CidrBlock = Lens.lens (\AssociateVpcCidrBlock' {amazonProvidedIpv6CidrBlock} -> amazonProvidedIpv6CidrBlock) (\s@AssociateVpcCidrBlock' {} a -> s {amazonProvidedIpv6CidrBlock = a} :: AssociateVpcCidrBlock)

-- | The netmask length of the IPv6 CIDR you would like to associate from an
-- Amazon VPC IP Address Manager (IPAM) pool. For more information about
-- IPAM, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
-- in the /Amazon VPC IPAM User Guide/.
associateVpcCidrBlock_ipv6NetmaskLength :: Lens.Lens' AssociateVpcCidrBlock (Prelude.Maybe Prelude.Int)
associateVpcCidrBlock_ipv6NetmaskLength = Lens.lens (\AssociateVpcCidrBlock' {ipv6NetmaskLength} -> ipv6NetmaskLength) (\s@AssociateVpcCidrBlock' {} a -> s {ipv6NetmaskLength = a} :: AssociateVpcCidrBlock)

-- | An IPv4 CIDR block to associate with the VPC.
associateVpcCidrBlock_cidrBlock :: Lens.Lens' AssociateVpcCidrBlock (Prelude.Maybe Prelude.Text)
associateVpcCidrBlock_cidrBlock = Lens.lens (\AssociateVpcCidrBlock' {cidrBlock} -> cidrBlock) (\s@AssociateVpcCidrBlock' {} a -> s {cidrBlock = a} :: AssociateVpcCidrBlock)

-- | An IPv6 CIDR block from the IPv6 address pool. You must also specify
-- @Ipv6Pool@ in the request.
--
-- To let Amazon choose the IPv6 CIDR block for you, omit this parameter.
associateVpcCidrBlock_ipv6CidrBlock :: Lens.Lens' AssociateVpcCidrBlock (Prelude.Maybe Prelude.Text)
associateVpcCidrBlock_ipv6CidrBlock = Lens.lens (\AssociateVpcCidrBlock' {ipv6CidrBlock} -> ipv6CidrBlock) (\s@AssociateVpcCidrBlock' {} a -> s {ipv6CidrBlock = a} :: AssociateVpcCidrBlock)

-- | The ID of the VPC.
associateVpcCidrBlock_vpcId :: Lens.Lens' AssociateVpcCidrBlock Prelude.Text
associateVpcCidrBlock_vpcId = Lens.lens (\AssociateVpcCidrBlock' {vpcId} -> vpcId) (\s@AssociateVpcCidrBlock' {} a -> s {vpcId = a} :: AssociateVpcCidrBlock)

instance Core.AWSRequest AssociateVpcCidrBlock where
  type
    AWSResponse AssociateVpcCidrBlock =
      AssociateVpcCidrBlockResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateVpcCidrBlockResponse'
            Prelude.<$> (x Core..@? "cidrBlockAssociation")
            Prelude.<*> (x Core..@? "ipv6CidrBlockAssociation")
            Prelude.<*> (x Core..@? "vpcId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateVpcCidrBlock where
  hashWithSalt _salt AssociateVpcCidrBlock' {..} =
    _salt
      `Prelude.hashWithSalt` ipv6CidrBlockNetworkBorderGroup
      `Prelude.hashWithSalt` ipv4IpamPoolId
      `Prelude.hashWithSalt` ipv6Pool
      `Prelude.hashWithSalt` ipv4NetmaskLength
      `Prelude.hashWithSalt` ipv6IpamPoolId
      `Prelude.hashWithSalt` amazonProvidedIpv6CidrBlock
      `Prelude.hashWithSalt` ipv6NetmaskLength
      `Prelude.hashWithSalt` cidrBlock
      `Prelude.hashWithSalt` ipv6CidrBlock
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData AssociateVpcCidrBlock where
  rnf AssociateVpcCidrBlock' {..} =
    Prelude.rnf ipv6CidrBlockNetworkBorderGroup
      `Prelude.seq` Prelude.rnf ipv4IpamPoolId
      `Prelude.seq` Prelude.rnf ipv6Pool
      `Prelude.seq` Prelude.rnf ipv4NetmaskLength
      `Prelude.seq` Prelude.rnf ipv6IpamPoolId
      `Prelude.seq` Prelude.rnf amazonProvidedIpv6CidrBlock
      `Prelude.seq` Prelude.rnf ipv6NetmaskLength
      `Prelude.seq` Prelude.rnf cidrBlock
      `Prelude.seq` Prelude.rnf ipv6CidrBlock
      `Prelude.seq` Prelude.rnf vpcId

instance Core.ToHeaders AssociateVpcCidrBlock where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath AssociateVpcCidrBlock where
  toPath = Prelude.const "/"

instance Core.ToQuery AssociateVpcCidrBlock where
  toQuery AssociateVpcCidrBlock' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("AssociateVpcCidrBlock" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "Ipv6CidrBlockNetworkBorderGroup"
          Core.=: ipv6CidrBlockNetworkBorderGroup,
        "Ipv4IpamPoolId" Core.=: ipv4IpamPoolId,
        "Ipv6Pool" Core.=: ipv6Pool,
        "Ipv4NetmaskLength" Core.=: ipv4NetmaskLength,
        "Ipv6IpamPoolId" Core.=: ipv6IpamPoolId,
        "AmazonProvidedIpv6CidrBlock"
          Core.=: amazonProvidedIpv6CidrBlock,
        "Ipv6NetmaskLength" Core.=: ipv6NetmaskLength,
        "CidrBlock" Core.=: cidrBlock,
        "Ipv6CidrBlock" Core.=: ipv6CidrBlock,
        "VpcId" Core.=: vpcId
      ]

-- | /See:/ 'newAssociateVpcCidrBlockResponse' smart constructor.
data AssociateVpcCidrBlockResponse = AssociateVpcCidrBlockResponse'
  { -- | Information about the IPv4 CIDR block association.
    cidrBlockAssociation :: Prelude.Maybe VpcCidrBlockAssociation,
    -- | Information about the IPv6 CIDR block association.
    ipv6CidrBlockAssociation :: Prelude.Maybe VpcIpv6CidrBlockAssociation,
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateVpcCidrBlockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrBlockAssociation', 'associateVpcCidrBlockResponse_cidrBlockAssociation' - Information about the IPv4 CIDR block association.
--
-- 'ipv6CidrBlockAssociation', 'associateVpcCidrBlockResponse_ipv6CidrBlockAssociation' - Information about the IPv6 CIDR block association.
--
-- 'vpcId', 'associateVpcCidrBlockResponse_vpcId' - The ID of the VPC.
--
-- 'httpStatus', 'associateVpcCidrBlockResponse_httpStatus' - The response's http status code.
newAssociateVpcCidrBlockResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateVpcCidrBlockResponse
newAssociateVpcCidrBlockResponse pHttpStatus_ =
  AssociateVpcCidrBlockResponse'
    { cidrBlockAssociation =
        Prelude.Nothing,
      ipv6CidrBlockAssociation = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the IPv4 CIDR block association.
associateVpcCidrBlockResponse_cidrBlockAssociation :: Lens.Lens' AssociateVpcCidrBlockResponse (Prelude.Maybe VpcCidrBlockAssociation)
associateVpcCidrBlockResponse_cidrBlockAssociation = Lens.lens (\AssociateVpcCidrBlockResponse' {cidrBlockAssociation} -> cidrBlockAssociation) (\s@AssociateVpcCidrBlockResponse' {} a -> s {cidrBlockAssociation = a} :: AssociateVpcCidrBlockResponse)

-- | Information about the IPv6 CIDR block association.
associateVpcCidrBlockResponse_ipv6CidrBlockAssociation :: Lens.Lens' AssociateVpcCidrBlockResponse (Prelude.Maybe VpcIpv6CidrBlockAssociation)
associateVpcCidrBlockResponse_ipv6CidrBlockAssociation = Lens.lens (\AssociateVpcCidrBlockResponse' {ipv6CidrBlockAssociation} -> ipv6CidrBlockAssociation) (\s@AssociateVpcCidrBlockResponse' {} a -> s {ipv6CidrBlockAssociation = a} :: AssociateVpcCidrBlockResponse)

-- | The ID of the VPC.
associateVpcCidrBlockResponse_vpcId :: Lens.Lens' AssociateVpcCidrBlockResponse (Prelude.Maybe Prelude.Text)
associateVpcCidrBlockResponse_vpcId = Lens.lens (\AssociateVpcCidrBlockResponse' {vpcId} -> vpcId) (\s@AssociateVpcCidrBlockResponse' {} a -> s {vpcId = a} :: AssociateVpcCidrBlockResponse)

-- | The response's http status code.
associateVpcCidrBlockResponse_httpStatus :: Lens.Lens' AssociateVpcCidrBlockResponse Prelude.Int
associateVpcCidrBlockResponse_httpStatus = Lens.lens (\AssociateVpcCidrBlockResponse' {httpStatus} -> httpStatus) (\s@AssociateVpcCidrBlockResponse' {} a -> s {httpStatus = a} :: AssociateVpcCidrBlockResponse)

instance Prelude.NFData AssociateVpcCidrBlockResponse where
  rnf AssociateVpcCidrBlockResponse' {..} =
    Prelude.rnf cidrBlockAssociation
      `Prelude.seq` Prelude.rnf ipv6CidrBlockAssociation
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf httpStatus
