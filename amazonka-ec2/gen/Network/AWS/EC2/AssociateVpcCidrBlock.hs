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
-- Module      : Network.AWS.EC2.AssociateVpcCidrBlock
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html#VPC_Sizing VPC and Subnet Sizing>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Network.AWS.EC2.AssociateVpcCidrBlock
  ( -- * Creating a Request
    AssociateVpcCidrBlock (..),
    newAssociateVpcCidrBlock,

    -- * Request Lenses
    associateVpcCidrBlock_ipv6Pool,
    associateVpcCidrBlock_ipv6CidrBlock,
    associateVpcCidrBlock_ipv6CidrBlockNetworkBorderGroup,
    associateVpcCidrBlock_amazonProvidedIpv6CidrBlock,
    associateVpcCidrBlock_cidrBlock,
    associateVpcCidrBlock_vpcId,

    -- * Destructuring the Response
    AssociateVpcCidrBlockResponse (..),
    newAssociateVpcCidrBlockResponse,

    -- * Response Lenses
    associateVpcCidrBlockResponse_ipv6CidrBlockAssociation,
    associateVpcCidrBlockResponse_cidrBlockAssociation,
    associateVpcCidrBlockResponse_vpcId,
    associateVpcCidrBlockResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateVpcCidrBlock' smart constructor.
data AssociateVpcCidrBlock = AssociateVpcCidrBlock'
  { -- | The ID of an IPv6 address pool from which to allocate the IPv6 CIDR
    -- block.
    ipv6Pool :: Prelude.Maybe Prelude.Text,
    -- | An IPv6 CIDR block from the IPv6 address pool. You must also specify
    -- @Ipv6Pool@ in the request.
    --
    -- To let Amazon choose the IPv6 CIDR block for you, omit this parameter.
    ipv6CidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The name of the location from which we advertise the IPV6 CIDR block.
    -- Use this parameter to limit the CIDR block to this location.
    --
    -- You must set @AmazonProvidedIpv6CidrBlock@ to @true@ to use this
    -- parameter.
    --
    -- You can have one IPv6 CIDR block association per network border group.
    ipv6CidrBlockNetworkBorderGroup :: Prelude.Maybe Prelude.Text,
    -- | Requests an Amazon-provided IPv6 CIDR block with a \/56 prefix length
    -- for the VPC. You cannot specify the range of IPv6 addresses, or the size
    -- of the CIDR block.
    amazonProvidedIpv6CidrBlock :: Prelude.Maybe Prelude.Bool,
    -- | An IPv4 CIDR block to associate with the VPC.
    cidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateVpcCidrBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6Pool', 'associateVpcCidrBlock_ipv6Pool' - The ID of an IPv6 address pool from which to allocate the IPv6 CIDR
-- block.
--
-- 'ipv6CidrBlock', 'associateVpcCidrBlock_ipv6CidrBlock' - An IPv6 CIDR block from the IPv6 address pool. You must also specify
-- @Ipv6Pool@ in the request.
--
-- To let Amazon choose the IPv6 CIDR block for you, omit this parameter.
--
-- 'ipv6CidrBlockNetworkBorderGroup', 'associateVpcCidrBlock_ipv6CidrBlockNetworkBorderGroup' - The name of the location from which we advertise the IPV6 CIDR block.
-- Use this parameter to limit the CIDR block to this location.
--
-- You must set @AmazonProvidedIpv6CidrBlock@ to @true@ to use this
-- parameter.
--
-- You can have one IPv6 CIDR block association per network border group.
--
-- 'amazonProvidedIpv6CidrBlock', 'associateVpcCidrBlock_amazonProvidedIpv6CidrBlock' - Requests an Amazon-provided IPv6 CIDR block with a \/56 prefix length
-- for the VPC. You cannot specify the range of IPv6 addresses, or the size
-- of the CIDR block.
--
-- 'cidrBlock', 'associateVpcCidrBlock_cidrBlock' - An IPv4 CIDR block to associate with the VPC.
--
-- 'vpcId', 'associateVpcCidrBlock_vpcId' - The ID of the VPC.
newAssociateVpcCidrBlock ::
  -- | 'vpcId'
  Prelude.Text ->
  AssociateVpcCidrBlock
newAssociateVpcCidrBlock pVpcId_ =
  AssociateVpcCidrBlock'
    { ipv6Pool = Prelude.Nothing,
      ipv6CidrBlock = Prelude.Nothing,
      ipv6CidrBlockNetworkBorderGroup = Prelude.Nothing,
      amazonProvidedIpv6CidrBlock = Prelude.Nothing,
      cidrBlock = Prelude.Nothing,
      vpcId = pVpcId_
    }

-- | The ID of an IPv6 address pool from which to allocate the IPv6 CIDR
-- block.
associateVpcCidrBlock_ipv6Pool :: Lens.Lens' AssociateVpcCidrBlock (Prelude.Maybe Prelude.Text)
associateVpcCidrBlock_ipv6Pool = Lens.lens (\AssociateVpcCidrBlock' {ipv6Pool} -> ipv6Pool) (\s@AssociateVpcCidrBlock' {} a -> s {ipv6Pool = a} :: AssociateVpcCidrBlock)

-- | An IPv6 CIDR block from the IPv6 address pool. You must also specify
-- @Ipv6Pool@ in the request.
--
-- To let Amazon choose the IPv6 CIDR block for you, omit this parameter.
associateVpcCidrBlock_ipv6CidrBlock :: Lens.Lens' AssociateVpcCidrBlock (Prelude.Maybe Prelude.Text)
associateVpcCidrBlock_ipv6CidrBlock = Lens.lens (\AssociateVpcCidrBlock' {ipv6CidrBlock} -> ipv6CidrBlock) (\s@AssociateVpcCidrBlock' {} a -> s {ipv6CidrBlock = a} :: AssociateVpcCidrBlock)

-- | The name of the location from which we advertise the IPV6 CIDR block.
-- Use this parameter to limit the CIDR block to this location.
--
-- You must set @AmazonProvidedIpv6CidrBlock@ to @true@ to use this
-- parameter.
--
-- You can have one IPv6 CIDR block association per network border group.
associateVpcCidrBlock_ipv6CidrBlockNetworkBorderGroup :: Lens.Lens' AssociateVpcCidrBlock (Prelude.Maybe Prelude.Text)
associateVpcCidrBlock_ipv6CidrBlockNetworkBorderGroup = Lens.lens (\AssociateVpcCidrBlock' {ipv6CidrBlockNetworkBorderGroup} -> ipv6CidrBlockNetworkBorderGroup) (\s@AssociateVpcCidrBlock' {} a -> s {ipv6CidrBlockNetworkBorderGroup = a} :: AssociateVpcCidrBlock)

-- | Requests an Amazon-provided IPv6 CIDR block with a \/56 prefix length
-- for the VPC. You cannot specify the range of IPv6 addresses, or the size
-- of the CIDR block.
associateVpcCidrBlock_amazonProvidedIpv6CidrBlock :: Lens.Lens' AssociateVpcCidrBlock (Prelude.Maybe Prelude.Bool)
associateVpcCidrBlock_amazonProvidedIpv6CidrBlock = Lens.lens (\AssociateVpcCidrBlock' {amazonProvidedIpv6CidrBlock} -> amazonProvidedIpv6CidrBlock) (\s@AssociateVpcCidrBlock' {} a -> s {amazonProvidedIpv6CidrBlock = a} :: AssociateVpcCidrBlock)

-- | An IPv4 CIDR block to associate with the VPC.
associateVpcCidrBlock_cidrBlock :: Lens.Lens' AssociateVpcCidrBlock (Prelude.Maybe Prelude.Text)
associateVpcCidrBlock_cidrBlock = Lens.lens (\AssociateVpcCidrBlock' {cidrBlock} -> cidrBlock) (\s@AssociateVpcCidrBlock' {} a -> s {cidrBlock = a} :: AssociateVpcCidrBlock)

-- | The ID of the VPC.
associateVpcCidrBlock_vpcId :: Lens.Lens' AssociateVpcCidrBlock Prelude.Text
associateVpcCidrBlock_vpcId = Lens.lens (\AssociateVpcCidrBlock' {vpcId} -> vpcId) (\s@AssociateVpcCidrBlock' {} a -> s {vpcId = a} :: AssociateVpcCidrBlock)

instance Prelude.AWSRequest AssociateVpcCidrBlock where
  type
    Rs AssociateVpcCidrBlock =
      AssociateVpcCidrBlockResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateVpcCidrBlockResponse'
            Prelude.<$> (x Prelude..@? "ipv6CidrBlockAssociation")
            Prelude.<*> (x Prelude..@? "cidrBlockAssociation")
            Prelude.<*> (x Prelude..@? "vpcId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateVpcCidrBlock

instance Prelude.NFData AssociateVpcCidrBlock

instance Prelude.ToHeaders AssociateVpcCidrBlock where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath AssociateVpcCidrBlock where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AssociateVpcCidrBlock where
  toQuery AssociateVpcCidrBlock' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("AssociateVpcCidrBlock" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "Ipv6Pool" Prelude.=: ipv6Pool,
        "Ipv6CidrBlock" Prelude.=: ipv6CidrBlock,
        "Ipv6CidrBlockNetworkBorderGroup"
          Prelude.=: ipv6CidrBlockNetworkBorderGroup,
        "AmazonProvidedIpv6CidrBlock"
          Prelude.=: amazonProvidedIpv6CidrBlock,
        "CidrBlock" Prelude.=: cidrBlock,
        "VpcId" Prelude.=: vpcId
      ]

-- | /See:/ 'newAssociateVpcCidrBlockResponse' smart constructor.
data AssociateVpcCidrBlockResponse = AssociateVpcCidrBlockResponse'
  { -- | Information about the IPv6 CIDR block association.
    ipv6CidrBlockAssociation :: Prelude.Maybe VpcIpv6CidrBlockAssociation,
    -- | Information about the IPv4 CIDR block association.
    cidrBlockAssociation :: Prelude.Maybe VpcCidrBlockAssociation,
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateVpcCidrBlockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6CidrBlockAssociation', 'associateVpcCidrBlockResponse_ipv6CidrBlockAssociation' - Information about the IPv6 CIDR block association.
--
-- 'cidrBlockAssociation', 'associateVpcCidrBlockResponse_cidrBlockAssociation' - Information about the IPv4 CIDR block association.
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
    { ipv6CidrBlockAssociation =
        Prelude.Nothing,
      cidrBlockAssociation = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the IPv6 CIDR block association.
associateVpcCidrBlockResponse_ipv6CidrBlockAssociation :: Lens.Lens' AssociateVpcCidrBlockResponse (Prelude.Maybe VpcIpv6CidrBlockAssociation)
associateVpcCidrBlockResponse_ipv6CidrBlockAssociation = Lens.lens (\AssociateVpcCidrBlockResponse' {ipv6CidrBlockAssociation} -> ipv6CidrBlockAssociation) (\s@AssociateVpcCidrBlockResponse' {} a -> s {ipv6CidrBlockAssociation = a} :: AssociateVpcCidrBlockResponse)

-- | Information about the IPv4 CIDR block association.
associateVpcCidrBlockResponse_cidrBlockAssociation :: Lens.Lens' AssociateVpcCidrBlockResponse (Prelude.Maybe VpcCidrBlockAssociation)
associateVpcCidrBlockResponse_cidrBlockAssociation = Lens.lens (\AssociateVpcCidrBlockResponse' {cidrBlockAssociation} -> cidrBlockAssociation) (\s@AssociateVpcCidrBlockResponse' {} a -> s {cidrBlockAssociation = a} :: AssociateVpcCidrBlockResponse)

-- | The ID of the VPC.
associateVpcCidrBlockResponse_vpcId :: Lens.Lens' AssociateVpcCidrBlockResponse (Prelude.Maybe Prelude.Text)
associateVpcCidrBlockResponse_vpcId = Lens.lens (\AssociateVpcCidrBlockResponse' {vpcId} -> vpcId) (\s@AssociateVpcCidrBlockResponse' {} a -> s {vpcId = a} :: AssociateVpcCidrBlockResponse)

-- | The response's http status code.
associateVpcCidrBlockResponse_httpStatus :: Lens.Lens' AssociateVpcCidrBlockResponse Prelude.Int
associateVpcCidrBlockResponse_httpStatus = Lens.lens (\AssociateVpcCidrBlockResponse' {httpStatus} -> httpStatus) (\s@AssociateVpcCidrBlockResponse' {} a -> s {httpStatus = a} :: AssociateVpcCidrBlockResponse)

instance Prelude.NFData AssociateVpcCidrBlockResponse
