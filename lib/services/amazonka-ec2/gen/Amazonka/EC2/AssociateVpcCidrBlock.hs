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
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html#VPC_Sizing VPC and subnet sizing>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Amazonka.EC2.AssociateVpcCidrBlock
  ( -- * Creating a Request
    AssociateVpcCidrBlock (..),
    newAssociateVpcCidrBlock,

    -- * Request Lenses
    associateVpcCidrBlock_ipv6CidrBlock,
    associateVpcCidrBlock_ipv6CidrBlockNetworkBorderGroup,
    associateVpcCidrBlock_cidrBlock,
    associateVpcCidrBlock_ipv6Pool,
    associateVpcCidrBlock_amazonProvidedIpv6CidrBlock,
    associateVpcCidrBlock_vpcId,

    -- * Destructuring the Response
    AssociateVpcCidrBlockResponse (..),
    newAssociateVpcCidrBlockResponse,

    -- * Response Lenses
    associateVpcCidrBlockResponse_vpcId,
    associateVpcCidrBlockResponse_cidrBlockAssociation,
    associateVpcCidrBlockResponse_ipv6CidrBlockAssociation,
    associateVpcCidrBlockResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateVpcCidrBlock' smart constructor.
data AssociateVpcCidrBlock = AssociateVpcCidrBlock'
  { -- | An IPv6 CIDR block from the IPv6 address pool. You must also specify
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
    -- | An IPv4 CIDR block to associate with the VPC.
    cidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The ID of an IPv6 address pool from which to allocate the IPv6 CIDR
    -- block.
    ipv6Pool :: Prelude.Maybe Prelude.Text,
    -- | Requests an Amazon-provided IPv6 CIDR block with a \/56 prefix length
    -- for the VPC. You cannot specify the range of IPv6 addresses, or the size
    -- of the CIDR block.
    amazonProvidedIpv6CidrBlock :: Prelude.Maybe Prelude.Bool,
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
-- 'cidrBlock', 'associateVpcCidrBlock_cidrBlock' - An IPv4 CIDR block to associate with the VPC.
--
-- 'ipv6Pool', 'associateVpcCidrBlock_ipv6Pool' - The ID of an IPv6 address pool from which to allocate the IPv6 CIDR
-- block.
--
-- 'amazonProvidedIpv6CidrBlock', 'associateVpcCidrBlock_amazonProvidedIpv6CidrBlock' - Requests an Amazon-provided IPv6 CIDR block with a \/56 prefix length
-- for the VPC. You cannot specify the range of IPv6 addresses, or the size
-- of the CIDR block.
--
-- 'vpcId', 'associateVpcCidrBlock_vpcId' - The ID of the VPC.
newAssociateVpcCidrBlock ::
  -- | 'vpcId'
  Prelude.Text ->
  AssociateVpcCidrBlock
newAssociateVpcCidrBlock pVpcId_ =
  AssociateVpcCidrBlock'
    { ipv6CidrBlock =
        Prelude.Nothing,
      ipv6CidrBlockNetworkBorderGroup = Prelude.Nothing,
      cidrBlock = Prelude.Nothing,
      ipv6Pool = Prelude.Nothing,
      amazonProvidedIpv6CidrBlock = Prelude.Nothing,
      vpcId = pVpcId_
    }

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

-- | An IPv4 CIDR block to associate with the VPC.
associateVpcCidrBlock_cidrBlock :: Lens.Lens' AssociateVpcCidrBlock (Prelude.Maybe Prelude.Text)
associateVpcCidrBlock_cidrBlock = Lens.lens (\AssociateVpcCidrBlock' {cidrBlock} -> cidrBlock) (\s@AssociateVpcCidrBlock' {} a -> s {cidrBlock = a} :: AssociateVpcCidrBlock)

-- | The ID of an IPv6 address pool from which to allocate the IPv6 CIDR
-- block.
associateVpcCidrBlock_ipv6Pool :: Lens.Lens' AssociateVpcCidrBlock (Prelude.Maybe Prelude.Text)
associateVpcCidrBlock_ipv6Pool = Lens.lens (\AssociateVpcCidrBlock' {ipv6Pool} -> ipv6Pool) (\s@AssociateVpcCidrBlock' {} a -> s {ipv6Pool = a} :: AssociateVpcCidrBlock)

-- | Requests an Amazon-provided IPv6 CIDR block with a \/56 prefix length
-- for the VPC. You cannot specify the range of IPv6 addresses, or the size
-- of the CIDR block.
associateVpcCidrBlock_amazonProvidedIpv6CidrBlock :: Lens.Lens' AssociateVpcCidrBlock (Prelude.Maybe Prelude.Bool)
associateVpcCidrBlock_amazonProvidedIpv6CidrBlock = Lens.lens (\AssociateVpcCidrBlock' {amazonProvidedIpv6CidrBlock} -> amazonProvidedIpv6CidrBlock) (\s@AssociateVpcCidrBlock' {} a -> s {amazonProvidedIpv6CidrBlock = a} :: AssociateVpcCidrBlock)

-- | The ID of the VPC.
associateVpcCidrBlock_vpcId :: Lens.Lens' AssociateVpcCidrBlock Prelude.Text
associateVpcCidrBlock_vpcId = Lens.lens (\AssociateVpcCidrBlock' {vpcId} -> vpcId) (\s@AssociateVpcCidrBlock' {} a -> s {vpcId = a} :: AssociateVpcCidrBlock)

instance Core.AWSRequest AssociateVpcCidrBlock where
  type
    AWSResponse AssociateVpcCidrBlock =
      AssociateVpcCidrBlockResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateVpcCidrBlockResponse'
            Prelude.<$> (x Core..@? "vpcId")
            Prelude.<*> (x Core..@? "cidrBlockAssociation")
            Prelude.<*> (x Core..@? "ipv6CidrBlockAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateVpcCidrBlock where
  hashWithSalt salt' AssociateVpcCidrBlock' {..} =
    salt' `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` amazonProvidedIpv6CidrBlock
      `Prelude.hashWithSalt` ipv6Pool
      `Prelude.hashWithSalt` cidrBlock
      `Prelude.hashWithSalt` ipv6CidrBlockNetworkBorderGroup
      `Prelude.hashWithSalt` ipv6CidrBlock

instance Prelude.NFData AssociateVpcCidrBlock where
  rnf AssociateVpcCidrBlock' {..} =
    Prelude.rnf ipv6CidrBlock
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf amazonProvidedIpv6CidrBlock
      `Prelude.seq` Prelude.rnf ipv6Pool
      `Prelude.seq` Prelude.rnf cidrBlock
      `Prelude.seq` Prelude.rnf ipv6CidrBlockNetworkBorderGroup

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
        "Ipv6CidrBlock" Core.=: ipv6CidrBlock,
        "Ipv6CidrBlockNetworkBorderGroup"
          Core.=: ipv6CidrBlockNetworkBorderGroup,
        "CidrBlock" Core.=: cidrBlock,
        "Ipv6Pool" Core.=: ipv6Pool,
        "AmazonProvidedIpv6CidrBlock"
          Core.=: amazonProvidedIpv6CidrBlock,
        "VpcId" Core.=: vpcId
      ]

-- | /See:/ 'newAssociateVpcCidrBlockResponse' smart constructor.
data AssociateVpcCidrBlockResponse = AssociateVpcCidrBlockResponse'
  { -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | Information about the IPv4 CIDR block association.
    cidrBlockAssociation :: Prelude.Maybe VpcCidrBlockAssociation,
    -- | Information about the IPv6 CIDR block association.
    ipv6CidrBlockAssociation :: Prelude.Maybe VpcIpv6CidrBlockAssociation,
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
-- 'vpcId', 'associateVpcCidrBlockResponse_vpcId' - The ID of the VPC.
--
-- 'cidrBlockAssociation', 'associateVpcCidrBlockResponse_cidrBlockAssociation' - Information about the IPv4 CIDR block association.
--
-- 'ipv6CidrBlockAssociation', 'associateVpcCidrBlockResponse_ipv6CidrBlockAssociation' - Information about the IPv6 CIDR block association.
--
-- 'httpStatus', 'associateVpcCidrBlockResponse_httpStatus' - The response's http status code.
newAssociateVpcCidrBlockResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateVpcCidrBlockResponse
newAssociateVpcCidrBlockResponse pHttpStatus_ =
  AssociateVpcCidrBlockResponse'
    { vpcId =
        Prelude.Nothing,
      cidrBlockAssociation = Prelude.Nothing,
      ipv6CidrBlockAssociation = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the VPC.
associateVpcCidrBlockResponse_vpcId :: Lens.Lens' AssociateVpcCidrBlockResponse (Prelude.Maybe Prelude.Text)
associateVpcCidrBlockResponse_vpcId = Lens.lens (\AssociateVpcCidrBlockResponse' {vpcId} -> vpcId) (\s@AssociateVpcCidrBlockResponse' {} a -> s {vpcId = a} :: AssociateVpcCidrBlockResponse)

-- | Information about the IPv4 CIDR block association.
associateVpcCidrBlockResponse_cidrBlockAssociation :: Lens.Lens' AssociateVpcCidrBlockResponse (Prelude.Maybe VpcCidrBlockAssociation)
associateVpcCidrBlockResponse_cidrBlockAssociation = Lens.lens (\AssociateVpcCidrBlockResponse' {cidrBlockAssociation} -> cidrBlockAssociation) (\s@AssociateVpcCidrBlockResponse' {} a -> s {cidrBlockAssociation = a} :: AssociateVpcCidrBlockResponse)

-- | Information about the IPv6 CIDR block association.
associateVpcCidrBlockResponse_ipv6CidrBlockAssociation :: Lens.Lens' AssociateVpcCidrBlockResponse (Prelude.Maybe VpcIpv6CidrBlockAssociation)
associateVpcCidrBlockResponse_ipv6CidrBlockAssociation = Lens.lens (\AssociateVpcCidrBlockResponse' {ipv6CidrBlockAssociation} -> ipv6CidrBlockAssociation) (\s@AssociateVpcCidrBlockResponse' {} a -> s {ipv6CidrBlockAssociation = a} :: AssociateVpcCidrBlockResponse)

-- | The response's http status code.
associateVpcCidrBlockResponse_httpStatus :: Lens.Lens' AssociateVpcCidrBlockResponse Prelude.Int
associateVpcCidrBlockResponse_httpStatus = Lens.lens (\AssociateVpcCidrBlockResponse' {httpStatus} -> httpStatus) (\s@AssociateVpcCidrBlockResponse' {} a -> s {httpStatus = a} :: AssociateVpcCidrBlockResponse)

instance Prelude.NFData AssociateVpcCidrBlockResponse where
  rnf AssociateVpcCidrBlockResponse' {..} =
    Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf ipv6CidrBlockAssociation
      `Prelude.seq` Prelude.rnf cidrBlockAssociation
