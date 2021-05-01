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
-- Module      : Network.AWS.EC2.DisassociateVpcCidrBlock
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a CIDR block from a VPC. To disassociate the CIDR block,
-- you must specify its association ID. You can get the association ID by
-- using DescribeVpcs. You must detach or delete all gateways and resources
-- that are associated with the CIDR block before you can disassociate it.
--
-- You cannot disassociate the CIDR block with which you originally created
-- the VPC (the primary CIDR block).
module Network.AWS.EC2.DisassociateVpcCidrBlock
  ( -- * Creating a Request
    DisassociateVpcCidrBlock (..),
    newDisassociateVpcCidrBlock,

    -- * Request Lenses
    disassociateVpcCidrBlock_associationId,

    -- * Destructuring the Response
    DisassociateVpcCidrBlockResponse (..),
    newDisassociateVpcCidrBlockResponse,

    -- * Response Lenses
    disassociateVpcCidrBlockResponse_ipv6CidrBlockAssociation,
    disassociateVpcCidrBlockResponse_cidrBlockAssociation,
    disassociateVpcCidrBlockResponse_vpcId,
    disassociateVpcCidrBlockResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateVpcCidrBlock' smart constructor.
data DisassociateVpcCidrBlock = DisassociateVpcCidrBlock'
  { -- | The association ID for the CIDR block.
    associationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateVpcCidrBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationId', 'disassociateVpcCidrBlock_associationId' - The association ID for the CIDR block.
newDisassociateVpcCidrBlock ::
  -- | 'associationId'
  Prelude.Text ->
  DisassociateVpcCidrBlock
newDisassociateVpcCidrBlock pAssociationId_ =
  DisassociateVpcCidrBlock'
    { associationId =
        pAssociationId_
    }

-- | The association ID for the CIDR block.
disassociateVpcCidrBlock_associationId :: Lens.Lens' DisassociateVpcCidrBlock Prelude.Text
disassociateVpcCidrBlock_associationId = Lens.lens (\DisassociateVpcCidrBlock' {associationId} -> associationId) (\s@DisassociateVpcCidrBlock' {} a -> s {associationId = a} :: DisassociateVpcCidrBlock)

instance Prelude.AWSRequest DisassociateVpcCidrBlock where
  type
    Rs DisassociateVpcCidrBlock =
      DisassociateVpcCidrBlockResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DisassociateVpcCidrBlockResponse'
            Prelude.<$> (x Prelude..@? "ipv6CidrBlockAssociation")
            Prelude.<*> (x Prelude..@? "cidrBlockAssociation")
            Prelude.<*> (x Prelude..@? "vpcId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateVpcCidrBlock

instance Prelude.NFData DisassociateVpcCidrBlock

instance Prelude.ToHeaders DisassociateVpcCidrBlock where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DisassociateVpcCidrBlock where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisassociateVpcCidrBlock where
  toQuery DisassociateVpcCidrBlock' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DisassociateVpcCidrBlock" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "AssociationId" Prelude.=: associationId
      ]

-- | /See:/ 'newDisassociateVpcCidrBlockResponse' smart constructor.
data DisassociateVpcCidrBlockResponse = DisassociateVpcCidrBlockResponse'
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
-- Create a value of 'DisassociateVpcCidrBlockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6CidrBlockAssociation', 'disassociateVpcCidrBlockResponse_ipv6CidrBlockAssociation' - Information about the IPv6 CIDR block association.
--
-- 'cidrBlockAssociation', 'disassociateVpcCidrBlockResponse_cidrBlockAssociation' - Information about the IPv4 CIDR block association.
--
-- 'vpcId', 'disassociateVpcCidrBlockResponse_vpcId' - The ID of the VPC.
--
-- 'httpStatus', 'disassociateVpcCidrBlockResponse_httpStatus' - The response's http status code.
newDisassociateVpcCidrBlockResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateVpcCidrBlockResponse
newDisassociateVpcCidrBlockResponse pHttpStatus_ =
  DisassociateVpcCidrBlockResponse'
    { ipv6CidrBlockAssociation =
        Prelude.Nothing,
      cidrBlockAssociation = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the IPv6 CIDR block association.
disassociateVpcCidrBlockResponse_ipv6CidrBlockAssociation :: Lens.Lens' DisassociateVpcCidrBlockResponse (Prelude.Maybe VpcIpv6CidrBlockAssociation)
disassociateVpcCidrBlockResponse_ipv6CidrBlockAssociation = Lens.lens (\DisassociateVpcCidrBlockResponse' {ipv6CidrBlockAssociation} -> ipv6CidrBlockAssociation) (\s@DisassociateVpcCidrBlockResponse' {} a -> s {ipv6CidrBlockAssociation = a} :: DisassociateVpcCidrBlockResponse)

-- | Information about the IPv4 CIDR block association.
disassociateVpcCidrBlockResponse_cidrBlockAssociation :: Lens.Lens' DisassociateVpcCidrBlockResponse (Prelude.Maybe VpcCidrBlockAssociation)
disassociateVpcCidrBlockResponse_cidrBlockAssociation = Lens.lens (\DisassociateVpcCidrBlockResponse' {cidrBlockAssociation} -> cidrBlockAssociation) (\s@DisassociateVpcCidrBlockResponse' {} a -> s {cidrBlockAssociation = a} :: DisassociateVpcCidrBlockResponse)

-- | The ID of the VPC.
disassociateVpcCidrBlockResponse_vpcId :: Lens.Lens' DisassociateVpcCidrBlockResponse (Prelude.Maybe Prelude.Text)
disassociateVpcCidrBlockResponse_vpcId = Lens.lens (\DisassociateVpcCidrBlockResponse' {vpcId} -> vpcId) (\s@DisassociateVpcCidrBlockResponse' {} a -> s {vpcId = a} :: DisassociateVpcCidrBlockResponse)

-- | The response's http status code.
disassociateVpcCidrBlockResponse_httpStatus :: Lens.Lens' DisassociateVpcCidrBlockResponse Prelude.Int
disassociateVpcCidrBlockResponse_httpStatus = Lens.lens (\DisassociateVpcCidrBlockResponse' {httpStatus} -> httpStatus) (\s@DisassociateVpcCidrBlockResponse' {} a -> s {httpStatus = a} :: DisassociateVpcCidrBlockResponse)

instance
  Prelude.NFData
    DisassociateVpcCidrBlockResponse
