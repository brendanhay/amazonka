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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateVpcCidrBlock' smart constructor.
data DisassociateVpcCidrBlock = DisassociateVpcCidrBlock'
  { -- | The association ID for the CIDR block.
    associationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DisassociateVpcCidrBlock
newDisassociateVpcCidrBlock pAssociationId_ =
  DisassociateVpcCidrBlock'
    { associationId =
        pAssociationId_
    }

-- | The association ID for the CIDR block.
disassociateVpcCidrBlock_associationId :: Lens.Lens' DisassociateVpcCidrBlock Core.Text
disassociateVpcCidrBlock_associationId = Lens.lens (\DisassociateVpcCidrBlock' {associationId} -> associationId) (\s@DisassociateVpcCidrBlock' {} a -> s {associationId = a} :: DisassociateVpcCidrBlock)

instance Core.AWSRequest DisassociateVpcCidrBlock where
  type
    AWSResponse DisassociateVpcCidrBlock =
      DisassociateVpcCidrBlockResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DisassociateVpcCidrBlockResponse'
            Core.<$> (x Core..@? "ipv6CidrBlockAssociation")
            Core.<*> (x Core..@? "cidrBlockAssociation")
            Core.<*> (x Core..@? "vpcId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DisassociateVpcCidrBlock

instance Core.NFData DisassociateVpcCidrBlock

instance Core.ToHeaders DisassociateVpcCidrBlock where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DisassociateVpcCidrBlock where
  toPath = Core.const "/"

instance Core.ToQuery DisassociateVpcCidrBlock where
  toQuery DisassociateVpcCidrBlock' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DisassociateVpcCidrBlock" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "AssociationId" Core.=: associationId
      ]

-- | /See:/ 'newDisassociateVpcCidrBlockResponse' smart constructor.
data DisassociateVpcCidrBlockResponse = DisassociateVpcCidrBlockResponse'
  { -- | Information about the IPv6 CIDR block association.
    ipv6CidrBlockAssociation :: Core.Maybe VpcIpv6CidrBlockAssociation,
    -- | Information about the IPv4 CIDR block association.
    cidrBlockAssociation :: Core.Maybe VpcCidrBlockAssociation,
    -- | The ID of the VPC.
    vpcId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DisassociateVpcCidrBlockResponse
newDisassociateVpcCidrBlockResponse pHttpStatus_ =
  DisassociateVpcCidrBlockResponse'
    { ipv6CidrBlockAssociation =
        Core.Nothing,
      cidrBlockAssociation = Core.Nothing,
      vpcId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the IPv6 CIDR block association.
disassociateVpcCidrBlockResponse_ipv6CidrBlockAssociation :: Lens.Lens' DisassociateVpcCidrBlockResponse (Core.Maybe VpcIpv6CidrBlockAssociation)
disassociateVpcCidrBlockResponse_ipv6CidrBlockAssociation = Lens.lens (\DisassociateVpcCidrBlockResponse' {ipv6CidrBlockAssociation} -> ipv6CidrBlockAssociation) (\s@DisassociateVpcCidrBlockResponse' {} a -> s {ipv6CidrBlockAssociation = a} :: DisassociateVpcCidrBlockResponse)

-- | Information about the IPv4 CIDR block association.
disassociateVpcCidrBlockResponse_cidrBlockAssociation :: Lens.Lens' DisassociateVpcCidrBlockResponse (Core.Maybe VpcCidrBlockAssociation)
disassociateVpcCidrBlockResponse_cidrBlockAssociation = Lens.lens (\DisassociateVpcCidrBlockResponse' {cidrBlockAssociation} -> cidrBlockAssociation) (\s@DisassociateVpcCidrBlockResponse' {} a -> s {cidrBlockAssociation = a} :: DisassociateVpcCidrBlockResponse)

-- | The ID of the VPC.
disassociateVpcCidrBlockResponse_vpcId :: Lens.Lens' DisassociateVpcCidrBlockResponse (Core.Maybe Core.Text)
disassociateVpcCidrBlockResponse_vpcId = Lens.lens (\DisassociateVpcCidrBlockResponse' {vpcId} -> vpcId) (\s@DisassociateVpcCidrBlockResponse' {} a -> s {vpcId = a} :: DisassociateVpcCidrBlockResponse)

-- | The response's http status code.
disassociateVpcCidrBlockResponse_httpStatus :: Lens.Lens' DisassociateVpcCidrBlockResponse Core.Int
disassociateVpcCidrBlockResponse_httpStatus = Lens.lens (\DisassociateVpcCidrBlockResponse' {httpStatus} -> httpStatus) (\s@DisassociateVpcCidrBlockResponse' {} a -> s {httpStatus = a} :: DisassociateVpcCidrBlockResponse)

instance Core.NFData DisassociateVpcCidrBlockResponse
