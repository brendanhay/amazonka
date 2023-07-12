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
-- Module      : Amazonka.EC2.DisassociateVpcCidrBlock
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.EC2.DisassociateVpcCidrBlock
  ( -- * Creating a Request
    DisassociateVpcCidrBlock (..),
    newDisassociateVpcCidrBlock,

    -- * Request Lenses
    disassociateVpcCidrBlock_associationId,

    -- * Destructuring the Response
    DisassociateVpcCidrBlockResponse (..),
    newDisassociateVpcCidrBlockResponse,

    -- * Response Lenses
    disassociateVpcCidrBlockResponse_cidrBlockAssociation,
    disassociateVpcCidrBlockResponse_ipv6CidrBlockAssociation,
    disassociateVpcCidrBlockResponse_vpcId,
    disassociateVpcCidrBlockResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateVpcCidrBlock' smart constructor.
data DisassociateVpcCidrBlock = DisassociateVpcCidrBlock'
  { -- | The association ID for the CIDR block.
    associationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DisassociateVpcCidrBlock where
  type
    AWSResponse DisassociateVpcCidrBlock =
      DisassociateVpcCidrBlockResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DisassociateVpcCidrBlockResponse'
            Prelude.<$> (x Data..@? "cidrBlockAssociation")
            Prelude.<*> (x Data..@? "ipv6CidrBlockAssociation")
            Prelude.<*> (x Data..@? "vpcId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateVpcCidrBlock where
  hashWithSalt _salt DisassociateVpcCidrBlock' {..} =
    _salt `Prelude.hashWithSalt` associationId

instance Prelude.NFData DisassociateVpcCidrBlock where
  rnf DisassociateVpcCidrBlock' {..} =
    Prelude.rnf associationId

instance Data.ToHeaders DisassociateVpcCidrBlock where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DisassociateVpcCidrBlock where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateVpcCidrBlock where
  toQuery DisassociateVpcCidrBlock' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DisassociateVpcCidrBlock" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "AssociationId" Data.=: associationId
      ]

-- | /See:/ 'newDisassociateVpcCidrBlockResponse' smart constructor.
data DisassociateVpcCidrBlockResponse = DisassociateVpcCidrBlockResponse'
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
-- Create a value of 'DisassociateVpcCidrBlockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrBlockAssociation', 'disassociateVpcCidrBlockResponse_cidrBlockAssociation' - Information about the IPv4 CIDR block association.
--
-- 'ipv6CidrBlockAssociation', 'disassociateVpcCidrBlockResponse_ipv6CidrBlockAssociation' - Information about the IPv6 CIDR block association.
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
    { cidrBlockAssociation =
        Prelude.Nothing,
      ipv6CidrBlockAssociation =
        Prelude.Nothing,
      vpcId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the IPv4 CIDR block association.
disassociateVpcCidrBlockResponse_cidrBlockAssociation :: Lens.Lens' DisassociateVpcCidrBlockResponse (Prelude.Maybe VpcCidrBlockAssociation)
disassociateVpcCidrBlockResponse_cidrBlockAssociation = Lens.lens (\DisassociateVpcCidrBlockResponse' {cidrBlockAssociation} -> cidrBlockAssociation) (\s@DisassociateVpcCidrBlockResponse' {} a -> s {cidrBlockAssociation = a} :: DisassociateVpcCidrBlockResponse)

-- | Information about the IPv6 CIDR block association.
disassociateVpcCidrBlockResponse_ipv6CidrBlockAssociation :: Lens.Lens' DisassociateVpcCidrBlockResponse (Prelude.Maybe VpcIpv6CidrBlockAssociation)
disassociateVpcCidrBlockResponse_ipv6CidrBlockAssociation = Lens.lens (\DisassociateVpcCidrBlockResponse' {ipv6CidrBlockAssociation} -> ipv6CidrBlockAssociation) (\s@DisassociateVpcCidrBlockResponse' {} a -> s {ipv6CidrBlockAssociation = a} :: DisassociateVpcCidrBlockResponse)

-- | The ID of the VPC.
disassociateVpcCidrBlockResponse_vpcId :: Lens.Lens' DisassociateVpcCidrBlockResponse (Prelude.Maybe Prelude.Text)
disassociateVpcCidrBlockResponse_vpcId = Lens.lens (\DisassociateVpcCidrBlockResponse' {vpcId} -> vpcId) (\s@DisassociateVpcCidrBlockResponse' {} a -> s {vpcId = a} :: DisassociateVpcCidrBlockResponse)

-- | The response's http status code.
disassociateVpcCidrBlockResponse_httpStatus :: Lens.Lens' DisassociateVpcCidrBlockResponse Prelude.Int
disassociateVpcCidrBlockResponse_httpStatus = Lens.lens (\DisassociateVpcCidrBlockResponse' {httpStatus} -> httpStatus) (\s@DisassociateVpcCidrBlockResponse' {} a -> s {httpStatus = a} :: DisassociateVpcCidrBlockResponse)

instance
  Prelude.NFData
    DisassociateVpcCidrBlockResponse
  where
  rnf DisassociateVpcCidrBlockResponse' {..} =
    Prelude.rnf cidrBlockAssociation
      `Prelude.seq` Prelude.rnf ipv6CidrBlockAssociation
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf httpStatus
