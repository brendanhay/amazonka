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
-- Module      : Amazonka.EC2.DisassociateSubnetCidrBlock
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a CIDR block from a subnet. Currently, you can
-- disassociate an IPv6 CIDR block only. You must detach or delete all
-- gateways and resources that are associated with the CIDR block before
-- you can disassociate it.
module Amazonka.EC2.DisassociateSubnetCidrBlock
  ( -- * Creating a Request
    DisassociateSubnetCidrBlock (..),
    newDisassociateSubnetCidrBlock,

    -- * Request Lenses
    disassociateSubnetCidrBlock_associationId,

    -- * Destructuring the Response
    DisassociateSubnetCidrBlockResponse (..),
    newDisassociateSubnetCidrBlockResponse,

    -- * Response Lenses
    disassociateSubnetCidrBlockResponse_ipv6CidrBlockAssociation,
    disassociateSubnetCidrBlockResponse_subnetId,
    disassociateSubnetCidrBlockResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateSubnetCidrBlock' smart constructor.
data DisassociateSubnetCidrBlock = DisassociateSubnetCidrBlock'
  { -- | The association ID for the CIDR block.
    associationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateSubnetCidrBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationId', 'disassociateSubnetCidrBlock_associationId' - The association ID for the CIDR block.
newDisassociateSubnetCidrBlock ::
  -- | 'associationId'
  Prelude.Text ->
  DisassociateSubnetCidrBlock
newDisassociateSubnetCidrBlock pAssociationId_ =
  DisassociateSubnetCidrBlock'
    { associationId =
        pAssociationId_
    }

-- | The association ID for the CIDR block.
disassociateSubnetCidrBlock_associationId :: Lens.Lens' DisassociateSubnetCidrBlock Prelude.Text
disassociateSubnetCidrBlock_associationId = Lens.lens (\DisassociateSubnetCidrBlock' {associationId} -> associationId) (\s@DisassociateSubnetCidrBlock' {} a -> s {associationId = a} :: DisassociateSubnetCidrBlock)

instance Core.AWSRequest DisassociateSubnetCidrBlock where
  type
    AWSResponse DisassociateSubnetCidrBlock =
      DisassociateSubnetCidrBlockResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DisassociateSubnetCidrBlockResponse'
            Prelude.<$> (x Data..@? "ipv6CidrBlockAssociation")
            Prelude.<*> (x Data..@? "subnetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateSubnetCidrBlock where
  hashWithSalt _salt DisassociateSubnetCidrBlock' {..} =
    _salt `Prelude.hashWithSalt` associationId

instance Prelude.NFData DisassociateSubnetCidrBlock where
  rnf DisassociateSubnetCidrBlock' {..} =
    Prelude.rnf associationId

instance Data.ToHeaders DisassociateSubnetCidrBlock where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DisassociateSubnetCidrBlock where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateSubnetCidrBlock where
  toQuery DisassociateSubnetCidrBlock' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DisassociateSubnetCidrBlock" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "AssociationId" Data.=: associationId
      ]

-- | /See:/ 'newDisassociateSubnetCidrBlockResponse' smart constructor.
data DisassociateSubnetCidrBlockResponse = DisassociateSubnetCidrBlockResponse'
  { -- | Information about the IPv6 CIDR block association.
    ipv6CidrBlockAssociation :: Prelude.Maybe SubnetIpv6CidrBlockAssociation,
    -- | The ID of the subnet.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateSubnetCidrBlockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6CidrBlockAssociation', 'disassociateSubnetCidrBlockResponse_ipv6CidrBlockAssociation' - Information about the IPv6 CIDR block association.
--
-- 'subnetId', 'disassociateSubnetCidrBlockResponse_subnetId' - The ID of the subnet.
--
-- 'httpStatus', 'disassociateSubnetCidrBlockResponse_httpStatus' - The response's http status code.
newDisassociateSubnetCidrBlockResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateSubnetCidrBlockResponse
newDisassociateSubnetCidrBlockResponse pHttpStatus_ =
  DisassociateSubnetCidrBlockResponse'
    { ipv6CidrBlockAssociation =
        Prelude.Nothing,
      subnetId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the IPv6 CIDR block association.
disassociateSubnetCidrBlockResponse_ipv6CidrBlockAssociation :: Lens.Lens' DisassociateSubnetCidrBlockResponse (Prelude.Maybe SubnetIpv6CidrBlockAssociation)
disassociateSubnetCidrBlockResponse_ipv6CidrBlockAssociation = Lens.lens (\DisassociateSubnetCidrBlockResponse' {ipv6CidrBlockAssociation} -> ipv6CidrBlockAssociation) (\s@DisassociateSubnetCidrBlockResponse' {} a -> s {ipv6CidrBlockAssociation = a} :: DisassociateSubnetCidrBlockResponse)

-- | The ID of the subnet.
disassociateSubnetCidrBlockResponse_subnetId :: Lens.Lens' DisassociateSubnetCidrBlockResponse (Prelude.Maybe Prelude.Text)
disassociateSubnetCidrBlockResponse_subnetId = Lens.lens (\DisassociateSubnetCidrBlockResponse' {subnetId} -> subnetId) (\s@DisassociateSubnetCidrBlockResponse' {} a -> s {subnetId = a} :: DisassociateSubnetCidrBlockResponse)

-- | The response's http status code.
disassociateSubnetCidrBlockResponse_httpStatus :: Lens.Lens' DisassociateSubnetCidrBlockResponse Prelude.Int
disassociateSubnetCidrBlockResponse_httpStatus = Lens.lens (\DisassociateSubnetCidrBlockResponse' {httpStatus} -> httpStatus) (\s@DisassociateSubnetCidrBlockResponse' {} a -> s {httpStatus = a} :: DisassociateSubnetCidrBlockResponse)

instance
  Prelude.NFData
    DisassociateSubnetCidrBlockResponse
  where
  rnf DisassociateSubnetCidrBlockResponse' {..} =
    Prelude.rnf ipv6CidrBlockAssociation
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf httpStatus
