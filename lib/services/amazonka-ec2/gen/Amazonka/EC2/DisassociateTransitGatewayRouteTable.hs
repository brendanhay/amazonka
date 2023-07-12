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
-- Module      : Amazonka.EC2.DisassociateTransitGatewayRouteTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a resource attachment from a transit gateway route table.
module Amazonka.EC2.DisassociateTransitGatewayRouteTable
  ( -- * Creating a Request
    DisassociateTransitGatewayRouteTable (..),
    newDisassociateTransitGatewayRouteTable,

    -- * Request Lenses
    disassociateTransitGatewayRouteTable_dryRun,
    disassociateTransitGatewayRouteTable_transitGatewayRouteTableId,
    disassociateTransitGatewayRouteTable_transitGatewayAttachmentId,

    -- * Destructuring the Response
    DisassociateTransitGatewayRouteTableResponse (..),
    newDisassociateTransitGatewayRouteTableResponse,

    -- * Response Lenses
    disassociateTransitGatewayRouteTableResponse_association,
    disassociateTransitGatewayRouteTableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateTransitGatewayRouteTable' smart constructor.
data DisassociateTransitGatewayRouteTable = DisassociateTransitGatewayRouteTable'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Prelude.Text,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateTransitGatewayRouteTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'disassociateTransitGatewayRouteTable_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayRouteTableId', 'disassociateTransitGatewayRouteTable_transitGatewayRouteTableId' - The ID of the transit gateway route table.
--
-- 'transitGatewayAttachmentId', 'disassociateTransitGatewayRouteTable_transitGatewayAttachmentId' - The ID of the attachment.
newDisassociateTransitGatewayRouteTable ::
  -- | 'transitGatewayRouteTableId'
  Prelude.Text ->
  -- | 'transitGatewayAttachmentId'
  Prelude.Text ->
  DisassociateTransitGatewayRouteTable
newDisassociateTransitGatewayRouteTable
  pTransitGatewayRouteTableId_
  pTransitGatewayAttachmentId_ =
    DisassociateTransitGatewayRouteTable'
      { dryRun =
          Prelude.Nothing,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
disassociateTransitGatewayRouteTable_dryRun :: Lens.Lens' DisassociateTransitGatewayRouteTable (Prelude.Maybe Prelude.Bool)
disassociateTransitGatewayRouteTable_dryRun = Lens.lens (\DisassociateTransitGatewayRouteTable' {dryRun} -> dryRun) (\s@DisassociateTransitGatewayRouteTable' {} a -> s {dryRun = a} :: DisassociateTransitGatewayRouteTable)

-- | The ID of the transit gateway route table.
disassociateTransitGatewayRouteTable_transitGatewayRouteTableId :: Lens.Lens' DisassociateTransitGatewayRouteTable Prelude.Text
disassociateTransitGatewayRouteTable_transitGatewayRouteTableId = Lens.lens (\DisassociateTransitGatewayRouteTable' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@DisassociateTransitGatewayRouteTable' {} a -> s {transitGatewayRouteTableId = a} :: DisassociateTransitGatewayRouteTable)

-- | The ID of the attachment.
disassociateTransitGatewayRouteTable_transitGatewayAttachmentId :: Lens.Lens' DisassociateTransitGatewayRouteTable Prelude.Text
disassociateTransitGatewayRouteTable_transitGatewayAttachmentId = Lens.lens (\DisassociateTransitGatewayRouteTable' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@DisassociateTransitGatewayRouteTable' {} a -> s {transitGatewayAttachmentId = a} :: DisassociateTransitGatewayRouteTable)

instance
  Core.AWSRequest
    DisassociateTransitGatewayRouteTable
  where
  type
    AWSResponse DisassociateTransitGatewayRouteTable =
      DisassociateTransitGatewayRouteTableResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DisassociateTransitGatewayRouteTableResponse'
            Prelude.<$> (x Data..@? "association")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateTransitGatewayRouteTable
  where
  hashWithSalt
    _salt
    DisassociateTransitGatewayRouteTable' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` transitGatewayRouteTableId
        `Prelude.hashWithSalt` transitGatewayAttachmentId

instance
  Prelude.NFData
    DisassociateTransitGatewayRouteTable
  where
  rnf DisassociateTransitGatewayRouteTable' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf transitGatewayRouteTableId
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentId

instance
  Data.ToHeaders
    DisassociateTransitGatewayRouteTable
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DisassociateTransitGatewayRouteTable
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DisassociateTransitGatewayRouteTable
  where
  toQuery DisassociateTransitGatewayRouteTable' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DisassociateTransitGatewayRouteTable" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "TransitGatewayRouteTableId"
          Data.=: transitGatewayRouteTableId,
        "TransitGatewayAttachmentId"
          Data.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'newDisassociateTransitGatewayRouteTableResponse' smart constructor.
data DisassociateTransitGatewayRouteTableResponse = DisassociateTransitGatewayRouteTableResponse'
  { -- | Information about the association.
    association :: Prelude.Maybe TransitGatewayAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateTransitGatewayRouteTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'association', 'disassociateTransitGatewayRouteTableResponse_association' - Information about the association.
--
-- 'httpStatus', 'disassociateTransitGatewayRouteTableResponse_httpStatus' - The response's http status code.
newDisassociateTransitGatewayRouteTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateTransitGatewayRouteTableResponse
newDisassociateTransitGatewayRouteTableResponse
  pHttpStatus_ =
    DisassociateTransitGatewayRouteTableResponse'
      { association =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the association.
disassociateTransitGatewayRouteTableResponse_association :: Lens.Lens' DisassociateTransitGatewayRouteTableResponse (Prelude.Maybe TransitGatewayAssociation)
disassociateTransitGatewayRouteTableResponse_association = Lens.lens (\DisassociateTransitGatewayRouteTableResponse' {association} -> association) (\s@DisassociateTransitGatewayRouteTableResponse' {} a -> s {association = a} :: DisassociateTransitGatewayRouteTableResponse)

-- | The response's http status code.
disassociateTransitGatewayRouteTableResponse_httpStatus :: Lens.Lens' DisassociateTransitGatewayRouteTableResponse Prelude.Int
disassociateTransitGatewayRouteTableResponse_httpStatus = Lens.lens (\DisassociateTransitGatewayRouteTableResponse' {httpStatus} -> httpStatus) (\s@DisassociateTransitGatewayRouteTableResponse' {} a -> s {httpStatus = a} :: DisassociateTransitGatewayRouteTableResponse)

instance
  Prelude.NFData
    DisassociateTransitGatewayRouteTableResponse
  where
  rnf DisassociateTransitGatewayRouteTableResponse' {..} =
    Prelude.rnf association
      `Prelude.seq` Prelude.rnf httpStatus
