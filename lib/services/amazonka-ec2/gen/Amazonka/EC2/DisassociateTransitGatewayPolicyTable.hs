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
-- Module      : Amazonka.EC2.DisassociateTransitGatewayPolicyTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the association between an an attachment and a policy table.
module Amazonka.EC2.DisassociateTransitGatewayPolicyTable
  ( -- * Creating a Request
    DisassociateTransitGatewayPolicyTable (..),
    newDisassociateTransitGatewayPolicyTable,

    -- * Request Lenses
    disassociateTransitGatewayPolicyTable_dryRun,
    disassociateTransitGatewayPolicyTable_transitGatewayPolicyTableId,
    disassociateTransitGatewayPolicyTable_transitGatewayAttachmentId,

    -- * Destructuring the Response
    DisassociateTransitGatewayPolicyTableResponse (..),
    newDisassociateTransitGatewayPolicyTableResponse,

    -- * Response Lenses
    disassociateTransitGatewayPolicyTableResponse_association,
    disassociateTransitGatewayPolicyTableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateTransitGatewayPolicyTable' smart constructor.
data DisassociateTransitGatewayPolicyTable = DisassociateTransitGatewayPolicyTable'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the disassociated policy table.
    transitGatewayPolicyTableId :: Prelude.Text,
    -- | The ID of the transit gateway attachment to disassociate from the policy
    -- table.
    transitGatewayAttachmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateTransitGatewayPolicyTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'disassociateTransitGatewayPolicyTable_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayPolicyTableId', 'disassociateTransitGatewayPolicyTable_transitGatewayPolicyTableId' - The ID of the disassociated policy table.
--
-- 'transitGatewayAttachmentId', 'disassociateTransitGatewayPolicyTable_transitGatewayAttachmentId' - The ID of the transit gateway attachment to disassociate from the policy
-- table.
newDisassociateTransitGatewayPolicyTable ::
  -- | 'transitGatewayPolicyTableId'
  Prelude.Text ->
  -- | 'transitGatewayAttachmentId'
  Prelude.Text ->
  DisassociateTransitGatewayPolicyTable
newDisassociateTransitGatewayPolicyTable
  pTransitGatewayPolicyTableId_
  pTransitGatewayAttachmentId_ =
    DisassociateTransitGatewayPolicyTable'
      { dryRun =
          Prelude.Nothing,
        transitGatewayPolicyTableId =
          pTransitGatewayPolicyTableId_,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
disassociateTransitGatewayPolicyTable_dryRun :: Lens.Lens' DisassociateTransitGatewayPolicyTable (Prelude.Maybe Prelude.Bool)
disassociateTransitGatewayPolicyTable_dryRun = Lens.lens (\DisassociateTransitGatewayPolicyTable' {dryRun} -> dryRun) (\s@DisassociateTransitGatewayPolicyTable' {} a -> s {dryRun = a} :: DisassociateTransitGatewayPolicyTable)

-- | The ID of the disassociated policy table.
disassociateTransitGatewayPolicyTable_transitGatewayPolicyTableId :: Lens.Lens' DisassociateTransitGatewayPolicyTable Prelude.Text
disassociateTransitGatewayPolicyTable_transitGatewayPolicyTableId = Lens.lens (\DisassociateTransitGatewayPolicyTable' {transitGatewayPolicyTableId} -> transitGatewayPolicyTableId) (\s@DisassociateTransitGatewayPolicyTable' {} a -> s {transitGatewayPolicyTableId = a} :: DisassociateTransitGatewayPolicyTable)

-- | The ID of the transit gateway attachment to disassociate from the policy
-- table.
disassociateTransitGatewayPolicyTable_transitGatewayAttachmentId :: Lens.Lens' DisassociateTransitGatewayPolicyTable Prelude.Text
disassociateTransitGatewayPolicyTable_transitGatewayAttachmentId = Lens.lens (\DisassociateTransitGatewayPolicyTable' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@DisassociateTransitGatewayPolicyTable' {} a -> s {transitGatewayAttachmentId = a} :: DisassociateTransitGatewayPolicyTable)

instance
  Core.AWSRequest
    DisassociateTransitGatewayPolicyTable
  where
  type
    AWSResponse
      DisassociateTransitGatewayPolicyTable =
      DisassociateTransitGatewayPolicyTableResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DisassociateTransitGatewayPolicyTableResponse'
            Prelude.<$> (x Data..@? "association")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateTransitGatewayPolicyTable
  where
  hashWithSalt
    _salt
    DisassociateTransitGatewayPolicyTable' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` transitGatewayPolicyTableId
        `Prelude.hashWithSalt` transitGatewayAttachmentId

instance
  Prelude.NFData
    DisassociateTransitGatewayPolicyTable
  where
  rnf DisassociateTransitGatewayPolicyTable' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf transitGatewayPolicyTableId `Prelude.seq`
        Prelude.rnf transitGatewayAttachmentId

instance
  Data.ToHeaders
    DisassociateTransitGatewayPolicyTable
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DisassociateTransitGatewayPolicyTable
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DisassociateTransitGatewayPolicyTable
  where
  toQuery DisassociateTransitGatewayPolicyTable' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DisassociateTransitGatewayPolicyTable" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "TransitGatewayPolicyTableId"
          Data.=: transitGatewayPolicyTableId,
        "TransitGatewayAttachmentId"
          Data.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'newDisassociateTransitGatewayPolicyTableResponse' smart constructor.
data DisassociateTransitGatewayPolicyTableResponse = DisassociateTransitGatewayPolicyTableResponse'
  { -- | Returns details about the transit gateway policy table disassociation.
    association :: Prelude.Maybe TransitGatewayPolicyTableAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateTransitGatewayPolicyTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'association', 'disassociateTransitGatewayPolicyTableResponse_association' - Returns details about the transit gateway policy table disassociation.
--
-- 'httpStatus', 'disassociateTransitGatewayPolicyTableResponse_httpStatus' - The response's http status code.
newDisassociateTransitGatewayPolicyTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateTransitGatewayPolicyTableResponse
newDisassociateTransitGatewayPolicyTableResponse
  pHttpStatus_ =
    DisassociateTransitGatewayPolicyTableResponse'
      { association =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns details about the transit gateway policy table disassociation.
disassociateTransitGatewayPolicyTableResponse_association :: Lens.Lens' DisassociateTransitGatewayPolicyTableResponse (Prelude.Maybe TransitGatewayPolicyTableAssociation)
disassociateTransitGatewayPolicyTableResponse_association = Lens.lens (\DisassociateTransitGatewayPolicyTableResponse' {association} -> association) (\s@DisassociateTransitGatewayPolicyTableResponse' {} a -> s {association = a} :: DisassociateTransitGatewayPolicyTableResponse)

-- | The response's http status code.
disassociateTransitGatewayPolicyTableResponse_httpStatus :: Lens.Lens' DisassociateTransitGatewayPolicyTableResponse Prelude.Int
disassociateTransitGatewayPolicyTableResponse_httpStatus = Lens.lens (\DisassociateTransitGatewayPolicyTableResponse' {httpStatus} -> httpStatus) (\s@DisassociateTransitGatewayPolicyTableResponse' {} a -> s {httpStatus = a} :: DisassociateTransitGatewayPolicyTableResponse)

instance
  Prelude.NFData
    DisassociateTransitGatewayPolicyTableResponse
  where
  rnf
    DisassociateTransitGatewayPolicyTableResponse' {..} =
      Prelude.rnf association `Prelude.seq`
        Prelude.rnf httpStatus
