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
-- Module      : Amazonka.EC2.AssociateTransitGatewayPolicyTable
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified transit gateway attachment with a transit
-- gateway policy table.
module Amazonka.EC2.AssociateTransitGatewayPolicyTable
  ( -- * Creating a Request
    AssociateTransitGatewayPolicyTable (..),
    newAssociateTransitGatewayPolicyTable,

    -- * Request Lenses
    associateTransitGatewayPolicyTable_dryRun,
    associateTransitGatewayPolicyTable_transitGatewayPolicyTableId,
    associateTransitGatewayPolicyTable_transitGatewayAttachmentId,

    -- * Destructuring the Response
    AssociateTransitGatewayPolicyTableResponse (..),
    newAssociateTransitGatewayPolicyTableResponse,

    -- * Response Lenses
    associateTransitGatewayPolicyTableResponse_association,
    associateTransitGatewayPolicyTableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateTransitGatewayPolicyTable' smart constructor.
data AssociateTransitGatewayPolicyTable = AssociateTransitGatewayPolicyTable'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the transit gateway policy table to associate with the transit
    -- gateway attachment.
    transitGatewayPolicyTableId :: Prelude.Text,
    -- | The ID of the transit gateway attachment to associate with the policy
    -- table.
    transitGatewayAttachmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateTransitGatewayPolicyTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'associateTransitGatewayPolicyTable_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayPolicyTableId', 'associateTransitGatewayPolicyTable_transitGatewayPolicyTableId' - The ID of the transit gateway policy table to associate with the transit
-- gateway attachment.
--
-- 'transitGatewayAttachmentId', 'associateTransitGatewayPolicyTable_transitGatewayAttachmentId' - The ID of the transit gateway attachment to associate with the policy
-- table.
newAssociateTransitGatewayPolicyTable ::
  -- | 'transitGatewayPolicyTableId'
  Prelude.Text ->
  -- | 'transitGatewayAttachmentId'
  Prelude.Text ->
  AssociateTransitGatewayPolicyTable
newAssociateTransitGatewayPolicyTable
  pTransitGatewayPolicyTableId_
  pTransitGatewayAttachmentId_ =
    AssociateTransitGatewayPolicyTable'
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
associateTransitGatewayPolicyTable_dryRun :: Lens.Lens' AssociateTransitGatewayPolicyTable (Prelude.Maybe Prelude.Bool)
associateTransitGatewayPolicyTable_dryRun = Lens.lens (\AssociateTransitGatewayPolicyTable' {dryRun} -> dryRun) (\s@AssociateTransitGatewayPolicyTable' {} a -> s {dryRun = a} :: AssociateTransitGatewayPolicyTable)

-- | The ID of the transit gateway policy table to associate with the transit
-- gateway attachment.
associateTransitGatewayPolicyTable_transitGatewayPolicyTableId :: Lens.Lens' AssociateTransitGatewayPolicyTable Prelude.Text
associateTransitGatewayPolicyTable_transitGatewayPolicyTableId = Lens.lens (\AssociateTransitGatewayPolicyTable' {transitGatewayPolicyTableId} -> transitGatewayPolicyTableId) (\s@AssociateTransitGatewayPolicyTable' {} a -> s {transitGatewayPolicyTableId = a} :: AssociateTransitGatewayPolicyTable)

-- | The ID of the transit gateway attachment to associate with the policy
-- table.
associateTransitGatewayPolicyTable_transitGatewayAttachmentId :: Lens.Lens' AssociateTransitGatewayPolicyTable Prelude.Text
associateTransitGatewayPolicyTable_transitGatewayAttachmentId = Lens.lens (\AssociateTransitGatewayPolicyTable' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@AssociateTransitGatewayPolicyTable' {} a -> s {transitGatewayAttachmentId = a} :: AssociateTransitGatewayPolicyTable)

instance
  Core.AWSRequest
    AssociateTransitGatewayPolicyTable
  where
  type
    AWSResponse AssociateTransitGatewayPolicyTable =
      AssociateTransitGatewayPolicyTableResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateTransitGatewayPolicyTableResponse'
            Prelude.<$> (x Core..@? "association")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateTransitGatewayPolicyTable
  where
  hashWithSalt
    _salt
    AssociateTransitGatewayPolicyTable' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` transitGatewayPolicyTableId
        `Prelude.hashWithSalt` transitGatewayAttachmentId

instance
  Prelude.NFData
    AssociateTransitGatewayPolicyTable
  where
  rnf AssociateTransitGatewayPolicyTable' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf transitGatewayPolicyTableId
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentId

instance
  Core.ToHeaders
    AssociateTransitGatewayPolicyTable
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    AssociateTransitGatewayPolicyTable
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    AssociateTransitGatewayPolicyTable
  where
  toQuery AssociateTransitGatewayPolicyTable' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "AssociateTransitGatewayPolicyTable" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "TransitGatewayPolicyTableId"
          Core.=: transitGatewayPolicyTableId,
        "TransitGatewayAttachmentId"
          Core.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'newAssociateTransitGatewayPolicyTableResponse' smart constructor.
data AssociateTransitGatewayPolicyTableResponse = AssociateTransitGatewayPolicyTableResponse'
  { -- | Describes the association of a transit gateway and a transit gateway
    -- policy table.
    association :: Prelude.Maybe TransitGatewayPolicyTableAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateTransitGatewayPolicyTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'association', 'associateTransitGatewayPolicyTableResponse_association' - Describes the association of a transit gateway and a transit gateway
-- policy table.
--
-- 'httpStatus', 'associateTransitGatewayPolicyTableResponse_httpStatus' - The response's http status code.
newAssociateTransitGatewayPolicyTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateTransitGatewayPolicyTableResponse
newAssociateTransitGatewayPolicyTableResponse
  pHttpStatus_ =
    AssociateTransitGatewayPolicyTableResponse'
      { association =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Describes the association of a transit gateway and a transit gateway
-- policy table.
associateTransitGatewayPolicyTableResponse_association :: Lens.Lens' AssociateTransitGatewayPolicyTableResponse (Prelude.Maybe TransitGatewayPolicyTableAssociation)
associateTransitGatewayPolicyTableResponse_association = Lens.lens (\AssociateTransitGatewayPolicyTableResponse' {association} -> association) (\s@AssociateTransitGatewayPolicyTableResponse' {} a -> s {association = a} :: AssociateTransitGatewayPolicyTableResponse)

-- | The response's http status code.
associateTransitGatewayPolicyTableResponse_httpStatus :: Lens.Lens' AssociateTransitGatewayPolicyTableResponse Prelude.Int
associateTransitGatewayPolicyTableResponse_httpStatus = Lens.lens (\AssociateTransitGatewayPolicyTableResponse' {httpStatus} -> httpStatus) (\s@AssociateTransitGatewayPolicyTableResponse' {} a -> s {httpStatus = a} :: AssociateTransitGatewayPolicyTableResponse)

instance
  Prelude.NFData
    AssociateTransitGatewayPolicyTableResponse
  where
  rnf AssociateTransitGatewayPolicyTableResponse' {..} =
    Prelude.rnf association
      `Prelude.seq` Prelude.rnf httpStatus
