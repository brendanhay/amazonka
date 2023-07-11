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
-- Module      : Amazonka.EC2.AssociateTransitGatewayRouteTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified attachment with the specified transit gateway
-- route table. You can associate only one route table with an attachment.
module Amazonka.EC2.AssociateTransitGatewayRouteTable
  ( -- * Creating a Request
    AssociateTransitGatewayRouteTable (..),
    newAssociateTransitGatewayRouteTable,

    -- * Request Lenses
    associateTransitGatewayRouteTable_dryRun,
    associateTransitGatewayRouteTable_transitGatewayRouteTableId,
    associateTransitGatewayRouteTable_transitGatewayAttachmentId,

    -- * Destructuring the Response
    AssociateTransitGatewayRouteTableResponse (..),
    newAssociateTransitGatewayRouteTableResponse,

    -- * Response Lenses
    associateTransitGatewayRouteTableResponse_association,
    associateTransitGatewayRouteTableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateTransitGatewayRouteTable' smart constructor.
data AssociateTransitGatewayRouteTable = AssociateTransitGatewayRouteTable'
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
-- Create a value of 'AssociateTransitGatewayRouteTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'associateTransitGatewayRouteTable_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayRouteTableId', 'associateTransitGatewayRouteTable_transitGatewayRouteTableId' - The ID of the transit gateway route table.
--
-- 'transitGatewayAttachmentId', 'associateTransitGatewayRouteTable_transitGatewayAttachmentId' - The ID of the attachment.
newAssociateTransitGatewayRouteTable ::
  -- | 'transitGatewayRouteTableId'
  Prelude.Text ->
  -- | 'transitGatewayAttachmentId'
  Prelude.Text ->
  AssociateTransitGatewayRouteTable
newAssociateTransitGatewayRouteTable
  pTransitGatewayRouteTableId_
  pTransitGatewayAttachmentId_ =
    AssociateTransitGatewayRouteTable'
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
associateTransitGatewayRouteTable_dryRun :: Lens.Lens' AssociateTransitGatewayRouteTable (Prelude.Maybe Prelude.Bool)
associateTransitGatewayRouteTable_dryRun = Lens.lens (\AssociateTransitGatewayRouteTable' {dryRun} -> dryRun) (\s@AssociateTransitGatewayRouteTable' {} a -> s {dryRun = a} :: AssociateTransitGatewayRouteTable)

-- | The ID of the transit gateway route table.
associateTransitGatewayRouteTable_transitGatewayRouteTableId :: Lens.Lens' AssociateTransitGatewayRouteTable Prelude.Text
associateTransitGatewayRouteTable_transitGatewayRouteTableId = Lens.lens (\AssociateTransitGatewayRouteTable' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@AssociateTransitGatewayRouteTable' {} a -> s {transitGatewayRouteTableId = a} :: AssociateTransitGatewayRouteTable)

-- | The ID of the attachment.
associateTransitGatewayRouteTable_transitGatewayAttachmentId :: Lens.Lens' AssociateTransitGatewayRouteTable Prelude.Text
associateTransitGatewayRouteTable_transitGatewayAttachmentId = Lens.lens (\AssociateTransitGatewayRouteTable' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@AssociateTransitGatewayRouteTable' {} a -> s {transitGatewayAttachmentId = a} :: AssociateTransitGatewayRouteTable)

instance
  Core.AWSRequest
    AssociateTransitGatewayRouteTable
  where
  type
    AWSResponse AssociateTransitGatewayRouteTable =
      AssociateTransitGatewayRouteTableResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateTransitGatewayRouteTableResponse'
            Prelude.<$> (x Data..@? "association")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateTransitGatewayRouteTable
  where
  hashWithSalt
    _salt
    AssociateTransitGatewayRouteTable' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` transitGatewayRouteTableId
        `Prelude.hashWithSalt` transitGatewayAttachmentId

instance
  Prelude.NFData
    AssociateTransitGatewayRouteTable
  where
  rnf AssociateTransitGatewayRouteTable' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf transitGatewayRouteTableId
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentId

instance
  Data.ToHeaders
    AssociateTransitGatewayRouteTable
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    AssociateTransitGatewayRouteTable
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    AssociateTransitGatewayRouteTable
  where
  toQuery AssociateTransitGatewayRouteTable' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "AssociateTransitGatewayRouteTable" ::
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

-- | /See:/ 'newAssociateTransitGatewayRouteTableResponse' smart constructor.
data AssociateTransitGatewayRouteTableResponse = AssociateTransitGatewayRouteTableResponse'
  { -- | The ID of the association.
    association :: Prelude.Maybe TransitGatewayAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateTransitGatewayRouteTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'association', 'associateTransitGatewayRouteTableResponse_association' - The ID of the association.
--
-- 'httpStatus', 'associateTransitGatewayRouteTableResponse_httpStatus' - The response's http status code.
newAssociateTransitGatewayRouteTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateTransitGatewayRouteTableResponse
newAssociateTransitGatewayRouteTableResponse
  pHttpStatus_ =
    AssociateTransitGatewayRouteTableResponse'
      { association =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the association.
associateTransitGatewayRouteTableResponse_association :: Lens.Lens' AssociateTransitGatewayRouteTableResponse (Prelude.Maybe TransitGatewayAssociation)
associateTransitGatewayRouteTableResponse_association = Lens.lens (\AssociateTransitGatewayRouteTableResponse' {association} -> association) (\s@AssociateTransitGatewayRouteTableResponse' {} a -> s {association = a} :: AssociateTransitGatewayRouteTableResponse)

-- | The response's http status code.
associateTransitGatewayRouteTableResponse_httpStatus :: Lens.Lens' AssociateTransitGatewayRouteTableResponse Prelude.Int
associateTransitGatewayRouteTableResponse_httpStatus = Lens.lens (\AssociateTransitGatewayRouteTableResponse' {httpStatus} -> httpStatus) (\s@AssociateTransitGatewayRouteTableResponse' {} a -> s {httpStatus = a} :: AssociateTransitGatewayRouteTableResponse)

instance
  Prelude.NFData
    AssociateTransitGatewayRouteTableResponse
  where
  rnf AssociateTransitGatewayRouteTableResponse' {..} =
    Prelude.rnf association
      `Prelude.seq` Prelude.rnf httpStatus
