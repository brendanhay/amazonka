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
-- Module      : Network.AWS.EC2.AssociateTransitGatewayRouteTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified attachment with the specified transit gateway
-- route table. You can associate only one route table with an attachment.
module Network.AWS.EC2.AssociateTransitGatewayRouteTable
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateTransitGatewayRouteTable' smart constructor.
data AssociateTransitGatewayRouteTable = AssociateTransitGatewayRouteTable'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Core.Text,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'transitGatewayAttachmentId'
  Core.Text ->
  AssociateTransitGatewayRouteTable
newAssociateTransitGatewayRouteTable
  pTransitGatewayRouteTableId_
  pTransitGatewayAttachmentId_ =
    AssociateTransitGatewayRouteTable'
      { dryRun =
          Core.Nothing,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
associateTransitGatewayRouteTable_dryRun :: Lens.Lens' AssociateTransitGatewayRouteTable (Core.Maybe Core.Bool)
associateTransitGatewayRouteTable_dryRun = Lens.lens (\AssociateTransitGatewayRouteTable' {dryRun} -> dryRun) (\s@AssociateTransitGatewayRouteTable' {} a -> s {dryRun = a} :: AssociateTransitGatewayRouteTable)

-- | The ID of the transit gateway route table.
associateTransitGatewayRouteTable_transitGatewayRouteTableId :: Lens.Lens' AssociateTransitGatewayRouteTable Core.Text
associateTransitGatewayRouteTable_transitGatewayRouteTableId = Lens.lens (\AssociateTransitGatewayRouteTable' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@AssociateTransitGatewayRouteTable' {} a -> s {transitGatewayRouteTableId = a} :: AssociateTransitGatewayRouteTable)

-- | The ID of the attachment.
associateTransitGatewayRouteTable_transitGatewayAttachmentId :: Lens.Lens' AssociateTransitGatewayRouteTable Core.Text
associateTransitGatewayRouteTable_transitGatewayAttachmentId = Lens.lens (\AssociateTransitGatewayRouteTable' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@AssociateTransitGatewayRouteTable' {} a -> s {transitGatewayAttachmentId = a} :: AssociateTransitGatewayRouteTable)

instance
  Core.AWSRequest
    AssociateTransitGatewayRouteTable
  where
  type
    AWSResponse AssociateTransitGatewayRouteTable =
      AssociateTransitGatewayRouteTableResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateTransitGatewayRouteTableResponse'
            Core.<$> (x Core..@? "association")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    AssociateTransitGatewayRouteTable

instance
  Core.NFData
    AssociateTransitGatewayRouteTable

instance
  Core.ToHeaders
    AssociateTransitGatewayRouteTable
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    AssociateTransitGatewayRouteTable
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    AssociateTransitGatewayRouteTable
  where
  toQuery AssociateTransitGatewayRouteTable' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "AssociateTransitGatewayRouteTable" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "TransitGatewayRouteTableId"
          Core.=: transitGatewayRouteTableId,
        "TransitGatewayAttachmentId"
          Core.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'newAssociateTransitGatewayRouteTableResponse' smart constructor.
data AssociateTransitGatewayRouteTableResponse = AssociateTransitGatewayRouteTableResponse'
  { -- | The ID of the association.
    association :: Core.Maybe TransitGatewayAssociation,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  AssociateTransitGatewayRouteTableResponse
newAssociateTransitGatewayRouteTableResponse
  pHttpStatus_ =
    AssociateTransitGatewayRouteTableResponse'
      { association =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the association.
associateTransitGatewayRouteTableResponse_association :: Lens.Lens' AssociateTransitGatewayRouteTableResponse (Core.Maybe TransitGatewayAssociation)
associateTransitGatewayRouteTableResponse_association = Lens.lens (\AssociateTransitGatewayRouteTableResponse' {association} -> association) (\s@AssociateTransitGatewayRouteTableResponse' {} a -> s {association = a} :: AssociateTransitGatewayRouteTableResponse)

-- | The response's http status code.
associateTransitGatewayRouteTableResponse_httpStatus :: Lens.Lens' AssociateTransitGatewayRouteTableResponse Core.Int
associateTransitGatewayRouteTableResponse_httpStatus = Lens.lens (\AssociateTransitGatewayRouteTableResponse' {httpStatus} -> httpStatus) (\s@AssociateTransitGatewayRouteTableResponse' {} a -> s {httpStatus = a} :: AssociateTransitGatewayRouteTableResponse)

instance
  Core.NFData
    AssociateTransitGatewayRouteTableResponse
