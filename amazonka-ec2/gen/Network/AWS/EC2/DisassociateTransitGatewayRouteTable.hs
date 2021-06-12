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
-- Module      : Network.AWS.EC2.DisassociateTransitGatewayRouteTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a resource attachment from a transit gateway route table.
module Network.AWS.EC2.DisassociateTransitGatewayRouteTable
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateTransitGatewayRouteTable' smart constructor.
data DisassociateTransitGatewayRouteTable = DisassociateTransitGatewayRouteTable'
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
  Core.Text ->
  -- | 'transitGatewayAttachmentId'
  Core.Text ->
  DisassociateTransitGatewayRouteTable
newDisassociateTransitGatewayRouteTable
  pTransitGatewayRouteTableId_
  pTransitGatewayAttachmentId_ =
    DisassociateTransitGatewayRouteTable'
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
disassociateTransitGatewayRouteTable_dryRun :: Lens.Lens' DisassociateTransitGatewayRouteTable (Core.Maybe Core.Bool)
disassociateTransitGatewayRouteTable_dryRun = Lens.lens (\DisassociateTransitGatewayRouteTable' {dryRun} -> dryRun) (\s@DisassociateTransitGatewayRouteTable' {} a -> s {dryRun = a} :: DisassociateTransitGatewayRouteTable)

-- | The ID of the transit gateway route table.
disassociateTransitGatewayRouteTable_transitGatewayRouteTableId :: Lens.Lens' DisassociateTransitGatewayRouteTable Core.Text
disassociateTransitGatewayRouteTable_transitGatewayRouteTableId = Lens.lens (\DisassociateTransitGatewayRouteTable' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@DisassociateTransitGatewayRouteTable' {} a -> s {transitGatewayRouteTableId = a} :: DisassociateTransitGatewayRouteTable)

-- | The ID of the attachment.
disassociateTransitGatewayRouteTable_transitGatewayAttachmentId :: Lens.Lens' DisassociateTransitGatewayRouteTable Core.Text
disassociateTransitGatewayRouteTable_transitGatewayAttachmentId = Lens.lens (\DisassociateTransitGatewayRouteTable' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@DisassociateTransitGatewayRouteTable' {} a -> s {transitGatewayAttachmentId = a} :: DisassociateTransitGatewayRouteTable)

instance
  Core.AWSRequest
    DisassociateTransitGatewayRouteTable
  where
  type
    AWSResponse DisassociateTransitGatewayRouteTable =
      DisassociateTransitGatewayRouteTableResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DisassociateTransitGatewayRouteTableResponse'
            Core.<$> (x Core..@? "association")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DisassociateTransitGatewayRouteTable

instance
  Core.NFData
    DisassociateTransitGatewayRouteTable

instance
  Core.ToHeaders
    DisassociateTransitGatewayRouteTable
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DisassociateTransitGatewayRouteTable
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DisassociateTransitGatewayRouteTable
  where
  toQuery DisassociateTransitGatewayRouteTable' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DisassociateTransitGatewayRouteTable" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "TransitGatewayRouteTableId"
          Core.=: transitGatewayRouteTableId,
        "TransitGatewayAttachmentId"
          Core.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'newDisassociateTransitGatewayRouteTableResponse' smart constructor.
data DisassociateTransitGatewayRouteTableResponse = DisassociateTransitGatewayRouteTableResponse'
  { -- | Information about the association.
    association :: Core.Maybe TransitGatewayAssociation,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DisassociateTransitGatewayRouteTableResponse
newDisassociateTransitGatewayRouteTableResponse
  pHttpStatus_ =
    DisassociateTransitGatewayRouteTableResponse'
      { association =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the association.
disassociateTransitGatewayRouteTableResponse_association :: Lens.Lens' DisassociateTransitGatewayRouteTableResponse (Core.Maybe TransitGatewayAssociation)
disassociateTransitGatewayRouteTableResponse_association = Lens.lens (\DisassociateTransitGatewayRouteTableResponse' {association} -> association) (\s@DisassociateTransitGatewayRouteTableResponse' {} a -> s {association = a} :: DisassociateTransitGatewayRouteTableResponse)

-- | The response's http status code.
disassociateTransitGatewayRouteTableResponse_httpStatus :: Lens.Lens' DisassociateTransitGatewayRouteTableResponse Core.Int
disassociateTransitGatewayRouteTableResponse_httpStatus = Lens.lens (\DisassociateTransitGatewayRouteTableResponse' {httpStatus} -> httpStatus) (\s@DisassociateTransitGatewayRouteTableResponse' {} a -> s {httpStatus = a} :: DisassociateTransitGatewayRouteTableResponse)

instance
  Core.NFData
    DisassociateTransitGatewayRouteTableResponse
