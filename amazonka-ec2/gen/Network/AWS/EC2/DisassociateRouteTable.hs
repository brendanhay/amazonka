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
-- Module      : Network.AWS.EC2.DisassociateRouteTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a subnet or gateway from a route table.
--
-- After you perform this action, the subnet no longer uses the routes in
-- the route table. Instead, it uses the routes in the VPC\'s main route
-- table. For more information about route tables, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route Tables>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Network.AWS.EC2.DisassociateRouteTable
  ( -- * Creating a Request
    DisassociateRouteTable (..),
    newDisassociateRouteTable,

    -- * Request Lenses
    disassociateRouteTable_dryRun,
    disassociateRouteTable_associationId,

    -- * Destructuring the Response
    DisassociateRouteTableResponse (..),
    newDisassociateRouteTableResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateRouteTable' smart constructor.
data DisassociateRouteTable = DisassociateRouteTable'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The association ID representing the current association between the
    -- route table and subnet or gateway.
    associationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateRouteTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'disassociateRouteTable_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'associationId', 'disassociateRouteTable_associationId' - The association ID representing the current association between the
-- route table and subnet or gateway.
newDisassociateRouteTable ::
  -- | 'associationId'
  Core.Text ->
  DisassociateRouteTable
newDisassociateRouteTable pAssociationId_ =
  DisassociateRouteTable'
    { dryRun = Core.Nothing,
      associationId = pAssociationId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
disassociateRouteTable_dryRun :: Lens.Lens' DisassociateRouteTable (Core.Maybe Core.Bool)
disassociateRouteTable_dryRun = Lens.lens (\DisassociateRouteTable' {dryRun} -> dryRun) (\s@DisassociateRouteTable' {} a -> s {dryRun = a} :: DisassociateRouteTable)

-- | The association ID representing the current association between the
-- route table and subnet or gateway.
disassociateRouteTable_associationId :: Lens.Lens' DisassociateRouteTable Core.Text
disassociateRouteTable_associationId = Lens.lens (\DisassociateRouteTable' {associationId} -> associationId) (\s@DisassociateRouteTable' {} a -> s {associationId = a} :: DisassociateRouteTable)

instance Core.AWSRequest DisassociateRouteTable where
  type
    AWSResponse DisassociateRouteTable =
      DisassociateRouteTableResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DisassociateRouteTableResponse'

instance Core.Hashable DisassociateRouteTable

instance Core.NFData DisassociateRouteTable

instance Core.ToHeaders DisassociateRouteTable where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DisassociateRouteTable where
  toPath = Core.const "/"

instance Core.ToQuery DisassociateRouteTable where
  toQuery DisassociateRouteTable' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DisassociateRouteTable" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "AssociationId" Core.=: associationId
      ]

-- | /See:/ 'newDisassociateRouteTableResponse' smart constructor.
data DisassociateRouteTableResponse = DisassociateRouteTableResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateRouteTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateRouteTableResponse ::
  DisassociateRouteTableResponse
newDisassociateRouteTableResponse =
  DisassociateRouteTableResponse'

instance Core.NFData DisassociateRouteTableResponse
