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
-- Module      : Network.AWS.EC2.DeleteRouteTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified route table. You must disassociate the route table
-- from any subnets before you can delete it. You can\'t delete the main
-- route table.
module Network.AWS.EC2.DeleteRouteTable
  ( -- * Creating a Request
    DeleteRouteTable (..),
    newDeleteRouteTable,

    -- * Request Lenses
    deleteRouteTable_dryRun,
    deleteRouteTable_routeTableId,

    -- * Destructuring the Response
    DeleteRouteTableResponse (..),
    newDeleteRouteTableResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRouteTable' smart constructor.
data DeleteRouteTable = DeleteRouteTable'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the route table.
    routeTableId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRouteTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteRouteTable_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'routeTableId', 'deleteRouteTable_routeTableId' - The ID of the route table.
newDeleteRouteTable ::
  -- | 'routeTableId'
  Core.Text ->
  DeleteRouteTable
newDeleteRouteTable pRouteTableId_ =
  DeleteRouteTable'
    { dryRun = Core.Nothing,
      routeTableId = pRouteTableId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteRouteTable_dryRun :: Lens.Lens' DeleteRouteTable (Core.Maybe Core.Bool)
deleteRouteTable_dryRun = Lens.lens (\DeleteRouteTable' {dryRun} -> dryRun) (\s@DeleteRouteTable' {} a -> s {dryRun = a} :: DeleteRouteTable)

-- | The ID of the route table.
deleteRouteTable_routeTableId :: Lens.Lens' DeleteRouteTable Core.Text
deleteRouteTable_routeTableId = Lens.lens (\DeleteRouteTable' {routeTableId} -> routeTableId) (\s@DeleteRouteTable' {} a -> s {routeTableId = a} :: DeleteRouteTable)

instance Core.AWSRequest DeleteRouteTable where
  type
    AWSResponse DeleteRouteTable =
      DeleteRouteTableResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteRouteTableResponse'

instance Core.Hashable DeleteRouteTable

instance Core.NFData DeleteRouteTable

instance Core.ToHeaders DeleteRouteTable where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteRouteTable where
  toPath = Core.const "/"

instance Core.ToQuery DeleteRouteTable where
  toQuery DeleteRouteTable' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteRouteTable" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "RouteTableId" Core.=: routeTableId
      ]

-- | /See:/ 'newDeleteRouteTableResponse' smart constructor.
data DeleteRouteTableResponse = DeleteRouteTableResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRouteTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRouteTableResponse ::
  DeleteRouteTableResponse
newDeleteRouteTableResponse =
  DeleteRouteTableResponse'

instance Core.NFData DeleteRouteTableResponse
