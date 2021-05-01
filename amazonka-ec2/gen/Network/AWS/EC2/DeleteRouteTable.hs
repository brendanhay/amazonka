{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRouteTable' smart constructor.
data DeleteRouteTable = DeleteRouteTable'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the route table.
    routeTableId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteRouteTable
newDeleteRouteTable pRouteTableId_ =
  DeleteRouteTable'
    { dryRun = Prelude.Nothing,
      routeTableId = pRouteTableId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteRouteTable_dryRun :: Lens.Lens' DeleteRouteTable (Prelude.Maybe Prelude.Bool)
deleteRouteTable_dryRun = Lens.lens (\DeleteRouteTable' {dryRun} -> dryRun) (\s@DeleteRouteTable' {} a -> s {dryRun = a} :: DeleteRouteTable)

-- | The ID of the route table.
deleteRouteTable_routeTableId :: Lens.Lens' DeleteRouteTable Prelude.Text
deleteRouteTable_routeTableId = Lens.lens (\DeleteRouteTable' {routeTableId} -> routeTableId) (\s@DeleteRouteTable' {} a -> s {routeTableId = a} :: DeleteRouteTable)

instance Prelude.AWSRequest DeleteRouteTable where
  type Rs DeleteRouteTable = DeleteRouteTableResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteRouteTableResponse'

instance Prelude.Hashable DeleteRouteTable

instance Prelude.NFData DeleteRouteTable

instance Prelude.ToHeaders DeleteRouteTable where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteRouteTable where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteRouteTable where
  toQuery DeleteRouteTable' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteRouteTable" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "RouteTableId" Prelude.=: routeTableId
      ]

-- | /See:/ 'newDeleteRouteTableResponse' smart constructor.
data DeleteRouteTableResponse = DeleteRouteTableResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteRouteTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRouteTableResponse ::
  DeleteRouteTableResponse
newDeleteRouteTableResponse =
  DeleteRouteTableResponse'

instance Prelude.NFData DeleteRouteTableResponse
