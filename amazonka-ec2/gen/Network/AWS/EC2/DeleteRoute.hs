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
-- Module      : Network.AWS.EC2.DeleteRoute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified route from the specified route table.
module Network.AWS.EC2.DeleteRoute
  ( -- * Creating a Request
    DeleteRoute (..),
    newDeleteRoute,

    -- * Request Lenses
    deleteRoute_dryRun,
    deleteRoute_destinationPrefixListId,
    deleteRoute_destinationIpv6CidrBlock,
    deleteRoute_destinationCidrBlock,
    deleteRoute_routeTableId,

    -- * Destructuring the Response
    DeleteRouteResponse (..),
    newDeleteRouteResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRoute' smart constructor.
data DeleteRoute = DeleteRoute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the prefix list for the route.
    destinationPrefixListId :: Core.Maybe Core.Text,
    -- | The IPv6 CIDR range for the route. The value you specify must match the
    -- CIDR for the route exactly.
    destinationIpv6CidrBlock :: Core.Maybe Core.Text,
    -- | The IPv4 CIDR range for the route. The value you specify must match the
    -- CIDR for the route exactly.
    destinationCidrBlock :: Core.Maybe Core.Text,
    -- | The ID of the route table.
    routeTableId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteRoute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'destinationPrefixListId', 'deleteRoute_destinationPrefixListId' - The ID of the prefix list for the route.
--
-- 'destinationIpv6CidrBlock', 'deleteRoute_destinationIpv6CidrBlock' - The IPv6 CIDR range for the route. The value you specify must match the
-- CIDR for the route exactly.
--
-- 'destinationCidrBlock', 'deleteRoute_destinationCidrBlock' - The IPv4 CIDR range for the route. The value you specify must match the
-- CIDR for the route exactly.
--
-- 'routeTableId', 'deleteRoute_routeTableId' - The ID of the route table.
newDeleteRoute ::
  -- | 'routeTableId'
  Core.Text ->
  DeleteRoute
newDeleteRoute pRouteTableId_ =
  DeleteRoute'
    { dryRun = Core.Nothing,
      destinationPrefixListId = Core.Nothing,
      destinationIpv6CidrBlock = Core.Nothing,
      destinationCidrBlock = Core.Nothing,
      routeTableId = pRouteTableId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteRoute_dryRun :: Lens.Lens' DeleteRoute (Core.Maybe Core.Bool)
deleteRoute_dryRun = Lens.lens (\DeleteRoute' {dryRun} -> dryRun) (\s@DeleteRoute' {} a -> s {dryRun = a} :: DeleteRoute)

-- | The ID of the prefix list for the route.
deleteRoute_destinationPrefixListId :: Lens.Lens' DeleteRoute (Core.Maybe Core.Text)
deleteRoute_destinationPrefixListId = Lens.lens (\DeleteRoute' {destinationPrefixListId} -> destinationPrefixListId) (\s@DeleteRoute' {} a -> s {destinationPrefixListId = a} :: DeleteRoute)

-- | The IPv6 CIDR range for the route. The value you specify must match the
-- CIDR for the route exactly.
deleteRoute_destinationIpv6CidrBlock :: Lens.Lens' DeleteRoute (Core.Maybe Core.Text)
deleteRoute_destinationIpv6CidrBlock = Lens.lens (\DeleteRoute' {destinationIpv6CidrBlock} -> destinationIpv6CidrBlock) (\s@DeleteRoute' {} a -> s {destinationIpv6CidrBlock = a} :: DeleteRoute)

-- | The IPv4 CIDR range for the route. The value you specify must match the
-- CIDR for the route exactly.
deleteRoute_destinationCidrBlock :: Lens.Lens' DeleteRoute (Core.Maybe Core.Text)
deleteRoute_destinationCidrBlock = Lens.lens (\DeleteRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@DeleteRoute' {} a -> s {destinationCidrBlock = a} :: DeleteRoute)

-- | The ID of the route table.
deleteRoute_routeTableId :: Lens.Lens' DeleteRoute Core.Text
deleteRoute_routeTableId = Lens.lens (\DeleteRoute' {routeTableId} -> routeTableId) (\s@DeleteRoute' {} a -> s {routeTableId = a} :: DeleteRoute)

instance Core.AWSRequest DeleteRoute where
  type AWSResponse DeleteRoute = DeleteRouteResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull DeleteRouteResponse'

instance Core.Hashable DeleteRoute

instance Core.NFData DeleteRoute

instance Core.ToHeaders DeleteRoute where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteRoute where
  toPath = Core.const "/"

instance Core.ToQuery DeleteRoute where
  toQuery DeleteRoute' {..} =
    Core.mconcat
      [ "Action" Core.=: ("DeleteRoute" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "DestinationPrefixListId"
          Core.=: destinationPrefixListId,
        "DestinationIpv6CidrBlock"
          Core.=: destinationIpv6CidrBlock,
        "DestinationCidrBlock" Core.=: destinationCidrBlock,
        "RouteTableId" Core.=: routeTableId
      ]

-- | /See:/ 'newDeleteRouteResponse' smart constructor.
data DeleteRouteResponse = DeleteRouteResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRouteResponse ::
  DeleteRouteResponse
newDeleteRouteResponse = DeleteRouteResponse'

instance Core.NFData DeleteRouteResponse
