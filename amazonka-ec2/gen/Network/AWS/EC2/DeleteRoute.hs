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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRoute' smart constructor.
data DeleteRoute = DeleteRoute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the prefix list for the route.
    destinationPrefixListId :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 CIDR range for the route. The value you specify must match the
    -- CIDR for the route exactly.
    destinationIpv6CidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 CIDR range for the route. The value you specify must match the
    -- CIDR for the route exactly.
    destinationCidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The ID of the route table.
    routeTableId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteRoute
newDeleteRoute pRouteTableId_ =
  DeleteRoute'
    { dryRun = Prelude.Nothing,
      destinationPrefixListId = Prelude.Nothing,
      destinationIpv6CidrBlock = Prelude.Nothing,
      destinationCidrBlock = Prelude.Nothing,
      routeTableId = pRouteTableId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteRoute_dryRun :: Lens.Lens' DeleteRoute (Prelude.Maybe Prelude.Bool)
deleteRoute_dryRun = Lens.lens (\DeleteRoute' {dryRun} -> dryRun) (\s@DeleteRoute' {} a -> s {dryRun = a} :: DeleteRoute)

-- | The ID of the prefix list for the route.
deleteRoute_destinationPrefixListId :: Lens.Lens' DeleteRoute (Prelude.Maybe Prelude.Text)
deleteRoute_destinationPrefixListId = Lens.lens (\DeleteRoute' {destinationPrefixListId} -> destinationPrefixListId) (\s@DeleteRoute' {} a -> s {destinationPrefixListId = a} :: DeleteRoute)

-- | The IPv6 CIDR range for the route. The value you specify must match the
-- CIDR for the route exactly.
deleteRoute_destinationIpv6CidrBlock :: Lens.Lens' DeleteRoute (Prelude.Maybe Prelude.Text)
deleteRoute_destinationIpv6CidrBlock = Lens.lens (\DeleteRoute' {destinationIpv6CidrBlock} -> destinationIpv6CidrBlock) (\s@DeleteRoute' {} a -> s {destinationIpv6CidrBlock = a} :: DeleteRoute)

-- | The IPv4 CIDR range for the route. The value you specify must match the
-- CIDR for the route exactly.
deleteRoute_destinationCidrBlock :: Lens.Lens' DeleteRoute (Prelude.Maybe Prelude.Text)
deleteRoute_destinationCidrBlock = Lens.lens (\DeleteRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@DeleteRoute' {} a -> s {destinationCidrBlock = a} :: DeleteRoute)

-- | The ID of the route table.
deleteRoute_routeTableId :: Lens.Lens' DeleteRoute Prelude.Text
deleteRoute_routeTableId = Lens.lens (\DeleteRoute' {routeTableId} -> routeTableId) (\s@DeleteRoute' {} a -> s {routeTableId = a} :: DeleteRoute)

instance Prelude.AWSRequest DeleteRoute where
  type Rs DeleteRoute = DeleteRouteResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull DeleteRouteResponse'

instance Prelude.Hashable DeleteRoute

instance Prelude.NFData DeleteRoute

instance Prelude.ToHeaders DeleteRoute where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteRoute where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteRoute where
  toQuery DeleteRoute' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteRoute" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "DestinationPrefixListId"
          Prelude.=: destinationPrefixListId,
        "DestinationIpv6CidrBlock"
          Prelude.=: destinationIpv6CidrBlock,
        "DestinationCidrBlock"
          Prelude.=: destinationCidrBlock,
        "RouteTableId" Prelude.=: routeTableId
      ]

-- | /See:/ 'newDeleteRouteResponse' smart constructor.
data DeleteRouteResponse = DeleteRouteResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRouteResponse ::
  DeleteRouteResponse
newDeleteRouteResponse = DeleteRouteResponse'

instance Prelude.NFData DeleteRouteResponse
