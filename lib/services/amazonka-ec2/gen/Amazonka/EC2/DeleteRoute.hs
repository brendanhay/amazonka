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
-- Module      : Amazonka.EC2.DeleteRoute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified route from the specified route table.
module Amazonka.EC2.DeleteRoute
  ( -- * Creating a Request
    DeleteRoute (..),
    newDeleteRoute,

    -- * Request Lenses
    deleteRoute_destinationPrefixListId,
    deleteRoute_dryRun,
    deleteRoute_destinationCidrBlock,
    deleteRoute_destinationIpv6CidrBlock,
    deleteRoute_routeTableId,

    -- * Destructuring the Response
    DeleteRouteResponse (..),
    newDeleteRouteResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRoute' smart constructor.
data DeleteRoute = DeleteRoute'
  { -- | The ID of the prefix list for the route.
    destinationPrefixListId :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IPv4 CIDR range for the route. The value you specify must match the
    -- CIDR for the route exactly.
    destinationCidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 CIDR range for the route. The value you specify must match the
    -- CIDR for the route exactly.
    destinationIpv6CidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The ID of the route table.
    routeTableId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationPrefixListId', 'deleteRoute_destinationPrefixListId' - The ID of the prefix list for the route.
--
-- 'dryRun', 'deleteRoute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'destinationCidrBlock', 'deleteRoute_destinationCidrBlock' - The IPv4 CIDR range for the route. The value you specify must match the
-- CIDR for the route exactly.
--
-- 'destinationIpv6CidrBlock', 'deleteRoute_destinationIpv6CidrBlock' - The IPv6 CIDR range for the route. The value you specify must match the
-- CIDR for the route exactly.
--
-- 'routeTableId', 'deleteRoute_routeTableId' - The ID of the route table.
newDeleteRoute ::
  -- | 'routeTableId'
  Prelude.Text ->
  DeleteRoute
newDeleteRoute pRouteTableId_ =
  DeleteRoute'
    { destinationPrefixListId =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      destinationCidrBlock = Prelude.Nothing,
      destinationIpv6CidrBlock = Prelude.Nothing,
      routeTableId = pRouteTableId_
    }

-- | The ID of the prefix list for the route.
deleteRoute_destinationPrefixListId :: Lens.Lens' DeleteRoute (Prelude.Maybe Prelude.Text)
deleteRoute_destinationPrefixListId = Lens.lens (\DeleteRoute' {destinationPrefixListId} -> destinationPrefixListId) (\s@DeleteRoute' {} a -> s {destinationPrefixListId = a} :: DeleteRoute)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteRoute_dryRun :: Lens.Lens' DeleteRoute (Prelude.Maybe Prelude.Bool)
deleteRoute_dryRun = Lens.lens (\DeleteRoute' {dryRun} -> dryRun) (\s@DeleteRoute' {} a -> s {dryRun = a} :: DeleteRoute)

-- | The IPv4 CIDR range for the route. The value you specify must match the
-- CIDR for the route exactly.
deleteRoute_destinationCidrBlock :: Lens.Lens' DeleteRoute (Prelude.Maybe Prelude.Text)
deleteRoute_destinationCidrBlock = Lens.lens (\DeleteRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@DeleteRoute' {} a -> s {destinationCidrBlock = a} :: DeleteRoute)

-- | The IPv6 CIDR range for the route. The value you specify must match the
-- CIDR for the route exactly.
deleteRoute_destinationIpv6CidrBlock :: Lens.Lens' DeleteRoute (Prelude.Maybe Prelude.Text)
deleteRoute_destinationIpv6CidrBlock = Lens.lens (\DeleteRoute' {destinationIpv6CidrBlock} -> destinationIpv6CidrBlock) (\s@DeleteRoute' {} a -> s {destinationIpv6CidrBlock = a} :: DeleteRoute)

-- | The ID of the route table.
deleteRoute_routeTableId :: Lens.Lens' DeleteRoute Prelude.Text
deleteRoute_routeTableId = Lens.lens (\DeleteRoute' {routeTableId} -> routeTableId) (\s@DeleteRoute' {} a -> s {routeTableId = a} :: DeleteRoute)

instance Core.AWSRequest DeleteRoute where
  type AWSResponse DeleteRoute = DeleteRouteResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response = Response.receiveNull DeleteRouteResponse'

instance Prelude.Hashable DeleteRoute where
  hashWithSalt _salt DeleteRoute' {..} =
    _salt
      `Prelude.hashWithSalt` destinationPrefixListId
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` destinationCidrBlock
      `Prelude.hashWithSalt` destinationIpv6CidrBlock
      `Prelude.hashWithSalt` routeTableId

instance Prelude.NFData DeleteRoute where
  rnf DeleteRoute' {..} =
    Prelude.rnf destinationPrefixListId
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf destinationCidrBlock
      `Prelude.seq` Prelude.rnf destinationIpv6CidrBlock
      `Prelude.seq` Prelude.rnf routeTableId

instance Core.ToHeaders DeleteRoute where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteRoute where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteRoute where
  toQuery DeleteRoute' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteRoute" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DestinationPrefixListId"
          Core.=: destinationPrefixListId,
        "DryRun" Core.=: dryRun,
        "DestinationCidrBlock" Core.=: destinationCidrBlock,
        "DestinationIpv6CidrBlock"
          Core.=: destinationIpv6CidrBlock,
        "RouteTableId" Core.=: routeTableId
      ]

-- | /See:/ 'newDeleteRouteResponse' smart constructor.
data DeleteRouteResponse = DeleteRouteResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRouteResponse ::
  DeleteRouteResponse
newDeleteRouteResponse = DeleteRouteResponse'

instance Prelude.NFData DeleteRouteResponse where
  rnf _ = ()
