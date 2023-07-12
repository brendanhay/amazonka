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
-- Module      : Amazonka.EC2.CreateLocalGatewayRoute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a static route for the specified local gateway route table. You
-- must specify one of the following targets:
--
-- -   @LocalGatewayVirtualInterfaceGroupId@
--
-- -   @NetworkInterfaceId@
module Amazonka.EC2.CreateLocalGatewayRoute
  ( -- * Creating a Request
    CreateLocalGatewayRoute (..),
    newCreateLocalGatewayRoute,

    -- * Request Lenses
    createLocalGatewayRoute_dryRun,
    createLocalGatewayRoute_localGatewayVirtualInterfaceGroupId,
    createLocalGatewayRoute_networkInterfaceId,
    createLocalGatewayRoute_destinationCidrBlock,
    createLocalGatewayRoute_localGatewayRouteTableId,

    -- * Destructuring the Response
    CreateLocalGatewayRouteResponse (..),
    newCreateLocalGatewayRouteResponse,

    -- * Response Lenses
    createLocalGatewayRouteResponse_route,
    createLocalGatewayRouteResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLocalGatewayRoute' smart constructor.
data CreateLocalGatewayRoute = CreateLocalGatewayRoute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the virtual interface group.
    localGatewayVirtualInterfaceGroupId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The CIDR range used for destination matches. Routing decisions are based
    -- on the most specific match.
    destinationCidrBlock :: Prelude.Text,
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocalGatewayRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createLocalGatewayRoute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'localGatewayVirtualInterfaceGroupId', 'createLocalGatewayRoute_localGatewayVirtualInterfaceGroupId' - The ID of the virtual interface group.
--
-- 'networkInterfaceId', 'createLocalGatewayRoute_networkInterfaceId' - The ID of the network interface.
--
-- 'destinationCidrBlock', 'createLocalGatewayRoute_destinationCidrBlock' - The CIDR range used for destination matches. Routing decisions are based
-- on the most specific match.
--
-- 'localGatewayRouteTableId', 'createLocalGatewayRoute_localGatewayRouteTableId' - The ID of the local gateway route table.
newCreateLocalGatewayRoute ::
  -- | 'destinationCidrBlock'
  Prelude.Text ->
  -- | 'localGatewayRouteTableId'
  Prelude.Text ->
  CreateLocalGatewayRoute
newCreateLocalGatewayRoute
  pDestinationCidrBlock_
  pLocalGatewayRouteTableId_ =
    CreateLocalGatewayRoute'
      { dryRun = Prelude.Nothing,
        localGatewayVirtualInterfaceGroupId =
          Prelude.Nothing,
        networkInterfaceId = Prelude.Nothing,
        destinationCidrBlock = pDestinationCidrBlock_,
        localGatewayRouteTableId =
          pLocalGatewayRouteTableId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createLocalGatewayRoute_dryRun :: Lens.Lens' CreateLocalGatewayRoute (Prelude.Maybe Prelude.Bool)
createLocalGatewayRoute_dryRun = Lens.lens (\CreateLocalGatewayRoute' {dryRun} -> dryRun) (\s@CreateLocalGatewayRoute' {} a -> s {dryRun = a} :: CreateLocalGatewayRoute)

-- | The ID of the virtual interface group.
createLocalGatewayRoute_localGatewayVirtualInterfaceGroupId :: Lens.Lens' CreateLocalGatewayRoute (Prelude.Maybe Prelude.Text)
createLocalGatewayRoute_localGatewayVirtualInterfaceGroupId = Lens.lens (\CreateLocalGatewayRoute' {localGatewayVirtualInterfaceGroupId} -> localGatewayVirtualInterfaceGroupId) (\s@CreateLocalGatewayRoute' {} a -> s {localGatewayVirtualInterfaceGroupId = a} :: CreateLocalGatewayRoute)

-- | The ID of the network interface.
createLocalGatewayRoute_networkInterfaceId :: Lens.Lens' CreateLocalGatewayRoute (Prelude.Maybe Prelude.Text)
createLocalGatewayRoute_networkInterfaceId = Lens.lens (\CreateLocalGatewayRoute' {networkInterfaceId} -> networkInterfaceId) (\s@CreateLocalGatewayRoute' {} a -> s {networkInterfaceId = a} :: CreateLocalGatewayRoute)

-- | The CIDR range used for destination matches. Routing decisions are based
-- on the most specific match.
createLocalGatewayRoute_destinationCidrBlock :: Lens.Lens' CreateLocalGatewayRoute Prelude.Text
createLocalGatewayRoute_destinationCidrBlock = Lens.lens (\CreateLocalGatewayRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@CreateLocalGatewayRoute' {} a -> s {destinationCidrBlock = a} :: CreateLocalGatewayRoute)

-- | The ID of the local gateway route table.
createLocalGatewayRoute_localGatewayRouteTableId :: Lens.Lens' CreateLocalGatewayRoute Prelude.Text
createLocalGatewayRoute_localGatewayRouteTableId = Lens.lens (\CreateLocalGatewayRoute' {localGatewayRouteTableId} -> localGatewayRouteTableId) (\s@CreateLocalGatewayRoute' {} a -> s {localGatewayRouteTableId = a} :: CreateLocalGatewayRoute)

instance Core.AWSRequest CreateLocalGatewayRoute where
  type
    AWSResponse CreateLocalGatewayRoute =
      CreateLocalGatewayRouteResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateLocalGatewayRouteResponse'
            Prelude.<$> (x Data..@? "route")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLocalGatewayRoute where
  hashWithSalt _salt CreateLocalGatewayRoute' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` localGatewayVirtualInterfaceGroupId
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` destinationCidrBlock
      `Prelude.hashWithSalt` localGatewayRouteTableId

instance Prelude.NFData CreateLocalGatewayRoute where
  rnf CreateLocalGatewayRoute' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf localGatewayVirtualInterfaceGroupId
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf destinationCidrBlock
      `Prelude.seq` Prelude.rnf localGatewayRouteTableId

instance Data.ToHeaders CreateLocalGatewayRoute where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateLocalGatewayRoute where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLocalGatewayRoute where
  toQuery CreateLocalGatewayRoute' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateLocalGatewayRoute" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "LocalGatewayVirtualInterfaceGroupId"
          Data.=: localGatewayVirtualInterfaceGroupId,
        "NetworkInterfaceId" Data.=: networkInterfaceId,
        "DestinationCidrBlock" Data.=: destinationCidrBlock,
        "LocalGatewayRouteTableId"
          Data.=: localGatewayRouteTableId
      ]

-- | /See:/ 'newCreateLocalGatewayRouteResponse' smart constructor.
data CreateLocalGatewayRouteResponse = CreateLocalGatewayRouteResponse'
  { -- | Information about the route.
    route :: Prelude.Maybe LocalGatewayRoute,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocalGatewayRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'route', 'createLocalGatewayRouteResponse_route' - Information about the route.
--
-- 'httpStatus', 'createLocalGatewayRouteResponse_httpStatus' - The response's http status code.
newCreateLocalGatewayRouteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLocalGatewayRouteResponse
newCreateLocalGatewayRouteResponse pHttpStatus_ =
  CreateLocalGatewayRouteResponse'
    { route =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the route.
createLocalGatewayRouteResponse_route :: Lens.Lens' CreateLocalGatewayRouteResponse (Prelude.Maybe LocalGatewayRoute)
createLocalGatewayRouteResponse_route = Lens.lens (\CreateLocalGatewayRouteResponse' {route} -> route) (\s@CreateLocalGatewayRouteResponse' {} a -> s {route = a} :: CreateLocalGatewayRouteResponse)

-- | The response's http status code.
createLocalGatewayRouteResponse_httpStatus :: Lens.Lens' CreateLocalGatewayRouteResponse Prelude.Int
createLocalGatewayRouteResponse_httpStatus = Lens.lens (\CreateLocalGatewayRouteResponse' {httpStatus} -> httpStatus) (\s@CreateLocalGatewayRouteResponse' {} a -> s {httpStatus = a} :: CreateLocalGatewayRouteResponse)

instance
  Prelude.NFData
    CreateLocalGatewayRouteResponse
  where
  rnf CreateLocalGatewayRouteResponse' {..} =
    Prelude.rnf route
      `Prelude.seq` Prelude.rnf httpStatus
