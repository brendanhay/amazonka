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
-- Module      : Amazonka.EC2.ModifyLocalGatewayRoute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified local gateway route.
module Amazonka.EC2.ModifyLocalGatewayRoute
  ( -- * Creating a Request
    ModifyLocalGatewayRoute (..),
    newModifyLocalGatewayRoute,

    -- * Request Lenses
    modifyLocalGatewayRoute_dryRun,
    modifyLocalGatewayRoute_networkInterfaceId,
    modifyLocalGatewayRoute_localGatewayVirtualInterfaceGroupId,
    modifyLocalGatewayRoute_destinationCidrBlock,
    modifyLocalGatewayRoute_localGatewayRouteTableId,

    -- * Destructuring the Response
    ModifyLocalGatewayRouteResponse (..),
    newModifyLocalGatewayRouteResponse,

    -- * Response Lenses
    modifyLocalGatewayRouteResponse_route,
    modifyLocalGatewayRouteResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyLocalGatewayRoute' smart constructor.
data ModifyLocalGatewayRoute = ModifyLocalGatewayRoute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual interface group.
    localGatewayVirtualInterfaceGroupId :: Prelude.Maybe Prelude.Text,
    -- | The CIDR block used for destination matches. The value that you provide
    -- must match the CIDR of an existing route in the table.
    destinationCidrBlock :: Prelude.Text,
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyLocalGatewayRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifyLocalGatewayRoute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'networkInterfaceId', 'modifyLocalGatewayRoute_networkInterfaceId' - The ID of the network interface.
--
-- 'localGatewayVirtualInterfaceGroupId', 'modifyLocalGatewayRoute_localGatewayVirtualInterfaceGroupId' - The ID of the virtual interface group.
--
-- 'destinationCidrBlock', 'modifyLocalGatewayRoute_destinationCidrBlock' - The CIDR block used for destination matches. The value that you provide
-- must match the CIDR of an existing route in the table.
--
-- 'localGatewayRouteTableId', 'modifyLocalGatewayRoute_localGatewayRouteTableId' - The ID of the local gateway route table.
newModifyLocalGatewayRoute ::
  -- | 'destinationCidrBlock'
  Prelude.Text ->
  -- | 'localGatewayRouteTableId'
  Prelude.Text ->
  ModifyLocalGatewayRoute
newModifyLocalGatewayRoute
  pDestinationCidrBlock_
  pLocalGatewayRouteTableId_ =
    ModifyLocalGatewayRoute'
      { dryRun = Prelude.Nothing,
        networkInterfaceId = Prelude.Nothing,
        localGatewayVirtualInterfaceGroupId =
          Prelude.Nothing,
        destinationCidrBlock = pDestinationCidrBlock_,
        localGatewayRouteTableId =
          pLocalGatewayRouteTableId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyLocalGatewayRoute_dryRun :: Lens.Lens' ModifyLocalGatewayRoute (Prelude.Maybe Prelude.Bool)
modifyLocalGatewayRoute_dryRun = Lens.lens (\ModifyLocalGatewayRoute' {dryRun} -> dryRun) (\s@ModifyLocalGatewayRoute' {} a -> s {dryRun = a} :: ModifyLocalGatewayRoute)

-- | The ID of the network interface.
modifyLocalGatewayRoute_networkInterfaceId :: Lens.Lens' ModifyLocalGatewayRoute (Prelude.Maybe Prelude.Text)
modifyLocalGatewayRoute_networkInterfaceId = Lens.lens (\ModifyLocalGatewayRoute' {networkInterfaceId} -> networkInterfaceId) (\s@ModifyLocalGatewayRoute' {} a -> s {networkInterfaceId = a} :: ModifyLocalGatewayRoute)

-- | The ID of the virtual interface group.
modifyLocalGatewayRoute_localGatewayVirtualInterfaceGroupId :: Lens.Lens' ModifyLocalGatewayRoute (Prelude.Maybe Prelude.Text)
modifyLocalGatewayRoute_localGatewayVirtualInterfaceGroupId = Lens.lens (\ModifyLocalGatewayRoute' {localGatewayVirtualInterfaceGroupId} -> localGatewayVirtualInterfaceGroupId) (\s@ModifyLocalGatewayRoute' {} a -> s {localGatewayVirtualInterfaceGroupId = a} :: ModifyLocalGatewayRoute)

-- | The CIDR block used for destination matches. The value that you provide
-- must match the CIDR of an existing route in the table.
modifyLocalGatewayRoute_destinationCidrBlock :: Lens.Lens' ModifyLocalGatewayRoute Prelude.Text
modifyLocalGatewayRoute_destinationCidrBlock = Lens.lens (\ModifyLocalGatewayRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@ModifyLocalGatewayRoute' {} a -> s {destinationCidrBlock = a} :: ModifyLocalGatewayRoute)

-- | The ID of the local gateway route table.
modifyLocalGatewayRoute_localGatewayRouteTableId :: Lens.Lens' ModifyLocalGatewayRoute Prelude.Text
modifyLocalGatewayRoute_localGatewayRouteTableId = Lens.lens (\ModifyLocalGatewayRoute' {localGatewayRouteTableId} -> localGatewayRouteTableId) (\s@ModifyLocalGatewayRoute' {} a -> s {localGatewayRouteTableId = a} :: ModifyLocalGatewayRoute)

instance Core.AWSRequest ModifyLocalGatewayRoute where
  type
    AWSResponse ModifyLocalGatewayRoute =
      ModifyLocalGatewayRouteResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyLocalGatewayRouteResponse'
            Prelude.<$> (x Core..@? "route")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyLocalGatewayRoute where
  hashWithSalt _salt ModifyLocalGatewayRoute' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` localGatewayVirtualInterfaceGroupId
      `Prelude.hashWithSalt` destinationCidrBlock
      `Prelude.hashWithSalt` localGatewayRouteTableId

instance Prelude.NFData ModifyLocalGatewayRoute where
  rnf ModifyLocalGatewayRoute' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf localGatewayVirtualInterfaceGroupId
      `Prelude.seq` Prelude.rnf destinationCidrBlock
      `Prelude.seq` Prelude.rnf localGatewayRouteTableId

instance Core.ToHeaders ModifyLocalGatewayRoute where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyLocalGatewayRoute where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyLocalGatewayRoute where
  toQuery ModifyLocalGatewayRoute' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyLocalGatewayRoute" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "NetworkInterfaceId" Core.=: networkInterfaceId,
        "LocalGatewayVirtualInterfaceGroupId"
          Core.=: localGatewayVirtualInterfaceGroupId,
        "DestinationCidrBlock" Core.=: destinationCidrBlock,
        "LocalGatewayRouteTableId"
          Core.=: localGatewayRouteTableId
      ]

-- | /See:/ 'newModifyLocalGatewayRouteResponse' smart constructor.
data ModifyLocalGatewayRouteResponse = ModifyLocalGatewayRouteResponse'
  { route :: Prelude.Maybe LocalGatewayRoute,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyLocalGatewayRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'route', 'modifyLocalGatewayRouteResponse_route' - Undocumented member.
--
-- 'httpStatus', 'modifyLocalGatewayRouteResponse_httpStatus' - The response's http status code.
newModifyLocalGatewayRouteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyLocalGatewayRouteResponse
newModifyLocalGatewayRouteResponse pHttpStatus_ =
  ModifyLocalGatewayRouteResponse'
    { route =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyLocalGatewayRouteResponse_route :: Lens.Lens' ModifyLocalGatewayRouteResponse (Prelude.Maybe LocalGatewayRoute)
modifyLocalGatewayRouteResponse_route = Lens.lens (\ModifyLocalGatewayRouteResponse' {route} -> route) (\s@ModifyLocalGatewayRouteResponse' {} a -> s {route = a} :: ModifyLocalGatewayRouteResponse)

-- | The response's http status code.
modifyLocalGatewayRouteResponse_httpStatus :: Lens.Lens' ModifyLocalGatewayRouteResponse Prelude.Int
modifyLocalGatewayRouteResponse_httpStatus = Lens.lens (\ModifyLocalGatewayRouteResponse' {httpStatus} -> httpStatus) (\s@ModifyLocalGatewayRouteResponse' {} a -> s {httpStatus = a} :: ModifyLocalGatewayRouteResponse)

instance
  Prelude.NFData
    ModifyLocalGatewayRouteResponse
  where
  rnf ModifyLocalGatewayRouteResponse' {..} =
    Prelude.rnf route
      `Prelude.seq` Prelude.rnf httpStatus
