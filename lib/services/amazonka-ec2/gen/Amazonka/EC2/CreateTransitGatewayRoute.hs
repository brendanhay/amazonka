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
-- Module      : Amazonka.EC2.CreateTransitGatewayRoute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a static route for the specified transit gateway route table.
module Amazonka.EC2.CreateTransitGatewayRoute
  ( -- * Creating a Request
    CreateTransitGatewayRoute (..),
    newCreateTransitGatewayRoute,

    -- * Request Lenses
    createTransitGatewayRoute_transitGatewayAttachmentId,
    createTransitGatewayRoute_dryRun,
    createTransitGatewayRoute_blackhole,
    createTransitGatewayRoute_destinationCidrBlock,
    createTransitGatewayRoute_transitGatewayRouteTableId,

    -- * Destructuring the Response
    CreateTransitGatewayRouteResponse (..),
    newCreateTransitGatewayRouteResponse,

    -- * Response Lenses
    createTransitGatewayRouteResponse_route,
    createTransitGatewayRouteResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTransitGatewayRoute' smart constructor.
data CreateTransitGatewayRoute = CreateTransitGatewayRoute'
  { -- | The ID of the attachment.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether to drop traffic that matches this route.
    blackhole :: Prelude.Maybe Prelude.Bool,
    -- | The CIDR range used for destination matches. Routing decisions are based
    -- on the most specific match.
    destinationCidrBlock :: Prelude.Text,
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayAttachmentId', 'createTransitGatewayRoute_transitGatewayAttachmentId' - The ID of the attachment.
--
-- 'dryRun', 'createTransitGatewayRoute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'blackhole', 'createTransitGatewayRoute_blackhole' - Indicates whether to drop traffic that matches this route.
--
-- 'destinationCidrBlock', 'createTransitGatewayRoute_destinationCidrBlock' - The CIDR range used for destination matches. Routing decisions are based
-- on the most specific match.
--
-- 'transitGatewayRouteTableId', 'createTransitGatewayRoute_transitGatewayRouteTableId' - The ID of the transit gateway route table.
newCreateTransitGatewayRoute ::
  -- | 'destinationCidrBlock'
  Prelude.Text ->
  -- | 'transitGatewayRouteTableId'
  Prelude.Text ->
  CreateTransitGatewayRoute
newCreateTransitGatewayRoute
  pDestinationCidrBlock_
  pTransitGatewayRouteTableId_ =
    CreateTransitGatewayRoute'
      { transitGatewayAttachmentId =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        blackhole = Prelude.Nothing,
        destinationCidrBlock = pDestinationCidrBlock_,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_
      }

-- | The ID of the attachment.
createTransitGatewayRoute_transitGatewayAttachmentId :: Lens.Lens' CreateTransitGatewayRoute (Prelude.Maybe Prelude.Text)
createTransitGatewayRoute_transitGatewayAttachmentId = Lens.lens (\CreateTransitGatewayRoute' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@CreateTransitGatewayRoute' {} a -> s {transitGatewayAttachmentId = a} :: CreateTransitGatewayRoute)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTransitGatewayRoute_dryRun :: Lens.Lens' CreateTransitGatewayRoute (Prelude.Maybe Prelude.Bool)
createTransitGatewayRoute_dryRun = Lens.lens (\CreateTransitGatewayRoute' {dryRun} -> dryRun) (\s@CreateTransitGatewayRoute' {} a -> s {dryRun = a} :: CreateTransitGatewayRoute)

-- | Indicates whether to drop traffic that matches this route.
createTransitGatewayRoute_blackhole :: Lens.Lens' CreateTransitGatewayRoute (Prelude.Maybe Prelude.Bool)
createTransitGatewayRoute_blackhole = Lens.lens (\CreateTransitGatewayRoute' {blackhole} -> blackhole) (\s@CreateTransitGatewayRoute' {} a -> s {blackhole = a} :: CreateTransitGatewayRoute)

-- | The CIDR range used for destination matches. Routing decisions are based
-- on the most specific match.
createTransitGatewayRoute_destinationCidrBlock :: Lens.Lens' CreateTransitGatewayRoute Prelude.Text
createTransitGatewayRoute_destinationCidrBlock = Lens.lens (\CreateTransitGatewayRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@CreateTransitGatewayRoute' {} a -> s {destinationCidrBlock = a} :: CreateTransitGatewayRoute)

-- | The ID of the transit gateway route table.
createTransitGatewayRoute_transitGatewayRouteTableId :: Lens.Lens' CreateTransitGatewayRoute Prelude.Text
createTransitGatewayRoute_transitGatewayRouteTableId = Lens.lens (\CreateTransitGatewayRoute' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@CreateTransitGatewayRoute' {} a -> s {transitGatewayRouteTableId = a} :: CreateTransitGatewayRoute)

instance Core.AWSRequest CreateTransitGatewayRoute where
  type
    AWSResponse CreateTransitGatewayRoute =
      CreateTransitGatewayRouteResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTransitGatewayRouteResponse'
            Prelude.<$> (x Data..@? "route")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTransitGatewayRoute where
  hashWithSalt _salt CreateTransitGatewayRoute' {..} =
    _salt
      `Prelude.hashWithSalt` transitGatewayAttachmentId
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` blackhole
      `Prelude.hashWithSalt` destinationCidrBlock
      `Prelude.hashWithSalt` transitGatewayRouteTableId

instance Prelude.NFData CreateTransitGatewayRoute where
  rnf CreateTransitGatewayRoute' {..} =
    Prelude.rnf transitGatewayAttachmentId
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf blackhole
      `Prelude.seq` Prelude.rnf destinationCidrBlock
      `Prelude.seq` Prelude.rnf transitGatewayRouteTableId

instance Data.ToHeaders CreateTransitGatewayRoute where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateTransitGatewayRoute where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateTransitGatewayRoute where
  toQuery CreateTransitGatewayRoute' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateTransitGatewayRoute" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "TransitGatewayAttachmentId"
          Data.=: transitGatewayAttachmentId,
        "DryRun" Data.=: dryRun,
        "Blackhole" Data.=: blackhole,
        "DestinationCidrBlock" Data.=: destinationCidrBlock,
        "TransitGatewayRouteTableId"
          Data.=: transitGatewayRouteTableId
      ]

-- | /See:/ 'newCreateTransitGatewayRouteResponse' smart constructor.
data CreateTransitGatewayRouteResponse = CreateTransitGatewayRouteResponse'
  { -- | Information about the route.
    route :: Prelude.Maybe TransitGatewayRoute,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'route', 'createTransitGatewayRouteResponse_route' - Information about the route.
--
-- 'httpStatus', 'createTransitGatewayRouteResponse_httpStatus' - The response's http status code.
newCreateTransitGatewayRouteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTransitGatewayRouteResponse
newCreateTransitGatewayRouteResponse pHttpStatus_ =
  CreateTransitGatewayRouteResponse'
    { route =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the route.
createTransitGatewayRouteResponse_route :: Lens.Lens' CreateTransitGatewayRouteResponse (Prelude.Maybe TransitGatewayRoute)
createTransitGatewayRouteResponse_route = Lens.lens (\CreateTransitGatewayRouteResponse' {route} -> route) (\s@CreateTransitGatewayRouteResponse' {} a -> s {route = a} :: CreateTransitGatewayRouteResponse)

-- | The response's http status code.
createTransitGatewayRouteResponse_httpStatus :: Lens.Lens' CreateTransitGatewayRouteResponse Prelude.Int
createTransitGatewayRouteResponse_httpStatus = Lens.lens (\CreateTransitGatewayRouteResponse' {httpStatus} -> httpStatus) (\s@CreateTransitGatewayRouteResponse' {} a -> s {httpStatus = a} :: CreateTransitGatewayRouteResponse)

instance
  Prelude.NFData
    CreateTransitGatewayRouteResponse
  where
  rnf CreateTransitGatewayRouteResponse' {..} =
    Prelude.rnf route
      `Prelude.seq` Prelude.rnf httpStatus
