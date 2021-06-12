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
-- Module      : Network.AWS.EC2.CreateTransitGatewayRoute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a static route for the specified transit gateway route table.
module Network.AWS.EC2.CreateTransitGatewayRoute
  ( -- * Creating a Request
    CreateTransitGatewayRoute (..),
    newCreateTransitGatewayRoute,

    -- * Request Lenses
    createTransitGatewayRoute_dryRun,
    createTransitGatewayRoute_blackhole,
    createTransitGatewayRoute_transitGatewayAttachmentId,
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateTransitGatewayRoute' smart constructor.
data CreateTransitGatewayRoute = CreateTransitGatewayRoute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | Indicates whether to drop traffic that matches this route.
    blackhole :: Core.Maybe Core.Bool,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Core.Maybe Core.Text,
    -- | The CIDR range used for destination matches. Routing decisions are based
    -- on the most specific match.
    destinationCidrBlock :: Core.Text,
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTransitGatewayRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createTransitGatewayRoute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'blackhole', 'createTransitGatewayRoute_blackhole' - Indicates whether to drop traffic that matches this route.
--
-- 'transitGatewayAttachmentId', 'createTransitGatewayRoute_transitGatewayAttachmentId' - The ID of the attachment.
--
-- 'destinationCidrBlock', 'createTransitGatewayRoute_destinationCidrBlock' - The CIDR range used for destination matches. Routing decisions are based
-- on the most specific match.
--
-- 'transitGatewayRouteTableId', 'createTransitGatewayRoute_transitGatewayRouteTableId' - The ID of the transit gateway route table.
newCreateTransitGatewayRoute ::
  -- | 'destinationCidrBlock'
  Core.Text ->
  -- | 'transitGatewayRouteTableId'
  Core.Text ->
  CreateTransitGatewayRoute
newCreateTransitGatewayRoute
  pDestinationCidrBlock_
  pTransitGatewayRouteTableId_ =
    CreateTransitGatewayRoute'
      { dryRun = Core.Nothing,
        blackhole = Core.Nothing,
        transitGatewayAttachmentId = Core.Nothing,
        destinationCidrBlock = pDestinationCidrBlock_,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTransitGatewayRoute_dryRun :: Lens.Lens' CreateTransitGatewayRoute (Core.Maybe Core.Bool)
createTransitGatewayRoute_dryRun = Lens.lens (\CreateTransitGatewayRoute' {dryRun} -> dryRun) (\s@CreateTransitGatewayRoute' {} a -> s {dryRun = a} :: CreateTransitGatewayRoute)

-- | Indicates whether to drop traffic that matches this route.
createTransitGatewayRoute_blackhole :: Lens.Lens' CreateTransitGatewayRoute (Core.Maybe Core.Bool)
createTransitGatewayRoute_blackhole = Lens.lens (\CreateTransitGatewayRoute' {blackhole} -> blackhole) (\s@CreateTransitGatewayRoute' {} a -> s {blackhole = a} :: CreateTransitGatewayRoute)

-- | The ID of the attachment.
createTransitGatewayRoute_transitGatewayAttachmentId :: Lens.Lens' CreateTransitGatewayRoute (Core.Maybe Core.Text)
createTransitGatewayRoute_transitGatewayAttachmentId = Lens.lens (\CreateTransitGatewayRoute' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@CreateTransitGatewayRoute' {} a -> s {transitGatewayAttachmentId = a} :: CreateTransitGatewayRoute)

-- | The CIDR range used for destination matches. Routing decisions are based
-- on the most specific match.
createTransitGatewayRoute_destinationCidrBlock :: Lens.Lens' CreateTransitGatewayRoute Core.Text
createTransitGatewayRoute_destinationCidrBlock = Lens.lens (\CreateTransitGatewayRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@CreateTransitGatewayRoute' {} a -> s {destinationCidrBlock = a} :: CreateTransitGatewayRoute)

-- | The ID of the transit gateway route table.
createTransitGatewayRoute_transitGatewayRouteTableId :: Lens.Lens' CreateTransitGatewayRoute Core.Text
createTransitGatewayRoute_transitGatewayRouteTableId = Lens.lens (\CreateTransitGatewayRoute' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@CreateTransitGatewayRoute' {} a -> s {transitGatewayRouteTableId = a} :: CreateTransitGatewayRoute)

instance Core.AWSRequest CreateTransitGatewayRoute where
  type
    AWSResponse CreateTransitGatewayRoute =
      CreateTransitGatewayRouteResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTransitGatewayRouteResponse'
            Core.<$> (x Core..@? "route")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateTransitGatewayRoute

instance Core.NFData CreateTransitGatewayRoute

instance Core.ToHeaders CreateTransitGatewayRoute where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateTransitGatewayRoute where
  toPath = Core.const "/"

instance Core.ToQuery CreateTransitGatewayRoute where
  toQuery CreateTransitGatewayRoute' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateTransitGatewayRoute" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "Blackhole" Core.=: blackhole,
        "TransitGatewayAttachmentId"
          Core.=: transitGatewayAttachmentId,
        "DestinationCidrBlock" Core.=: destinationCidrBlock,
        "TransitGatewayRouteTableId"
          Core.=: transitGatewayRouteTableId
      ]

-- | /See:/ 'newCreateTransitGatewayRouteResponse' smart constructor.
data CreateTransitGatewayRouteResponse = CreateTransitGatewayRouteResponse'
  { -- | Information about the route.
    route :: Core.Maybe TransitGatewayRoute,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateTransitGatewayRouteResponse
newCreateTransitGatewayRouteResponse pHttpStatus_ =
  CreateTransitGatewayRouteResponse'
    { route =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the route.
createTransitGatewayRouteResponse_route :: Lens.Lens' CreateTransitGatewayRouteResponse (Core.Maybe TransitGatewayRoute)
createTransitGatewayRouteResponse_route = Lens.lens (\CreateTransitGatewayRouteResponse' {route} -> route) (\s@CreateTransitGatewayRouteResponse' {} a -> s {route = a} :: CreateTransitGatewayRouteResponse)

-- | The response's http status code.
createTransitGatewayRouteResponse_httpStatus :: Lens.Lens' CreateTransitGatewayRouteResponse Core.Int
createTransitGatewayRouteResponse_httpStatus = Lens.lens (\CreateTransitGatewayRouteResponse' {httpStatus} -> httpStatus) (\s@CreateTransitGatewayRouteResponse' {} a -> s {httpStatus = a} :: CreateTransitGatewayRouteResponse)

instance
  Core.NFData
    CreateTransitGatewayRouteResponse
