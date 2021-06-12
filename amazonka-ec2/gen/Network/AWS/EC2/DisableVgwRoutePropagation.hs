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
-- Module      : Network.AWS.EC2.DisableVgwRoutePropagation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables a virtual private gateway (VGW) from propagating routes to a
-- specified route table of a VPC.
module Network.AWS.EC2.DisableVgwRoutePropagation
  ( -- * Creating a Request
    DisableVgwRoutePropagation (..),
    newDisableVgwRoutePropagation,

    -- * Request Lenses
    disableVgwRoutePropagation_dryRun,
    disableVgwRoutePropagation_gatewayId,
    disableVgwRoutePropagation_routeTableId,

    -- * Destructuring the Response
    DisableVgwRoutePropagationResponse (..),
    newDisableVgwRoutePropagationResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DisableVgwRoutePropagation.
--
-- /See:/ 'newDisableVgwRoutePropagation' smart constructor.
data DisableVgwRoutePropagation = DisableVgwRoutePropagation'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the virtual private gateway.
    gatewayId :: Core.Text,
    -- | The ID of the route table.
    routeTableId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableVgwRoutePropagation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'disableVgwRoutePropagation_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'gatewayId', 'disableVgwRoutePropagation_gatewayId' - The ID of the virtual private gateway.
--
-- 'routeTableId', 'disableVgwRoutePropagation_routeTableId' - The ID of the route table.
newDisableVgwRoutePropagation ::
  -- | 'gatewayId'
  Core.Text ->
  -- | 'routeTableId'
  Core.Text ->
  DisableVgwRoutePropagation
newDisableVgwRoutePropagation
  pGatewayId_
  pRouteTableId_ =
    DisableVgwRoutePropagation'
      { dryRun = Core.Nothing,
        gatewayId = pGatewayId_,
        routeTableId = pRouteTableId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
disableVgwRoutePropagation_dryRun :: Lens.Lens' DisableVgwRoutePropagation (Core.Maybe Core.Bool)
disableVgwRoutePropagation_dryRun = Lens.lens (\DisableVgwRoutePropagation' {dryRun} -> dryRun) (\s@DisableVgwRoutePropagation' {} a -> s {dryRun = a} :: DisableVgwRoutePropagation)

-- | The ID of the virtual private gateway.
disableVgwRoutePropagation_gatewayId :: Lens.Lens' DisableVgwRoutePropagation Core.Text
disableVgwRoutePropagation_gatewayId = Lens.lens (\DisableVgwRoutePropagation' {gatewayId} -> gatewayId) (\s@DisableVgwRoutePropagation' {} a -> s {gatewayId = a} :: DisableVgwRoutePropagation)

-- | The ID of the route table.
disableVgwRoutePropagation_routeTableId :: Lens.Lens' DisableVgwRoutePropagation Core.Text
disableVgwRoutePropagation_routeTableId = Lens.lens (\DisableVgwRoutePropagation' {routeTableId} -> routeTableId) (\s@DisableVgwRoutePropagation' {} a -> s {routeTableId = a} :: DisableVgwRoutePropagation)

instance Core.AWSRequest DisableVgwRoutePropagation where
  type
    AWSResponse DisableVgwRoutePropagation =
      DisableVgwRoutePropagationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DisableVgwRoutePropagationResponse'

instance Core.Hashable DisableVgwRoutePropagation

instance Core.NFData DisableVgwRoutePropagation

instance Core.ToHeaders DisableVgwRoutePropagation where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DisableVgwRoutePropagation where
  toPath = Core.const "/"

instance Core.ToQuery DisableVgwRoutePropagation where
  toQuery DisableVgwRoutePropagation' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DisableVgwRoutePropagation" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "GatewayId" Core.=: gatewayId,
        "RouteTableId" Core.=: routeTableId
      ]

-- | /See:/ 'newDisableVgwRoutePropagationResponse' smart constructor.
data DisableVgwRoutePropagationResponse = DisableVgwRoutePropagationResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableVgwRoutePropagationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableVgwRoutePropagationResponse ::
  DisableVgwRoutePropagationResponse
newDisableVgwRoutePropagationResponse =
  DisableVgwRoutePropagationResponse'

instance
  Core.NFData
    DisableVgwRoutePropagationResponse
