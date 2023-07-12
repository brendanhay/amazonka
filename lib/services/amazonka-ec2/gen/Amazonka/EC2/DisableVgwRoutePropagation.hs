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
-- Module      : Amazonka.EC2.DisableVgwRoutePropagation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables a virtual private gateway (VGW) from propagating routes to a
-- specified route table of a VPC.
module Amazonka.EC2.DisableVgwRoutePropagation
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DisableVgwRoutePropagation.
--
-- /See:/ 'newDisableVgwRoutePropagation' smart constructor.
data DisableVgwRoutePropagation = DisableVgwRoutePropagation'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the virtual private gateway.
    gatewayId :: Prelude.Text,
    -- | The ID of the route table.
    routeTableId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'routeTableId'
  Prelude.Text ->
  DisableVgwRoutePropagation
newDisableVgwRoutePropagation
  pGatewayId_
  pRouteTableId_ =
    DisableVgwRoutePropagation'
      { dryRun =
          Prelude.Nothing,
        gatewayId = pGatewayId_,
        routeTableId = pRouteTableId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
disableVgwRoutePropagation_dryRun :: Lens.Lens' DisableVgwRoutePropagation (Prelude.Maybe Prelude.Bool)
disableVgwRoutePropagation_dryRun = Lens.lens (\DisableVgwRoutePropagation' {dryRun} -> dryRun) (\s@DisableVgwRoutePropagation' {} a -> s {dryRun = a} :: DisableVgwRoutePropagation)

-- | The ID of the virtual private gateway.
disableVgwRoutePropagation_gatewayId :: Lens.Lens' DisableVgwRoutePropagation Prelude.Text
disableVgwRoutePropagation_gatewayId = Lens.lens (\DisableVgwRoutePropagation' {gatewayId} -> gatewayId) (\s@DisableVgwRoutePropagation' {} a -> s {gatewayId = a} :: DisableVgwRoutePropagation)

-- | The ID of the route table.
disableVgwRoutePropagation_routeTableId :: Lens.Lens' DisableVgwRoutePropagation Prelude.Text
disableVgwRoutePropagation_routeTableId = Lens.lens (\DisableVgwRoutePropagation' {routeTableId} -> routeTableId) (\s@DisableVgwRoutePropagation' {} a -> s {routeTableId = a} :: DisableVgwRoutePropagation)

instance Core.AWSRequest DisableVgwRoutePropagation where
  type
    AWSResponse DisableVgwRoutePropagation =
      DisableVgwRoutePropagationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DisableVgwRoutePropagationResponse'

instance Prelude.Hashable DisableVgwRoutePropagation where
  hashWithSalt _salt DisableVgwRoutePropagation' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` gatewayId
      `Prelude.hashWithSalt` routeTableId

instance Prelude.NFData DisableVgwRoutePropagation where
  rnf DisableVgwRoutePropagation' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf gatewayId
      `Prelude.seq` Prelude.rnf routeTableId

instance Data.ToHeaders DisableVgwRoutePropagation where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DisableVgwRoutePropagation where
  toPath = Prelude.const "/"

instance Data.ToQuery DisableVgwRoutePropagation where
  toQuery DisableVgwRoutePropagation' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DisableVgwRoutePropagation" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "GatewayId" Data.=: gatewayId,
        "RouteTableId" Data.=: routeTableId
      ]

-- | /See:/ 'newDisableVgwRoutePropagationResponse' smart constructor.
data DisableVgwRoutePropagationResponse = DisableVgwRoutePropagationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableVgwRoutePropagationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableVgwRoutePropagationResponse ::
  DisableVgwRoutePropagationResponse
newDisableVgwRoutePropagationResponse =
  DisableVgwRoutePropagationResponse'

instance
  Prelude.NFData
    DisableVgwRoutePropagationResponse
  where
  rnf _ = ()
