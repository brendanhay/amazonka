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
-- Module      : Amazonka.EC2.EnableVgwRoutePropagation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a virtual private gateway (VGW) to propagate routes to the
-- specified route table of a VPC.
module Amazonka.EC2.EnableVgwRoutePropagation
  ( -- * Creating a Request
    EnableVgwRoutePropagation (..),
    newEnableVgwRoutePropagation,

    -- * Request Lenses
    enableVgwRoutePropagation_dryRun,
    enableVgwRoutePropagation_gatewayId,
    enableVgwRoutePropagation_routeTableId,

    -- * Destructuring the Response
    EnableVgwRoutePropagationResponse (..),
    newEnableVgwRoutePropagationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for EnableVgwRoutePropagation.
--
-- /See:/ 'newEnableVgwRoutePropagation' smart constructor.
data EnableVgwRoutePropagation = EnableVgwRoutePropagation'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the virtual private gateway that is attached to a VPC. The
    -- virtual private gateway must be attached to the same VPC that the
    -- routing tables are associated with.
    gatewayId :: Prelude.Text,
    -- | The ID of the route table. The routing table must be associated with the
    -- same VPC that the virtual private gateway is attached to.
    routeTableId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableVgwRoutePropagation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'enableVgwRoutePropagation_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'gatewayId', 'enableVgwRoutePropagation_gatewayId' - The ID of the virtual private gateway that is attached to a VPC. The
-- virtual private gateway must be attached to the same VPC that the
-- routing tables are associated with.
--
-- 'routeTableId', 'enableVgwRoutePropagation_routeTableId' - The ID of the route table. The routing table must be associated with the
-- same VPC that the virtual private gateway is attached to.
newEnableVgwRoutePropagation ::
  -- | 'gatewayId'
  Prelude.Text ->
  -- | 'routeTableId'
  Prelude.Text ->
  EnableVgwRoutePropagation
newEnableVgwRoutePropagation
  pGatewayId_
  pRouteTableId_ =
    EnableVgwRoutePropagation'
      { dryRun =
          Prelude.Nothing,
        gatewayId = pGatewayId_,
        routeTableId = pRouteTableId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
enableVgwRoutePropagation_dryRun :: Lens.Lens' EnableVgwRoutePropagation (Prelude.Maybe Prelude.Bool)
enableVgwRoutePropagation_dryRun = Lens.lens (\EnableVgwRoutePropagation' {dryRun} -> dryRun) (\s@EnableVgwRoutePropagation' {} a -> s {dryRun = a} :: EnableVgwRoutePropagation)

-- | The ID of the virtual private gateway that is attached to a VPC. The
-- virtual private gateway must be attached to the same VPC that the
-- routing tables are associated with.
enableVgwRoutePropagation_gatewayId :: Lens.Lens' EnableVgwRoutePropagation Prelude.Text
enableVgwRoutePropagation_gatewayId = Lens.lens (\EnableVgwRoutePropagation' {gatewayId} -> gatewayId) (\s@EnableVgwRoutePropagation' {} a -> s {gatewayId = a} :: EnableVgwRoutePropagation)

-- | The ID of the route table. The routing table must be associated with the
-- same VPC that the virtual private gateway is attached to.
enableVgwRoutePropagation_routeTableId :: Lens.Lens' EnableVgwRoutePropagation Prelude.Text
enableVgwRoutePropagation_routeTableId = Lens.lens (\EnableVgwRoutePropagation' {routeTableId} -> routeTableId) (\s@EnableVgwRoutePropagation' {} a -> s {routeTableId = a} :: EnableVgwRoutePropagation)

instance Core.AWSRequest EnableVgwRoutePropagation where
  type
    AWSResponse EnableVgwRoutePropagation =
      EnableVgwRoutePropagationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      EnableVgwRoutePropagationResponse'

instance Prelude.Hashable EnableVgwRoutePropagation where
  hashWithSalt _salt EnableVgwRoutePropagation' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` gatewayId
      `Prelude.hashWithSalt` routeTableId

instance Prelude.NFData EnableVgwRoutePropagation where
  rnf EnableVgwRoutePropagation' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf gatewayId
      `Prelude.seq` Prelude.rnf routeTableId

instance Data.ToHeaders EnableVgwRoutePropagation where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath EnableVgwRoutePropagation where
  toPath = Prelude.const "/"

instance Data.ToQuery EnableVgwRoutePropagation where
  toQuery EnableVgwRoutePropagation' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("EnableVgwRoutePropagation" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "GatewayId" Data.=: gatewayId,
        "RouteTableId" Data.=: routeTableId
      ]

-- | /See:/ 'newEnableVgwRoutePropagationResponse' smart constructor.
data EnableVgwRoutePropagationResponse = EnableVgwRoutePropagationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableVgwRoutePropagationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableVgwRoutePropagationResponse ::
  EnableVgwRoutePropagationResponse
newEnableVgwRoutePropagationResponse =
  EnableVgwRoutePropagationResponse'

instance
  Prelude.NFData
    EnableVgwRoutePropagationResponse
  where
  rnf _ = ()
