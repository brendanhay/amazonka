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
-- Module      : Network.AWS.EC2.EnableVgwRoutePropagation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a virtual private gateway (VGW) to propagate routes to the
-- specified route table of a VPC.
module Network.AWS.EC2.EnableVgwRoutePropagation
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest EnableVgwRoutePropagation where
  type
    Rs EnableVgwRoutePropagation =
      EnableVgwRoutePropagationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      EnableVgwRoutePropagationResponse'

instance Prelude.Hashable EnableVgwRoutePropagation

instance Prelude.NFData EnableVgwRoutePropagation

instance Prelude.ToHeaders EnableVgwRoutePropagation where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath EnableVgwRoutePropagation where
  toPath = Prelude.const "/"

instance Prelude.ToQuery EnableVgwRoutePropagation where
  toQuery EnableVgwRoutePropagation' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("EnableVgwRoutePropagation" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "GatewayId" Prelude.=: gatewayId,
        "RouteTableId" Prelude.=: routeTableId
      ]

-- | /See:/ 'newEnableVgwRoutePropagationResponse' smart constructor.
data EnableVgwRoutePropagationResponse = EnableVgwRoutePropagationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
