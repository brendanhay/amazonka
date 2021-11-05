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
-- Module      : Network.AWS.Route53RecoveryCluster.GetRoutingControlState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the state for a routing control. A routing control is a simple
-- on\/off switch that you can use to route traffic to cells. When the
-- state is On, traffic flows to a cell. When it\'s off, traffic does not
-- flow.
--
-- Before you can create a routing control, you first must create a cluster
-- to host the control. For more information, see
-- <https://docs.aws.amazon.com/recovery-cluster/latest/api/cluster.html CreateCluster>.
-- Access one of the endpoints for the cluster to get or update the routing
-- control state to redirect traffic.
--
-- For more information about working with routing controls, see
-- <https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.html Routing control>
-- in the Route 53 Application Recovery Controller Developer Guide.
module Network.AWS.Route53RecoveryCluster.GetRoutingControlState
  ( -- * Creating a Request
    GetRoutingControlState (..),
    newGetRoutingControlState,

    -- * Request Lenses
    getRoutingControlState_routingControlArn,

    -- * Destructuring the Response
    GetRoutingControlStateResponse (..),
    newGetRoutingControlStateResponse,

    -- * Response Lenses
    getRoutingControlStateResponse_httpStatus,
    getRoutingControlStateResponse_routingControlArn,
    getRoutingControlStateResponse_routingControlState,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53RecoveryCluster.Types

-- | /See:/ 'newGetRoutingControlState' smart constructor.
data GetRoutingControlState = GetRoutingControlState'
  { -- | The Amazon Resource Number (ARN) for the routing control that you want
    -- to get the state for.
    routingControlArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRoutingControlState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routingControlArn', 'getRoutingControlState_routingControlArn' - The Amazon Resource Number (ARN) for the routing control that you want
-- to get the state for.
newGetRoutingControlState ::
  -- | 'routingControlArn'
  Prelude.Text ->
  GetRoutingControlState
newGetRoutingControlState pRoutingControlArn_ =
  GetRoutingControlState'
    { routingControlArn =
        pRoutingControlArn_
    }

-- | The Amazon Resource Number (ARN) for the routing control that you want
-- to get the state for.
getRoutingControlState_routingControlArn :: Lens.Lens' GetRoutingControlState Prelude.Text
getRoutingControlState_routingControlArn = Lens.lens (\GetRoutingControlState' {routingControlArn} -> routingControlArn) (\s@GetRoutingControlState' {} a -> s {routingControlArn = a} :: GetRoutingControlState)

instance Core.AWSRequest GetRoutingControlState where
  type
    AWSResponse GetRoutingControlState =
      GetRoutingControlStateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRoutingControlStateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "RoutingControlArn")
            Prelude.<*> (x Core..:> "RoutingControlState")
      )

instance Prelude.Hashable GetRoutingControlState

instance Prelude.NFData GetRoutingControlState

instance Core.ToHeaders GetRoutingControlState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ToggleCustomerAPI.GetRoutingControlState" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetRoutingControlState where
  toJSON GetRoutingControlState' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RoutingControlArn" Core..= routingControlArn)
          ]
      )

instance Core.ToPath GetRoutingControlState where
  toPath = Prelude.const "/"

instance Core.ToQuery GetRoutingControlState where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRoutingControlStateResponse' smart constructor.
data GetRoutingControlStateResponse = GetRoutingControlStateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Number (ARN) of the response.
    routingControlArn :: Prelude.Text,
    -- | The state of the routing control.
    routingControlState :: RoutingControlState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRoutingControlStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getRoutingControlStateResponse_httpStatus' - The response's http status code.
--
-- 'routingControlArn', 'getRoutingControlStateResponse_routingControlArn' - The Amazon Resource Number (ARN) of the response.
--
-- 'routingControlState', 'getRoutingControlStateResponse_routingControlState' - The state of the routing control.
newGetRoutingControlStateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'routingControlArn'
  Prelude.Text ->
  -- | 'routingControlState'
  RoutingControlState ->
  GetRoutingControlStateResponse
newGetRoutingControlStateResponse
  pHttpStatus_
  pRoutingControlArn_
  pRoutingControlState_ =
    GetRoutingControlStateResponse'
      { httpStatus =
          pHttpStatus_,
        routingControlArn = pRoutingControlArn_,
        routingControlState = pRoutingControlState_
      }

-- | The response's http status code.
getRoutingControlStateResponse_httpStatus :: Lens.Lens' GetRoutingControlStateResponse Prelude.Int
getRoutingControlStateResponse_httpStatus = Lens.lens (\GetRoutingControlStateResponse' {httpStatus} -> httpStatus) (\s@GetRoutingControlStateResponse' {} a -> s {httpStatus = a} :: GetRoutingControlStateResponse)

-- | The Amazon Resource Number (ARN) of the response.
getRoutingControlStateResponse_routingControlArn :: Lens.Lens' GetRoutingControlStateResponse Prelude.Text
getRoutingControlStateResponse_routingControlArn = Lens.lens (\GetRoutingControlStateResponse' {routingControlArn} -> routingControlArn) (\s@GetRoutingControlStateResponse' {} a -> s {routingControlArn = a} :: GetRoutingControlStateResponse)

-- | The state of the routing control.
getRoutingControlStateResponse_routingControlState :: Lens.Lens' GetRoutingControlStateResponse RoutingControlState
getRoutingControlStateResponse_routingControlState = Lens.lens (\GetRoutingControlStateResponse' {routingControlState} -> routingControlState) (\s@GetRoutingControlStateResponse' {} a -> s {routingControlState = a} :: GetRoutingControlStateResponse)

instance
  Prelude.NFData
    GetRoutingControlStateResponse
