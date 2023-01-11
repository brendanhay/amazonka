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
-- Module      : Amazonka.Route53RecoveryCluster.GetRoutingControlState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the state for a routing control. A routing control is a simple
-- on\/off switch that you can use to route traffic to cells. When a
-- routing control state is On, traffic flows to a cell. When the state is
-- Off, traffic does not flow.
--
-- Before you can create a routing control, you must first create a
-- cluster, and then host the control in a control panel on the cluster.
-- For more information, see
-- <https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.create.html Create routing control structures>
-- in the Amazon Route 53 Application Recovery Controller Developer Guide.
-- You access one of the endpoints for the cluster to get or update the
-- routing control state to redirect traffic for your application.
--
-- /You must specify Regional endpoints when you work with API cluster
-- operations to get or update routing control states in Route 53 ARC./
--
-- To see a code example for getting a routing control state, including
-- accessing Regional cluster endpoints in sequence, see
-- <https://docs.aws.amazon.com/r53recovery/latest/dg/service_code_examples_actions.html API examples>
-- in the Amazon Route 53 Application Recovery Controller Developer Guide.
--
-- Learn more about working with routing controls in the following topics
-- in the Amazon Route 53 Application Recovery Controller Developer Guide:
--
-- -   <https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.update.html Viewing and updating routing control states>
--
-- -   <https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.html Working with routing controls in Route 53 ARC>
module Amazonka.Route53RecoveryCluster.GetRoutingControlState
  ( -- * Creating a Request
    GetRoutingControlState (..),
    newGetRoutingControlState,

    -- * Request Lenses
    getRoutingControlState_routingControlArn,

    -- * Destructuring the Response
    GetRoutingControlStateResponse (..),
    newGetRoutingControlStateResponse,

    -- * Response Lenses
    getRoutingControlStateResponse_routingControlName,
    getRoutingControlStateResponse_httpStatus,
    getRoutingControlStateResponse_routingControlArn,
    getRoutingControlStateResponse_routingControlState,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryCluster.Types

-- | /See:/ 'newGetRoutingControlState' smart constructor.
data GetRoutingControlState = GetRoutingControlState'
  { -- | The Amazon Resource Name (ARN) for the routing control that you want to
    -- get the state for.
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
-- 'routingControlArn', 'getRoutingControlState_routingControlArn' - The Amazon Resource Name (ARN) for the routing control that you want to
-- get the state for.
newGetRoutingControlState ::
  -- | 'routingControlArn'
  Prelude.Text ->
  GetRoutingControlState
newGetRoutingControlState pRoutingControlArn_ =
  GetRoutingControlState'
    { routingControlArn =
        pRoutingControlArn_
    }

-- | The Amazon Resource Name (ARN) for the routing control that you want to
-- get the state for.
getRoutingControlState_routingControlArn :: Lens.Lens' GetRoutingControlState Prelude.Text
getRoutingControlState_routingControlArn = Lens.lens (\GetRoutingControlState' {routingControlArn} -> routingControlArn) (\s@GetRoutingControlState' {} a -> s {routingControlArn = a} :: GetRoutingControlState)

instance Core.AWSRequest GetRoutingControlState where
  type
    AWSResponse GetRoutingControlState =
      GetRoutingControlStateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRoutingControlStateResponse'
            Prelude.<$> (x Data..?> "RoutingControlName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "RoutingControlArn")
            Prelude.<*> (x Data..:> "RoutingControlState")
      )

instance Prelude.Hashable GetRoutingControlState where
  hashWithSalt _salt GetRoutingControlState' {..} =
    _salt `Prelude.hashWithSalt` routingControlArn

instance Prelude.NFData GetRoutingControlState where
  rnf GetRoutingControlState' {..} =
    Prelude.rnf routingControlArn

instance Data.ToHeaders GetRoutingControlState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ToggleCustomerAPI.GetRoutingControlState" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRoutingControlState where
  toJSON GetRoutingControlState' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RoutingControlArn" Data..= routingControlArn)
          ]
      )

instance Data.ToPath GetRoutingControlState where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRoutingControlState where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRoutingControlStateResponse' smart constructor.
data GetRoutingControlStateResponse = GetRoutingControlStateResponse'
  { -- | The routing control name.
    routingControlName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the response.
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
-- 'routingControlName', 'getRoutingControlStateResponse_routingControlName' - The routing control name.
--
-- 'httpStatus', 'getRoutingControlStateResponse_httpStatus' - The response's http status code.
--
-- 'routingControlArn', 'getRoutingControlStateResponse_routingControlArn' - The Amazon Resource Name (ARN) of the response.
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
      { routingControlName =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        routingControlArn = pRoutingControlArn_,
        routingControlState = pRoutingControlState_
      }

-- | The routing control name.
getRoutingControlStateResponse_routingControlName :: Lens.Lens' GetRoutingControlStateResponse (Prelude.Maybe Prelude.Text)
getRoutingControlStateResponse_routingControlName = Lens.lens (\GetRoutingControlStateResponse' {routingControlName} -> routingControlName) (\s@GetRoutingControlStateResponse' {} a -> s {routingControlName = a} :: GetRoutingControlStateResponse)

-- | The response's http status code.
getRoutingControlStateResponse_httpStatus :: Lens.Lens' GetRoutingControlStateResponse Prelude.Int
getRoutingControlStateResponse_httpStatus = Lens.lens (\GetRoutingControlStateResponse' {httpStatus} -> httpStatus) (\s@GetRoutingControlStateResponse' {} a -> s {httpStatus = a} :: GetRoutingControlStateResponse)

-- | The Amazon Resource Name (ARN) of the response.
getRoutingControlStateResponse_routingControlArn :: Lens.Lens' GetRoutingControlStateResponse Prelude.Text
getRoutingControlStateResponse_routingControlArn = Lens.lens (\GetRoutingControlStateResponse' {routingControlArn} -> routingControlArn) (\s@GetRoutingControlStateResponse' {} a -> s {routingControlArn = a} :: GetRoutingControlStateResponse)

-- | The state of the routing control.
getRoutingControlStateResponse_routingControlState :: Lens.Lens' GetRoutingControlStateResponse RoutingControlState
getRoutingControlStateResponse_routingControlState = Lens.lens (\GetRoutingControlStateResponse' {routingControlState} -> routingControlState) (\s@GetRoutingControlStateResponse' {} a -> s {routingControlState = a} :: GetRoutingControlStateResponse)

instance
  Prelude.NFData
    GetRoutingControlStateResponse
  where
  rnf GetRoutingControlStateResponse' {..} =
    Prelude.rnf routingControlName
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf routingControlArn
      `Prelude.seq` Prelude.rnf routingControlState
