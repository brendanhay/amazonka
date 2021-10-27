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
-- Module      : Network.AWS.Route53RecoveryControlConfig.CreateRoutingControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new routing control.
--
-- A routing control has one of two states: ON and OFF. You can map the
-- routing control state to the state of an Amazon Route 53 health check,
-- which can be used to control traffic routing.
--
-- To get or update the routing control state, see the Recovery Cluster
-- (data plane) API actions for Amazon Route 53 Application Recovery
-- Controller.
module Network.AWS.Route53RecoveryControlConfig.CreateRoutingControl
  ( -- * Creating a Request
    CreateRoutingControl (..),
    newCreateRoutingControl,

    -- * Request Lenses
    createRoutingControl_controlPanelArn,
    createRoutingControl_clientToken,
    createRoutingControl_clusterArn,
    createRoutingControl_routingControlName,

    -- * Destructuring the Response
    CreateRoutingControlResponse (..),
    newCreateRoutingControlResponse,

    -- * Response Lenses
    createRoutingControlResponse_routingControl,
    createRoutingControlResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53RecoveryControlConfig.Types

-- | The details of the routing control that you\'re creating.
--
-- /See:/ 'newCreateRoutingControl' smart constructor.
data CreateRoutingControl = CreateRoutingControl'
  { -- | The Amazon Resource Name (ARN) of the control panel that includes the
    -- routing control.
    controlPanelArn :: Prelude.Maybe Prelude.Text,
    -- | Unique client idempotency token.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the cluster that includes the routing
    -- control.
    clusterArn :: Prelude.Text,
    -- | The name of the routing control.
    routingControlName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRoutingControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlPanelArn', 'createRoutingControl_controlPanelArn' - The Amazon Resource Name (ARN) of the control panel that includes the
-- routing control.
--
-- 'clientToken', 'createRoutingControl_clientToken' - Unique client idempotency token.
--
-- 'clusterArn', 'createRoutingControl_clusterArn' - The Amazon Resource Name (ARN) of the cluster that includes the routing
-- control.
--
-- 'routingControlName', 'createRoutingControl_routingControlName' - The name of the routing control.
newCreateRoutingControl ::
  -- | 'clusterArn'
  Prelude.Text ->
  -- | 'routingControlName'
  Prelude.Text ->
  CreateRoutingControl
newCreateRoutingControl
  pClusterArn_
  pRoutingControlName_ =
    CreateRoutingControl'
      { controlPanelArn =
          Prelude.Nothing,
        clientToken = Prelude.Nothing,
        clusterArn = pClusterArn_,
        routingControlName = pRoutingControlName_
      }

-- | The Amazon Resource Name (ARN) of the control panel that includes the
-- routing control.
createRoutingControl_controlPanelArn :: Lens.Lens' CreateRoutingControl (Prelude.Maybe Prelude.Text)
createRoutingControl_controlPanelArn = Lens.lens (\CreateRoutingControl' {controlPanelArn} -> controlPanelArn) (\s@CreateRoutingControl' {} a -> s {controlPanelArn = a} :: CreateRoutingControl)

-- | Unique client idempotency token.
createRoutingControl_clientToken :: Lens.Lens' CreateRoutingControl (Prelude.Maybe Prelude.Text)
createRoutingControl_clientToken = Lens.lens (\CreateRoutingControl' {clientToken} -> clientToken) (\s@CreateRoutingControl' {} a -> s {clientToken = a} :: CreateRoutingControl)

-- | The Amazon Resource Name (ARN) of the cluster that includes the routing
-- control.
createRoutingControl_clusterArn :: Lens.Lens' CreateRoutingControl Prelude.Text
createRoutingControl_clusterArn = Lens.lens (\CreateRoutingControl' {clusterArn} -> clusterArn) (\s@CreateRoutingControl' {} a -> s {clusterArn = a} :: CreateRoutingControl)

-- | The name of the routing control.
createRoutingControl_routingControlName :: Lens.Lens' CreateRoutingControl Prelude.Text
createRoutingControl_routingControlName = Lens.lens (\CreateRoutingControl' {routingControlName} -> routingControlName) (\s@CreateRoutingControl' {} a -> s {routingControlName = a} :: CreateRoutingControl)

instance Core.AWSRequest CreateRoutingControl where
  type
    AWSResponse CreateRoutingControl =
      CreateRoutingControlResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRoutingControlResponse'
            Prelude.<$> (x Core..?> "RoutingControl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRoutingControl

instance Prelude.NFData CreateRoutingControl

instance Core.ToHeaders CreateRoutingControl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateRoutingControl where
  toJSON CreateRoutingControl' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ControlPanelArn" Core..=)
              Prelude.<$> controlPanelArn,
            ("ClientToken" Core..=) Prelude.<$> clientToken,
            Prelude.Just ("ClusterArn" Core..= clusterArn),
            Prelude.Just
              ("RoutingControlName" Core..= routingControlName)
          ]
      )

instance Core.ToPath CreateRoutingControl where
  toPath = Prelude.const "/routingcontrol"

instance Core.ToQuery CreateRoutingControl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRoutingControlResponse' smart constructor.
data CreateRoutingControlResponse = CreateRoutingControlResponse'
  { -- | The routing control that is created.
    routingControl :: Prelude.Maybe RoutingControl,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRoutingControlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routingControl', 'createRoutingControlResponse_routingControl' - The routing control that is created.
--
-- 'httpStatus', 'createRoutingControlResponse_httpStatus' - The response's http status code.
newCreateRoutingControlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRoutingControlResponse
newCreateRoutingControlResponse pHttpStatus_ =
  CreateRoutingControlResponse'
    { routingControl =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The routing control that is created.
createRoutingControlResponse_routingControl :: Lens.Lens' CreateRoutingControlResponse (Prelude.Maybe RoutingControl)
createRoutingControlResponse_routingControl = Lens.lens (\CreateRoutingControlResponse' {routingControl} -> routingControl) (\s@CreateRoutingControlResponse' {} a -> s {routingControl = a} :: CreateRoutingControlResponse)

-- | The response's http status code.
createRoutingControlResponse_httpStatus :: Lens.Lens' CreateRoutingControlResponse Prelude.Int
createRoutingControlResponse_httpStatus = Lens.lens (\CreateRoutingControlResponse' {httpStatus} -> httpStatus) (\s@CreateRoutingControlResponse' {} a -> s {httpStatus = a} :: CreateRoutingControlResponse)

instance Prelude.NFData CreateRoutingControlResponse
