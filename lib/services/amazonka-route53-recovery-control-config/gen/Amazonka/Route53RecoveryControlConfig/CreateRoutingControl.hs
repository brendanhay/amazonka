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
-- Module      : Amazonka.Route53RecoveryControlConfig.CreateRoutingControl
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Route53RecoveryControlConfig.CreateRoutingControl
  ( -- * Creating a Request
    CreateRoutingControl (..),
    newCreateRoutingControl,

    -- * Request Lenses
    createRoutingControl_clientToken,
    createRoutingControl_controlPanelArn,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryControlConfig.Types

-- | The details of the routing control that you\'re creating.
--
-- /See:/ 'newCreateRoutingControl' smart constructor.
data CreateRoutingControl = CreateRoutingControl'
  { -- | A unique, case-sensitive string of up to 64 ASCII characters. To make an
    -- idempotent API request with an action, specify a client token in the
    -- request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the control panel that includes the
    -- routing control.
    controlPanelArn :: Prelude.Maybe Prelude.Text,
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
-- 'clientToken', 'createRoutingControl_clientToken' - A unique, case-sensitive string of up to 64 ASCII characters. To make an
-- idempotent API request with an action, specify a client token in the
-- request.
--
-- 'controlPanelArn', 'createRoutingControl_controlPanelArn' - The Amazon Resource Name (ARN) of the control panel that includes the
-- routing control.
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
      { clientToken =
          Prelude.Nothing,
        controlPanelArn = Prelude.Nothing,
        clusterArn = pClusterArn_,
        routingControlName = pRoutingControlName_
      }

-- | A unique, case-sensitive string of up to 64 ASCII characters. To make an
-- idempotent API request with an action, specify a client token in the
-- request.
createRoutingControl_clientToken :: Lens.Lens' CreateRoutingControl (Prelude.Maybe Prelude.Text)
createRoutingControl_clientToken = Lens.lens (\CreateRoutingControl' {clientToken} -> clientToken) (\s@CreateRoutingControl' {} a -> s {clientToken = a} :: CreateRoutingControl)

-- | The Amazon Resource Name (ARN) of the control panel that includes the
-- routing control.
createRoutingControl_controlPanelArn :: Lens.Lens' CreateRoutingControl (Prelude.Maybe Prelude.Text)
createRoutingControl_controlPanelArn = Lens.lens (\CreateRoutingControl' {controlPanelArn} -> controlPanelArn) (\s@CreateRoutingControl' {} a -> s {controlPanelArn = a} :: CreateRoutingControl)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRoutingControlResponse'
            Prelude.<$> (x Data..?> "RoutingControl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRoutingControl where
  hashWithSalt _salt CreateRoutingControl' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` controlPanelArn
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` routingControlName

instance Prelude.NFData CreateRoutingControl where
  rnf CreateRoutingControl' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf controlPanelArn
      `Prelude.seq` Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf routingControlName

instance Data.ToHeaders CreateRoutingControl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateRoutingControl where
  toJSON CreateRoutingControl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("ControlPanelArn" Data..=)
              Prelude.<$> controlPanelArn,
            Prelude.Just ("ClusterArn" Data..= clusterArn),
            Prelude.Just
              ("RoutingControlName" Data..= routingControlName)
          ]
      )

instance Data.ToPath CreateRoutingControl where
  toPath = Prelude.const "/routingcontrol"

instance Data.ToQuery CreateRoutingControl where
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

instance Prelude.NFData CreateRoutingControlResponse where
  rnf CreateRoutingControlResponse' {..} =
    Prelude.rnf routingControl
      `Prelude.seq` Prelude.rnf httpStatus
