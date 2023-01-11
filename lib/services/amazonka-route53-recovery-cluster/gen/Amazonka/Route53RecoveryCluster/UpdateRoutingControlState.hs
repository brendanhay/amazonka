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
-- Module      : Amazonka.Route53RecoveryCluster.UpdateRoutingControlState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the state of the routing control to reroute traffic. You can set the
-- value to be On or Off. When the state is On, traffic flows to a cell.
-- When the state is Off, traffic does not flow.
--
-- With Route 53 ARC, you can add safety rules for routing controls, which
-- are safeguards for routing control state updates that help prevent
-- unexpected outcomes, like fail open traffic routing. However, there are
-- scenarios when you might want to bypass the routing control safeguards
-- that are enforced with safety rules that you\'ve configured. For
-- example, you might want to fail over quickly for disaster recovery, and
-- one or more safety rules might be unexpectedly preventing you from
-- updating a routing control state to reroute traffic. In a \"break
-- glass\" scenario like this, you can override one or more safety rules to
-- change a routing control state and fail over your application.
--
-- The @SafetyRulesToOverride@ property enables you override one or more
-- safety rules and update routing control states. For more information,
-- see
-- <https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.override-safety-rule.html Override safety rules to reroute traffic>
-- in the Amazon Route 53 Application Recovery Controller Developer Guide.
--
-- /You must specify Regional endpoints when you work with API cluster
-- operations to get or update routing control states in Route 53 ARC./
--
-- To see a code example for getting a routing control state, including
-- accessing Regional cluster endpoints in sequence, see
-- <https://docs.aws.amazon.com/r53recovery/latest/dg/service_code_examples_actions.html API examples>
-- in the Amazon Route 53 Application Recovery Controller Developer Guide.
--
-- -   <https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.update.html Viewing and updating routing control states>
--
-- -   <https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.html Working with routing controls overall>
module Amazonka.Route53RecoveryCluster.UpdateRoutingControlState
  ( -- * Creating a Request
    UpdateRoutingControlState (..),
    newUpdateRoutingControlState,

    -- * Request Lenses
    updateRoutingControlState_safetyRulesToOverride,
    updateRoutingControlState_routingControlArn,
    updateRoutingControlState_routingControlState,

    -- * Destructuring the Response
    UpdateRoutingControlStateResponse (..),
    newUpdateRoutingControlStateResponse,

    -- * Response Lenses
    updateRoutingControlStateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryCluster.Types

-- | /See:/ 'newUpdateRoutingControlState' smart constructor.
data UpdateRoutingControlState = UpdateRoutingControlState'
  { -- | The Amazon Resource Names (ARNs) for the safety rules that you want to
    -- override when you\'re updating the state of a routing control. You can
    -- override one safety rule or multiple safety rules by including one or
    -- more ARNs, separated by commas.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.override-safety-rule.html Override safety rules to reroute traffic>
    -- in the Amazon Route 53 Application Recovery Controller Developer Guide.
    safetyRulesToOverride :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) for the routing control that you want to
    -- update the state for.
    routingControlArn :: Prelude.Text,
    -- | The state of the routing control. You can set the value to be On or Off.
    routingControlState :: RoutingControlState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoutingControlState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'safetyRulesToOverride', 'updateRoutingControlState_safetyRulesToOverride' - The Amazon Resource Names (ARNs) for the safety rules that you want to
-- override when you\'re updating the state of a routing control. You can
-- override one safety rule or multiple safety rules by including one or
-- more ARNs, separated by commas.
--
-- For more information, see
-- <https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.override-safety-rule.html Override safety rules to reroute traffic>
-- in the Amazon Route 53 Application Recovery Controller Developer Guide.
--
-- 'routingControlArn', 'updateRoutingControlState_routingControlArn' - The Amazon Resource Name (ARN) for the routing control that you want to
-- update the state for.
--
-- 'routingControlState', 'updateRoutingControlState_routingControlState' - The state of the routing control. You can set the value to be On or Off.
newUpdateRoutingControlState ::
  -- | 'routingControlArn'
  Prelude.Text ->
  -- | 'routingControlState'
  RoutingControlState ->
  UpdateRoutingControlState
newUpdateRoutingControlState
  pRoutingControlArn_
  pRoutingControlState_ =
    UpdateRoutingControlState'
      { safetyRulesToOverride =
          Prelude.Nothing,
        routingControlArn = pRoutingControlArn_,
        routingControlState = pRoutingControlState_
      }

-- | The Amazon Resource Names (ARNs) for the safety rules that you want to
-- override when you\'re updating the state of a routing control. You can
-- override one safety rule or multiple safety rules by including one or
-- more ARNs, separated by commas.
--
-- For more information, see
-- <https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.override-safety-rule.html Override safety rules to reroute traffic>
-- in the Amazon Route 53 Application Recovery Controller Developer Guide.
updateRoutingControlState_safetyRulesToOverride :: Lens.Lens' UpdateRoutingControlState (Prelude.Maybe [Prelude.Text])
updateRoutingControlState_safetyRulesToOverride = Lens.lens (\UpdateRoutingControlState' {safetyRulesToOverride} -> safetyRulesToOverride) (\s@UpdateRoutingControlState' {} a -> s {safetyRulesToOverride = a} :: UpdateRoutingControlState) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) for the routing control that you want to
-- update the state for.
updateRoutingControlState_routingControlArn :: Lens.Lens' UpdateRoutingControlState Prelude.Text
updateRoutingControlState_routingControlArn = Lens.lens (\UpdateRoutingControlState' {routingControlArn} -> routingControlArn) (\s@UpdateRoutingControlState' {} a -> s {routingControlArn = a} :: UpdateRoutingControlState)

-- | The state of the routing control. You can set the value to be On or Off.
updateRoutingControlState_routingControlState :: Lens.Lens' UpdateRoutingControlState RoutingControlState
updateRoutingControlState_routingControlState = Lens.lens (\UpdateRoutingControlState' {routingControlState} -> routingControlState) (\s@UpdateRoutingControlState' {} a -> s {routingControlState = a} :: UpdateRoutingControlState)

instance Core.AWSRequest UpdateRoutingControlState where
  type
    AWSResponse UpdateRoutingControlState =
      UpdateRoutingControlStateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateRoutingControlStateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRoutingControlState where
  hashWithSalt _salt UpdateRoutingControlState' {..} =
    _salt `Prelude.hashWithSalt` safetyRulesToOverride
      `Prelude.hashWithSalt` routingControlArn
      `Prelude.hashWithSalt` routingControlState

instance Prelude.NFData UpdateRoutingControlState where
  rnf UpdateRoutingControlState' {..} =
    Prelude.rnf safetyRulesToOverride
      `Prelude.seq` Prelude.rnf routingControlArn
      `Prelude.seq` Prelude.rnf routingControlState

instance Data.ToHeaders UpdateRoutingControlState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ToggleCustomerAPI.UpdateRoutingControlState" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRoutingControlState where
  toJSON UpdateRoutingControlState' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SafetyRulesToOverride" Data..=)
              Prelude.<$> safetyRulesToOverride,
            Prelude.Just
              ("RoutingControlArn" Data..= routingControlArn),
            Prelude.Just
              ("RoutingControlState" Data..= routingControlState)
          ]
      )

instance Data.ToPath UpdateRoutingControlState where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateRoutingControlState where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRoutingControlStateResponse' smart constructor.
data UpdateRoutingControlStateResponse = UpdateRoutingControlStateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoutingControlStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateRoutingControlStateResponse_httpStatus' - The response's http status code.
newUpdateRoutingControlStateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRoutingControlStateResponse
newUpdateRoutingControlStateResponse pHttpStatus_ =
  UpdateRoutingControlStateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateRoutingControlStateResponse_httpStatus :: Lens.Lens' UpdateRoutingControlStateResponse Prelude.Int
updateRoutingControlStateResponse_httpStatus = Lens.lens (\UpdateRoutingControlStateResponse' {httpStatus} -> httpStatus) (\s@UpdateRoutingControlStateResponse' {} a -> s {httpStatus = a} :: UpdateRoutingControlStateResponse)

instance
  Prelude.NFData
    UpdateRoutingControlStateResponse
  where
  rnf UpdateRoutingControlStateResponse' {..} =
    Prelude.rnf httpStatus
