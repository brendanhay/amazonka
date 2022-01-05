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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the state of the routing control to reroute traffic. You can set the
-- value to be On or Off. When the state is On, traffic flows to a cell.
-- When it\'s off, traffic does not flow.
--
-- For more information about working with routing controls, see
-- <https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.html Routing control>
-- in the Route 53 Application Recovery Controller Developer Guide.
module Amazonka.Route53RecoveryCluster.UpdateRoutingControlState
  ( -- * Creating a Request
    UpdateRoutingControlState (..),
    newUpdateRoutingControlState,

    -- * Request Lenses
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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryCluster.Types

-- | /See:/ 'newUpdateRoutingControlState' smart constructor.
data UpdateRoutingControlState = UpdateRoutingControlState'
  { -- | The Amazon Resource Number (ARN) for the routing control that you want
    -- to update the state for.
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
-- 'routingControlArn', 'updateRoutingControlState_routingControlArn' - The Amazon Resource Number (ARN) for the routing control that you want
-- to update the state for.
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
      { routingControlArn =
          pRoutingControlArn_,
        routingControlState = pRoutingControlState_
      }

-- | The Amazon Resource Number (ARN) for the routing control that you want
-- to update the state for.
updateRoutingControlState_routingControlArn :: Lens.Lens' UpdateRoutingControlState Prelude.Text
updateRoutingControlState_routingControlArn = Lens.lens (\UpdateRoutingControlState' {routingControlArn} -> routingControlArn) (\s@UpdateRoutingControlState' {} a -> s {routingControlArn = a} :: UpdateRoutingControlState)

-- | The state of the routing control. You can set the value to be On or Off.
updateRoutingControlState_routingControlState :: Lens.Lens' UpdateRoutingControlState RoutingControlState
updateRoutingControlState_routingControlState = Lens.lens (\UpdateRoutingControlState' {routingControlState} -> routingControlState) (\s@UpdateRoutingControlState' {} a -> s {routingControlState = a} :: UpdateRoutingControlState)

instance Core.AWSRequest UpdateRoutingControlState where
  type
    AWSResponse UpdateRoutingControlState =
      UpdateRoutingControlStateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateRoutingControlStateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRoutingControlState where
  hashWithSalt _salt UpdateRoutingControlState' {..} =
    _salt `Prelude.hashWithSalt` routingControlArn
      `Prelude.hashWithSalt` routingControlState

instance Prelude.NFData UpdateRoutingControlState where
  rnf UpdateRoutingControlState' {..} =
    Prelude.rnf routingControlArn
      `Prelude.seq` Prelude.rnf routingControlState

instance Core.ToHeaders UpdateRoutingControlState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ToggleCustomerAPI.UpdateRoutingControlState" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateRoutingControlState where
  toJSON UpdateRoutingControlState' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RoutingControlArn" Core..= routingControlArn),
            Prelude.Just
              ("RoutingControlState" Core..= routingControlState)
          ]
      )

instance Core.ToPath UpdateRoutingControlState where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateRoutingControlState where
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
