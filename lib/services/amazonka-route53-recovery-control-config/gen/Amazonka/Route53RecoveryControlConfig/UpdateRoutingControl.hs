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
-- Module      : Amazonka.Route53RecoveryControlConfig.UpdateRoutingControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a routing control. You can only update the name of the routing
-- control. To get or update the routing control state, see the Recovery
-- Cluster (data plane) API actions for Amazon Route 53 Application
-- Recovery Controller.
module Amazonka.Route53RecoveryControlConfig.UpdateRoutingControl
  ( -- * Creating a Request
    UpdateRoutingControl (..),
    newUpdateRoutingControl,

    -- * Request Lenses
    updateRoutingControl_routingControlName,
    updateRoutingControl_routingControlArn,

    -- * Destructuring the Response
    UpdateRoutingControlResponse (..),
    newUpdateRoutingControlResponse,

    -- * Response Lenses
    updateRoutingControlResponse_routingControl,
    updateRoutingControlResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryControlConfig.Types

-- | The details of the routing control that you\'re updating.
--
-- /See:/ 'newUpdateRoutingControl' smart constructor.
data UpdateRoutingControl = UpdateRoutingControl'
  { -- | The name of the routing control.
    routingControlName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the routing control.
    routingControlArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoutingControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routingControlName', 'updateRoutingControl_routingControlName' - The name of the routing control.
--
-- 'routingControlArn', 'updateRoutingControl_routingControlArn' - The Amazon Resource Name (ARN) of the routing control.
newUpdateRoutingControl ::
  -- | 'routingControlName'
  Prelude.Text ->
  -- | 'routingControlArn'
  Prelude.Text ->
  UpdateRoutingControl
newUpdateRoutingControl
  pRoutingControlName_
  pRoutingControlArn_ =
    UpdateRoutingControl'
      { routingControlName =
          pRoutingControlName_,
        routingControlArn = pRoutingControlArn_
      }

-- | The name of the routing control.
updateRoutingControl_routingControlName :: Lens.Lens' UpdateRoutingControl Prelude.Text
updateRoutingControl_routingControlName = Lens.lens (\UpdateRoutingControl' {routingControlName} -> routingControlName) (\s@UpdateRoutingControl' {} a -> s {routingControlName = a} :: UpdateRoutingControl)

-- | The Amazon Resource Name (ARN) of the routing control.
updateRoutingControl_routingControlArn :: Lens.Lens' UpdateRoutingControl Prelude.Text
updateRoutingControl_routingControlArn = Lens.lens (\UpdateRoutingControl' {routingControlArn} -> routingControlArn) (\s@UpdateRoutingControl' {} a -> s {routingControlArn = a} :: UpdateRoutingControl)

instance Core.AWSRequest UpdateRoutingControl where
  type
    AWSResponse UpdateRoutingControl =
      UpdateRoutingControlResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRoutingControlResponse'
            Prelude.<$> (x Data..?> "RoutingControl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRoutingControl where
  hashWithSalt _salt UpdateRoutingControl' {..} =
    _salt `Prelude.hashWithSalt` routingControlName
      `Prelude.hashWithSalt` routingControlArn

instance Prelude.NFData UpdateRoutingControl where
  rnf UpdateRoutingControl' {..} =
    Prelude.rnf routingControlName
      `Prelude.seq` Prelude.rnf routingControlArn

instance Data.ToHeaders UpdateRoutingControl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRoutingControl where
  toJSON UpdateRoutingControl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RoutingControlName" Data..= routingControlName),
            Prelude.Just
              ("RoutingControlArn" Data..= routingControlArn)
          ]
      )

instance Data.ToPath UpdateRoutingControl where
  toPath = Prelude.const "/routingcontrol"

instance Data.ToQuery UpdateRoutingControl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRoutingControlResponse' smart constructor.
data UpdateRoutingControlResponse = UpdateRoutingControlResponse'
  { -- | The routing control that was updated.
    routingControl :: Prelude.Maybe RoutingControl,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoutingControlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routingControl', 'updateRoutingControlResponse_routingControl' - The routing control that was updated.
--
-- 'httpStatus', 'updateRoutingControlResponse_httpStatus' - The response's http status code.
newUpdateRoutingControlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRoutingControlResponse
newUpdateRoutingControlResponse pHttpStatus_ =
  UpdateRoutingControlResponse'
    { routingControl =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The routing control that was updated.
updateRoutingControlResponse_routingControl :: Lens.Lens' UpdateRoutingControlResponse (Prelude.Maybe RoutingControl)
updateRoutingControlResponse_routingControl = Lens.lens (\UpdateRoutingControlResponse' {routingControl} -> routingControl) (\s@UpdateRoutingControlResponse' {} a -> s {routingControl = a} :: UpdateRoutingControlResponse)

-- | The response's http status code.
updateRoutingControlResponse_httpStatus :: Lens.Lens' UpdateRoutingControlResponse Prelude.Int
updateRoutingControlResponse_httpStatus = Lens.lens (\UpdateRoutingControlResponse' {httpStatus} -> httpStatus) (\s@UpdateRoutingControlResponse' {} a -> s {httpStatus = a} :: UpdateRoutingControlResponse)

instance Prelude.NFData UpdateRoutingControlResponse where
  rnf UpdateRoutingControlResponse' {..} =
    Prelude.rnf routingControl
      `Prelude.seq` Prelude.rnf httpStatus
