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
-- Module      : Amazonka.MediaConnect.UpdateBridgeState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the bridge state
module Amazonka.MediaConnect.UpdateBridgeState
  ( -- * Creating a Request
    UpdateBridgeState (..),
    newUpdateBridgeState,

    -- * Request Lenses
    updateBridgeState_bridgeArn,
    updateBridgeState_desiredState,

    -- * Destructuring the Response
    UpdateBridgeStateResponse (..),
    newUpdateBridgeStateResponse,

    -- * Response Lenses
    updateBridgeStateResponse_bridgeArn,
    updateBridgeStateResponse_desiredState,
    updateBridgeStateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to update the bridge state.
--
-- /See:/ 'newUpdateBridgeState' smart constructor.
data UpdateBridgeState = UpdateBridgeState'
  { -- | The ARN of the bridge that you want to update.
    bridgeArn :: Prelude.Text,
    desiredState :: DesiredState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBridgeState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bridgeArn', 'updateBridgeState_bridgeArn' - The ARN of the bridge that you want to update.
--
-- 'desiredState', 'updateBridgeState_desiredState' - Undocumented member.
newUpdateBridgeState ::
  -- | 'bridgeArn'
  Prelude.Text ->
  -- | 'desiredState'
  DesiredState ->
  UpdateBridgeState
newUpdateBridgeState pBridgeArn_ pDesiredState_ =
  UpdateBridgeState'
    { bridgeArn = pBridgeArn_,
      desiredState = pDesiredState_
    }

-- | The ARN of the bridge that you want to update.
updateBridgeState_bridgeArn :: Lens.Lens' UpdateBridgeState Prelude.Text
updateBridgeState_bridgeArn = Lens.lens (\UpdateBridgeState' {bridgeArn} -> bridgeArn) (\s@UpdateBridgeState' {} a -> s {bridgeArn = a} :: UpdateBridgeState)

-- | Undocumented member.
updateBridgeState_desiredState :: Lens.Lens' UpdateBridgeState DesiredState
updateBridgeState_desiredState = Lens.lens (\UpdateBridgeState' {desiredState} -> desiredState) (\s@UpdateBridgeState' {} a -> s {desiredState = a} :: UpdateBridgeState)

instance Core.AWSRequest UpdateBridgeState where
  type
    AWSResponse UpdateBridgeState =
      UpdateBridgeStateResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBridgeStateResponse'
            Prelude.<$> (x Data..?> "bridgeArn")
            Prelude.<*> (x Data..?> "desiredState")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBridgeState where
  hashWithSalt _salt UpdateBridgeState' {..} =
    _salt
      `Prelude.hashWithSalt` bridgeArn
      `Prelude.hashWithSalt` desiredState

instance Prelude.NFData UpdateBridgeState where
  rnf UpdateBridgeState' {..} =
    Prelude.rnf bridgeArn
      `Prelude.seq` Prelude.rnf desiredState

instance Data.ToHeaders UpdateBridgeState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateBridgeState where
  toJSON UpdateBridgeState' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("desiredState" Data..= desiredState)]
      )

instance Data.ToPath UpdateBridgeState where
  toPath UpdateBridgeState' {..} =
    Prelude.mconcat
      ["/v1/bridges/", Data.toBS bridgeArn, "/state"]

instance Data.ToQuery UpdateBridgeState where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBridgeStateResponse' smart constructor.
data UpdateBridgeStateResponse = UpdateBridgeStateResponse'
  { -- | The Amazon Resource Number (ARN) of the bridge.
    bridgeArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the bridge. ACTIVE or STANDBY.
    desiredState :: Prelude.Maybe DesiredState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBridgeStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bridgeArn', 'updateBridgeStateResponse_bridgeArn' - The Amazon Resource Number (ARN) of the bridge.
--
-- 'desiredState', 'updateBridgeStateResponse_desiredState' - The state of the bridge. ACTIVE or STANDBY.
--
-- 'httpStatus', 'updateBridgeStateResponse_httpStatus' - The response's http status code.
newUpdateBridgeStateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBridgeStateResponse
newUpdateBridgeStateResponse pHttpStatus_ =
  UpdateBridgeStateResponse'
    { bridgeArn =
        Prelude.Nothing,
      desiredState = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Number (ARN) of the bridge.
updateBridgeStateResponse_bridgeArn :: Lens.Lens' UpdateBridgeStateResponse (Prelude.Maybe Prelude.Text)
updateBridgeStateResponse_bridgeArn = Lens.lens (\UpdateBridgeStateResponse' {bridgeArn} -> bridgeArn) (\s@UpdateBridgeStateResponse' {} a -> s {bridgeArn = a} :: UpdateBridgeStateResponse)

-- | The state of the bridge. ACTIVE or STANDBY.
updateBridgeStateResponse_desiredState :: Lens.Lens' UpdateBridgeStateResponse (Prelude.Maybe DesiredState)
updateBridgeStateResponse_desiredState = Lens.lens (\UpdateBridgeStateResponse' {desiredState} -> desiredState) (\s@UpdateBridgeStateResponse' {} a -> s {desiredState = a} :: UpdateBridgeStateResponse)

-- | The response's http status code.
updateBridgeStateResponse_httpStatus :: Lens.Lens' UpdateBridgeStateResponse Prelude.Int
updateBridgeStateResponse_httpStatus = Lens.lens (\UpdateBridgeStateResponse' {httpStatus} -> httpStatus) (\s@UpdateBridgeStateResponse' {} a -> s {httpStatus = a} :: UpdateBridgeStateResponse)

instance Prelude.NFData UpdateBridgeStateResponse where
  rnf UpdateBridgeStateResponse' {..} =
    Prelude.rnf bridgeArn
      `Prelude.seq` Prelude.rnf desiredState
      `Prelude.seq` Prelude.rnf httpStatus
