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
-- Module      : Amazonka.Route53RecoveryControlConfig.UpdateControlPanel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a control panel. The only update you can make to a control panel
-- is to change the name of the control panel.
module Amazonka.Route53RecoveryControlConfig.UpdateControlPanel
  ( -- * Creating a Request
    UpdateControlPanel (..),
    newUpdateControlPanel,

    -- * Request Lenses
    updateControlPanel_controlPanelArn,
    updateControlPanel_controlPanelName,

    -- * Destructuring the Response
    UpdateControlPanelResponse (..),
    newUpdateControlPanelResponse,

    -- * Response Lenses
    updateControlPanelResponse_controlPanel,
    updateControlPanelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryControlConfig.Types

-- | The details of the control panel that you\'re updating.
--
-- /See:/ 'newUpdateControlPanel' smart constructor.
data UpdateControlPanel = UpdateControlPanel'
  { -- | The Amazon Resource Name (ARN) of the control panel.
    controlPanelArn :: Prelude.Text,
    -- | The name of the control panel.
    controlPanelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateControlPanel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlPanelArn', 'updateControlPanel_controlPanelArn' - The Amazon Resource Name (ARN) of the control panel.
--
-- 'controlPanelName', 'updateControlPanel_controlPanelName' - The name of the control panel.
newUpdateControlPanel ::
  -- | 'controlPanelArn'
  Prelude.Text ->
  -- | 'controlPanelName'
  Prelude.Text ->
  UpdateControlPanel
newUpdateControlPanel
  pControlPanelArn_
  pControlPanelName_ =
    UpdateControlPanel'
      { controlPanelArn =
          pControlPanelArn_,
        controlPanelName = pControlPanelName_
      }

-- | The Amazon Resource Name (ARN) of the control panel.
updateControlPanel_controlPanelArn :: Lens.Lens' UpdateControlPanel Prelude.Text
updateControlPanel_controlPanelArn = Lens.lens (\UpdateControlPanel' {controlPanelArn} -> controlPanelArn) (\s@UpdateControlPanel' {} a -> s {controlPanelArn = a} :: UpdateControlPanel)

-- | The name of the control panel.
updateControlPanel_controlPanelName :: Lens.Lens' UpdateControlPanel Prelude.Text
updateControlPanel_controlPanelName = Lens.lens (\UpdateControlPanel' {controlPanelName} -> controlPanelName) (\s@UpdateControlPanel' {} a -> s {controlPanelName = a} :: UpdateControlPanel)

instance Core.AWSRequest UpdateControlPanel where
  type
    AWSResponse UpdateControlPanel =
      UpdateControlPanelResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateControlPanelResponse'
            Prelude.<$> (x Core..?> "ControlPanel")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateControlPanel where
  hashWithSalt _salt UpdateControlPanel' {..} =
    _salt `Prelude.hashWithSalt` controlPanelArn
      `Prelude.hashWithSalt` controlPanelName

instance Prelude.NFData UpdateControlPanel where
  rnf UpdateControlPanel' {..} =
    Prelude.rnf controlPanelArn
      `Prelude.seq` Prelude.rnf controlPanelName

instance Core.ToHeaders UpdateControlPanel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateControlPanel where
  toJSON UpdateControlPanel' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ControlPanelArn" Core..= controlPanelArn),
            Prelude.Just
              ("ControlPanelName" Core..= controlPanelName)
          ]
      )

instance Core.ToPath UpdateControlPanel where
  toPath = Prelude.const "/controlpanel"

instance Core.ToQuery UpdateControlPanel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateControlPanelResponse' smart constructor.
data UpdateControlPanelResponse = UpdateControlPanelResponse'
  { -- | The control panel to update.
    controlPanel :: Prelude.Maybe ControlPanel,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateControlPanelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlPanel', 'updateControlPanelResponse_controlPanel' - The control panel to update.
--
-- 'httpStatus', 'updateControlPanelResponse_httpStatus' - The response's http status code.
newUpdateControlPanelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateControlPanelResponse
newUpdateControlPanelResponse pHttpStatus_ =
  UpdateControlPanelResponse'
    { controlPanel =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The control panel to update.
updateControlPanelResponse_controlPanel :: Lens.Lens' UpdateControlPanelResponse (Prelude.Maybe ControlPanel)
updateControlPanelResponse_controlPanel = Lens.lens (\UpdateControlPanelResponse' {controlPanel} -> controlPanel) (\s@UpdateControlPanelResponse' {} a -> s {controlPanel = a} :: UpdateControlPanelResponse)

-- | The response's http status code.
updateControlPanelResponse_httpStatus :: Lens.Lens' UpdateControlPanelResponse Prelude.Int
updateControlPanelResponse_httpStatus = Lens.lens (\UpdateControlPanelResponse' {httpStatus} -> httpStatus) (\s@UpdateControlPanelResponse' {} a -> s {httpStatus = a} :: UpdateControlPanelResponse)

instance Prelude.NFData UpdateControlPanelResponse where
  rnf UpdateControlPanelResponse' {..} =
    Prelude.rnf controlPanel
      `Prelude.seq` Prelude.rnf httpStatus
