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
-- Module      : Amazonka.CostExplorer.UpdateAnomalyMonitor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing cost anomaly monitor. The changes made are applied
-- going forward, and doesn\'t change anomalies detected in the past.
module Amazonka.CostExplorer.UpdateAnomalyMonitor
  ( -- * Creating a Request
    UpdateAnomalyMonitor (..),
    newUpdateAnomalyMonitor,

    -- * Request Lenses
    updateAnomalyMonitor_monitorName,
    updateAnomalyMonitor_monitorArn,

    -- * Destructuring the Response
    UpdateAnomalyMonitorResponse (..),
    newUpdateAnomalyMonitorResponse,

    -- * Response Lenses
    updateAnomalyMonitorResponse_httpStatus,
    updateAnomalyMonitorResponse_monitorArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAnomalyMonitor' smart constructor.
data UpdateAnomalyMonitor = UpdateAnomalyMonitor'
  { -- | The new name for the cost anomaly monitor.
    monitorName :: Prelude.Maybe Prelude.Text,
    -- | Cost anomaly monitor Amazon Resource Names (ARNs).
    monitorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAnomalyMonitor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitorName', 'updateAnomalyMonitor_monitorName' - The new name for the cost anomaly monitor.
--
-- 'monitorArn', 'updateAnomalyMonitor_monitorArn' - Cost anomaly monitor Amazon Resource Names (ARNs).
newUpdateAnomalyMonitor ::
  -- | 'monitorArn'
  Prelude.Text ->
  UpdateAnomalyMonitor
newUpdateAnomalyMonitor pMonitorArn_ =
  UpdateAnomalyMonitor'
    { monitorName =
        Prelude.Nothing,
      monitorArn = pMonitorArn_
    }

-- | The new name for the cost anomaly monitor.
updateAnomalyMonitor_monitorName :: Lens.Lens' UpdateAnomalyMonitor (Prelude.Maybe Prelude.Text)
updateAnomalyMonitor_monitorName = Lens.lens (\UpdateAnomalyMonitor' {monitorName} -> monitorName) (\s@UpdateAnomalyMonitor' {} a -> s {monitorName = a} :: UpdateAnomalyMonitor)

-- | Cost anomaly monitor Amazon Resource Names (ARNs).
updateAnomalyMonitor_monitorArn :: Lens.Lens' UpdateAnomalyMonitor Prelude.Text
updateAnomalyMonitor_monitorArn = Lens.lens (\UpdateAnomalyMonitor' {monitorArn} -> monitorArn) (\s@UpdateAnomalyMonitor' {} a -> s {monitorArn = a} :: UpdateAnomalyMonitor)

instance Core.AWSRequest UpdateAnomalyMonitor where
  type
    AWSResponse UpdateAnomalyMonitor =
      UpdateAnomalyMonitorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAnomalyMonitorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "MonitorArn")
      )

instance Prelude.Hashable UpdateAnomalyMonitor where
  hashWithSalt _salt UpdateAnomalyMonitor' {..} =
    _salt `Prelude.hashWithSalt` monitorName
      `Prelude.hashWithSalt` monitorArn

instance Prelude.NFData UpdateAnomalyMonitor where
  rnf UpdateAnomalyMonitor' {..} =
    Prelude.rnf monitorName
      `Prelude.seq` Prelude.rnf monitorArn

instance Core.ToHeaders UpdateAnomalyMonitor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.UpdateAnomalyMonitor" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateAnomalyMonitor where
  toJSON UpdateAnomalyMonitor' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MonitorName" Core..=) Prelude.<$> monitorName,
            Prelude.Just ("MonitorArn" Core..= monitorArn)
          ]
      )

instance Core.ToPath UpdateAnomalyMonitor where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateAnomalyMonitor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAnomalyMonitorResponse' smart constructor.
data UpdateAnomalyMonitorResponse = UpdateAnomalyMonitorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A cost anomaly monitor ARN.
    monitorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAnomalyMonitorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAnomalyMonitorResponse_httpStatus' - The response's http status code.
--
-- 'monitorArn', 'updateAnomalyMonitorResponse_monitorArn' - A cost anomaly monitor ARN.
newUpdateAnomalyMonitorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'monitorArn'
  Prelude.Text ->
  UpdateAnomalyMonitorResponse
newUpdateAnomalyMonitorResponse
  pHttpStatus_
  pMonitorArn_ =
    UpdateAnomalyMonitorResponse'
      { httpStatus =
          pHttpStatus_,
        monitorArn = pMonitorArn_
      }

-- | The response's http status code.
updateAnomalyMonitorResponse_httpStatus :: Lens.Lens' UpdateAnomalyMonitorResponse Prelude.Int
updateAnomalyMonitorResponse_httpStatus = Lens.lens (\UpdateAnomalyMonitorResponse' {httpStatus} -> httpStatus) (\s@UpdateAnomalyMonitorResponse' {} a -> s {httpStatus = a} :: UpdateAnomalyMonitorResponse)

-- | A cost anomaly monitor ARN.
updateAnomalyMonitorResponse_monitorArn :: Lens.Lens' UpdateAnomalyMonitorResponse Prelude.Text
updateAnomalyMonitorResponse_monitorArn = Lens.lens (\UpdateAnomalyMonitorResponse' {monitorArn} -> monitorArn) (\s@UpdateAnomalyMonitorResponse' {} a -> s {monitorArn = a} :: UpdateAnomalyMonitorResponse)

instance Prelude.NFData UpdateAnomalyMonitorResponse where
  rnf UpdateAnomalyMonitorResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf monitorArn
