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
-- Module      : Amazonka.IoTSiteWise.UpdateDashboard
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an IoT SiteWise Monitor dashboard.
module Amazonka.IoTSiteWise.UpdateDashboard
  ( -- * Creating a Request
    UpdateDashboard (..),
    newUpdateDashboard,

    -- * Request Lenses
    updateDashboard_dashboardDescription,
    updateDashboard_clientToken,
    updateDashboard_dashboardId,
    updateDashboard_dashboardName,
    updateDashboard_dashboardDefinition,

    -- * Destructuring the Response
    UpdateDashboardResponse (..),
    newUpdateDashboardResponse,

    -- * Response Lenses
    updateDashboardResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDashboard' smart constructor.
data UpdateDashboard = UpdateDashboard'
  { -- | A new description for the dashboard.
    dashboardDescription :: Prelude.Maybe Prelude.Text,
    -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the dashboard to update.
    dashboardId :: Prelude.Text,
    -- | A new friendly name for the dashboard.
    dashboardName :: Prelude.Text,
    -- | The new dashboard definition, as specified in a JSON literal. For
    -- detailed information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/create-dashboards-using-aws-cli.html Creating dashboards (CLI)>
    -- in the /IoT SiteWise User Guide/.
    dashboardDefinition :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDashboard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashboardDescription', 'updateDashboard_dashboardDescription' - A new description for the dashboard.
--
-- 'clientToken', 'updateDashboard_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'dashboardId', 'updateDashboard_dashboardId' - The ID of the dashboard to update.
--
-- 'dashboardName', 'updateDashboard_dashboardName' - A new friendly name for the dashboard.
--
-- 'dashboardDefinition', 'updateDashboard_dashboardDefinition' - The new dashboard definition, as specified in a JSON literal. For
-- detailed information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/create-dashboards-using-aws-cli.html Creating dashboards (CLI)>
-- in the /IoT SiteWise User Guide/.
newUpdateDashboard ::
  -- | 'dashboardId'
  Prelude.Text ->
  -- | 'dashboardName'
  Prelude.Text ->
  -- | 'dashboardDefinition'
  Prelude.Text ->
  UpdateDashboard
newUpdateDashboard
  pDashboardId_
  pDashboardName_
  pDashboardDefinition_ =
    UpdateDashboard'
      { dashboardDescription =
          Prelude.Nothing,
        clientToken = Prelude.Nothing,
        dashboardId = pDashboardId_,
        dashboardName = pDashboardName_,
        dashboardDefinition = pDashboardDefinition_
      }

-- | A new description for the dashboard.
updateDashboard_dashboardDescription :: Lens.Lens' UpdateDashboard (Prelude.Maybe Prelude.Text)
updateDashboard_dashboardDescription = Lens.lens (\UpdateDashboard' {dashboardDescription} -> dashboardDescription) (\s@UpdateDashboard' {} a -> s {dashboardDescription = a} :: UpdateDashboard)

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
updateDashboard_clientToken :: Lens.Lens' UpdateDashboard (Prelude.Maybe Prelude.Text)
updateDashboard_clientToken = Lens.lens (\UpdateDashboard' {clientToken} -> clientToken) (\s@UpdateDashboard' {} a -> s {clientToken = a} :: UpdateDashboard)

-- | The ID of the dashboard to update.
updateDashboard_dashboardId :: Lens.Lens' UpdateDashboard Prelude.Text
updateDashboard_dashboardId = Lens.lens (\UpdateDashboard' {dashboardId} -> dashboardId) (\s@UpdateDashboard' {} a -> s {dashboardId = a} :: UpdateDashboard)

-- | A new friendly name for the dashboard.
updateDashboard_dashboardName :: Lens.Lens' UpdateDashboard Prelude.Text
updateDashboard_dashboardName = Lens.lens (\UpdateDashboard' {dashboardName} -> dashboardName) (\s@UpdateDashboard' {} a -> s {dashboardName = a} :: UpdateDashboard)

-- | The new dashboard definition, as specified in a JSON literal. For
-- detailed information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/create-dashboards-using-aws-cli.html Creating dashboards (CLI)>
-- in the /IoT SiteWise User Guide/.
updateDashboard_dashboardDefinition :: Lens.Lens' UpdateDashboard Prelude.Text
updateDashboard_dashboardDefinition = Lens.lens (\UpdateDashboard' {dashboardDefinition} -> dashboardDefinition) (\s@UpdateDashboard' {} a -> s {dashboardDefinition = a} :: UpdateDashboard)

instance Core.AWSRequest UpdateDashboard where
  type
    AWSResponse UpdateDashboard =
      UpdateDashboardResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDashboardResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDashboard where
  hashWithSalt _salt UpdateDashboard' {..} =
    _salt `Prelude.hashWithSalt` dashboardDescription
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` dashboardId
      `Prelude.hashWithSalt` dashboardName
      `Prelude.hashWithSalt` dashboardDefinition

instance Prelude.NFData UpdateDashboard where
  rnf UpdateDashboard' {..} =
    Prelude.rnf dashboardDescription
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf dashboardId
      `Prelude.seq` Prelude.rnf dashboardName
      `Prelude.seq` Prelude.rnf dashboardDefinition

instance Core.ToHeaders UpdateDashboard where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDashboard where
  toJSON UpdateDashboard' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("dashboardDescription" Core..=)
              Prelude.<$> dashboardDescription,
            ("clientToken" Core..=) Prelude.<$> clientToken,
            Prelude.Just ("dashboardName" Core..= dashboardName),
            Prelude.Just
              ("dashboardDefinition" Core..= dashboardDefinition)
          ]
      )

instance Core.ToPath UpdateDashboard where
  toPath UpdateDashboard' {..} =
    Prelude.mconcat
      ["/dashboards/", Core.toBS dashboardId]

instance Core.ToQuery UpdateDashboard where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDashboardResponse' smart constructor.
data UpdateDashboardResponse = UpdateDashboardResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDashboardResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDashboardResponse_httpStatus' - The response's http status code.
newUpdateDashboardResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDashboardResponse
newUpdateDashboardResponse pHttpStatus_ =
  UpdateDashboardResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateDashboardResponse_httpStatus :: Lens.Lens' UpdateDashboardResponse Prelude.Int
updateDashboardResponse_httpStatus = Lens.lens (\UpdateDashboardResponse' {httpStatus} -> httpStatus) (\s@UpdateDashboardResponse' {} a -> s {httpStatus = a} :: UpdateDashboardResponse)

instance Prelude.NFData UpdateDashboardResponse where
  rnf UpdateDashboardResponse' {..} =
    Prelude.rnf httpStatus
