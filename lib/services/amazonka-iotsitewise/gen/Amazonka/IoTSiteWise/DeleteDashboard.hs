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
-- Module      : Amazonka.IoTSiteWise.DeleteDashboard
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a dashboard from IoT SiteWise Monitor.
module Amazonka.IoTSiteWise.DeleteDashboard
  ( -- * Creating a Request
    DeleteDashboard (..),
    newDeleteDashboard,

    -- * Request Lenses
    deleteDashboard_clientToken,
    deleteDashboard_dashboardId,

    -- * Destructuring the Response
    DeleteDashboardResponse (..),
    newDeleteDashboardResponse,

    -- * Response Lenses
    deleteDashboardResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDashboard' smart constructor.
data DeleteDashboard = DeleteDashboard'
  { -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the dashboard to delete.
    dashboardId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDashboard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteDashboard_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'dashboardId', 'deleteDashboard_dashboardId' - The ID of the dashboard to delete.
newDeleteDashboard ::
  -- | 'dashboardId'
  Prelude.Text ->
  DeleteDashboard
newDeleteDashboard pDashboardId_ =
  DeleteDashboard'
    { clientToken = Prelude.Nothing,
      dashboardId = pDashboardId_
    }

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
deleteDashboard_clientToken :: Lens.Lens' DeleteDashboard (Prelude.Maybe Prelude.Text)
deleteDashboard_clientToken = Lens.lens (\DeleteDashboard' {clientToken} -> clientToken) (\s@DeleteDashboard' {} a -> s {clientToken = a} :: DeleteDashboard)

-- | The ID of the dashboard to delete.
deleteDashboard_dashboardId :: Lens.Lens' DeleteDashboard Prelude.Text
deleteDashboard_dashboardId = Lens.lens (\DeleteDashboard' {dashboardId} -> dashboardId) (\s@DeleteDashboard' {} a -> s {dashboardId = a} :: DeleteDashboard)

instance Core.AWSRequest DeleteDashboard where
  type
    AWSResponse DeleteDashboard =
      DeleteDashboardResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDashboardResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDashboard where
  hashWithSalt _salt DeleteDashboard' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` dashboardId

instance Prelude.NFData DeleteDashboard where
  rnf DeleteDashboard' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf dashboardId

instance Data.ToHeaders DeleteDashboard where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteDashboard where
  toPath DeleteDashboard' {..} =
    Prelude.mconcat
      ["/dashboards/", Data.toBS dashboardId]

instance Data.ToQuery DeleteDashboard where
  toQuery DeleteDashboard' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newDeleteDashboardResponse' smart constructor.
data DeleteDashboardResponse = DeleteDashboardResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDashboardResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDashboardResponse_httpStatus' - The response's http status code.
newDeleteDashboardResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDashboardResponse
newDeleteDashboardResponse pHttpStatus_ =
  DeleteDashboardResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteDashboardResponse_httpStatus :: Lens.Lens' DeleteDashboardResponse Prelude.Int
deleteDashboardResponse_httpStatus = Lens.lens (\DeleteDashboardResponse' {httpStatus} -> httpStatus) (\s@DeleteDashboardResponse' {} a -> s {httpStatus = a} :: DeleteDashboardResponse)

instance Prelude.NFData DeleteDashboardResponse where
  rnf DeleteDashboardResponse' {..} =
    Prelude.rnf httpStatus
