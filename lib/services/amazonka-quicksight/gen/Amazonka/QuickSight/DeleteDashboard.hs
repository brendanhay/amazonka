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
-- Module      : Amazonka.QuickSight.DeleteDashboard
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a dashboard.
module Amazonka.QuickSight.DeleteDashboard
  ( -- * Creating a Request
    DeleteDashboard (..),
    newDeleteDashboard,

    -- * Request Lenses
    deleteDashboard_versionNumber,
    deleteDashboard_awsAccountId,
    deleteDashboard_dashboardId,

    -- * Destructuring the Response
    DeleteDashboardResponse (..),
    newDeleteDashboardResponse,

    -- * Response Lenses
    deleteDashboardResponse_arn,
    deleteDashboardResponse_dashboardId,
    deleteDashboardResponse_requestId,
    deleteDashboardResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDashboard' smart constructor.
data DeleteDashboard = DeleteDashboard'
  { -- | The version number of the dashboard. If the version number property is
    -- provided, only the specified version of the dashboard is deleted.
    versionNumber :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the Amazon Web Services account that contains the dashboard
    -- that you\'re deleting.
    awsAccountId :: Prelude.Text,
    -- | The ID for the dashboard.
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
-- 'versionNumber', 'deleteDashboard_versionNumber' - The version number of the dashboard. If the version number property is
-- provided, only the specified version of the dashboard is deleted.
--
-- 'awsAccountId', 'deleteDashboard_awsAccountId' - The ID of the Amazon Web Services account that contains the dashboard
-- that you\'re deleting.
--
-- 'dashboardId', 'deleteDashboard_dashboardId' - The ID for the dashboard.
newDeleteDashboard ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dashboardId'
  Prelude.Text ->
  DeleteDashboard
newDeleteDashboard pAwsAccountId_ pDashboardId_ =
  DeleteDashboard'
    { versionNumber = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      dashboardId = pDashboardId_
    }

-- | The version number of the dashboard. If the version number property is
-- provided, only the specified version of the dashboard is deleted.
deleteDashboard_versionNumber :: Lens.Lens' DeleteDashboard (Prelude.Maybe Prelude.Natural)
deleteDashboard_versionNumber = Lens.lens (\DeleteDashboard' {versionNumber} -> versionNumber) (\s@DeleteDashboard' {} a -> s {versionNumber = a} :: DeleteDashboard)

-- | The ID of the Amazon Web Services account that contains the dashboard
-- that you\'re deleting.
deleteDashboard_awsAccountId :: Lens.Lens' DeleteDashboard Prelude.Text
deleteDashboard_awsAccountId = Lens.lens (\DeleteDashboard' {awsAccountId} -> awsAccountId) (\s@DeleteDashboard' {} a -> s {awsAccountId = a} :: DeleteDashboard)

-- | The ID for the dashboard.
deleteDashboard_dashboardId :: Lens.Lens' DeleteDashboard Prelude.Text
deleteDashboard_dashboardId = Lens.lens (\DeleteDashboard' {dashboardId} -> dashboardId) (\s@DeleteDashboard' {} a -> s {dashboardId = a} :: DeleteDashboard)

instance Core.AWSRequest DeleteDashboard where
  type
    AWSResponse DeleteDashboard =
      DeleteDashboardResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDashboardResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "DashboardId")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDashboard where
  hashWithSalt _salt DeleteDashboard' {..} =
    _salt `Prelude.hashWithSalt` versionNumber
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dashboardId

instance Prelude.NFData DeleteDashboard where
  rnf DeleteDashboard' {..} =
    Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dashboardId

instance Data.ToHeaders DeleteDashboard where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteDashboard where
  toPath DeleteDashboard' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/dashboards/",
        Data.toBS dashboardId
      ]

instance Data.ToQuery DeleteDashboard where
  toQuery DeleteDashboard' {..} =
    Prelude.mconcat
      ["version-number" Data.=: versionNumber]

-- | /See:/ 'newDeleteDashboardResponse' smart constructor.
data DeleteDashboardResponse = DeleteDashboardResponse'
  { -- | The Secure Socket Layer (SSL) properties that apply for the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the dashboard.
    dashboardId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
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
-- 'arn', 'deleteDashboardResponse_arn' - The Secure Socket Layer (SSL) properties that apply for the resource.
--
-- 'dashboardId', 'deleteDashboardResponse_dashboardId' - The ID of the dashboard.
--
-- 'requestId', 'deleteDashboardResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'deleteDashboardResponse_status' - The HTTP status of the request.
newDeleteDashboardResponse ::
  -- | 'status'
  Prelude.Int ->
  DeleteDashboardResponse
newDeleteDashboardResponse pStatus_ =
  DeleteDashboardResponse'
    { arn = Prelude.Nothing,
      dashboardId = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Secure Socket Layer (SSL) properties that apply for the resource.
deleteDashboardResponse_arn :: Lens.Lens' DeleteDashboardResponse (Prelude.Maybe Prelude.Text)
deleteDashboardResponse_arn = Lens.lens (\DeleteDashboardResponse' {arn} -> arn) (\s@DeleteDashboardResponse' {} a -> s {arn = a} :: DeleteDashboardResponse)

-- | The ID of the dashboard.
deleteDashboardResponse_dashboardId :: Lens.Lens' DeleteDashboardResponse (Prelude.Maybe Prelude.Text)
deleteDashboardResponse_dashboardId = Lens.lens (\DeleteDashboardResponse' {dashboardId} -> dashboardId) (\s@DeleteDashboardResponse' {} a -> s {dashboardId = a} :: DeleteDashboardResponse)

-- | The Amazon Web Services request ID for this operation.
deleteDashboardResponse_requestId :: Lens.Lens' DeleteDashboardResponse (Prelude.Maybe Prelude.Text)
deleteDashboardResponse_requestId = Lens.lens (\DeleteDashboardResponse' {requestId} -> requestId) (\s@DeleteDashboardResponse' {} a -> s {requestId = a} :: DeleteDashboardResponse)

-- | The HTTP status of the request.
deleteDashboardResponse_status :: Lens.Lens' DeleteDashboardResponse Prelude.Int
deleteDashboardResponse_status = Lens.lens (\DeleteDashboardResponse' {status} -> status) (\s@DeleteDashboardResponse' {} a -> s {status = a} :: DeleteDashboardResponse)

instance Prelude.NFData DeleteDashboardResponse where
  rnf DeleteDashboardResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf dashboardId
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
