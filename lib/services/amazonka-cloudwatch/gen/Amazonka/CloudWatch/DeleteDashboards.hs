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
-- Module      : Amazonka.CloudWatch.DeleteDashboards
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all dashboards that you specify. You can specify up to 100
-- dashboards to delete. If there is an error during this call, no
-- dashboards are deleted.
module Amazonka.CloudWatch.DeleteDashboards
  ( -- * Creating a Request
    DeleteDashboards (..),
    newDeleteDashboards,

    -- * Request Lenses
    deleteDashboards_dashboardNames,

    -- * Destructuring the Response
    DeleteDashboardsResponse (..),
    newDeleteDashboardsResponse,

    -- * Response Lenses
    deleteDashboardsResponse_httpStatus,
  )
where

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDashboards' smart constructor.
data DeleteDashboards = DeleteDashboards'
  { -- | The dashboards to be deleted. This parameter is required.
    dashboardNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDashboards' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashboardNames', 'deleteDashboards_dashboardNames' - The dashboards to be deleted. This parameter is required.
newDeleteDashboards ::
  DeleteDashboards
newDeleteDashboards =
  DeleteDashboards' {dashboardNames = Prelude.mempty}

-- | The dashboards to be deleted. This parameter is required.
deleteDashboards_dashboardNames :: Lens.Lens' DeleteDashboards [Prelude.Text]
deleteDashboards_dashboardNames = Lens.lens (\DeleteDashboards' {dashboardNames} -> dashboardNames) (\s@DeleteDashboards' {} a -> s {dashboardNames = a} :: DeleteDashboards) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteDashboards where
  type
    AWSResponse DeleteDashboards =
      DeleteDashboardsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteDashboardsResult"
      ( \s h x ->
          DeleteDashboardsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDashboards where
  hashWithSalt _salt DeleteDashboards' {..} =
    _salt `Prelude.hashWithSalt` dashboardNames

instance Prelude.NFData DeleteDashboards where
  rnf DeleteDashboards' {..} =
    Prelude.rnf dashboardNames

instance Data.ToHeaders DeleteDashboards where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteDashboards where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDashboards where
  toQuery DeleteDashboards' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteDashboards" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-08-01" :: Prelude.ByteString),
        "DashboardNames"
          Data.=: Data.toQueryList "member" dashboardNames
      ]

-- | /See:/ 'newDeleteDashboardsResponse' smart constructor.
data DeleteDashboardsResponse = DeleteDashboardsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDashboardsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDashboardsResponse_httpStatus' - The response's http status code.
newDeleteDashboardsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDashboardsResponse
newDeleteDashboardsResponse pHttpStatus_ =
  DeleteDashboardsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDashboardsResponse_httpStatus :: Lens.Lens' DeleteDashboardsResponse Prelude.Int
deleteDashboardsResponse_httpStatus = Lens.lens (\DeleteDashboardsResponse' {httpStatus} -> httpStatus) (\s@DeleteDashboardsResponse' {} a -> s {httpStatus = a} :: DeleteDashboardsResponse)

instance Prelude.NFData DeleteDashboardsResponse where
  rnf DeleteDashboardsResponse' {..} =
    Prelude.rnf httpStatus
