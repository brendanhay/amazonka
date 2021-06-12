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
-- Module      : Network.AWS.CloudWatch.DeleteDashboards
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all dashboards that you specify. You can specify up to 100
-- dashboards to delete. If there is an error during this call, no
-- dashboards are deleted.
module Network.AWS.CloudWatch.DeleteDashboards
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

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDashboards' smart constructor.
data DeleteDashboards = DeleteDashboards'
  { -- | The dashboards to be deleted. This parameter is required.
    dashboardNames :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  DeleteDashboards' {dashboardNames = Core.mempty}

-- | The dashboards to be deleted. This parameter is required.
deleteDashboards_dashboardNames :: Lens.Lens' DeleteDashboards [Core.Text]
deleteDashboards_dashboardNames = Lens.lens (\DeleteDashboards' {dashboardNames} -> dashboardNames) (\s@DeleteDashboards' {} a -> s {dashboardNames = a} :: DeleteDashboards) Core.. Lens._Coerce

instance Core.AWSRequest DeleteDashboards where
  type
    AWSResponse DeleteDashboards =
      DeleteDashboardsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteDashboardsResult"
      ( \s h x ->
          DeleteDashboardsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteDashboards

instance Core.NFData DeleteDashboards

instance Core.ToHeaders DeleteDashboards where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteDashboards where
  toPath = Core.const "/"

instance Core.ToQuery DeleteDashboards where
  toQuery DeleteDashboards' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteDashboards" :: Core.ByteString),
        "Version" Core.=: ("2010-08-01" :: Core.ByteString),
        "DashboardNames"
          Core.=: Core.toQueryList "member" dashboardNames
      ]

-- | /See:/ 'newDeleteDashboardsResponse' smart constructor.
data DeleteDashboardsResponse = DeleteDashboardsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteDashboardsResponse
newDeleteDashboardsResponse pHttpStatus_ =
  DeleteDashboardsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDashboardsResponse_httpStatus :: Lens.Lens' DeleteDashboardsResponse Core.Int
deleteDashboardsResponse_httpStatus = Lens.lens (\DeleteDashboardsResponse' {httpStatus} -> httpStatus) (\s@DeleteDashboardsResponse' {} a -> s {httpStatus = a} :: DeleteDashboardsResponse)

instance Core.NFData DeleteDashboardsResponse
