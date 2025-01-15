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
-- Module      : Amazonka.QuickSight.DescribeDashboardPermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes read and write permissions for a dashboard.
module Amazonka.QuickSight.DescribeDashboardPermissions
  ( -- * Creating a Request
    DescribeDashboardPermissions (..),
    newDescribeDashboardPermissions,

    -- * Request Lenses
    describeDashboardPermissions_awsAccountId,
    describeDashboardPermissions_dashboardId,

    -- * Destructuring the Response
    DescribeDashboardPermissionsResponse (..),
    newDescribeDashboardPermissionsResponse,

    -- * Response Lenses
    describeDashboardPermissionsResponse_dashboardArn,
    describeDashboardPermissionsResponse_dashboardId,
    describeDashboardPermissionsResponse_linkSharingConfiguration,
    describeDashboardPermissionsResponse_permissions,
    describeDashboardPermissionsResponse_requestId,
    describeDashboardPermissionsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDashboardPermissions' smart constructor.
data DescribeDashboardPermissions = DescribeDashboardPermissions'
  { -- | The ID of the Amazon Web Services account that contains the dashboard
    -- that you\'re describing permissions for.
    awsAccountId :: Prelude.Text,
    -- | The ID for the dashboard, also added to the IAM policy.
    dashboardId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDashboardPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeDashboardPermissions_awsAccountId' - The ID of the Amazon Web Services account that contains the dashboard
-- that you\'re describing permissions for.
--
-- 'dashboardId', 'describeDashboardPermissions_dashboardId' - The ID for the dashboard, also added to the IAM policy.
newDescribeDashboardPermissions ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dashboardId'
  Prelude.Text ->
  DescribeDashboardPermissions
newDescribeDashboardPermissions
  pAwsAccountId_
  pDashboardId_ =
    DescribeDashboardPermissions'
      { awsAccountId =
          pAwsAccountId_,
        dashboardId = pDashboardId_
      }

-- | The ID of the Amazon Web Services account that contains the dashboard
-- that you\'re describing permissions for.
describeDashboardPermissions_awsAccountId :: Lens.Lens' DescribeDashboardPermissions Prelude.Text
describeDashboardPermissions_awsAccountId = Lens.lens (\DescribeDashboardPermissions' {awsAccountId} -> awsAccountId) (\s@DescribeDashboardPermissions' {} a -> s {awsAccountId = a} :: DescribeDashboardPermissions)

-- | The ID for the dashboard, also added to the IAM policy.
describeDashboardPermissions_dashboardId :: Lens.Lens' DescribeDashboardPermissions Prelude.Text
describeDashboardPermissions_dashboardId = Lens.lens (\DescribeDashboardPermissions' {dashboardId} -> dashboardId) (\s@DescribeDashboardPermissions' {} a -> s {dashboardId = a} :: DescribeDashboardPermissions)

instance Core.AWSRequest DescribeDashboardPermissions where
  type
    AWSResponse DescribeDashboardPermissions =
      DescribeDashboardPermissionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDashboardPermissionsResponse'
            Prelude.<$> (x Data..?> "DashboardArn")
            Prelude.<*> (x Data..?> "DashboardId")
            Prelude.<*> (x Data..?> "LinkSharingConfiguration")
            Prelude.<*> (x Data..?> "Permissions")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDashboardPermissions
  where
  hashWithSalt _salt DescribeDashboardPermissions' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dashboardId

instance Prelude.NFData DescribeDashboardPermissions where
  rnf DescribeDashboardPermissions' {..} =
    Prelude.rnf awsAccountId `Prelude.seq`
      Prelude.rnf dashboardId

instance Data.ToHeaders DescribeDashboardPermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeDashboardPermissions where
  toPath DescribeDashboardPermissions' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/dashboards/",
        Data.toBS dashboardId,
        "/permissions"
      ]

instance Data.ToQuery DescribeDashboardPermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDashboardPermissionsResponse' smart constructor.
data DescribeDashboardPermissionsResponse = DescribeDashboardPermissionsResponse'
  { -- | The Amazon Resource Name (ARN) of the dashboard.
    dashboardArn :: Prelude.Maybe Prelude.Text,
    -- | The ID for the dashboard.
    dashboardId :: Prelude.Maybe Prelude.Text,
    -- | A structure that contains the configuration of a shareable link that
    -- grants access to the dashboard. Your users can use the link to view and
    -- interact with the dashboard, if the dashboard has been shared with them.
    -- For more information about sharing dashboards, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/sharing-a-dashboard.html Sharing Dashboards>.
    linkSharingConfiguration :: Prelude.Maybe LinkSharingConfiguration,
    -- | A structure that contains the permissions for the dashboard.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDashboardPermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashboardArn', 'describeDashboardPermissionsResponse_dashboardArn' - The Amazon Resource Name (ARN) of the dashboard.
--
-- 'dashboardId', 'describeDashboardPermissionsResponse_dashboardId' - The ID for the dashboard.
--
-- 'linkSharingConfiguration', 'describeDashboardPermissionsResponse_linkSharingConfiguration' - A structure that contains the configuration of a shareable link that
-- grants access to the dashboard. Your users can use the link to view and
-- interact with the dashboard, if the dashboard has been shared with them.
-- For more information about sharing dashboards, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/sharing-a-dashboard.html Sharing Dashboards>.
--
-- 'permissions', 'describeDashboardPermissionsResponse_permissions' - A structure that contains the permissions for the dashboard.
--
-- 'requestId', 'describeDashboardPermissionsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'describeDashboardPermissionsResponse_status' - The HTTP status of the request.
newDescribeDashboardPermissionsResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeDashboardPermissionsResponse
newDescribeDashboardPermissionsResponse pStatus_ =
  DescribeDashboardPermissionsResponse'
    { dashboardArn =
        Prelude.Nothing,
      dashboardId = Prelude.Nothing,
      linkSharingConfiguration =
        Prelude.Nothing,
      permissions = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the dashboard.
describeDashboardPermissionsResponse_dashboardArn :: Lens.Lens' DescribeDashboardPermissionsResponse (Prelude.Maybe Prelude.Text)
describeDashboardPermissionsResponse_dashboardArn = Lens.lens (\DescribeDashboardPermissionsResponse' {dashboardArn} -> dashboardArn) (\s@DescribeDashboardPermissionsResponse' {} a -> s {dashboardArn = a} :: DescribeDashboardPermissionsResponse)

-- | The ID for the dashboard.
describeDashboardPermissionsResponse_dashboardId :: Lens.Lens' DescribeDashboardPermissionsResponse (Prelude.Maybe Prelude.Text)
describeDashboardPermissionsResponse_dashboardId = Lens.lens (\DescribeDashboardPermissionsResponse' {dashboardId} -> dashboardId) (\s@DescribeDashboardPermissionsResponse' {} a -> s {dashboardId = a} :: DescribeDashboardPermissionsResponse)

-- | A structure that contains the configuration of a shareable link that
-- grants access to the dashboard. Your users can use the link to view and
-- interact with the dashboard, if the dashboard has been shared with them.
-- For more information about sharing dashboards, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/sharing-a-dashboard.html Sharing Dashboards>.
describeDashboardPermissionsResponse_linkSharingConfiguration :: Lens.Lens' DescribeDashboardPermissionsResponse (Prelude.Maybe LinkSharingConfiguration)
describeDashboardPermissionsResponse_linkSharingConfiguration = Lens.lens (\DescribeDashboardPermissionsResponse' {linkSharingConfiguration} -> linkSharingConfiguration) (\s@DescribeDashboardPermissionsResponse' {} a -> s {linkSharingConfiguration = a} :: DescribeDashboardPermissionsResponse)

-- | A structure that contains the permissions for the dashboard.
describeDashboardPermissionsResponse_permissions :: Lens.Lens' DescribeDashboardPermissionsResponse (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
describeDashboardPermissionsResponse_permissions = Lens.lens (\DescribeDashboardPermissionsResponse' {permissions} -> permissions) (\s@DescribeDashboardPermissionsResponse' {} a -> s {permissions = a} :: DescribeDashboardPermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services request ID for this operation.
describeDashboardPermissionsResponse_requestId :: Lens.Lens' DescribeDashboardPermissionsResponse (Prelude.Maybe Prelude.Text)
describeDashboardPermissionsResponse_requestId = Lens.lens (\DescribeDashboardPermissionsResponse' {requestId} -> requestId) (\s@DescribeDashboardPermissionsResponse' {} a -> s {requestId = a} :: DescribeDashboardPermissionsResponse)

-- | The HTTP status of the request.
describeDashboardPermissionsResponse_status :: Lens.Lens' DescribeDashboardPermissionsResponse Prelude.Int
describeDashboardPermissionsResponse_status = Lens.lens (\DescribeDashboardPermissionsResponse' {status} -> status) (\s@DescribeDashboardPermissionsResponse' {} a -> s {status = a} :: DescribeDashboardPermissionsResponse)

instance
  Prelude.NFData
    DescribeDashboardPermissionsResponse
  where
  rnf DescribeDashboardPermissionsResponse' {..} =
    Prelude.rnf dashboardArn `Prelude.seq`
      Prelude.rnf dashboardId `Prelude.seq`
        Prelude.rnf linkSharingConfiguration `Prelude.seq`
          Prelude.rnf permissions `Prelude.seq`
            Prelude.rnf requestId `Prelude.seq`
              Prelude.rnf status
