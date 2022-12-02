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
-- Module      : Amazonka.QuickSight.UpdateDashboardPermissions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates read and write permissions on a dashboard.
module Amazonka.QuickSight.UpdateDashboardPermissions
  ( -- * Creating a Request
    UpdateDashboardPermissions (..),
    newUpdateDashboardPermissions,

    -- * Request Lenses
    updateDashboardPermissions_grantPermissions,
    updateDashboardPermissions_revokeLinkPermissions,
    updateDashboardPermissions_grantLinkPermissions,
    updateDashboardPermissions_revokePermissions,
    updateDashboardPermissions_awsAccountId,
    updateDashboardPermissions_dashboardId,

    -- * Destructuring the Response
    UpdateDashboardPermissionsResponse (..),
    newUpdateDashboardPermissionsResponse,

    -- * Response Lenses
    updateDashboardPermissionsResponse_linkSharingConfiguration,
    updateDashboardPermissionsResponse_requestId,
    updateDashboardPermissionsResponse_permissions,
    updateDashboardPermissionsResponse_dashboardId,
    updateDashboardPermissionsResponse_dashboardArn,
    updateDashboardPermissionsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDashboardPermissions' smart constructor.
data UpdateDashboardPermissions = UpdateDashboardPermissions'
  { -- | The permissions that you want to grant on this resource.
    grantPermissions :: Prelude.Maybe [ResourcePermission],
    -- | Revokes link permissions from all users in a defined namespace.
    revokeLinkPermissions :: Prelude.Maybe [ResourcePermission],
    -- | Grants link permissions to all users in a defined namespace.
    grantLinkPermissions :: Prelude.Maybe [ResourcePermission],
    -- | The permissions that you want to revoke from this resource.
    revokePermissions :: Prelude.Maybe [ResourcePermission],
    -- | The ID of the Amazon Web Services account that contains the dashboard
    -- whose permissions you\'re updating.
    awsAccountId :: Prelude.Text,
    -- | The ID for the dashboard.
    dashboardId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDashboardPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantPermissions', 'updateDashboardPermissions_grantPermissions' - The permissions that you want to grant on this resource.
--
-- 'revokeLinkPermissions', 'updateDashboardPermissions_revokeLinkPermissions' - Revokes link permissions from all users in a defined namespace.
--
-- 'grantLinkPermissions', 'updateDashboardPermissions_grantLinkPermissions' - Grants link permissions to all users in a defined namespace.
--
-- 'revokePermissions', 'updateDashboardPermissions_revokePermissions' - The permissions that you want to revoke from this resource.
--
-- 'awsAccountId', 'updateDashboardPermissions_awsAccountId' - The ID of the Amazon Web Services account that contains the dashboard
-- whose permissions you\'re updating.
--
-- 'dashboardId', 'updateDashboardPermissions_dashboardId' - The ID for the dashboard.
newUpdateDashboardPermissions ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dashboardId'
  Prelude.Text ->
  UpdateDashboardPermissions
newUpdateDashboardPermissions
  pAwsAccountId_
  pDashboardId_ =
    UpdateDashboardPermissions'
      { grantPermissions =
          Prelude.Nothing,
        revokeLinkPermissions = Prelude.Nothing,
        grantLinkPermissions = Prelude.Nothing,
        revokePermissions = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        dashboardId = pDashboardId_
      }

-- | The permissions that you want to grant on this resource.
updateDashboardPermissions_grantPermissions :: Lens.Lens' UpdateDashboardPermissions (Prelude.Maybe [ResourcePermission])
updateDashboardPermissions_grantPermissions = Lens.lens (\UpdateDashboardPermissions' {grantPermissions} -> grantPermissions) (\s@UpdateDashboardPermissions' {} a -> s {grantPermissions = a} :: UpdateDashboardPermissions) Prelude.. Lens.mapping Lens.coerced

-- | Revokes link permissions from all users in a defined namespace.
updateDashboardPermissions_revokeLinkPermissions :: Lens.Lens' UpdateDashboardPermissions (Prelude.Maybe [ResourcePermission])
updateDashboardPermissions_revokeLinkPermissions = Lens.lens (\UpdateDashboardPermissions' {revokeLinkPermissions} -> revokeLinkPermissions) (\s@UpdateDashboardPermissions' {} a -> s {revokeLinkPermissions = a} :: UpdateDashboardPermissions) Prelude.. Lens.mapping Lens.coerced

-- | Grants link permissions to all users in a defined namespace.
updateDashboardPermissions_grantLinkPermissions :: Lens.Lens' UpdateDashboardPermissions (Prelude.Maybe [ResourcePermission])
updateDashboardPermissions_grantLinkPermissions = Lens.lens (\UpdateDashboardPermissions' {grantLinkPermissions} -> grantLinkPermissions) (\s@UpdateDashboardPermissions' {} a -> s {grantLinkPermissions = a} :: UpdateDashboardPermissions) Prelude.. Lens.mapping Lens.coerced

-- | The permissions that you want to revoke from this resource.
updateDashboardPermissions_revokePermissions :: Lens.Lens' UpdateDashboardPermissions (Prelude.Maybe [ResourcePermission])
updateDashboardPermissions_revokePermissions = Lens.lens (\UpdateDashboardPermissions' {revokePermissions} -> revokePermissions) (\s@UpdateDashboardPermissions' {} a -> s {revokePermissions = a} :: UpdateDashboardPermissions) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services account that contains the dashboard
-- whose permissions you\'re updating.
updateDashboardPermissions_awsAccountId :: Lens.Lens' UpdateDashboardPermissions Prelude.Text
updateDashboardPermissions_awsAccountId = Lens.lens (\UpdateDashboardPermissions' {awsAccountId} -> awsAccountId) (\s@UpdateDashboardPermissions' {} a -> s {awsAccountId = a} :: UpdateDashboardPermissions)

-- | The ID for the dashboard.
updateDashboardPermissions_dashboardId :: Lens.Lens' UpdateDashboardPermissions Prelude.Text
updateDashboardPermissions_dashboardId = Lens.lens (\UpdateDashboardPermissions' {dashboardId} -> dashboardId) (\s@UpdateDashboardPermissions' {} a -> s {dashboardId = a} :: UpdateDashboardPermissions)

instance Core.AWSRequest UpdateDashboardPermissions where
  type
    AWSResponse UpdateDashboardPermissions =
      UpdateDashboardPermissionsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDashboardPermissionsResponse'
            Prelude.<$> (x Data..?> "LinkSharingConfiguration")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "Permissions")
            Prelude.<*> (x Data..?> "DashboardId")
            Prelude.<*> (x Data..?> "DashboardArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDashboardPermissions where
  hashWithSalt _salt UpdateDashboardPermissions' {..} =
    _salt `Prelude.hashWithSalt` grantPermissions
      `Prelude.hashWithSalt` revokeLinkPermissions
      `Prelude.hashWithSalt` grantLinkPermissions
      `Prelude.hashWithSalt` revokePermissions
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dashboardId

instance Prelude.NFData UpdateDashboardPermissions where
  rnf UpdateDashboardPermissions' {..} =
    Prelude.rnf grantPermissions
      `Prelude.seq` Prelude.rnf revokeLinkPermissions
      `Prelude.seq` Prelude.rnf grantLinkPermissions
      `Prelude.seq` Prelude.rnf revokePermissions
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dashboardId

instance Data.ToHeaders UpdateDashboardPermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDashboardPermissions where
  toJSON UpdateDashboardPermissions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GrantPermissions" Data..=)
              Prelude.<$> grantPermissions,
            ("RevokeLinkPermissions" Data..=)
              Prelude.<$> revokeLinkPermissions,
            ("GrantLinkPermissions" Data..=)
              Prelude.<$> grantLinkPermissions,
            ("RevokePermissions" Data..=)
              Prelude.<$> revokePermissions
          ]
      )

instance Data.ToPath UpdateDashboardPermissions where
  toPath UpdateDashboardPermissions' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/dashboards/",
        Data.toBS dashboardId,
        "/permissions"
      ]

instance Data.ToQuery UpdateDashboardPermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDashboardPermissionsResponse' smart constructor.
data UpdateDashboardPermissionsResponse = UpdateDashboardPermissionsResponse'
  { -- | Updates the permissions of a shared link to an Amazon QuickSight
    -- dashboard.
    linkSharingConfiguration :: Prelude.Maybe LinkSharingConfiguration,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | Information about the permissions on the dashboard.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The ID for the dashboard.
    dashboardId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the dashboard.
    dashboardArn :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDashboardPermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'linkSharingConfiguration', 'updateDashboardPermissionsResponse_linkSharingConfiguration' - Updates the permissions of a shared link to an Amazon QuickSight
-- dashboard.
--
-- 'requestId', 'updateDashboardPermissionsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'permissions', 'updateDashboardPermissionsResponse_permissions' - Information about the permissions on the dashboard.
--
-- 'dashboardId', 'updateDashboardPermissionsResponse_dashboardId' - The ID for the dashboard.
--
-- 'dashboardArn', 'updateDashboardPermissionsResponse_dashboardArn' - The Amazon Resource Name (ARN) of the dashboard.
--
-- 'status', 'updateDashboardPermissionsResponse_status' - The HTTP status of the request.
newUpdateDashboardPermissionsResponse ::
  -- | 'status'
  Prelude.Int ->
  UpdateDashboardPermissionsResponse
newUpdateDashboardPermissionsResponse pStatus_ =
  UpdateDashboardPermissionsResponse'
    { linkSharingConfiguration =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      permissions = Prelude.Nothing,
      dashboardId = Prelude.Nothing,
      dashboardArn = Prelude.Nothing,
      status = pStatus_
    }

-- | Updates the permissions of a shared link to an Amazon QuickSight
-- dashboard.
updateDashboardPermissionsResponse_linkSharingConfiguration :: Lens.Lens' UpdateDashboardPermissionsResponse (Prelude.Maybe LinkSharingConfiguration)
updateDashboardPermissionsResponse_linkSharingConfiguration = Lens.lens (\UpdateDashboardPermissionsResponse' {linkSharingConfiguration} -> linkSharingConfiguration) (\s@UpdateDashboardPermissionsResponse' {} a -> s {linkSharingConfiguration = a} :: UpdateDashboardPermissionsResponse)

-- | The Amazon Web Services request ID for this operation.
updateDashboardPermissionsResponse_requestId :: Lens.Lens' UpdateDashboardPermissionsResponse (Prelude.Maybe Prelude.Text)
updateDashboardPermissionsResponse_requestId = Lens.lens (\UpdateDashboardPermissionsResponse' {requestId} -> requestId) (\s@UpdateDashboardPermissionsResponse' {} a -> s {requestId = a} :: UpdateDashboardPermissionsResponse)

-- | Information about the permissions on the dashboard.
updateDashboardPermissionsResponse_permissions :: Lens.Lens' UpdateDashboardPermissionsResponse (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
updateDashboardPermissionsResponse_permissions = Lens.lens (\UpdateDashboardPermissionsResponse' {permissions} -> permissions) (\s@UpdateDashboardPermissionsResponse' {} a -> s {permissions = a} :: UpdateDashboardPermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID for the dashboard.
updateDashboardPermissionsResponse_dashboardId :: Lens.Lens' UpdateDashboardPermissionsResponse (Prelude.Maybe Prelude.Text)
updateDashboardPermissionsResponse_dashboardId = Lens.lens (\UpdateDashboardPermissionsResponse' {dashboardId} -> dashboardId) (\s@UpdateDashboardPermissionsResponse' {} a -> s {dashboardId = a} :: UpdateDashboardPermissionsResponse)

-- | The Amazon Resource Name (ARN) of the dashboard.
updateDashboardPermissionsResponse_dashboardArn :: Lens.Lens' UpdateDashboardPermissionsResponse (Prelude.Maybe Prelude.Text)
updateDashboardPermissionsResponse_dashboardArn = Lens.lens (\UpdateDashboardPermissionsResponse' {dashboardArn} -> dashboardArn) (\s@UpdateDashboardPermissionsResponse' {} a -> s {dashboardArn = a} :: UpdateDashboardPermissionsResponse)

-- | The HTTP status of the request.
updateDashboardPermissionsResponse_status :: Lens.Lens' UpdateDashboardPermissionsResponse Prelude.Int
updateDashboardPermissionsResponse_status = Lens.lens (\UpdateDashboardPermissionsResponse' {status} -> status) (\s@UpdateDashboardPermissionsResponse' {} a -> s {status = a} :: UpdateDashboardPermissionsResponse)

instance
  Prelude.NFData
    UpdateDashboardPermissionsResponse
  where
  rnf UpdateDashboardPermissionsResponse' {..} =
    Prelude.rnf linkSharingConfiguration
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf dashboardId
      `Prelude.seq` Prelude.rnf dashboardArn
      `Prelude.seq` Prelude.rnf status
