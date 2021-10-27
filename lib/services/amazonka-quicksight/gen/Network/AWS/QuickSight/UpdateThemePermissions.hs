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
-- Module      : Network.AWS.QuickSight.UpdateThemePermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the resource permissions for a theme. Permissions apply to the
-- action to grant or revoke permissions on, for example
-- @\"quicksight:DescribeTheme\"@.
--
-- Theme permissions apply in groupings. Valid groupings include the
-- following for the three levels of permissions, which are user, owner, or
-- no permissions:
--
-- -   User
--
--     -   @\"quicksight:DescribeTheme\"@
--
--     -   @\"quicksight:DescribeThemeAlias\"@
--
--     -   @\"quicksight:ListThemeAliases\"@
--
--     -   @\"quicksight:ListThemeVersions\"@
--
-- -   Owner
--
--     -   @\"quicksight:DescribeTheme\"@
--
--     -   @\"quicksight:DescribeThemeAlias\"@
--
--     -   @\"quicksight:ListThemeAliases\"@
--
--     -   @\"quicksight:ListThemeVersions\"@
--
--     -   @\"quicksight:DeleteTheme\"@
--
--     -   @\"quicksight:UpdateTheme\"@
--
--     -   @\"quicksight:CreateThemeAlias\"@
--
--     -   @\"quicksight:DeleteThemeAlias\"@
--
--     -   @\"quicksight:UpdateThemeAlias\"@
--
--     -   @\"quicksight:UpdateThemePermissions\"@
--
--     -   @\"quicksight:DescribeThemePermissions\"@
--
-- -   To specify no permissions, omit the permissions list.
module Network.AWS.QuickSight.UpdateThemePermissions
  ( -- * Creating a Request
    UpdateThemePermissions (..),
    newUpdateThemePermissions,

    -- * Request Lenses
    updateThemePermissions_revokePermissions,
    updateThemePermissions_grantPermissions,
    updateThemePermissions_awsAccountId,
    updateThemePermissions_themeId,

    -- * Destructuring the Response
    UpdateThemePermissionsResponse (..),
    newUpdateThemePermissionsResponse,

    -- * Response Lenses
    updateThemePermissionsResponse_requestId,
    updateThemePermissionsResponse_themeArn,
    updateThemePermissionsResponse_themeId,
    updateThemePermissionsResponse_permissions,
    updateThemePermissionsResponse_status,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QuickSight.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateThemePermissions' smart constructor.
data UpdateThemePermissions = UpdateThemePermissions'
  { -- | A list of resource permissions to be revoked from the theme.
    revokePermissions :: Prelude.Maybe [ResourcePermission],
    -- | A list of resource permissions to be granted for the theme.
    grantPermissions :: Prelude.Maybe [ResourcePermission],
    -- | The ID of the Amazon Web Services account that contains the theme.
    awsAccountId :: Prelude.Text,
    -- | The ID for the theme.
    themeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateThemePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revokePermissions', 'updateThemePermissions_revokePermissions' - A list of resource permissions to be revoked from the theme.
--
-- 'grantPermissions', 'updateThemePermissions_grantPermissions' - A list of resource permissions to be granted for the theme.
--
-- 'awsAccountId', 'updateThemePermissions_awsAccountId' - The ID of the Amazon Web Services account that contains the theme.
--
-- 'themeId', 'updateThemePermissions_themeId' - The ID for the theme.
newUpdateThemePermissions ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'themeId'
  Prelude.Text ->
  UpdateThemePermissions
newUpdateThemePermissions pAwsAccountId_ pThemeId_ =
  UpdateThemePermissions'
    { revokePermissions =
        Prelude.Nothing,
      grantPermissions = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      themeId = pThemeId_
    }

-- | A list of resource permissions to be revoked from the theme.
updateThemePermissions_revokePermissions :: Lens.Lens' UpdateThemePermissions (Prelude.Maybe [ResourcePermission])
updateThemePermissions_revokePermissions = Lens.lens (\UpdateThemePermissions' {revokePermissions} -> revokePermissions) (\s@UpdateThemePermissions' {} a -> s {revokePermissions = a} :: UpdateThemePermissions) Prelude.. Lens.mapping Lens.coerced

-- | A list of resource permissions to be granted for the theme.
updateThemePermissions_grantPermissions :: Lens.Lens' UpdateThemePermissions (Prelude.Maybe [ResourcePermission])
updateThemePermissions_grantPermissions = Lens.lens (\UpdateThemePermissions' {grantPermissions} -> grantPermissions) (\s@UpdateThemePermissions' {} a -> s {grantPermissions = a} :: UpdateThemePermissions) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services account that contains the theme.
updateThemePermissions_awsAccountId :: Lens.Lens' UpdateThemePermissions Prelude.Text
updateThemePermissions_awsAccountId = Lens.lens (\UpdateThemePermissions' {awsAccountId} -> awsAccountId) (\s@UpdateThemePermissions' {} a -> s {awsAccountId = a} :: UpdateThemePermissions)

-- | The ID for the theme.
updateThemePermissions_themeId :: Lens.Lens' UpdateThemePermissions Prelude.Text
updateThemePermissions_themeId = Lens.lens (\UpdateThemePermissions' {themeId} -> themeId) (\s@UpdateThemePermissions' {} a -> s {themeId = a} :: UpdateThemePermissions)

instance Core.AWSRequest UpdateThemePermissions where
  type
    AWSResponse UpdateThemePermissions =
      UpdateThemePermissionsResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateThemePermissionsResponse'
            Prelude.<$> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "ThemeArn")
            Prelude.<*> (x Core..?> "ThemeId")
            Prelude.<*> (x Core..?> "Permissions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateThemePermissions

instance Prelude.NFData UpdateThemePermissions

instance Core.ToHeaders UpdateThemePermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateThemePermissions where
  toJSON UpdateThemePermissions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RevokePermissions" Core..=)
              Prelude.<$> revokePermissions,
            ("GrantPermissions" Core..=)
              Prelude.<$> grantPermissions
          ]
      )

instance Core.ToPath UpdateThemePermissions where
  toPath UpdateThemePermissions' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/themes/",
        Core.toBS themeId,
        "/permissions"
      ]

instance Core.ToQuery UpdateThemePermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateThemePermissionsResponse' smart constructor.
data UpdateThemePermissionsResponse = UpdateThemePermissionsResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the theme.
    themeArn :: Prelude.Maybe Prelude.Text,
    -- | The ID for the theme.
    themeId :: Prelude.Maybe Prelude.Text,
    -- | The resulting list of resource permissions for the theme.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateThemePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'updateThemePermissionsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'themeArn', 'updateThemePermissionsResponse_themeArn' - The Amazon Resource Name (ARN) of the theme.
--
-- 'themeId', 'updateThemePermissionsResponse_themeId' - The ID for the theme.
--
-- 'permissions', 'updateThemePermissionsResponse_permissions' - The resulting list of resource permissions for the theme.
--
-- 'status', 'updateThemePermissionsResponse_status' - The HTTP status of the request.
newUpdateThemePermissionsResponse ::
  -- | 'status'
  Prelude.Int ->
  UpdateThemePermissionsResponse
newUpdateThemePermissionsResponse pStatus_ =
  UpdateThemePermissionsResponse'
    { requestId =
        Prelude.Nothing,
      themeArn = Prelude.Nothing,
      themeId = Prelude.Nothing,
      permissions = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
updateThemePermissionsResponse_requestId :: Lens.Lens' UpdateThemePermissionsResponse (Prelude.Maybe Prelude.Text)
updateThemePermissionsResponse_requestId = Lens.lens (\UpdateThemePermissionsResponse' {requestId} -> requestId) (\s@UpdateThemePermissionsResponse' {} a -> s {requestId = a} :: UpdateThemePermissionsResponse)

-- | The Amazon Resource Name (ARN) of the theme.
updateThemePermissionsResponse_themeArn :: Lens.Lens' UpdateThemePermissionsResponse (Prelude.Maybe Prelude.Text)
updateThemePermissionsResponse_themeArn = Lens.lens (\UpdateThemePermissionsResponse' {themeArn} -> themeArn) (\s@UpdateThemePermissionsResponse' {} a -> s {themeArn = a} :: UpdateThemePermissionsResponse)

-- | The ID for the theme.
updateThemePermissionsResponse_themeId :: Lens.Lens' UpdateThemePermissionsResponse (Prelude.Maybe Prelude.Text)
updateThemePermissionsResponse_themeId = Lens.lens (\UpdateThemePermissionsResponse' {themeId} -> themeId) (\s@UpdateThemePermissionsResponse' {} a -> s {themeId = a} :: UpdateThemePermissionsResponse)

-- | The resulting list of resource permissions for the theme.
updateThemePermissionsResponse_permissions :: Lens.Lens' UpdateThemePermissionsResponse (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
updateThemePermissionsResponse_permissions = Lens.lens (\UpdateThemePermissionsResponse' {permissions} -> permissions) (\s@UpdateThemePermissionsResponse' {} a -> s {permissions = a} :: UpdateThemePermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP status of the request.
updateThemePermissionsResponse_status :: Lens.Lens' UpdateThemePermissionsResponse Prelude.Int
updateThemePermissionsResponse_status = Lens.lens (\UpdateThemePermissionsResponse' {status} -> status) (\s@UpdateThemePermissionsResponse' {} a -> s {status = a} :: UpdateThemePermissionsResponse)

instance
  Prelude.NFData
    UpdateThemePermissionsResponse
