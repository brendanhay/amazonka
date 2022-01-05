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
-- Module      : Amazonka.QuickSight.UpdateFolderPermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates permissions of a folder.
module Amazonka.QuickSight.UpdateFolderPermissions
  ( -- * Creating a Request
    UpdateFolderPermissions (..),
    newUpdateFolderPermissions,

    -- * Request Lenses
    updateFolderPermissions_revokePermissions,
    updateFolderPermissions_grantPermissions,
    updateFolderPermissions_awsAccountId,
    updateFolderPermissions_folderId,

    -- * Destructuring the Response
    UpdateFolderPermissionsResponse (..),
    newUpdateFolderPermissionsResponse,

    -- * Response Lenses
    updateFolderPermissionsResponse_requestId,
    updateFolderPermissionsResponse_status,
    updateFolderPermissionsResponse_arn,
    updateFolderPermissionsResponse_folderId,
    updateFolderPermissionsResponse_permissions,
    updateFolderPermissionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFolderPermissions' smart constructor.
data UpdateFolderPermissions = UpdateFolderPermissions'
  { -- | The permissions that you want to revoke from a resource.
    revokePermissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The permissions that you want to grant on a resource.
    grantPermissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The AWS account ID.
    awsAccountId :: Prelude.Text,
    -- | The folder ID.
    folderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFolderPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revokePermissions', 'updateFolderPermissions_revokePermissions' - The permissions that you want to revoke from a resource.
--
-- 'grantPermissions', 'updateFolderPermissions_grantPermissions' - The permissions that you want to grant on a resource.
--
-- 'awsAccountId', 'updateFolderPermissions_awsAccountId' - The AWS account ID.
--
-- 'folderId', 'updateFolderPermissions_folderId' - The folder ID.
newUpdateFolderPermissions ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'folderId'
  Prelude.Text ->
  UpdateFolderPermissions
newUpdateFolderPermissions pAwsAccountId_ pFolderId_ =
  UpdateFolderPermissions'
    { revokePermissions =
        Prelude.Nothing,
      grantPermissions = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      folderId = pFolderId_
    }

-- | The permissions that you want to revoke from a resource.
updateFolderPermissions_revokePermissions :: Lens.Lens' UpdateFolderPermissions (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
updateFolderPermissions_revokePermissions = Lens.lens (\UpdateFolderPermissions' {revokePermissions} -> revokePermissions) (\s@UpdateFolderPermissions' {} a -> s {revokePermissions = a} :: UpdateFolderPermissions) Prelude.. Lens.mapping Lens.coerced

-- | The permissions that you want to grant on a resource.
updateFolderPermissions_grantPermissions :: Lens.Lens' UpdateFolderPermissions (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
updateFolderPermissions_grantPermissions = Lens.lens (\UpdateFolderPermissions' {grantPermissions} -> grantPermissions) (\s@UpdateFolderPermissions' {} a -> s {grantPermissions = a} :: UpdateFolderPermissions) Prelude.. Lens.mapping Lens.coerced

-- | The AWS account ID.
updateFolderPermissions_awsAccountId :: Lens.Lens' UpdateFolderPermissions Prelude.Text
updateFolderPermissions_awsAccountId = Lens.lens (\UpdateFolderPermissions' {awsAccountId} -> awsAccountId) (\s@UpdateFolderPermissions' {} a -> s {awsAccountId = a} :: UpdateFolderPermissions)

-- | The folder ID.
updateFolderPermissions_folderId :: Lens.Lens' UpdateFolderPermissions Prelude.Text
updateFolderPermissions_folderId = Lens.lens (\UpdateFolderPermissions' {folderId} -> folderId) (\s@UpdateFolderPermissions' {} a -> s {folderId = a} :: UpdateFolderPermissions)

instance Core.AWSRequest UpdateFolderPermissions where
  type
    AWSResponse UpdateFolderPermissions =
      UpdateFolderPermissionsResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFolderPermissionsResponse'
            Prelude.<$> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "FolderId")
            Prelude.<*> (x Core..?> "Permissions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFolderPermissions where
  hashWithSalt _salt UpdateFolderPermissions' {..} =
    _salt `Prelude.hashWithSalt` revokePermissions
      `Prelude.hashWithSalt` grantPermissions
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` folderId

instance Prelude.NFData UpdateFolderPermissions where
  rnf UpdateFolderPermissions' {..} =
    Prelude.rnf revokePermissions
      `Prelude.seq` Prelude.rnf grantPermissions
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf folderId

instance Core.ToHeaders UpdateFolderPermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateFolderPermissions where
  toJSON UpdateFolderPermissions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RevokePermissions" Core..=)
              Prelude.<$> revokePermissions,
            ("GrantPermissions" Core..=)
              Prelude.<$> grantPermissions
          ]
      )

instance Core.ToPath UpdateFolderPermissions where
  toPath UpdateFolderPermissions' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/folders/",
        Core.toBS folderId,
        "/permissions"
      ]

instance Core.ToQuery UpdateFolderPermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFolderPermissionsResponse' smart constructor.
data UpdateFolderPermissionsResponse = UpdateFolderPermissionsResponse'
  { -- | The request ID.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The status. If succeeded, the status is @SC_OK@.
    status :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN).
    arn :: Prelude.Maybe Prelude.Text,
    -- | The folder ID.
    folderId :: Prelude.Maybe Prelude.Text,
    -- | Information about the permissions on the dashboard.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFolderPermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'updateFolderPermissionsResponse_requestId' - The request ID.
--
-- 'status', 'updateFolderPermissionsResponse_status' - The status. If succeeded, the status is @SC_OK@.
--
-- 'arn', 'updateFolderPermissionsResponse_arn' - The Amazon Resource Name (ARN).
--
-- 'folderId', 'updateFolderPermissionsResponse_folderId' - The folder ID.
--
-- 'permissions', 'updateFolderPermissionsResponse_permissions' - Information about the permissions on the dashboard.
--
-- 'httpStatus', 'updateFolderPermissionsResponse_httpStatus' - The response's http status code.
newUpdateFolderPermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFolderPermissionsResponse
newUpdateFolderPermissionsResponse pHttpStatus_ =
  UpdateFolderPermissionsResponse'
    { requestId =
        Prelude.Nothing,
      status = Prelude.Nothing,
      arn = Prelude.Nothing,
      folderId = Prelude.Nothing,
      permissions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The request ID.
updateFolderPermissionsResponse_requestId :: Lens.Lens' UpdateFolderPermissionsResponse (Prelude.Maybe Prelude.Text)
updateFolderPermissionsResponse_requestId = Lens.lens (\UpdateFolderPermissionsResponse' {requestId} -> requestId) (\s@UpdateFolderPermissionsResponse' {} a -> s {requestId = a} :: UpdateFolderPermissionsResponse)

-- | The status. If succeeded, the status is @SC_OK@.
updateFolderPermissionsResponse_status :: Lens.Lens' UpdateFolderPermissionsResponse (Prelude.Maybe Prelude.Int)
updateFolderPermissionsResponse_status = Lens.lens (\UpdateFolderPermissionsResponse' {status} -> status) (\s@UpdateFolderPermissionsResponse' {} a -> s {status = a} :: UpdateFolderPermissionsResponse)

-- | The Amazon Resource Name (ARN).
updateFolderPermissionsResponse_arn :: Lens.Lens' UpdateFolderPermissionsResponse (Prelude.Maybe Prelude.Text)
updateFolderPermissionsResponse_arn = Lens.lens (\UpdateFolderPermissionsResponse' {arn} -> arn) (\s@UpdateFolderPermissionsResponse' {} a -> s {arn = a} :: UpdateFolderPermissionsResponse)

-- | The folder ID.
updateFolderPermissionsResponse_folderId :: Lens.Lens' UpdateFolderPermissionsResponse (Prelude.Maybe Prelude.Text)
updateFolderPermissionsResponse_folderId = Lens.lens (\UpdateFolderPermissionsResponse' {folderId} -> folderId) (\s@UpdateFolderPermissionsResponse' {} a -> s {folderId = a} :: UpdateFolderPermissionsResponse)

-- | Information about the permissions on the dashboard.
updateFolderPermissionsResponse_permissions :: Lens.Lens' UpdateFolderPermissionsResponse (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
updateFolderPermissionsResponse_permissions = Lens.lens (\UpdateFolderPermissionsResponse' {permissions} -> permissions) (\s@UpdateFolderPermissionsResponse' {} a -> s {permissions = a} :: UpdateFolderPermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateFolderPermissionsResponse_httpStatus :: Lens.Lens' UpdateFolderPermissionsResponse Prelude.Int
updateFolderPermissionsResponse_httpStatus = Lens.lens (\UpdateFolderPermissionsResponse' {httpStatus} -> httpStatus) (\s@UpdateFolderPermissionsResponse' {} a -> s {httpStatus = a} :: UpdateFolderPermissionsResponse)

instance
  Prelude.NFData
    UpdateFolderPermissionsResponse
  where
  rnf UpdateFolderPermissionsResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf folderId
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf httpStatus
