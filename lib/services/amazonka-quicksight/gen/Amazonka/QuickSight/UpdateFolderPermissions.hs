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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    updateFolderPermissions_grantPermissions,
    updateFolderPermissions_revokePermissions,
    updateFolderPermissions_awsAccountId,
    updateFolderPermissions_folderId,

    -- * Destructuring the Response
    UpdateFolderPermissionsResponse (..),
    newUpdateFolderPermissionsResponse,

    -- * Response Lenses
    updateFolderPermissionsResponse_arn,
    updateFolderPermissionsResponse_folderId,
    updateFolderPermissionsResponse_permissions,
    updateFolderPermissionsResponse_requestId,
    updateFolderPermissionsResponse_status,
    updateFolderPermissionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFolderPermissions' smart constructor.
data UpdateFolderPermissions = UpdateFolderPermissions'
  { -- | The permissions that you want to grant on a resource.
    grantPermissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The permissions that you want to revoke from a resource.
    revokePermissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The ID for the Amazon Web Services account that contains the folder to
    -- update.
    awsAccountId :: Prelude.Text,
    -- | The ID of the folder.
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
-- 'grantPermissions', 'updateFolderPermissions_grantPermissions' - The permissions that you want to grant on a resource.
--
-- 'revokePermissions', 'updateFolderPermissions_revokePermissions' - The permissions that you want to revoke from a resource.
--
-- 'awsAccountId', 'updateFolderPermissions_awsAccountId' - The ID for the Amazon Web Services account that contains the folder to
-- update.
--
-- 'folderId', 'updateFolderPermissions_folderId' - The ID of the folder.
newUpdateFolderPermissions ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'folderId'
  Prelude.Text ->
  UpdateFolderPermissions
newUpdateFolderPermissions pAwsAccountId_ pFolderId_ =
  UpdateFolderPermissions'
    { grantPermissions =
        Prelude.Nothing,
      revokePermissions = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      folderId = pFolderId_
    }

-- | The permissions that you want to grant on a resource.
updateFolderPermissions_grantPermissions :: Lens.Lens' UpdateFolderPermissions (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
updateFolderPermissions_grantPermissions = Lens.lens (\UpdateFolderPermissions' {grantPermissions} -> grantPermissions) (\s@UpdateFolderPermissions' {} a -> s {grantPermissions = a} :: UpdateFolderPermissions) Prelude.. Lens.mapping Lens.coerced

-- | The permissions that you want to revoke from a resource.
updateFolderPermissions_revokePermissions :: Lens.Lens' UpdateFolderPermissions (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
updateFolderPermissions_revokePermissions = Lens.lens (\UpdateFolderPermissions' {revokePermissions} -> revokePermissions) (\s@UpdateFolderPermissions' {} a -> s {revokePermissions = a} :: UpdateFolderPermissions) Prelude.. Lens.mapping Lens.coerced

-- | The ID for the Amazon Web Services account that contains the folder to
-- update.
updateFolderPermissions_awsAccountId :: Lens.Lens' UpdateFolderPermissions Prelude.Text
updateFolderPermissions_awsAccountId = Lens.lens (\UpdateFolderPermissions' {awsAccountId} -> awsAccountId) (\s@UpdateFolderPermissions' {} a -> s {awsAccountId = a} :: UpdateFolderPermissions)

-- | The ID of the folder.
updateFolderPermissions_folderId :: Lens.Lens' UpdateFolderPermissions Prelude.Text
updateFolderPermissions_folderId = Lens.lens (\UpdateFolderPermissions' {folderId} -> folderId) (\s@UpdateFolderPermissions' {} a -> s {folderId = a} :: UpdateFolderPermissions)

instance Core.AWSRequest UpdateFolderPermissions where
  type
    AWSResponse UpdateFolderPermissions =
      UpdateFolderPermissionsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFolderPermissionsResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "FolderId")
            Prelude.<*> (x Data..?> "Permissions")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFolderPermissions where
  hashWithSalt _salt UpdateFolderPermissions' {..} =
    _salt
      `Prelude.hashWithSalt` grantPermissions
      `Prelude.hashWithSalt` revokePermissions
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` folderId

instance Prelude.NFData UpdateFolderPermissions where
  rnf UpdateFolderPermissions' {..} =
    Prelude.rnf grantPermissions
      `Prelude.seq` Prelude.rnf revokePermissions
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf folderId

instance Data.ToHeaders UpdateFolderPermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFolderPermissions where
  toJSON UpdateFolderPermissions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GrantPermissions" Data..=)
              Prelude.<$> grantPermissions,
            ("RevokePermissions" Data..=)
              Prelude.<$> revokePermissions
          ]
      )

instance Data.ToPath UpdateFolderPermissions where
  toPath UpdateFolderPermissions' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/folders/",
        Data.toBS folderId,
        "/permissions"
      ]

instance Data.ToQuery UpdateFolderPermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFolderPermissionsResponse' smart constructor.
data UpdateFolderPermissionsResponse = UpdateFolderPermissionsResponse'
  { -- | The Amazon Resource Name (ARN) of the folder.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the folder.
    folderId :: Prelude.Maybe Prelude.Text,
    -- | Information about the permissions for the folder.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Maybe Prelude.Int,
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
-- 'arn', 'updateFolderPermissionsResponse_arn' - The Amazon Resource Name (ARN) of the folder.
--
-- 'folderId', 'updateFolderPermissionsResponse_folderId' - The ID of the folder.
--
-- 'permissions', 'updateFolderPermissionsResponse_permissions' - Information about the permissions for the folder.
--
-- 'requestId', 'updateFolderPermissionsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'updateFolderPermissionsResponse_status' - The HTTP status of the request.
--
-- 'httpStatus', 'updateFolderPermissionsResponse_httpStatus' - The response's http status code.
newUpdateFolderPermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFolderPermissionsResponse
newUpdateFolderPermissionsResponse pHttpStatus_ =
  UpdateFolderPermissionsResponse'
    { arn =
        Prelude.Nothing,
      folderId = Prelude.Nothing,
      permissions = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the folder.
updateFolderPermissionsResponse_arn :: Lens.Lens' UpdateFolderPermissionsResponse (Prelude.Maybe Prelude.Text)
updateFolderPermissionsResponse_arn = Lens.lens (\UpdateFolderPermissionsResponse' {arn} -> arn) (\s@UpdateFolderPermissionsResponse' {} a -> s {arn = a} :: UpdateFolderPermissionsResponse)

-- | The ID of the folder.
updateFolderPermissionsResponse_folderId :: Lens.Lens' UpdateFolderPermissionsResponse (Prelude.Maybe Prelude.Text)
updateFolderPermissionsResponse_folderId = Lens.lens (\UpdateFolderPermissionsResponse' {folderId} -> folderId) (\s@UpdateFolderPermissionsResponse' {} a -> s {folderId = a} :: UpdateFolderPermissionsResponse)

-- | Information about the permissions for the folder.
updateFolderPermissionsResponse_permissions :: Lens.Lens' UpdateFolderPermissionsResponse (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
updateFolderPermissionsResponse_permissions = Lens.lens (\UpdateFolderPermissionsResponse' {permissions} -> permissions) (\s@UpdateFolderPermissionsResponse' {} a -> s {permissions = a} :: UpdateFolderPermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services request ID for this operation.
updateFolderPermissionsResponse_requestId :: Lens.Lens' UpdateFolderPermissionsResponse (Prelude.Maybe Prelude.Text)
updateFolderPermissionsResponse_requestId = Lens.lens (\UpdateFolderPermissionsResponse' {requestId} -> requestId) (\s@UpdateFolderPermissionsResponse' {} a -> s {requestId = a} :: UpdateFolderPermissionsResponse)

-- | The HTTP status of the request.
updateFolderPermissionsResponse_status :: Lens.Lens' UpdateFolderPermissionsResponse (Prelude.Maybe Prelude.Int)
updateFolderPermissionsResponse_status = Lens.lens (\UpdateFolderPermissionsResponse' {status} -> status) (\s@UpdateFolderPermissionsResponse' {} a -> s {status = a} :: UpdateFolderPermissionsResponse)

-- | The response's http status code.
updateFolderPermissionsResponse_httpStatus :: Lens.Lens' UpdateFolderPermissionsResponse Prelude.Int
updateFolderPermissionsResponse_httpStatus = Lens.lens (\UpdateFolderPermissionsResponse' {httpStatus} -> httpStatus) (\s@UpdateFolderPermissionsResponse' {} a -> s {httpStatus = a} :: UpdateFolderPermissionsResponse)

instance
  Prelude.NFData
    UpdateFolderPermissionsResponse
  where
  rnf UpdateFolderPermissionsResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf folderId
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
