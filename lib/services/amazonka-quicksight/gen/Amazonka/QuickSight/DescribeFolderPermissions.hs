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
-- Module      : Amazonka.QuickSight.DescribeFolderPermissions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes permissions for a folder.
module Amazonka.QuickSight.DescribeFolderPermissions
  ( -- * Creating a Request
    DescribeFolderPermissions (..),
    newDescribeFolderPermissions,

    -- * Request Lenses
    describeFolderPermissions_awsAccountId,
    describeFolderPermissions_folderId,

    -- * Destructuring the Response
    DescribeFolderPermissionsResponse (..),
    newDescribeFolderPermissionsResponse,

    -- * Response Lenses
    describeFolderPermissionsResponse_requestId,
    describeFolderPermissionsResponse_arn,
    describeFolderPermissionsResponse_permissions,
    describeFolderPermissionsResponse_folderId,
    describeFolderPermissionsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFolderPermissions' smart constructor.
data DescribeFolderPermissions = DescribeFolderPermissions'
  { -- | The ID for the Amazon Web Services account that contains the folder.
    awsAccountId :: Prelude.Text,
    -- | The ID of the folder.
    folderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFolderPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeFolderPermissions_awsAccountId' - The ID for the Amazon Web Services account that contains the folder.
--
-- 'folderId', 'describeFolderPermissions_folderId' - The ID of the folder.
newDescribeFolderPermissions ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'folderId'
  Prelude.Text ->
  DescribeFolderPermissions
newDescribeFolderPermissions
  pAwsAccountId_
  pFolderId_ =
    DescribeFolderPermissions'
      { awsAccountId =
          pAwsAccountId_,
        folderId = pFolderId_
      }

-- | The ID for the Amazon Web Services account that contains the folder.
describeFolderPermissions_awsAccountId :: Lens.Lens' DescribeFolderPermissions Prelude.Text
describeFolderPermissions_awsAccountId = Lens.lens (\DescribeFolderPermissions' {awsAccountId} -> awsAccountId) (\s@DescribeFolderPermissions' {} a -> s {awsAccountId = a} :: DescribeFolderPermissions)

-- | The ID of the folder.
describeFolderPermissions_folderId :: Lens.Lens' DescribeFolderPermissions Prelude.Text
describeFolderPermissions_folderId = Lens.lens (\DescribeFolderPermissions' {folderId} -> folderId) (\s@DescribeFolderPermissions' {} a -> s {folderId = a} :: DescribeFolderPermissions)

instance Core.AWSRequest DescribeFolderPermissions where
  type
    AWSResponse DescribeFolderPermissions =
      DescribeFolderPermissionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFolderPermissionsResponse'
            Prelude.<$> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Permissions")
            Prelude.<*> (x Core..?> "FolderId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFolderPermissions where
  hashWithSalt _salt DescribeFolderPermissions' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` folderId

instance Prelude.NFData DescribeFolderPermissions where
  rnf DescribeFolderPermissions' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf folderId

instance Core.ToHeaders DescribeFolderPermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeFolderPermissions where
  toPath DescribeFolderPermissions' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/folders/",
        Core.toBS folderId,
        "/permissions"
      ]

instance Core.ToQuery DescribeFolderPermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFolderPermissionsResponse' smart constructor.
data DescribeFolderPermissionsResponse = DescribeFolderPermissionsResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the folder.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Information about the permissions on the folder.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The ID of the folder.
    folderId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFolderPermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'describeFolderPermissionsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'arn', 'describeFolderPermissionsResponse_arn' - The Amazon Resource Name (ARN) for the folder.
--
-- 'permissions', 'describeFolderPermissionsResponse_permissions' - Information about the permissions on the folder.
--
-- 'folderId', 'describeFolderPermissionsResponse_folderId' - The ID of the folder.
--
-- 'status', 'describeFolderPermissionsResponse_status' - The HTTP status of the request.
newDescribeFolderPermissionsResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeFolderPermissionsResponse
newDescribeFolderPermissionsResponse pStatus_ =
  DescribeFolderPermissionsResponse'
    { requestId =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      permissions = Prelude.Nothing,
      folderId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
describeFolderPermissionsResponse_requestId :: Lens.Lens' DescribeFolderPermissionsResponse (Prelude.Maybe Prelude.Text)
describeFolderPermissionsResponse_requestId = Lens.lens (\DescribeFolderPermissionsResponse' {requestId} -> requestId) (\s@DescribeFolderPermissionsResponse' {} a -> s {requestId = a} :: DescribeFolderPermissionsResponse)

-- | The Amazon Resource Name (ARN) for the folder.
describeFolderPermissionsResponse_arn :: Lens.Lens' DescribeFolderPermissionsResponse (Prelude.Maybe Prelude.Text)
describeFolderPermissionsResponse_arn = Lens.lens (\DescribeFolderPermissionsResponse' {arn} -> arn) (\s@DescribeFolderPermissionsResponse' {} a -> s {arn = a} :: DescribeFolderPermissionsResponse)

-- | Information about the permissions on the folder.
describeFolderPermissionsResponse_permissions :: Lens.Lens' DescribeFolderPermissionsResponse (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
describeFolderPermissionsResponse_permissions = Lens.lens (\DescribeFolderPermissionsResponse' {permissions} -> permissions) (\s@DescribeFolderPermissionsResponse' {} a -> s {permissions = a} :: DescribeFolderPermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the folder.
describeFolderPermissionsResponse_folderId :: Lens.Lens' DescribeFolderPermissionsResponse (Prelude.Maybe Prelude.Text)
describeFolderPermissionsResponse_folderId = Lens.lens (\DescribeFolderPermissionsResponse' {folderId} -> folderId) (\s@DescribeFolderPermissionsResponse' {} a -> s {folderId = a} :: DescribeFolderPermissionsResponse)

-- | The HTTP status of the request.
describeFolderPermissionsResponse_status :: Lens.Lens' DescribeFolderPermissionsResponse Prelude.Int
describeFolderPermissionsResponse_status = Lens.lens (\DescribeFolderPermissionsResponse' {status} -> status) (\s@DescribeFolderPermissionsResponse' {} a -> s {status = a} :: DescribeFolderPermissionsResponse)

instance
  Prelude.NFData
    DescribeFolderPermissionsResponse
  where
  rnf DescribeFolderPermissionsResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf folderId
      `Prelude.seq` Prelude.rnf status
