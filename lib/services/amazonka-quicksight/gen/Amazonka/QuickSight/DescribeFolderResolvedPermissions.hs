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
-- Module      : Amazonka.QuickSight.DescribeFolderResolvedPermissions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the folder resolved permissions. Permissions consists of both
-- folder direct permissions and the inherited permissions from the
-- ancestor folders.
module Amazonka.QuickSight.DescribeFolderResolvedPermissions
  ( -- * Creating a Request
    DescribeFolderResolvedPermissions (..),
    newDescribeFolderResolvedPermissions,

    -- * Request Lenses
    describeFolderResolvedPermissions_awsAccountId,
    describeFolderResolvedPermissions_folderId,

    -- * Destructuring the Response
    DescribeFolderResolvedPermissionsResponse (..),
    newDescribeFolderResolvedPermissionsResponse,

    -- * Response Lenses
    describeFolderResolvedPermissionsResponse_arn,
    describeFolderResolvedPermissionsResponse_folderId,
    describeFolderResolvedPermissionsResponse_permissions,
    describeFolderResolvedPermissionsResponse_requestId,
    describeFolderResolvedPermissionsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFolderResolvedPermissions' smart constructor.
data DescribeFolderResolvedPermissions = DescribeFolderResolvedPermissions'
  { -- | The ID for the Amazon Web Services account that contains the folder.
    awsAccountId :: Prelude.Text,
    -- | The ID of the folder.
    folderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFolderResolvedPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeFolderResolvedPermissions_awsAccountId' - The ID for the Amazon Web Services account that contains the folder.
--
-- 'folderId', 'describeFolderResolvedPermissions_folderId' - The ID of the folder.
newDescribeFolderResolvedPermissions ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'folderId'
  Prelude.Text ->
  DescribeFolderResolvedPermissions
newDescribeFolderResolvedPermissions
  pAwsAccountId_
  pFolderId_ =
    DescribeFolderResolvedPermissions'
      { awsAccountId =
          pAwsAccountId_,
        folderId = pFolderId_
      }

-- | The ID for the Amazon Web Services account that contains the folder.
describeFolderResolvedPermissions_awsAccountId :: Lens.Lens' DescribeFolderResolvedPermissions Prelude.Text
describeFolderResolvedPermissions_awsAccountId = Lens.lens (\DescribeFolderResolvedPermissions' {awsAccountId} -> awsAccountId) (\s@DescribeFolderResolvedPermissions' {} a -> s {awsAccountId = a} :: DescribeFolderResolvedPermissions)

-- | The ID of the folder.
describeFolderResolvedPermissions_folderId :: Lens.Lens' DescribeFolderResolvedPermissions Prelude.Text
describeFolderResolvedPermissions_folderId = Lens.lens (\DescribeFolderResolvedPermissions' {folderId} -> folderId) (\s@DescribeFolderResolvedPermissions' {} a -> s {folderId = a} :: DescribeFolderResolvedPermissions)

instance
  Core.AWSRequest
    DescribeFolderResolvedPermissions
  where
  type
    AWSResponse DescribeFolderResolvedPermissions =
      DescribeFolderResolvedPermissionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFolderResolvedPermissionsResponse'
            Prelude.<$> (x Data..?> "Arn")
              Prelude.<*> (x Data..?> "FolderId")
              Prelude.<*> (x Data..?> "Permissions")
              Prelude.<*> (x Data..?> "RequestId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeFolderResolvedPermissions
  where
  hashWithSalt
    _salt
    DescribeFolderResolvedPermissions' {..} =
      _salt `Prelude.hashWithSalt` awsAccountId
        `Prelude.hashWithSalt` folderId

instance
  Prelude.NFData
    DescribeFolderResolvedPermissions
  where
  rnf DescribeFolderResolvedPermissions' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf folderId

instance
  Data.ToHeaders
    DescribeFolderResolvedPermissions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    DescribeFolderResolvedPermissions
  where
  toPath DescribeFolderResolvedPermissions' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/folders/",
        Data.toBS folderId,
        "/resolved-permissions"
      ]

instance
  Data.ToQuery
    DescribeFolderResolvedPermissions
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFolderResolvedPermissionsResponse' smart constructor.
data DescribeFolderResolvedPermissionsResponse = DescribeFolderResolvedPermissionsResponse'
  { -- | The Amazon Resource Name (ARN) of the folder.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the folder.
    folderId :: Prelude.Maybe Prelude.Text,
    -- | Information about the permissions for the folder.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFolderResolvedPermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeFolderResolvedPermissionsResponse_arn' - The Amazon Resource Name (ARN) of the folder.
--
-- 'folderId', 'describeFolderResolvedPermissionsResponse_folderId' - The ID of the folder.
--
-- 'permissions', 'describeFolderResolvedPermissionsResponse_permissions' - Information about the permissions for the folder.
--
-- 'requestId', 'describeFolderResolvedPermissionsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'describeFolderResolvedPermissionsResponse_status' - The HTTP status of the request.
newDescribeFolderResolvedPermissionsResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeFolderResolvedPermissionsResponse
newDescribeFolderResolvedPermissionsResponse pStatus_ =
  DescribeFolderResolvedPermissionsResponse'
    { arn =
        Prelude.Nothing,
      folderId = Prelude.Nothing,
      permissions = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the folder.
describeFolderResolvedPermissionsResponse_arn :: Lens.Lens' DescribeFolderResolvedPermissionsResponse (Prelude.Maybe Prelude.Text)
describeFolderResolvedPermissionsResponse_arn = Lens.lens (\DescribeFolderResolvedPermissionsResponse' {arn} -> arn) (\s@DescribeFolderResolvedPermissionsResponse' {} a -> s {arn = a} :: DescribeFolderResolvedPermissionsResponse)

-- | The ID of the folder.
describeFolderResolvedPermissionsResponse_folderId :: Lens.Lens' DescribeFolderResolvedPermissionsResponse (Prelude.Maybe Prelude.Text)
describeFolderResolvedPermissionsResponse_folderId = Lens.lens (\DescribeFolderResolvedPermissionsResponse' {folderId} -> folderId) (\s@DescribeFolderResolvedPermissionsResponse' {} a -> s {folderId = a} :: DescribeFolderResolvedPermissionsResponse)

-- | Information about the permissions for the folder.
describeFolderResolvedPermissionsResponse_permissions :: Lens.Lens' DescribeFolderResolvedPermissionsResponse (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
describeFolderResolvedPermissionsResponse_permissions = Lens.lens (\DescribeFolderResolvedPermissionsResponse' {permissions} -> permissions) (\s@DescribeFolderResolvedPermissionsResponse' {} a -> s {permissions = a} :: DescribeFolderResolvedPermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services request ID for this operation.
describeFolderResolvedPermissionsResponse_requestId :: Lens.Lens' DescribeFolderResolvedPermissionsResponse (Prelude.Maybe Prelude.Text)
describeFolderResolvedPermissionsResponse_requestId = Lens.lens (\DescribeFolderResolvedPermissionsResponse' {requestId} -> requestId) (\s@DescribeFolderResolvedPermissionsResponse' {} a -> s {requestId = a} :: DescribeFolderResolvedPermissionsResponse)

-- | The HTTP status of the request.
describeFolderResolvedPermissionsResponse_status :: Lens.Lens' DescribeFolderResolvedPermissionsResponse Prelude.Int
describeFolderResolvedPermissionsResponse_status = Lens.lens (\DescribeFolderResolvedPermissionsResponse' {status} -> status) (\s@DescribeFolderResolvedPermissionsResponse' {} a -> s {status = a} :: DescribeFolderResolvedPermissionsResponse)

instance
  Prelude.NFData
    DescribeFolderResolvedPermissionsResponse
  where
  rnf DescribeFolderResolvedPermissionsResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf folderId
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
