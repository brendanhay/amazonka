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
-- Module      : Amazonka.QuickSight.CreateFolder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an empty shared folder.
module Amazonka.QuickSight.CreateFolder
  ( -- * Creating a Request
    CreateFolder (..),
    newCreateFolder,

    -- * Request Lenses
    createFolder_folderType,
    createFolder_name,
    createFolder_parentFolderArn,
    createFolder_permissions,
    createFolder_tags,
    createFolder_awsAccountId,
    createFolder_folderId,

    -- * Destructuring the Response
    CreateFolderResponse (..),
    newCreateFolderResponse,

    -- * Response Lenses
    createFolderResponse_arn,
    createFolderResponse_folderId,
    createFolderResponse_requestId,
    createFolderResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFolder' smart constructor.
data CreateFolder = CreateFolder'
  { -- | The type of folder. By default, @folderType@ is @SHARED@.
    folderType :: Prelude.Maybe FolderType,
    -- | The name of the folder.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the parent folder.
    --
    -- @ParentFolderArn@ can be null. An empty @parentFolderArn@ creates a
    -- root-level folder.
    parentFolderArn :: Prelude.Maybe Prelude.Text,
    -- | A structure that describes the principals and the resource-level
    -- permissions of a folder.
    --
    -- To specify no permissions, omit @Permissions@.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | Tags for the folder.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The ID for the Amazon Web Services account where you want to create the
    -- folder.
    awsAccountId :: Prelude.Text,
    -- | The ID of the folder.
    folderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFolder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'folderType', 'createFolder_folderType' - The type of folder. By default, @folderType@ is @SHARED@.
--
-- 'name', 'createFolder_name' - The name of the folder.
--
-- 'parentFolderArn', 'createFolder_parentFolderArn' - The Amazon Resource Name (ARN) for the parent folder.
--
-- @ParentFolderArn@ can be null. An empty @parentFolderArn@ creates a
-- root-level folder.
--
-- 'permissions', 'createFolder_permissions' - A structure that describes the principals and the resource-level
-- permissions of a folder.
--
-- To specify no permissions, omit @Permissions@.
--
-- 'tags', 'createFolder_tags' - Tags for the folder.
--
-- 'awsAccountId', 'createFolder_awsAccountId' - The ID for the Amazon Web Services account where you want to create the
-- folder.
--
-- 'folderId', 'createFolder_folderId' - The ID of the folder.
newCreateFolder ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'folderId'
  Prelude.Text ->
  CreateFolder
newCreateFolder pAwsAccountId_ pFolderId_ =
  CreateFolder'
    { folderType = Prelude.Nothing,
      name = Prelude.Nothing,
      parentFolderArn = Prelude.Nothing,
      permissions = Prelude.Nothing,
      tags = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      folderId = pFolderId_
    }

-- | The type of folder. By default, @folderType@ is @SHARED@.
createFolder_folderType :: Lens.Lens' CreateFolder (Prelude.Maybe FolderType)
createFolder_folderType = Lens.lens (\CreateFolder' {folderType} -> folderType) (\s@CreateFolder' {} a -> s {folderType = a} :: CreateFolder)

-- | The name of the folder.
createFolder_name :: Lens.Lens' CreateFolder (Prelude.Maybe Prelude.Text)
createFolder_name = Lens.lens (\CreateFolder' {name} -> name) (\s@CreateFolder' {} a -> s {name = a} :: CreateFolder)

-- | The Amazon Resource Name (ARN) for the parent folder.
--
-- @ParentFolderArn@ can be null. An empty @parentFolderArn@ creates a
-- root-level folder.
createFolder_parentFolderArn :: Lens.Lens' CreateFolder (Prelude.Maybe Prelude.Text)
createFolder_parentFolderArn = Lens.lens (\CreateFolder' {parentFolderArn} -> parentFolderArn) (\s@CreateFolder' {} a -> s {parentFolderArn = a} :: CreateFolder)

-- | A structure that describes the principals and the resource-level
-- permissions of a folder.
--
-- To specify no permissions, omit @Permissions@.
createFolder_permissions :: Lens.Lens' CreateFolder (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
createFolder_permissions = Lens.lens (\CreateFolder' {permissions} -> permissions) (\s@CreateFolder' {} a -> s {permissions = a} :: CreateFolder) Prelude.. Lens.mapping Lens.coerced

-- | Tags for the folder.
createFolder_tags :: Lens.Lens' CreateFolder (Prelude.Maybe (Prelude.NonEmpty Tag))
createFolder_tags = Lens.lens (\CreateFolder' {tags} -> tags) (\s@CreateFolder' {} a -> s {tags = a} :: CreateFolder) Prelude.. Lens.mapping Lens.coerced

-- | The ID for the Amazon Web Services account where you want to create the
-- folder.
createFolder_awsAccountId :: Lens.Lens' CreateFolder Prelude.Text
createFolder_awsAccountId = Lens.lens (\CreateFolder' {awsAccountId} -> awsAccountId) (\s@CreateFolder' {} a -> s {awsAccountId = a} :: CreateFolder)

-- | The ID of the folder.
createFolder_folderId :: Lens.Lens' CreateFolder Prelude.Text
createFolder_folderId = Lens.lens (\CreateFolder' {folderId} -> folderId) (\s@CreateFolder' {} a -> s {folderId = a} :: CreateFolder)

instance Core.AWSRequest CreateFolder where
  type AWSResponse CreateFolder = CreateFolderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFolderResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "FolderId")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFolder where
  hashWithSalt _salt CreateFolder' {..} =
    _salt `Prelude.hashWithSalt` folderType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` parentFolderArn
      `Prelude.hashWithSalt` permissions
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` folderId

instance Prelude.NFData CreateFolder where
  rnf CreateFolder' {..} =
    Prelude.rnf folderType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf parentFolderArn
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf folderId

instance Data.ToHeaders CreateFolder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFolder where
  toJSON CreateFolder' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FolderType" Data..=) Prelude.<$> folderType,
            ("Name" Data..=) Prelude.<$> name,
            ("ParentFolderArn" Data..=)
              Prelude.<$> parentFolderArn,
            ("Permissions" Data..=) Prelude.<$> permissions,
            ("Tags" Data..=) Prelude.<$> tags
          ]
      )

instance Data.ToPath CreateFolder where
  toPath CreateFolder' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/folders/",
        Data.toBS folderId
      ]

instance Data.ToQuery CreateFolder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFolderResponse' smart constructor.
data CreateFolderResponse = CreateFolderResponse'
  { -- | The Amazon Resource Name (ARN) for the newly created folder.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The folder ID for the newly created folder.
    folderId :: Prelude.Maybe Prelude.Text,
    -- | The request ID for the newly created folder.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFolderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createFolderResponse_arn' - The Amazon Resource Name (ARN) for the newly created folder.
--
-- 'folderId', 'createFolderResponse_folderId' - The folder ID for the newly created folder.
--
-- 'requestId', 'createFolderResponse_requestId' - The request ID for the newly created folder.
--
-- 'status', 'createFolderResponse_status' - The HTTP status of the request.
newCreateFolderResponse ::
  -- | 'status'
  Prelude.Int ->
  CreateFolderResponse
newCreateFolderResponse pStatus_ =
  CreateFolderResponse'
    { arn = Prelude.Nothing,
      folderId = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) for the newly created folder.
createFolderResponse_arn :: Lens.Lens' CreateFolderResponse (Prelude.Maybe Prelude.Text)
createFolderResponse_arn = Lens.lens (\CreateFolderResponse' {arn} -> arn) (\s@CreateFolderResponse' {} a -> s {arn = a} :: CreateFolderResponse)

-- | The folder ID for the newly created folder.
createFolderResponse_folderId :: Lens.Lens' CreateFolderResponse (Prelude.Maybe Prelude.Text)
createFolderResponse_folderId = Lens.lens (\CreateFolderResponse' {folderId} -> folderId) (\s@CreateFolderResponse' {} a -> s {folderId = a} :: CreateFolderResponse)

-- | The request ID for the newly created folder.
createFolderResponse_requestId :: Lens.Lens' CreateFolderResponse (Prelude.Maybe Prelude.Text)
createFolderResponse_requestId = Lens.lens (\CreateFolderResponse' {requestId} -> requestId) (\s@CreateFolderResponse' {} a -> s {requestId = a} :: CreateFolderResponse)

-- | The HTTP status of the request.
createFolderResponse_status :: Lens.Lens' CreateFolderResponse Prelude.Int
createFolderResponse_status = Lens.lens (\CreateFolderResponse' {status} -> status) (\s@CreateFolderResponse' {} a -> s {status = a} :: CreateFolderResponse)

instance Prelude.NFData CreateFolderResponse where
  rnf CreateFolderResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf folderId
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
