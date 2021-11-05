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
-- Module      : Network.AWS.QuickSight.CreateFolder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an empty shared folder.
module Network.AWS.QuickSight.CreateFolder
  ( -- * Creating a Request
    CreateFolder (..),
    newCreateFolder,

    -- * Request Lenses
    createFolder_parentFolderArn,
    createFolder_name,
    createFolder_permissions,
    createFolder_folderType,
    createFolder_tags,
    createFolder_awsAccountId,
    createFolder_folderId,

    -- * Destructuring the Response
    CreateFolderResponse (..),
    newCreateFolderResponse,

    -- * Response Lenses
    createFolderResponse_requestId,
    createFolderResponse_arn,
    createFolderResponse_folderId,
    createFolderResponse_status,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QuickSight.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateFolder' smart constructor.
data CreateFolder = CreateFolder'
  { -- | The Amazon Resource Name (ARN) for the parent folder.
    --
    -- @ParentFolderArn@ can be null. An empty @parentFolderArn@ creates a
    -- root-level folder.
    parentFolderArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the folder.
    name :: Prelude.Maybe Prelude.Text,
    -- | A structure that describes the principals and the resource-level
    -- permissions of a folder.
    --
    -- To specify no permissions, omit @Permissions@.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The type of folder. By default, @folderType@ is @SHARED@.
    folderType :: Prelude.Maybe FolderType,
    -- | Tags for the folder.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The AWS Account ID.
    awsAccountId :: Prelude.Text,
    -- | The folder ID.
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
-- 'parentFolderArn', 'createFolder_parentFolderArn' - The Amazon Resource Name (ARN) for the parent folder.
--
-- @ParentFolderArn@ can be null. An empty @parentFolderArn@ creates a
-- root-level folder.
--
-- 'name', 'createFolder_name' - The name of the folder.
--
-- 'permissions', 'createFolder_permissions' - A structure that describes the principals and the resource-level
-- permissions of a folder.
--
-- To specify no permissions, omit @Permissions@.
--
-- 'folderType', 'createFolder_folderType' - The type of folder. By default, @folderType@ is @SHARED@.
--
-- 'tags', 'createFolder_tags' - Tags for the folder.
--
-- 'awsAccountId', 'createFolder_awsAccountId' - The AWS Account ID.
--
-- 'folderId', 'createFolder_folderId' - The folder ID.
newCreateFolder ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'folderId'
  Prelude.Text ->
  CreateFolder
newCreateFolder pAwsAccountId_ pFolderId_ =
  CreateFolder'
    { parentFolderArn = Prelude.Nothing,
      name = Prelude.Nothing,
      permissions = Prelude.Nothing,
      folderType = Prelude.Nothing,
      tags = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      folderId = pFolderId_
    }

-- | The Amazon Resource Name (ARN) for the parent folder.
--
-- @ParentFolderArn@ can be null. An empty @parentFolderArn@ creates a
-- root-level folder.
createFolder_parentFolderArn :: Lens.Lens' CreateFolder (Prelude.Maybe Prelude.Text)
createFolder_parentFolderArn = Lens.lens (\CreateFolder' {parentFolderArn} -> parentFolderArn) (\s@CreateFolder' {} a -> s {parentFolderArn = a} :: CreateFolder)

-- | The name of the folder.
createFolder_name :: Lens.Lens' CreateFolder (Prelude.Maybe Prelude.Text)
createFolder_name = Lens.lens (\CreateFolder' {name} -> name) (\s@CreateFolder' {} a -> s {name = a} :: CreateFolder)

-- | A structure that describes the principals and the resource-level
-- permissions of a folder.
--
-- To specify no permissions, omit @Permissions@.
createFolder_permissions :: Lens.Lens' CreateFolder (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
createFolder_permissions = Lens.lens (\CreateFolder' {permissions} -> permissions) (\s@CreateFolder' {} a -> s {permissions = a} :: CreateFolder) Prelude.. Lens.mapping Lens.coerced

-- | The type of folder. By default, @folderType@ is @SHARED@.
createFolder_folderType :: Lens.Lens' CreateFolder (Prelude.Maybe FolderType)
createFolder_folderType = Lens.lens (\CreateFolder' {folderType} -> folderType) (\s@CreateFolder' {} a -> s {folderType = a} :: CreateFolder)

-- | Tags for the folder.
createFolder_tags :: Lens.Lens' CreateFolder (Prelude.Maybe (Prelude.NonEmpty Tag))
createFolder_tags = Lens.lens (\CreateFolder' {tags} -> tags) (\s@CreateFolder' {} a -> s {tags = a} :: CreateFolder) Prelude.. Lens.mapping Lens.coerced

-- | The AWS Account ID.
createFolder_awsAccountId :: Lens.Lens' CreateFolder Prelude.Text
createFolder_awsAccountId = Lens.lens (\CreateFolder' {awsAccountId} -> awsAccountId) (\s@CreateFolder' {} a -> s {awsAccountId = a} :: CreateFolder)

-- | The folder ID.
createFolder_folderId :: Lens.Lens' CreateFolder Prelude.Text
createFolder_folderId = Lens.lens (\CreateFolder' {folderId} -> folderId) (\s@CreateFolder' {} a -> s {folderId = a} :: CreateFolder)

instance Core.AWSRequest CreateFolder where
  type AWSResponse CreateFolder = CreateFolderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFolderResponse'
            Prelude.<$> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "FolderId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFolder

instance Prelude.NFData CreateFolder

instance Core.ToHeaders CreateFolder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateFolder where
  toJSON CreateFolder' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ParentFolderArn" Core..=)
              Prelude.<$> parentFolderArn,
            ("Name" Core..=) Prelude.<$> name,
            ("Permissions" Core..=) Prelude.<$> permissions,
            ("FolderType" Core..=) Prelude.<$> folderType,
            ("Tags" Core..=) Prelude.<$> tags
          ]
      )

instance Core.ToPath CreateFolder where
  toPath CreateFolder' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/folders/",
        Core.toBS folderId
      ]

instance Core.ToQuery CreateFolder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFolderResponse' smart constructor.
data CreateFolderResponse = CreateFolderResponse'
  { -- | The request ID for the newly created folder.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the newly created folder.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The folder ID for the newly created folder.
    folderId :: Prelude.Maybe Prelude.Text,
    -- | The status of the newly created folder. If succeeded, the status is
    -- @SC_OK (200)@.
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
-- 'requestId', 'createFolderResponse_requestId' - The request ID for the newly created folder.
--
-- 'arn', 'createFolderResponse_arn' - The Amazon Resource Name (ARN) for the newly created folder.
--
-- 'folderId', 'createFolderResponse_folderId' - The folder ID for the newly created folder.
--
-- 'status', 'createFolderResponse_status' - The status of the newly created folder. If succeeded, the status is
-- @SC_OK (200)@.
newCreateFolderResponse ::
  -- | 'status'
  Prelude.Int ->
  CreateFolderResponse
newCreateFolderResponse pStatus_ =
  CreateFolderResponse'
    { requestId = Prelude.Nothing,
      arn = Prelude.Nothing,
      folderId = Prelude.Nothing,
      status = pStatus_
    }

-- | The request ID for the newly created folder.
createFolderResponse_requestId :: Lens.Lens' CreateFolderResponse (Prelude.Maybe Prelude.Text)
createFolderResponse_requestId = Lens.lens (\CreateFolderResponse' {requestId} -> requestId) (\s@CreateFolderResponse' {} a -> s {requestId = a} :: CreateFolderResponse)

-- | The Amazon Resource Name (ARN) for the newly created folder.
createFolderResponse_arn :: Lens.Lens' CreateFolderResponse (Prelude.Maybe Prelude.Text)
createFolderResponse_arn = Lens.lens (\CreateFolderResponse' {arn} -> arn) (\s@CreateFolderResponse' {} a -> s {arn = a} :: CreateFolderResponse)

-- | The folder ID for the newly created folder.
createFolderResponse_folderId :: Lens.Lens' CreateFolderResponse (Prelude.Maybe Prelude.Text)
createFolderResponse_folderId = Lens.lens (\CreateFolderResponse' {folderId} -> folderId) (\s@CreateFolderResponse' {} a -> s {folderId = a} :: CreateFolderResponse)

-- | The status of the newly created folder. If succeeded, the status is
-- @SC_OK (200)@.
createFolderResponse_status :: Lens.Lens' CreateFolderResponse Prelude.Int
createFolderResponse_status = Lens.lens (\CreateFolderResponse' {status} -> status) (\s@CreateFolderResponse' {} a -> s {status = a} :: CreateFolderResponse)

instance Prelude.NFData CreateFolderResponse
