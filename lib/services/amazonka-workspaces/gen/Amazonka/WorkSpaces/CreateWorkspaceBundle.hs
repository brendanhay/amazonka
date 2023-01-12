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
-- Module      : Amazonka.WorkSpaces.CreateWorkspaceBundle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the specified WorkSpace bundle. For more information about
-- creating WorkSpace bundles, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/create-custom-bundle.html Create a Custom WorkSpaces Image and Bundle>.
module Amazonka.WorkSpaces.CreateWorkspaceBundle
  ( -- * Creating a Request
    CreateWorkspaceBundle (..),
    newCreateWorkspaceBundle,

    -- * Request Lenses
    createWorkspaceBundle_rootStorage,
    createWorkspaceBundle_tags,
    createWorkspaceBundle_bundleName,
    createWorkspaceBundle_bundleDescription,
    createWorkspaceBundle_imageId,
    createWorkspaceBundle_computeType,
    createWorkspaceBundle_userStorage,

    -- * Destructuring the Response
    CreateWorkspaceBundleResponse (..),
    newCreateWorkspaceBundleResponse,

    -- * Response Lenses
    createWorkspaceBundleResponse_workspaceBundle,
    createWorkspaceBundleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newCreateWorkspaceBundle' smart constructor.
data CreateWorkspaceBundle = CreateWorkspaceBundle'
  { rootStorage :: Prelude.Maybe RootStorage,
    -- | The tags associated with the bundle.
    --
    -- To add tags at the same time when you\'re creating the bundle, you must
    -- create an IAM policy that grants your IAM user permissions to use
    -- @workspaces:CreateTags@.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the bundle.
    bundleName :: Prelude.Text,
    -- | The description of the bundle.
    bundleDescription :: Prelude.Text,
    -- | The identifier of the image that is used to create the bundle.
    imageId :: Prelude.Text,
    computeType :: ComputeType,
    userStorage :: UserStorage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkspaceBundle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rootStorage', 'createWorkspaceBundle_rootStorage' - Undocumented member.
--
-- 'tags', 'createWorkspaceBundle_tags' - The tags associated with the bundle.
--
-- To add tags at the same time when you\'re creating the bundle, you must
-- create an IAM policy that grants your IAM user permissions to use
-- @workspaces:CreateTags@.
--
-- 'bundleName', 'createWorkspaceBundle_bundleName' - The name of the bundle.
--
-- 'bundleDescription', 'createWorkspaceBundle_bundleDescription' - The description of the bundle.
--
-- 'imageId', 'createWorkspaceBundle_imageId' - The identifier of the image that is used to create the bundle.
--
-- 'computeType', 'createWorkspaceBundle_computeType' - Undocumented member.
--
-- 'userStorage', 'createWorkspaceBundle_userStorage' - Undocumented member.
newCreateWorkspaceBundle ::
  -- | 'bundleName'
  Prelude.Text ->
  -- | 'bundleDescription'
  Prelude.Text ->
  -- | 'imageId'
  Prelude.Text ->
  -- | 'computeType'
  ComputeType ->
  -- | 'userStorage'
  UserStorage ->
  CreateWorkspaceBundle
newCreateWorkspaceBundle
  pBundleName_
  pBundleDescription_
  pImageId_
  pComputeType_
  pUserStorage_ =
    CreateWorkspaceBundle'
      { rootStorage =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        bundleName = pBundleName_,
        bundleDescription = pBundleDescription_,
        imageId = pImageId_,
        computeType = pComputeType_,
        userStorage = pUserStorage_
      }

-- | Undocumented member.
createWorkspaceBundle_rootStorage :: Lens.Lens' CreateWorkspaceBundle (Prelude.Maybe RootStorage)
createWorkspaceBundle_rootStorage = Lens.lens (\CreateWorkspaceBundle' {rootStorage} -> rootStorage) (\s@CreateWorkspaceBundle' {} a -> s {rootStorage = a} :: CreateWorkspaceBundle)

-- | The tags associated with the bundle.
--
-- To add tags at the same time when you\'re creating the bundle, you must
-- create an IAM policy that grants your IAM user permissions to use
-- @workspaces:CreateTags@.
createWorkspaceBundle_tags :: Lens.Lens' CreateWorkspaceBundle (Prelude.Maybe [Tag])
createWorkspaceBundle_tags = Lens.lens (\CreateWorkspaceBundle' {tags} -> tags) (\s@CreateWorkspaceBundle' {} a -> s {tags = a} :: CreateWorkspaceBundle) Prelude.. Lens.mapping Lens.coerced

-- | The name of the bundle.
createWorkspaceBundle_bundleName :: Lens.Lens' CreateWorkspaceBundle Prelude.Text
createWorkspaceBundle_bundleName = Lens.lens (\CreateWorkspaceBundle' {bundleName} -> bundleName) (\s@CreateWorkspaceBundle' {} a -> s {bundleName = a} :: CreateWorkspaceBundle)

-- | The description of the bundle.
createWorkspaceBundle_bundleDescription :: Lens.Lens' CreateWorkspaceBundle Prelude.Text
createWorkspaceBundle_bundleDescription = Lens.lens (\CreateWorkspaceBundle' {bundleDescription} -> bundleDescription) (\s@CreateWorkspaceBundle' {} a -> s {bundleDescription = a} :: CreateWorkspaceBundle)

-- | The identifier of the image that is used to create the bundle.
createWorkspaceBundle_imageId :: Lens.Lens' CreateWorkspaceBundle Prelude.Text
createWorkspaceBundle_imageId = Lens.lens (\CreateWorkspaceBundle' {imageId} -> imageId) (\s@CreateWorkspaceBundle' {} a -> s {imageId = a} :: CreateWorkspaceBundle)

-- | Undocumented member.
createWorkspaceBundle_computeType :: Lens.Lens' CreateWorkspaceBundle ComputeType
createWorkspaceBundle_computeType = Lens.lens (\CreateWorkspaceBundle' {computeType} -> computeType) (\s@CreateWorkspaceBundle' {} a -> s {computeType = a} :: CreateWorkspaceBundle)

-- | Undocumented member.
createWorkspaceBundle_userStorage :: Lens.Lens' CreateWorkspaceBundle UserStorage
createWorkspaceBundle_userStorage = Lens.lens (\CreateWorkspaceBundle' {userStorage} -> userStorage) (\s@CreateWorkspaceBundle' {} a -> s {userStorage = a} :: CreateWorkspaceBundle)

instance Core.AWSRequest CreateWorkspaceBundle where
  type
    AWSResponse CreateWorkspaceBundle =
      CreateWorkspaceBundleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorkspaceBundleResponse'
            Prelude.<$> (x Data..?> "WorkspaceBundle")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWorkspaceBundle where
  hashWithSalt _salt CreateWorkspaceBundle' {..} =
    _salt `Prelude.hashWithSalt` rootStorage
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` bundleName
      `Prelude.hashWithSalt` bundleDescription
      `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` computeType
      `Prelude.hashWithSalt` userStorage

instance Prelude.NFData CreateWorkspaceBundle where
  rnf CreateWorkspaceBundle' {..} =
    Prelude.rnf rootStorage
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf bundleName
      `Prelude.seq` Prelude.rnf bundleDescription
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf computeType
      `Prelude.seq` Prelude.rnf userStorage

instance Data.ToHeaders CreateWorkspaceBundle where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.CreateWorkspaceBundle" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateWorkspaceBundle where
  toJSON CreateWorkspaceBundle' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RootStorage" Data..=) Prelude.<$> rootStorage,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("BundleName" Data..= bundleName),
            Prelude.Just
              ("BundleDescription" Data..= bundleDescription),
            Prelude.Just ("ImageId" Data..= imageId),
            Prelude.Just ("ComputeType" Data..= computeType),
            Prelude.Just ("UserStorage" Data..= userStorage)
          ]
      )

instance Data.ToPath CreateWorkspaceBundle where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateWorkspaceBundle where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWorkspaceBundleResponse' smart constructor.
data CreateWorkspaceBundleResponse = CreateWorkspaceBundleResponse'
  { workspaceBundle :: Prelude.Maybe WorkspaceBundle,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkspaceBundleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceBundle', 'createWorkspaceBundleResponse_workspaceBundle' - Undocumented member.
--
-- 'httpStatus', 'createWorkspaceBundleResponse_httpStatus' - The response's http status code.
newCreateWorkspaceBundleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWorkspaceBundleResponse
newCreateWorkspaceBundleResponse pHttpStatus_ =
  CreateWorkspaceBundleResponse'
    { workspaceBundle =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createWorkspaceBundleResponse_workspaceBundle :: Lens.Lens' CreateWorkspaceBundleResponse (Prelude.Maybe WorkspaceBundle)
createWorkspaceBundleResponse_workspaceBundle = Lens.lens (\CreateWorkspaceBundleResponse' {workspaceBundle} -> workspaceBundle) (\s@CreateWorkspaceBundleResponse' {} a -> s {workspaceBundle = a} :: CreateWorkspaceBundleResponse)

-- | The response's http status code.
createWorkspaceBundleResponse_httpStatus :: Lens.Lens' CreateWorkspaceBundleResponse Prelude.Int
createWorkspaceBundleResponse_httpStatus = Lens.lens (\CreateWorkspaceBundleResponse' {httpStatus} -> httpStatus) (\s@CreateWorkspaceBundleResponse' {} a -> s {httpStatus = a} :: CreateWorkspaceBundleResponse)

instance Prelude.NFData CreateWorkspaceBundleResponse where
  rnf CreateWorkspaceBundleResponse' {..} =
    Prelude.rnf workspaceBundle
      `Prelude.seq` Prelude.rnf httpStatus
