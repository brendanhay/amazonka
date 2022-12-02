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
-- Module      : Amazonka.WorkDocs.CreateFolder
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a folder with the specified name and parent folder.
module Amazonka.WorkDocs.CreateFolder
  ( -- * Creating a Request
    CreateFolder (..),
    newCreateFolder,

    -- * Request Lenses
    createFolder_name,
    createFolder_authenticationToken,
    createFolder_parentFolderId,

    -- * Destructuring the Response
    CreateFolderResponse (..),
    newCreateFolderResponse,

    -- * Response Lenses
    createFolderResponse_metadata,
    createFolderResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newCreateFolder' smart constructor.
data CreateFolder = CreateFolder'
  { -- | The name of the new folder.
    name :: Prelude.Maybe Prelude.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the parent folder.
    parentFolderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFolder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createFolder_name' - The name of the new folder.
--
-- 'authenticationToken', 'createFolder_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'parentFolderId', 'createFolder_parentFolderId' - The ID of the parent folder.
newCreateFolder ::
  -- | 'parentFolderId'
  Prelude.Text ->
  CreateFolder
newCreateFolder pParentFolderId_ =
  CreateFolder'
    { name = Prelude.Nothing,
      authenticationToken = Prelude.Nothing,
      parentFolderId = pParentFolderId_
    }

-- | The name of the new folder.
createFolder_name :: Lens.Lens' CreateFolder (Prelude.Maybe Prelude.Text)
createFolder_name = Lens.lens (\CreateFolder' {name} -> name) (\s@CreateFolder' {} a -> s {name = a} :: CreateFolder)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
createFolder_authenticationToken :: Lens.Lens' CreateFolder (Prelude.Maybe Prelude.Text)
createFolder_authenticationToken = Lens.lens (\CreateFolder' {authenticationToken} -> authenticationToken) (\s@CreateFolder' {} a -> s {authenticationToken = a} :: CreateFolder) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the parent folder.
createFolder_parentFolderId :: Lens.Lens' CreateFolder Prelude.Text
createFolder_parentFolderId = Lens.lens (\CreateFolder' {parentFolderId} -> parentFolderId) (\s@CreateFolder' {} a -> s {parentFolderId = a} :: CreateFolder)

instance Core.AWSRequest CreateFolder where
  type AWSResponse CreateFolder = CreateFolderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFolderResponse'
            Prelude.<$> (x Data..?> "Metadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFolder where
  hashWithSalt _salt CreateFolder' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` parentFolderId

instance Prelude.NFData CreateFolder where
  rnf CreateFolder' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf authenticationToken
      `Prelude.seq` Prelude.rnf parentFolderId

instance Data.ToHeaders CreateFolder where
  toHeaders CreateFolder' {..} =
    Prelude.mconcat
      [ "Authentication" Data.=# authenticationToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreateFolder where
  toJSON CreateFolder' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            Prelude.Just
              ("ParentFolderId" Data..= parentFolderId)
          ]
      )

instance Data.ToPath CreateFolder where
  toPath = Prelude.const "/api/v1/folders"

instance Data.ToQuery CreateFolder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFolderResponse' smart constructor.
data CreateFolderResponse = CreateFolderResponse'
  { -- | The metadata of the folder.
    metadata :: Prelude.Maybe FolderMetadata,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'metadata', 'createFolderResponse_metadata' - The metadata of the folder.
--
-- 'httpStatus', 'createFolderResponse_httpStatus' - The response's http status code.
newCreateFolderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFolderResponse
newCreateFolderResponse pHttpStatus_ =
  CreateFolderResponse'
    { metadata = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The metadata of the folder.
createFolderResponse_metadata :: Lens.Lens' CreateFolderResponse (Prelude.Maybe FolderMetadata)
createFolderResponse_metadata = Lens.lens (\CreateFolderResponse' {metadata} -> metadata) (\s@CreateFolderResponse' {} a -> s {metadata = a} :: CreateFolderResponse)

-- | The response's http status code.
createFolderResponse_httpStatus :: Lens.Lens' CreateFolderResponse Prelude.Int
createFolderResponse_httpStatus = Lens.lens (\CreateFolderResponse' {httpStatus} -> httpStatus) (\s@CreateFolderResponse' {} a -> s {httpStatus = a} :: CreateFolderResponse)

instance Prelude.NFData CreateFolderResponse where
  rnf CreateFolderResponse' {..} =
    Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf httpStatus
