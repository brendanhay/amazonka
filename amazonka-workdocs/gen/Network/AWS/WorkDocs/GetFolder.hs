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
-- Module      : Network.AWS.WorkDocs.GetFolder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the metadata of the specified folder.
module Network.AWS.WorkDocs.GetFolder
  ( -- * Creating a Request
    GetFolder (..),
    newGetFolder,

    -- * Request Lenses
    getFolder_includeCustomMetadata,
    getFolder_authenticationToken,
    getFolder_folderId,

    -- * Destructuring the Response
    GetFolderResponse (..),
    newGetFolderResponse,

    -- * Response Lenses
    getFolderResponse_metadata,
    getFolderResponse_customMetadata,
    getFolderResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newGetFolder' smart constructor.
data GetFolder = GetFolder'
  { -- | Set to TRUE to include custom metadata in the response.
    includeCustomMetadata :: Core.Maybe Core.Bool,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The ID of the folder.
    folderId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetFolder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeCustomMetadata', 'getFolder_includeCustomMetadata' - Set to TRUE to include custom metadata in the response.
--
-- 'authenticationToken', 'getFolder_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'folderId', 'getFolder_folderId' - The ID of the folder.
newGetFolder ::
  -- | 'folderId'
  Core.Text ->
  GetFolder
newGetFolder pFolderId_ =
  GetFolder'
    { includeCustomMetadata = Core.Nothing,
      authenticationToken = Core.Nothing,
      folderId = pFolderId_
    }

-- | Set to TRUE to include custom metadata in the response.
getFolder_includeCustomMetadata :: Lens.Lens' GetFolder (Core.Maybe Core.Bool)
getFolder_includeCustomMetadata = Lens.lens (\GetFolder' {includeCustomMetadata} -> includeCustomMetadata) (\s@GetFolder' {} a -> s {includeCustomMetadata = a} :: GetFolder)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
getFolder_authenticationToken :: Lens.Lens' GetFolder (Core.Maybe Core.Text)
getFolder_authenticationToken = Lens.lens (\GetFolder' {authenticationToken} -> authenticationToken) (\s@GetFolder' {} a -> s {authenticationToken = a} :: GetFolder) Core.. Lens.mapping Core._Sensitive

-- | The ID of the folder.
getFolder_folderId :: Lens.Lens' GetFolder Core.Text
getFolder_folderId = Lens.lens (\GetFolder' {folderId} -> folderId) (\s@GetFolder' {} a -> s {folderId = a} :: GetFolder)

instance Core.AWSRequest GetFolder where
  type AWSResponse GetFolder = GetFolderResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFolderResponse'
            Core.<$> (x Core..?> "Metadata")
            Core.<*> (x Core..?> "CustomMetadata" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetFolder

instance Core.NFData GetFolder

instance Core.ToHeaders GetFolder where
  toHeaders GetFolder' {..} =
    Core.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToPath GetFolder where
  toPath GetFolder' {..} =
    Core.mconcat
      ["/api/v1/folders/", Core.toBS folderId]

instance Core.ToQuery GetFolder where
  toQuery GetFolder' {..} =
    Core.mconcat
      [ "includeCustomMetadata"
          Core.=: includeCustomMetadata
      ]

-- | /See:/ 'newGetFolderResponse' smart constructor.
data GetFolderResponse = GetFolderResponse'
  { -- | The metadata of the folder.
    metadata :: Core.Maybe FolderMetadata,
    -- | The custom metadata on the folder.
    customMetadata :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetFolderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadata', 'getFolderResponse_metadata' - The metadata of the folder.
--
-- 'customMetadata', 'getFolderResponse_customMetadata' - The custom metadata on the folder.
--
-- 'httpStatus', 'getFolderResponse_httpStatus' - The response's http status code.
newGetFolderResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetFolderResponse
newGetFolderResponse pHttpStatus_ =
  GetFolderResponse'
    { metadata = Core.Nothing,
      customMetadata = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The metadata of the folder.
getFolderResponse_metadata :: Lens.Lens' GetFolderResponse (Core.Maybe FolderMetadata)
getFolderResponse_metadata = Lens.lens (\GetFolderResponse' {metadata} -> metadata) (\s@GetFolderResponse' {} a -> s {metadata = a} :: GetFolderResponse)

-- | The custom metadata on the folder.
getFolderResponse_customMetadata :: Lens.Lens' GetFolderResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
getFolderResponse_customMetadata = Lens.lens (\GetFolderResponse' {customMetadata} -> customMetadata) (\s@GetFolderResponse' {} a -> s {customMetadata = a} :: GetFolderResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getFolderResponse_httpStatus :: Lens.Lens' GetFolderResponse Core.Int
getFolderResponse_httpStatus = Lens.lens (\GetFolderResponse' {httpStatus} -> httpStatus) (\s@GetFolderResponse' {} a -> s {httpStatus = a} :: GetFolderResponse)

instance Core.NFData GetFolderResponse
