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
-- Module      : Amazonka.WorkDocs.GetFolder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the metadata of the specified folder.
module Amazonka.WorkDocs.GetFolder
  ( -- * Creating a Request
    GetFolder (..),
    newGetFolder,

    -- * Request Lenses
    getFolder_authenticationToken,
    getFolder_includeCustomMetadata,
    getFolder_folderId,

    -- * Destructuring the Response
    GetFolderResponse (..),
    newGetFolderResponse,

    -- * Response Lenses
    getFolderResponse_customMetadata,
    getFolderResponse_metadata,
    getFolderResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newGetFolder' smart constructor.
data GetFolder = GetFolder'
  { -- | Amazon WorkDocs authentication token. Not required when using Amazon Web
    -- Services administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Set to TRUE to include custom metadata in the response.
    includeCustomMetadata :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the folder.
    folderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFolder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'getFolder_authenticationToken' - Amazon WorkDocs authentication token. Not required when using Amazon Web
-- Services administrator credentials to access the API.
--
-- 'includeCustomMetadata', 'getFolder_includeCustomMetadata' - Set to TRUE to include custom metadata in the response.
--
-- 'folderId', 'getFolder_folderId' - The ID of the folder.
newGetFolder ::
  -- | 'folderId'
  Prelude.Text ->
  GetFolder
newGetFolder pFolderId_ =
  GetFolder'
    { authenticationToken = Prelude.Nothing,
      includeCustomMetadata = Prelude.Nothing,
      folderId = pFolderId_
    }

-- | Amazon WorkDocs authentication token. Not required when using Amazon Web
-- Services administrator credentials to access the API.
getFolder_authenticationToken :: Lens.Lens' GetFolder (Prelude.Maybe Prelude.Text)
getFolder_authenticationToken = Lens.lens (\GetFolder' {authenticationToken} -> authenticationToken) (\s@GetFolder' {} a -> s {authenticationToken = a} :: GetFolder) Prelude.. Lens.mapping Data._Sensitive

-- | Set to TRUE to include custom metadata in the response.
getFolder_includeCustomMetadata :: Lens.Lens' GetFolder (Prelude.Maybe Prelude.Bool)
getFolder_includeCustomMetadata = Lens.lens (\GetFolder' {includeCustomMetadata} -> includeCustomMetadata) (\s@GetFolder' {} a -> s {includeCustomMetadata = a} :: GetFolder)

-- | The ID of the folder.
getFolder_folderId :: Lens.Lens' GetFolder Prelude.Text
getFolder_folderId = Lens.lens (\GetFolder' {folderId} -> folderId) (\s@GetFolder' {} a -> s {folderId = a} :: GetFolder)

instance Core.AWSRequest GetFolder where
  type AWSResponse GetFolder = GetFolderResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFolderResponse'
            Prelude.<$> (x Data..?> "CustomMetadata" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Metadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFolder where
  hashWithSalt _salt GetFolder' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` includeCustomMetadata
      `Prelude.hashWithSalt` folderId

instance Prelude.NFData GetFolder where
  rnf GetFolder' {..} =
    Prelude.rnf authenticationToken
      `Prelude.seq` Prelude.rnf includeCustomMetadata
      `Prelude.seq` Prelude.rnf folderId

instance Data.ToHeaders GetFolder where
  toHeaders GetFolder' {..} =
    Prelude.mconcat
      [ "Authentication" Data.=# authenticationToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath GetFolder where
  toPath GetFolder' {..} =
    Prelude.mconcat
      ["/api/v1/folders/", Data.toBS folderId]

instance Data.ToQuery GetFolder where
  toQuery GetFolder' {..} =
    Prelude.mconcat
      [ "includeCustomMetadata"
          Data.=: includeCustomMetadata
      ]

-- | /See:/ 'newGetFolderResponse' smart constructor.
data GetFolderResponse = GetFolderResponse'
  { -- | The custom metadata on the folder.
    customMetadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The metadata of the folder.
    metadata :: Prelude.Maybe FolderMetadata,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFolderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customMetadata', 'getFolderResponse_customMetadata' - The custom metadata on the folder.
--
-- 'metadata', 'getFolderResponse_metadata' - The metadata of the folder.
--
-- 'httpStatus', 'getFolderResponse_httpStatus' - The response's http status code.
newGetFolderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFolderResponse
newGetFolderResponse pHttpStatus_ =
  GetFolderResponse'
    { customMetadata =
        Prelude.Nothing,
      metadata = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The custom metadata on the folder.
getFolderResponse_customMetadata :: Lens.Lens' GetFolderResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getFolderResponse_customMetadata = Lens.lens (\GetFolderResponse' {customMetadata} -> customMetadata) (\s@GetFolderResponse' {} a -> s {customMetadata = a} :: GetFolderResponse) Prelude.. Lens.mapping Lens.coerced

-- | The metadata of the folder.
getFolderResponse_metadata :: Lens.Lens' GetFolderResponse (Prelude.Maybe FolderMetadata)
getFolderResponse_metadata = Lens.lens (\GetFolderResponse' {metadata} -> metadata) (\s@GetFolderResponse' {} a -> s {metadata = a} :: GetFolderResponse)

-- | The response's http status code.
getFolderResponse_httpStatus :: Lens.Lens' GetFolderResponse Prelude.Int
getFolderResponse_httpStatus = Lens.lens (\GetFolderResponse' {httpStatus} -> httpStatus) (\s@GetFolderResponse' {} a -> s {httpStatus = a} :: GetFolderResponse)

instance Prelude.NFData GetFolderResponse where
  rnf GetFolderResponse' {..} =
    Prelude.rnf customMetadata
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf httpStatus
