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
-- Module      : Network.AWS.WorkDocs.GetDocumentVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves version metadata for the specified document.
module Network.AWS.WorkDocs.GetDocumentVersion
  ( -- * Creating a Request
    GetDocumentVersion (..),
    newGetDocumentVersion,

    -- * Request Lenses
    getDocumentVersion_includeCustomMetadata,
    getDocumentVersion_fields,
    getDocumentVersion_authenticationToken,
    getDocumentVersion_documentId,
    getDocumentVersion_versionId,

    -- * Destructuring the Response
    GetDocumentVersionResponse (..),
    newGetDocumentVersionResponse,

    -- * Response Lenses
    getDocumentVersionResponse_metadata,
    getDocumentVersionResponse_customMetadata,
    getDocumentVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newGetDocumentVersion' smart constructor.
data GetDocumentVersion = GetDocumentVersion'
  { -- | Set this to TRUE to include custom metadata in the response.
    includeCustomMetadata :: Core.Maybe Core.Bool,
    -- | A comma-separated list of values. Specify \"SOURCE\" to include a URL
    -- for the source document.
    fields :: Core.Maybe Core.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The ID of the document.
    documentId :: Core.Text,
    -- | The version ID of the document.
    versionId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDocumentVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeCustomMetadata', 'getDocumentVersion_includeCustomMetadata' - Set this to TRUE to include custom metadata in the response.
--
-- 'fields', 'getDocumentVersion_fields' - A comma-separated list of values. Specify \"SOURCE\" to include a URL
-- for the source document.
--
-- 'authenticationToken', 'getDocumentVersion_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'documentId', 'getDocumentVersion_documentId' - The ID of the document.
--
-- 'versionId', 'getDocumentVersion_versionId' - The version ID of the document.
newGetDocumentVersion ::
  -- | 'documentId'
  Core.Text ->
  -- | 'versionId'
  Core.Text ->
  GetDocumentVersion
newGetDocumentVersion pDocumentId_ pVersionId_ =
  GetDocumentVersion'
    { includeCustomMetadata =
        Core.Nothing,
      fields = Core.Nothing,
      authenticationToken = Core.Nothing,
      documentId = pDocumentId_,
      versionId = pVersionId_
    }

-- | Set this to TRUE to include custom metadata in the response.
getDocumentVersion_includeCustomMetadata :: Lens.Lens' GetDocumentVersion (Core.Maybe Core.Bool)
getDocumentVersion_includeCustomMetadata = Lens.lens (\GetDocumentVersion' {includeCustomMetadata} -> includeCustomMetadata) (\s@GetDocumentVersion' {} a -> s {includeCustomMetadata = a} :: GetDocumentVersion)

-- | A comma-separated list of values. Specify \"SOURCE\" to include a URL
-- for the source document.
getDocumentVersion_fields :: Lens.Lens' GetDocumentVersion (Core.Maybe Core.Text)
getDocumentVersion_fields = Lens.lens (\GetDocumentVersion' {fields} -> fields) (\s@GetDocumentVersion' {} a -> s {fields = a} :: GetDocumentVersion)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
getDocumentVersion_authenticationToken :: Lens.Lens' GetDocumentVersion (Core.Maybe Core.Text)
getDocumentVersion_authenticationToken = Lens.lens (\GetDocumentVersion' {authenticationToken} -> authenticationToken) (\s@GetDocumentVersion' {} a -> s {authenticationToken = a} :: GetDocumentVersion) Core.. Lens.mapping Core._Sensitive

-- | The ID of the document.
getDocumentVersion_documentId :: Lens.Lens' GetDocumentVersion Core.Text
getDocumentVersion_documentId = Lens.lens (\GetDocumentVersion' {documentId} -> documentId) (\s@GetDocumentVersion' {} a -> s {documentId = a} :: GetDocumentVersion)

-- | The version ID of the document.
getDocumentVersion_versionId :: Lens.Lens' GetDocumentVersion Core.Text
getDocumentVersion_versionId = Lens.lens (\GetDocumentVersion' {versionId} -> versionId) (\s@GetDocumentVersion' {} a -> s {versionId = a} :: GetDocumentVersion)

instance Core.AWSRequest GetDocumentVersion where
  type
    AWSResponse GetDocumentVersion =
      GetDocumentVersionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDocumentVersionResponse'
            Core.<$> (x Core..?> "Metadata")
            Core.<*> (x Core..?> "CustomMetadata" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDocumentVersion

instance Core.NFData GetDocumentVersion

instance Core.ToHeaders GetDocumentVersion where
  toHeaders GetDocumentVersion' {..} =
    Core.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToPath GetDocumentVersion where
  toPath GetDocumentVersion' {..} =
    Core.mconcat
      [ "/api/v1/documents/",
        Core.toBS documentId,
        "/versions/",
        Core.toBS versionId
      ]

instance Core.ToQuery GetDocumentVersion where
  toQuery GetDocumentVersion' {..} =
    Core.mconcat
      [ "includeCustomMetadata"
          Core.=: includeCustomMetadata,
        "fields" Core.=: fields
      ]

-- | /See:/ 'newGetDocumentVersionResponse' smart constructor.
data GetDocumentVersionResponse = GetDocumentVersionResponse'
  { -- | The version metadata.
    metadata :: Core.Maybe DocumentVersionMetadata,
    -- | The custom metadata on the document version.
    customMetadata :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDocumentVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadata', 'getDocumentVersionResponse_metadata' - The version metadata.
--
-- 'customMetadata', 'getDocumentVersionResponse_customMetadata' - The custom metadata on the document version.
--
-- 'httpStatus', 'getDocumentVersionResponse_httpStatus' - The response's http status code.
newGetDocumentVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDocumentVersionResponse
newGetDocumentVersionResponse pHttpStatus_ =
  GetDocumentVersionResponse'
    { metadata =
        Core.Nothing,
      customMetadata = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version metadata.
getDocumentVersionResponse_metadata :: Lens.Lens' GetDocumentVersionResponse (Core.Maybe DocumentVersionMetadata)
getDocumentVersionResponse_metadata = Lens.lens (\GetDocumentVersionResponse' {metadata} -> metadata) (\s@GetDocumentVersionResponse' {} a -> s {metadata = a} :: GetDocumentVersionResponse)

-- | The custom metadata on the document version.
getDocumentVersionResponse_customMetadata :: Lens.Lens' GetDocumentVersionResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
getDocumentVersionResponse_customMetadata = Lens.lens (\GetDocumentVersionResponse' {customMetadata} -> customMetadata) (\s@GetDocumentVersionResponse' {} a -> s {customMetadata = a} :: GetDocumentVersionResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getDocumentVersionResponse_httpStatus :: Lens.Lens' GetDocumentVersionResponse Core.Int
getDocumentVersionResponse_httpStatus = Lens.lens (\GetDocumentVersionResponse' {httpStatus} -> httpStatus) (\s@GetDocumentVersionResponse' {} a -> s {httpStatus = a} :: GetDocumentVersionResponse)

instance Core.NFData GetDocumentVersionResponse
