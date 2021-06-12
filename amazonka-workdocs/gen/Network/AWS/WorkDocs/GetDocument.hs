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
-- Module      : Network.AWS.WorkDocs.GetDocument
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details of a document.
module Network.AWS.WorkDocs.GetDocument
  ( -- * Creating a Request
    GetDocument (..),
    newGetDocument,

    -- * Request Lenses
    getDocument_includeCustomMetadata,
    getDocument_authenticationToken,
    getDocument_documentId,

    -- * Destructuring the Response
    GetDocumentResponse (..),
    newGetDocumentResponse,

    -- * Response Lenses
    getDocumentResponse_metadata,
    getDocumentResponse_customMetadata,
    getDocumentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newGetDocument' smart constructor.
data GetDocument = GetDocument'
  { -- | Set this to @TRUE@ to include custom metadata in the response.
    includeCustomMetadata :: Core.Maybe Core.Bool,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The ID of the document.
    documentId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeCustomMetadata', 'getDocument_includeCustomMetadata' - Set this to @TRUE@ to include custom metadata in the response.
--
-- 'authenticationToken', 'getDocument_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'documentId', 'getDocument_documentId' - The ID of the document.
newGetDocument ::
  -- | 'documentId'
  Core.Text ->
  GetDocument
newGetDocument pDocumentId_ =
  GetDocument'
    { includeCustomMetadata = Core.Nothing,
      authenticationToken = Core.Nothing,
      documentId = pDocumentId_
    }

-- | Set this to @TRUE@ to include custom metadata in the response.
getDocument_includeCustomMetadata :: Lens.Lens' GetDocument (Core.Maybe Core.Bool)
getDocument_includeCustomMetadata = Lens.lens (\GetDocument' {includeCustomMetadata} -> includeCustomMetadata) (\s@GetDocument' {} a -> s {includeCustomMetadata = a} :: GetDocument)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
getDocument_authenticationToken :: Lens.Lens' GetDocument (Core.Maybe Core.Text)
getDocument_authenticationToken = Lens.lens (\GetDocument' {authenticationToken} -> authenticationToken) (\s@GetDocument' {} a -> s {authenticationToken = a} :: GetDocument) Core.. Lens.mapping Core._Sensitive

-- | The ID of the document.
getDocument_documentId :: Lens.Lens' GetDocument Core.Text
getDocument_documentId = Lens.lens (\GetDocument' {documentId} -> documentId) (\s@GetDocument' {} a -> s {documentId = a} :: GetDocument)

instance Core.AWSRequest GetDocument where
  type AWSResponse GetDocument = GetDocumentResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDocumentResponse'
            Core.<$> (x Core..?> "Metadata")
            Core.<*> (x Core..?> "CustomMetadata" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDocument

instance Core.NFData GetDocument

instance Core.ToHeaders GetDocument where
  toHeaders GetDocument' {..} =
    Core.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToPath GetDocument where
  toPath GetDocument' {..} =
    Core.mconcat
      ["/api/v1/documents/", Core.toBS documentId]

instance Core.ToQuery GetDocument where
  toQuery GetDocument' {..} =
    Core.mconcat
      [ "includeCustomMetadata"
          Core.=: includeCustomMetadata
      ]

-- | /See:/ 'newGetDocumentResponse' smart constructor.
data GetDocumentResponse = GetDocumentResponse'
  { -- | The metadata details of the document.
    metadata :: Core.Maybe DocumentMetadata,
    -- | The custom metadata on the document.
    customMetadata :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDocumentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadata', 'getDocumentResponse_metadata' - The metadata details of the document.
--
-- 'customMetadata', 'getDocumentResponse_customMetadata' - The custom metadata on the document.
--
-- 'httpStatus', 'getDocumentResponse_httpStatus' - The response's http status code.
newGetDocumentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDocumentResponse
newGetDocumentResponse pHttpStatus_ =
  GetDocumentResponse'
    { metadata = Core.Nothing,
      customMetadata = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The metadata details of the document.
getDocumentResponse_metadata :: Lens.Lens' GetDocumentResponse (Core.Maybe DocumentMetadata)
getDocumentResponse_metadata = Lens.lens (\GetDocumentResponse' {metadata} -> metadata) (\s@GetDocumentResponse' {} a -> s {metadata = a} :: GetDocumentResponse)

-- | The custom metadata on the document.
getDocumentResponse_customMetadata :: Lens.Lens' GetDocumentResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
getDocumentResponse_customMetadata = Lens.lens (\GetDocumentResponse' {customMetadata} -> customMetadata) (\s@GetDocumentResponse' {} a -> s {customMetadata = a} :: GetDocumentResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getDocumentResponse_httpStatus :: Lens.Lens' GetDocumentResponse Core.Int
getDocumentResponse_httpStatus = Lens.lens (\GetDocumentResponse' {httpStatus} -> httpStatus) (\s@GetDocumentResponse' {} a -> s {httpStatus = a} :: GetDocumentResponse)

instance Core.NFData GetDocumentResponse
