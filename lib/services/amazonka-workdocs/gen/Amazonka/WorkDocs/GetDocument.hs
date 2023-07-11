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
-- Module      : Amazonka.WorkDocs.GetDocument
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details of a document.
module Amazonka.WorkDocs.GetDocument
  ( -- * Creating a Request
    GetDocument (..),
    newGetDocument,

    -- * Request Lenses
    getDocument_authenticationToken,
    getDocument_includeCustomMetadata,
    getDocument_documentId,

    -- * Destructuring the Response
    GetDocumentResponse (..),
    newGetDocumentResponse,

    -- * Response Lenses
    getDocumentResponse_customMetadata,
    getDocumentResponse_metadata,
    getDocumentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newGetDocument' smart constructor.
data GetDocument = GetDocument'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Set this to @TRUE@ to include custom metadata in the response.
    includeCustomMetadata :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the document.
    documentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'getDocument_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'includeCustomMetadata', 'getDocument_includeCustomMetadata' - Set this to @TRUE@ to include custom metadata in the response.
--
-- 'documentId', 'getDocument_documentId' - The ID of the document.
newGetDocument ::
  -- | 'documentId'
  Prelude.Text ->
  GetDocument
newGetDocument pDocumentId_ =
  GetDocument'
    { authenticationToken = Prelude.Nothing,
      includeCustomMetadata = Prelude.Nothing,
      documentId = pDocumentId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
getDocument_authenticationToken :: Lens.Lens' GetDocument (Prelude.Maybe Prelude.Text)
getDocument_authenticationToken = Lens.lens (\GetDocument' {authenticationToken} -> authenticationToken) (\s@GetDocument' {} a -> s {authenticationToken = a} :: GetDocument) Prelude.. Lens.mapping Data._Sensitive

-- | Set this to @TRUE@ to include custom metadata in the response.
getDocument_includeCustomMetadata :: Lens.Lens' GetDocument (Prelude.Maybe Prelude.Bool)
getDocument_includeCustomMetadata = Lens.lens (\GetDocument' {includeCustomMetadata} -> includeCustomMetadata) (\s@GetDocument' {} a -> s {includeCustomMetadata = a} :: GetDocument)

-- | The ID of the document.
getDocument_documentId :: Lens.Lens' GetDocument Prelude.Text
getDocument_documentId = Lens.lens (\GetDocument' {documentId} -> documentId) (\s@GetDocument' {} a -> s {documentId = a} :: GetDocument)

instance Core.AWSRequest GetDocument where
  type AWSResponse GetDocument = GetDocumentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDocumentResponse'
            Prelude.<$> (x Data..?> "CustomMetadata" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Metadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDocument where
  hashWithSalt _salt GetDocument' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` includeCustomMetadata
      `Prelude.hashWithSalt` documentId

instance Prelude.NFData GetDocument where
  rnf GetDocument' {..} =
    Prelude.rnf authenticationToken
      `Prelude.seq` Prelude.rnf includeCustomMetadata
      `Prelude.seq` Prelude.rnf documentId

instance Data.ToHeaders GetDocument where
  toHeaders GetDocument' {..} =
    Prelude.mconcat
      [ "Authentication" Data.=# authenticationToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath GetDocument where
  toPath GetDocument' {..} =
    Prelude.mconcat
      ["/api/v1/documents/", Data.toBS documentId]

instance Data.ToQuery GetDocument where
  toQuery GetDocument' {..} =
    Prelude.mconcat
      [ "includeCustomMetadata"
          Data.=: includeCustomMetadata
      ]

-- | /See:/ 'newGetDocumentResponse' smart constructor.
data GetDocumentResponse = GetDocumentResponse'
  { -- | The custom metadata on the document.
    customMetadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The metadata details of the document.
    metadata :: Prelude.Maybe DocumentMetadata,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDocumentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customMetadata', 'getDocumentResponse_customMetadata' - The custom metadata on the document.
--
-- 'metadata', 'getDocumentResponse_metadata' - The metadata details of the document.
--
-- 'httpStatus', 'getDocumentResponse_httpStatus' - The response's http status code.
newGetDocumentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDocumentResponse
newGetDocumentResponse pHttpStatus_ =
  GetDocumentResponse'
    { customMetadata =
        Prelude.Nothing,
      metadata = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The custom metadata on the document.
getDocumentResponse_customMetadata :: Lens.Lens' GetDocumentResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getDocumentResponse_customMetadata = Lens.lens (\GetDocumentResponse' {customMetadata} -> customMetadata) (\s@GetDocumentResponse' {} a -> s {customMetadata = a} :: GetDocumentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The metadata details of the document.
getDocumentResponse_metadata :: Lens.Lens' GetDocumentResponse (Prelude.Maybe DocumentMetadata)
getDocumentResponse_metadata = Lens.lens (\GetDocumentResponse' {metadata} -> metadata) (\s@GetDocumentResponse' {} a -> s {metadata = a} :: GetDocumentResponse)

-- | The response's http status code.
getDocumentResponse_httpStatus :: Lens.Lens' GetDocumentResponse Prelude.Int
getDocumentResponse_httpStatus = Lens.lens (\GetDocumentResponse' {httpStatus} -> httpStatus) (\s@GetDocumentResponse' {} a -> s {httpStatus = a} :: GetDocumentResponse)

instance Prelude.NFData GetDocumentResponse where
  rnf GetDocumentResponse' {..} =
    Prelude.rnf customMetadata
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf httpStatus
