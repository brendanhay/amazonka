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
-- Module      : Amazonka.WorkDocs.GetDocumentVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves version metadata for the specified document.
module Amazonka.WorkDocs.GetDocumentVersion
  ( -- * Creating a Request
    GetDocumentVersion (..),
    newGetDocumentVersion,

    -- * Request Lenses
    getDocumentVersion_authenticationToken,
    getDocumentVersion_fields,
    getDocumentVersion_includeCustomMetadata,
    getDocumentVersion_documentId,
    getDocumentVersion_versionId,

    -- * Destructuring the Response
    GetDocumentVersionResponse (..),
    newGetDocumentVersionResponse,

    -- * Response Lenses
    getDocumentVersionResponse_customMetadata,
    getDocumentVersionResponse_metadata,
    getDocumentVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newGetDocumentVersion' smart constructor.
data GetDocumentVersion = GetDocumentVersion'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A comma-separated list of values. Specify \"SOURCE\" to include a URL
    -- for the source document.
    fields :: Prelude.Maybe Prelude.Text,
    -- | Set this to TRUE to include custom metadata in the response.
    includeCustomMetadata :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the document.
    documentId :: Prelude.Text,
    -- | The version ID of the document.
    versionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDocumentVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'getDocumentVersion_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'fields', 'getDocumentVersion_fields' - A comma-separated list of values. Specify \"SOURCE\" to include a URL
-- for the source document.
--
-- 'includeCustomMetadata', 'getDocumentVersion_includeCustomMetadata' - Set this to TRUE to include custom metadata in the response.
--
-- 'documentId', 'getDocumentVersion_documentId' - The ID of the document.
--
-- 'versionId', 'getDocumentVersion_versionId' - The version ID of the document.
newGetDocumentVersion ::
  -- | 'documentId'
  Prelude.Text ->
  -- | 'versionId'
  Prelude.Text ->
  GetDocumentVersion
newGetDocumentVersion pDocumentId_ pVersionId_ =
  GetDocumentVersion'
    { authenticationToken =
        Prelude.Nothing,
      fields = Prelude.Nothing,
      includeCustomMetadata = Prelude.Nothing,
      documentId = pDocumentId_,
      versionId = pVersionId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
getDocumentVersion_authenticationToken :: Lens.Lens' GetDocumentVersion (Prelude.Maybe Prelude.Text)
getDocumentVersion_authenticationToken = Lens.lens (\GetDocumentVersion' {authenticationToken} -> authenticationToken) (\s@GetDocumentVersion' {} a -> s {authenticationToken = a} :: GetDocumentVersion) Prelude.. Lens.mapping Data._Sensitive

-- | A comma-separated list of values. Specify \"SOURCE\" to include a URL
-- for the source document.
getDocumentVersion_fields :: Lens.Lens' GetDocumentVersion (Prelude.Maybe Prelude.Text)
getDocumentVersion_fields = Lens.lens (\GetDocumentVersion' {fields} -> fields) (\s@GetDocumentVersion' {} a -> s {fields = a} :: GetDocumentVersion)

-- | Set this to TRUE to include custom metadata in the response.
getDocumentVersion_includeCustomMetadata :: Lens.Lens' GetDocumentVersion (Prelude.Maybe Prelude.Bool)
getDocumentVersion_includeCustomMetadata = Lens.lens (\GetDocumentVersion' {includeCustomMetadata} -> includeCustomMetadata) (\s@GetDocumentVersion' {} a -> s {includeCustomMetadata = a} :: GetDocumentVersion)

-- | The ID of the document.
getDocumentVersion_documentId :: Lens.Lens' GetDocumentVersion Prelude.Text
getDocumentVersion_documentId = Lens.lens (\GetDocumentVersion' {documentId} -> documentId) (\s@GetDocumentVersion' {} a -> s {documentId = a} :: GetDocumentVersion)

-- | The version ID of the document.
getDocumentVersion_versionId :: Lens.Lens' GetDocumentVersion Prelude.Text
getDocumentVersion_versionId = Lens.lens (\GetDocumentVersion' {versionId} -> versionId) (\s@GetDocumentVersion' {} a -> s {versionId = a} :: GetDocumentVersion)

instance Core.AWSRequest GetDocumentVersion where
  type
    AWSResponse GetDocumentVersion =
      GetDocumentVersionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDocumentVersionResponse'
            Prelude.<$> (x Data..?> "CustomMetadata" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Metadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDocumentVersion where
  hashWithSalt _salt GetDocumentVersion' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` fields
      `Prelude.hashWithSalt` includeCustomMetadata
      `Prelude.hashWithSalt` documentId
      `Prelude.hashWithSalt` versionId

instance Prelude.NFData GetDocumentVersion where
  rnf GetDocumentVersion' {..} =
    Prelude.rnf authenticationToken
      `Prelude.seq` Prelude.rnf fields
      `Prelude.seq` Prelude.rnf includeCustomMetadata
      `Prelude.seq` Prelude.rnf documentId
      `Prelude.seq` Prelude.rnf versionId

instance Data.ToHeaders GetDocumentVersion where
  toHeaders GetDocumentVersion' {..} =
    Prelude.mconcat
      [ "Authentication" Data.=# authenticationToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath GetDocumentVersion where
  toPath GetDocumentVersion' {..} =
    Prelude.mconcat
      [ "/api/v1/documents/",
        Data.toBS documentId,
        "/versions/",
        Data.toBS versionId
      ]

instance Data.ToQuery GetDocumentVersion where
  toQuery GetDocumentVersion' {..} =
    Prelude.mconcat
      [ "fields" Data.=: fields,
        "includeCustomMetadata"
          Data.=: includeCustomMetadata
      ]

-- | /See:/ 'newGetDocumentVersionResponse' smart constructor.
data GetDocumentVersionResponse = GetDocumentVersionResponse'
  { -- | The custom metadata on the document version.
    customMetadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The version metadata.
    metadata :: Prelude.Maybe DocumentVersionMetadata,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDocumentVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customMetadata', 'getDocumentVersionResponse_customMetadata' - The custom metadata on the document version.
--
-- 'metadata', 'getDocumentVersionResponse_metadata' - The version metadata.
--
-- 'httpStatus', 'getDocumentVersionResponse_httpStatus' - The response's http status code.
newGetDocumentVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDocumentVersionResponse
newGetDocumentVersionResponse pHttpStatus_ =
  GetDocumentVersionResponse'
    { customMetadata =
        Prelude.Nothing,
      metadata = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The custom metadata on the document version.
getDocumentVersionResponse_customMetadata :: Lens.Lens' GetDocumentVersionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getDocumentVersionResponse_customMetadata = Lens.lens (\GetDocumentVersionResponse' {customMetadata} -> customMetadata) (\s@GetDocumentVersionResponse' {} a -> s {customMetadata = a} :: GetDocumentVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The version metadata.
getDocumentVersionResponse_metadata :: Lens.Lens' GetDocumentVersionResponse (Prelude.Maybe DocumentVersionMetadata)
getDocumentVersionResponse_metadata = Lens.lens (\GetDocumentVersionResponse' {metadata} -> metadata) (\s@GetDocumentVersionResponse' {} a -> s {metadata = a} :: GetDocumentVersionResponse)

-- | The response's http status code.
getDocumentVersionResponse_httpStatus :: Lens.Lens' GetDocumentVersionResponse Prelude.Int
getDocumentVersionResponse_httpStatus = Lens.lens (\GetDocumentVersionResponse' {httpStatus} -> httpStatus) (\s@GetDocumentVersionResponse' {} a -> s {httpStatus = a} :: GetDocumentVersionResponse)

instance Prelude.NFData GetDocumentVersionResponse where
  rnf GetDocumentVersionResponse' {..} =
    Prelude.rnf customMetadata
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf httpStatus
