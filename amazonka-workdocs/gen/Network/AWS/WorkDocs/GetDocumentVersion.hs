{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newGetDocumentVersion' smart constructor.
data GetDocumentVersion = GetDocumentVersion'
  { -- | Set this to TRUE to include custom metadata in the response.
    includeCustomMetadata :: Prelude.Maybe Prelude.Bool,
    -- | A comma-separated list of values. Specify \"SOURCE\" to include a URL
    -- for the source document.
    fields :: Prelude.Maybe Prelude.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The ID of the document.
    documentId :: Prelude.Text,
    -- | The version ID of the document.
    versionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'versionId'
  Prelude.Text ->
  GetDocumentVersion
newGetDocumentVersion pDocumentId_ pVersionId_ =
  GetDocumentVersion'
    { includeCustomMetadata =
        Prelude.Nothing,
      fields = Prelude.Nothing,
      authenticationToken = Prelude.Nothing,
      documentId = pDocumentId_,
      versionId = pVersionId_
    }

-- | Set this to TRUE to include custom metadata in the response.
getDocumentVersion_includeCustomMetadata :: Lens.Lens' GetDocumentVersion (Prelude.Maybe Prelude.Bool)
getDocumentVersion_includeCustomMetadata = Lens.lens (\GetDocumentVersion' {includeCustomMetadata} -> includeCustomMetadata) (\s@GetDocumentVersion' {} a -> s {includeCustomMetadata = a} :: GetDocumentVersion)

-- | A comma-separated list of values. Specify \"SOURCE\" to include a URL
-- for the source document.
getDocumentVersion_fields :: Lens.Lens' GetDocumentVersion (Prelude.Maybe Prelude.Text)
getDocumentVersion_fields = Lens.lens (\GetDocumentVersion' {fields} -> fields) (\s@GetDocumentVersion' {} a -> s {fields = a} :: GetDocumentVersion)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
getDocumentVersion_authenticationToken :: Lens.Lens' GetDocumentVersion (Prelude.Maybe Prelude.Text)
getDocumentVersion_authenticationToken = Lens.lens (\GetDocumentVersion' {authenticationToken} -> authenticationToken) (\s@GetDocumentVersion' {} a -> s {authenticationToken = a} :: GetDocumentVersion) Prelude.. Lens.mapping Prelude._Sensitive

-- | The ID of the document.
getDocumentVersion_documentId :: Lens.Lens' GetDocumentVersion Prelude.Text
getDocumentVersion_documentId = Lens.lens (\GetDocumentVersion' {documentId} -> documentId) (\s@GetDocumentVersion' {} a -> s {documentId = a} :: GetDocumentVersion)

-- | The version ID of the document.
getDocumentVersion_versionId :: Lens.Lens' GetDocumentVersion Prelude.Text
getDocumentVersion_versionId = Lens.lens (\GetDocumentVersion' {versionId} -> versionId) (\s@GetDocumentVersion' {} a -> s {versionId = a} :: GetDocumentVersion)

instance Prelude.AWSRequest GetDocumentVersion where
  type
    Rs GetDocumentVersion =
      GetDocumentVersionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDocumentVersionResponse'
            Prelude.<$> (x Prelude..?> "Metadata")
            Prelude.<*> ( x Prelude..?> "CustomMetadata"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDocumentVersion

instance Prelude.NFData GetDocumentVersion

instance Prelude.ToHeaders GetDocumentVersion where
  toHeaders GetDocumentVersion' {..} =
    Prelude.mconcat
      [ "Authentication" Prelude.=# authenticationToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Prelude.ToPath GetDocumentVersion where
  toPath GetDocumentVersion' {..} =
    Prelude.mconcat
      [ "/api/v1/documents/",
        Prelude.toBS documentId,
        "/versions/",
        Prelude.toBS versionId
      ]

instance Prelude.ToQuery GetDocumentVersion where
  toQuery GetDocumentVersion' {..} =
    Prelude.mconcat
      [ "includeCustomMetadata"
          Prelude.=: includeCustomMetadata,
        "fields" Prelude.=: fields
      ]

-- | /See:/ 'newGetDocumentVersionResponse' smart constructor.
data GetDocumentVersionResponse = GetDocumentVersionResponse'
  { -- | The version metadata.
    metadata :: Prelude.Maybe DocumentVersionMetadata,
    -- | The custom metadata on the document version.
    customMetadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetDocumentVersionResponse
newGetDocumentVersionResponse pHttpStatus_ =
  GetDocumentVersionResponse'
    { metadata =
        Prelude.Nothing,
      customMetadata = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version metadata.
getDocumentVersionResponse_metadata :: Lens.Lens' GetDocumentVersionResponse (Prelude.Maybe DocumentVersionMetadata)
getDocumentVersionResponse_metadata = Lens.lens (\GetDocumentVersionResponse' {metadata} -> metadata) (\s@GetDocumentVersionResponse' {} a -> s {metadata = a} :: GetDocumentVersionResponse)

-- | The custom metadata on the document version.
getDocumentVersionResponse_customMetadata :: Lens.Lens' GetDocumentVersionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getDocumentVersionResponse_customMetadata = Lens.lens (\GetDocumentVersionResponse' {customMetadata} -> customMetadata) (\s@GetDocumentVersionResponse' {} a -> s {customMetadata = a} :: GetDocumentVersionResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getDocumentVersionResponse_httpStatus :: Lens.Lens' GetDocumentVersionResponse Prelude.Int
getDocumentVersionResponse_httpStatus = Lens.lens (\GetDocumentVersionResponse' {httpStatus} -> httpStatus) (\s@GetDocumentVersionResponse' {} a -> s {httpStatus = a} :: GetDocumentVersionResponse)

instance Prelude.NFData GetDocumentVersionResponse
