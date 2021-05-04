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
-- Module      : Network.AWS.WorkDocs.AbortDocumentVersionUpload
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Aborts the upload of the specified document version that was previously
-- initiated by InitiateDocumentVersionUpload. The client should make this
-- call only when it no longer intends to upload the document version, or
-- fails to do so.
module Network.AWS.WorkDocs.AbortDocumentVersionUpload
  ( -- * Creating a Request
    AbortDocumentVersionUpload (..),
    newAbortDocumentVersionUpload,

    -- * Request Lenses
    abortDocumentVersionUpload_authenticationToken,
    abortDocumentVersionUpload_documentId,
    abortDocumentVersionUpload_versionId,

    -- * Destructuring the Response
    AbortDocumentVersionUploadResponse (..),
    newAbortDocumentVersionUploadResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newAbortDocumentVersionUpload' smart constructor.
data AbortDocumentVersionUpload = AbortDocumentVersionUpload'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The ID of the document.
    documentId :: Prelude.Text,
    -- | The ID of the version.
    versionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AbortDocumentVersionUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'abortDocumentVersionUpload_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'documentId', 'abortDocumentVersionUpload_documentId' - The ID of the document.
--
-- 'versionId', 'abortDocumentVersionUpload_versionId' - The ID of the version.
newAbortDocumentVersionUpload ::
  -- | 'documentId'
  Prelude.Text ->
  -- | 'versionId'
  Prelude.Text ->
  AbortDocumentVersionUpload
newAbortDocumentVersionUpload
  pDocumentId_
  pVersionId_ =
    AbortDocumentVersionUpload'
      { authenticationToken =
          Prelude.Nothing,
        documentId = pDocumentId_,
        versionId = pVersionId_
      }

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
abortDocumentVersionUpload_authenticationToken :: Lens.Lens' AbortDocumentVersionUpload (Prelude.Maybe Prelude.Text)
abortDocumentVersionUpload_authenticationToken = Lens.lens (\AbortDocumentVersionUpload' {authenticationToken} -> authenticationToken) (\s@AbortDocumentVersionUpload' {} a -> s {authenticationToken = a} :: AbortDocumentVersionUpload) Prelude.. Lens.mapping Prelude._Sensitive

-- | The ID of the document.
abortDocumentVersionUpload_documentId :: Lens.Lens' AbortDocumentVersionUpload Prelude.Text
abortDocumentVersionUpload_documentId = Lens.lens (\AbortDocumentVersionUpload' {documentId} -> documentId) (\s@AbortDocumentVersionUpload' {} a -> s {documentId = a} :: AbortDocumentVersionUpload)

-- | The ID of the version.
abortDocumentVersionUpload_versionId :: Lens.Lens' AbortDocumentVersionUpload Prelude.Text
abortDocumentVersionUpload_versionId = Lens.lens (\AbortDocumentVersionUpload' {versionId} -> versionId) (\s@AbortDocumentVersionUpload' {} a -> s {versionId = a} :: AbortDocumentVersionUpload)

instance
  Prelude.AWSRequest
    AbortDocumentVersionUpload
  where
  type
    Rs AbortDocumentVersionUpload =
      AbortDocumentVersionUploadResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      AbortDocumentVersionUploadResponse'

instance Prelude.Hashable AbortDocumentVersionUpload

instance Prelude.NFData AbortDocumentVersionUpload

instance Prelude.ToHeaders AbortDocumentVersionUpload where
  toHeaders AbortDocumentVersionUpload' {..} =
    Prelude.mconcat
      [ "Authentication" Prelude.=# authenticationToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Prelude.ToPath AbortDocumentVersionUpload where
  toPath AbortDocumentVersionUpload' {..} =
    Prelude.mconcat
      [ "/api/v1/documents/",
        Prelude.toBS documentId,
        "/versions/",
        Prelude.toBS versionId
      ]

instance Prelude.ToQuery AbortDocumentVersionUpload where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAbortDocumentVersionUploadResponse' smart constructor.
data AbortDocumentVersionUploadResponse = AbortDocumentVersionUploadResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AbortDocumentVersionUploadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAbortDocumentVersionUploadResponse ::
  AbortDocumentVersionUploadResponse
newAbortDocumentVersionUploadResponse =
  AbortDocumentVersionUploadResponse'

instance
  Prelude.NFData
    AbortDocumentVersionUploadResponse
