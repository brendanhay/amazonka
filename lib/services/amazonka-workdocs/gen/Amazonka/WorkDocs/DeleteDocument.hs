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
-- Module      : Amazonka.WorkDocs.DeleteDocument
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the specified document and its associated metadata.
module Amazonka.WorkDocs.DeleteDocument
  ( -- * Creating a Request
    DeleteDocument (..),
    newDeleteDocument,

    -- * Request Lenses
    deleteDocument_authenticationToken,
    deleteDocument_documentId,

    -- * Destructuring the Response
    DeleteDocumentResponse (..),
    newDeleteDocumentResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newDeleteDocument' smart constructor.
data DeleteDocument = DeleteDocument'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the document.
    documentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'deleteDocument_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'documentId', 'deleteDocument_documentId' - The ID of the document.
newDeleteDocument ::
  -- | 'documentId'
  Prelude.Text ->
  DeleteDocument
newDeleteDocument pDocumentId_ =
  DeleteDocument'
    { authenticationToken =
        Prelude.Nothing,
      documentId = pDocumentId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
deleteDocument_authenticationToken :: Lens.Lens' DeleteDocument (Prelude.Maybe Prelude.Text)
deleteDocument_authenticationToken = Lens.lens (\DeleteDocument' {authenticationToken} -> authenticationToken) (\s@DeleteDocument' {} a -> s {authenticationToken = a} :: DeleteDocument) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the document.
deleteDocument_documentId :: Lens.Lens' DeleteDocument Prelude.Text
deleteDocument_documentId = Lens.lens (\DeleteDocument' {documentId} -> documentId) (\s@DeleteDocument' {} a -> s {documentId = a} :: DeleteDocument)

instance Core.AWSRequest DeleteDocument where
  type
    AWSResponse DeleteDocument =
      DeleteDocumentResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteDocumentResponse'

instance Prelude.Hashable DeleteDocument where
  hashWithSalt _salt DeleteDocument' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` documentId

instance Prelude.NFData DeleteDocument where
  rnf DeleteDocument' {..} =
    Prelude.rnf authenticationToken
      `Prelude.seq` Prelude.rnf documentId

instance Data.ToHeaders DeleteDocument where
  toHeaders DeleteDocument' {..} =
    Prelude.mconcat
      [ "Authentication" Data.=# authenticationToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath DeleteDocument where
  toPath DeleteDocument' {..} =
    Prelude.mconcat
      ["/api/v1/documents/", Data.toBS documentId]

instance Data.ToQuery DeleteDocument where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDocumentResponse' smart constructor.
data DeleteDocumentResponse = DeleteDocumentResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDocumentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDocumentResponse ::
  DeleteDocumentResponse
newDeleteDocumentResponse = DeleteDocumentResponse'

instance Prelude.NFData DeleteDocumentResponse where
  rnf _ = ()
