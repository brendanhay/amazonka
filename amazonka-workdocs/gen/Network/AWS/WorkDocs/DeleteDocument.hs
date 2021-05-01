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
-- Module      : Network.AWS.WorkDocs.DeleteDocument
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the specified document and its associated metadata.
module Network.AWS.WorkDocs.DeleteDocument
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDeleteDocument' smart constructor.
data DeleteDocument = DeleteDocument'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The ID of the document.
    documentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
deleteDocument_authenticationToken = Lens.lens (\DeleteDocument' {authenticationToken} -> authenticationToken) (\s@DeleteDocument' {} a -> s {authenticationToken = a} :: DeleteDocument) Prelude.. Lens.mapping Prelude._Sensitive

-- | The ID of the document.
deleteDocument_documentId :: Lens.Lens' DeleteDocument Prelude.Text
deleteDocument_documentId = Lens.lens (\DeleteDocument' {documentId} -> documentId) (\s@DeleteDocument' {} a -> s {documentId = a} :: DeleteDocument)

instance Prelude.AWSRequest DeleteDocument where
  type Rs DeleteDocument = DeleteDocumentResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteDocumentResponse'

instance Prelude.Hashable DeleteDocument

instance Prelude.NFData DeleteDocument

instance Prelude.ToHeaders DeleteDocument where
  toHeaders DeleteDocument' {..} =
    Prelude.mconcat
      [ "Authentication" Prelude.=# authenticationToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Prelude.ToPath DeleteDocument where
  toPath DeleteDocument' {..} =
    Prelude.mconcat
      ["/api/v1/documents/", Prelude.toBS documentId]

instance Prelude.ToQuery DeleteDocument where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDocumentResponse' smart constructor.
data DeleteDocumentResponse = DeleteDocumentResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDocumentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDocumentResponse ::
  DeleteDocumentResponse
newDeleteDocumentResponse = DeleteDocumentResponse'

instance Prelude.NFData DeleteDocumentResponse
