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
-- Module      : Amazonka.WorkDocs.UpdateDocument
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attributes of a document. The user must have
-- access to both the document and its parent folder, if applicable.
module Amazonka.WorkDocs.UpdateDocument
  ( -- * Creating a Request
    UpdateDocument (..),
    newUpdateDocument,

    -- * Request Lenses
    updateDocument_authenticationToken,
    updateDocument_name,
    updateDocument_parentFolderId,
    updateDocument_resourceState,
    updateDocument_documentId,

    -- * Destructuring the Response
    UpdateDocumentResponse (..),
    newUpdateDocumentResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newUpdateDocument' smart constructor.
data UpdateDocument = UpdateDocument'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the document.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the parent folder.
    parentFolderId :: Prelude.Maybe Prelude.Text,
    -- | The resource state of the document. Only ACTIVE and RECYCLED are
    -- supported.
    resourceState :: Prelude.Maybe ResourceStateType,
    -- | The ID of the document.
    documentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'updateDocument_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'name', 'updateDocument_name' - The name of the document.
--
-- 'parentFolderId', 'updateDocument_parentFolderId' - The ID of the parent folder.
--
-- 'resourceState', 'updateDocument_resourceState' - The resource state of the document. Only ACTIVE and RECYCLED are
-- supported.
--
-- 'documentId', 'updateDocument_documentId' - The ID of the document.
newUpdateDocument ::
  -- | 'documentId'
  Prelude.Text ->
  UpdateDocument
newUpdateDocument pDocumentId_ =
  UpdateDocument'
    { authenticationToken =
        Prelude.Nothing,
      name = Prelude.Nothing,
      parentFolderId = Prelude.Nothing,
      resourceState = Prelude.Nothing,
      documentId = pDocumentId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
updateDocument_authenticationToken :: Lens.Lens' UpdateDocument (Prelude.Maybe Prelude.Text)
updateDocument_authenticationToken = Lens.lens (\UpdateDocument' {authenticationToken} -> authenticationToken) (\s@UpdateDocument' {} a -> s {authenticationToken = a} :: UpdateDocument) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the document.
updateDocument_name :: Lens.Lens' UpdateDocument (Prelude.Maybe Prelude.Text)
updateDocument_name = Lens.lens (\UpdateDocument' {name} -> name) (\s@UpdateDocument' {} a -> s {name = a} :: UpdateDocument)

-- | The ID of the parent folder.
updateDocument_parentFolderId :: Lens.Lens' UpdateDocument (Prelude.Maybe Prelude.Text)
updateDocument_parentFolderId = Lens.lens (\UpdateDocument' {parentFolderId} -> parentFolderId) (\s@UpdateDocument' {} a -> s {parentFolderId = a} :: UpdateDocument)

-- | The resource state of the document. Only ACTIVE and RECYCLED are
-- supported.
updateDocument_resourceState :: Lens.Lens' UpdateDocument (Prelude.Maybe ResourceStateType)
updateDocument_resourceState = Lens.lens (\UpdateDocument' {resourceState} -> resourceState) (\s@UpdateDocument' {} a -> s {resourceState = a} :: UpdateDocument)

-- | The ID of the document.
updateDocument_documentId :: Lens.Lens' UpdateDocument Prelude.Text
updateDocument_documentId = Lens.lens (\UpdateDocument' {documentId} -> documentId) (\s@UpdateDocument' {} a -> s {documentId = a} :: UpdateDocument)

instance Core.AWSRequest UpdateDocument where
  type
    AWSResponse UpdateDocument =
      UpdateDocumentResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateDocumentResponse'

instance Prelude.Hashable UpdateDocument where
  hashWithSalt _salt UpdateDocument' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` parentFolderId
      `Prelude.hashWithSalt` resourceState
      `Prelude.hashWithSalt` documentId

instance Prelude.NFData UpdateDocument where
  rnf UpdateDocument' {..} =
    Prelude.rnf authenticationToken `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf parentFolderId `Prelude.seq`
          Prelude.rnf resourceState `Prelude.seq`
            Prelude.rnf documentId

instance Data.ToHeaders UpdateDocument where
  toHeaders UpdateDocument' {..} =
    Prelude.mconcat
      [ "Authentication" Data.=# authenticationToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON UpdateDocument where
  toJSON UpdateDocument' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("ParentFolderId" Data..=)
              Prelude.<$> parentFolderId,
            ("ResourceState" Data..=) Prelude.<$> resourceState
          ]
      )

instance Data.ToPath UpdateDocument where
  toPath UpdateDocument' {..} =
    Prelude.mconcat
      ["/api/v1/documents/", Data.toBS documentId]

instance Data.ToQuery UpdateDocument where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDocumentResponse' smart constructor.
data UpdateDocumentResponse = UpdateDocumentResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDocumentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateDocumentResponse ::
  UpdateDocumentResponse
newUpdateDocumentResponse = UpdateDocumentResponse'

instance Prelude.NFData UpdateDocumentResponse where
  rnf _ = ()
