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
-- Module      : Amazonka.SSM.DeleteDocument
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the Amazon Web Services Systems Manager document (SSM document)
-- and all managed node associations to the document.
--
-- Before you delete the document, we recommend that you use
-- DeleteAssociation to disassociate all managed nodes that are associated
-- with the document.
module Amazonka.SSM.DeleteDocument
  ( -- * Creating a Request
    DeleteDocument (..),
    newDeleteDocument,

    -- * Request Lenses
    deleteDocument_documentVersion,
    deleteDocument_force,
    deleteDocument_versionName,
    deleteDocument_name,

    -- * Destructuring the Response
    DeleteDocumentResponse (..),
    newDeleteDocumentResponse,

    -- * Response Lenses
    deleteDocumentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDeleteDocument' smart constructor.
data DeleteDocument = DeleteDocument'
  { -- | The version of the document that you want to delete. If not provided,
    -- all versions of the document are deleted.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | Some SSM document types require that you specify a @Force@ flag before
    -- you can delete the document. For example, you must specify a @Force@
    -- flag to delete a document of type @ApplicationConfigurationSchema@. You
    -- can restrict access to the @Force@ flag in an Identity and Access
    -- Management (IAM) policy.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The version name of the document that you want to delete. If not
    -- provided, all versions of the document are deleted.
    versionName :: Prelude.Maybe Prelude.Text,
    -- | The name of the document.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentVersion', 'deleteDocument_documentVersion' - The version of the document that you want to delete. If not provided,
-- all versions of the document are deleted.
--
-- 'force', 'deleteDocument_force' - Some SSM document types require that you specify a @Force@ flag before
-- you can delete the document. For example, you must specify a @Force@
-- flag to delete a document of type @ApplicationConfigurationSchema@. You
-- can restrict access to the @Force@ flag in an Identity and Access
-- Management (IAM) policy.
--
-- 'versionName', 'deleteDocument_versionName' - The version name of the document that you want to delete. If not
-- provided, all versions of the document are deleted.
--
-- 'name', 'deleteDocument_name' - The name of the document.
newDeleteDocument ::
  -- | 'name'
  Prelude.Text ->
  DeleteDocument
newDeleteDocument pName_ =
  DeleteDocument'
    { documentVersion = Prelude.Nothing,
      force = Prelude.Nothing,
      versionName = Prelude.Nothing,
      name = pName_
    }

-- | The version of the document that you want to delete. If not provided,
-- all versions of the document are deleted.
deleteDocument_documentVersion :: Lens.Lens' DeleteDocument (Prelude.Maybe Prelude.Text)
deleteDocument_documentVersion = Lens.lens (\DeleteDocument' {documentVersion} -> documentVersion) (\s@DeleteDocument' {} a -> s {documentVersion = a} :: DeleteDocument)

-- | Some SSM document types require that you specify a @Force@ flag before
-- you can delete the document. For example, you must specify a @Force@
-- flag to delete a document of type @ApplicationConfigurationSchema@. You
-- can restrict access to the @Force@ flag in an Identity and Access
-- Management (IAM) policy.
deleteDocument_force :: Lens.Lens' DeleteDocument (Prelude.Maybe Prelude.Bool)
deleteDocument_force = Lens.lens (\DeleteDocument' {force} -> force) (\s@DeleteDocument' {} a -> s {force = a} :: DeleteDocument)

-- | The version name of the document that you want to delete. If not
-- provided, all versions of the document are deleted.
deleteDocument_versionName :: Lens.Lens' DeleteDocument (Prelude.Maybe Prelude.Text)
deleteDocument_versionName = Lens.lens (\DeleteDocument' {versionName} -> versionName) (\s@DeleteDocument' {} a -> s {versionName = a} :: DeleteDocument)

-- | The name of the document.
deleteDocument_name :: Lens.Lens' DeleteDocument Prelude.Text
deleteDocument_name = Lens.lens (\DeleteDocument' {name} -> name) (\s@DeleteDocument' {} a -> s {name = a} :: DeleteDocument)

instance Core.AWSRequest DeleteDocument where
  type
    AWSResponse DeleteDocument =
      DeleteDocumentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDocumentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDocument where
  hashWithSalt _salt DeleteDocument' {..} =
    _salt
      `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` force
      `Prelude.hashWithSalt` versionName
      `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteDocument where
  rnf DeleteDocument' {..} =
    Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf force
      `Prelude.seq` Prelude.rnf versionName
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders DeleteDocument where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.DeleteDocument" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDocument where
  toJSON DeleteDocument' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DocumentVersion" Data..=)
              Prelude.<$> documentVersion,
            ("Force" Data..=) Prelude.<$> force,
            ("VersionName" Data..=) Prelude.<$> versionName,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath DeleteDocument where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDocument where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDocumentResponse' smart constructor.
data DeleteDocumentResponse = DeleteDocumentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDocumentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDocumentResponse_httpStatus' - The response's http status code.
newDeleteDocumentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDocumentResponse
newDeleteDocumentResponse pHttpStatus_ =
  DeleteDocumentResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteDocumentResponse_httpStatus :: Lens.Lens' DeleteDocumentResponse Prelude.Int
deleteDocumentResponse_httpStatus = Lens.lens (\DeleteDocumentResponse' {httpStatus} -> httpStatus) (\s@DeleteDocumentResponse' {} a -> s {httpStatus = a} :: DeleteDocumentResponse)

instance Prelude.NFData DeleteDocumentResponse where
  rnf DeleteDocumentResponse' {..} =
    Prelude.rnf httpStatus
