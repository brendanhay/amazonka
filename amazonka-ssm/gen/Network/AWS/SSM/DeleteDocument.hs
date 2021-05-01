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
-- Module      : Network.AWS.SSM.DeleteDocument
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the Systems Manager document and all instance associations to
-- the document.
--
-- Before you delete the document, we recommend that you use
-- DeleteAssociation to disassociate all instances that are associated with
-- the document.
module Network.AWS.SSM.DeleteDocument
  ( -- * Creating a Request
    DeleteDocument (..),
    newDeleteDocument,

    -- * Request Lenses
    deleteDocument_force,
    deleteDocument_versionName,
    deleteDocument_documentVersion,
    deleteDocument_name,

    -- * Destructuring the Response
    DeleteDocumentResponse (..),
    newDeleteDocumentResponse,

    -- * Response Lenses
    deleteDocumentResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDeleteDocument' smart constructor.
data DeleteDocument = DeleteDocument'
  { -- | Some SSM document types require that you specify a @Force@ flag before
    -- you can delete the document. For example, you must specify a @Force@
    -- flag to delete a document of type @ApplicationConfigurationSchema@. You
    -- can restrict access to the @Force@ flag in an AWS Identity and Access
    -- Management (IAM) policy.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The version name of the document that you want to delete. If not
    -- provided, all versions of the document are deleted.
    versionName :: Prelude.Maybe Prelude.Text,
    -- | The version of the document that you want to delete. If not provided,
    -- all versions of the document are deleted.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the document.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'force', 'deleteDocument_force' - Some SSM document types require that you specify a @Force@ flag before
-- you can delete the document. For example, you must specify a @Force@
-- flag to delete a document of type @ApplicationConfigurationSchema@. You
-- can restrict access to the @Force@ flag in an AWS Identity and Access
-- Management (IAM) policy.
--
-- 'versionName', 'deleteDocument_versionName' - The version name of the document that you want to delete. If not
-- provided, all versions of the document are deleted.
--
-- 'documentVersion', 'deleteDocument_documentVersion' - The version of the document that you want to delete. If not provided,
-- all versions of the document are deleted.
--
-- 'name', 'deleteDocument_name' - The name of the document.
newDeleteDocument ::
  -- | 'name'
  Prelude.Text ->
  DeleteDocument
newDeleteDocument pName_ =
  DeleteDocument'
    { force = Prelude.Nothing,
      versionName = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      name = pName_
    }

-- | Some SSM document types require that you specify a @Force@ flag before
-- you can delete the document. For example, you must specify a @Force@
-- flag to delete a document of type @ApplicationConfigurationSchema@. You
-- can restrict access to the @Force@ flag in an AWS Identity and Access
-- Management (IAM) policy.
deleteDocument_force :: Lens.Lens' DeleteDocument (Prelude.Maybe Prelude.Bool)
deleteDocument_force = Lens.lens (\DeleteDocument' {force} -> force) (\s@DeleteDocument' {} a -> s {force = a} :: DeleteDocument)

-- | The version name of the document that you want to delete. If not
-- provided, all versions of the document are deleted.
deleteDocument_versionName :: Lens.Lens' DeleteDocument (Prelude.Maybe Prelude.Text)
deleteDocument_versionName = Lens.lens (\DeleteDocument' {versionName} -> versionName) (\s@DeleteDocument' {} a -> s {versionName = a} :: DeleteDocument)

-- | The version of the document that you want to delete. If not provided,
-- all versions of the document are deleted.
deleteDocument_documentVersion :: Lens.Lens' DeleteDocument (Prelude.Maybe Prelude.Text)
deleteDocument_documentVersion = Lens.lens (\DeleteDocument' {documentVersion} -> documentVersion) (\s@DeleteDocument' {} a -> s {documentVersion = a} :: DeleteDocument)

-- | The name of the document.
deleteDocument_name :: Lens.Lens' DeleteDocument Prelude.Text
deleteDocument_name = Lens.lens (\DeleteDocument' {name} -> name) (\s@DeleteDocument' {} a -> s {name = a} :: DeleteDocument)

instance Prelude.AWSRequest DeleteDocument where
  type Rs DeleteDocument = DeleteDocumentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDocumentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDocument

instance Prelude.NFData DeleteDocument

instance Prelude.ToHeaders DeleteDocument where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AmazonSSM.DeleteDocument" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteDocument where
  toJSON DeleteDocument' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Force" Prelude..=) Prelude.<$> force,
            ("VersionName" Prelude..=) Prelude.<$> versionName,
            ("DocumentVersion" Prelude..=)
              Prelude.<$> documentVersion,
            Prelude.Just ("Name" Prelude..= name)
          ]
      )

instance Prelude.ToPath DeleteDocument where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteDocument where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDocumentResponse' smart constructor.
data DeleteDocumentResponse = DeleteDocumentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteDocumentResponse
