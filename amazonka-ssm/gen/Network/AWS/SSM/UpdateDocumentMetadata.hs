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
-- Module      : Network.AWS.SSM.UpdateDocumentMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information related to approval reviews for a specific version
-- of a document.
module Network.AWS.SSM.UpdateDocumentMetadata
  ( -- * Creating a Request
    UpdateDocumentMetadata (..),
    newUpdateDocumentMetadata,

    -- * Request Lenses
    updateDocumentMetadata_documentVersion,
    updateDocumentMetadata_name,
    updateDocumentMetadata_documentReviews,

    -- * Destructuring the Response
    UpdateDocumentMetadataResponse (..),
    newUpdateDocumentMetadataResponse,

    -- * Response Lenses
    updateDocumentMetadataResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newUpdateDocumentMetadata' smart constructor.
data UpdateDocumentMetadata = UpdateDocumentMetadata'
  { -- | The version of a document to update.
    documentVersion :: Core.Maybe Core.Text,
    -- | The name of the document for which a version is to be updated.
    name :: Core.Text,
    -- | The document review details to update.
    documentReviews :: DocumentReviews
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDocumentMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentVersion', 'updateDocumentMetadata_documentVersion' - The version of a document to update.
--
-- 'name', 'updateDocumentMetadata_name' - The name of the document for which a version is to be updated.
--
-- 'documentReviews', 'updateDocumentMetadata_documentReviews' - The document review details to update.
newUpdateDocumentMetadata ::
  -- | 'name'
  Core.Text ->
  -- | 'documentReviews'
  DocumentReviews ->
  UpdateDocumentMetadata
newUpdateDocumentMetadata pName_ pDocumentReviews_ =
  UpdateDocumentMetadata'
    { documentVersion =
        Core.Nothing,
      name = pName_,
      documentReviews = pDocumentReviews_
    }

-- | The version of a document to update.
updateDocumentMetadata_documentVersion :: Lens.Lens' UpdateDocumentMetadata (Core.Maybe Core.Text)
updateDocumentMetadata_documentVersion = Lens.lens (\UpdateDocumentMetadata' {documentVersion} -> documentVersion) (\s@UpdateDocumentMetadata' {} a -> s {documentVersion = a} :: UpdateDocumentMetadata)

-- | The name of the document for which a version is to be updated.
updateDocumentMetadata_name :: Lens.Lens' UpdateDocumentMetadata Core.Text
updateDocumentMetadata_name = Lens.lens (\UpdateDocumentMetadata' {name} -> name) (\s@UpdateDocumentMetadata' {} a -> s {name = a} :: UpdateDocumentMetadata)

-- | The document review details to update.
updateDocumentMetadata_documentReviews :: Lens.Lens' UpdateDocumentMetadata DocumentReviews
updateDocumentMetadata_documentReviews = Lens.lens (\UpdateDocumentMetadata' {documentReviews} -> documentReviews) (\s@UpdateDocumentMetadata' {} a -> s {documentReviews = a} :: UpdateDocumentMetadata)

instance Core.AWSRequest UpdateDocumentMetadata where
  type
    AWSResponse UpdateDocumentMetadata =
      UpdateDocumentMetadataResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDocumentMetadataResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateDocumentMetadata

instance Core.NFData UpdateDocumentMetadata

instance Core.ToHeaders UpdateDocumentMetadata where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.UpdateDocumentMetadata" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateDocumentMetadata where
  toJSON UpdateDocumentMetadata' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DocumentVersion" Core..=)
              Core.<$> documentVersion,
            Core.Just ("Name" Core..= name),
            Core.Just
              ("DocumentReviews" Core..= documentReviews)
          ]
      )

instance Core.ToPath UpdateDocumentMetadata where
  toPath = Core.const "/"

instance Core.ToQuery UpdateDocumentMetadata where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateDocumentMetadataResponse' smart constructor.
data UpdateDocumentMetadataResponse = UpdateDocumentMetadataResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDocumentMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDocumentMetadataResponse_httpStatus' - The response's http status code.
newUpdateDocumentMetadataResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateDocumentMetadataResponse
newUpdateDocumentMetadataResponse pHttpStatus_ =
  UpdateDocumentMetadataResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDocumentMetadataResponse_httpStatus :: Lens.Lens' UpdateDocumentMetadataResponse Core.Int
updateDocumentMetadataResponse_httpStatus = Lens.lens (\UpdateDocumentMetadataResponse' {httpStatus} -> httpStatus) (\s@UpdateDocumentMetadataResponse' {} a -> s {httpStatus = a} :: UpdateDocumentMetadataResponse)

instance Core.NFData UpdateDocumentMetadataResponse
