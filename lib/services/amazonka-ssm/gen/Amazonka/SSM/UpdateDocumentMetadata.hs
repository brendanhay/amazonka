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
-- Module      : Amazonka.SSM.UpdateDocumentMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information related to approval reviews for a specific version
-- of a change template in Change Manager.
module Amazonka.SSM.UpdateDocumentMetadata
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newUpdateDocumentMetadata' smart constructor.
data UpdateDocumentMetadata = UpdateDocumentMetadata'
  { -- | The version of a change template in which to update approval metadata.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the change template for which a version\'s metadata is to be
    -- updated.
    name :: Prelude.Text,
    -- | The change template review details to update.
    documentReviews :: DocumentReviews
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDocumentMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentVersion', 'updateDocumentMetadata_documentVersion' - The version of a change template in which to update approval metadata.
--
-- 'name', 'updateDocumentMetadata_name' - The name of the change template for which a version\'s metadata is to be
-- updated.
--
-- 'documentReviews', 'updateDocumentMetadata_documentReviews' - The change template review details to update.
newUpdateDocumentMetadata ::
  -- | 'name'
  Prelude.Text ->
  -- | 'documentReviews'
  DocumentReviews ->
  UpdateDocumentMetadata
newUpdateDocumentMetadata pName_ pDocumentReviews_ =
  UpdateDocumentMetadata'
    { documentVersion =
        Prelude.Nothing,
      name = pName_,
      documentReviews = pDocumentReviews_
    }

-- | The version of a change template in which to update approval metadata.
updateDocumentMetadata_documentVersion :: Lens.Lens' UpdateDocumentMetadata (Prelude.Maybe Prelude.Text)
updateDocumentMetadata_documentVersion = Lens.lens (\UpdateDocumentMetadata' {documentVersion} -> documentVersion) (\s@UpdateDocumentMetadata' {} a -> s {documentVersion = a} :: UpdateDocumentMetadata)

-- | The name of the change template for which a version\'s metadata is to be
-- updated.
updateDocumentMetadata_name :: Lens.Lens' UpdateDocumentMetadata Prelude.Text
updateDocumentMetadata_name = Lens.lens (\UpdateDocumentMetadata' {name} -> name) (\s@UpdateDocumentMetadata' {} a -> s {name = a} :: UpdateDocumentMetadata)

-- | The change template review details to update.
updateDocumentMetadata_documentReviews :: Lens.Lens' UpdateDocumentMetadata DocumentReviews
updateDocumentMetadata_documentReviews = Lens.lens (\UpdateDocumentMetadata' {documentReviews} -> documentReviews) (\s@UpdateDocumentMetadata' {} a -> s {documentReviews = a} :: UpdateDocumentMetadata)

instance Core.AWSRequest UpdateDocumentMetadata where
  type
    AWSResponse UpdateDocumentMetadata =
      UpdateDocumentMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDocumentMetadataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDocumentMetadata where
  hashWithSalt _salt UpdateDocumentMetadata' {..} =
    _salt `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` documentReviews

instance Prelude.NFData UpdateDocumentMetadata where
  rnf UpdateDocumentMetadata' {..} =
    Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf documentReviews

instance Data.ToHeaders UpdateDocumentMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.UpdateDocumentMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDocumentMetadata where
  toJSON UpdateDocumentMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DocumentVersion" Data..=)
              Prelude.<$> documentVersion,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("DocumentReviews" Data..= documentReviews)
          ]
      )

instance Data.ToPath UpdateDocumentMetadata where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDocumentMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDocumentMetadataResponse' smart constructor.
data UpdateDocumentMetadataResponse = UpdateDocumentMetadataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateDocumentMetadataResponse
newUpdateDocumentMetadataResponse pHttpStatus_ =
  UpdateDocumentMetadataResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDocumentMetadataResponse_httpStatus :: Lens.Lens' UpdateDocumentMetadataResponse Prelude.Int
updateDocumentMetadataResponse_httpStatus = Lens.lens (\UpdateDocumentMetadataResponse' {httpStatus} -> httpStatus) (\s@UpdateDocumentMetadataResponse' {} a -> s {httpStatus = a} :: UpdateDocumentMetadataResponse)

instance
  Prelude.NFData
    UpdateDocumentMetadataResponse
  where
  rnf UpdateDocumentMetadataResponse' {..} =
    Prelude.rnf httpStatus
