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
-- Module      : Amazonka.FraudDetector.UpdateDetectorVersionMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the detector version\'s description. You can update the metadata
-- for any detector version (@DRAFT, ACTIVE,@ or @INACTIVE@).
module Amazonka.FraudDetector.UpdateDetectorVersionMetadata
  ( -- * Creating a Request
    UpdateDetectorVersionMetadata (..),
    newUpdateDetectorVersionMetadata,

    -- * Request Lenses
    updateDetectorVersionMetadata_detectorId,
    updateDetectorVersionMetadata_detectorVersionId,
    updateDetectorVersionMetadata_description,

    -- * Destructuring the Response
    UpdateDetectorVersionMetadataResponse (..),
    newUpdateDetectorVersionMetadataResponse,

    -- * Response Lenses
    updateDetectorVersionMetadataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDetectorVersionMetadata' smart constructor.
data UpdateDetectorVersionMetadata = UpdateDetectorVersionMetadata'
  { -- | The detector ID.
    detectorId :: Prelude.Text,
    -- | The detector version ID.
    detectorVersionId :: Prelude.Text,
    -- | The description.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDetectorVersionMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'updateDetectorVersionMetadata_detectorId' - The detector ID.
--
-- 'detectorVersionId', 'updateDetectorVersionMetadata_detectorVersionId' - The detector version ID.
--
-- 'description', 'updateDetectorVersionMetadata_description' - The description.
newUpdateDetectorVersionMetadata ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'detectorVersionId'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  UpdateDetectorVersionMetadata
newUpdateDetectorVersionMetadata
  pDetectorId_
  pDetectorVersionId_
  pDescription_ =
    UpdateDetectorVersionMetadata'
      { detectorId =
          pDetectorId_,
        detectorVersionId = pDetectorVersionId_,
        description = pDescription_
      }

-- | The detector ID.
updateDetectorVersionMetadata_detectorId :: Lens.Lens' UpdateDetectorVersionMetadata Prelude.Text
updateDetectorVersionMetadata_detectorId = Lens.lens (\UpdateDetectorVersionMetadata' {detectorId} -> detectorId) (\s@UpdateDetectorVersionMetadata' {} a -> s {detectorId = a} :: UpdateDetectorVersionMetadata)

-- | The detector version ID.
updateDetectorVersionMetadata_detectorVersionId :: Lens.Lens' UpdateDetectorVersionMetadata Prelude.Text
updateDetectorVersionMetadata_detectorVersionId = Lens.lens (\UpdateDetectorVersionMetadata' {detectorVersionId} -> detectorVersionId) (\s@UpdateDetectorVersionMetadata' {} a -> s {detectorVersionId = a} :: UpdateDetectorVersionMetadata)

-- | The description.
updateDetectorVersionMetadata_description :: Lens.Lens' UpdateDetectorVersionMetadata Prelude.Text
updateDetectorVersionMetadata_description = Lens.lens (\UpdateDetectorVersionMetadata' {description} -> description) (\s@UpdateDetectorVersionMetadata' {} a -> s {description = a} :: UpdateDetectorVersionMetadata)

instance
  Core.AWSRequest
    UpdateDetectorVersionMetadata
  where
  type
    AWSResponse UpdateDetectorVersionMetadata =
      UpdateDetectorVersionMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDetectorVersionMetadataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateDetectorVersionMetadata
  where
  hashWithSalt _salt UpdateDetectorVersionMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` detectorVersionId
      `Prelude.hashWithSalt` description

instance Prelude.NFData UpdateDetectorVersionMetadata where
  rnf UpdateDetectorVersionMetadata' {..} =
    Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf detectorVersionId
      `Prelude.seq` Prelude.rnf description

instance Data.ToHeaders UpdateDetectorVersionMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.UpdateDetectorVersionMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDetectorVersionMetadata where
  toJSON UpdateDetectorVersionMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("detectorId" Data..= detectorId),
            Prelude.Just
              ("detectorVersionId" Data..= detectorVersionId),
            Prelude.Just ("description" Data..= description)
          ]
      )

instance Data.ToPath UpdateDetectorVersionMetadata where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDetectorVersionMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDetectorVersionMetadataResponse' smart constructor.
data UpdateDetectorVersionMetadataResponse = UpdateDetectorVersionMetadataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDetectorVersionMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDetectorVersionMetadataResponse_httpStatus' - The response's http status code.
newUpdateDetectorVersionMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDetectorVersionMetadataResponse
newUpdateDetectorVersionMetadataResponse pHttpStatus_ =
  UpdateDetectorVersionMetadataResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDetectorVersionMetadataResponse_httpStatus :: Lens.Lens' UpdateDetectorVersionMetadataResponse Prelude.Int
updateDetectorVersionMetadataResponse_httpStatus = Lens.lens (\UpdateDetectorVersionMetadataResponse' {httpStatus} -> httpStatus) (\s@UpdateDetectorVersionMetadataResponse' {} a -> s {httpStatus = a} :: UpdateDetectorVersionMetadataResponse)

instance
  Prelude.NFData
    UpdateDetectorVersionMetadataResponse
  where
  rnf UpdateDetectorVersionMetadataResponse' {..} =
    Prelude.rnf httpStatus
