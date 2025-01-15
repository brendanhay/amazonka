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
-- Module      : Amazonka.Rekognition.UpdateDatasetEntries
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates one or more entries (images) in a dataset. An entry is a
-- JSON Line which contains the information for a single image, including
-- the image location, assigned labels, and object location bounding boxes.
-- For more information, see Image-Level labels in manifest files and
-- Object localization in manifest files in the /Amazon Rekognition Custom
-- Labels Developer Guide/.
--
-- If the @source-ref@ field in the JSON line references an existing image,
-- the existing image in the dataset is updated. If @source-ref@ field
-- doesn\'t reference an existing image, the image is added as a new image
-- to the dataset.
--
-- You specify the changes that you want to make in the @Changes@ input
-- parameter. There isn\'t a limit to the number JSON Lines that you can
-- change, but the size of @Changes@ must be less than 5MB.
--
-- @UpdateDatasetEntries@ returns immediatly, but the dataset update might
-- take a while to complete. Use DescribeDataset to check the current
-- status. The dataset updated successfully if the value of @Status@ is
-- @UPDATE_COMPLETE@.
--
-- To check if any non-terminal errors occured, call ListDatasetEntries and
-- check for the presence of @errors@ lists in the JSON Lines.
--
-- Dataset update fails if a terminal error occurs (@Status@ =
-- @UPDATE_FAILED@). Currently, you can\'t access the terminal error
-- information from the Amazon Rekognition Custom Labels SDK.
--
-- This operation requires permissions to perform the
-- @rekognition:UpdateDatasetEntries@ action.
module Amazonka.Rekognition.UpdateDatasetEntries
  ( -- * Creating a Request
    UpdateDatasetEntries (..),
    newUpdateDatasetEntries,

    -- * Request Lenses
    updateDatasetEntries_datasetArn,
    updateDatasetEntries_changes,

    -- * Destructuring the Response
    UpdateDatasetEntriesResponse (..),
    newUpdateDatasetEntriesResponse,

    -- * Response Lenses
    updateDatasetEntriesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDatasetEntries' smart constructor.
data UpdateDatasetEntries = UpdateDatasetEntries'
  { -- | The Amazon Resource Name (ARN) of the dataset that you want to update.
    datasetArn :: Prelude.Text,
    -- | The changes that you want to make to the dataset.
    changes :: DatasetChanges
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDatasetEntries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetArn', 'updateDatasetEntries_datasetArn' - The Amazon Resource Name (ARN) of the dataset that you want to update.
--
-- 'changes', 'updateDatasetEntries_changes' - The changes that you want to make to the dataset.
newUpdateDatasetEntries ::
  -- | 'datasetArn'
  Prelude.Text ->
  -- | 'changes'
  DatasetChanges ->
  UpdateDatasetEntries
newUpdateDatasetEntries pDatasetArn_ pChanges_ =
  UpdateDatasetEntries'
    { datasetArn = pDatasetArn_,
      changes = pChanges_
    }

-- | The Amazon Resource Name (ARN) of the dataset that you want to update.
updateDatasetEntries_datasetArn :: Lens.Lens' UpdateDatasetEntries Prelude.Text
updateDatasetEntries_datasetArn = Lens.lens (\UpdateDatasetEntries' {datasetArn} -> datasetArn) (\s@UpdateDatasetEntries' {} a -> s {datasetArn = a} :: UpdateDatasetEntries)

-- | The changes that you want to make to the dataset.
updateDatasetEntries_changes :: Lens.Lens' UpdateDatasetEntries DatasetChanges
updateDatasetEntries_changes = Lens.lens (\UpdateDatasetEntries' {changes} -> changes) (\s@UpdateDatasetEntries' {} a -> s {changes = a} :: UpdateDatasetEntries)

instance Core.AWSRequest UpdateDatasetEntries where
  type
    AWSResponse UpdateDatasetEntries =
      UpdateDatasetEntriesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDatasetEntriesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDatasetEntries where
  hashWithSalt _salt UpdateDatasetEntries' {..} =
    _salt
      `Prelude.hashWithSalt` datasetArn
      `Prelude.hashWithSalt` changes

instance Prelude.NFData UpdateDatasetEntries where
  rnf UpdateDatasetEntries' {..} =
    Prelude.rnf datasetArn `Prelude.seq`
      Prelude.rnf changes

instance Data.ToHeaders UpdateDatasetEntries where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.UpdateDatasetEntries" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDatasetEntries where
  toJSON UpdateDatasetEntries' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DatasetArn" Data..= datasetArn),
            Prelude.Just ("Changes" Data..= changes)
          ]
      )

instance Data.ToPath UpdateDatasetEntries where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDatasetEntries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDatasetEntriesResponse' smart constructor.
data UpdateDatasetEntriesResponse = UpdateDatasetEntriesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDatasetEntriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDatasetEntriesResponse_httpStatus' - The response's http status code.
newUpdateDatasetEntriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDatasetEntriesResponse
newUpdateDatasetEntriesResponse pHttpStatus_ =
  UpdateDatasetEntriesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDatasetEntriesResponse_httpStatus :: Lens.Lens' UpdateDatasetEntriesResponse Prelude.Int
updateDatasetEntriesResponse_httpStatus = Lens.lens (\UpdateDatasetEntriesResponse' {httpStatus} -> httpStatus) (\s@UpdateDatasetEntriesResponse' {} a -> s {httpStatus = a} :: UpdateDatasetEntriesResponse)

instance Prelude.NFData UpdateDatasetEntriesResponse where
  rnf UpdateDatasetEntriesResponse' {..} =
    Prelude.rnf httpStatus
