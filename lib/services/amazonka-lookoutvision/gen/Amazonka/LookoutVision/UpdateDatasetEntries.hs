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
-- Module      : Amazonka.LookoutVision.UpdateDatasetEntries
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates one or more JSON Line entries in a dataset. A JSON Line
-- includes information about an image used for training or testing an
-- Amazon Lookout for Vision model.
--
-- To update an existing JSON Line, use the @source-ref@ field to identify
-- the JSON Line. The JSON line that you supply replaces the existing JSON
-- line. Any existing annotations that are not in the new JSON line are
-- removed from the dataset.
--
-- For more information, see /Defining JSON lines for anomaly
-- classification/ in the Amazon Lookout for Vision Developer Guide.
--
-- The images you reference in the @source-ref@ field of a JSON line, must
-- be in the same S3 bucket as the existing images in the dataset.
--
-- Updating a dataset might take a while to complete. To check the current
-- status, call DescribeDataset and check the @Status@ field in the
-- response.
--
-- This operation requires permissions to perform the
-- @lookoutvision:UpdateDatasetEntries@ operation.
module Amazonka.LookoutVision.UpdateDatasetEntries
  ( -- * Creating a Request
    UpdateDatasetEntries (..),
    newUpdateDatasetEntries,

    -- * Request Lenses
    updateDatasetEntries_clientToken,
    updateDatasetEntries_projectName,
    updateDatasetEntries_datasetType,
    updateDatasetEntries_changes,

    -- * Destructuring the Response
    UpdateDatasetEntriesResponse (..),
    newUpdateDatasetEntriesResponse,

    -- * Response Lenses
    updateDatasetEntriesResponse_status,
    updateDatasetEntriesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutVision.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDatasetEntries' smart constructor.
data UpdateDatasetEntries = UpdateDatasetEntries'
  { -- | ClientToken is an idempotency token that ensures a call to
    -- @UpdateDatasetEntries@ completes only once. You choose the value to
    -- pass. For example, An issue might prevent you from getting a response
    -- from @UpdateDatasetEntries@. In this case, safely retry your call to
    -- @UpdateDatasetEntries@ by using the same @ClientToken@ parameter value.
    --
    -- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
    -- using inserts a value for you. This prevents retries after a network
    -- error from making multiple updates with the same dataset entries.
    -- You\'ll need to provide your own value for other use cases.
    --
    -- An error occurs if the other input parameters are not the same as in the
    -- first request. Using a different value for @ClientToken@ is considered a
    -- new call to @UpdateDatasetEntries@. An idempotency token is active for 8
    -- hours.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the project that contains the dataset that you want to
    -- update.
    projectName :: Prelude.Text,
    -- | The type of the dataset that you want to update. Specify @train@ to
    -- update the training dataset. Specify @test@ to update the test dataset.
    -- If you have a single dataset project, specify @train@.
    datasetType :: Prelude.Text,
    -- | The entries to add to the dataset.
    changes :: Data.Base64
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
-- 'clientToken', 'updateDatasetEntries_clientToken' - ClientToken is an idempotency token that ensures a call to
-- @UpdateDatasetEntries@ completes only once. You choose the value to
-- pass. For example, An issue might prevent you from getting a response
-- from @UpdateDatasetEntries@. In this case, safely retry your call to
-- @UpdateDatasetEntries@ by using the same @ClientToken@ parameter value.
--
-- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
-- using inserts a value for you. This prevents retries after a network
-- error from making multiple updates with the same dataset entries.
-- You\'ll need to provide your own value for other use cases.
--
-- An error occurs if the other input parameters are not the same as in the
-- first request. Using a different value for @ClientToken@ is considered a
-- new call to @UpdateDatasetEntries@. An idempotency token is active for 8
-- hours.
--
-- 'projectName', 'updateDatasetEntries_projectName' - The name of the project that contains the dataset that you want to
-- update.
--
-- 'datasetType', 'updateDatasetEntries_datasetType' - The type of the dataset that you want to update. Specify @train@ to
-- update the training dataset. Specify @test@ to update the test dataset.
-- If you have a single dataset project, specify @train@.
--
-- 'changes', 'updateDatasetEntries_changes' - The entries to add to the dataset.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newUpdateDatasetEntries ::
  -- | 'projectName'
  Prelude.Text ->
  -- | 'datasetType'
  Prelude.Text ->
  -- | 'changes'
  Prelude.ByteString ->
  UpdateDatasetEntries
newUpdateDatasetEntries
  pProjectName_
  pDatasetType_
  pChanges_ =
    UpdateDatasetEntries'
      { clientToken =
          Prelude.Nothing,
        projectName = pProjectName_,
        datasetType = pDatasetType_,
        changes = Data._Base64 Lens.# pChanges_
      }

-- | ClientToken is an idempotency token that ensures a call to
-- @UpdateDatasetEntries@ completes only once. You choose the value to
-- pass. For example, An issue might prevent you from getting a response
-- from @UpdateDatasetEntries@. In this case, safely retry your call to
-- @UpdateDatasetEntries@ by using the same @ClientToken@ parameter value.
--
-- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
-- using inserts a value for you. This prevents retries after a network
-- error from making multiple updates with the same dataset entries.
-- You\'ll need to provide your own value for other use cases.
--
-- An error occurs if the other input parameters are not the same as in the
-- first request. Using a different value for @ClientToken@ is considered a
-- new call to @UpdateDatasetEntries@. An idempotency token is active for 8
-- hours.
updateDatasetEntries_clientToken :: Lens.Lens' UpdateDatasetEntries (Prelude.Maybe Prelude.Text)
updateDatasetEntries_clientToken = Lens.lens (\UpdateDatasetEntries' {clientToken} -> clientToken) (\s@UpdateDatasetEntries' {} a -> s {clientToken = a} :: UpdateDatasetEntries)

-- | The name of the project that contains the dataset that you want to
-- update.
updateDatasetEntries_projectName :: Lens.Lens' UpdateDatasetEntries Prelude.Text
updateDatasetEntries_projectName = Lens.lens (\UpdateDatasetEntries' {projectName} -> projectName) (\s@UpdateDatasetEntries' {} a -> s {projectName = a} :: UpdateDatasetEntries)

-- | The type of the dataset that you want to update. Specify @train@ to
-- update the training dataset. Specify @test@ to update the test dataset.
-- If you have a single dataset project, specify @train@.
updateDatasetEntries_datasetType :: Lens.Lens' UpdateDatasetEntries Prelude.Text
updateDatasetEntries_datasetType = Lens.lens (\UpdateDatasetEntries' {datasetType} -> datasetType) (\s@UpdateDatasetEntries' {} a -> s {datasetType = a} :: UpdateDatasetEntries)

-- | The entries to add to the dataset.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
updateDatasetEntries_changes :: Lens.Lens' UpdateDatasetEntries Prelude.ByteString
updateDatasetEntries_changes = Lens.lens (\UpdateDatasetEntries' {changes} -> changes) (\s@UpdateDatasetEntries' {} a -> s {changes = a} :: UpdateDatasetEntries) Prelude.. Data._Base64

instance Core.AWSRequest UpdateDatasetEntries where
  type
    AWSResponse UpdateDatasetEntries =
      UpdateDatasetEntriesResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDatasetEntriesResponse'
            Prelude.<$> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDatasetEntries where
  hashWithSalt _salt UpdateDatasetEntries' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` datasetType
      `Prelude.hashWithSalt` changes

instance Prelude.NFData UpdateDatasetEntries where
  rnf UpdateDatasetEntries' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf datasetType
      `Prelude.seq` Prelude.rnf changes

instance Data.ToHeaders UpdateDatasetEntries where
  toHeaders UpdateDatasetEntries' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON UpdateDatasetEntries where
  toJSON UpdateDatasetEntries' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Changes" Data..= changes)]
      )

instance Data.ToPath UpdateDatasetEntries where
  toPath UpdateDatasetEntries' {..} =
    Prelude.mconcat
      [ "/2020-11-20/projects/",
        Data.toBS projectName,
        "/datasets/",
        Data.toBS datasetType,
        "/entries"
      ]

instance Data.ToQuery UpdateDatasetEntries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDatasetEntriesResponse' smart constructor.
data UpdateDatasetEntriesResponse = UpdateDatasetEntriesResponse'
  { -- | The status of the dataset update.
    status :: Prelude.Maybe DatasetStatus,
    -- | The response's http status code.
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
-- 'status', 'updateDatasetEntriesResponse_status' - The status of the dataset update.
--
-- 'httpStatus', 'updateDatasetEntriesResponse_httpStatus' - The response's http status code.
newUpdateDatasetEntriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDatasetEntriesResponse
newUpdateDatasetEntriesResponse pHttpStatus_ =
  UpdateDatasetEntriesResponse'
    { status =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the dataset update.
updateDatasetEntriesResponse_status :: Lens.Lens' UpdateDatasetEntriesResponse (Prelude.Maybe DatasetStatus)
updateDatasetEntriesResponse_status = Lens.lens (\UpdateDatasetEntriesResponse' {status} -> status) (\s@UpdateDatasetEntriesResponse' {} a -> s {status = a} :: UpdateDatasetEntriesResponse)

-- | The response's http status code.
updateDatasetEntriesResponse_httpStatus :: Lens.Lens' UpdateDatasetEntriesResponse Prelude.Int
updateDatasetEntriesResponse_httpStatus = Lens.lens (\UpdateDatasetEntriesResponse' {httpStatus} -> httpStatus) (\s@UpdateDatasetEntriesResponse' {} a -> s {httpStatus = a} :: UpdateDatasetEntriesResponse)

instance Prelude.NFData UpdateDatasetEntriesResponse where
  rnf UpdateDatasetEntriesResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
