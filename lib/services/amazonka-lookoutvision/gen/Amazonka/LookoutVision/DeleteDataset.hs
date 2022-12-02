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
-- Module      : Amazonka.LookoutVision.DeleteDataset
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing Amazon Lookout for Vision @dataset@.
--
-- If your the project has a single dataset, you must create a new dataset
-- before you can create a model.
--
-- If you project has a training dataset and a test dataset consider the
-- following.
--
-- -   If you delete the test dataset, your project reverts to a single
--     dataset project. If you then train the model, Amazon Lookout for
--     Vision internally splits the remaining dataset into a training and
--     test dataset.
--
-- -   If you delete the training dataset, you must create a training
--     dataset before you can create a model.
--
-- This operation requires permissions to perform the
-- @lookoutvision:DeleteDataset@ operation.
module Amazonka.LookoutVision.DeleteDataset
  ( -- * Creating a Request
    DeleteDataset (..),
    newDeleteDataset,

    -- * Request Lenses
    deleteDataset_clientToken,
    deleteDataset_projectName,
    deleteDataset_datasetType,

    -- * Destructuring the Response
    DeleteDatasetResponse (..),
    newDeleteDatasetResponse,

    -- * Response Lenses
    deleteDatasetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutVision.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDataset' smart constructor.
data DeleteDataset = DeleteDataset'
  { -- | ClientToken is an idempotency token that ensures a call to
    -- @DeleteDataset@ completes only once. You choose the value to pass. For
    -- example, An issue might prevent you from getting a response from
    -- @DeleteDataset@. In this case, safely retry your call to @DeleteDataset@
    -- by using the same @ClientToken@ parameter value.
    --
    -- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
    -- using inserts a value for you. This prevents retries after a network
    -- error from making multiple deletetion requests. You\'ll need to provide
    -- your own value for other use cases.
    --
    -- An error occurs if the other input parameters are not the same as in the
    -- first request. Using a different value for @ClientToken@ is considered a
    -- new call to @DeleteDataset@. An idempotency token is active for 8 hours.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the project that contains the dataset that you want to
    -- delete.
    projectName :: Prelude.Text,
    -- | The type of the dataset to delete. Specify @train@ to delete the
    -- training dataset. Specify @test@ to delete the test dataset. To delete
    -- the dataset in a single dataset project, specify @train@.
    datasetType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteDataset_clientToken' - ClientToken is an idempotency token that ensures a call to
-- @DeleteDataset@ completes only once. You choose the value to pass. For
-- example, An issue might prevent you from getting a response from
-- @DeleteDataset@. In this case, safely retry your call to @DeleteDataset@
-- by using the same @ClientToken@ parameter value.
--
-- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
-- using inserts a value for you. This prevents retries after a network
-- error from making multiple deletetion requests. You\'ll need to provide
-- your own value for other use cases.
--
-- An error occurs if the other input parameters are not the same as in the
-- first request. Using a different value for @ClientToken@ is considered a
-- new call to @DeleteDataset@. An idempotency token is active for 8 hours.
--
-- 'projectName', 'deleteDataset_projectName' - The name of the project that contains the dataset that you want to
-- delete.
--
-- 'datasetType', 'deleteDataset_datasetType' - The type of the dataset to delete. Specify @train@ to delete the
-- training dataset. Specify @test@ to delete the test dataset. To delete
-- the dataset in a single dataset project, specify @train@.
newDeleteDataset ::
  -- | 'projectName'
  Prelude.Text ->
  -- | 'datasetType'
  Prelude.Text ->
  DeleteDataset
newDeleteDataset pProjectName_ pDatasetType_ =
  DeleteDataset'
    { clientToken = Prelude.Nothing,
      projectName = pProjectName_,
      datasetType = pDatasetType_
    }

-- | ClientToken is an idempotency token that ensures a call to
-- @DeleteDataset@ completes only once. You choose the value to pass. For
-- example, An issue might prevent you from getting a response from
-- @DeleteDataset@. In this case, safely retry your call to @DeleteDataset@
-- by using the same @ClientToken@ parameter value.
--
-- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
-- using inserts a value for you. This prevents retries after a network
-- error from making multiple deletetion requests. You\'ll need to provide
-- your own value for other use cases.
--
-- An error occurs if the other input parameters are not the same as in the
-- first request. Using a different value for @ClientToken@ is considered a
-- new call to @DeleteDataset@. An idempotency token is active for 8 hours.
deleteDataset_clientToken :: Lens.Lens' DeleteDataset (Prelude.Maybe Prelude.Text)
deleteDataset_clientToken = Lens.lens (\DeleteDataset' {clientToken} -> clientToken) (\s@DeleteDataset' {} a -> s {clientToken = a} :: DeleteDataset)

-- | The name of the project that contains the dataset that you want to
-- delete.
deleteDataset_projectName :: Lens.Lens' DeleteDataset Prelude.Text
deleteDataset_projectName = Lens.lens (\DeleteDataset' {projectName} -> projectName) (\s@DeleteDataset' {} a -> s {projectName = a} :: DeleteDataset)

-- | The type of the dataset to delete. Specify @train@ to delete the
-- training dataset. Specify @test@ to delete the test dataset. To delete
-- the dataset in a single dataset project, specify @train@.
deleteDataset_datasetType :: Lens.Lens' DeleteDataset Prelude.Text
deleteDataset_datasetType = Lens.lens (\DeleteDataset' {datasetType} -> datasetType) (\s@DeleteDataset' {} a -> s {datasetType = a} :: DeleteDataset)

instance Core.AWSRequest DeleteDataset where
  type
    AWSResponse DeleteDataset =
      DeleteDatasetResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDatasetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDataset where
  hashWithSalt _salt DeleteDataset' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` datasetType

instance Prelude.NFData DeleteDataset where
  rnf DeleteDataset' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf datasetType

instance Data.ToHeaders DeleteDataset where
  toHeaders DeleteDataset' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath DeleteDataset where
  toPath DeleteDataset' {..} =
    Prelude.mconcat
      [ "/2020-11-20/projects/",
        Data.toBS projectName,
        "/datasets/",
        Data.toBS datasetType
      ]

instance Data.ToQuery DeleteDataset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDatasetResponse' smart constructor.
data DeleteDatasetResponse = DeleteDatasetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDatasetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDatasetResponse_httpStatus' - The response's http status code.
newDeleteDatasetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDatasetResponse
newDeleteDatasetResponse pHttpStatus_ =
  DeleteDatasetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteDatasetResponse_httpStatus :: Lens.Lens' DeleteDatasetResponse Prelude.Int
deleteDatasetResponse_httpStatus = Lens.lens (\DeleteDatasetResponse' {httpStatus} -> httpStatus) (\s@DeleteDatasetResponse' {} a -> s {httpStatus = a} :: DeleteDatasetResponse)

instance Prelude.NFData DeleteDatasetResponse where
  rnf DeleteDatasetResponse' {..} =
    Prelude.rnf httpStatus
