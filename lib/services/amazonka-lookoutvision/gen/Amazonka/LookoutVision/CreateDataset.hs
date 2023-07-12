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
-- Module      : Amazonka.LookoutVision.CreateDataset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new dataset in an Amazon Lookout for Vision project.
-- @CreateDataset@ can create a training or a test dataset from a valid
-- dataset source (@DatasetSource@).
--
-- If you want a single dataset project, specify @train@ for the value of
-- @DatasetType@.
--
-- To have a project with separate training and test datasets, call
-- @CreateDataset@ twice. On the first call, specify @train@ for the value
-- of @DatasetType@. On the second call, specify @test@ for the value of
-- @DatasetType@.
--
-- This operation requires permissions to perform the
-- @lookoutvision:CreateDataset@ operation.
module Amazonka.LookoutVision.CreateDataset
  ( -- * Creating a Request
    CreateDataset (..),
    newCreateDataset,

    -- * Request Lenses
    createDataset_clientToken,
    createDataset_datasetSource,
    createDataset_projectName,
    createDataset_datasetType,

    -- * Destructuring the Response
    CreateDatasetResponse (..),
    newCreateDatasetResponse,

    -- * Response Lenses
    createDatasetResponse_datasetMetadata,
    createDatasetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutVision.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDataset' smart constructor.
data CreateDataset = CreateDataset'
  { -- | ClientToken is an idempotency token that ensures a call to
    -- @CreateDataset@ completes only once. You choose the value to pass. For
    -- example, An issue might prevent you from getting a response from
    -- @CreateDataset@. In this case, safely retry your call to @CreateDataset@
    -- by using the same @ClientToken@ parameter value.
    --
    -- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
    -- using inserts a value for you. This prevents retries after a network
    -- error from making multiple dataset creation requests. You\'ll need to
    -- provide your own value for other use cases.
    --
    -- An error occurs if the other input parameters are not the same as in the
    -- first request. Using a different value for @ClientToken@ is considered a
    -- new call to @CreateDataset@. An idempotency token is active for 8 hours.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The location of the manifest file that Amazon Lookout for Vision uses to
    -- create the dataset.
    --
    -- If you don\'t specify @DatasetSource@, an empty dataset is created and
    -- the operation synchronously returns. Later, you can add JSON Lines by
    -- calling UpdateDatasetEntries.
    --
    -- If you specify a value for @DataSource@, the manifest at the S3 location
    -- is validated and used to create the dataset. The call to @CreateDataset@
    -- is asynchronous and might take a while to complete. To find out the
    -- current status, Check the value of @Status@ returned in a call to
    -- DescribeDataset.
    datasetSource :: Prelude.Maybe DatasetSource,
    -- | The name of the project in which you want to create a dataset.
    projectName :: Prelude.Text,
    -- | The type of the dataset. Specify @train@ for a training dataset. Specify
    -- @test@ for a test dataset.
    datasetType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createDataset_clientToken' - ClientToken is an idempotency token that ensures a call to
-- @CreateDataset@ completes only once. You choose the value to pass. For
-- example, An issue might prevent you from getting a response from
-- @CreateDataset@. In this case, safely retry your call to @CreateDataset@
-- by using the same @ClientToken@ parameter value.
--
-- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
-- using inserts a value for you. This prevents retries after a network
-- error from making multiple dataset creation requests. You\'ll need to
-- provide your own value for other use cases.
--
-- An error occurs if the other input parameters are not the same as in the
-- first request. Using a different value for @ClientToken@ is considered a
-- new call to @CreateDataset@. An idempotency token is active for 8 hours.
--
-- 'datasetSource', 'createDataset_datasetSource' - The location of the manifest file that Amazon Lookout for Vision uses to
-- create the dataset.
--
-- If you don\'t specify @DatasetSource@, an empty dataset is created and
-- the operation synchronously returns. Later, you can add JSON Lines by
-- calling UpdateDatasetEntries.
--
-- If you specify a value for @DataSource@, the manifest at the S3 location
-- is validated and used to create the dataset. The call to @CreateDataset@
-- is asynchronous and might take a while to complete. To find out the
-- current status, Check the value of @Status@ returned in a call to
-- DescribeDataset.
--
-- 'projectName', 'createDataset_projectName' - The name of the project in which you want to create a dataset.
--
-- 'datasetType', 'createDataset_datasetType' - The type of the dataset. Specify @train@ for a training dataset. Specify
-- @test@ for a test dataset.
newCreateDataset ::
  -- | 'projectName'
  Prelude.Text ->
  -- | 'datasetType'
  Prelude.Text ->
  CreateDataset
newCreateDataset pProjectName_ pDatasetType_ =
  CreateDataset'
    { clientToken = Prelude.Nothing,
      datasetSource = Prelude.Nothing,
      projectName = pProjectName_,
      datasetType = pDatasetType_
    }

-- | ClientToken is an idempotency token that ensures a call to
-- @CreateDataset@ completes only once. You choose the value to pass. For
-- example, An issue might prevent you from getting a response from
-- @CreateDataset@. In this case, safely retry your call to @CreateDataset@
-- by using the same @ClientToken@ parameter value.
--
-- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
-- using inserts a value for you. This prevents retries after a network
-- error from making multiple dataset creation requests. You\'ll need to
-- provide your own value for other use cases.
--
-- An error occurs if the other input parameters are not the same as in the
-- first request. Using a different value for @ClientToken@ is considered a
-- new call to @CreateDataset@. An idempotency token is active for 8 hours.
createDataset_clientToken :: Lens.Lens' CreateDataset (Prelude.Maybe Prelude.Text)
createDataset_clientToken = Lens.lens (\CreateDataset' {clientToken} -> clientToken) (\s@CreateDataset' {} a -> s {clientToken = a} :: CreateDataset)

-- | The location of the manifest file that Amazon Lookout for Vision uses to
-- create the dataset.
--
-- If you don\'t specify @DatasetSource@, an empty dataset is created and
-- the operation synchronously returns. Later, you can add JSON Lines by
-- calling UpdateDatasetEntries.
--
-- If you specify a value for @DataSource@, the manifest at the S3 location
-- is validated and used to create the dataset. The call to @CreateDataset@
-- is asynchronous and might take a while to complete. To find out the
-- current status, Check the value of @Status@ returned in a call to
-- DescribeDataset.
createDataset_datasetSource :: Lens.Lens' CreateDataset (Prelude.Maybe DatasetSource)
createDataset_datasetSource = Lens.lens (\CreateDataset' {datasetSource} -> datasetSource) (\s@CreateDataset' {} a -> s {datasetSource = a} :: CreateDataset)

-- | The name of the project in which you want to create a dataset.
createDataset_projectName :: Lens.Lens' CreateDataset Prelude.Text
createDataset_projectName = Lens.lens (\CreateDataset' {projectName} -> projectName) (\s@CreateDataset' {} a -> s {projectName = a} :: CreateDataset)

-- | The type of the dataset. Specify @train@ for a training dataset. Specify
-- @test@ for a test dataset.
createDataset_datasetType :: Lens.Lens' CreateDataset Prelude.Text
createDataset_datasetType = Lens.lens (\CreateDataset' {datasetType} -> datasetType) (\s@CreateDataset' {} a -> s {datasetType = a} :: CreateDataset)

instance Core.AWSRequest CreateDataset where
  type
    AWSResponse CreateDataset =
      CreateDatasetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDatasetResponse'
            Prelude.<$> (x Data..?> "DatasetMetadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDataset where
  hashWithSalt _salt CreateDataset' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` datasetSource
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` datasetType

instance Prelude.NFData CreateDataset where
  rnf CreateDataset' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf datasetSource
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf datasetType

instance Data.ToHeaders CreateDataset where
  toHeaders CreateDataset' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreateDataset where
  toJSON CreateDataset' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DatasetSource" Data..=) Prelude.<$> datasetSource,
            Prelude.Just ("DatasetType" Data..= datasetType)
          ]
      )

instance Data.ToPath CreateDataset where
  toPath CreateDataset' {..} =
    Prelude.mconcat
      [ "/2020-11-20/projects/",
        Data.toBS projectName,
        "/datasets"
      ]

instance Data.ToQuery CreateDataset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDatasetResponse' smart constructor.
data CreateDatasetResponse = CreateDatasetResponse'
  { -- | Information about the dataset.
    datasetMetadata :: Prelude.Maybe DatasetMetadata,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatasetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetMetadata', 'createDatasetResponse_datasetMetadata' - Information about the dataset.
--
-- 'httpStatus', 'createDatasetResponse_httpStatus' - The response's http status code.
newCreateDatasetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDatasetResponse
newCreateDatasetResponse pHttpStatus_ =
  CreateDatasetResponse'
    { datasetMetadata =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the dataset.
createDatasetResponse_datasetMetadata :: Lens.Lens' CreateDatasetResponse (Prelude.Maybe DatasetMetadata)
createDatasetResponse_datasetMetadata = Lens.lens (\CreateDatasetResponse' {datasetMetadata} -> datasetMetadata) (\s@CreateDatasetResponse' {} a -> s {datasetMetadata = a} :: CreateDatasetResponse)

-- | The response's http status code.
createDatasetResponse_httpStatus :: Lens.Lens' CreateDatasetResponse Prelude.Int
createDatasetResponse_httpStatus = Lens.lens (\CreateDatasetResponse' {httpStatus} -> httpStatus) (\s@CreateDatasetResponse' {} a -> s {httpStatus = a} :: CreateDatasetResponse)

instance Prelude.NFData CreateDatasetResponse where
  rnf CreateDatasetResponse' {..} =
    Prelude.rnf datasetMetadata
      `Prelude.seq` Prelude.rnf httpStatus
