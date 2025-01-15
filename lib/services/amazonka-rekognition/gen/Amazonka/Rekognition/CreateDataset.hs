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
-- Module      : Amazonka.Rekognition.CreateDataset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Rekognition Custom Labels dataset. You can create a
-- dataset by using an Amazon Sagemaker format manifest file or by copying
-- an existing Amazon Rekognition Custom Labels dataset.
--
-- To create a training dataset for a project, specify @train@ for the
-- value of @DatasetType@. To create the test dataset for a project,
-- specify @test@ for the value of @DatasetType@.
--
-- The response from @CreateDataset@ is the Amazon Resource Name (ARN) for
-- the dataset. Creating a dataset takes a while to complete. Use
-- DescribeDataset to check the current status. The dataset created
-- successfully if the value of @Status@ is @CREATE_COMPLETE@.
--
-- To check if any non-terminal errors occurred, call ListDatasetEntries
-- and check for the presence of @errors@ lists in the JSON Lines.
--
-- Dataset creation fails if a terminal error occurs (@Status@ =
-- @CREATE_FAILED@). Currently, you can\'t access the terminal error
-- information.
--
-- For more information, see Creating dataset in the /Amazon Rekognition
-- Custom Labels Developer Guide/.
--
-- This operation requires permissions to perform the
-- @rekognition:CreateDataset@ action. If you want to copy an existing
-- dataset, you also require permission to perform the
-- @rekognition:ListDatasetEntries@ action.
module Amazonka.Rekognition.CreateDataset
  ( -- * Creating a Request
    CreateDataset (..),
    newCreateDataset,

    -- * Request Lenses
    createDataset_datasetSource,
    createDataset_datasetType,
    createDataset_projectArn,

    -- * Destructuring the Response
    CreateDatasetResponse (..),
    newCreateDatasetResponse,

    -- * Response Lenses
    createDatasetResponse_datasetArn,
    createDatasetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDataset' smart constructor.
data CreateDataset = CreateDataset'
  { -- | The source files for the dataset. You can specify the ARN of an existing
    -- dataset or specify the Amazon S3 bucket location of an Amazon Sagemaker
    -- format manifest file. If you don\'t specify @datasetSource@, an empty
    -- dataset is created. To add labeled images to the dataset, You can use
    -- the console or call UpdateDatasetEntries.
    datasetSource :: Prelude.Maybe DatasetSource,
    -- | The type of the dataset. Specify @train@ to create a training dataset.
    -- Specify @test@ to create a test dataset.
    datasetType :: DatasetType,
    -- | The ARN of the Amazon Rekognition Custom Labels project to which you
    -- want to asssign the dataset.
    projectArn :: Prelude.Text
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
-- 'datasetSource', 'createDataset_datasetSource' - The source files for the dataset. You can specify the ARN of an existing
-- dataset or specify the Amazon S3 bucket location of an Amazon Sagemaker
-- format manifest file. If you don\'t specify @datasetSource@, an empty
-- dataset is created. To add labeled images to the dataset, You can use
-- the console or call UpdateDatasetEntries.
--
-- 'datasetType', 'createDataset_datasetType' - The type of the dataset. Specify @train@ to create a training dataset.
-- Specify @test@ to create a test dataset.
--
-- 'projectArn', 'createDataset_projectArn' - The ARN of the Amazon Rekognition Custom Labels project to which you
-- want to asssign the dataset.
newCreateDataset ::
  -- | 'datasetType'
  DatasetType ->
  -- | 'projectArn'
  Prelude.Text ->
  CreateDataset
newCreateDataset pDatasetType_ pProjectArn_ =
  CreateDataset'
    { datasetSource = Prelude.Nothing,
      datasetType = pDatasetType_,
      projectArn = pProjectArn_
    }

-- | The source files for the dataset. You can specify the ARN of an existing
-- dataset or specify the Amazon S3 bucket location of an Amazon Sagemaker
-- format manifest file. If you don\'t specify @datasetSource@, an empty
-- dataset is created. To add labeled images to the dataset, You can use
-- the console or call UpdateDatasetEntries.
createDataset_datasetSource :: Lens.Lens' CreateDataset (Prelude.Maybe DatasetSource)
createDataset_datasetSource = Lens.lens (\CreateDataset' {datasetSource} -> datasetSource) (\s@CreateDataset' {} a -> s {datasetSource = a} :: CreateDataset)

-- | The type of the dataset. Specify @train@ to create a training dataset.
-- Specify @test@ to create a test dataset.
createDataset_datasetType :: Lens.Lens' CreateDataset DatasetType
createDataset_datasetType = Lens.lens (\CreateDataset' {datasetType} -> datasetType) (\s@CreateDataset' {} a -> s {datasetType = a} :: CreateDataset)

-- | The ARN of the Amazon Rekognition Custom Labels project to which you
-- want to asssign the dataset.
createDataset_projectArn :: Lens.Lens' CreateDataset Prelude.Text
createDataset_projectArn = Lens.lens (\CreateDataset' {projectArn} -> projectArn) (\s@CreateDataset' {} a -> s {projectArn = a} :: CreateDataset)

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
            Prelude.<$> (x Data..?> "DatasetArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDataset where
  hashWithSalt _salt CreateDataset' {..} =
    _salt
      `Prelude.hashWithSalt` datasetSource
      `Prelude.hashWithSalt` datasetType
      `Prelude.hashWithSalt` projectArn

instance Prelude.NFData CreateDataset where
  rnf CreateDataset' {..} =
    Prelude.rnf datasetSource `Prelude.seq`
      Prelude.rnf datasetType `Prelude.seq`
        Prelude.rnf projectArn

instance Data.ToHeaders CreateDataset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.CreateDataset" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDataset where
  toJSON CreateDataset' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DatasetSource" Data..=) Prelude.<$> datasetSource,
            Prelude.Just ("DatasetType" Data..= datasetType),
            Prelude.Just ("ProjectArn" Data..= projectArn)
          ]
      )

instance Data.ToPath CreateDataset where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDataset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDatasetResponse' smart constructor.
data CreateDatasetResponse = CreateDatasetResponse'
  { -- | The ARN of the created Amazon Rekognition Custom Labels dataset.
    datasetArn :: Prelude.Maybe Prelude.Text,
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
-- 'datasetArn', 'createDatasetResponse_datasetArn' - The ARN of the created Amazon Rekognition Custom Labels dataset.
--
-- 'httpStatus', 'createDatasetResponse_httpStatus' - The response's http status code.
newCreateDatasetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDatasetResponse
newCreateDatasetResponse pHttpStatus_ =
  CreateDatasetResponse'
    { datasetArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the created Amazon Rekognition Custom Labels dataset.
createDatasetResponse_datasetArn :: Lens.Lens' CreateDatasetResponse (Prelude.Maybe Prelude.Text)
createDatasetResponse_datasetArn = Lens.lens (\CreateDatasetResponse' {datasetArn} -> datasetArn) (\s@CreateDatasetResponse' {} a -> s {datasetArn = a} :: CreateDatasetResponse)

-- | The response's http status code.
createDatasetResponse_httpStatus :: Lens.Lens' CreateDatasetResponse Prelude.Int
createDatasetResponse_httpStatus = Lens.lens (\CreateDatasetResponse' {httpStatus} -> httpStatus) (\s@CreateDatasetResponse' {} a -> s {httpStatus = a} :: CreateDatasetResponse)

instance Prelude.NFData CreateDatasetResponse where
  rnf CreateDatasetResponse' {..} =
    Prelude.rnf datasetArn `Prelude.seq`
      Prelude.rnf httpStatus
