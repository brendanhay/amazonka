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
-- Module      : Amazonka.Comprehend.CreateDataset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a dataset to upload training or test data for a model associated
-- with a flywheel. For more information about datasets, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/flywheels-about.html Flywheel overview>
-- in the /Amazon Comprehend Developer Guide/.
module Amazonka.Comprehend.CreateDataset
  ( -- * Creating a Request
    CreateDataset (..),
    newCreateDataset,

    -- * Request Lenses
    createDataset_clientRequestToken,
    createDataset_datasetType,
    createDataset_description,
    createDataset_tags,
    createDataset_flywheelArn,
    createDataset_datasetName,
    createDataset_inputDataConfig,

    -- * Destructuring the Response
    CreateDatasetResponse (..),
    newCreateDatasetResponse,

    -- * Response Lenses
    createDatasetResponse_datasetArn,
    createDatasetResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDataset' smart constructor.
data CreateDataset = CreateDataset'
  { -- | A unique identifier for the request. If you don\'t set the client
    -- request token, Amazon Comprehend generates one.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The dataset type. You can specify that the data in a dataset is for
    -- training the model or for testing the model.
    datasetType :: Prelude.Maybe DatasetType,
    -- | Description of the dataset.
    description :: Prelude.Maybe Prelude.Text,
    -- | Tags for the dataset.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon Resource Number (ARN) of the flywheel of the flywheel to
    -- receive the data.
    flywheelArn :: Prelude.Text,
    -- | Name of the dataset.
    datasetName :: Prelude.Text,
    -- | Information about the input data configuration. The type of input data
    -- varies based on the format of the input and whether the data is for a
    -- classifier model or an entity recognition model.
    inputDataConfig :: DatasetInputDataConfig
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
-- 'clientRequestToken', 'createDataset_clientRequestToken' - A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
--
-- 'datasetType', 'createDataset_datasetType' - The dataset type. You can specify that the data in a dataset is for
-- training the model or for testing the model.
--
-- 'description', 'createDataset_description' - Description of the dataset.
--
-- 'tags', 'createDataset_tags' - Tags for the dataset.
--
-- 'flywheelArn', 'createDataset_flywheelArn' - The Amazon Resource Number (ARN) of the flywheel of the flywheel to
-- receive the data.
--
-- 'datasetName', 'createDataset_datasetName' - Name of the dataset.
--
-- 'inputDataConfig', 'createDataset_inputDataConfig' - Information about the input data configuration. The type of input data
-- varies based on the format of the input and whether the data is for a
-- classifier model or an entity recognition model.
newCreateDataset ::
  -- | 'flywheelArn'
  Prelude.Text ->
  -- | 'datasetName'
  Prelude.Text ->
  -- | 'inputDataConfig'
  DatasetInputDataConfig ->
  CreateDataset
newCreateDataset
  pFlywheelArn_
  pDatasetName_
  pInputDataConfig_ =
    CreateDataset'
      { clientRequestToken =
          Prelude.Nothing,
        datasetType = Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        flywheelArn = pFlywheelArn_,
        datasetName = pDatasetName_,
        inputDataConfig = pInputDataConfig_
      }

-- | A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
createDataset_clientRequestToken :: Lens.Lens' CreateDataset (Prelude.Maybe Prelude.Text)
createDataset_clientRequestToken = Lens.lens (\CreateDataset' {clientRequestToken} -> clientRequestToken) (\s@CreateDataset' {} a -> s {clientRequestToken = a} :: CreateDataset)

-- | The dataset type. You can specify that the data in a dataset is for
-- training the model or for testing the model.
createDataset_datasetType :: Lens.Lens' CreateDataset (Prelude.Maybe DatasetType)
createDataset_datasetType = Lens.lens (\CreateDataset' {datasetType} -> datasetType) (\s@CreateDataset' {} a -> s {datasetType = a} :: CreateDataset)

-- | Description of the dataset.
createDataset_description :: Lens.Lens' CreateDataset (Prelude.Maybe Prelude.Text)
createDataset_description = Lens.lens (\CreateDataset' {description} -> description) (\s@CreateDataset' {} a -> s {description = a} :: CreateDataset)

-- | Tags for the dataset.
createDataset_tags :: Lens.Lens' CreateDataset (Prelude.Maybe [Tag])
createDataset_tags = Lens.lens (\CreateDataset' {tags} -> tags) (\s@CreateDataset' {} a -> s {tags = a} :: CreateDataset) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Number (ARN) of the flywheel of the flywheel to
-- receive the data.
createDataset_flywheelArn :: Lens.Lens' CreateDataset Prelude.Text
createDataset_flywheelArn = Lens.lens (\CreateDataset' {flywheelArn} -> flywheelArn) (\s@CreateDataset' {} a -> s {flywheelArn = a} :: CreateDataset)

-- | Name of the dataset.
createDataset_datasetName :: Lens.Lens' CreateDataset Prelude.Text
createDataset_datasetName = Lens.lens (\CreateDataset' {datasetName} -> datasetName) (\s@CreateDataset' {} a -> s {datasetName = a} :: CreateDataset)

-- | Information about the input data configuration. The type of input data
-- varies based on the format of the input and whether the data is for a
-- classifier model or an entity recognition model.
createDataset_inputDataConfig :: Lens.Lens' CreateDataset DatasetInputDataConfig
createDataset_inputDataConfig = Lens.lens (\CreateDataset' {inputDataConfig} -> inputDataConfig) (\s@CreateDataset' {} a -> s {inputDataConfig = a} :: CreateDataset)

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
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` datasetType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` flywheelArn
      `Prelude.hashWithSalt` datasetName
      `Prelude.hashWithSalt` inputDataConfig

instance Prelude.NFData CreateDataset where
  rnf CreateDataset' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf datasetType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf flywheelArn
      `Prelude.seq` Prelude.rnf datasetName
      `Prelude.seq` Prelude.rnf inputDataConfig

instance Data.ToHeaders CreateDataset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.CreateDataset" ::
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
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("DatasetType" Data..=) Prelude.<$> datasetType,
            ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("FlywheelArn" Data..= flywheelArn),
            Prelude.Just ("DatasetName" Data..= datasetName),
            Prelude.Just
              ("InputDataConfig" Data..= inputDataConfig)
          ]
      )

instance Data.ToPath CreateDataset where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDataset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDatasetResponse' smart constructor.
data CreateDatasetResponse = CreateDatasetResponse'
  { -- | The ARN of the dataset.
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
-- 'datasetArn', 'createDatasetResponse_datasetArn' - The ARN of the dataset.
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

-- | The ARN of the dataset.
createDatasetResponse_datasetArn :: Lens.Lens' CreateDatasetResponse (Prelude.Maybe Prelude.Text)
createDatasetResponse_datasetArn = Lens.lens (\CreateDatasetResponse' {datasetArn} -> datasetArn) (\s@CreateDatasetResponse' {} a -> s {datasetArn = a} :: CreateDatasetResponse)

-- | The response's http status code.
createDatasetResponse_httpStatus :: Lens.Lens' CreateDatasetResponse Prelude.Int
createDatasetResponse_httpStatus = Lens.lens (\CreateDatasetResponse' {httpStatus} -> httpStatus) (\s@CreateDatasetResponse' {} a -> s {httpStatus = a} :: CreateDatasetResponse)

instance Prelude.NFData CreateDatasetResponse where
  rnf CreateDatasetResponse' {..} =
    Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf httpStatus
