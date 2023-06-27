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
-- Module      : Amazonka.Comprehend.CreateFlywheel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A flywheel is an Amazon Web Services resource that orchestrates the
-- ongoing training of a model for custom classification or custom entity
-- recognition. You can create a flywheel to start with an existing trained
-- model, or Comprehend can create and train a new model.
--
-- When you create the flywheel, Comprehend creates a data lake in your
-- account. The data lake holds the training data and test data for all
-- versions of the model.
--
-- To use a flywheel with an existing trained model, you specify the active
-- model version. Comprehend copies the model\'s training data and test
-- data into the flywheel\'s data lake.
--
-- To use the flywheel with a new model, you need to provide a dataset for
-- training data (and optional test data) when you create the flywheel.
--
-- For more information about flywheels, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/flywheels-about.html Flywheel overview>
-- in the /Amazon Comprehend Developer Guide/.
module Amazonka.Comprehend.CreateFlywheel
  ( -- * Creating a Request
    CreateFlywheel (..),
    newCreateFlywheel,

    -- * Request Lenses
    createFlywheel_activeModelArn,
    createFlywheel_clientRequestToken,
    createFlywheel_dataSecurityConfig,
    createFlywheel_modelType,
    createFlywheel_tags,
    createFlywheel_taskConfig,
    createFlywheel_flywheelName,
    createFlywheel_dataAccessRoleArn,
    createFlywheel_dataLakeS3Uri,

    -- * Destructuring the Response
    CreateFlywheelResponse (..),
    newCreateFlywheelResponse,

    -- * Response Lenses
    createFlywheelResponse_activeModelArn,
    createFlywheelResponse_flywheelArn,
    createFlywheelResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFlywheel' smart constructor.
data CreateFlywheel = CreateFlywheel'
  { -- | To associate an existing model with the flywheel, specify the Amazon
    -- Resource Number (ARN) of the model version.
    activeModelArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the request. If you don\'t set the client
    -- request token, Amazon Comprehend generates one.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Data security configurations.
    dataSecurityConfig :: Prelude.Maybe DataSecurityConfig,
    -- | The model type.
    modelType :: Prelude.Maybe ModelType,
    -- | The tags to associate with this flywheel.
    tags :: Prelude.Maybe [Tag],
    -- | Configuration about the custom classifier associated with the flywheel.
    taskConfig :: Prelude.Maybe TaskConfig,
    -- | Name for the flywheel.
    flywheelName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that grants Amazon
    -- Comprehend the permissions required to access the flywheel data in the
    -- data lake.
    dataAccessRoleArn :: Prelude.Text,
    -- | Enter the S3 location for the data lake. You can specify a new S3 bucket
    -- or a new folder of an existing S3 bucket. The flywheel creates the data
    -- lake at this location.
    dataLakeS3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFlywheel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeModelArn', 'createFlywheel_activeModelArn' - To associate an existing model with the flywheel, specify the Amazon
-- Resource Number (ARN) of the model version.
--
-- 'clientRequestToken', 'createFlywheel_clientRequestToken' - A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
--
-- 'dataSecurityConfig', 'createFlywheel_dataSecurityConfig' - Data security configurations.
--
-- 'modelType', 'createFlywheel_modelType' - The model type.
--
-- 'tags', 'createFlywheel_tags' - The tags to associate with this flywheel.
--
-- 'taskConfig', 'createFlywheel_taskConfig' - Configuration about the custom classifier associated with the flywheel.
--
-- 'flywheelName', 'createFlywheel_flywheelName' - Name for the flywheel.
--
-- 'dataAccessRoleArn', 'createFlywheel_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the IAM role that grants Amazon
-- Comprehend the permissions required to access the flywheel data in the
-- data lake.
--
-- 'dataLakeS3Uri', 'createFlywheel_dataLakeS3Uri' - Enter the S3 location for the data lake. You can specify a new S3 bucket
-- or a new folder of an existing S3 bucket. The flywheel creates the data
-- lake at this location.
newCreateFlywheel ::
  -- | 'flywheelName'
  Prelude.Text ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  -- | 'dataLakeS3Uri'
  Prelude.Text ->
  CreateFlywheel
newCreateFlywheel
  pFlywheelName_
  pDataAccessRoleArn_
  pDataLakeS3Uri_ =
    CreateFlywheel'
      { activeModelArn = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        dataSecurityConfig = Prelude.Nothing,
        modelType = Prelude.Nothing,
        tags = Prelude.Nothing,
        taskConfig = Prelude.Nothing,
        flywheelName = pFlywheelName_,
        dataAccessRoleArn = pDataAccessRoleArn_,
        dataLakeS3Uri = pDataLakeS3Uri_
      }

-- | To associate an existing model with the flywheel, specify the Amazon
-- Resource Number (ARN) of the model version.
createFlywheel_activeModelArn :: Lens.Lens' CreateFlywheel (Prelude.Maybe Prelude.Text)
createFlywheel_activeModelArn = Lens.lens (\CreateFlywheel' {activeModelArn} -> activeModelArn) (\s@CreateFlywheel' {} a -> s {activeModelArn = a} :: CreateFlywheel)

-- | A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
createFlywheel_clientRequestToken :: Lens.Lens' CreateFlywheel (Prelude.Maybe Prelude.Text)
createFlywheel_clientRequestToken = Lens.lens (\CreateFlywheel' {clientRequestToken} -> clientRequestToken) (\s@CreateFlywheel' {} a -> s {clientRequestToken = a} :: CreateFlywheel)

-- | Data security configurations.
createFlywheel_dataSecurityConfig :: Lens.Lens' CreateFlywheel (Prelude.Maybe DataSecurityConfig)
createFlywheel_dataSecurityConfig = Lens.lens (\CreateFlywheel' {dataSecurityConfig} -> dataSecurityConfig) (\s@CreateFlywheel' {} a -> s {dataSecurityConfig = a} :: CreateFlywheel)

-- | The model type.
createFlywheel_modelType :: Lens.Lens' CreateFlywheel (Prelude.Maybe ModelType)
createFlywheel_modelType = Lens.lens (\CreateFlywheel' {modelType} -> modelType) (\s@CreateFlywheel' {} a -> s {modelType = a} :: CreateFlywheel)

-- | The tags to associate with this flywheel.
createFlywheel_tags :: Lens.Lens' CreateFlywheel (Prelude.Maybe [Tag])
createFlywheel_tags = Lens.lens (\CreateFlywheel' {tags} -> tags) (\s@CreateFlywheel' {} a -> s {tags = a} :: CreateFlywheel) Prelude.. Lens.mapping Lens.coerced

-- | Configuration about the custom classifier associated with the flywheel.
createFlywheel_taskConfig :: Lens.Lens' CreateFlywheel (Prelude.Maybe TaskConfig)
createFlywheel_taskConfig = Lens.lens (\CreateFlywheel' {taskConfig} -> taskConfig) (\s@CreateFlywheel' {} a -> s {taskConfig = a} :: CreateFlywheel)

-- | Name for the flywheel.
createFlywheel_flywheelName :: Lens.Lens' CreateFlywheel Prelude.Text
createFlywheel_flywheelName = Lens.lens (\CreateFlywheel' {flywheelName} -> flywheelName) (\s@CreateFlywheel' {} a -> s {flywheelName = a} :: CreateFlywheel)

-- | The Amazon Resource Name (ARN) of the IAM role that grants Amazon
-- Comprehend the permissions required to access the flywheel data in the
-- data lake.
createFlywheel_dataAccessRoleArn :: Lens.Lens' CreateFlywheel Prelude.Text
createFlywheel_dataAccessRoleArn = Lens.lens (\CreateFlywheel' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@CreateFlywheel' {} a -> s {dataAccessRoleArn = a} :: CreateFlywheel)

-- | Enter the S3 location for the data lake. You can specify a new S3 bucket
-- or a new folder of an existing S3 bucket. The flywheel creates the data
-- lake at this location.
createFlywheel_dataLakeS3Uri :: Lens.Lens' CreateFlywheel Prelude.Text
createFlywheel_dataLakeS3Uri = Lens.lens (\CreateFlywheel' {dataLakeS3Uri} -> dataLakeS3Uri) (\s@CreateFlywheel' {} a -> s {dataLakeS3Uri = a} :: CreateFlywheel)

instance Core.AWSRequest CreateFlywheel where
  type
    AWSResponse CreateFlywheel =
      CreateFlywheelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFlywheelResponse'
            Prelude.<$> (x Data..?> "ActiveModelArn")
            Prelude.<*> (x Data..?> "FlywheelArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFlywheel where
  hashWithSalt _salt CreateFlywheel' {..} =
    _salt
      `Prelude.hashWithSalt` activeModelArn
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` dataSecurityConfig
      `Prelude.hashWithSalt` modelType
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` taskConfig
      `Prelude.hashWithSalt` flywheelName
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` dataLakeS3Uri

instance Prelude.NFData CreateFlywheel where
  rnf CreateFlywheel' {..} =
    Prelude.rnf activeModelArn
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf dataSecurityConfig
      `Prelude.seq` Prelude.rnf modelType
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf taskConfig
      `Prelude.seq` Prelude.rnf flywheelName
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf dataLakeS3Uri

instance Data.ToHeaders CreateFlywheel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.CreateFlywheel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFlywheel where
  toJSON CreateFlywheel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ActiveModelArn" Data..=)
              Prelude.<$> activeModelArn,
            ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("DataSecurityConfig" Data..=)
              Prelude.<$> dataSecurityConfig,
            ("ModelType" Data..=) Prelude.<$> modelType,
            ("Tags" Data..=) Prelude.<$> tags,
            ("TaskConfig" Data..=) Prelude.<$> taskConfig,
            Prelude.Just ("FlywheelName" Data..= flywheelName),
            Prelude.Just
              ("DataAccessRoleArn" Data..= dataAccessRoleArn),
            Prelude.Just
              ("DataLakeS3Uri" Data..= dataLakeS3Uri)
          ]
      )

instance Data.ToPath CreateFlywheel where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateFlywheel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFlywheelResponse' smart constructor.
data CreateFlywheelResponse = CreateFlywheelResponse'
  { -- | The Amazon Resource Number (ARN) of the active model version.
    activeModelArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Number (ARN) of the flywheel.
    flywheelArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFlywheelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeModelArn', 'createFlywheelResponse_activeModelArn' - The Amazon Resource Number (ARN) of the active model version.
--
-- 'flywheelArn', 'createFlywheelResponse_flywheelArn' - The Amazon Resource Number (ARN) of the flywheel.
--
-- 'httpStatus', 'createFlywheelResponse_httpStatus' - The response's http status code.
newCreateFlywheelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFlywheelResponse
newCreateFlywheelResponse pHttpStatus_ =
  CreateFlywheelResponse'
    { activeModelArn =
        Prelude.Nothing,
      flywheelArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Number (ARN) of the active model version.
createFlywheelResponse_activeModelArn :: Lens.Lens' CreateFlywheelResponse (Prelude.Maybe Prelude.Text)
createFlywheelResponse_activeModelArn = Lens.lens (\CreateFlywheelResponse' {activeModelArn} -> activeModelArn) (\s@CreateFlywheelResponse' {} a -> s {activeModelArn = a} :: CreateFlywheelResponse)

-- | The Amazon Resource Number (ARN) of the flywheel.
createFlywheelResponse_flywheelArn :: Lens.Lens' CreateFlywheelResponse (Prelude.Maybe Prelude.Text)
createFlywheelResponse_flywheelArn = Lens.lens (\CreateFlywheelResponse' {flywheelArn} -> flywheelArn) (\s@CreateFlywheelResponse' {} a -> s {flywheelArn = a} :: CreateFlywheelResponse)

-- | The response's http status code.
createFlywheelResponse_httpStatus :: Lens.Lens' CreateFlywheelResponse Prelude.Int
createFlywheelResponse_httpStatus = Lens.lens (\CreateFlywheelResponse' {httpStatus} -> httpStatus) (\s@CreateFlywheelResponse' {} a -> s {httpStatus = a} :: CreateFlywheelResponse)

instance Prelude.NFData CreateFlywheelResponse where
  rnf CreateFlywheelResponse' {..} =
    Prelude.rnf activeModelArn
      `Prelude.seq` Prelude.rnf flywheelArn
      `Prelude.seq` Prelude.rnf httpStatus
