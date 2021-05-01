{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Rekognition.CreateProjectVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of a model and begins training. Models are managed
-- as part of an Amazon Rekognition Custom Labels project. You can specify
-- one training dataset and one testing dataset. The response from
-- @CreateProjectVersion@ is an Amazon Resource Name (ARN) for the version
-- of the model.
--
-- Training takes a while to complete. You can get the current status by
-- calling DescribeProjectVersions.
--
-- Once training has successfully completed, call DescribeProjectVersions
-- to get the training results and evaluate the model.
--
-- After evaluating the model, you start the model by calling
-- StartProjectVersion.
--
-- This operation requires permissions to perform the
-- @rekognition:CreateProjectVersion@ action.
module Network.AWS.Rekognition.CreateProjectVersion
  ( -- * Creating a Request
    CreateProjectVersion (..),
    newCreateProjectVersion,

    -- * Request Lenses
    createProjectVersion_projectArn,
    createProjectVersion_versionName,
    createProjectVersion_outputConfig,
    createProjectVersion_trainingData,
    createProjectVersion_testingData,

    -- * Destructuring the Response
    CreateProjectVersionResponse (..),
    newCreateProjectVersionResponse,

    -- * Response Lenses
    createProjectVersionResponse_projectVersionArn,
    createProjectVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateProjectVersion' smart constructor.
data CreateProjectVersion = CreateProjectVersion'
  { -- | The ARN of the Amazon Rekognition Custom Labels project that manages the
    -- model that you want to train.
    projectArn :: Prelude.Text,
    -- | A name for the version of the model. This value must be unique.
    versionName :: Prelude.Text,
    -- | The Amazon S3 location to store the results of training.
    outputConfig :: OutputConfig,
    -- | The dataset to use for training.
    trainingData :: TrainingData,
    -- | The dataset to use for testing.
    testingData :: TestingData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateProjectVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectArn', 'createProjectVersion_projectArn' - The ARN of the Amazon Rekognition Custom Labels project that manages the
-- model that you want to train.
--
-- 'versionName', 'createProjectVersion_versionName' - A name for the version of the model. This value must be unique.
--
-- 'outputConfig', 'createProjectVersion_outputConfig' - The Amazon S3 location to store the results of training.
--
-- 'trainingData', 'createProjectVersion_trainingData' - The dataset to use for training.
--
-- 'testingData', 'createProjectVersion_testingData' - The dataset to use for testing.
newCreateProjectVersion ::
  -- | 'projectArn'
  Prelude.Text ->
  -- | 'versionName'
  Prelude.Text ->
  -- | 'outputConfig'
  OutputConfig ->
  -- | 'trainingData'
  TrainingData ->
  -- | 'testingData'
  TestingData ->
  CreateProjectVersion
newCreateProjectVersion
  pProjectArn_
  pVersionName_
  pOutputConfig_
  pTrainingData_
  pTestingData_ =
    CreateProjectVersion'
      { projectArn = pProjectArn_,
        versionName = pVersionName_,
        outputConfig = pOutputConfig_,
        trainingData = pTrainingData_,
        testingData = pTestingData_
      }

-- | The ARN of the Amazon Rekognition Custom Labels project that manages the
-- model that you want to train.
createProjectVersion_projectArn :: Lens.Lens' CreateProjectVersion Prelude.Text
createProjectVersion_projectArn = Lens.lens (\CreateProjectVersion' {projectArn} -> projectArn) (\s@CreateProjectVersion' {} a -> s {projectArn = a} :: CreateProjectVersion)

-- | A name for the version of the model. This value must be unique.
createProjectVersion_versionName :: Lens.Lens' CreateProjectVersion Prelude.Text
createProjectVersion_versionName = Lens.lens (\CreateProjectVersion' {versionName} -> versionName) (\s@CreateProjectVersion' {} a -> s {versionName = a} :: CreateProjectVersion)

-- | The Amazon S3 location to store the results of training.
createProjectVersion_outputConfig :: Lens.Lens' CreateProjectVersion OutputConfig
createProjectVersion_outputConfig = Lens.lens (\CreateProjectVersion' {outputConfig} -> outputConfig) (\s@CreateProjectVersion' {} a -> s {outputConfig = a} :: CreateProjectVersion)

-- | The dataset to use for training.
createProjectVersion_trainingData :: Lens.Lens' CreateProjectVersion TrainingData
createProjectVersion_trainingData = Lens.lens (\CreateProjectVersion' {trainingData} -> trainingData) (\s@CreateProjectVersion' {} a -> s {trainingData = a} :: CreateProjectVersion)

-- | The dataset to use for testing.
createProjectVersion_testingData :: Lens.Lens' CreateProjectVersion TestingData
createProjectVersion_testingData = Lens.lens (\CreateProjectVersion' {testingData} -> testingData) (\s@CreateProjectVersion' {} a -> s {testingData = a} :: CreateProjectVersion)

instance Prelude.AWSRequest CreateProjectVersion where
  type
    Rs CreateProjectVersion =
      CreateProjectVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProjectVersionResponse'
            Prelude.<$> (x Prelude..?> "ProjectVersionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProjectVersion

instance Prelude.NFData CreateProjectVersion

instance Prelude.ToHeaders CreateProjectVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "RekognitionService.CreateProjectVersion" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateProjectVersion where
  toJSON CreateProjectVersion' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ProjectArn" Prelude..= projectArn),
            Prelude.Just ("VersionName" Prelude..= versionName),
            Prelude.Just
              ("OutputConfig" Prelude..= outputConfig),
            Prelude.Just
              ("TrainingData" Prelude..= trainingData),
            Prelude.Just ("TestingData" Prelude..= testingData)
          ]
      )

instance Prelude.ToPath CreateProjectVersion where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateProjectVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProjectVersionResponse' smart constructor.
data CreateProjectVersionResponse = CreateProjectVersionResponse'
  { -- | The ARN of the model version that was created. Use
    -- @DescribeProjectVersion@ to get the current status of the training
    -- operation.
    projectVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateProjectVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectVersionArn', 'createProjectVersionResponse_projectVersionArn' - The ARN of the model version that was created. Use
-- @DescribeProjectVersion@ to get the current status of the training
-- operation.
--
-- 'httpStatus', 'createProjectVersionResponse_httpStatus' - The response's http status code.
newCreateProjectVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProjectVersionResponse
newCreateProjectVersionResponse pHttpStatus_ =
  CreateProjectVersionResponse'
    { projectVersionArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the model version that was created. Use
-- @DescribeProjectVersion@ to get the current status of the training
-- operation.
createProjectVersionResponse_projectVersionArn :: Lens.Lens' CreateProjectVersionResponse (Prelude.Maybe Prelude.Text)
createProjectVersionResponse_projectVersionArn = Lens.lens (\CreateProjectVersionResponse' {projectVersionArn} -> projectVersionArn) (\s@CreateProjectVersionResponse' {} a -> s {projectVersionArn = a} :: CreateProjectVersionResponse)

-- | The response's http status code.
createProjectVersionResponse_httpStatus :: Lens.Lens' CreateProjectVersionResponse Prelude.Int
createProjectVersionResponse_httpStatus = Lens.lens (\CreateProjectVersionResponse' {httpStatus} -> httpStatus) (\s@CreateProjectVersionResponse' {} a -> s {httpStatus = a} :: CreateProjectVersionResponse)

instance Prelude.NFData CreateProjectVersionResponse
