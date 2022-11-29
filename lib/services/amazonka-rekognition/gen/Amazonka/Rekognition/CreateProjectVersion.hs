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
-- Module      : Amazonka.Rekognition.CreateProjectVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of a model and begins training. Models are managed
-- as part of an Amazon Rekognition Custom Labels project. The response
-- from @CreateProjectVersion@ is an Amazon Resource Name (ARN) for the
-- version of the model.
--
-- Training uses the training and test datasets associated with the
-- project. For more information, see Creating training and test dataset in
-- the /Amazon Rekognition Custom Labels Developer Guide/.
--
-- You can train a model in a project that doesn\'t have associated
-- datasets by specifying manifest files in the @TrainingData@ and
-- @TestingData@ fields.
--
-- If you open the console after training a model with manifest files,
-- Amazon Rekognition Custom Labels creates the datasets for you using the
-- most recent manifest files. You can no longer train a model version for
-- the project by specifying manifest files.
--
-- Instead of training with a project without associated datasets, we
-- recommend that you use the manifest files to create training and test
-- datasets for the project.
--
-- Training takes a while to complete. You can get the current status by
-- calling DescribeProjectVersions. Training completed successfully if the
-- value of the @Status@ field is @TRAINING_COMPLETED@.
--
-- If training fails, see Debugging a failed model training in the /Amazon
-- Rekognition Custom Labels/ developer guide.
--
-- Once training has successfully completed, call DescribeProjectVersions
-- to get the training results and evaluate the model. For more
-- information, see Improving a trained Amazon Rekognition Custom Labels
-- model in the /Amazon Rekognition Custom Labels/ developers guide.
--
-- After evaluating the model, you start the model by calling
-- StartProjectVersion.
--
-- This operation requires permissions to perform the
-- @rekognition:CreateProjectVersion@ action.
module Amazonka.Rekognition.CreateProjectVersion
  ( -- * Creating a Request
    CreateProjectVersion (..),
    newCreateProjectVersion,

    -- * Request Lenses
    createProjectVersion_tags,
    createProjectVersion_testingData,
    createProjectVersion_kmsKeyId,
    createProjectVersion_trainingData,
    createProjectVersion_projectArn,
    createProjectVersion_versionName,
    createProjectVersion_outputConfig,

    -- * Destructuring the Response
    CreateProjectVersionResponse (..),
    newCreateProjectVersionResponse,

    -- * Response Lenses
    createProjectVersionResponse_projectVersionArn,
    createProjectVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateProjectVersion' smart constructor.
data CreateProjectVersion = CreateProjectVersion'
  { -- | A set of tags (key-value pairs) that you want to attach to the model.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies an external manifest that the service uses to test the model.
    -- If you specify @TestingData@ you must also specify @TrainingData@. The
    -- project must not have any associated datasets.
    testingData :: Prelude.Maybe TestingData,
    -- | The identifier for your AWS Key Management Service key (AWS KMS key).
    -- You can supply the Amazon Resource Name (ARN) of your KMS key, the ID of
    -- your KMS key, an alias for your KMS key, or an alias ARN. The key is
    -- used to encrypt training and test images copied into the service for
    -- model training. Your source images are unaffected. The key is also used
    -- to encrypt training results and manifest files written to the output
    -- Amazon S3 bucket (@OutputConfig@).
    --
    -- If you choose to use your own KMS key, you need the following
    -- permissions on the KMS key.
    --
    -- -   kms:CreateGrant
    --
    -- -   kms:DescribeKey
    --
    -- -   kms:GenerateDataKey
    --
    -- -   kms:Decrypt
    --
    -- If you don\'t specify a value for @KmsKeyId@, images copied into the
    -- service are encrypted using a key that AWS owns and manages.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies an external manifest that the services uses to train the
    -- model. If you specify @TrainingData@ you must also specify
    -- @TestingData@. The project must not have any associated datasets.
    trainingData :: Prelude.Maybe TrainingData,
    -- | The ARN of the Amazon Rekognition Custom Labels project that manages the
    -- model that you want to train.
    projectArn :: Prelude.Text,
    -- | A name for the version of the model. This value must be unique.
    versionName :: Prelude.Text,
    -- | The Amazon S3 bucket location to store the results of training. The S3
    -- bucket can be in any AWS account as long as the caller has
    -- @s3:PutObject@ permissions on the S3 bucket.
    outputConfig :: OutputConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProjectVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createProjectVersion_tags' - A set of tags (key-value pairs) that you want to attach to the model.
--
-- 'testingData', 'createProjectVersion_testingData' - Specifies an external manifest that the service uses to test the model.
-- If you specify @TestingData@ you must also specify @TrainingData@. The
-- project must not have any associated datasets.
--
-- 'kmsKeyId', 'createProjectVersion_kmsKeyId' - The identifier for your AWS Key Management Service key (AWS KMS key).
-- You can supply the Amazon Resource Name (ARN) of your KMS key, the ID of
-- your KMS key, an alias for your KMS key, or an alias ARN. The key is
-- used to encrypt training and test images copied into the service for
-- model training. Your source images are unaffected. The key is also used
-- to encrypt training results and manifest files written to the output
-- Amazon S3 bucket (@OutputConfig@).
--
-- If you choose to use your own KMS key, you need the following
-- permissions on the KMS key.
--
-- -   kms:CreateGrant
--
-- -   kms:DescribeKey
--
-- -   kms:GenerateDataKey
--
-- -   kms:Decrypt
--
-- If you don\'t specify a value for @KmsKeyId@, images copied into the
-- service are encrypted using a key that AWS owns and manages.
--
-- 'trainingData', 'createProjectVersion_trainingData' - Specifies an external manifest that the services uses to train the
-- model. If you specify @TrainingData@ you must also specify
-- @TestingData@. The project must not have any associated datasets.
--
-- 'projectArn', 'createProjectVersion_projectArn' - The ARN of the Amazon Rekognition Custom Labels project that manages the
-- model that you want to train.
--
-- 'versionName', 'createProjectVersion_versionName' - A name for the version of the model. This value must be unique.
--
-- 'outputConfig', 'createProjectVersion_outputConfig' - The Amazon S3 bucket location to store the results of training. The S3
-- bucket can be in any AWS account as long as the caller has
-- @s3:PutObject@ permissions on the S3 bucket.
newCreateProjectVersion ::
  -- | 'projectArn'
  Prelude.Text ->
  -- | 'versionName'
  Prelude.Text ->
  -- | 'outputConfig'
  OutputConfig ->
  CreateProjectVersion
newCreateProjectVersion
  pProjectArn_
  pVersionName_
  pOutputConfig_ =
    CreateProjectVersion'
      { tags = Prelude.Nothing,
        testingData = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        trainingData = Prelude.Nothing,
        projectArn = pProjectArn_,
        versionName = pVersionName_,
        outputConfig = pOutputConfig_
      }

-- | A set of tags (key-value pairs) that you want to attach to the model.
createProjectVersion_tags :: Lens.Lens' CreateProjectVersion (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createProjectVersion_tags = Lens.lens (\CreateProjectVersion' {tags} -> tags) (\s@CreateProjectVersion' {} a -> s {tags = a} :: CreateProjectVersion) Prelude.. Lens.mapping Lens.coerced

-- | Specifies an external manifest that the service uses to test the model.
-- If you specify @TestingData@ you must also specify @TrainingData@. The
-- project must not have any associated datasets.
createProjectVersion_testingData :: Lens.Lens' CreateProjectVersion (Prelude.Maybe TestingData)
createProjectVersion_testingData = Lens.lens (\CreateProjectVersion' {testingData} -> testingData) (\s@CreateProjectVersion' {} a -> s {testingData = a} :: CreateProjectVersion)

-- | The identifier for your AWS Key Management Service key (AWS KMS key).
-- You can supply the Amazon Resource Name (ARN) of your KMS key, the ID of
-- your KMS key, an alias for your KMS key, or an alias ARN. The key is
-- used to encrypt training and test images copied into the service for
-- model training. Your source images are unaffected. The key is also used
-- to encrypt training results and manifest files written to the output
-- Amazon S3 bucket (@OutputConfig@).
--
-- If you choose to use your own KMS key, you need the following
-- permissions on the KMS key.
--
-- -   kms:CreateGrant
--
-- -   kms:DescribeKey
--
-- -   kms:GenerateDataKey
--
-- -   kms:Decrypt
--
-- If you don\'t specify a value for @KmsKeyId@, images copied into the
-- service are encrypted using a key that AWS owns and manages.
createProjectVersion_kmsKeyId :: Lens.Lens' CreateProjectVersion (Prelude.Maybe Prelude.Text)
createProjectVersion_kmsKeyId = Lens.lens (\CreateProjectVersion' {kmsKeyId} -> kmsKeyId) (\s@CreateProjectVersion' {} a -> s {kmsKeyId = a} :: CreateProjectVersion)

-- | Specifies an external manifest that the services uses to train the
-- model. If you specify @TrainingData@ you must also specify
-- @TestingData@. The project must not have any associated datasets.
createProjectVersion_trainingData :: Lens.Lens' CreateProjectVersion (Prelude.Maybe TrainingData)
createProjectVersion_trainingData = Lens.lens (\CreateProjectVersion' {trainingData} -> trainingData) (\s@CreateProjectVersion' {} a -> s {trainingData = a} :: CreateProjectVersion)

-- | The ARN of the Amazon Rekognition Custom Labels project that manages the
-- model that you want to train.
createProjectVersion_projectArn :: Lens.Lens' CreateProjectVersion Prelude.Text
createProjectVersion_projectArn = Lens.lens (\CreateProjectVersion' {projectArn} -> projectArn) (\s@CreateProjectVersion' {} a -> s {projectArn = a} :: CreateProjectVersion)

-- | A name for the version of the model. This value must be unique.
createProjectVersion_versionName :: Lens.Lens' CreateProjectVersion Prelude.Text
createProjectVersion_versionName = Lens.lens (\CreateProjectVersion' {versionName} -> versionName) (\s@CreateProjectVersion' {} a -> s {versionName = a} :: CreateProjectVersion)

-- | The Amazon S3 bucket location to store the results of training. The S3
-- bucket can be in any AWS account as long as the caller has
-- @s3:PutObject@ permissions on the S3 bucket.
createProjectVersion_outputConfig :: Lens.Lens' CreateProjectVersion OutputConfig
createProjectVersion_outputConfig = Lens.lens (\CreateProjectVersion' {outputConfig} -> outputConfig) (\s@CreateProjectVersion' {} a -> s {outputConfig = a} :: CreateProjectVersion)

instance Core.AWSRequest CreateProjectVersion where
  type
    AWSResponse CreateProjectVersion =
      CreateProjectVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProjectVersionResponse'
            Prelude.<$> (x Core..?> "ProjectVersionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProjectVersion where
  hashWithSalt _salt CreateProjectVersion' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` testingData
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` trainingData
      `Prelude.hashWithSalt` projectArn
      `Prelude.hashWithSalt` versionName
      `Prelude.hashWithSalt` outputConfig

instance Prelude.NFData CreateProjectVersion where
  rnf CreateProjectVersion' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf testingData
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf trainingData
      `Prelude.seq` Prelude.rnf projectArn
      `Prelude.seq` Prelude.rnf versionName
      `Prelude.seq` Prelude.rnf outputConfig

instance Core.ToHeaders CreateProjectVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.CreateProjectVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateProjectVersion where
  toJSON CreateProjectVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("TestingData" Core..=) Prelude.<$> testingData,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("TrainingData" Core..=) Prelude.<$> trainingData,
            Prelude.Just ("ProjectArn" Core..= projectArn),
            Prelude.Just ("VersionName" Core..= versionName),
            Prelude.Just ("OutputConfig" Core..= outputConfig)
          ]
      )

instance Core.ToPath CreateProjectVersion where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateProjectVersion where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateProjectVersionResponse where
  rnf CreateProjectVersionResponse' {..} =
    Prelude.rnf projectVersionArn
      `Prelude.seq` Prelude.rnf httpStatus
