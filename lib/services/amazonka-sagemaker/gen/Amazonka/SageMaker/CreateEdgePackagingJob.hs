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
-- Module      : Amazonka.SageMaker.CreateEdgePackagingJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a SageMaker Edge Manager model packaging job. Edge Manager will
-- use the model artifacts from the Amazon Simple Storage Service bucket
-- that you specify. After the model has been packaged, Amazon SageMaker
-- saves the resulting artifacts to an S3 bucket that you specify.
module Amazonka.SageMaker.CreateEdgePackagingJob
  ( -- * Creating a Request
    CreateEdgePackagingJob (..),
    newCreateEdgePackagingJob,

    -- * Request Lenses
    createEdgePackagingJob_tags,
    createEdgePackagingJob_resourceKey,
    createEdgePackagingJob_edgePackagingJobName,
    createEdgePackagingJob_compilationJobName,
    createEdgePackagingJob_modelName,
    createEdgePackagingJob_modelVersion,
    createEdgePackagingJob_roleArn,
    createEdgePackagingJob_outputConfig,

    -- * Destructuring the Response
    CreateEdgePackagingJobResponse (..),
    newCreateEdgePackagingJobResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateEdgePackagingJob' smart constructor.
data CreateEdgePackagingJob = CreateEdgePackagingJob'
  { -- | Creates tags for the packaging job.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon Web Services KMS key to use when encrypting the EBS volume
    -- the edge packaging job runs on.
    resourceKey :: Prelude.Maybe Prelude.Text,
    -- | The name of the edge packaging job.
    edgePackagingJobName :: Prelude.Text,
    -- | The name of the SageMaker Neo compilation job that will be used to
    -- locate model artifacts for packaging.
    compilationJobName :: Prelude.Text,
    -- | The name of the model.
    modelName :: Prelude.Text,
    -- | The version of the model.
    modelVersion :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an IAM role that enables Amazon
    -- SageMaker to download and upload the model, and to contact SageMaker
    -- Neo.
    roleArn :: Prelude.Text,
    -- | Provides information about the output location for the packaged model.
    outputConfig :: EdgeOutputConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEdgePackagingJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createEdgePackagingJob_tags' - Creates tags for the packaging job.
--
-- 'resourceKey', 'createEdgePackagingJob_resourceKey' - The Amazon Web Services KMS key to use when encrypting the EBS volume
-- the edge packaging job runs on.
--
-- 'edgePackagingJobName', 'createEdgePackagingJob_edgePackagingJobName' - The name of the edge packaging job.
--
-- 'compilationJobName', 'createEdgePackagingJob_compilationJobName' - The name of the SageMaker Neo compilation job that will be used to
-- locate model artifacts for packaging.
--
-- 'modelName', 'createEdgePackagingJob_modelName' - The name of the model.
--
-- 'modelVersion', 'createEdgePackagingJob_modelVersion' - The version of the model.
--
-- 'roleArn', 'createEdgePackagingJob_roleArn' - The Amazon Resource Name (ARN) of an IAM role that enables Amazon
-- SageMaker to download and upload the model, and to contact SageMaker
-- Neo.
--
-- 'outputConfig', 'createEdgePackagingJob_outputConfig' - Provides information about the output location for the packaged model.
newCreateEdgePackagingJob ::
  -- | 'edgePackagingJobName'
  Prelude.Text ->
  -- | 'compilationJobName'
  Prelude.Text ->
  -- | 'modelName'
  Prelude.Text ->
  -- | 'modelVersion'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'outputConfig'
  EdgeOutputConfig ->
  CreateEdgePackagingJob
newCreateEdgePackagingJob
  pEdgePackagingJobName_
  pCompilationJobName_
  pModelName_
  pModelVersion_
  pRoleArn_
  pOutputConfig_ =
    CreateEdgePackagingJob'
      { tags = Prelude.Nothing,
        resourceKey = Prelude.Nothing,
        edgePackagingJobName = pEdgePackagingJobName_,
        compilationJobName = pCompilationJobName_,
        modelName = pModelName_,
        modelVersion = pModelVersion_,
        roleArn = pRoleArn_,
        outputConfig = pOutputConfig_
      }

-- | Creates tags for the packaging job.
createEdgePackagingJob_tags :: Lens.Lens' CreateEdgePackagingJob (Prelude.Maybe [Tag])
createEdgePackagingJob_tags = Lens.lens (\CreateEdgePackagingJob' {tags} -> tags) (\s@CreateEdgePackagingJob' {} a -> s {tags = a} :: CreateEdgePackagingJob) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services KMS key to use when encrypting the EBS volume
-- the edge packaging job runs on.
createEdgePackagingJob_resourceKey :: Lens.Lens' CreateEdgePackagingJob (Prelude.Maybe Prelude.Text)
createEdgePackagingJob_resourceKey = Lens.lens (\CreateEdgePackagingJob' {resourceKey} -> resourceKey) (\s@CreateEdgePackagingJob' {} a -> s {resourceKey = a} :: CreateEdgePackagingJob)

-- | The name of the edge packaging job.
createEdgePackagingJob_edgePackagingJobName :: Lens.Lens' CreateEdgePackagingJob Prelude.Text
createEdgePackagingJob_edgePackagingJobName = Lens.lens (\CreateEdgePackagingJob' {edgePackagingJobName} -> edgePackagingJobName) (\s@CreateEdgePackagingJob' {} a -> s {edgePackagingJobName = a} :: CreateEdgePackagingJob)

-- | The name of the SageMaker Neo compilation job that will be used to
-- locate model artifacts for packaging.
createEdgePackagingJob_compilationJobName :: Lens.Lens' CreateEdgePackagingJob Prelude.Text
createEdgePackagingJob_compilationJobName = Lens.lens (\CreateEdgePackagingJob' {compilationJobName} -> compilationJobName) (\s@CreateEdgePackagingJob' {} a -> s {compilationJobName = a} :: CreateEdgePackagingJob)

-- | The name of the model.
createEdgePackagingJob_modelName :: Lens.Lens' CreateEdgePackagingJob Prelude.Text
createEdgePackagingJob_modelName = Lens.lens (\CreateEdgePackagingJob' {modelName} -> modelName) (\s@CreateEdgePackagingJob' {} a -> s {modelName = a} :: CreateEdgePackagingJob)

-- | The version of the model.
createEdgePackagingJob_modelVersion :: Lens.Lens' CreateEdgePackagingJob Prelude.Text
createEdgePackagingJob_modelVersion = Lens.lens (\CreateEdgePackagingJob' {modelVersion} -> modelVersion) (\s@CreateEdgePackagingJob' {} a -> s {modelVersion = a} :: CreateEdgePackagingJob)

-- | The Amazon Resource Name (ARN) of an IAM role that enables Amazon
-- SageMaker to download and upload the model, and to contact SageMaker
-- Neo.
createEdgePackagingJob_roleArn :: Lens.Lens' CreateEdgePackagingJob Prelude.Text
createEdgePackagingJob_roleArn = Lens.lens (\CreateEdgePackagingJob' {roleArn} -> roleArn) (\s@CreateEdgePackagingJob' {} a -> s {roleArn = a} :: CreateEdgePackagingJob)

-- | Provides information about the output location for the packaged model.
createEdgePackagingJob_outputConfig :: Lens.Lens' CreateEdgePackagingJob EdgeOutputConfig
createEdgePackagingJob_outputConfig = Lens.lens (\CreateEdgePackagingJob' {outputConfig} -> outputConfig) (\s@CreateEdgePackagingJob' {} a -> s {outputConfig = a} :: CreateEdgePackagingJob)

instance Core.AWSRequest CreateEdgePackagingJob where
  type
    AWSResponse CreateEdgePackagingJob =
      CreateEdgePackagingJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      CreateEdgePackagingJobResponse'

instance Prelude.Hashable CreateEdgePackagingJob where
  hashWithSalt _salt CreateEdgePackagingJob' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` resourceKey
      `Prelude.hashWithSalt` edgePackagingJobName
      `Prelude.hashWithSalt` compilationJobName
      `Prelude.hashWithSalt` modelName
      `Prelude.hashWithSalt` modelVersion
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` outputConfig

instance Prelude.NFData CreateEdgePackagingJob where
  rnf CreateEdgePackagingJob' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf resourceKey
      `Prelude.seq` Prelude.rnf edgePackagingJobName
      `Prelude.seq` Prelude.rnf compilationJobName
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf modelVersion
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf outputConfig

instance Data.ToHeaders CreateEdgePackagingJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.CreateEdgePackagingJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEdgePackagingJob where
  toJSON CreateEdgePackagingJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("ResourceKey" Data..=) Prelude.<$> resourceKey,
            Prelude.Just
              ( "EdgePackagingJobName"
                  Data..= edgePackagingJobName
              ),
            Prelude.Just
              ("CompilationJobName" Data..= compilationJobName),
            Prelude.Just ("ModelName" Data..= modelName),
            Prelude.Just ("ModelVersion" Data..= modelVersion),
            Prelude.Just ("RoleArn" Data..= roleArn),
            Prelude.Just ("OutputConfig" Data..= outputConfig)
          ]
      )

instance Data.ToPath CreateEdgePackagingJob where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateEdgePackagingJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEdgePackagingJobResponse' smart constructor.
data CreateEdgePackagingJobResponse = CreateEdgePackagingJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEdgePackagingJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateEdgePackagingJobResponse ::
  CreateEdgePackagingJobResponse
newCreateEdgePackagingJobResponse =
  CreateEdgePackagingJobResponse'

instance
  Prelude.NFData
    CreateEdgePackagingJobResponse
  where
  rnf _ = ()
