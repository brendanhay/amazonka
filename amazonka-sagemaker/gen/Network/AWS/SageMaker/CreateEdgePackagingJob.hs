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
-- Module      : Network.AWS.SageMaker.CreateEdgePackagingJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a SageMaker Edge Manager model packaging job. Edge Manager will
-- use the model artifacts from the Amazon Simple Storage Service bucket
-- that you specify. After the model has been packaged, Amazon SageMaker
-- saves the resulting artifacts to an S3 bucket that you specify.
module Network.AWS.SageMaker.CreateEdgePackagingJob
  ( -- * Creating a Request
    CreateEdgePackagingJob (..),
    newCreateEdgePackagingJob,

    -- * Request Lenses
    createEdgePackagingJob_resourceKey,
    createEdgePackagingJob_tags,
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateEdgePackagingJob' smart constructor.
data CreateEdgePackagingJob = CreateEdgePackagingJob'
  { -- | The CMK to use when encrypting the EBS volume the edge packaging job
    -- runs on.
    resourceKey :: Prelude.Maybe Prelude.Text,
    -- | Creates tags for the packaging job.
    tags :: Prelude.Maybe [Tag],
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateEdgePackagingJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceKey', 'createEdgePackagingJob_resourceKey' - The CMK to use when encrypting the EBS volume the edge packaging job
-- runs on.
--
-- 'tags', 'createEdgePackagingJob_tags' - Creates tags for the packaging job.
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
      { resourceKey =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        edgePackagingJobName = pEdgePackagingJobName_,
        compilationJobName = pCompilationJobName_,
        modelName = pModelName_,
        modelVersion = pModelVersion_,
        roleArn = pRoleArn_,
        outputConfig = pOutputConfig_
      }

-- | The CMK to use when encrypting the EBS volume the edge packaging job
-- runs on.
createEdgePackagingJob_resourceKey :: Lens.Lens' CreateEdgePackagingJob (Prelude.Maybe Prelude.Text)
createEdgePackagingJob_resourceKey = Lens.lens (\CreateEdgePackagingJob' {resourceKey} -> resourceKey) (\s@CreateEdgePackagingJob' {} a -> s {resourceKey = a} :: CreateEdgePackagingJob)

-- | Creates tags for the packaging job.
createEdgePackagingJob_tags :: Lens.Lens' CreateEdgePackagingJob (Prelude.Maybe [Tag])
createEdgePackagingJob_tags = Lens.lens (\CreateEdgePackagingJob' {tags} -> tags) (\s@CreateEdgePackagingJob' {} a -> s {tags = a} :: CreateEdgePackagingJob) Prelude.. Lens.mapping Prelude._Coerce

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

instance Prelude.AWSRequest CreateEdgePackagingJob where
  type
    Rs CreateEdgePackagingJob =
      CreateEdgePackagingJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      CreateEdgePackagingJobResponse'

instance Prelude.Hashable CreateEdgePackagingJob

instance Prelude.NFData CreateEdgePackagingJob

instance Prelude.ToHeaders CreateEdgePackagingJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.CreateEdgePackagingJob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateEdgePackagingJob where
  toJSON CreateEdgePackagingJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ResourceKey" Prelude..=) Prelude.<$> resourceKey,
            ("Tags" Prelude..=) Prelude.<$> tags,
            Prelude.Just
              ( "EdgePackagingJobName"
                  Prelude..= edgePackagingJobName
              ),
            Prelude.Just
              ("CompilationJobName" Prelude..= compilationJobName),
            Prelude.Just ("ModelName" Prelude..= modelName),
            Prelude.Just
              ("ModelVersion" Prelude..= modelVersion),
            Prelude.Just ("RoleArn" Prelude..= roleArn),
            Prelude.Just
              ("OutputConfig" Prelude..= outputConfig)
          ]
      )

instance Prelude.ToPath CreateEdgePackagingJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateEdgePackagingJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEdgePackagingJobResponse' smart constructor.
data CreateEdgePackagingJobResponse = CreateEdgePackagingJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
