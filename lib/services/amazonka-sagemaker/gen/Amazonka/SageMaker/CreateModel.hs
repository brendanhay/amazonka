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
-- Module      : Amazonka.SageMaker.CreateModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a model in SageMaker. In the request, you name the model and
-- describe a primary container. For the primary container, you specify the
-- Docker image that contains inference code, artifacts (from prior
-- training), and a custom environment map that the inference code uses
-- when you deploy the model for predictions.
--
-- Use this API to create a model if you want to use SageMaker hosting
-- services or run a batch transform job.
--
-- To host your model, you create an endpoint configuration with the
-- @CreateEndpointConfig@ API, and then create an endpoint with the
-- @CreateEndpoint@ API. SageMaker then deploys all of the containers that
-- you defined for the model in the hosting environment.
--
-- For an example that calls this method when deploying a model to
-- SageMaker hosting services, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/realtime-endpoints-deployment.html#realtime-endpoints-deployment-create-model Create a Model (Amazon Web Services SDK for Python (Boto 3)).>
--
-- To run a batch transform using your model, you start a job with the
-- @CreateTransformJob@ API. SageMaker uses your model and your dataset to
-- get inferences which are then saved to a specified S3 location.
--
-- In the request, you also provide an IAM role that SageMaker can assume
-- to access model artifacts and docker image for deployment on ML compute
-- hosting instances or for batch transform jobs. In addition, you also use
-- the IAM role to manage permissions the inference code needs. For
-- example, if the inference code access any other Amazon Web Services
-- resources, you grant necessary permissions via this role.
module Amazonka.SageMaker.CreateModel
  ( -- * Creating a Request
    CreateModel (..),
    newCreateModel,

    -- * Request Lenses
    createModel_containers,
    createModel_enableNetworkIsolation,
    createModel_inferenceExecutionConfig,
    createModel_primaryContainer,
    createModel_tags,
    createModel_vpcConfig,
    createModel_modelName,
    createModel_executionRoleArn,

    -- * Destructuring the Response
    CreateModelResponse (..),
    newCreateModelResponse,

    -- * Response Lenses
    createModelResponse_httpStatus,
    createModelResponse_modelArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateModel' smart constructor.
data CreateModel = CreateModel'
  { -- | Specifies the containers in the inference pipeline.
    containers :: Prelude.Maybe [ContainerDefinition],
    -- | Isolates the model container. No inbound or outbound network calls can
    -- be made to or from the model container.
    enableNetworkIsolation :: Prelude.Maybe Prelude.Bool,
    -- | Specifies details of how containers in a multi-container endpoint are
    -- called.
    inferenceExecutionConfig :: Prelude.Maybe InferenceExecutionConfig,
    -- | The location of the primary docker image containing inference code,
    -- associated artifacts, and custom environment map that the inference code
    -- uses when the model is deployed for predictions.
    primaryContainer :: Prelude.Maybe ContainerDefinition,
    -- | An array of key-value pairs. You can use tags to categorize your Amazon
    -- Web Services resources in different ways, for example, by purpose,
    -- owner, or environment. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
    tags :: Prelude.Maybe [Tag],
    -- | A
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_VpcConfig.html VpcConfig>
    -- object that specifies the VPC that you want your model to connect to.
    -- Control access to and from your model container by configuring the VPC.
    -- @VpcConfig@ is used in hosting services and in batch transform. For more
    -- information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud>
    -- and
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-vpc.html Protect Data in Batch Transform Jobs by Using an Amazon Virtual Private Cloud>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | The name of the new model.
    modelName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that SageMaker can assume
    -- to access model artifacts and docker image for deployment on ML compute
    -- instances or for batch transform jobs. Deploying on ML compute instances
    -- is part of model hosting. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html SageMaker Roles>.
    --
    -- To be able to pass this role to SageMaker, the caller of this API must
    -- have the @iam:PassRole@ permission.
    executionRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containers', 'createModel_containers' - Specifies the containers in the inference pipeline.
--
-- 'enableNetworkIsolation', 'createModel_enableNetworkIsolation' - Isolates the model container. No inbound or outbound network calls can
-- be made to or from the model container.
--
-- 'inferenceExecutionConfig', 'createModel_inferenceExecutionConfig' - Specifies details of how containers in a multi-container endpoint are
-- called.
--
-- 'primaryContainer', 'createModel_primaryContainer' - The location of the primary docker image containing inference code,
-- associated artifacts, and custom environment map that the inference code
-- uses when the model is deployed for predictions.
--
-- 'tags', 'createModel_tags' - An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
--
-- 'vpcConfig', 'createModel_vpcConfig' - A
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_VpcConfig.html VpcConfig>
-- object that specifies the VPC that you want your model to connect to.
-- Control access to and from your model container by configuring the VPC.
-- @VpcConfig@ is used in hosting services and in batch transform. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud>
-- and
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-vpc.html Protect Data in Batch Transform Jobs by Using an Amazon Virtual Private Cloud>.
--
-- 'modelName', 'createModel_modelName' - The name of the new model.
--
-- 'executionRoleArn', 'createModel_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM role that SageMaker can assume
-- to access model artifacts and docker image for deployment on ML compute
-- instances or for batch transform jobs. Deploying on ML compute instances
-- is part of model hosting. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html SageMaker Roles>.
--
-- To be able to pass this role to SageMaker, the caller of this API must
-- have the @iam:PassRole@ permission.
newCreateModel ::
  -- | 'modelName'
  Prelude.Text ->
  -- | 'executionRoleArn'
  Prelude.Text ->
  CreateModel
newCreateModel pModelName_ pExecutionRoleArn_ =
  CreateModel'
    { containers = Prelude.Nothing,
      enableNetworkIsolation = Prelude.Nothing,
      inferenceExecutionConfig = Prelude.Nothing,
      primaryContainer = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      modelName = pModelName_,
      executionRoleArn = pExecutionRoleArn_
    }

-- | Specifies the containers in the inference pipeline.
createModel_containers :: Lens.Lens' CreateModel (Prelude.Maybe [ContainerDefinition])
createModel_containers = Lens.lens (\CreateModel' {containers} -> containers) (\s@CreateModel' {} a -> s {containers = a} :: CreateModel) Prelude.. Lens.mapping Lens.coerced

-- | Isolates the model container. No inbound or outbound network calls can
-- be made to or from the model container.
createModel_enableNetworkIsolation :: Lens.Lens' CreateModel (Prelude.Maybe Prelude.Bool)
createModel_enableNetworkIsolation = Lens.lens (\CreateModel' {enableNetworkIsolation} -> enableNetworkIsolation) (\s@CreateModel' {} a -> s {enableNetworkIsolation = a} :: CreateModel)

-- | Specifies details of how containers in a multi-container endpoint are
-- called.
createModel_inferenceExecutionConfig :: Lens.Lens' CreateModel (Prelude.Maybe InferenceExecutionConfig)
createModel_inferenceExecutionConfig = Lens.lens (\CreateModel' {inferenceExecutionConfig} -> inferenceExecutionConfig) (\s@CreateModel' {} a -> s {inferenceExecutionConfig = a} :: CreateModel)

-- | The location of the primary docker image containing inference code,
-- associated artifacts, and custom environment map that the inference code
-- uses when the model is deployed for predictions.
createModel_primaryContainer :: Lens.Lens' CreateModel (Prelude.Maybe ContainerDefinition)
createModel_primaryContainer = Lens.lens (\CreateModel' {primaryContainer} -> primaryContainer) (\s@CreateModel' {} a -> s {primaryContainer = a} :: CreateModel)

-- | An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
createModel_tags :: Lens.Lens' CreateModel (Prelude.Maybe [Tag])
createModel_tags = Lens.lens (\CreateModel' {tags} -> tags) (\s@CreateModel' {} a -> s {tags = a} :: CreateModel) Prelude.. Lens.mapping Lens.coerced

-- | A
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_VpcConfig.html VpcConfig>
-- object that specifies the VPC that you want your model to connect to.
-- Control access to and from your model container by configuring the VPC.
-- @VpcConfig@ is used in hosting services and in batch transform. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud>
-- and
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-vpc.html Protect Data in Batch Transform Jobs by Using an Amazon Virtual Private Cloud>.
createModel_vpcConfig :: Lens.Lens' CreateModel (Prelude.Maybe VpcConfig)
createModel_vpcConfig = Lens.lens (\CreateModel' {vpcConfig} -> vpcConfig) (\s@CreateModel' {} a -> s {vpcConfig = a} :: CreateModel)

-- | The name of the new model.
createModel_modelName :: Lens.Lens' CreateModel Prelude.Text
createModel_modelName = Lens.lens (\CreateModel' {modelName} -> modelName) (\s@CreateModel' {} a -> s {modelName = a} :: CreateModel)

-- | The Amazon Resource Name (ARN) of the IAM role that SageMaker can assume
-- to access model artifacts and docker image for deployment on ML compute
-- instances or for batch transform jobs. Deploying on ML compute instances
-- is part of model hosting. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html SageMaker Roles>.
--
-- To be able to pass this role to SageMaker, the caller of this API must
-- have the @iam:PassRole@ permission.
createModel_executionRoleArn :: Lens.Lens' CreateModel Prelude.Text
createModel_executionRoleArn = Lens.lens (\CreateModel' {executionRoleArn} -> executionRoleArn) (\s@CreateModel' {} a -> s {executionRoleArn = a} :: CreateModel)

instance Core.AWSRequest CreateModel where
  type AWSResponse CreateModel = CreateModelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateModelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ModelArn")
      )

instance Prelude.Hashable CreateModel where
  hashWithSalt _salt CreateModel' {..} =
    _salt
      `Prelude.hashWithSalt` containers
      `Prelude.hashWithSalt` enableNetworkIsolation
      `Prelude.hashWithSalt` inferenceExecutionConfig
      `Prelude.hashWithSalt` primaryContainer
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` modelName
      `Prelude.hashWithSalt` executionRoleArn

instance Prelude.NFData CreateModel where
  rnf CreateModel' {..} =
    Prelude.rnf containers
      `Prelude.seq` Prelude.rnf enableNetworkIsolation
      `Prelude.seq` Prelude.rnf inferenceExecutionConfig
      `Prelude.seq` Prelude.rnf primaryContainer
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf executionRoleArn

instance Data.ToHeaders CreateModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.CreateModel" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateModel where
  toJSON CreateModel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Containers" Data..=) Prelude.<$> containers,
            ("EnableNetworkIsolation" Data..=)
              Prelude.<$> enableNetworkIsolation,
            ("InferenceExecutionConfig" Data..=)
              Prelude.<$> inferenceExecutionConfig,
            ("PrimaryContainer" Data..=)
              Prelude.<$> primaryContainer,
            ("Tags" Data..=) Prelude.<$> tags,
            ("VpcConfig" Data..=) Prelude.<$> vpcConfig,
            Prelude.Just ("ModelName" Data..= modelName),
            Prelude.Just
              ("ExecutionRoleArn" Data..= executionRoleArn)
          ]
      )

instance Data.ToPath CreateModel where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateModelResponse' smart constructor.
data CreateModelResponse = CreateModelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the model created in SageMaker.
    modelArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createModelResponse_httpStatus' - The response's http status code.
--
-- 'modelArn', 'createModelResponse_modelArn' - The ARN of the model created in SageMaker.
newCreateModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'modelArn'
  Prelude.Text ->
  CreateModelResponse
newCreateModelResponse pHttpStatus_ pModelArn_ =
  CreateModelResponse'
    { httpStatus = pHttpStatus_,
      modelArn = pModelArn_
    }

-- | The response's http status code.
createModelResponse_httpStatus :: Lens.Lens' CreateModelResponse Prelude.Int
createModelResponse_httpStatus = Lens.lens (\CreateModelResponse' {httpStatus} -> httpStatus) (\s@CreateModelResponse' {} a -> s {httpStatus = a} :: CreateModelResponse)

-- | The ARN of the model created in SageMaker.
createModelResponse_modelArn :: Lens.Lens' CreateModelResponse Prelude.Text
createModelResponse_modelArn = Lens.lens (\CreateModelResponse' {modelArn} -> modelArn) (\s@CreateModelResponse' {} a -> s {modelArn = a} :: CreateModelResponse)

instance Prelude.NFData CreateModelResponse where
  rnf CreateModelResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf modelArn
