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
-- Module      : Network.AWS.SageMaker.CreateModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a model in Amazon SageMaker. In the request, you name the model
-- and describe a primary container. For the primary container, you specify
-- the Docker image that contains inference code, artifacts (from prior
-- training), and a custom environment map that the inference code uses
-- when you deploy the model for predictions.
--
-- Use this API to create a model if you want to use Amazon SageMaker
-- hosting services or run a batch transform job.
--
-- To host your model, you create an endpoint configuration with the
-- @CreateEndpointConfig@ API, and then create an endpoint with the
-- @CreateEndpoint@ API. Amazon SageMaker then deploys all of the
-- containers that you defined for the model in the hosting environment.
--
-- For an example that calls this method when deploying a model to Amazon
-- SageMaker hosting services, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/ex1-deploy-model.html#ex1-deploy-model-boto Deploy the Model to Amazon SageMaker Hosting Services (AWS SDK for Python (Boto 3)).>
--
-- To run a batch transform using your model, you start a job with the
-- @CreateTransformJob@ API. Amazon SageMaker uses your model and your
-- dataset to get inferences which are then saved to a specified S3
-- location.
--
-- In the @CreateModel@ request, you must define a container with the
-- @PrimaryContainer@ parameter.
--
-- In the request, you also provide an IAM role that Amazon SageMaker can
-- assume to access model artifacts and docker image for deployment on ML
-- compute hosting instances or for batch transform jobs. In addition, you
-- also use the IAM role to manage permissions the inference code needs.
-- For example, if the inference code access any other AWS resources, you
-- grant necessary permissions via this role.
module Network.AWS.SageMaker.CreateModel
  ( -- * Creating a Request
    CreateModel (..),
    newCreateModel,

    -- * Request Lenses
    createModel_vpcConfig,
    createModel_primaryContainer,
    createModel_enableNetworkIsolation,
    createModel_containers,
    createModel_tags,
    createModel_inferenceExecutionConfig,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateModel' smart constructor.
data CreateModel = CreateModel'
  { -- | A VpcConfig object that specifies the VPC that you want your model to
    -- connect to. Control access to and from your model container by
    -- configuring the VPC. @VpcConfig@ is used in hosting services and in
    -- batch transform. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud>
    -- and
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-vpc.html Protect Data in Batch Transform Jobs by Using an Amazon Virtual Private Cloud>.
    vpcConfig :: Core.Maybe VpcConfig,
    -- | The location of the primary docker image containing inference code,
    -- associated artifacts, and custom environment map that the inference code
    -- uses when the model is deployed for predictions.
    primaryContainer :: Core.Maybe ContainerDefinition,
    -- | Isolates the model container. No inbound or outbound network calls can
    -- be made to or from the model container.
    enableNetworkIsolation :: Core.Maybe Core.Bool,
    -- | Specifies the containers in the inference pipeline.
    containers :: Core.Maybe [ContainerDefinition],
    -- | An array of key-value pairs. You can use tags to categorize your AWS
    -- resources in different ways, for example, by purpose, owner, or
    -- environment. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
    tags :: Core.Maybe [Tag],
    -- | Specifies details of how containers in a multi-container endpoint are
    -- called.
    inferenceExecutionConfig :: Core.Maybe InferenceExecutionConfig,
    -- | The name of the new model.
    modelName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that Amazon SageMaker can
    -- assume to access model artifacts and docker image for deployment on ML
    -- compute instances or for batch transform jobs. Deploying on ML compute
    -- instances is part of model hosting. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles>.
    --
    -- To be able to pass this role to Amazon SageMaker, the caller of this API
    -- must have the @iam:PassRole@ permission.
    executionRoleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'createModel_vpcConfig' - A VpcConfig object that specifies the VPC that you want your model to
-- connect to. Control access to and from your model container by
-- configuring the VPC. @VpcConfig@ is used in hosting services and in
-- batch transform. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud>
-- and
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-vpc.html Protect Data in Batch Transform Jobs by Using an Amazon Virtual Private Cloud>.
--
-- 'primaryContainer', 'createModel_primaryContainer' - The location of the primary docker image containing inference code,
-- associated artifacts, and custom environment map that the inference code
-- uses when the model is deployed for predictions.
--
-- 'enableNetworkIsolation', 'createModel_enableNetworkIsolation' - Isolates the model container. No inbound or outbound network calls can
-- be made to or from the model container.
--
-- 'containers', 'createModel_containers' - Specifies the containers in the inference pipeline.
--
-- 'tags', 'createModel_tags' - An array of key-value pairs. You can use tags to categorize your AWS
-- resources in different ways, for example, by purpose, owner, or
-- environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
--
-- 'inferenceExecutionConfig', 'createModel_inferenceExecutionConfig' - Specifies details of how containers in a multi-container endpoint are
-- called.
--
-- 'modelName', 'createModel_modelName' - The name of the new model.
--
-- 'executionRoleArn', 'createModel_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM role that Amazon SageMaker can
-- assume to access model artifacts and docker image for deployment on ML
-- compute instances or for batch transform jobs. Deploying on ML compute
-- instances is part of model hosting. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles>.
--
-- To be able to pass this role to Amazon SageMaker, the caller of this API
-- must have the @iam:PassRole@ permission.
newCreateModel ::
  -- | 'modelName'
  Core.Text ->
  -- | 'executionRoleArn'
  Core.Text ->
  CreateModel
newCreateModel pModelName_ pExecutionRoleArn_ =
  CreateModel'
    { vpcConfig = Core.Nothing,
      primaryContainer = Core.Nothing,
      enableNetworkIsolation = Core.Nothing,
      containers = Core.Nothing,
      tags = Core.Nothing,
      inferenceExecutionConfig = Core.Nothing,
      modelName = pModelName_,
      executionRoleArn = pExecutionRoleArn_
    }

-- | A VpcConfig object that specifies the VPC that you want your model to
-- connect to. Control access to and from your model container by
-- configuring the VPC. @VpcConfig@ is used in hosting services and in
-- batch transform. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud>
-- and
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-vpc.html Protect Data in Batch Transform Jobs by Using an Amazon Virtual Private Cloud>.
createModel_vpcConfig :: Lens.Lens' CreateModel (Core.Maybe VpcConfig)
createModel_vpcConfig = Lens.lens (\CreateModel' {vpcConfig} -> vpcConfig) (\s@CreateModel' {} a -> s {vpcConfig = a} :: CreateModel)

-- | The location of the primary docker image containing inference code,
-- associated artifacts, and custom environment map that the inference code
-- uses when the model is deployed for predictions.
createModel_primaryContainer :: Lens.Lens' CreateModel (Core.Maybe ContainerDefinition)
createModel_primaryContainer = Lens.lens (\CreateModel' {primaryContainer} -> primaryContainer) (\s@CreateModel' {} a -> s {primaryContainer = a} :: CreateModel)

-- | Isolates the model container. No inbound or outbound network calls can
-- be made to or from the model container.
createModel_enableNetworkIsolation :: Lens.Lens' CreateModel (Core.Maybe Core.Bool)
createModel_enableNetworkIsolation = Lens.lens (\CreateModel' {enableNetworkIsolation} -> enableNetworkIsolation) (\s@CreateModel' {} a -> s {enableNetworkIsolation = a} :: CreateModel)

-- | Specifies the containers in the inference pipeline.
createModel_containers :: Lens.Lens' CreateModel (Core.Maybe [ContainerDefinition])
createModel_containers = Lens.lens (\CreateModel' {containers} -> containers) (\s@CreateModel' {} a -> s {containers = a} :: CreateModel) Core.. Lens.mapping Lens._Coerce

-- | An array of key-value pairs. You can use tags to categorize your AWS
-- resources in different ways, for example, by purpose, owner, or
-- environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
createModel_tags :: Lens.Lens' CreateModel (Core.Maybe [Tag])
createModel_tags = Lens.lens (\CreateModel' {tags} -> tags) (\s@CreateModel' {} a -> s {tags = a} :: CreateModel) Core.. Lens.mapping Lens._Coerce

-- | Specifies details of how containers in a multi-container endpoint are
-- called.
createModel_inferenceExecutionConfig :: Lens.Lens' CreateModel (Core.Maybe InferenceExecutionConfig)
createModel_inferenceExecutionConfig = Lens.lens (\CreateModel' {inferenceExecutionConfig} -> inferenceExecutionConfig) (\s@CreateModel' {} a -> s {inferenceExecutionConfig = a} :: CreateModel)

-- | The name of the new model.
createModel_modelName :: Lens.Lens' CreateModel Core.Text
createModel_modelName = Lens.lens (\CreateModel' {modelName} -> modelName) (\s@CreateModel' {} a -> s {modelName = a} :: CreateModel)

-- | The Amazon Resource Name (ARN) of the IAM role that Amazon SageMaker can
-- assume to access model artifacts and docker image for deployment on ML
-- compute instances or for batch transform jobs. Deploying on ML compute
-- instances is part of model hosting. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles>.
--
-- To be able to pass this role to Amazon SageMaker, the caller of this API
-- must have the @iam:PassRole@ permission.
createModel_executionRoleArn :: Lens.Lens' CreateModel Core.Text
createModel_executionRoleArn = Lens.lens (\CreateModel' {executionRoleArn} -> executionRoleArn) (\s@CreateModel' {} a -> s {executionRoleArn = a} :: CreateModel)

instance Core.AWSRequest CreateModel where
  type AWSResponse CreateModel = CreateModelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateModelResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "ModelArn")
      )

instance Core.Hashable CreateModel

instance Core.NFData CreateModel

instance Core.ToHeaders CreateModel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.CreateModel" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateModel where
  toJSON CreateModel' {..} =
    Core.object
      ( Core.catMaybes
          [ ("VpcConfig" Core..=) Core.<$> vpcConfig,
            ("PrimaryContainer" Core..=)
              Core.<$> primaryContainer,
            ("EnableNetworkIsolation" Core..=)
              Core.<$> enableNetworkIsolation,
            ("Containers" Core..=) Core.<$> containers,
            ("Tags" Core..=) Core.<$> tags,
            ("InferenceExecutionConfig" Core..=)
              Core.<$> inferenceExecutionConfig,
            Core.Just ("ModelName" Core..= modelName),
            Core.Just
              ("ExecutionRoleArn" Core..= executionRoleArn)
          ]
      )

instance Core.ToPath CreateModel where
  toPath = Core.const "/"

instance Core.ToQuery CreateModel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateModelResponse' smart constructor.
data CreateModelResponse = CreateModelResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The ARN of the model created in Amazon SageMaker.
    modelArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'modelArn', 'createModelResponse_modelArn' - The ARN of the model created in Amazon SageMaker.
newCreateModelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'modelArn'
  Core.Text ->
  CreateModelResponse
newCreateModelResponse pHttpStatus_ pModelArn_ =
  CreateModelResponse'
    { httpStatus = pHttpStatus_,
      modelArn = pModelArn_
    }

-- | The response's http status code.
createModelResponse_httpStatus :: Lens.Lens' CreateModelResponse Core.Int
createModelResponse_httpStatus = Lens.lens (\CreateModelResponse' {httpStatus} -> httpStatus) (\s@CreateModelResponse' {} a -> s {httpStatus = a} :: CreateModelResponse)

-- | The ARN of the model created in Amazon SageMaker.
createModelResponse_modelArn :: Lens.Lens' CreateModelResponse Core.Text
createModelResponse_modelArn = Lens.lens (\CreateModelResponse' {modelArn} -> modelArn) (\s@CreateModelResponse' {} a -> s {modelArn = a} :: CreateModelResponse)

instance Core.NFData CreateModelResponse
