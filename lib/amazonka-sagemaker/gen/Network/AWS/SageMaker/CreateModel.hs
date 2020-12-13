{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a model in Amazon SageMaker. In the request, you name the model and describe a primary container. For the primary container, you specify the Docker image that contains inference code, artifacts (from prior training), and a custom environment map that the inference code uses when you deploy the model for predictions.
--
-- Use this API to create a model if you want to use Amazon SageMaker hosting services or run a batch transform job.
-- To host your model, you create an endpoint configuration with the @CreateEndpointConfig@ API, and then create an endpoint with the @CreateEndpoint@ API. Amazon SageMaker then deploys all of the containers that you defined for the model in the hosting environment.
-- For an example that calls this method when deploying a model to Amazon SageMaker hosting services, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ex1-deploy-model.html#ex1-deploy-model-boto Deploy the Model to Amazon SageMaker Hosting Services (AWS SDK for Python (Boto 3)).>
-- To run a batch transform using your model, you start a job with the @CreateTransformJob@ API. Amazon SageMaker uses your model and your dataset to get inferences which are then saved to a specified S3 location.
-- In the @CreateModel@ request, you must define a container with the @PrimaryContainer@ parameter.
-- In the request, you also provide an IAM role that Amazon SageMaker can assume to access model artifacts and docker image for deployment on ML compute hosting instances or for batch transform jobs. In addition, you also use the IAM role to manage permissions the inference code needs. For example, if the inference code access any other AWS resources, you grant necessary permissions via this role.
module Network.AWS.SageMaker.CreateModel
  ( -- * Creating a request
    CreateModel (..),
    mkCreateModel,

    -- ** Request lenses
    cmModelName,
    cmPrimaryContainer,
    cmExecutionRoleARN,
    cmEnableNetworkIsolation,
    cmContainers,
    cmVPCConfig,
    cmTags,

    -- * Destructuring the response
    CreateModelResponse (..),
    mkCreateModelResponse,

    -- ** Response lenses
    cmrsModelARN,
    cmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateModel' smart constructor.
data CreateModel = CreateModel'
  { -- | The name of the new model.
    modelName :: Lude.Text,
    -- | The location of the primary docker image containing inference code, associated artifacts, and custom environment map that the inference code uses when the model is deployed for predictions.
    primaryContainer :: Lude.Maybe ContainerDefinition,
    -- | The Amazon Resource Name (ARN) of the IAM role that Amazon SageMaker can assume to access model artifacts and docker image for deployment on ML compute instances or for batch transform jobs. Deploying on ML compute instances is part of model hosting. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
    executionRoleARN :: Lude.Text,
    -- | Isolates the model container. No inbound or outbound network calls can be made to or from the model container.
    enableNetworkIsolation :: Lude.Maybe Lude.Bool,
    -- | Specifies the containers in the inference pipeline.
    containers :: Lude.Maybe [ContainerDefinition],
    -- | A 'VpcConfig' object that specifies the VPC that you want your model to connect to. Control access to and from your model container by configuring the VPC. @VpcConfig@ is used in hosting services and in batch transform. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud> and <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-vpc.html Protect Data in Batch Transform Jobs by Using an Amazon Virtual Private Cloud> .
    vpcConfig :: Lude.Maybe VPCConfig,
    -- | An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateModel' with the minimum fields required to make a request.
--
-- * 'modelName' - The name of the new model.
-- * 'primaryContainer' - The location of the primary docker image containing inference code, associated artifacts, and custom environment map that the inference code uses when the model is deployed for predictions.
-- * 'executionRoleARN' - The Amazon Resource Name (ARN) of the IAM role that Amazon SageMaker can assume to access model artifacts and docker image for deployment on ML compute instances or for batch transform jobs. Deploying on ML compute instances is part of model hosting. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
-- * 'enableNetworkIsolation' - Isolates the model container. No inbound or outbound network calls can be made to or from the model container.
-- * 'containers' - Specifies the containers in the inference pipeline.
-- * 'vpcConfig' - A 'VpcConfig' object that specifies the VPC that you want your model to connect to. Control access to and from your model container by configuring the VPC. @VpcConfig@ is used in hosting services and in batch transform. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud> and <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-vpc.html Protect Data in Batch Transform Jobs by Using an Amazon Virtual Private Cloud> .
-- * 'tags' - An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
mkCreateModel ::
  -- | 'modelName'
  Lude.Text ->
  -- | 'executionRoleARN'
  Lude.Text ->
  CreateModel
mkCreateModel pModelName_ pExecutionRoleARN_ =
  CreateModel'
    { modelName = pModelName_,
      primaryContainer = Lude.Nothing,
      executionRoleARN = pExecutionRoleARN_,
      enableNetworkIsolation = Lude.Nothing,
      containers = Lude.Nothing,
      vpcConfig = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The name of the new model.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmModelName :: Lens.Lens' CreateModel Lude.Text
cmModelName = Lens.lens (modelName :: CreateModel -> Lude.Text) (\s a -> s {modelName = a} :: CreateModel)
{-# DEPRECATED cmModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

-- | The location of the primary docker image containing inference code, associated artifacts, and custom environment map that the inference code uses when the model is deployed for predictions.
--
-- /Note:/ Consider using 'primaryContainer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmPrimaryContainer :: Lens.Lens' CreateModel (Lude.Maybe ContainerDefinition)
cmPrimaryContainer = Lens.lens (primaryContainer :: CreateModel -> Lude.Maybe ContainerDefinition) (\s a -> s {primaryContainer = a} :: CreateModel)
{-# DEPRECATED cmPrimaryContainer "Use generic-lens or generic-optics with 'primaryContainer' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role that Amazon SageMaker can assume to access model artifacts and docker image for deployment on ML compute instances or for batch transform jobs. Deploying on ML compute instances is part of model hosting. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
--
-- /Note:/ Consider using 'executionRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmExecutionRoleARN :: Lens.Lens' CreateModel Lude.Text
cmExecutionRoleARN = Lens.lens (executionRoleARN :: CreateModel -> Lude.Text) (\s a -> s {executionRoleARN = a} :: CreateModel)
{-# DEPRECATED cmExecutionRoleARN "Use generic-lens or generic-optics with 'executionRoleARN' instead." #-}

-- | Isolates the model container. No inbound or outbound network calls can be made to or from the model container.
--
-- /Note:/ Consider using 'enableNetworkIsolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmEnableNetworkIsolation :: Lens.Lens' CreateModel (Lude.Maybe Lude.Bool)
cmEnableNetworkIsolation = Lens.lens (enableNetworkIsolation :: CreateModel -> Lude.Maybe Lude.Bool) (\s a -> s {enableNetworkIsolation = a} :: CreateModel)
{-# DEPRECATED cmEnableNetworkIsolation "Use generic-lens or generic-optics with 'enableNetworkIsolation' instead." #-}

-- | Specifies the containers in the inference pipeline.
--
-- /Note:/ Consider using 'containers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmContainers :: Lens.Lens' CreateModel (Lude.Maybe [ContainerDefinition])
cmContainers = Lens.lens (containers :: CreateModel -> Lude.Maybe [ContainerDefinition]) (\s a -> s {containers = a} :: CreateModel)
{-# DEPRECATED cmContainers "Use generic-lens or generic-optics with 'containers' instead." #-}

-- | A 'VpcConfig' object that specifies the VPC that you want your model to connect to. Control access to and from your model container by configuring the VPC. @VpcConfig@ is used in hosting services and in batch transform. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud> and <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-vpc.html Protect Data in Batch Transform Jobs by Using an Amazon Virtual Private Cloud> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmVPCConfig :: Lens.Lens' CreateModel (Lude.Maybe VPCConfig)
cmVPCConfig = Lens.lens (vpcConfig :: CreateModel -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: CreateModel)
{-# DEPRECATED cmVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmTags :: Lens.Lens' CreateModel (Lude.Maybe [Tag])
cmTags = Lens.lens (tags :: CreateModel -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateModel)
{-# DEPRECATED cmTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateModel where
  type Rs CreateModel = CreateModelResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateModelResponse'
            Lude.<$> (x Lude..:> "ModelArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateModel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateModel" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateModel where
  toJSON CreateModel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ModelName" Lude..= modelName),
            ("PrimaryContainer" Lude..=) Lude.<$> primaryContainer,
            Lude.Just ("ExecutionRoleArn" Lude..= executionRoleARN),
            ("EnableNetworkIsolation" Lude..=) Lude.<$> enableNetworkIsolation,
            ("Containers" Lude..=) Lude.<$> containers,
            ("VpcConfig" Lude..=) Lude.<$> vpcConfig,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateModel where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateModel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateModelResponse' smart constructor.
data CreateModelResponse = CreateModelResponse'
  { -- | The ARN of the model created in Amazon SageMaker.
    modelARN :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateModelResponse' with the minimum fields required to make a request.
--
-- * 'modelARN' - The ARN of the model created in Amazon SageMaker.
-- * 'responseStatus' - The response status code.
mkCreateModelResponse ::
  -- | 'modelARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateModelResponse
mkCreateModelResponse pModelARN_ pResponseStatus_ =
  CreateModelResponse'
    { modelARN = pModelARN_,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the model created in Amazon SageMaker.
--
-- /Note:/ Consider using 'modelARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrsModelARN :: Lens.Lens' CreateModelResponse Lude.Text
cmrsModelARN = Lens.lens (modelARN :: CreateModelResponse -> Lude.Text) (\s a -> s {modelARN = a} :: CreateModelResponse)
{-# DEPRECATED cmrsModelARN "Use generic-lens or generic-optics with 'modelARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrsResponseStatus :: Lens.Lens' CreateModelResponse Lude.Int
cmrsResponseStatus = Lens.lens (responseStatus :: CreateModelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateModelResponse)
{-# DEPRECATED cmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
