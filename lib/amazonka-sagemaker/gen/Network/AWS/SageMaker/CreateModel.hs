{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateModel (..)
    , mkCreateModel
    -- ** Request lenses
    , cmModelName
    , cmExecutionRoleArn
    , cmContainers
    , cmEnableNetworkIsolation
    , cmPrimaryContainer
    , cmTags
    , cmVpcConfig

    -- * Destructuring the response
    , CreateModelResponse (..)
    , mkCreateModelResponse
    -- ** Response lenses
    , cmrrsModelArn
    , cmrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateModel' smart constructor.
data CreateModel = CreateModel'
  { modelName :: Types.ModelName
    -- ^ The name of the new model.
  , executionRoleArn :: Types.ExecutionRoleArn
    -- ^ The Amazon Resource Name (ARN) of the IAM role that Amazon SageMaker can assume to access model artifacts and docker image for deployment on ML compute instances or for batch transform jobs. Deploying on ML compute instances is part of model hosting. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> . 
  , containers :: Core.Maybe [Types.ContainerDefinition]
    -- ^ Specifies the containers in the inference pipeline.
  , enableNetworkIsolation :: Core.Maybe Core.Bool
    -- ^ Isolates the model container. No inbound or outbound network calls can be made to or from the model container.
  , primaryContainer :: Core.Maybe Types.ContainerDefinition
    -- ^ The location of the primary docker image containing inference code, associated artifacts, and custom environment map that the inference code uses when the model is deployed for predictions. 
  , tags :: Core.Maybe [Types.Tag]
    -- ^ An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ . 
  , vpcConfig :: Core.Maybe Types.VpcConfig
    -- ^ A 'VpcConfig' object that specifies the VPC that you want your model to connect to. Control access to and from your model container by configuring the VPC. @VpcConfig@ is used in hosting services and in batch transform. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud> and <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-vpc.html Protect Data in Batch Transform Jobs by Using an Amazon Virtual Private Cloud> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateModel' value with any optional fields omitted.
mkCreateModel
    :: Types.ModelName -- ^ 'modelName'
    -> Types.ExecutionRoleArn -- ^ 'executionRoleArn'
    -> CreateModel
mkCreateModel modelName executionRoleArn
  = CreateModel'{modelName, executionRoleArn,
                 containers = Core.Nothing, enableNetworkIsolation = Core.Nothing,
                 primaryContainer = Core.Nothing, tags = Core.Nothing,
                 vpcConfig = Core.Nothing}

-- | The name of the new model.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmModelName :: Lens.Lens' CreateModel Types.ModelName
cmModelName = Lens.field @"modelName"
{-# INLINEABLE cmModelName #-}
{-# DEPRECATED modelName "Use generic-lens or generic-optics with 'modelName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the IAM role that Amazon SageMaker can assume to access model artifacts and docker image for deployment on ML compute instances or for batch transform jobs. Deploying on ML compute instances is part of model hosting. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> . 
--
-- /Note:/ Consider using 'executionRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmExecutionRoleArn :: Lens.Lens' CreateModel Types.ExecutionRoleArn
cmExecutionRoleArn = Lens.field @"executionRoleArn"
{-# INLINEABLE cmExecutionRoleArn #-}
{-# DEPRECATED executionRoleArn "Use generic-lens or generic-optics with 'executionRoleArn' instead"  #-}

-- | Specifies the containers in the inference pipeline.
--
-- /Note:/ Consider using 'containers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmContainers :: Lens.Lens' CreateModel (Core.Maybe [Types.ContainerDefinition])
cmContainers = Lens.field @"containers"
{-# INLINEABLE cmContainers #-}
{-# DEPRECATED containers "Use generic-lens or generic-optics with 'containers' instead"  #-}

-- | Isolates the model container. No inbound or outbound network calls can be made to or from the model container.
--
-- /Note:/ Consider using 'enableNetworkIsolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmEnableNetworkIsolation :: Lens.Lens' CreateModel (Core.Maybe Core.Bool)
cmEnableNetworkIsolation = Lens.field @"enableNetworkIsolation"
{-# INLINEABLE cmEnableNetworkIsolation #-}
{-# DEPRECATED enableNetworkIsolation "Use generic-lens or generic-optics with 'enableNetworkIsolation' instead"  #-}

-- | The location of the primary docker image containing inference code, associated artifacts, and custom environment map that the inference code uses when the model is deployed for predictions. 
--
-- /Note:/ Consider using 'primaryContainer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmPrimaryContainer :: Lens.Lens' CreateModel (Core.Maybe Types.ContainerDefinition)
cmPrimaryContainer = Lens.field @"primaryContainer"
{-# INLINEABLE cmPrimaryContainer #-}
{-# DEPRECATED primaryContainer "Use generic-lens or generic-optics with 'primaryContainer' instead"  #-}

-- | An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ . 
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmTags :: Lens.Lens' CreateModel (Core.Maybe [Types.Tag])
cmTags = Lens.field @"tags"
{-# INLINEABLE cmTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | A 'VpcConfig' object that specifies the VPC that you want your model to connect to. Control access to and from your model container by configuring the VPC. @VpcConfig@ is used in hosting services and in batch transform. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud> and <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-vpc.html Protect Data in Batch Transform Jobs by Using an Amazon Virtual Private Cloud> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmVpcConfig :: Lens.Lens' CreateModel (Core.Maybe Types.VpcConfig)
cmVpcConfig = Lens.field @"vpcConfig"
{-# INLINEABLE cmVpcConfig #-}
{-# DEPRECATED vpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead"  #-}

instance Core.ToQuery CreateModel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateModel where
        toHeaders CreateModel{..}
          = Core.pure ("X-Amz-Target", "SageMaker.CreateModel") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateModel where
        toJSON CreateModel{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ModelName" Core..= modelName),
                  Core.Just ("ExecutionRoleArn" Core..= executionRoleArn),
                  ("Containers" Core..=) Core.<$> containers,
                  ("EnableNetworkIsolation" Core..=) Core.<$> enableNetworkIsolation,
                  ("PrimaryContainer" Core..=) Core.<$> primaryContainer,
                  ("Tags" Core..=) Core.<$> tags,
                  ("VpcConfig" Core..=) Core.<$> vpcConfig])

instance Core.AWSRequest CreateModel where
        type Rs CreateModel = CreateModelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateModelResponse' Core.<$>
                   (x Core..: "ModelArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateModelResponse' smart constructor.
data CreateModelResponse = CreateModelResponse'
  { modelArn :: Types.ModelArn
    -- ^ The ARN of the model created in Amazon SageMaker.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateModelResponse' value with any optional fields omitted.
mkCreateModelResponse
    :: Types.ModelArn -- ^ 'modelArn'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateModelResponse
mkCreateModelResponse modelArn responseStatus
  = CreateModelResponse'{modelArn, responseStatus}

-- | The ARN of the model created in Amazon SageMaker.
--
-- /Note:/ Consider using 'modelArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrrsModelArn :: Lens.Lens' CreateModelResponse Types.ModelArn
cmrrsModelArn = Lens.field @"modelArn"
{-# INLINEABLE cmrrsModelArn #-}
{-# DEPRECATED modelArn "Use generic-lens or generic-optics with 'modelArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrrsResponseStatus :: Lens.Lens' CreateModelResponse Core.Int
cmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
