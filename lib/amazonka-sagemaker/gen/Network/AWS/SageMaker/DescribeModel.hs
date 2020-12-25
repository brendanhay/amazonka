{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a model that you created using the @CreateModel@ API.
module Network.AWS.SageMaker.DescribeModel
  ( -- * Creating a request
    DescribeModel (..),
    mkDescribeModel,

    -- ** Request lenses
    dModelName,

    -- * Destructuring the response
    DescribeModelResponse (..),
    mkDescribeModelResponse,

    -- ** Response lenses
    dmrrsModelName,
    dmrrsExecutionRoleArn,
    dmrrsCreationTime,
    dmrrsModelArn,
    dmrrsContainers,
    dmrrsEnableNetworkIsolation,
    dmrrsPrimaryContainer,
    dmrrsVpcConfig,
    dmrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeModel' smart constructor.
newtype DescribeModel = DescribeModel'
  { -- | The name of the model.
    modelName :: Types.ModelName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeModel' value with any optional fields omitted.
mkDescribeModel ::
  -- | 'modelName'
  Types.ModelName ->
  DescribeModel
mkDescribeModel modelName = DescribeModel' {modelName}

-- | The name of the model.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dModelName :: Lens.Lens' DescribeModel Types.ModelName
dModelName = Lens.field @"modelName"
{-# DEPRECATED dModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

instance Core.FromJSON DescribeModel where
  toJSON DescribeModel {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ModelName" Core..= modelName)])

instance Core.AWSRequest DescribeModel where
  type Rs DescribeModel = DescribeModelResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DescribeModel")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeModelResponse'
            Core.<$> (x Core..: "ModelName")
            Core.<*> (x Core..: "ExecutionRoleArn")
            Core.<*> (x Core..: "CreationTime")
            Core.<*> (x Core..: "ModelArn")
            Core.<*> (x Core..:? "Containers")
            Core.<*> (x Core..:? "EnableNetworkIsolation")
            Core.<*> (x Core..:? "PrimaryContainer")
            Core.<*> (x Core..:? "VpcConfig")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeModelResponse' smart constructor.
data DescribeModelResponse = DescribeModelResponse'
  { -- | Name of the Amazon SageMaker model.
    modelName :: Types.ModelName,
    -- | The Amazon Resource Name (ARN) of the IAM role that you specified for the model.
    executionRoleArn :: Types.ExecutionRoleArn,
    -- | A timestamp that shows when the model was created.
    creationTime :: Core.NominalDiffTime,
    -- | The Amazon Resource Name (ARN) of the model.
    modelArn :: Types.ModelArn,
    -- | The containers in the inference pipeline.
    containers :: Core.Maybe [Types.ContainerDefinition],
    -- | If @True@ , no inbound or outbound network calls can be made to or from the model container.
    enableNetworkIsolation :: Core.Maybe Core.Bool,
    -- | The location of the primary inference code, associated artifacts, and custom environment map that the inference code uses when it is deployed in production.
    primaryContainer :: Core.Maybe Types.ContainerDefinition,
    -- | A 'VpcConfig' object that specifies the VPC that this model has access to. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud>
    vpcConfig :: Core.Maybe Types.VpcConfig,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeModelResponse' value with any optional fields omitted.
mkDescribeModelResponse ::
  -- | 'modelName'
  Types.ModelName ->
  -- | 'executionRoleArn'
  Types.ExecutionRoleArn ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  -- | 'modelArn'
  Types.ModelArn ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeModelResponse
mkDescribeModelResponse
  modelName
  executionRoleArn
  creationTime
  modelArn
  responseStatus =
    DescribeModelResponse'
      { modelName,
        executionRoleArn,
        creationTime,
        modelArn,
        containers = Core.Nothing,
        enableNetworkIsolation = Core.Nothing,
        primaryContainer = Core.Nothing,
        vpcConfig = Core.Nothing,
        responseStatus
      }

-- | Name of the Amazon SageMaker model.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsModelName :: Lens.Lens' DescribeModelResponse Types.ModelName
dmrrsModelName = Lens.field @"modelName"
{-# DEPRECATED dmrrsModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role that you specified for the model.
--
-- /Note:/ Consider using 'executionRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsExecutionRoleArn :: Lens.Lens' DescribeModelResponse Types.ExecutionRoleArn
dmrrsExecutionRoleArn = Lens.field @"executionRoleArn"
{-# DEPRECATED dmrrsExecutionRoleArn "Use generic-lens or generic-optics with 'executionRoleArn' instead." #-}

-- | A timestamp that shows when the model was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsCreationTime :: Lens.Lens' DescribeModelResponse Core.NominalDiffTime
dmrrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED dmrrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the model.
--
-- /Note:/ Consider using 'modelArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsModelArn :: Lens.Lens' DescribeModelResponse Types.ModelArn
dmrrsModelArn = Lens.field @"modelArn"
{-# DEPRECATED dmrrsModelArn "Use generic-lens or generic-optics with 'modelArn' instead." #-}

-- | The containers in the inference pipeline.
--
-- /Note:/ Consider using 'containers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsContainers :: Lens.Lens' DescribeModelResponse (Core.Maybe [Types.ContainerDefinition])
dmrrsContainers = Lens.field @"containers"
{-# DEPRECATED dmrrsContainers "Use generic-lens or generic-optics with 'containers' instead." #-}

-- | If @True@ , no inbound or outbound network calls can be made to or from the model container.
--
-- /Note:/ Consider using 'enableNetworkIsolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsEnableNetworkIsolation :: Lens.Lens' DescribeModelResponse (Core.Maybe Core.Bool)
dmrrsEnableNetworkIsolation = Lens.field @"enableNetworkIsolation"
{-# DEPRECATED dmrrsEnableNetworkIsolation "Use generic-lens or generic-optics with 'enableNetworkIsolation' instead." #-}

-- | The location of the primary inference code, associated artifacts, and custom environment map that the inference code uses when it is deployed in production.
--
-- /Note:/ Consider using 'primaryContainer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsPrimaryContainer :: Lens.Lens' DescribeModelResponse (Core.Maybe Types.ContainerDefinition)
dmrrsPrimaryContainer = Lens.field @"primaryContainer"
{-# DEPRECATED dmrrsPrimaryContainer "Use generic-lens or generic-optics with 'primaryContainer' instead." #-}

-- | A 'VpcConfig' object that specifies the VPC that this model has access to. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud>
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsVpcConfig :: Lens.Lens' DescribeModelResponse (Core.Maybe Types.VpcConfig)
dmrrsVpcConfig = Lens.field @"vpcConfig"
{-# DEPRECATED dmrrsVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsResponseStatus :: Lens.Lens' DescribeModelResponse Core.Int
dmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
