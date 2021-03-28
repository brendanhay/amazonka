{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeModel (..)
    , mkDescribeModel
    -- ** Request lenses
    , dModelName

    -- * Destructuring the response
    , DescribeModelResponse (..)
    , mkDescribeModelResponse
    -- ** Response lenses
    , dmrrsModelName
    , dmrrsExecutionRoleArn
    , dmrrsCreationTime
    , dmrrsModelArn
    , dmrrsContainers
    , dmrrsEnableNetworkIsolation
    , dmrrsPrimaryContainer
    , dmrrsVpcConfig
    , dmrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeModel' smart constructor.
newtype DescribeModel = DescribeModel'
  { modelName :: Types.ModelName
    -- ^ The name of the model.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeModel' value with any optional fields omitted.
mkDescribeModel
    :: Types.ModelName -- ^ 'modelName'
    -> DescribeModel
mkDescribeModel modelName = DescribeModel'{modelName}

-- | The name of the model.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dModelName :: Lens.Lens' DescribeModel Types.ModelName
dModelName = Lens.field @"modelName"
{-# INLINEABLE dModelName #-}
{-# DEPRECATED modelName "Use generic-lens or generic-optics with 'modelName' instead"  #-}

instance Core.ToQuery DescribeModel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeModel where
        toHeaders DescribeModel{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DescribeModel") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeModel where
        toJSON DescribeModel{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ModelName" Core..= modelName)])

instance Core.AWSRequest DescribeModel where
        type Rs DescribeModel = DescribeModelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeModelResponse' Core.<$>
                   (x Core..: "ModelName") Core.<*> x Core..: "ExecutionRoleArn"
                     Core.<*> x Core..: "CreationTime"
                     Core.<*> x Core..: "ModelArn"
                     Core.<*> x Core..:? "Containers"
                     Core.<*> x Core..:? "EnableNetworkIsolation"
                     Core.<*> x Core..:? "PrimaryContainer"
                     Core.<*> x Core..:? "VpcConfig"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeModelResponse' smart constructor.
data DescribeModelResponse = DescribeModelResponse'
  { modelName :: Types.ModelName
    -- ^ Name of the Amazon SageMaker model.
  , executionRoleArn :: Types.ExecutionRoleArn
    -- ^ The Amazon Resource Name (ARN) of the IAM role that you specified for the model.
  , creationTime :: Core.NominalDiffTime
    -- ^ A timestamp that shows when the model was created.
  , modelArn :: Types.ModelArn
    -- ^ The Amazon Resource Name (ARN) of the model.
  , containers :: Core.Maybe [Types.ContainerDefinition]
    -- ^ The containers in the inference pipeline.
  , enableNetworkIsolation :: Core.Maybe Core.Bool
    -- ^ If @True@ , no inbound or outbound network calls can be made to or from the model container.
  , primaryContainer :: Core.Maybe Types.ContainerDefinition
    -- ^ The location of the primary inference code, associated artifacts, and custom environment map that the inference code uses when it is deployed in production. 
  , vpcConfig :: Core.Maybe Types.VpcConfig
    -- ^ A 'VpcConfig' object that specifies the VPC that this model has access to. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud> 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeModelResponse' value with any optional fields omitted.
mkDescribeModelResponse
    :: Types.ModelName -- ^ 'modelName'
    -> Types.ExecutionRoleArn -- ^ 'executionRoleArn'
    -> Core.NominalDiffTime -- ^ 'creationTime'
    -> Types.ModelArn -- ^ 'modelArn'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeModelResponse
mkDescribeModelResponse modelName executionRoleArn creationTime
  modelArn responseStatus
  = DescribeModelResponse'{modelName, executionRoleArn, creationTime,
                           modelArn, containers = Core.Nothing,
                           enableNetworkIsolation = Core.Nothing,
                           primaryContainer = Core.Nothing, vpcConfig = Core.Nothing,
                           responseStatus}

-- | Name of the Amazon SageMaker model.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsModelName :: Lens.Lens' DescribeModelResponse Types.ModelName
dmrrsModelName = Lens.field @"modelName"
{-# INLINEABLE dmrrsModelName #-}
{-# DEPRECATED modelName "Use generic-lens or generic-optics with 'modelName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the IAM role that you specified for the model.
--
-- /Note:/ Consider using 'executionRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsExecutionRoleArn :: Lens.Lens' DescribeModelResponse Types.ExecutionRoleArn
dmrrsExecutionRoleArn = Lens.field @"executionRoleArn"
{-# INLINEABLE dmrrsExecutionRoleArn #-}
{-# DEPRECATED executionRoleArn "Use generic-lens or generic-optics with 'executionRoleArn' instead"  #-}

-- | A timestamp that shows when the model was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsCreationTime :: Lens.Lens' DescribeModelResponse Core.NominalDiffTime
dmrrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE dmrrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The Amazon Resource Name (ARN) of the model.
--
-- /Note:/ Consider using 'modelArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsModelArn :: Lens.Lens' DescribeModelResponse Types.ModelArn
dmrrsModelArn = Lens.field @"modelArn"
{-# INLINEABLE dmrrsModelArn #-}
{-# DEPRECATED modelArn "Use generic-lens or generic-optics with 'modelArn' instead"  #-}

-- | The containers in the inference pipeline.
--
-- /Note:/ Consider using 'containers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsContainers :: Lens.Lens' DescribeModelResponse (Core.Maybe [Types.ContainerDefinition])
dmrrsContainers = Lens.field @"containers"
{-# INLINEABLE dmrrsContainers #-}
{-# DEPRECATED containers "Use generic-lens or generic-optics with 'containers' instead"  #-}

-- | If @True@ , no inbound or outbound network calls can be made to or from the model container.
--
-- /Note:/ Consider using 'enableNetworkIsolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsEnableNetworkIsolation :: Lens.Lens' DescribeModelResponse (Core.Maybe Core.Bool)
dmrrsEnableNetworkIsolation = Lens.field @"enableNetworkIsolation"
{-# INLINEABLE dmrrsEnableNetworkIsolation #-}
{-# DEPRECATED enableNetworkIsolation "Use generic-lens or generic-optics with 'enableNetworkIsolation' instead"  #-}

-- | The location of the primary inference code, associated artifacts, and custom environment map that the inference code uses when it is deployed in production. 
--
-- /Note:/ Consider using 'primaryContainer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsPrimaryContainer :: Lens.Lens' DescribeModelResponse (Core.Maybe Types.ContainerDefinition)
dmrrsPrimaryContainer = Lens.field @"primaryContainer"
{-# INLINEABLE dmrrsPrimaryContainer #-}
{-# DEPRECATED primaryContainer "Use generic-lens or generic-optics with 'primaryContainer' instead"  #-}

-- | A 'VpcConfig' object that specifies the VPC that this model has access to. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud> 
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsVpcConfig :: Lens.Lens' DescribeModelResponse (Core.Maybe Types.VpcConfig)
dmrrsVpcConfig = Lens.field @"vpcConfig"
{-# INLINEABLE dmrrsVpcConfig #-}
{-# DEPRECATED vpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsResponseStatus :: Lens.Lens' DescribeModelResponse Core.Int
dmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
