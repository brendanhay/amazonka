{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeFlowDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified flow definition.
module Network.AWS.SageMaker.DescribeFlowDefinition
    (
    -- * Creating a request
      DescribeFlowDefinition (..)
    , mkDescribeFlowDefinition
    -- ** Request lenses
    , dFlowDefinitionName

    -- * Destructuring the response
    , DescribeFlowDefinitionResponse (..)
    , mkDescribeFlowDefinitionResponse
    -- ** Response lenses
    , dfdrfrsFlowDefinitionArn
    , dfdrfrsFlowDefinitionName
    , dfdrfrsFlowDefinitionStatus
    , dfdrfrsCreationTime
    , dfdrfrsHumanLoopConfig
    , dfdrfrsOutputConfig
    , dfdrfrsRoleArn
    , dfdrfrsFailureReason
    , dfdrfrsHumanLoopActivationConfig
    , dfdrfrsHumanLoopRequestSource
    , dfdrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeFlowDefinition' smart constructor.
newtype DescribeFlowDefinition = DescribeFlowDefinition'
  { flowDefinitionName :: Types.FlowDefinitionName
    -- ^ The name of the flow definition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeFlowDefinition' value with any optional fields omitted.
mkDescribeFlowDefinition
    :: Types.FlowDefinitionName -- ^ 'flowDefinitionName'
    -> DescribeFlowDefinition
mkDescribeFlowDefinition flowDefinitionName
  = DescribeFlowDefinition'{flowDefinitionName}

-- | The name of the flow definition.
--
-- /Note:/ Consider using 'flowDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFlowDefinitionName :: Lens.Lens' DescribeFlowDefinition Types.FlowDefinitionName
dFlowDefinitionName = Lens.field @"flowDefinitionName"
{-# INLINEABLE dFlowDefinitionName #-}
{-# DEPRECATED flowDefinitionName "Use generic-lens or generic-optics with 'flowDefinitionName' instead"  #-}

instance Core.ToQuery DescribeFlowDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeFlowDefinition where
        toHeaders DescribeFlowDefinition{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DescribeFlowDefinition")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeFlowDefinition where
        toJSON DescribeFlowDefinition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("FlowDefinitionName" Core..= flowDefinitionName)])

instance Core.AWSRequest DescribeFlowDefinition where
        type Rs DescribeFlowDefinition = DescribeFlowDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeFlowDefinitionResponse' Core.<$>
                   (x Core..: "FlowDefinitionArn") Core.<*>
                     x Core..: "FlowDefinitionName"
                     Core.<*> x Core..: "FlowDefinitionStatus"
                     Core.<*> x Core..: "CreationTime"
                     Core.<*> x Core..: "HumanLoopConfig"
                     Core.<*> x Core..: "OutputConfig"
                     Core.<*> x Core..: "RoleArn"
                     Core.<*> x Core..:? "FailureReason"
                     Core.<*> x Core..:? "HumanLoopActivationConfig"
                     Core.<*> x Core..:? "HumanLoopRequestSource"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeFlowDefinitionResponse' smart constructor.
data DescribeFlowDefinitionResponse = DescribeFlowDefinitionResponse'
  { flowDefinitionArn :: Types.FlowDefinitionArn
    -- ^ The Amazon Resource Name (ARN) of the flow defintion.
  , flowDefinitionName :: Types.FlowDefinitionName
    -- ^ The Amazon Resource Name (ARN) of the flow definition.
  , flowDefinitionStatus :: Types.FlowDefinitionStatus
    -- ^ The status of the flow definition. Valid values are listed below.
  , creationTime :: Core.NominalDiffTime
    -- ^ The timestamp when the flow definition was created.
  , humanLoopConfig :: Types.HumanLoopConfig
    -- ^ An object containing information about who works on the task, the workforce task price, and other task details.
  , outputConfig :: Types.FlowDefinitionOutputConfig
    -- ^ An object containing information about the output file.
  , roleArn :: Types.RoleArn
    -- ^ The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) execution role for the flow definition.
  , failureReason :: Core.Maybe Types.FailureReason
    -- ^ The reason your flow definition failed.
  , humanLoopActivationConfig :: Core.Maybe Types.HumanLoopActivationConfig
    -- ^ An object containing information about what triggers a human review workflow.
  , humanLoopRequestSource :: Core.Maybe Types.HumanLoopRequestSource
    -- ^ Container for configuring the source of human task requests. Used to specify if Amazon Rekognition or Amazon Textract is used as an integration source.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeFlowDefinitionResponse' value with any optional fields omitted.
mkDescribeFlowDefinitionResponse
    :: Types.FlowDefinitionArn -- ^ 'flowDefinitionArn'
    -> Types.FlowDefinitionName -- ^ 'flowDefinitionName'
    -> Types.FlowDefinitionStatus -- ^ 'flowDefinitionStatus'
    -> Core.NominalDiffTime -- ^ 'creationTime'
    -> Types.HumanLoopConfig -- ^ 'humanLoopConfig'
    -> Types.FlowDefinitionOutputConfig -- ^ 'outputConfig'
    -> Types.RoleArn -- ^ 'roleArn'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeFlowDefinitionResponse
mkDescribeFlowDefinitionResponse flowDefinitionArn
  flowDefinitionName flowDefinitionStatus creationTime
  humanLoopConfig outputConfig roleArn responseStatus
  = DescribeFlowDefinitionResponse'{flowDefinitionArn,
                                    flowDefinitionName, flowDefinitionStatus, creationTime,
                                    humanLoopConfig, outputConfig, roleArn,
                                    failureReason = Core.Nothing,
                                    humanLoopActivationConfig = Core.Nothing,
                                    humanLoopRequestSource = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the flow defintion.
--
-- /Note:/ Consider using 'flowDefinitionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrfrsFlowDefinitionArn :: Lens.Lens' DescribeFlowDefinitionResponse Types.FlowDefinitionArn
dfdrfrsFlowDefinitionArn = Lens.field @"flowDefinitionArn"
{-# INLINEABLE dfdrfrsFlowDefinitionArn #-}
{-# DEPRECATED flowDefinitionArn "Use generic-lens or generic-optics with 'flowDefinitionArn' instead"  #-}

-- | The Amazon Resource Name (ARN) of the flow definition.
--
-- /Note:/ Consider using 'flowDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrfrsFlowDefinitionName :: Lens.Lens' DescribeFlowDefinitionResponse Types.FlowDefinitionName
dfdrfrsFlowDefinitionName = Lens.field @"flowDefinitionName"
{-# INLINEABLE dfdrfrsFlowDefinitionName #-}
{-# DEPRECATED flowDefinitionName "Use generic-lens or generic-optics with 'flowDefinitionName' instead"  #-}

-- | The status of the flow definition. Valid values are listed below.
--
-- /Note:/ Consider using 'flowDefinitionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrfrsFlowDefinitionStatus :: Lens.Lens' DescribeFlowDefinitionResponse Types.FlowDefinitionStatus
dfdrfrsFlowDefinitionStatus = Lens.field @"flowDefinitionStatus"
{-# INLINEABLE dfdrfrsFlowDefinitionStatus #-}
{-# DEPRECATED flowDefinitionStatus "Use generic-lens or generic-optics with 'flowDefinitionStatus' instead"  #-}

-- | The timestamp when the flow definition was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrfrsCreationTime :: Lens.Lens' DescribeFlowDefinitionResponse Core.NominalDiffTime
dfdrfrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE dfdrfrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | An object containing information about who works on the task, the workforce task price, and other task details.
--
-- /Note:/ Consider using 'humanLoopConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrfrsHumanLoopConfig :: Lens.Lens' DescribeFlowDefinitionResponse Types.HumanLoopConfig
dfdrfrsHumanLoopConfig = Lens.field @"humanLoopConfig"
{-# INLINEABLE dfdrfrsHumanLoopConfig #-}
{-# DEPRECATED humanLoopConfig "Use generic-lens or generic-optics with 'humanLoopConfig' instead"  #-}

-- | An object containing information about the output file.
--
-- /Note:/ Consider using 'outputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrfrsOutputConfig :: Lens.Lens' DescribeFlowDefinitionResponse Types.FlowDefinitionOutputConfig
dfdrfrsOutputConfig = Lens.field @"outputConfig"
{-# INLINEABLE dfdrfrsOutputConfig #-}
{-# DEPRECATED outputConfig "Use generic-lens or generic-optics with 'outputConfig' instead"  #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) execution role for the flow definition.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrfrsRoleArn :: Lens.Lens' DescribeFlowDefinitionResponse Types.RoleArn
dfdrfrsRoleArn = Lens.field @"roleArn"
{-# INLINEABLE dfdrfrsRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The reason your flow definition failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrfrsFailureReason :: Lens.Lens' DescribeFlowDefinitionResponse (Core.Maybe Types.FailureReason)
dfdrfrsFailureReason = Lens.field @"failureReason"
{-# INLINEABLE dfdrfrsFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | An object containing information about what triggers a human review workflow.
--
-- /Note:/ Consider using 'humanLoopActivationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrfrsHumanLoopActivationConfig :: Lens.Lens' DescribeFlowDefinitionResponse (Core.Maybe Types.HumanLoopActivationConfig)
dfdrfrsHumanLoopActivationConfig = Lens.field @"humanLoopActivationConfig"
{-# INLINEABLE dfdrfrsHumanLoopActivationConfig #-}
{-# DEPRECATED humanLoopActivationConfig "Use generic-lens or generic-optics with 'humanLoopActivationConfig' instead"  #-}

-- | Container for configuring the source of human task requests. Used to specify if Amazon Rekognition or Amazon Textract is used as an integration source.
--
-- /Note:/ Consider using 'humanLoopRequestSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrfrsHumanLoopRequestSource :: Lens.Lens' DescribeFlowDefinitionResponse (Core.Maybe Types.HumanLoopRequestSource)
dfdrfrsHumanLoopRequestSource = Lens.field @"humanLoopRequestSource"
{-# INLINEABLE dfdrfrsHumanLoopRequestSource #-}
{-# DEPRECATED humanLoopRequestSource "Use generic-lens or generic-optics with 'humanLoopRequestSource' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrfrsResponseStatus :: Lens.Lens' DescribeFlowDefinitionResponse Core.Int
dfdrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dfdrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
