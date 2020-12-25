{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeFlowDefinition (..),
    mkDescribeFlowDefinition,

    -- ** Request lenses
    dFlowDefinitionName,

    -- * Destructuring the response
    DescribeFlowDefinitionResponse (..),
    mkDescribeFlowDefinitionResponse,

    -- ** Response lenses
    dfdrfrsFlowDefinitionArn,
    dfdrfrsFlowDefinitionName,
    dfdrfrsFlowDefinitionStatus,
    dfdrfrsCreationTime,
    dfdrfrsHumanLoopConfig,
    dfdrfrsOutputConfig,
    dfdrfrsRoleArn,
    dfdrfrsFailureReason,
    dfdrfrsHumanLoopActivationConfig,
    dfdrfrsHumanLoopRequestSource,
    dfdrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeFlowDefinition' smart constructor.
newtype DescribeFlowDefinition = DescribeFlowDefinition'
  { -- | The name of the flow definition.
    flowDefinitionName :: Types.FlowDefinitionName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeFlowDefinition' value with any optional fields omitted.
mkDescribeFlowDefinition ::
  -- | 'flowDefinitionName'
  Types.FlowDefinitionName ->
  DescribeFlowDefinition
mkDescribeFlowDefinition flowDefinitionName =
  DescribeFlowDefinition' {flowDefinitionName}

-- | The name of the flow definition.
--
-- /Note:/ Consider using 'flowDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFlowDefinitionName :: Lens.Lens' DescribeFlowDefinition Types.FlowDefinitionName
dFlowDefinitionName = Lens.field @"flowDefinitionName"
{-# DEPRECATED dFlowDefinitionName "Use generic-lens or generic-optics with 'flowDefinitionName' instead." #-}

instance Core.FromJSON DescribeFlowDefinition where
  toJSON DescribeFlowDefinition {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("FlowDefinitionName" Core..= flowDefinitionName)]
      )

instance Core.AWSRequest DescribeFlowDefinition where
  type Rs DescribeFlowDefinition = DescribeFlowDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DescribeFlowDefinition")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFlowDefinitionResponse'
            Core.<$> (x Core..: "FlowDefinitionArn")
            Core.<*> (x Core..: "FlowDefinitionName")
            Core.<*> (x Core..: "FlowDefinitionStatus")
            Core.<*> (x Core..: "CreationTime")
            Core.<*> (x Core..: "HumanLoopConfig")
            Core.<*> (x Core..: "OutputConfig")
            Core.<*> (x Core..: "RoleArn")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "HumanLoopActivationConfig")
            Core.<*> (x Core..:? "HumanLoopRequestSource")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeFlowDefinitionResponse' smart constructor.
data DescribeFlowDefinitionResponse = DescribeFlowDefinitionResponse'
  { -- | The Amazon Resource Name (ARN) of the flow defintion.
    flowDefinitionArn :: Types.FlowDefinitionArn,
    -- | The Amazon Resource Name (ARN) of the flow definition.
    flowDefinitionName :: Types.FlowDefinitionName,
    -- | The status of the flow definition. Valid values are listed below.
    flowDefinitionStatus :: Types.FlowDefinitionStatus,
    -- | The timestamp when the flow definition was created.
    creationTime :: Core.NominalDiffTime,
    -- | An object containing information about who works on the task, the workforce task price, and other task details.
    humanLoopConfig :: Types.HumanLoopConfig,
    -- | An object containing information about the output file.
    outputConfig :: Types.FlowDefinitionOutputConfig,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) execution role for the flow definition.
    roleArn :: Types.RoleArn,
    -- | The reason your flow definition failed.
    failureReason :: Core.Maybe Types.FailureReason,
    -- | An object containing information about what triggers a human review workflow.
    humanLoopActivationConfig :: Core.Maybe Types.HumanLoopActivationConfig,
    -- | Container for configuring the source of human task requests. Used to specify if Amazon Rekognition or Amazon Textract is used as an integration source.
    humanLoopRequestSource :: Core.Maybe Types.HumanLoopRequestSource,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeFlowDefinitionResponse' value with any optional fields omitted.
mkDescribeFlowDefinitionResponse ::
  -- | 'flowDefinitionArn'
  Types.FlowDefinitionArn ->
  -- | 'flowDefinitionName'
  Types.FlowDefinitionName ->
  -- | 'flowDefinitionStatus'
  Types.FlowDefinitionStatus ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  -- | 'humanLoopConfig'
  Types.HumanLoopConfig ->
  -- | 'outputConfig'
  Types.FlowDefinitionOutputConfig ->
  -- | 'roleArn'
  Types.RoleArn ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeFlowDefinitionResponse
mkDescribeFlowDefinitionResponse
  flowDefinitionArn
  flowDefinitionName
  flowDefinitionStatus
  creationTime
  humanLoopConfig
  outputConfig
  roleArn
  responseStatus =
    DescribeFlowDefinitionResponse'
      { flowDefinitionArn,
        flowDefinitionName,
        flowDefinitionStatus,
        creationTime,
        humanLoopConfig,
        outputConfig,
        roleArn,
        failureReason = Core.Nothing,
        humanLoopActivationConfig = Core.Nothing,
        humanLoopRequestSource = Core.Nothing,
        responseStatus
      }

-- | The Amazon Resource Name (ARN) of the flow defintion.
--
-- /Note:/ Consider using 'flowDefinitionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrfrsFlowDefinitionArn :: Lens.Lens' DescribeFlowDefinitionResponse Types.FlowDefinitionArn
dfdrfrsFlowDefinitionArn = Lens.field @"flowDefinitionArn"
{-# DEPRECATED dfdrfrsFlowDefinitionArn "Use generic-lens or generic-optics with 'flowDefinitionArn' instead." #-}

-- | The Amazon Resource Name (ARN) of the flow definition.
--
-- /Note:/ Consider using 'flowDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrfrsFlowDefinitionName :: Lens.Lens' DescribeFlowDefinitionResponse Types.FlowDefinitionName
dfdrfrsFlowDefinitionName = Lens.field @"flowDefinitionName"
{-# DEPRECATED dfdrfrsFlowDefinitionName "Use generic-lens or generic-optics with 'flowDefinitionName' instead." #-}

-- | The status of the flow definition. Valid values are listed below.
--
-- /Note:/ Consider using 'flowDefinitionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrfrsFlowDefinitionStatus :: Lens.Lens' DescribeFlowDefinitionResponse Types.FlowDefinitionStatus
dfdrfrsFlowDefinitionStatus = Lens.field @"flowDefinitionStatus"
{-# DEPRECATED dfdrfrsFlowDefinitionStatus "Use generic-lens or generic-optics with 'flowDefinitionStatus' instead." #-}

-- | The timestamp when the flow definition was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrfrsCreationTime :: Lens.Lens' DescribeFlowDefinitionResponse Core.NominalDiffTime
dfdrfrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED dfdrfrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | An object containing information about who works on the task, the workforce task price, and other task details.
--
-- /Note:/ Consider using 'humanLoopConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrfrsHumanLoopConfig :: Lens.Lens' DescribeFlowDefinitionResponse Types.HumanLoopConfig
dfdrfrsHumanLoopConfig = Lens.field @"humanLoopConfig"
{-# DEPRECATED dfdrfrsHumanLoopConfig "Use generic-lens or generic-optics with 'humanLoopConfig' instead." #-}

-- | An object containing information about the output file.
--
-- /Note:/ Consider using 'outputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrfrsOutputConfig :: Lens.Lens' DescribeFlowDefinitionResponse Types.FlowDefinitionOutputConfig
dfdrfrsOutputConfig = Lens.field @"outputConfig"
{-# DEPRECATED dfdrfrsOutputConfig "Use generic-lens or generic-optics with 'outputConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) execution role for the flow definition.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrfrsRoleArn :: Lens.Lens' DescribeFlowDefinitionResponse Types.RoleArn
dfdrfrsRoleArn = Lens.field @"roleArn"
{-# DEPRECATED dfdrfrsRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The reason your flow definition failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrfrsFailureReason :: Lens.Lens' DescribeFlowDefinitionResponse (Core.Maybe Types.FailureReason)
dfdrfrsFailureReason = Lens.field @"failureReason"
{-# DEPRECATED dfdrfrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | An object containing information about what triggers a human review workflow.
--
-- /Note:/ Consider using 'humanLoopActivationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrfrsHumanLoopActivationConfig :: Lens.Lens' DescribeFlowDefinitionResponse (Core.Maybe Types.HumanLoopActivationConfig)
dfdrfrsHumanLoopActivationConfig = Lens.field @"humanLoopActivationConfig"
{-# DEPRECATED dfdrfrsHumanLoopActivationConfig "Use generic-lens or generic-optics with 'humanLoopActivationConfig' instead." #-}

-- | Container for configuring the source of human task requests. Used to specify if Amazon Rekognition or Amazon Textract is used as an integration source.
--
-- /Note:/ Consider using 'humanLoopRequestSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrfrsHumanLoopRequestSource :: Lens.Lens' DescribeFlowDefinitionResponse (Core.Maybe Types.HumanLoopRequestSource)
dfdrfrsHumanLoopRequestSource = Lens.field @"humanLoopRequestSource"
{-# DEPRECATED dfdrfrsHumanLoopRequestSource "Use generic-lens or generic-optics with 'humanLoopRequestSource' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrfrsResponseStatus :: Lens.Lens' DescribeFlowDefinitionResponse Core.Int
dfdrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dfdrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
