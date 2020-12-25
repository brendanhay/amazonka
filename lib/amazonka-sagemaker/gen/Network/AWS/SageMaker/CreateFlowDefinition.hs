{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateFlowDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a flow definition.
module Network.AWS.SageMaker.CreateFlowDefinition
  ( -- * Creating a request
    CreateFlowDefinition (..),
    mkCreateFlowDefinition,

    -- ** Request lenses
    cfdFlowDefinitionName,
    cfdHumanLoopConfig,
    cfdOutputConfig,
    cfdRoleArn,
    cfdHumanLoopActivationConfig,
    cfdHumanLoopRequestSource,
    cfdTags,

    -- * Destructuring the response
    CreateFlowDefinitionResponse (..),
    mkCreateFlowDefinitionResponse,

    -- ** Response lenses
    cfdrrsFlowDefinitionArn,
    cfdrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateFlowDefinition' smart constructor.
data CreateFlowDefinition = CreateFlowDefinition'
  { -- | The name of your flow definition.
    flowDefinitionName :: Types.FlowDefinitionName,
    -- | An object containing information about the tasks the human reviewers will perform.
    humanLoopConfig :: Types.HumanLoopConfig,
    -- | An object containing information about where the human review results will be uploaded.
    outputConfig :: Types.FlowDefinitionOutputConfig,
    -- | The Amazon Resource Name (ARN) of the role needed to call other services on your behalf. For example, @arn:aws:iam::1234567890:role/service-role/AmazonSageMaker-ExecutionRole-20180111T151298@ .
    roleArn :: Types.RoleArn,
    -- | An object containing information about the events that trigger a human workflow.
    humanLoopActivationConfig :: Core.Maybe Types.HumanLoopActivationConfig,
    -- | Container for configuring the source of human task requests. Use to specify if Amazon Rekognition or Amazon Textract is used as an integration source.
    humanLoopRequestSource :: Core.Maybe Types.HumanLoopRequestSource,
    -- | An array of key-value pairs that contain metadata to help you categorize and organize a flow definition. Each tag consists of a key and a value, both of which you define.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFlowDefinition' value with any optional fields omitted.
mkCreateFlowDefinition ::
  -- | 'flowDefinitionName'
  Types.FlowDefinitionName ->
  -- | 'humanLoopConfig'
  Types.HumanLoopConfig ->
  -- | 'outputConfig'
  Types.FlowDefinitionOutputConfig ->
  -- | 'roleArn'
  Types.RoleArn ->
  CreateFlowDefinition
mkCreateFlowDefinition
  flowDefinitionName
  humanLoopConfig
  outputConfig
  roleArn =
    CreateFlowDefinition'
      { flowDefinitionName,
        humanLoopConfig,
        outputConfig,
        roleArn,
        humanLoopActivationConfig = Core.Nothing,
        humanLoopRequestSource = Core.Nothing,
        tags = Core.Nothing
      }

-- | The name of your flow definition.
--
-- /Note:/ Consider using 'flowDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdFlowDefinitionName :: Lens.Lens' CreateFlowDefinition Types.FlowDefinitionName
cfdFlowDefinitionName = Lens.field @"flowDefinitionName"
{-# DEPRECATED cfdFlowDefinitionName "Use generic-lens or generic-optics with 'flowDefinitionName' instead." #-}

-- | An object containing information about the tasks the human reviewers will perform.
--
-- /Note:/ Consider using 'humanLoopConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdHumanLoopConfig :: Lens.Lens' CreateFlowDefinition Types.HumanLoopConfig
cfdHumanLoopConfig = Lens.field @"humanLoopConfig"
{-# DEPRECATED cfdHumanLoopConfig "Use generic-lens or generic-optics with 'humanLoopConfig' instead." #-}

-- | An object containing information about where the human review results will be uploaded.
--
-- /Note:/ Consider using 'outputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdOutputConfig :: Lens.Lens' CreateFlowDefinition Types.FlowDefinitionOutputConfig
cfdOutputConfig = Lens.field @"outputConfig"
{-# DEPRECATED cfdOutputConfig "Use generic-lens or generic-optics with 'outputConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the role needed to call other services on your behalf. For example, @arn:aws:iam::1234567890:role/service-role/AmazonSageMaker-ExecutionRole-20180111T151298@ .
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdRoleArn :: Lens.Lens' CreateFlowDefinition Types.RoleArn
cfdRoleArn = Lens.field @"roleArn"
{-# DEPRECATED cfdRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | An object containing information about the events that trigger a human workflow.
--
-- /Note:/ Consider using 'humanLoopActivationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdHumanLoopActivationConfig :: Lens.Lens' CreateFlowDefinition (Core.Maybe Types.HumanLoopActivationConfig)
cfdHumanLoopActivationConfig = Lens.field @"humanLoopActivationConfig"
{-# DEPRECATED cfdHumanLoopActivationConfig "Use generic-lens or generic-optics with 'humanLoopActivationConfig' instead." #-}

-- | Container for configuring the source of human task requests. Use to specify if Amazon Rekognition or Amazon Textract is used as an integration source.
--
-- /Note:/ Consider using 'humanLoopRequestSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdHumanLoopRequestSource :: Lens.Lens' CreateFlowDefinition (Core.Maybe Types.HumanLoopRequestSource)
cfdHumanLoopRequestSource = Lens.field @"humanLoopRequestSource"
{-# DEPRECATED cfdHumanLoopRequestSource "Use generic-lens or generic-optics with 'humanLoopRequestSource' instead." #-}

-- | An array of key-value pairs that contain metadata to help you categorize and organize a flow definition. Each tag consists of a key and a value, both of which you define.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdTags :: Lens.Lens' CreateFlowDefinition (Core.Maybe [Types.Tag])
cfdTags = Lens.field @"tags"
{-# DEPRECATED cfdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateFlowDefinition where
  toJSON CreateFlowDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("FlowDefinitionName" Core..= flowDefinitionName),
            Core.Just ("HumanLoopConfig" Core..= humanLoopConfig),
            Core.Just ("OutputConfig" Core..= outputConfig),
            Core.Just ("RoleArn" Core..= roleArn),
            ("HumanLoopActivationConfig" Core..=)
              Core.<$> humanLoopActivationConfig,
            ("HumanLoopRequestSource" Core..=) Core.<$> humanLoopRequestSource,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateFlowDefinition where
  type Rs CreateFlowDefinition = CreateFlowDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.CreateFlowDefinition")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFlowDefinitionResponse'
            Core.<$> (x Core..: "FlowDefinitionArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateFlowDefinitionResponse' smart constructor.
data CreateFlowDefinitionResponse = CreateFlowDefinitionResponse'
  { -- | The Amazon Resource Name (ARN) of the flow definition you create.
    flowDefinitionArn :: Types.FlowDefinitionArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFlowDefinitionResponse' value with any optional fields omitted.
mkCreateFlowDefinitionResponse ::
  -- | 'flowDefinitionArn'
  Types.FlowDefinitionArn ->
  -- | 'responseStatus'
  Core.Int ->
  CreateFlowDefinitionResponse
mkCreateFlowDefinitionResponse flowDefinitionArn responseStatus =
  CreateFlowDefinitionResponse' {flowDefinitionArn, responseStatus}

-- | The Amazon Resource Name (ARN) of the flow definition you create.
--
-- /Note:/ Consider using 'flowDefinitionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrrsFlowDefinitionArn :: Lens.Lens' CreateFlowDefinitionResponse Types.FlowDefinitionArn
cfdrrsFlowDefinitionArn = Lens.field @"flowDefinitionArn"
{-# DEPRECATED cfdrrsFlowDefinitionArn "Use generic-lens or generic-optics with 'flowDefinitionArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrrsResponseStatus :: Lens.Lens' CreateFlowDefinitionResponse Core.Int
cfdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cfdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
