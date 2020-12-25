{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.StartAutomationExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates execution of an Automation document.
module Network.AWS.SSM.StartAutomationExecution
  ( -- * Creating a request
    StartAutomationExecution (..),
    mkStartAutomationExecution,

    -- ** Request lenses
    saeDocumentName,
    saeClientToken,
    saeDocumentVersion,
    saeMaxConcurrency,
    saeMaxErrors,
    saeMode,
    saeParameters,
    saeTags,
    saeTargetLocations,
    saeTargetMaps,
    saeTargetParameterName,
    saeTargets,

    -- * Destructuring the response
    StartAutomationExecutionResponse (..),
    mkStartAutomationExecutionResponse,

    -- ** Response lenses
    saerrsAutomationExecutionId,
    saerrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkStartAutomationExecution' smart constructor.
data StartAutomationExecution = StartAutomationExecution'
  { -- | The name of the Automation document to use for this execution.
    documentName :: Types.DocumentARN,
    -- | User-provided idempotency token. The token must be unique, is case insensitive, enforces the UUID format, and can't be reused.
    clientToken :: Core.Maybe Types.IdempotencyToken,
    -- | The version of the Automation document to use for this execution.
    documentVersion :: Core.Maybe Types.DocumentVersion,
    -- | The maximum number of targets allowed to run this task in parallel. You can specify a number, such as 10, or a percentage, such as 10%. The default value is 10.
    maxConcurrency :: Core.Maybe Types.MaxConcurrency,
    -- | The number of errors that are allowed before the system stops running the automation on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops running the automation when the fourth error is received. If you specify 0, then the system stops running the automation on additional targets after the first error result is returned. If you run an automation on 50 resources and set max-errors to 10%, then the system stops running the automation on additional targets when the sixth error is received.
    --
    -- Executions that are already running an automation when max-errors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set max-concurrency to 1 so the executions proceed one at a time.
    maxErrors :: Core.Maybe Types.MaxErrors,
    -- | The execution mode of the automation. Valid modes include the following: Auto and Interactive. The default mode is Auto.
    mode :: Core.Maybe Types.ExecutionMode,
    -- | A key-value map of execution parameters, which match the declared parameters in the Automation document.
    parameters :: Core.Maybe (Core.HashMap Types.AutomationParameterKey [Types.AutomationParameterValue]),
    -- | Optional metadata that you assign to a resource. You can specify a maximum of five tags for an automation. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag an automation to identify an environment or operating system. In this case, you could specify the following key name/value pairs:
    --
    --
    --     * @Key=environment,Value=test@
    --
    --
    --     * @Key=OS,Value=Windows@
    tags :: Core.Maybe [Types.Tag],
    -- | A location is a combination of AWS Regions and/or AWS accounts where you want to run the Automation. Use this action to start an Automation in multiple Regions and multiple accounts. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-automation-multiple-accounts-and-regions.html Running Automation workflows in multiple AWS Regions and accounts> in the /AWS Systems Manager User Guide/ .
    targetLocations :: Core.Maybe (Core.NonEmpty Types.TargetLocation),
    -- | A key-value mapping of document parameters to target resources. Both Targets and TargetMaps cannot be specified together.
    targetMaps :: Core.Maybe [Core.HashMap Types.TargetMapKey [Types.TargetMapValue]],
    -- | The name of the parameter used as the target resource for the rate-controlled execution. Required if you specify targets.
    targetParameterName :: Core.Maybe Types.TargetParameterName,
    -- | A key-value mapping to target resources. Required if you specify TargetParameterName.
    targets :: Core.Maybe [Types.Target]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartAutomationExecution' value with any optional fields omitted.
mkStartAutomationExecution ::
  -- | 'documentName'
  Types.DocumentARN ->
  StartAutomationExecution
mkStartAutomationExecution documentName =
  StartAutomationExecution'
    { documentName,
      clientToken = Core.Nothing,
      documentVersion = Core.Nothing,
      maxConcurrency = Core.Nothing,
      maxErrors = Core.Nothing,
      mode = Core.Nothing,
      parameters = Core.Nothing,
      tags = Core.Nothing,
      targetLocations = Core.Nothing,
      targetMaps = Core.Nothing,
      targetParameterName = Core.Nothing,
      targets = Core.Nothing
    }

-- | The name of the Automation document to use for this execution.
--
-- /Note:/ Consider using 'documentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeDocumentName :: Lens.Lens' StartAutomationExecution Types.DocumentARN
saeDocumentName = Lens.field @"documentName"
{-# DEPRECATED saeDocumentName "Use generic-lens or generic-optics with 'documentName' instead." #-}

-- | User-provided idempotency token. The token must be unique, is case insensitive, enforces the UUID format, and can't be reused.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeClientToken :: Lens.Lens' StartAutomationExecution (Core.Maybe Types.IdempotencyToken)
saeClientToken = Lens.field @"clientToken"
{-# DEPRECATED saeClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The version of the Automation document to use for this execution.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeDocumentVersion :: Lens.Lens' StartAutomationExecution (Core.Maybe Types.DocumentVersion)
saeDocumentVersion = Lens.field @"documentVersion"
{-# DEPRECATED saeDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | The maximum number of targets allowed to run this task in parallel. You can specify a number, such as 10, or a percentage, such as 10%. The default value is 10.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeMaxConcurrency :: Lens.Lens' StartAutomationExecution (Core.Maybe Types.MaxConcurrency)
saeMaxConcurrency = Lens.field @"maxConcurrency"
{-# DEPRECATED saeMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

-- | The number of errors that are allowed before the system stops running the automation on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops running the automation when the fourth error is received. If you specify 0, then the system stops running the automation on additional targets after the first error result is returned. If you run an automation on 50 resources and set max-errors to 10%, then the system stops running the automation on additional targets when the sixth error is received.
--
-- Executions that are already running an automation when max-errors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set max-concurrency to 1 so the executions proceed one at a time.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeMaxErrors :: Lens.Lens' StartAutomationExecution (Core.Maybe Types.MaxErrors)
saeMaxErrors = Lens.field @"maxErrors"
{-# DEPRECATED saeMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

-- | The execution mode of the automation. Valid modes include the following: Auto and Interactive. The default mode is Auto.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeMode :: Lens.Lens' StartAutomationExecution (Core.Maybe Types.ExecutionMode)
saeMode = Lens.field @"mode"
{-# DEPRECATED saeMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | A key-value map of execution parameters, which match the declared parameters in the Automation document.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeParameters :: Lens.Lens' StartAutomationExecution (Core.Maybe (Core.HashMap Types.AutomationParameterKey [Types.AutomationParameterValue]))
saeParameters = Lens.field @"parameters"
{-# DEPRECATED saeParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | Optional metadata that you assign to a resource. You can specify a maximum of five tags for an automation. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag an automation to identify an environment or operating system. In this case, you could specify the following key name/value pairs:
--
--
--     * @Key=environment,Value=test@
--
--
--     * @Key=OS,Value=Windows@
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeTags :: Lens.Lens' StartAutomationExecution (Core.Maybe [Types.Tag])
saeTags = Lens.field @"tags"
{-# DEPRECATED saeTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A location is a combination of AWS Regions and/or AWS accounts where you want to run the Automation. Use this action to start an Automation in multiple Regions and multiple accounts. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-automation-multiple-accounts-and-regions.html Running Automation workflows in multiple AWS Regions and accounts> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'targetLocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeTargetLocations :: Lens.Lens' StartAutomationExecution (Core.Maybe (Core.NonEmpty Types.TargetLocation))
saeTargetLocations = Lens.field @"targetLocations"
{-# DEPRECATED saeTargetLocations "Use generic-lens or generic-optics with 'targetLocations' instead." #-}

-- | A key-value mapping of document parameters to target resources. Both Targets and TargetMaps cannot be specified together.
--
-- /Note:/ Consider using 'targetMaps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeTargetMaps :: Lens.Lens' StartAutomationExecution (Core.Maybe [Core.HashMap Types.TargetMapKey [Types.TargetMapValue]])
saeTargetMaps = Lens.field @"targetMaps"
{-# DEPRECATED saeTargetMaps "Use generic-lens or generic-optics with 'targetMaps' instead." #-}

-- | The name of the parameter used as the target resource for the rate-controlled execution. Required if you specify targets.
--
-- /Note:/ Consider using 'targetParameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeTargetParameterName :: Lens.Lens' StartAutomationExecution (Core.Maybe Types.TargetParameterName)
saeTargetParameterName = Lens.field @"targetParameterName"
{-# DEPRECATED saeTargetParameterName "Use generic-lens or generic-optics with 'targetParameterName' instead." #-}

-- | A key-value mapping to target resources. Required if you specify TargetParameterName.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeTargets :: Lens.Lens' StartAutomationExecution (Core.Maybe [Types.Target])
saeTargets = Lens.field @"targets"
{-# DEPRECATED saeTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

instance Core.FromJSON StartAutomationExecution where
  toJSON StartAutomationExecution {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DocumentName" Core..= documentName),
            ("ClientToken" Core..=) Core.<$> clientToken,
            ("DocumentVersion" Core..=) Core.<$> documentVersion,
            ("MaxConcurrency" Core..=) Core.<$> maxConcurrency,
            ("MaxErrors" Core..=) Core.<$> maxErrors,
            ("Mode" Core..=) Core.<$> mode,
            ("Parameters" Core..=) Core.<$> parameters,
            ("Tags" Core..=) Core.<$> tags,
            ("TargetLocations" Core..=) Core.<$> targetLocations,
            ("TargetMaps" Core..=) Core.<$> targetMaps,
            ("TargetParameterName" Core..=) Core.<$> targetParameterName,
            ("Targets" Core..=) Core.<$> targets
          ]
      )

instance Core.AWSRequest StartAutomationExecution where
  type Rs StartAutomationExecution = StartAutomationExecutionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.StartAutomationExecution")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartAutomationExecutionResponse'
            Core.<$> (x Core..:? "AutomationExecutionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartAutomationExecutionResponse' smart constructor.
data StartAutomationExecutionResponse = StartAutomationExecutionResponse'
  { -- | The unique ID of a newly scheduled automation execution.
    automationExecutionId :: Core.Maybe Types.AutomationExecutionId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartAutomationExecutionResponse' value with any optional fields omitted.
mkStartAutomationExecutionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartAutomationExecutionResponse
mkStartAutomationExecutionResponse responseStatus =
  StartAutomationExecutionResponse'
    { automationExecutionId =
        Core.Nothing,
      responseStatus
    }

-- | The unique ID of a newly scheduled automation execution.
--
-- /Note:/ Consider using 'automationExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saerrsAutomationExecutionId :: Lens.Lens' StartAutomationExecutionResponse (Core.Maybe Types.AutomationExecutionId)
saerrsAutomationExecutionId = Lens.field @"automationExecutionId"
{-# DEPRECATED saerrsAutomationExecutionId "Use generic-lens or generic-optics with 'automationExecutionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saerrsResponseStatus :: Lens.Lens' StartAutomationExecutionResponse Core.Int
saerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED saerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
