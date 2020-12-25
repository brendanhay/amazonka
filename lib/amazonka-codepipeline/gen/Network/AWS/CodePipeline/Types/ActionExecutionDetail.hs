{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionExecutionDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionExecutionDetail
  ( ActionExecutionDetail (..),

    -- * Smart constructor
    mkActionExecutionDetail,

    -- * Lenses
    aedActionExecutionId,
    aedActionName,
    aedInput,
    aedLastUpdateTime,
    aedOutput,
    aedPipelineExecutionId,
    aedPipelineVersion,
    aedStageName,
    aedStartTime,
    aedStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types.ActionExecutionId as Types
import qualified Network.AWS.CodePipeline.Types.ActionExecutionInput as Types
import qualified Network.AWS.CodePipeline.Types.ActionExecutionOutput as Types
import qualified Network.AWS.CodePipeline.Types.ActionExecutionStatus as Types
import qualified Network.AWS.CodePipeline.Types.ActionName as Types
import qualified Network.AWS.CodePipeline.Types.PipelineExecutionId as Types
import qualified Network.AWS.CodePipeline.Types.StageName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about an execution of an action, including the action execution ID, and the name, version, and timing of the action.
--
-- /See:/ 'mkActionExecutionDetail' smart constructor.
data ActionExecutionDetail = ActionExecutionDetail'
  { -- | The action execution ID.
    actionExecutionId :: Core.Maybe Types.ActionExecutionId,
    -- | The name of the action.
    actionName :: Core.Maybe Types.ActionName,
    -- | Input details for the action execution, such as role ARN, Region, and input artifacts.
    input :: Core.Maybe Types.ActionExecutionInput,
    -- | The last update time of the action execution.
    lastUpdateTime :: Core.Maybe Core.NominalDiffTime,
    -- | Output details for the action execution, such as the action execution result.
    output :: Core.Maybe Types.ActionExecutionOutput,
    -- | The pipeline execution ID for the action execution.
    pipelineExecutionId :: Core.Maybe Types.PipelineExecutionId,
    -- | The version of the pipeline where the action was run.
    pipelineVersion :: Core.Maybe Core.Natural,
    -- | The name of the stage that contains the action.
    stageName :: Core.Maybe Types.StageName,
    -- | The start time of the action execution.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | The status of the action execution. Status categories are @InProgress@ , @Succeeded@ , and @Failed@ .
    status :: Core.Maybe Types.ActionExecutionStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ActionExecutionDetail' value with any optional fields omitted.
mkActionExecutionDetail ::
  ActionExecutionDetail
mkActionExecutionDetail =
  ActionExecutionDetail'
    { actionExecutionId = Core.Nothing,
      actionName = Core.Nothing,
      input = Core.Nothing,
      lastUpdateTime = Core.Nothing,
      output = Core.Nothing,
      pipelineExecutionId = Core.Nothing,
      pipelineVersion = Core.Nothing,
      stageName = Core.Nothing,
      startTime = Core.Nothing,
      status = Core.Nothing
    }

-- | The action execution ID.
--
-- /Note:/ Consider using 'actionExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aedActionExecutionId :: Lens.Lens' ActionExecutionDetail (Core.Maybe Types.ActionExecutionId)
aedActionExecutionId = Lens.field @"actionExecutionId"
{-# DEPRECATED aedActionExecutionId "Use generic-lens or generic-optics with 'actionExecutionId' instead." #-}

-- | The name of the action.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aedActionName :: Lens.Lens' ActionExecutionDetail (Core.Maybe Types.ActionName)
aedActionName = Lens.field @"actionName"
{-# DEPRECATED aedActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | Input details for the action execution, such as role ARN, Region, and input artifacts.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aedInput :: Lens.Lens' ActionExecutionDetail (Core.Maybe Types.ActionExecutionInput)
aedInput = Lens.field @"input"
{-# DEPRECATED aedInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The last update time of the action execution.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aedLastUpdateTime :: Lens.Lens' ActionExecutionDetail (Core.Maybe Core.NominalDiffTime)
aedLastUpdateTime = Lens.field @"lastUpdateTime"
{-# DEPRECATED aedLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

-- | Output details for the action execution, such as the action execution result.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aedOutput :: Lens.Lens' ActionExecutionDetail (Core.Maybe Types.ActionExecutionOutput)
aedOutput = Lens.field @"output"
{-# DEPRECATED aedOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | The pipeline execution ID for the action execution.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aedPipelineExecutionId :: Lens.Lens' ActionExecutionDetail (Core.Maybe Types.PipelineExecutionId)
aedPipelineExecutionId = Lens.field @"pipelineExecutionId"
{-# DEPRECATED aedPipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

-- | The version of the pipeline where the action was run.
--
-- /Note:/ Consider using 'pipelineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aedPipelineVersion :: Lens.Lens' ActionExecutionDetail (Core.Maybe Core.Natural)
aedPipelineVersion = Lens.field @"pipelineVersion"
{-# DEPRECATED aedPipelineVersion "Use generic-lens or generic-optics with 'pipelineVersion' instead." #-}

-- | The name of the stage that contains the action.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aedStageName :: Lens.Lens' ActionExecutionDetail (Core.Maybe Types.StageName)
aedStageName = Lens.field @"stageName"
{-# DEPRECATED aedStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

-- | The start time of the action execution.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aedStartTime :: Lens.Lens' ActionExecutionDetail (Core.Maybe Core.NominalDiffTime)
aedStartTime = Lens.field @"startTime"
{-# DEPRECATED aedStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The status of the action execution. Status categories are @InProgress@ , @Succeeded@ , and @Failed@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aedStatus :: Lens.Lens' ActionExecutionDetail (Core.Maybe Types.ActionExecutionStatus)
aedStatus = Lens.field @"status"
{-# DEPRECATED aedStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON ActionExecutionDetail where
  parseJSON =
    Core.withObject "ActionExecutionDetail" Core.$
      \x ->
        ActionExecutionDetail'
          Core.<$> (x Core..:? "actionExecutionId")
          Core.<*> (x Core..:? "actionName")
          Core.<*> (x Core..:? "input")
          Core.<*> (x Core..:? "lastUpdateTime")
          Core.<*> (x Core..:? "output")
          Core.<*> (x Core..:? "pipelineExecutionId")
          Core.<*> (x Core..:? "pipelineVersion")
          Core.<*> (x Core..:? "stageName")
          Core.<*> (x Core..:? "startTime")
          Core.<*> (x Core..:? "status")
