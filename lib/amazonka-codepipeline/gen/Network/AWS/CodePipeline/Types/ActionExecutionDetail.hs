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
    aedStatus,
    aedStartTime,
    aedPipelineVersion,
    aedInput,
    aedActionName,
    aedOutput,
    aedPipelineExecutionId,
    aedStageName,
    aedLastUpdateTime,
    aedActionExecutionId,
  )
where

import Network.AWS.CodePipeline.Types.ActionExecutionInput
import Network.AWS.CodePipeline.Types.ActionExecutionOutput
import Network.AWS.CodePipeline.Types.ActionExecutionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about an execution of an action, including the action execution ID, and the name, version, and timing of the action.
--
-- /See:/ 'mkActionExecutionDetail' smart constructor.
data ActionExecutionDetail = ActionExecutionDetail'
  { -- | The status of the action execution. Status categories are @InProgress@ , @Succeeded@ , and @Failed@ .
    status :: Lude.Maybe ActionExecutionStatus,
    -- | The start time of the action execution.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | The version of the pipeline where the action was run.
    pipelineVersion :: Lude.Maybe Lude.Natural,
    -- | Input details for the action execution, such as role ARN, Region, and input artifacts.
    input :: Lude.Maybe ActionExecutionInput,
    -- | The name of the action.
    actionName :: Lude.Maybe Lude.Text,
    -- | Output details for the action execution, such as the action execution result.
    output :: Lude.Maybe ActionExecutionOutput,
    -- | The pipeline execution ID for the action execution.
    pipelineExecutionId :: Lude.Maybe Lude.Text,
    -- | The name of the stage that contains the action.
    stageName :: Lude.Maybe Lude.Text,
    -- | The last update time of the action execution.
    lastUpdateTime :: Lude.Maybe Lude.Timestamp,
    -- | The action execution ID.
    actionExecutionId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActionExecutionDetail' with the minimum fields required to make a request.
--
-- * 'status' - The status of the action execution. Status categories are @InProgress@ , @Succeeded@ , and @Failed@ .
-- * 'startTime' - The start time of the action execution.
-- * 'pipelineVersion' - The version of the pipeline where the action was run.
-- * 'input' - Input details for the action execution, such as role ARN, Region, and input artifacts.
-- * 'actionName' - The name of the action.
-- * 'output' - Output details for the action execution, such as the action execution result.
-- * 'pipelineExecutionId' - The pipeline execution ID for the action execution.
-- * 'stageName' - The name of the stage that contains the action.
-- * 'lastUpdateTime' - The last update time of the action execution.
-- * 'actionExecutionId' - The action execution ID.
mkActionExecutionDetail ::
  ActionExecutionDetail
mkActionExecutionDetail =
  ActionExecutionDetail'
    { status = Lude.Nothing,
      startTime = Lude.Nothing,
      pipelineVersion = Lude.Nothing,
      input = Lude.Nothing,
      actionName = Lude.Nothing,
      output = Lude.Nothing,
      pipelineExecutionId = Lude.Nothing,
      stageName = Lude.Nothing,
      lastUpdateTime = Lude.Nothing,
      actionExecutionId = Lude.Nothing
    }

-- | The status of the action execution. Status categories are @InProgress@ , @Succeeded@ , and @Failed@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aedStatus :: Lens.Lens' ActionExecutionDetail (Lude.Maybe ActionExecutionStatus)
aedStatus = Lens.lens (status :: ActionExecutionDetail -> Lude.Maybe ActionExecutionStatus) (\s a -> s {status = a} :: ActionExecutionDetail)
{-# DEPRECATED aedStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The start time of the action execution.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aedStartTime :: Lens.Lens' ActionExecutionDetail (Lude.Maybe Lude.Timestamp)
aedStartTime = Lens.lens (startTime :: ActionExecutionDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: ActionExecutionDetail)
{-# DEPRECATED aedStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The version of the pipeline where the action was run.
--
-- /Note:/ Consider using 'pipelineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aedPipelineVersion :: Lens.Lens' ActionExecutionDetail (Lude.Maybe Lude.Natural)
aedPipelineVersion = Lens.lens (pipelineVersion :: ActionExecutionDetail -> Lude.Maybe Lude.Natural) (\s a -> s {pipelineVersion = a} :: ActionExecutionDetail)
{-# DEPRECATED aedPipelineVersion "Use generic-lens or generic-optics with 'pipelineVersion' instead." #-}

-- | Input details for the action execution, such as role ARN, Region, and input artifacts.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aedInput :: Lens.Lens' ActionExecutionDetail (Lude.Maybe ActionExecutionInput)
aedInput = Lens.lens (input :: ActionExecutionDetail -> Lude.Maybe ActionExecutionInput) (\s a -> s {input = a} :: ActionExecutionDetail)
{-# DEPRECATED aedInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The name of the action.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aedActionName :: Lens.Lens' ActionExecutionDetail (Lude.Maybe Lude.Text)
aedActionName = Lens.lens (actionName :: ActionExecutionDetail -> Lude.Maybe Lude.Text) (\s a -> s {actionName = a} :: ActionExecutionDetail)
{-# DEPRECATED aedActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | Output details for the action execution, such as the action execution result.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aedOutput :: Lens.Lens' ActionExecutionDetail (Lude.Maybe ActionExecutionOutput)
aedOutput = Lens.lens (output :: ActionExecutionDetail -> Lude.Maybe ActionExecutionOutput) (\s a -> s {output = a} :: ActionExecutionDetail)
{-# DEPRECATED aedOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | The pipeline execution ID for the action execution.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aedPipelineExecutionId :: Lens.Lens' ActionExecutionDetail (Lude.Maybe Lude.Text)
aedPipelineExecutionId = Lens.lens (pipelineExecutionId :: ActionExecutionDetail -> Lude.Maybe Lude.Text) (\s a -> s {pipelineExecutionId = a} :: ActionExecutionDetail)
{-# DEPRECATED aedPipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

-- | The name of the stage that contains the action.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aedStageName :: Lens.Lens' ActionExecutionDetail (Lude.Maybe Lude.Text)
aedStageName = Lens.lens (stageName :: ActionExecutionDetail -> Lude.Maybe Lude.Text) (\s a -> s {stageName = a} :: ActionExecutionDetail)
{-# DEPRECATED aedStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

-- | The last update time of the action execution.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aedLastUpdateTime :: Lens.Lens' ActionExecutionDetail (Lude.Maybe Lude.Timestamp)
aedLastUpdateTime = Lens.lens (lastUpdateTime :: ActionExecutionDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTime = a} :: ActionExecutionDetail)
{-# DEPRECATED aedLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

-- | The action execution ID.
--
-- /Note:/ Consider using 'actionExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aedActionExecutionId :: Lens.Lens' ActionExecutionDetail (Lude.Maybe Lude.Text)
aedActionExecutionId = Lens.lens (actionExecutionId :: ActionExecutionDetail -> Lude.Maybe Lude.Text) (\s a -> s {actionExecutionId = a} :: ActionExecutionDetail)
{-# DEPRECATED aedActionExecutionId "Use generic-lens or generic-optics with 'actionExecutionId' instead." #-}

instance Lude.FromJSON ActionExecutionDetail where
  parseJSON =
    Lude.withObject
      "ActionExecutionDetail"
      ( \x ->
          ActionExecutionDetail'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "startTime")
            Lude.<*> (x Lude..:? "pipelineVersion")
            Lude.<*> (x Lude..:? "input")
            Lude.<*> (x Lude..:? "actionName")
            Lude.<*> (x Lude..:? "output")
            Lude.<*> (x Lude..:? "pipelineExecutionId")
            Lude.<*> (x Lude..:? "stageName")
            Lude.<*> (x Lude..:? "lastUpdateTime")
            Lude.<*> (x Lude..:? "actionExecutionId")
      )
