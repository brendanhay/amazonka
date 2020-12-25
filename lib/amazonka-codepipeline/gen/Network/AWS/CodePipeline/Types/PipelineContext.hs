{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.PipelineContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.PipelineContext
  ( PipelineContext (..),

    -- * Smart constructor
    mkPipelineContext,

    -- * Lenses
    pcAction,
    pcPipelineArn,
    pcPipelineExecutionId,
    pcPipelineName,
    pcStage,
  )
where

import qualified Network.AWS.CodePipeline.Types.ActionContext as Types
import qualified Network.AWS.CodePipeline.Types.PipelineArn as Types
import qualified Network.AWS.CodePipeline.Types.PipelineExecutionId as Types
import qualified Network.AWS.CodePipeline.Types.PipelineName as Types
import qualified Network.AWS.CodePipeline.Types.StageContext as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about a pipeline to a job worker.
--
-- /See:/ 'mkPipelineContext' smart constructor.
data PipelineContext = PipelineContext'
  { -- | The context of an action to a job worker in the stage of a pipeline.
    action :: Core.Maybe Types.ActionContext,
    -- | The Amazon Resource Name (ARN) of the pipeline.
    pipelineArn :: Core.Maybe Types.PipelineArn,
    -- | The execution ID of the pipeline.
    pipelineExecutionId :: Core.Maybe Types.PipelineExecutionId,
    -- | The name of the pipeline. This is a user-specified value. Pipeline names must be unique across all pipeline names under an Amazon Web Services account.
    pipelineName :: Core.Maybe Types.PipelineName,
    -- | The stage of the pipeline.
    stage :: Core.Maybe Types.StageContext
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PipelineContext' value with any optional fields omitted.
mkPipelineContext ::
  PipelineContext
mkPipelineContext =
  PipelineContext'
    { action = Core.Nothing,
      pipelineArn = Core.Nothing,
      pipelineExecutionId = Core.Nothing,
      pipelineName = Core.Nothing,
      stage = Core.Nothing
    }

-- | The context of an action to a job worker in the stage of a pipeline.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcAction :: Lens.Lens' PipelineContext (Core.Maybe Types.ActionContext)
pcAction = Lens.field @"action"
{-# DEPRECATED pcAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The Amazon Resource Name (ARN) of the pipeline.
--
-- /Note:/ Consider using 'pipelineArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcPipelineArn :: Lens.Lens' PipelineContext (Core.Maybe Types.PipelineArn)
pcPipelineArn = Lens.field @"pipelineArn"
{-# DEPRECATED pcPipelineArn "Use generic-lens or generic-optics with 'pipelineArn' instead." #-}

-- | The execution ID of the pipeline.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcPipelineExecutionId :: Lens.Lens' PipelineContext (Core.Maybe Types.PipelineExecutionId)
pcPipelineExecutionId = Lens.field @"pipelineExecutionId"
{-# DEPRECATED pcPipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

-- | The name of the pipeline. This is a user-specified value. Pipeline names must be unique across all pipeline names under an Amazon Web Services account.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcPipelineName :: Lens.Lens' PipelineContext (Core.Maybe Types.PipelineName)
pcPipelineName = Lens.field @"pipelineName"
{-# DEPRECATED pcPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The stage of the pipeline.
--
-- /Note:/ Consider using 'stage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcStage :: Lens.Lens' PipelineContext (Core.Maybe Types.StageContext)
pcStage = Lens.field @"stage"
{-# DEPRECATED pcStage "Use generic-lens or generic-optics with 'stage' instead." #-}

instance Core.FromJSON PipelineContext where
  parseJSON =
    Core.withObject "PipelineContext" Core.$
      \x ->
        PipelineContext'
          Core.<$> (x Core..:? "action")
          Core.<*> (x Core..:? "pipelineArn")
          Core.<*> (x Core..:? "pipelineExecutionId")
          Core.<*> (x Core..:? "pipelineName")
          Core.<*> (x Core..:? "stage")
