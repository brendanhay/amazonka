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
    pcStage,
    pcPipelineName,
    pcAction,
    pcPipelineARN,
    pcPipelineExecutionId,
  )
where

import Network.AWS.CodePipeline.Types.ActionContext
import Network.AWS.CodePipeline.Types.StageContext
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about a pipeline to a job worker.
--
-- /See:/ 'mkPipelineContext' smart constructor.
data PipelineContext = PipelineContext'
  { stage ::
      Lude.Maybe StageContext,
    pipelineName :: Lude.Maybe Lude.Text,
    action :: Lude.Maybe ActionContext,
    pipelineARN :: Lude.Maybe Lude.Text,
    pipelineExecutionId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PipelineContext' with the minimum fields required to make a request.
--
-- * 'action' - The context of an action to a job worker in the stage of a pipeline.
-- * 'pipelineARN' - The Amazon Resource Name (ARN) of the pipeline.
-- * 'pipelineExecutionId' - The execution ID of the pipeline.
-- * 'pipelineName' - The name of the pipeline. This is a user-specified value. Pipeline names must be unique across all pipeline names under an Amazon Web Services account.
-- * 'stage' - The stage of the pipeline.
mkPipelineContext ::
  PipelineContext
mkPipelineContext =
  PipelineContext'
    { stage = Lude.Nothing,
      pipelineName = Lude.Nothing,
      action = Lude.Nothing,
      pipelineARN = Lude.Nothing,
      pipelineExecutionId = Lude.Nothing
    }

-- | The stage of the pipeline.
--
-- /Note:/ Consider using 'stage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcStage :: Lens.Lens' PipelineContext (Lude.Maybe StageContext)
pcStage = Lens.lens (stage :: PipelineContext -> Lude.Maybe StageContext) (\s a -> s {stage = a} :: PipelineContext)
{-# DEPRECATED pcStage "Use generic-lens or generic-optics with 'stage' instead." #-}

-- | The name of the pipeline. This is a user-specified value. Pipeline names must be unique across all pipeline names under an Amazon Web Services account.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcPipelineName :: Lens.Lens' PipelineContext (Lude.Maybe Lude.Text)
pcPipelineName = Lens.lens (pipelineName :: PipelineContext -> Lude.Maybe Lude.Text) (\s a -> s {pipelineName = a} :: PipelineContext)
{-# DEPRECATED pcPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The context of an action to a job worker in the stage of a pipeline.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcAction :: Lens.Lens' PipelineContext (Lude.Maybe ActionContext)
pcAction = Lens.lens (action :: PipelineContext -> Lude.Maybe ActionContext) (\s a -> s {action = a} :: PipelineContext)
{-# DEPRECATED pcAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The Amazon Resource Name (ARN) of the pipeline.
--
-- /Note:/ Consider using 'pipelineARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcPipelineARN :: Lens.Lens' PipelineContext (Lude.Maybe Lude.Text)
pcPipelineARN = Lens.lens (pipelineARN :: PipelineContext -> Lude.Maybe Lude.Text) (\s a -> s {pipelineARN = a} :: PipelineContext)
{-# DEPRECATED pcPipelineARN "Use generic-lens or generic-optics with 'pipelineARN' instead." #-}

-- | The execution ID of the pipeline.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcPipelineExecutionId :: Lens.Lens' PipelineContext (Lude.Maybe Lude.Text)
pcPipelineExecutionId = Lens.lens (pipelineExecutionId :: PipelineContext -> Lude.Maybe Lude.Text) (\s a -> s {pipelineExecutionId = a} :: PipelineContext)
{-# DEPRECATED pcPipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

instance Lude.FromJSON PipelineContext where
  parseJSON =
    Lude.withObject
      "PipelineContext"
      ( \x ->
          PipelineContext'
            Lude.<$> (x Lude..:? "stage")
            Lude.<*> (x Lude..:? "pipelineName")
            Lude.<*> (x Lude..:? "action")
            Lude.<*> (x Lude..:? "pipelineArn")
            Lude.<*> (x Lude..:? "pipelineExecutionId")
      )
