-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.StageExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.StageExecution
  ( StageExecution (..),

    -- * Smart constructor
    mkStageExecution,

    -- * Lenses
    sePipelineExecutionId,
    seStatus,
  )
where

import Network.AWS.CodePipeline.Types.StageExecutionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about the run of a stage.
--
-- /See:/ 'mkStageExecution' smart constructor.
data StageExecution = StageExecution'
  { pipelineExecutionId ::
      Lude.Text,
    status :: StageExecutionStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StageExecution' with the minimum fields required to make a request.
--
-- * 'pipelineExecutionId' - The ID of the pipeline execution associated with the stage.
-- * 'status' - The status of the stage, or for a completed stage, the last status of the stage.
mkStageExecution ::
  -- | 'pipelineExecutionId'
  Lude.Text ->
  -- | 'status'
  StageExecutionStatus ->
  StageExecution
mkStageExecution pPipelineExecutionId_ pStatus_ =
  StageExecution'
    { pipelineExecutionId = pPipelineExecutionId_,
      status = pStatus_
    }

-- | The ID of the pipeline execution associated with the stage.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sePipelineExecutionId :: Lens.Lens' StageExecution Lude.Text
sePipelineExecutionId = Lens.lens (pipelineExecutionId :: StageExecution -> Lude.Text) (\s a -> s {pipelineExecutionId = a} :: StageExecution)
{-# DEPRECATED sePipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

-- | The status of the stage, or for a completed stage, the last status of the stage.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seStatus :: Lens.Lens' StageExecution StageExecutionStatus
seStatus = Lens.lens (status :: StageExecution -> StageExecutionStatus) (\s a -> s {status = a} :: StageExecution)
{-# DEPRECATED seStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromJSON StageExecution where
  parseJSON =
    Lude.withObject
      "StageExecution"
      ( \x ->
          StageExecution'
            Lude.<$> (x Lude..: "pipelineExecutionId") Lude.<*> (x Lude..: "status")
      )
