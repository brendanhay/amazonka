{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.PipelineExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.PipelineExecution
  ( PipelineExecution (..),

    -- * Smart constructor
    mkPipelineExecution,

    -- * Lenses
    peStatus,
    pePipelineName,
    pePipelineVersion,
    pePipelineExecutionId,
    peArtifactRevisions,
  )
where

import Network.AWS.CodePipeline.Types.ArtifactRevision
import Network.AWS.CodePipeline.Types.PipelineExecutionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about an execution of a pipeline.
--
-- /See:/ 'mkPipelineExecution' smart constructor.
data PipelineExecution = PipelineExecution'
  { -- | The status of the pipeline execution.
    --
    --
    --     * InProgress: The pipeline execution is currently running.
    --
    --
    --     * Stopped: The pipeline execution was manually stopped. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-executions-stopped Stopped Executions> .
    --
    --
    --     * Stopping: The pipeline execution received a request to be manually stopped. Depending on the selected stop mode, the execution is either completing or abandoning in-progress actions. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-executions-stopped Stopped Executions> .
    --
    --
    --     * Succeeded: The pipeline execution was completed successfully.
    --
    --
    --     * Superseded: While this pipeline execution was waiting for the next stage to be completed, a newer pipeline execution advanced and continued through the pipeline instead. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-superseded Superseded Executions> .
    --
    --
    --     * Failed: The pipeline execution was not completed successfully.
    status :: Lude.Maybe PipelineExecutionStatus,
    -- | The name of the pipeline with the specified pipeline execution.
    pipelineName :: Lude.Maybe Lude.Text,
    -- | The version number of the pipeline with the specified pipeline execution.
    pipelineVersion :: Lude.Maybe Lude.Natural,
    -- | The ID of the pipeline execution.
    pipelineExecutionId :: Lude.Maybe Lude.Text,
    -- | A list of @ArtifactRevision@ objects included in a pipeline execution.
    artifactRevisions :: Lude.Maybe [ArtifactRevision]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PipelineExecution' with the minimum fields required to make a request.
--
-- * 'status' - The status of the pipeline execution.
--
--
--     * InProgress: The pipeline execution is currently running.
--
--
--     * Stopped: The pipeline execution was manually stopped. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-executions-stopped Stopped Executions> .
--
--
--     * Stopping: The pipeline execution received a request to be manually stopped. Depending on the selected stop mode, the execution is either completing or abandoning in-progress actions. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-executions-stopped Stopped Executions> .
--
--
--     * Succeeded: The pipeline execution was completed successfully.
--
--
--     * Superseded: While this pipeline execution was waiting for the next stage to be completed, a newer pipeline execution advanced and continued through the pipeline instead. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-superseded Superseded Executions> .
--
--
--     * Failed: The pipeline execution was not completed successfully.
--
--
-- * 'pipelineName' - The name of the pipeline with the specified pipeline execution.
-- * 'pipelineVersion' - The version number of the pipeline with the specified pipeline execution.
-- * 'pipelineExecutionId' - The ID of the pipeline execution.
-- * 'artifactRevisions' - A list of @ArtifactRevision@ objects included in a pipeline execution.
mkPipelineExecution ::
  PipelineExecution
mkPipelineExecution =
  PipelineExecution'
    { status = Lude.Nothing,
      pipelineName = Lude.Nothing,
      pipelineVersion = Lude.Nothing,
      pipelineExecutionId = Lude.Nothing,
      artifactRevisions = Lude.Nothing
    }

-- | The status of the pipeline execution.
--
--
--     * InProgress: The pipeline execution is currently running.
--
--
--     * Stopped: The pipeline execution was manually stopped. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-executions-stopped Stopped Executions> .
--
--
--     * Stopping: The pipeline execution received a request to be manually stopped. Depending on the selected stop mode, the execution is either completing or abandoning in-progress actions. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-executions-stopped Stopped Executions> .
--
--
--     * Succeeded: The pipeline execution was completed successfully.
--
--
--     * Superseded: While this pipeline execution was waiting for the next stage to be completed, a newer pipeline execution advanced and continued through the pipeline instead. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-superseded Superseded Executions> .
--
--
--     * Failed: The pipeline execution was not completed successfully.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peStatus :: Lens.Lens' PipelineExecution (Lude.Maybe PipelineExecutionStatus)
peStatus = Lens.lens (status :: PipelineExecution -> Lude.Maybe PipelineExecutionStatus) (\s a -> s {status = a} :: PipelineExecution)
{-# DEPRECATED peStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the pipeline with the specified pipeline execution.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pePipelineName :: Lens.Lens' PipelineExecution (Lude.Maybe Lude.Text)
pePipelineName = Lens.lens (pipelineName :: PipelineExecution -> Lude.Maybe Lude.Text) (\s a -> s {pipelineName = a} :: PipelineExecution)
{-# DEPRECATED pePipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The version number of the pipeline with the specified pipeline execution.
--
-- /Note:/ Consider using 'pipelineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pePipelineVersion :: Lens.Lens' PipelineExecution (Lude.Maybe Lude.Natural)
pePipelineVersion = Lens.lens (pipelineVersion :: PipelineExecution -> Lude.Maybe Lude.Natural) (\s a -> s {pipelineVersion = a} :: PipelineExecution)
{-# DEPRECATED pePipelineVersion "Use generic-lens or generic-optics with 'pipelineVersion' instead." #-}

-- | The ID of the pipeline execution.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pePipelineExecutionId :: Lens.Lens' PipelineExecution (Lude.Maybe Lude.Text)
pePipelineExecutionId = Lens.lens (pipelineExecutionId :: PipelineExecution -> Lude.Maybe Lude.Text) (\s a -> s {pipelineExecutionId = a} :: PipelineExecution)
{-# DEPRECATED pePipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

-- | A list of @ArtifactRevision@ objects included in a pipeline execution.
--
-- /Note:/ Consider using 'artifactRevisions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peArtifactRevisions :: Lens.Lens' PipelineExecution (Lude.Maybe [ArtifactRevision])
peArtifactRevisions = Lens.lens (artifactRevisions :: PipelineExecution -> Lude.Maybe [ArtifactRevision]) (\s a -> s {artifactRevisions = a} :: PipelineExecution)
{-# DEPRECATED peArtifactRevisions "Use generic-lens or generic-optics with 'artifactRevisions' instead." #-}

instance Lude.FromJSON PipelineExecution where
  parseJSON =
    Lude.withObject
      "PipelineExecution"
      ( \x ->
          PipelineExecution'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "pipelineName")
            Lude.<*> (x Lude..:? "pipelineVersion")
            Lude.<*> (x Lude..:? "pipelineExecutionId")
            Lude.<*> (x Lude..:? "artifactRevisions" Lude..!= Lude.mempty)
      )
