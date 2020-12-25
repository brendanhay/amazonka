{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.NotebookExecutionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.NotebookExecutionSummary
  ( NotebookExecutionSummary (..),

    -- * Smart constructor
    mkNotebookExecutionSummary,

    -- * Lenses
    nesEditorId,
    nesEndTime,
    nesNotebookExecutionId,
    nesNotebookExecutionName,
    nesStartTime,
    nesStatus,
  )
where

import qualified Network.AWS.EMR.Types.NotebookExecutionStatus as Types
import qualified Network.AWS.EMR.Types.XmlStringMaxLen256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- |
--
-- /See:/ 'mkNotebookExecutionSummary' smart constructor.
data NotebookExecutionSummary = NotebookExecutionSummary'
  { -- | The unique identifier of the editor associated with the notebook execution.
    editorId :: Core.Maybe Types.XmlStringMaxLen256,
    -- | The timestamp when notebook execution started.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The unique identifier of the notebook execution.
    notebookExecutionId :: Core.Maybe Types.XmlStringMaxLen256,
    -- | The name of the notebook execution.
    notebookExecutionName :: Core.Maybe Types.XmlStringMaxLen256,
    -- | The timestamp when notebook execution started.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | The status of the notebook execution.
    --
    --
    --     * @START_PENDING@ indicates that the cluster has received the execution request but execution has not begun.
    --
    --
    --     * @STARTING@ indicates that the execution is starting on the cluster.
    --
    --
    --     * @RUNNING@ indicates that the execution is being processed by the cluster.
    --
    --
    --     * @FINISHING@ indicates that execution processing is in the final stages.
    --
    --
    --     * @FINISHED@ indicates that the execution has completed without error.
    --
    --
    --     * @FAILING@ indicates that the execution is failing and will not finish successfully.
    --
    --
    --     * @FAILED@ indicates that the execution failed.
    --
    --
    --     * @STOP_PENDING@ indicates that the cluster has received a @StopNotebookExecution@ request and the stop is pending.
    --
    --
    --     * @STOPPING@ indicates that the cluster is in the process of stopping the execution as a result of a @StopNotebookExecution@ request.
    --
    --
    --     * @STOPPED@ indicates that the execution stopped because of a @StopNotebookExecution@ request.
    status :: Core.Maybe Types.NotebookExecutionStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'NotebookExecutionSummary' value with any optional fields omitted.
mkNotebookExecutionSummary ::
  NotebookExecutionSummary
mkNotebookExecutionSummary =
  NotebookExecutionSummary'
    { editorId = Core.Nothing,
      endTime = Core.Nothing,
      notebookExecutionId = Core.Nothing,
      notebookExecutionName = Core.Nothing,
      startTime = Core.Nothing,
      status = Core.Nothing
    }

-- | The unique identifier of the editor associated with the notebook execution.
--
-- /Note:/ Consider using 'editorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nesEditorId :: Lens.Lens' NotebookExecutionSummary (Core.Maybe Types.XmlStringMaxLen256)
nesEditorId = Lens.field @"editorId"
{-# DEPRECATED nesEditorId "Use generic-lens or generic-optics with 'editorId' instead." #-}

-- | The timestamp when notebook execution started.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nesEndTime :: Lens.Lens' NotebookExecutionSummary (Core.Maybe Core.NominalDiffTime)
nesEndTime = Lens.field @"endTime"
{-# DEPRECATED nesEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The unique identifier of the notebook execution.
--
-- /Note:/ Consider using 'notebookExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nesNotebookExecutionId :: Lens.Lens' NotebookExecutionSummary (Core.Maybe Types.XmlStringMaxLen256)
nesNotebookExecutionId = Lens.field @"notebookExecutionId"
{-# DEPRECATED nesNotebookExecutionId "Use generic-lens or generic-optics with 'notebookExecutionId' instead." #-}

-- | The name of the notebook execution.
--
-- /Note:/ Consider using 'notebookExecutionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nesNotebookExecutionName :: Lens.Lens' NotebookExecutionSummary (Core.Maybe Types.XmlStringMaxLen256)
nesNotebookExecutionName = Lens.field @"notebookExecutionName"
{-# DEPRECATED nesNotebookExecutionName "Use generic-lens or generic-optics with 'notebookExecutionName' instead." #-}

-- | The timestamp when notebook execution started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nesStartTime :: Lens.Lens' NotebookExecutionSummary (Core.Maybe Core.NominalDiffTime)
nesStartTime = Lens.field @"startTime"
{-# DEPRECATED nesStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The status of the notebook execution.
--
--
--     * @START_PENDING@ indicates that the cluster has received the execution request but execution has not begun.
--
--
--     * @STARTING@ indicates that the execution is starting on the cluster.
--
--
--     * @RUNNING@ indicates that the execution is being processed by the cluster.
--
--
--     * @FINISHING@ indicates that execution processing is in the final stages.
--
--
--     * @FINISHED@ indicates that the execution has completed without error.
--
--
--     * @FAILING@ indicates that the execution is failing and will not finish successfully.
--
--
--     * @FAILED@ indicates that the execution failed.
--
--
--     * @STOP_PENDING@ indicates that the cluster has received a @StopNotebookExecution@ request and the stop is pending.
--
--
--     * @STOPPING@ indicates that the cluster is in the process of stopping the execution as a result of a @StopNotebookExecution@ request.
--
--
--     * @STOPPED@ indicates that the execution stopped because of a @StopNotebookExecution@ request.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nesStatus :: Lens.Lens' NotebookExecutionSummary (Core.Maybe Types.NotebookExecutionStatus)
nesStatus = Lens.field @"status"
{-# DEPRECATED nesStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON NotebookExecutionSummary where
  parseJSON =
    Core.withObject "NotebookExecutionSummary" Core.$
      \x ->
        NotebookExecutionSummary'
          Core.<$> (x Core..:? "EditorId")
          Core.<*> (x Core..:? "EndTime")
          Core.<*> (x Core..:? "NotebookExecutionId")
          Core.<*> (x Core..:? "NotebookExecutionName")
          Core.<*> (x Core..:? "StartTime")
          Core.<*> (x Core..:? "Status")
