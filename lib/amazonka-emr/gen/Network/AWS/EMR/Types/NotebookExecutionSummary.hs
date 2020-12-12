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
    nesStatus,
    nesEditorId,
    nesStartTime,
    nesNotebookExecutionId,
    nesNotebookExecutionName,
    nesEndTime,
  )
where

import Network.AWS.EMR.Types.NotebookExecutionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- |
--
-- /See:/ 'mkNotebookExecutionSummary' smart constructor.
data NotebookExecutionSummary = NotebookExecutionSummary'
  { status ::
      Lude.Maybe NotebookExecutionStatus,
    editorId :: Lude.Maybe Lude.Text,
    startTime :: Lude.Maybe Lude.Timestamp,
    notebookExecutionId ::
      Lude.Maybe Lude.Text,
    notebookExecutionName ::
      Lude.Maybe Lude.Text,
    endTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotebookExecutionSummary' with the minimum fields required to make a request.
--
-- * 'editorId' - The unique identifier of the editor associated with the notebook execution.
-- * 'endTime' - The timestamp when notebook execution started.
-- * 'notebookExecutionId' - The unique identifier of the notebook execution.
-- * 'notebookExecutionName' - The name of the notebook execution.
-- * 'startTime' - The timestamp when notebook execution started.
-- * 'status' - The status of the notebook execution.
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
mkNotebookExecutionSummary ::
  NotebookExecutionSummary
mkNotebookExecutionSummary =
  NotebookExecutionSummary'
    { status = Lude.Nothing,
      editorId = Lude.Nothing,
      startTime = Lude.Nothing,
      notebookExecutionId = Lude.Nothing,
      notebookExecutionName = Lude.Nothing,
      endTime = Lude.Nothing
    }

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
nesStatus :: Lens.Lens' NotebookExecutionSummary (Lude.Maybe NotebookExecutionStatus)
nesStatus = Lens.lens (status :: NotebookExecutionSummary -> Lude.Maybe NotebookExecutionStatus) (\s a -> s {status = a} :: NotebookExecutionSummary)
{-# DEPRECATED nesStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The unique identifier of the editor associated with the notebook execution.
--
-- /Note:/ Consider using 'editorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nesEditorId :: Lens.Lens' NotebookExecutionSummary (Lude.Maybe Lude.Text)
nesEditorId = Lens.lens (editorId :: NotebookExecutionSummary -> Lude.Maybe Lude.Text) (\s a -> s {editorId = a} :: NotebookExecutionSummary)
{-# DEPRECATED nesEditorId "Use generic-lens or generic-optics with 'editorId' instead." #-}

-- | The timestamp when notebook execution started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nesStartTime :: Lens.Lens' NotebookExecutionSummary (Lude.Maybe Lude.Timestamp)
nesStartTime = Lens.lens (startTime :: NotebookExecutionSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: NotebookExecutionSummary)
{-# DEPRECATED nesStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The unique identifier of the notebook execution.
--
-- /Note:/ Consider using 'notebookExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nesNotebookExecutionId :: Lens.Lens' NotebookExecutionSummary (Lude.Maybe Lude.Text)
nesNotebookExecutionId = Lens.lens (notebookExecutionId :: NotebookExecutionSummary -> Lude.Maybe Lude.Text) (\s a -> s {notebookExecutionId = a} :: NotebookExecutionSummary)
{-# DEPRECATED nesNotebookExecutionId "Use generic-lens or generic-optics with 'notebookExecutionId' instead." #-}

-- | The name of the notebook execution.
--
-- /Note:/ Consider using 'notebookExecutionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nesNotebookExecutionName :: Lens.Lens' NotebookExecutionSummary (Lude.Maybe Lude.Text)
nesNotebookExecutionName = Lens.lens (notebookExecutionName :: NotebookExecutionSummary -> Lude.Maybe Lude.Text) (\s a -> s {notebookExecutionName = a} :: NotebookExecutionSummary)
{-# DEPRECATED nesNotebookExecutionName "Use generic-lens or generic-optics with 'notebookExecutionName' instead." #-}

-- | The timestamp when notebook execution started.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nesEndTime :: Lens.Lens' NotebookExecutionSummary (Lude.Maybe Lude.Timestamp)
nesEndTime = Lens.lens (endTime :: NotebookExecutionSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: NotebookExecutionSummary)
{-# DEPRECATED nesEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

instance Lude.FromJSON NotebookExecutionSummary where
  parseJSON =
    Lude.withObject
      "NotebookExecutionSummary"
      ( \x ->
          NotebookExecutionSummary'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "EditorId")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "NotebookExecutionId")
            Lude.<*> (x Lude..:? "NotebookExecutionName")
            Lude.<*> (x Lude..:? "EndTime")
      )
