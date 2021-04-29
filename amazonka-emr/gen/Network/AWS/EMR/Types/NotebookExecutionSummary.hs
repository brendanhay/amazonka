{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.NotebookExecutionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.NotebookExecutionSummary where

import Network.AWS.EMR.Types.NotebookExecutionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- |
--
-- /See:/ 'newNotebookExecutionSummary' smart constructor.
data NotebookExecutionSummary = NotebookExecutionSummary'
  { -- | The status of the notebook execution.
    --
    -- -   @START_PENDING@ indicates that the cluster has received the
    --     execution request but execution has not begun.
    --
    -- -   @STARTING@ indicates that the execution is starting on the cluster.
    --
    -- -   @RUNNING@ indicates that the execution is being processed by the
    --     cluster.
    --
    -- -   @FINISHING@ indicates that execution processing is in the final
    --     stages.
    --
    -- -   @FINISHED@ indicates that the execution has completed without error.
    --
    -- -   @FAILING@ indicates that the execution is failing and will not
    --     finish successfully.
    --
    -- -   @FAILED@ indicates that the execution failed.
    --
    -- -   @STOP_PENDING@ indicates that the cluster has received a
    --     @StopNotebookExecution@ request and the stop is pending.
    --
    -- -   @STOPPING@ indicates that the cluster is in the process of stopping
    --     the execution as a result of a @StopNotebookExecution@ request.
    --
    -- -   @STOPPED@ indicates that the execution stopped because of a
    --     @StopNotebookExecution@ request.
    status :: Prelude.Maybe NotebookExecutionStatus,
    -- | The name of the notebook execution.
    notebookExecutionName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the editor associated with the notebook
    -- execution.
    editorId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the notebook execution.
    notebookExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when notebook execution started.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The timestamp when notebook execution started.
    endTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NotebookExecutionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'notebookExecutionSummary_status' - The status of the notebook execution.
--
-- -   @START_PENDING@ indicates that the cluster has received the
--     execution request but execution has not begun.
--
-- -   @STARTING@ indicates that the execution is starting on the cluster.
--
-- -   @RUNNING@ indicates that the execution is being processed by the
--     cluster.
--
-- -   @FINISHING@ indicates that execution processing is in the final
--     stages.
--
-- -   @FINISHED@ indicates that the execution has completed without error.
--
-- -   @FAILING@ indicates that the execution is failing and will not
--     finish successfully.
--
-- -   @FAILED@ indicates that the execution failed.
--
-- -   @STOP_PENDING@ indicates that the cluster has received a
--     @StopNotebookExecution@ request and the stop is pending.
--
-- -   @STOPPING@ indicates that the cluster is in the process of stopping
--     the execution as a result of a @StopNotebookExecution@ request.
--
-- -   @STOPPED@ indicates that the execution stopped because of a
--     @StopNotebookExecution@ request.
--
-- 'notebookExecutionName', 'notebookExecutionSummary_notebookExecutionName' - The name of the notebook execution.
--
-- 'editorId', 'notebookExecutionSummary_editorId' - The unique identifier of the editor associated with the notebook
-- execution.
--
-- 'notebookExecutionId', 'notebookExecutionSummary_notebookExecutionId' - The unique identifier of the notebook execution.
--
-- 'startTime', 'notebookExecutionSummary_startTime' - The timestamp when notebook execution started.
--
-- 'endTime', 'notebookExecutionSummary_endTime' - The timestamp when notebook execution started.
newNotebookExecutionSummary ::
  NotebookExecutionSummary
newNotebookExecutionSummary =
  NotebookExecutionSummary'
    { status = Prelude.Nothing,
      notebookExecutionName = Prelude.Nothing,
      editorId = Prelude.Nothing,
      notebookExecutionId = Prelude.Nothing,
      startTime = Prelude.Nothing,
      endTime = Prelude.Nothing
    }

-- | The status of the notebook execution.
--
-- -   @START_PENDING@ indicates that the cluster has received the
--     execution request but execution has not begun.
--
-- -   @STARTING@ indicates that the execution is starting on the cluster.
--
-- -   @RUNNING@ indicates that the execution is being processed by the
--     cluster.
--
-- -   @FINISHING@ indicates that execution processing is in the final
--     stages.
--
-- -   @FINISHED@ indicates that the execution has completed without error.
--
-- -   @FAILING@ indicates that the execution is failing and will not
--     finish successfully.
--
-- -   @FAILED@ indicates that the execution failed.
--
-- -   @STOP_PENDING@ indicates that the cluster has received a
--     @StopNotebookExecution@ request and the stop is pending.
--
-- -   @STOPPING@ indicates that the cluster is in the process of stopping
--     the execution as a result of a @StopNotebookExecution@ request.
--
-- -   @STOPPED@ indicates that the execution stopped because of a
--     @StopNotebookExecution@ request.
notebookExecutionSummary_status :: Lens.Lens' NotebookExecutionSummary (Prelude.Maybe NotebookExecutionStatus)
notebookExecutionSummary_status = Lens.lens (\NotebookExecutionSummary' {status} -> status) (\s@NotebookExecutionSummary' {} a -> s {status = a} :: NotebookExecutionSummary)

-- | The name of the notebook execution.
notebookExecutionSummary_notebookExecutionName :: Lens.Lens' NotebookExecutionSummary (Prelude.Maybe Prelude.Text)
notebookExecutionSummary_notebookExecutionName = Lens.lens (\NotebookExecutionSummary' {notebookExecutionName} -> notebookExecutionName) (\s@NotebookExecutionSummary' {} a -> s {notebookExecutionName = a} :: NotebookExecutionSummary)

-- | The unique identifier of the editor associated with the notebook
-- execution.
notebookExecutionSummary_editorId :: Lens.Lens' NotebookExecutionSummary (Prelude.Maybe Prelude.Text)
notebookExecutionSummary_editorId = Lens.lens (\NotebookExecutionSummary' {editorId} -> editorId) (\s@NotebookExecutionSummary' {} a -> s {editorId = a} :: NotebookExecutionSummary)

-- | The unique identifier of the notebook execution.
notebookExecutionSummary_notebookExecutionId :: Lens.Lens' NotebookExecutionSummary (Prelude.Maybe Prelude.Text)
notebookExecutionSummary_notebookExecutionId = Lens.lens (\NotebookExecutionSummary' {notebookExecutionId} -> notebookExecutionId) (\s@NotebookExecutionSummary' {} a -> s {notebookExecutionId = a} :: NotebookExecutionSummary)

-- | The timestamp when notebook execution started.
notebookExecutionSummary_startTime :: Lens.Lens' NotebookExecutionSummary (Prelude.Maybe Prelude.UTCTime)
notebookExecutionSummary_startTime = Lens.lens (\NotebookExecutionSummary' {startTime} -> startTime) (\s@NotebookExecutionSummary' {} a -> s {startTime = a} :: NotebookExecutionSummary) Prelude.. Lens.mapping Prelude._Time

-- | The timestamp when notebook execution started.
notebookExecutionSummary_endTime :: Lens.Lens' NotebookExecutionSummary (Prelude.Maybe Prelude.UTCTime)
notebookExecutionSummary_endTime = Lens.lens (\NotebookExecutionSummary' {endTime} -> endTime) (\s@NotebookExecutionSummary' {} a -> s {endTime = a} :: NotebookExecutionSummary) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON NotebookExecutionSummary where
  parseJSON =
    Prelude.withObject
      "NotebookExecutionSummary"
      ( \x ->
          NotebookExecutionSummary'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "NotebookExecutionName")
            Prelude.<*> (x Prelude..:? "EditorId")
            Prelude.<*> (x Prelude..:? "NotebookExecutionId")
            Prelude.<*> (x Prelude..:? "StartTime")
            Prelude.<*> (x Prelude..:? "EndTime")
      )

instance Prelude.Hashable NotebookExecutionSummary

instance Prelude.NFData NotebookExecutionSummary
