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
-- Module      : Amazonka.EMR.Types.NotebookExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.NotebookExecution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EMR.Types.ExecutionEngineConfig
import Amazonka.EMR.Types.NotebookExecutionStatus
import Amazonka.EMR.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | A notebook execution. An execution is a specific instance that an EMR
-- Notebook is run using the @StartNotebookExecution@ action.
--
-- /See:/ 'newNotebookExecution' smart constructor.
data NotebookExecution = NotebookExecution'
  { -- | A list of tags associated with a notebook execution. Tags are
    -- user-defined key-value pairs that consist of a required key string with
    -- a maximum of 128 characters and an optional value string with a maximum
    -- of 256 characters.
    tags :: Prelude.Maybe [Tag],
    -- | The reason for the latest status change of the notebook execution.
    lastStateChangeReason :: Prelude.Maybe Prelude.Text,
    -- | The execution engine, such as an EMR cluster, used to run the EMR
    -- notebook and perform the notebook execution.
    executionEngine :: Prelude.Maybe ExecutionEngineConfig,
    -- | The unique identifier of the EC2 security group associated with the EMR
    -- Notebook instance. For more information see
    -- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-managed-notebooks-security-groups.html Specifying EC2 Security Groups for EMR Notebooks>
    -- in the /EMR Management Guide/.
    notebookInstanceSecurityGroupId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the notebook execution.
    arn :: Prelude.Maybe Prelude.Text,
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
    status :: Prelude.Maybe NotebookExecutionStatus,
    -- | The location of the notebook execution\'s output file in Amazon S3.
    outputNotebookURI :: Prelude.Maybe Prelude.Text,
    -- | A name for the notebook execution.
    notebookExecutionName :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when notebook execution ended.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The unique identifier of the EMR Notebook that is used for the notebook
    -- execution.
    editorId :: Prelude.Maybe Prelude.Text,
    -- | Input parameters in JSON format passed to the EMR Notebook at runtime
    -- for execution.
    notebookParams :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when notebook execution started.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The unique identifier of a notebook execution.
    notebookExecutionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotebookExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'notebookExecution_tags' - A list of tags associated with a notebook execution. Tags are
-- user-defined key-value pairs that consist of a required key string with
-- a maximum of 128 characters and an optional value string with a maximum
-- of 256 characters.
--
-- 'lastStateChangeReason', 'notebookExecution_lastStateChangeReason' - The reason for the latest status change of the notebook execution.
--
-- 'executionEngine', 'notebookExecution_executionEngine' - The execution engine, such as an EMR cluster, used to run the EMR
-- notebook and perform the notebook execution.
--
-- 'notebookInstanceSecurityGroupId', 'notebookExecution_notebookInstanceSecurityGroupId' - The unique identifier of the EC2 security group associated with the EMR
-- Notebook instance. For more information see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-managed-notebooks-security-groups.html Specifying EC2 Security Groups for EMR Notebooks>
-- in the /EMR Management Guide/.
--
-- 'arn', 'notebookExecution_arn' - The Amazon Resource Name (ARN) of the notebook execution.
--
-- 'status', 'notebookExecution_status' - The status of the notebook execution.
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
-- 'outputNotebookURI', 'notebookExecution_outputNotebookURI' - The location of the notebook execution\'s output file in Amazon S3.
--
-- 'notebookExecutionName', 'notebookExecution_notebookExecutionName' - A name for the notebook execution.
--
-- 'endTime', 'notebookExecution_endTime' - The timestamp when notebook execution ended.
--
-- 'editorId', 'notebookExecution_editorId' - The unique identifier of the EMR Notebook that is used for the notebook
-- execution.
--
-- 'notebookParams', 'notebookExecution_notebookParams' - Input parameters in JSON format passed to the EMR Notebook at runtime
-- for execution.
--
-- 'startTime', 'notebookExecution_startTime' - The timestamp when notebook execution started.
--
-- 'notebookExecutionId', 'notebookExecution_notebookExecutionId' - The unique identifier of a notebook execution.
newNotebookExecution ::
  NotebookExecution
newNotebookExecution =
  NotebookExecution'
    { tags = Prelude.Nothing,
      lastStateChangeReason = Prelude.Nothing,
      executionEngine = Prelude.Nothing,
      notebookInstanceSecurityGroupId = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      outputNotebookURI = Prelude.Nothing,
      notebookExecutionName = Prelude.Nothing,
      endTime = Prelude.Nothing,
      editorId = Prelude.Nothing,
      notebookParams = Prelude.Nothing,
      startTime = Prelude.Nothing,
      notebookExecutionId = Prelude.Nothing
    }

-- | A list of tags associated with a notebook execution. Tags are
-- user-defined key-value pairs that consist of a required key string with
-- a maximum of 128 characters and an optional value string with a maximum
-- of 256 characters.
notebookExecution_tags :: Lens.Lens' NotebookExecution (Prelude.Maybe [Tag])
notebookExecution_tags = Lens.lens (\NotebookExecution' {tags} -> tags) (\s@NotebookExecution' {} a -> s {tags = a} :: NotebookExecution) Prelude.. Lens.mapping Lens.coerced

-- | The reason for the latest status change of the notebook execution.
notebookExecution_lastStateChangeReason :: Lens.Lens' NotebookExecution (Prelude.Maybe Prelude.Text)
notebookExecution_lastStateChangeReason = Lens.lens (\NotebookExecution' {lastStateChangeReason} -> lastStateChangeReason) (\s@NotebookExecution' {} a -> s {lastStateChangeReason = a} :: NotebookExecution)

-- | The execution engine, such as an EMR cluster, used to run the EMR
-- notebook and perform the notebook execution.
notebookExecution_executionEngine :: Lens.Lens' NotebookExecution (Prelude.Maybe ExecutionEngineConfig)
notebookExecution_executionEngine = Lens.lens (\NotebookExecution' {executionEngine} -> executionEngine) (\s@NotebookExecution' {} a -> s {executionEngine = a} :: NotebookExecution)

-- | The unique identifier of the EC2 security group associated with the EMR
-- Notebook instance. For more information see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-managed-notebooks-security-groups.html Specifying EC2 Security Groups for EMR Notebooks>
-- in the /EMR Management Guide/.
notebookExecution_notebookInstanceSecurityGroupId :: Lens.Lens' NotebookExecution (Prelude.Maybe Prelude.Text)
notebookExecution_notebookInstanceSecurityGroupId = Lens.lens (\NotebookExecution' {notebookInstanceSecurityGroupId} -> notebookInstanceSecurityGroupId) (\s@NotebookExecution' {} a -> s {notebookInstanceSecurityGroupId = a} :: NotebookExecution)

-- | The Amazon Resource Name (ARN) of the notebook execution.
notebookExecution_arn :: Lens.Lens' NotebookExecution (Prelude.Maybe Prelude.Text)
notebookExecution_arn = Lens.lens (\NotebookExecution' {arn} -> arn) (\s@NotebookExecution' {} a -> s {arn = a} :: NotebookExecution)

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
notebookExecution_status :: Lens.Lens' NotebookExecution (Prelude.Maybe NotebookExecutionStatus)
notebookExecution_status = Lens.lens (\NotebookExecution' {status} -> status) (\s@NotebookExecution' {} a -> s {status = a} :: NotebookExecution)

-- | The location of the notebook execution\'s output file in Amazon S3.
notebookExecution_outputNotebookURI :: Lens.Lens' NotebookExecution (Prelude.Maybe Prelude.Text)
notebookExecution_outputNotebookURI = Lens.lens (\NotebookExecution' {outputNotebookURI} -> outputNotebookURI) (\s@NotebookExecution' {} a -> s {outputNotebookURI = a} :: NotebookExecution)

-- | A name for the notebook execution.
notebookExecution_notebookExecutionName :: Lens.Lens' NotebookExecution (Prelude.Maybe Prelude.Text)
notebookExecution_notebookExecutionName = Lens.lens (\NotebookExecution' {notebookExecutionName} -> notebookExecutionName) (\s@NotebookExecution' {} a -> s {notebookExecutionName = a} :: NotebookExecution)

-- | The timestamp when notebook execution ended.
notebookExecution_endTime :: Lens.Lens' NotebookExecution (Prelude.Maybe Prelude.UTCTime)
notebookExecution_endTime = Lens.lens (\NotebookExecution' {endTime} -> endTime) (\s@NotebookExecution' {} a -> s {endTime = a} :: NotebookExecution) Prelude.. Lens.mapping Core._Time

-- | The unique identifier of the EMR Notebook that is used for the notebook
-- execution.
notebookExecution_editorId :: Lens.Lens' NotebookExecution (Prelude.Maybe Prelude.Text)
notebookExecution_editorId = Lens.lens (\NotebookExecution' {editorId} -> editorId) (\s@NotebookExecution' {} a -> s {editorId = a} :: NotebookExecution)

-- | Input parameters in JSON format passed to the EMR Notebook at runtime
-- for execution.
notebookExecution_notebookParams :: Lens.Lens' NotebookExecution (Prelude.Maybe Prelude.Text)
notebookExecution_notebookParams = Lens.lens (\NotebookExecution' {notebookParams} -> notebookParams) (\s@NotebookExecution' {} a -> s {notebookParams = a} :: NotebookExecution)

-- | The timestamp when notebook execution started.
notebookExecution_startTime :: Lens.Lens' NotebookExecution (Prelude.Maybe Prelude.UTCTime)
notebookExecution_startTime = Lens.lens (\NotebookExecution' {startTime} -> startTime) (\s@NotebookExecution' {} a -> s {startTime = a} :: NotebookExecution) Prelude.. Lens.mapping Core._Time

-- | The unique identifier of a notebook execution.
notebookExecution_notebookExecutionId :: Lens.Lens' NotebookExecution (Prelude.Maybe Prelude.Text)
notebookExecution_notebookExecutionId = Lens.lens (\NotebookExecution' {notebookExecutionId} -> notebookExecutionId) (\s@NotebookExecution' {} a -> s {notebookExecutionId = a} :: NotebookExecution)

instance Core.FromJSON NotebookExecution where
  parseJSON =
    Core.withObject
      "NotebookExecution"
      ( \x ->
          NotebookExecution'
            Prelude.<$> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "LastStateChangeReason")
            Prelude.<*> (x Core..:? "ExecutionEngine")
            Prelude.<*> (x Core..:? "NotebookInstanceSecurityGroupId")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "OutputNotebookURI")
            Prelude.<*> (x Core..:? "NotebookExecutionName")
            Prelude.<*> (x Core..:? "EndTime")
            Prelude.<*> (x Core..:? "EditorId")
            Prelude.<*> (x Core..:? "NotebookParams")
            Prelude.<*> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "NotebookExecutionId")
      )

instance Prelude.Hashable NotebookExecution where
  hashWithSalt _salt NotebookExecution' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` lastStateChangeReason
      `Prelude.hashWithSalt` executionEngine
      `Prelude.hashWithSalt` notebookInstanceSecurityGroupId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` outputNotebookURI
      `Prelude.hashWithSalt` notebookExecutionName
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` editorId
      `Prelude.hashWithSalt` notebookParams
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` notebookExecutionId

instance Prelude.NFData NotebookExecution where
  rnf NotebookExecution' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf lastStateChangeReason
      `Prelude.seq` Prelude.rnf executionEngine
      `Prelude.seq` Prelude.rnf notebookInstanceSecurityGroupId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf outputNotebookURI
      `Prelude.seq` Prelude.rnf notebookExecutionName
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf editorId
      `Prelude.seq` Prelude.rnf notebookParams
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf notebookExecutionId
