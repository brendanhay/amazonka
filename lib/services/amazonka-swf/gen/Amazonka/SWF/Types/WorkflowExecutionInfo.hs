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
-- Module      : Amazonka.SWF.Types.WorkflowExecutionInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.WorkflowExecutionInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SWF.Types.CloseStatus
import Amazonka.SWF.Types.ExecutionStatus
import Amazonka.SWF.Types.WorkflowExecution
import Amazonka.SWF.Types.WorkflowType

-- | Contains information about a workflow execution.
--
-- /See:/ 'newWorkflowExecutionInfo' smart constructor.
data WorkflowExecutionInfo = WorkflowExecutionInfo'
  { -- | Set to true if a cancellation is requested for this workflow execution.
    cancelRequested :: Prelude.Maybe Prelude.Bool,
    -- | If the execution status is closed then this specifies how the execution
    -- was closed:
    --
    -- -   @COMPLETED@ – the execution was successfully completed.
    --
    -- -   @CANCELED@ – the execution was canceled.Cancellation allows the
    --     implementation to gracefully clean up before the execution is
    --     closed.
    --
    -- -   @TERMINATED@ – the execution was force terminated.
    --
    -- -   @FAILED@ – the execution failed to complete.
    --
    -- -   @TIMED_OUT@ – the execution did not complete in the alloted time and
    --     was automatically timed out.
    --
    -- -   @CONTINUED_AS_NEW@ – the execution is logically continued. This
    --     means the current execution was completed and a new execution was
    --     started to carry on the workflow.
    closeStatus :: Prelude.Maybe CloseStatus,
    -- | The time when the workflow execution was closed. Set only if the
    -- execution status is CLOSED.
    closeTimestamp :: Prelude.Maybe Data.POSIX,
    -- | If this workflow execution is a child of another execution then contains
    -- the workflow execution that started this execution.
    parent :: Prelude.Maybe WorkflowExecution,
    -- | The list of tags associated with the workflow execution. Tags can be
    -- used to identify and list workflow executions of interest through the
    -- visibility APIs. A workflow execution can have a maximum of 5 tags.
    tagList :: Prelude.Maybe [Prelude.Text],
    -- | The workflow execution this information is about.
    execution :: WorkflowExecution,
    -- | The type of the workflow execution.
    workflowType :: WorkflowType,
    -- | The time when the execution was started.
    startTimestamp :: Data.POSIX,
    -- | The current status of the execution.
    executionStatus :: ExecutionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowExecutionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cancelRequested', 'workflowExecutionInfo_cancelRequested' - Set to true if a cancellation is requested for this workflow execution.
--
-- 'closeStatus', 'workflowExecutionInfo_closeStatus' - If the execution status is closed then this specifies how the execution
-- was closed:
--
-- -   @COMPLETED@ – the execution was successfully completed.
--
-- -   @CANCELED@ – the execution was canceled.Cancellation allows the
--     implementation to gracefully clean up before the execution is
--     closed.
--
-- -   @TERMINATED@ – the execution was force terminated.
--
-- -   @FAILED@ – the execution failed to complete.
--
-- -   @TIMED_OUT@ – the execution did not complete in the alloted time and
--     was automatically timed out.
--
-- -   @CONTINUED_AS_NEW@ – the execution is logically continued. This
--     means the current execution was completed and a new execution was
--     started to carry on the workflow.
--
-- 'closeTimestamp', 'workflowExecutionInfo_closeTimestamp' - The time when the workflow execution was closed. Set only if the
-- execution status is CLOSED.
--
-- 'parent', 'workflowExecutionInfo_parent' - If this workflow execution is a child of another execution then contains
-- the workflow execution that started this execution.
--
-- 'tagList', 'workflowExecutionInfo_tagList' - The list of tags associated with the workflow execution. Tags can be
-- used to identify and list workflow executions of interest through the
-- visibility APIs. A workflow execution can have a maximum of 5 tags.
--
-- 'execution', 'workflowExecutionInfo_execution' - The workflow execution this information is about.
--
-- 'workflowType', 'workflowExecutionInfo_workflowType' - The type of the workflow execution.
--
-- 'startTimestamp', 'workflowExecutionInfo_startTimestamp' - The time when the execution was started.
--
-- 'executionStatus', 'workflowExecutionInfo_executionStatus' - The current status of the execution.
newWorkflowExecutionInfo ::
  -- | 'execution'
  WorkflowExecution ->
  -- | 'workflowType'
  WorkflowType ->
  -- | 'startTimestamp'
  Prelude.UTCTime ->
  -- | 'executionStatus'
  ExecutionStatus ->
  WorkflowExecutionInfo
newWorkflowExecutionInfo
  pExecution_
  pWorkflowType_
  pStartTimestamp_
  pExecutionStatus_ =
    WorkflowExecutionInfo'
      { cancelRequested =
          Prelude.Nothing,
        closeStatus = Prelude.Nothing,
        closeTimestamp = Prelude.Nothing,
        parent = Prelude.Nothing,
        tagList = Prelude.Nothing,
        execution = pExecution_,
        workflowType = pWorkflowType_,
        startTimestamp = Data._Time Lens.# pStartTimestamp_,
        executionStatus = pExecutionStatus_
      }

-- | Set to true if a cancellation is requested for this workflow execution.
workflowExecutionInfo_cancelRequested :: Lens.Lens' WorkflowExecutionInfo (Prelude.Maybe Prelude.Bool)
workflowExecutionInfo_cancelRequested = Lens.lens (\WorkflowExecutionInfo' {cancelRequested} -> cancelRequested) (\s@WorkflowExecutionInfo' {} a -> s {cancelRequested = a} :: WorkflowExecutionInfo)

-- | If the execution status is closed then this specifies how the execution
-- was closed:
--
-- -   @COMPLETED@ – the execution was successfully completed.
--
-- -   @CANCELED@ – the execution was canceled.Cancellation allows the
--     implementation to gracefully clean up before the execution is
--     closed.
--
-- -   @TERMINATED@ – the execution was force terminated.
--
-- -   @FAILED@ – the execution failed to complete.
--
-- -   @TIMED_OUT@ – the execution did not complete in the alloted time and
--     was automatically timed out.
--
-- -   @CONTINUED_AS_NEW@ – the execution is logically continued. This
--     means the current execution was completed and a new execution was
--     started to carry on the workflow.
workflowExecutionInfo_closeStatus :: Lens.Lens' WorkflowExecutionInfo (Prelude.Maybe CloseStatus)
workflowExecutionInfo_closeStatus = Lens.lens (\WorkflowExecutionInfo' {closeStatus} -> closeStatus) (\s@WorkflowExecutionInfo' {} a -> s {closeStatus = a} :: WorkflowExecutionInfo)

-- | The time when the workflow execution was closed. Set only if the
-- execution status is CLOSED.
workflowExecutionInfo_closeTimestamp :: Lens.Lens' WorkflowExecutionInfo (Prelude.Maybe Prelude.UTCTime)
workflowExecutionInfo_closeTimestamp = Lens.lens (\WorkflowExecutionInfo' {closeTimestamp} -> closeTimestamp) (\s@WorkflowExecutionInfo' {} a -> s {closeTimestamp = a} :: WorkflowExecutionInfo) Prelude.. Lens.mapping Data._Time

-- | If this workflow execution is a child of another execution then contains
-- the workflow execution that started this execution.
workflowExecutionInfo_parent :: Lens.Lens' WorkflowExecutionInfo (Prelude.Maybe WorkflowExecution)
workflowExecutionInfo_parent = Lens.lens (\WorkflowExecutionInfo' {parent} -> parent) (\s@WorkflowExecutionInfo' {} a -> s {parent = a} :: WorkflowExecutionInfo)

-- | The list of tags associated with the workflow execution. Tags can be
-- used to identify and list workflow executions of interest through the
-- visibility APIs. A workflow execution can have a maximum of 5 tags.
workflowExecutionInfo_tagList :: Lens.Lens' WorkflowExecutionInfo (Prelude.Maybe [Prelude.Text])
workflowExecutionInfo_tagList = Lens.lens (\WorkflowExecutionInfo' {tagList} -> tagList) (\s@WorkflowExecutionInfo' {} a -> s {tagList = a} :: WorkflowExecutionInfo) Prelude.. Lens.mapping Lens.coerced

-- | The workflow execution this information is about.
workflowExecutionInfo_execution :: Lens.Lens' WorkflowExecutionInfo WorkflowExecution
workflowExecutionInfo_execution = Lens.lens (\WorkflowExecutionInfo' {execution} -> execution) (\s@WorkflowExecutionInfo' {} a -> s {execution = a} :: WorkflowExecutionInfo)

-- | The type of the workflow execution.
workflowExecutionInfo_workflowType :: Lens.Lens' WorkflowExecutionInfo WorkflowType
workflowExecutionInfo_workflowType = Lens.lens (\WorkflowExecutionInfo' {workflowType} -> workflowType) (\s@WorkflowExecutionInfo' {} a -> s {workflowType = a} :: WorkflowExecutionInfo)

-- | The time when the execution was started.
workflowExecutionInfo_startTimestamp :: Lens.Lens' WorkflowExecutionInfo Prelude.UTCTime
workflowExecutionInfo_startTimestamp = Lens.lens (\WorkflowExecutionInfo' {startTimestamp} -> startTimestamp) (\s@WorkflowExecutionInfo' {} a -> s {startTimestamp = a} :: WorkflowExecutionInfo) Prelude.. Data._Time

-- | The current status of the execution.
workflowExecutionInfo_executionStatus :: Lens.Lens' WorkflowExecutionInfo ExecutionStatus
workflowExecutionInfo_executionStatus = Lens.lens (\WorkflowExecutionInfo' {executionStatus} -> executionStatus) (\s@WorkflowExecutionInfo' {} a -> s {executionStatus = a} :: WorkflowExecutionInfo)

instance Data.FromJSON WorkflowExecutionInfo where
  parseJSON =
    Data.withObject
      "WorkflowExecutionInfo"
      ( \x ->
          WorkflowExecutionInfo'
            Prelude.<$> (x Data..:? "cancelRequested")
            Prelude.<*> (x Data..:? "closeStatus")
            Prelude.<*> (x Data..:? "closeTimestamp")
            Prelude.<*> (x Data..:? "parent")
            Prelude.<*> (x Data..:? "tagList" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "execution")
            Prelude.<*> (x Data..: "workflowType")
            Prelude.<*> (x Data..: "startTimestamp")
            Prelude.<*> (x Data..: "executionStatus")
      )

instance Prelude.Hashable WorkflowExecutionInfo where
  hashWithSalt _salt WorkflowExecutionInfo' {..} =
    _salt `Prelude.hashWithSalt` cancelRequested
      `Prelude.hashWithSalt` closeStatus
      `Prelude.hashWithSalt` closeTimestamp
      `Prelude.hashWithSalt` parent
      `Prelude.hashWithSalt` tagList
      `Prelude.hashWithSalt` execution
      `Prelude.hashWithSalt` workflowType
      `Prelude.hashWithSalt` startTimestamp
      `Prelude.hashWithSalt` executionStatus

instance Prelude.NFData WorkflowExecutionInfo where
  rnf WorkflowExecutionInfo' {..} =
    Prelude.rnf cancelRequested
      `Prelude.seq` Prelude.rnf closeStatus
      `Prelude.seq` Prelude.rnf closeTimestamp
      `Prelude.seq` Prelude.rnf parent
      `Prelude.seq` Prelude.rnf tagList
      `Prelude.seq` Prelude.rnf execution
      `Prelude.seq` Prelude.rnf workflowType
      `Prelude.seq` Prelude.rnf startTimestamp
      `Prelude.seq` Prelude.rnf executionStatus
