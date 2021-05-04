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
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SWF.Types.CloseStatus
import Network.AWS.SWF.Types.ExecutionStatus
import Network.AWS.SWF.Types.WorkflowExecution
import Network.AWS.SWF.Types.WorkflowType

-- | Contains information about a workflow execution.
--
-- /See:/ 'newWorkflowExecutionInfo' smart constructor.
data WorkflowExecutionInfo = WorkflowExecutionInfo'
  { -- | If this workflow execution is a child of another execution then contains
    -- the workflow execution that started this execution.
    parent :: Prelude.Maybe WorkflowExecution,
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
    -- | Set to true if a cancellation is requested for this workflow execution.
    cancelRequested :: Prelude.Maybe Prelude.Bool,
    -- | The time when the workflow execution was closed. Set only if the
    -- execution status is CLOSED.
    closeTimestamp :: Prelude.Maybe Prelude.POSIX,
    -- | The list of tags associated with the workflow execution. Tags can be
    -- used to identify and list workflow executions of interest through the
    -- visibility APIs. A workflow execution can have a maximum of 5 tags.
    tagList :: Prelude.Maybe [Prelude.Text],
    -- | The workflow execution this information is about.
    execution :: WorkflowExecution,
    -- | The type of the workflow execution.
    workflowType :: WorkflowType,
    -- | The time when the execution was started.
    startTimestamp :: Prelude.POSIX,
    -- | The current status of the execution.
    executionStatus :: ExecutionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WorkflowExecutionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parent', 'workflowExecutionInfo_parent' - If this workflow execution is a child of another execution then contains
-- the workflow execution that started this execution.
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
-- 'cancelRequested', 'workflowExecutionInfo_cancelRequested' - Set to true if a cancellation is requested for this workflow execution.
--
-- 'closeTimestamp', 'workflowExecutionInfo_closeTimestamp' - The time when the workflow execution was closed. Set only if the
-- execution status is CLOSED.
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
      { parent = Prelude.Nothing,
        closeStatus = Prelude.Nothing,
        cancelRequested = Prelude.Nothing,
        closeTimestamp = Prelude.Nothing,
        tagList = Prelude.Nothing,
        execution = pExecution_,
        workflowType = pWorkflowType_,
        startTimestamp =
          Prelude._Time Lens.# pStartTimestamp_,
        executionStatus = pExecutionStatus_
      }

-- | If this workflow execution is a child of another execution then contains
-- the workflow execution that started this execution.
workflowExecutionInfo_parent :: Lens.Lens' WorkflowExecutionInfo (Prelude.Maybe WorkflowExecution)
workflowExecutionInfo_parent = Lens.lens (\WorkflowExecutionInfo' {parent} -> parent) (\s@WorkflowExecutionInfo' {} a -> s {parent = a} :: WorkflowExecutionInfo)

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

-- | Set to true if a cancellation is requested for this workflow execution.
workflowExecutionInfo_cancelRequested :: Lens.Lens' WorkflowExecutionInfo (Prelude.Maybe Prelude.Bool)
workflowExecutionInfo_cancelRequested = Lens.lens (\WorkflowExecutionInfo' {cancelRequested} -> cancelRequested) (\s@WorkflowExecutionInfo' {} a -> s {cancelRequested = a} :: WorkflowExecutionInfo)

-- | The time when the workflow execution was closed. Set only if the
-- execution status is CLOSED.
workflowExecutionInfo_closeTimestamp :: Lens.Lens' WorkflowExecutionInfo (Prelude.Maybe Prelude.UTCTime)
workflowExecutionInfo_closeTimestamp = Lens.lens (\WorkflowExecutionInfo' {closeTimestamp} -> closeTimestamp) (\s@WorkflowExecutionInfo' {} a -> s {closeTimestamp = a} :: WorkflowExecutionInfo) Prelude.. Lens.mapping Prelude._Time

-- | The list of tags associated with the workflow execution. Tags can be
-- used to identify and list workflow executions of interest through the
-- visibility APIs. A workflow execution can have a maximum of 5 tags.
workflowExecutionInfo_tagList :: Lens.Lens' WorkflowExecutionInfo (Prelude.Maybe [Prelude.Text])
workflowExecutionInfo_tagList = Lens.lens (\WorkflowExecutionInfo' {tagList} -> tagList) (\s@WorkflowExecutionInfo' {} a -> s {tagList = a} :: WorkflowExecutionInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | The workflow execution this information is about.
workflowExecutionInfo_execution :: Lens.Lens' WorkflowExecutionInfo WorkflowExecution
workflowExecutionInfo_execution = Lens.lens (\WorkflowExecutionInfo' {execution} -> execution) (\s@WorkflowExecutionInfo' {} a -> s {execution = a} :: WorkflowExecutionInfo)

-- | The type of the workflow execution.
workflowExecutionInfo_workflowType :: Lens.Lens' WorkflowExecutionInfo WorkflowType
workflowExecutionInfo_workflowType = Lens.lens (\WorkflowExecutionInfo' {workflowType} -> workflowType) (\s@WorkflowExecutionInfo' {} a -> s {workflowType = a} :: WorkflowExecutionInfo)

-- | The time when the execution was started.
workflowExecutionInfo_startTimestamp :: Lens.Lens' WorkflowExecutionInfo Prelude.UTCTime
workflowExecutionInfo_startTimestamp = Lens.lens (\WorkflowExecutionInfo' {startTimestamp} -> startTimestamp) (\s@WorkflowExecutionInfo' {} a -> s {startTimestamp = a} :: WorkflowExecutionInfo) Prelude.. Prelude._Time

-- | The current status of the execution.
workflowExecutionInfo_executionStatus :: Lens.Lens' WorkflowExecutionInfo ExecutionStatus
workflowExecutionInfo_executionStatus = Lens.lens (\WorkflowExecutionInfo' {executionStatus} -> executionStatus) (\s@WorkflowExecutionInfo' {} a -> s {executionStatus = a} :: WorkflowExecutionInfo)

instance Prelude.FromJSON WorkflowExecutionInfo where
  parseJSON =
    Prelude.withObject
      "WorkflowExecutionInfo"
      ( \x ->
          WorkflowExecutionInfo'
            Prelude.<$> (x Prelude..:? "parent")
            Prelude.<*> (x Prelude..:? "closeStatus")
            Prelude.<*> (x Prelude..:? "cancelRequested")
            Prelude.<*> (x Prelude..:? "closeTimestamp")
            Prelude.<*> (x Prelude..:? "tagList" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..: "execution")
            Prelude.<*> (x Prelude..: "workflowType")
            Prelude.<*> (x Prelude..: "startTimestamp")
            Prelude.<*> (x Prelude..: "executionStatus")
      )

instance Prelude.Hashable WorkflowExecutionInfo

instance Prelude.NFData WorkflowExecutionInfo
