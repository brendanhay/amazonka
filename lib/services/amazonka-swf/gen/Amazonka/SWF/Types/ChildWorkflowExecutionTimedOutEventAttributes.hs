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
-- Module      : Amazonka.SWF.Types.ChildWorkflowExecutionTimedOutEventAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.ChildWorkflowExecutionTimedOutEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SWF.Types.WorkflowExecution
import Amazonka.SWF.Types.WorkflowExecutionTimeoutType
import Amazonka.SWF.Types.WorkflowType

-- | Provides the details of the @ChildWorkflowExecutionTimedOut@ event.
--
-- /See:/ 'newChildWorkflowExecutionTimedOutEventAttributes' smart constructor.
data ChildWorkflowExecutionTimedOutEventAttributes = ChildWorkflowExecutionTimedOutEventAttributes'
  { -- | The child workflow execution that timed out.
    workflowExecution :: WorkflowExecution,
    -- | The type of the child workflow execution.
    workflowType :: WorkflowType,
    -- | The type of the timeout that caused the child workflow execution to time
    -- out.
    timeoutType :: WorkflowExecutionTimeoutType,
    -- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding
    -- to the @StartChildWorkflowExecution@ Decision to start this child
    -- workflow execution. This information can be useful for diagnosing
    -- problems by tracing back the chain of events leading up to this event.
    initiatedEventId :: Prelude.Integer,
    -- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this
    -- child workflow execution was started. This information can be useful for
    -- diagnosing problems by tracing back the chain of events leading up to
    -- this event.
    startedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChildWorkflowExecutionTimedOutEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workflowExecution', 'childWorkflowExecutionTimedOutEventAttributes_workflowExecution' - The child workflow execution that timed out.
--
-- 'workflowType', 'childWorkflowExecutionTimedOutEventAttributes_workflowType' - The type of the child workflow execution.
--
-- 'timeoutType', 'childWorkflowExecutionTimedOutEventAttributes_timeoutType' - The type of the timeout that caused the child workflow execution to time
-- out.
--
-- 'initiatedEventId', 'childWorkflowExecutionTimedOutEventAttributes_initiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding
-- to the @StartChildWorkflowExecution@ Decision to start this child
-- workflow execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
--
-- 'startedEventId', 'childWorkflowExecutionTimedOutEventAttributes_startedEventId' - The ID of the @ChildWorkflowExecutionStarted@ event recorded when this
-- child workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
newChildWorkflowExecutionTimedOutEventAttributes ::
  -- | 'workflowExecution'
  WorkflowExecution ->
  -- | 'workflowType'
  WorkflowType ->
  -- | 'timeoutType'
  WorkflowExecutionTimeoutType ->
  -- | 'initiatedEventId'
  Prelude.Integer ->
  -- | 'startedEventId'
  Prelude.Integer ->
  ChildWorkflowExecutionTimedOutEventAttributes
newChildWorkflowExecutionTimedOutEventAttributes
  pWorkflowExecution_
  pWorkflowType_
  pTimeoutType_
  pInitiatedEventId_
  pStartedEventId_ =
    ChildWorkflowExecutionTimedOutEventAttributes'
      { workflowExecution =
          pWorkflowExecution_,
        workflowType =
          pWorkflowType_,
        timeoutType = pTimeoutType_,
        initiatedEventId =
          pInitiatedEventId_,
        startedEventId =
          pStartedEventId_
      }

-- | The child workflow execution that timed out.
childWorkflowExecutionTimedOutEventAttributes_workflowExecution :: Lens.Lens' ChildWorkflowExecutionTimedOutEventAttributes WorkflowExecution
childWorkflowExecutionTimedOutEventAttributes_workflowExecution = Lens.lens (\ChildWorkflowExecutionTimedOutEventAttributes' {workflowExecution} -> workflowExecution) (\s@ChildWorkflowExecutionTimedOutEventAttributes' {} a -> s {workflowExecution = a} :: ChildWorkflowExecutionTimedOutEventAttributes)

-- | The type of the child workflow execution.
childWorkflowExecutionTimedOutEventAttributes_workflowType :: Lens.Lens' ChildWorkflowExecutionTimedOutEventAttributes WorkflowType
childWorkflowExecutionTimedOutEventAttributes_workflowType = Lens.lens (\ChildWorkflowExecutionTimedOutEventAttributes' {workflowType} -> workflowType) (\s@ChildWorkflowExecutionTimedOutEventAttributes' {} a -> s {workflowType = a} :: ChildWorkflowExecutionTimedOutEventAttributes)

-- | The type of the timeout that caused the child workflow execution to time
-- out.
childWorkflowExecutionTimedOutEventAttributes_timeoutType :: Lens.Lens' ChildWorkflowExecutionTimedOutEventAttributes WorkflowExecutionTimeoutType
childWorkflowExecutionTimedOutEventAttributes_timeoutType = Lens.lens (\ChildWorkflowExecutionTimedOutEventAttributes' {timeoutType} -> timeoutType) (\s@ChildWorkflowExecutionTimedOutEventAttributes' {} a -> s {timeoutType = a} :: ChildWorkflowExecutionTimedOutEventAttributes)

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding
-- to the @StartChildWorkflowExecution@ Decision to start this child
-- workflow execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
childWorkflowExecutionTimedOutEventAttributes_initiatedEventId :: Lens.Lens' ChildWorkflowExecutionTimedOutEventAttributes Prelude.Integer
childWorkflowExecutionTimedOutEventAttributes_initiatedEventId = Lens.lens (\ChildWorkflowExecutionTimedOutEventAttributes' {initiatedEventId} -> initiatedEventId) (\s@ChildWorkflowExecutionTimedOutEventAttributes' {} a -> s {initiatedEventId = a} :: ChildWorkflowExecutionTimedOutEventAttributes)

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this
-- child workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
childWorkflowExecutionTimedOutEventAttributes_startedEventId :: Lens.Lens' ChildWorkflowExecutionTimedOutEventAttributes Prelude.Integer
childWorkflowExecutionTimedOutEventAttributes_startedEventId = Lens.lens (\ChildWorkflowExecutionTimedOutEventAttributes' {startedEventId} -> startedEventId) (\s@ChildWorkflowExecutionTimedOutEventAttributes' {} a -> s {startedEventId = a} :: ChildWorkflowExecutionTimedOutEventAttributes)

instance
  Data.FromJSON
    ChildWorkflowExecutionTimedOutEventAttributes
  where
  parseJSON =
    Data.withObject
      "ChildWorkflowExecutionTimedOutEventAttributes"
      ( \x ->
          ChildWorkflowExecutionTimedOutEventAttributes'
            Prelude.<$> (x Data..: "workflowExecution")
            Prelude.<*> (x Data..: "workflowType")
            Prelude.<*> (x Data..: "timeoutType")
            Prelude.<*> (x Data..: "initiatedEventId")
            Prelude.<*> (x Data..: "startedEventId")
      )

instance
  Prelude.Hashable
    ChildWorkflowExecutionTimedOutEventAttributes
  where
  hashWithSalt
    _salt
    ChildWorkflowExecutionTimedOutEventAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` workflowExecution
        `Prelude.hashWithSalt` workflowType
        `Prelude.hashWithSalt` timeoutType
        `Prelude.hashWithSalt` initiatedEventId
        `Prelude.hashWithSalt` startedEventId

instance
  Prelude.NFData
    ChildWorkflowExecutionTimedOutEventAttributes
  where
  rnf
    ChildWorkflowExecutionTimedOutEventAttributes' {..} =
      Prelude.rnf workflowExecution
        `Prelude.seq` Prelude.rnf workflowType
        `Prelude.seq` Prelude.rnf timeoutType
        `Prelude.seq` Prelude.rnf initiatedEventId
        `Prelude.seq` Prelude.rnf startedEventId
