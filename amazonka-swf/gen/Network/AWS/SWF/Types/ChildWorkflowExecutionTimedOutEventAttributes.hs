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
-- Module      : Network.AWS.SWF.Types.ChildWorkflowExecutionTimedOutEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ChildWorkflowExecutionTimedOutEventAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SWF.Types.WorkflowExecution
import Network.AWS.SWF.Types.WorkflowExecutionTimeoutType
import Network.AWS.SWF.Types.WorkflowType

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
    initiatedEventId :: Core.Integer,
    -- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this
    -- child workflow execution was started. This information can be useful for
    -- diagnosing problems by tracing back the chain of events leading up to
    -- this event.
    startedEventId :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Integer ->
  -- | 'startedEventId'
  Core.Integer ->
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
childWorkflowExecutionTimedOutEventAttributes_initiatedEventId :: Lens.Lens' ChildWorkflowExecutionTimedOutEventAttributes Core.Integer
childWorkflowExecutionTimedOutEventAttributes_initiatedEventId = Lens.lens (\ChildWorkflowExecutionTimedOutEventAttributes' {initiatedEventId} -> initiatedEventId) (\s@ChildWorkflowExecutionTimedOutEventAttributes' {} a -> s {initiatedEventId = a} :: ChildWorkflowExecutionTimedOutEventAttributes)

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this
-- child workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
childWorkflowExecutionTimedOutEventAttributes_startedEventId :: Lens.Lens' ChildWorkflowExecutionTimedOutEventAttributes Core.Integer
childWorkflowExecutionTimedOutEventAttributes_startedEventId = Lens.lens (\ChildWorkflowExecutionTimedOutEventAttributes' {startedEventId} -> startedEventId) (\s@ChildWorkflowExecutionTimedOutEventAttributes' {} a -> s {startedEventId = a} :: ChildWorkflowExecutionTimedOutEventAttributes)

instance
  Core.FromJSON
    ChildWorkflowExecutionTimedOutEventAttributes
  where
  parseJSON =
    Core.withObject
      "ChildWorkflowExecutionTimedOutEventAttributes"
      ( \x ->
          ChildWorkflowExecutionTimedOutEventAttributes'
            Core.<$> (x Core..: "workflowExecution")
              Core.<*> (x Core..: "workflowType")
              Core.<*> (x Core..: "timeoutType")
              Core.<*> (x Core..: "initiatedEventId")
              Core.<*> (x Core..: "startedEventId")
      )

instance
  Core.Hashable
    ChildWorkflowExecutionTimedOutEventAttributes

instance
  Core.NFData
    ChildWorkflowExecutionTimedOutEventAttributes
