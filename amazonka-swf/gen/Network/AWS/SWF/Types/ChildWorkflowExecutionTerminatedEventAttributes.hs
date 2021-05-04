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
-- Module      : Network.AWS.SWF.Types.ChildWorkflowExecutionTerminatedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ChildWorkflowExecutionTerminatedEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SWF.Types.WorkflowExecution
import Network.AWS.SWF.Types.WorkflowType

-- | Provides the details of the @ChildWorkflowExecutionTerminated@ event.
--
-- /See:/ 'newChildWorkflowExecutionTerminatedEventAttributes' smart constructor.
data ChildWorkflowExecutionTerminatedEventAttributes = ChildWorkflowExecutionTerminatedEventAttributes'
  { -- | The child workflow execution that was terminated.
    workflowExecution :: WorkflowExecution,
    -- | The type of the child workflow execution.
    workflowType :: WorkflowType,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ChildWorkflowExecutionTerminatedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workflowExecution', 'childWorkflowExecutionTerminatedEventAttributes_workflowExecution' - The child workflow execution that was terminated.
--
-- 'workflowType', 'childWorkflowExecutionTerminatedEventAttributes_workflowType' - The type of the child workflow execution.
--
-- 'initiatedEventId', 'childWorkflowExecutionTerminatedEventAttributes_initiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding
-- to the @StartChildWorkflowExecution@ Decision to start this child
-- workflow execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
--
-- 'startedEventId', 'childWorkflowExecutionTerminatedEventAttributes_startedEventId' - The ID of the @ChildWorkflowExecutionStarted@ event recorded when this
-- child workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
newChildWorkflowExecutionTerminatedEventAttributes ::
  -- | 'workflowExecution'
  WorkflowExecution ->
  -- | 'workflowType'
  WorkflowType ->
  -- | 'initiatedEventId'
  Prelude.Integer ->
  -- | 'startedEventId'
  Prelude.Integer ->
  ChildWorkflowExecutionTerminatedEventAttributes
newChildWorkflowExecutionTerminatedEventAttributes
  pWorkflowExecution_
  pWorkflowType_
  pInitiatedEventId_
  pStartedEventId_ =
    ChildWorkflowExecutionTerminatedEventAttributes'
      { workflowExecution =
          pWorkflowExecution_,
        workflowType =
          pWorkflowType_,
        initiatedEventId =
          pInitiatedEventId_,
        startedEventId =
          pStartedEventId_
      }

-- | The child workflow execution that was terminated.
childWorkflowExecutionTerminatedEventAttributes_workflowExecution :: Lens.Lens' ChildWorkflowExecutionTerminatedEventAttributes WorkflowExecution
childWorkflowExecutionTerminatedEventAttributes_workflowExecution = Lens.lens (\ChildWorkflowExecutionTerminatedEventAttributes' {workflowExecution} -> workflowExecution) (\s@ChildWorkflowExecutionTerminatedEventAttributes' {} a -> s {workflowExecution = a} :: ChildWorkflowExecutionTerminatedEventAttributes)

-- | The type of the child workflow execution.
childWorkflowExecutionTerminatedEventAttributes_workflowType :: Lens.Lens' ChildWorkflowExecutionTerminatedEventAttributes WorkflowType
childWorkflowExecutionTerminatedEventAttributes_workflowType = Lens.lens (\ChildWorkflowExecutionTerminatedEventAttributes' {workflowType} -> workflowType) (\s@ChildWorkflowExecutionTerminatedEventAttributes' {} a -> s {workflowType = a} :: ChildWorkflowExecutionTerminatedEventAttributes)

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding
-- to the @StartChildWorkflowExecution@ Decision to start this child
-- workflow execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
childWorkflowExecutionTerminatedEventAttributes_initiatedEventId :: Lens.Lens' ChildWorkflowExecutionTerminatedEventAttributes Prelude.Integer
childWorkflowExecutionTerminatedEventAttributes_initiatedEventId = Lens.lens (\ChildWorkflowExecutionTerminatedEventAttributes' {initiatedEventId} -> initiatedEventId) (\s@ChildWorkflowExecutionTerminatedEventAttributes' {} a -> s {initiatedEventId = a} :: ChildWorkflowExecutionTerminatedEventAttributes)

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this
-- child workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
childWorkflowExecutionTerminatedEventAttributes_startedEventId :: Lens.Lens' ChildWorkflowExecutionTerminatedEventAttributes Prelude.Integer
childWorkflowExecutionTerminatedEventAttributes_startedEventId = Lens.lens (\ChildWorkflowExecutionTerminatedEventAttributes' {startedEventId} -> startedEventId) (\s@ChildWorkflowExecutionTerminatedEventAttributes' {} a -> s {startedEventId = a} :: ChildWorkflowExecutionTerminatedEventAttributes)

instance
  Prelude.FromJSON
    ChildWorkflowExecutionTerminatedEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "ChildWorkflowExecutionTerminatedEventAttributes"
      ( \x ->
          ChildWorkflowExecutionTerminatedEventAttributes'
            Prelude.<$> (x Prelude..: "workflowExecution")
              Prelude.<*> (x Prelude..: "workflowType")
              Prelude.<*> (x Prelude..: "initiatedEventId")
              Prelude.<*> (x Prelude..: "startedEventId")
      )

instance
  Prelude.Hashable
    ChildWorkflowExecutionTerminatedEventAttributes

instance
  Prelude.NFData
    ChildWorkflowExecutionTerminatedEventAttributes
