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
-- Module      : Network.AWS.SWF.Types.ChildWorkflowExecutionCompletedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ChildWorkflowExecutionCompletedEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SWF.Types.WorkflowExecution
import Network.AWS.SWF.Types.WorkflowType

-- | Provides the details of the @ChildWorkflowExecutionCompleted@ event.
--
-- /See:/ 'newChildWorkflowExecutionCompletedEventAttributes' smart constructor.
data ChildWorkflowExecutionCompletedEventAttributes = ChildWorkflowExecutionCompletedEventAttributes'
  { -- | The result of the child workflow execution.
    result :: Prelude.Maybe Prelude.Text,
    -- | The child workflow execution that was completed.
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
-- Create a value of 'ChildWorkflowExecutionCompletedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'result', 'childWorkflowExecutionCompletedEventAttributes_result' - The result of the child workflow execution.
--
-- 'workflowExecution', 'childWorkflowExecutionCompletedEventAttributes_workflowExecution' - The child workflow execution that was completed.
--
-- 'workflowType', 'childWorkflowExecutionCompletedEventAttributes_workflowType' - The type of the child workflow execution.
--
-- 'initiatedEventId', 'childWorkflowExecutionCompletedEventAttributes_initiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding
-- to the @StartChildWorkflowExecution@ Decision to start this child
-- workflow execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
--
-- 'startedEventId', 'childWorkflowExecutionCompletedEventAttributes_startedEventId' - The ID of the @ChildWorkflowExecutionStarted@ event recorded when this
-- child workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
newChildWorkflowExecutionCompletedEventAttributes ::
  -- | 'workflowExecution'
  WorkflowExecution ->
  -- | 'workflowType'
  WorkflowType ->
  -- | 'initiatedEventId'
  Prelude.Integer ->
  -- | 'startedEventId'
  Prelude.Integer ->
  ChildWorkflowExecutionCompletedEventAttributes
newChildWorkflowExecutionCompletedEventAttributes
  pWorkflowExecution_
  pWorkflowType_
  pInitiatedEventId_
  pStartedEventId_ =
    ChildWorkflowExecutionCompletedEventAttributes'
      { result =
          Prelude.Nothing,
        workflowExecution =
          pWorkflowExecution_,
        workflowType =
          pWorkflowType_,
        initiatedEventId =
          pInitiatedEventId_,
        startedEventId =
          pStartedEventId_
      }

-- | The result of the child workflow execution.
childWorkflowExecutionCompletedEventAttributes_result :: Lens.Lens' ChildWorkflowExecutionCompletedEventAttributes (Prelude.Maybe Prelude.Text)
childWorkflowExecutionCompletedEventAttributes_result = Lens.lens (\ChildWorkflowExecutionCompletedEventAttributes' {result} -> result) (\s@ChildWorkflowExecutionCompletedEventAttributes' {} a -> s {result = a} :: ChildWorkflowExecutionCompletedEventAttributes)

-- | The child workflow execution that was completed.
childWorkflowExecutionCompletedEventAttributes_workflowExecution :: Lens.Lens' ChildWorkflowExecutionCompletedEventAttributes WorkflowExecution
childWorkflowExecutionCompletedEventAttributes_workflowExecution = Lens.lens (\ChildWorkflowExecutionCompletedEventAttributes' {workflowExecution} -> workflowExecution) (\s@ChildWorkflowExecutionCompletedEventAttributes' {} a -> s {workflowExecution = a} :: ChildWorkflowExecutionCompletedEventAttributes)

-- | The type of the child workflow execution.
childWorkflowExecutionCompletedEventAttributes_workflowType :: Lens.Lens' ChildWorkflowExecutionCompletedEventAttributes WorkflowType
childWorkflowExecutionCompletedEventAttributes_workflowType = Lens.lens (\ChildWorkflowExecutionCompletedEventAttributes' {workflowType} -> workflowType) (\s@ChildWorkflowExecutionCompletedEventAttributes' {} a -> s {workflowType = a} :: ChildWorkflowExecutionCompletedEventAttributes)

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding
-- to the @StartChildWorkflowExecution@ Decision to start this child
-- workflow execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
childWorkflowExecutionCompletedEventAttributes_initiatedEventId :: Lens.Lens' ChildWorkflowExecutionCompletedEventAttributes Prelude.Integer
childWorkflowExecutionCompletedEventAttributes_initiatedEventId = Lens.lens (\ChildWorkflowExecutionCompletedEventAttributes' {initiatedEventId} -> initiatedEventId) (\s@ChildWorkflowExecutionCompletedEventAttributes' {} a -> s {initiatedEventId = a} :: ChildWorkflowExecutionCompletedEventAttributes)

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this
-- child workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
childWorkflowExecutionCompletedEventAttributes_startedEventId :: Lens.Lens' ChildWorkflowExecutionCompletedEventAttributes Prelude.Integer
childWorkflowExecutionCompletedEventAttributes_startedEventId = Lens.lens (\ChildWorkflowExecutionCompletedEventAttributes' {startedEventId} -> startedEventId) (\s@ChildWorkflowExecutionCompletedEventAttributes' {} a -> s {startedEventId = a} :: ChildWorkflowExecutionCompletedEventAttributes)

instance
  Prelude.FromJSON
    ChildWorkflowExecutionCompletedEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "ChildWorkflowExecutionCompletedEventAttributes"
      ( \x ->
          ChildWorkflowExecutionCompletedEventAttributes'
            Prelude.<$> (x Prelude..:? "result")
              Prelude.<*> (x Prelude..: "workflowExecution")
              Prelude.<*> (x Prelude..: "workflowType")
              Prelude.<*> (x Prelude..: "initiatedEventId")
              Prelude.<*> (x Prelude..: "startedEventId")
      )

instance
  Prelude.Hashable
    ChildWorkflowExecutionCompletedEventAttributes

instance
  Prelude.NFData
    ChildWorkflowExecutionCompletedEventAttributes
