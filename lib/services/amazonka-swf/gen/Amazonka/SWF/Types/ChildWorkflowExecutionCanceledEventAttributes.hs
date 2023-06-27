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
-- Module      : Amazonka.SWF.Types.ChildWorkflowExecutionCanceledEventAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.ChildWorkflowExecutionCanceledEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SWF.Types.WorkflowExecution
import Amazonka.SWF.Types.WorkflowType

-- | Provide details of the @ChildWorkflowExecutionCanceled@ event.
--
-- /See:/ 'newChildWorkflowExecutionCanceledEventAttributes' smart constructor.
data ChildWorkflowExecutionCanceledEventAttributes = ChildWorkflowExecutionCanceledEventAttributes'
  { -- | Details of the cancellation (if provided).
    details :: Prelude.Maybe Prelude.Text,
    -- | The child workflow execution that was canceled.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChildWorkflowExecutionCanceledEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'childWorkflowExecutionCanceledEventAttributes_details' - Details of the cancellation (if provided).
--
-- 'workflowExecution', 'childWorkflowExecutionCanceledEventAttributes_workflowExecution' - The child workflow execution that was canceled.
--
-- 'workflowType', 'childWorkflowExecutionCanceledEventAttributes_workflowType' - The type of the child workflow execution.
--
-- 'initiatedEventId', 'childWorkflowExecutionCanceledEventAttributes_initiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding
-- to the @StartChildWorkflowExecution@ Decision to start this child
-- workflow execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
--
-- 'startedEventId', 'childWorkflowExecutionCanceledEventAttributes_startedEventId' - The ID of the @ChildWorkflowExecutionStarted@ event recorded when this
-- child workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
newChildWorkflowExecutionCanceledEventAttributes ::
  -- | 'workflowExecution'
  WorkflowExecution ->
  -- | 'workflowType'
  WorkflowType ->
  -- | 'initiatedEventId'
  Prelude.Integer ->
  -- | 'startedEventId'
  Prelude.Integer ->
  ChildWorkflowExecutionCanceledEventAttributes
newChildWorkflowExecutionCanceledEventAttributes
  pWorkflowExecution_
  pWorkflowType_
  pInitiatedEventId_
  pStartedEventId_ =
    ChildWorkflowExecutionCanceledEventAttributes'
      { details =
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

-- | Details of the cancellation (if provided).
childWorkflowExecutionCanceledEventAttributes_details :: Lens.Lens' ChildWorkflowExecutionCanceledEventAttributes (Prelude.Maybe Prelude.Text)
childWorkflowExecutionCanceledEventAttributes_details = Lens.lens (\ChildWorkflowExecutionCanceledEventAttributes' {details} -> details) (\s@ChildWorkflowExecutionCanceledEventAttributes' {} a -> s {details = a} :: ChildWorkflowExecutionCanceledEventAttributes)

-- | The child workflow execution that was canceled.
childWorkflowExecutionCanceledEventAttributes_workflowExecution :: Lens.Lens' ChildWorkflowExecutionCanceledEventAttributes WorkflowExecution
childWorkflowExecutionCanceledEventAttributes_workflowExecution = Lens.lens (\ChildWorkflowExecutionCanceledEventAttributes' {workflowExecution} -> workflowExecution) (\s@ChildWorkflowExecutionCanceledEventAttributes' {} a -> s {workflowExecution = a} :: ChildWorkflowExecutionCanceledEventAttributes)

-- | The type of the child workflow execution.
childWorkflowExecutionCanceledEventAttributes_workflowType :: Lens.Lens' ChildWorkflowExecutionCanceledEventAttributes WorkflowType
childWorkflowExecutionCanceledEventAttributes_workflowType = Lens.lens (\ChildWorkflowExecutionCanceledEventAttributes' {workflowType} -> workflowType) (\s@ChildWorkflowExecutionCanceledEventAttributes' {} a -> s {workflowType = a} :: ChildWorkflowExecutionCanceledEventAttributes)

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding
-- to the @StartChildWorkflowExecution@ Decision to start this child
-- workflow execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
childWorkflowExecutionCanceledEventAttributes_initiatedEventId :: Lens.Lens' ChildWorkflowExecutionCanceledEventAttributes Prelude.Integer
childWorkflowExecutionCanceledEventAttributes_initiatedEventId = Lens.lens (\ChildWorkflowExecutionCanceledEventAttributes' {initiatedEventId} -> initiatedEventId) (\s@ChildWorkflowExecutionCanceledEventAttributes' {} a -> s {initiatedEventId = a} :: ChildWorkflowExecutionCanceledEventAttributes)

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this
-- child workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
childWorkflowExecutionCanceledEventAttributes_startedEventId :: Lens.Lens' ChildWorkflowExecutionCanceledEventAttributes Prelude.Integer
childWorkflowExecutionCanceledEventAttributes_startedEventId = Lens.lens (\ChildWorkflowExecutionCanceledEventAttributes' {startedEventId} -> startedEventId) (\s@ChildWorkflowExecutionCanceledEventAttributes' {} a -> s {startedEventId = a} :: ChildWorkflowExecutionCanceledEventAttributes)

instance
  Data.FromJSON
    ChildWorkflowExecutionCanceledEventAttributes
  where
  parseJSON =
    Data.withObject
      "ChildWorkflowExecutionCanceledEventAttributes"
      ( \x ->
          ChildWorkflowExecutionCanceledEventAttributes'
            Prelude.<$> (x Data..:? "details")
            Prelude.<*> (x Data..: "workflowExecution")
            Prelude.<*> (x Data..: "workflowType")
            Prelude.<*> (x Data..: "initiatedEventId")
            Prelude.<*> (x Data..: "startedEventId")
      )

instance
  Prelude.Hashable
    ChildWorkflowExecutionCanceledEventAttributes
  where
  hashWithSalt
    _salt
    ChildWorkflowExecutionCanceledEventAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` details
        `Prelude.hashWithSalt` workflowExecution
        `Prelude.hashWithSalt` workflowType
        `Prelude.hashWithSalt` initiatedEventId
        `Prelude.hashWithSalt` startedEventId

instance
  Prelude.NFData
    ChildWorkflowExecutionCanceledEventAttributes
  where
  rnf
    ChildWorkflowExecutionCanceledEventAttributes' {..} =
      Prelude.rnf details
        `Prelude.seq` Prelude.rnf workflowExecution
        `Prelude.seq` Prelude.rnf workflowType
        `Prelude.seq` Prelude.rnf initiatedEventId
        `Prelude.seq` Prelude.rnf startedEventId
