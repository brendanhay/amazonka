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
-- Module      : Amazonka.SWF.Types.WorkflowExecutionTerminatedEventAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.WorkflowExecutionTerminatedEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SWF.Types.ChildPolicy
import Amazonka.SWF.Types.WorkflowExecutionTerminatedCause

-- | Provides the details of the @WorkflowExecutionTerminated@ event.
--
-- /See:/ 'newWorkflowExecutionTerminatedEventAttributes' smart constructor.
data WorkflowExecutionTerminatedEventAttributes = WorkflowExecutionTerminatedEventAttributes'
  { -- | If set, indicates that the workflow execution was automatically
    -- terminated, and specifies the cause. This happens if the parent workflow
    -- execution times out or is terminated and the child policy is set to
    -- terminate child executions.
    cause :: Prelude.Maybe WorkflowExecutionTerminatedCause,
    -- | The details provided for the termination.
    details :: Prelude.Maybe Prelude.Text,
    -- | The reason provided for the termination.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The policy used for the child workflow executions of this workflow
    -- execution.
    --
    -- The supported child policies are:
    --
    -- -   @TERMINATE@ – The child executions are terminated.
    --
    -- -   @REQUEST_CANCEL@ – A request to cancel is attempted for each child
    --     execution by recording a @WorkflowExecutionCancelRequested@ event in
    --     its history. It is up to the decider to take appropriate actions
    --     when it receives an execution history with this event.
    --
    -- -   @ABANDON@ – No action is taken. The child executions continue to
    --     run.
    childPolicy :: ChildPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowExecutionTerminatedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cause', 'workflowExecutionTerminatedEventAttributes_cause' - If set, indicates that the workflow execution was automatically
-- terminated, and specifies the cause. This happens if the parent workflow
-- execution times out or is terminated and the child policy is set to
-- terminate child executions.
--
-- 'details', 'workflowExecutionTerminatedEventAttributes_details' - The details provided for the termination.
--
-- 'reason', 'workflowExecutionTerminatedEventAttributes_reason' - The reason provided for the termination.
--
-- 'childPolicy', 'workflowExecutionTerminatedEventAttributes_childPolicy' - The policy used for the child workflow executions of this workflow
-- execution.
--
-- The supported child policies are:
--
-- -   @TERMINATE@ – The child executions are terminated.
--
-- -   @REQUEST_CANCEL@ – A request to cancel is attempted for each child
--     execution by recording a @WorkflowExecutionCancelRequested@ event in
--     its history. It is up to the decider to take appropriate actions
--     when it receives an execution history with this event.
--
-- -   @ABANDON@ – No action is taken. The child executions continue to
--     run.
newWorkflowExecutionTerminatedEventAttributes ::
  -- | 'childPolicy'
  ChildPolicy ->
  WorkflowExecutionTerminatedEventAttributes
newWorkflowExecutionTerminatedEventAttributes
  pChildPolicy_ =
    WorkflowExecutionTerminatedEventAttributes'
      { cause =
          Prelude.Nothing,
        details = Prelude.Nothing,
        reason = Prelude.Nothing,
        childPolicy = pChildPolicy_
      }

-- | If set, indicates that the workflow execution was automatically
-- terminated, and specifies the cause. This happens if the parent workflow
-- execution times out or is terminated and the child policy is set to
-- terminate child executions.
workflowExecutionTerminatedEventAttributes_cause :: Lens.Lens' WorkflowExecutionTerminatedEventAttributes (Prelude.Maybe WorkflowExecutionTerminatedCause)
workflowExecutionTerminatedEventAttributes_cause = Lens.lens (\WorkflowExecutionTerminatedEventAttributes' {cause} -> cause) (\s@WorkflowExecutionTerminatedEventAttributes' {} a -> s {cause = a} :: WorkflowExecutionTerminatedEventAttributes)

-- | The details provided for the termination.
workflowExecutionTerminatedEventAttributes_details :: Lens.Lens' WorkflowExecutionTerminatedEventAttributes (Prelude.Maybe Prelude.Text)
workflowExecutionTerminatedEventAttributes_details = Lens.lens (\WorkflowExecutionTerminatedEventAttributes' {details} -> details) (\s@WorkflowExecutionTerminatedEventAttributes' {} a -> s {details = a} :: WorkflowExecutionTerminatedEventAttributes)

-- | The reason provided for the termination.
workflowExecutionTerminatedEventAttributes_reason :: Lens.Lens' WorkflowExecutionTerminatedEventAttributes (Prelude.Maybe Prelude.Text)
workflowExecutionTerminatedEventAttributes_reason = Lens.lens (\WorkflowExecutionTerminatedEventAttributes' {reason} -> reason) (\s@WorkflowExecutionTerminatedEventAttributes' {} a -> s {reason = a} :: WorkflowExecutionTerminatedEventAttributes)

-- | The policy used for the child workflow executions of this workflow
-- execution.
--
-- The supported child policies are:
--
-- -   @TERMINATE@ – The child executions are terminated.
--
-- -   @REQUEST_CANCEL@ – A request to cancel is attempted for each child
--     execution by recording a @WorkflowExecutionCancelRequested@ event in
--     its history. It is up to the decider to take appropriate actions
--     when it receives an execution history with this event.
--
-- -   @ABANDON@ – No action is taken. The child executions continue to
--     run.
workflowExecutionTerminatedEventAttributes_childPolicy :: Lens.Lens' WorkflowExecutionTerminatedEventAttributes ChildPolicy
workflowExecutionTerminatedEventAttributes_childPolicy = Lens.lens (\WorkflowExecutionTerminatedEventAttributes' {childPolicy} -> childPolicy) (\s@WorkflowExecutionTerminatedEventAttributes' {} a -> s {childPolicy = a} :: WorkflowExecutionTerminatedEventAttributes)

instance
  Data.FromJSON
    WorkflowExecutionTerminatedEventAttributes
  where
  parseJSON =
    Data.withObject
      "WorkflowExecutionTerminatedEventAttributes"
      ( \x ->
          WorkflowExecutionTerminatedEventAttributes'
            Prelude.<$> (x Data..:? "cause")
            Prelude.<*> (x Data..:? "details")
            Prelude.<*> (x Data..:? "reason")
            Prelude.<*> (x Data..: "childPolicy")
      )

instance
  Prelude.Hashable
    WorkflowExecutionTerminatedEventAttributes
  where
  hashWithSalt
    _salt
    WorkflowExecutionTerminatedEventAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` cause
        `Prelude.hashWithSalt` details
        `Prelude.hashWithSalt` reason
        `Prelude.hashWithSalt` childPolicy

instance
  Prelude.NFData
    WorkflowExecutionTerminatedEventAttributes
  where
  rnf WorkflowExecutionTerminatedEventAttributes' {..} =
    Prelude.rnf cause `Prelude.seq`
      Prelude.rnf details `Prelude.seq`
        Prelude.rnf reason `Prelude.seq`
          Prelude.rnf childPolicy
