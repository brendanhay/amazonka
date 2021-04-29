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
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionTerminatedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionTerminatedEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SWF.Types.ChildPolicy
import Network.AWS.SWF.Types.WorkflowExecutionTerminatedCause

-- | Provides the details of the @WorkflowExecutionTerminated@ event.
--
-- /See:/ 'newWorkflowExecutionTerminatedEventAttributes' smart constructor.
data WorkflowExecutionTerminatedEventAttributes = WorkflowExecutionTerminatedEventAttributes'
  { -- | The details provided for the termination.
    details :: Prelude.Maybe Prelude.Text,
    -- | The reason provided for the termination.
    reason :: Prelude.Maybe Prelude.Text,
    -- | If set, indicates that the workflow execution was automatically
    -- terminated, and specifies the cause. This happens if the parent workflow
    -- execution times out or is terminated and the child policy is set to
    -- terminate child executions.
    cause :: Prelude.Maybe WorkflowExecutionTerminatedCause,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WorkflowExecutionTerminatedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'workflowExecutionTerminatedEventAttributes_details' - The details provided for the termination.
--
-- 'reason', 'workflowExecutionTerminatedEventAttributes_reason' - The reason provided for the termination.
--
-- 'cause', 'workflowExecutionTerminatedEventAttributes_cause' - If set, indicates that the workflow execution was automatically
-- terminated, and specifies the cause. This happens if the parent workflow
-- execution times out or is terminated and the child policy is set to
-- terminate child executions.
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
      { details =
          Prelude.Nothing,
        reason = Prelude.Nothing,
        cause = Prelude.Nothing,
        childPolicy = pChildPolicy_
      }

-- | The details provided for the termination.
workflowExecutionTerminatedEventAttributes_details :: Lens.Lens' WorkflowExecutionTerminatedEventAttributes (Prelude.Maybe Prelude.Text)
workflowExecutionTerminatedEventAttributes_details = Lens.lens (\WorkflowExecutionTerminatedEventAttributes' {details} -> details) (\s@WorkflowExecutionTerminatedEventAttributes' {} a -> s {details = a} :: WorkflowExecutionTerminatedEventAttributes)

-- | The reason provided for the termination.
workflowExecutionTerminatedEventAttributes_reason :: Lens.Lens' WorkflowExecutionTerminatedEventAttributes (Prelude.Maybe Prelude.Text)
workflowExecutionTerminatedEventAttributes_reason = Lens.lens (\WorkflowExecutionTerminatedEventAttributes' {reason} -> reason) (\s@WorkflowExecutionTerminatedEventAttributes' {} a -> s {reason = a} :: WorkflowExecutionTerminatedEventAttributes)

-- | If set, indicates that the workflow execution was automatically
-- terminated, and specifies the cause. This happens if the parent workflow
-- execution times out or is terminated and the child policy is set to
-- terminate child executions.
workflowExecutionTerminatedEventAttributes_cause :: Lens.Lens' WorkflowExecutionTerminatedEventAttributes (Prelude.Maybe WorkflowExecutionTerminatedCause)
workflowExecutionTerminatedEventAttributes_cause = Lens.lens (\WorkflowExecutionTerminatedEventAttributes' {cause} -> cause) (\s@WorkflowExecutionTerminatedEventAttributes' {} a -> s {cause = a} :: WorkflowExecutionTerminatedEventAttributes)

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
  Prelude.FromJSON
    WorkflowExecutionTerminatedEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "WorkflowExecutionTerminatedEventAttributes"
      ( \x ->
          WorkflowExecutionTerminatedEventAttributes'
            Prelude.<$> (x Prelude..:? "details")
              Prelude.<*> (x Prelude..:? "reason")
              Prelude.<*> (x Prelude..:? "cause")
              Prelude.<*> (x Prelude..: "childPolicy")
      )

instance
  Prelude.Hashable
    WorkflowExecutionTerminatedEventAttributes

instance
  Prelude.NFData
    WorkflowExecutionTerminatedEventAttributes
