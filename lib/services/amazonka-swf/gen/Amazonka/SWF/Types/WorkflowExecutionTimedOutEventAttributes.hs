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
-- Module      : Amazonka.SWF.Types.WorkflowExecutionTimedOutEventAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.WorkflowExecutionTimedOutEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SWF.Types.ChildPolicy
import Amazonka.SWF.Types.WorkflowExecutionTimeoutType

-- | Provides the details of the @WorkflowExecutionTimedOut@ event.
--
-- /See:/ 'newWorkflowExecutionTimedOutEventAttributes' smart constructor.
data WorkflowExecutionTimedOutEventAttributes = WorkflowExecutionTimedOutEventAttributes'
  { -- | The type of timeout that caused this event.
    timeoutType :: WorkflowExecutionTimeoutType,
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
-- Create a value of 'WorkflowExecutionTimedOutEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeoutType', 'workflowExecutionTimedOutEventAttributes_timeoutType' - The type of timeout that caused this event.
--
-- 'childPolicy', 'workflowExecutionTimedOutEventAttributes_childPolicy' - The policy used for the child workflow executions of this workflow
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
newWorkflowExecutionTimedOutEventAttributes ::
  -- | 'timeoutType'
  WorkflowExecutionTimeoutType ->
  -- | 'childPolicy'
  ChildPolicy ->
  WorkflowExecutionTimedOutEventAttributes
newWorkflowExecutionTimedOutEventAttributes
  pTimeoutType_
  pChildPolicy_ =
    WorkflowExecutionTimedOutEventAttributes'
      { timeoutType =
          pTimeoutType_,
        childPolicy = pChildPolicy_
      }

-- | The type of timeout that caused this event.
workflowExecutionTimedOutEventAttributes_timeoutType :: Lens.Lens' WorkflowExecutionTimedOutEventAttributes WorkflowExecutionTimeoutType
workflowExecutionTimedOutEventAttributes_timeoutType = Lens.lens (\WorkflowExecutionTimedOutEventAttributes' {timeoutType} -> timeoutType) (\s@WorkflowExecutionTimedOutEventAttributes' {} a -> s {timeoutType = a} :: WorkflowExecutionTimedOutEventAttributes)

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
workflowExecutionTimedOutEventAttributes_childPolicy :: Lens.Lens' WorkflowExecutionTimedOutEventAttributes ChildPolicy
workflowExecutionTimedOutEventAttributes_childPolicy = Lens.lens (\WorkflowExecutionTimedOutEventAttributes' {childPolicy} -> childPolicy) (\s@WorkflowExecutionTimedOutEventAttributes' {} a -> s {childPolicy = a} :: WorkflowExecutionTimedOutEventAttributes)

instance
  Data.FromJSON
    WorkflowExecutionTimedOutEventAttributes
  where
  parseJSON =
    Data.withObject
      "WorkflowExecutionTimedOutEventAttributes"
      ( \x ->
          WorkflowExecutionTimedOutEventAttributes'
            Prelude.<$> (x Data..: "timeoutType")
            Prelude.<*> (x Data..: "childPolicy")
      )

instance
  Prelude.Hashable
    WorkflowExecutionTimedOutEventAttributes
  where
  hashWithSalt
    _salt
    WorkflowExecutionTimedOutEventAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` timeoutType
        `Prelude.hashWithSalt` childPolicy

instance
  Prelude.NFData
    WorkflowExecutionTimedOutEventAttributes
  where
  rnf WorkflowExecutionTimedOutEventAttributes' {..} =
    Prelude.rnf timeoutType
      `Prelude.seq` Prelude.rnf childPolicy
