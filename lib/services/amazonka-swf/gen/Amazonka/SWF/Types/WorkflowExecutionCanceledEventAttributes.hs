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
-- Module      : Amazonka.SWF.Types.WorkflowExecutionCanceledEventAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.WorkflowExecutionCanceledEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the details of the @WorkflowExecutionCanceled@ event.
--
-- /See:/ 'newWorkflowExecutionCanceledEventAttributes' smart constructor.
data WorkflowExecutionCanceledEventAttributes = WorkflowExecutionCanceledEventAttributes'
  { -- | The details of the cancellation.
    details :: Prelude.Maybe Prelude.Text,
    -- | The ID of the @DecisionTaskCompleted@ event corresponding to the
    -- decision task that resulted in the @CancelWorkflowExecution@ decision
    -- for this cancellation request. This information can be useful for
    -- diagnosing problems by tracing back the chain of events leading up to
    -- this event.
    decisionTaskCompletedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowExecutionCanceledEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'workflowExecutionCanceledEventAttributes_details' - The details of the cancellation.
--
-- 'decisionTaskCompletedEventId', 'workflowExecutionCanceledEventAttributes_decisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @CancelWorkflowExecution@ decision
-- for this cancellation request. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
newWorkflowExecutionCanceledEventAttributes ::
  -- | 'decisionTaskCompletedEventId'
  Prelude.Integer ->
  WorkflowExecutionCanceledEventAttributes
newWorkflowExecutionCanceledEventAttributes
  pDecisionTaskCompletedEventId_ =
    WorkflowExecutionCanceledEventAttributes'
      { details =
          Prelude.Nothing,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | The details of the cancellation.
workflowExecutionCanceledEventAttributes_details :: Lens.Lens' WorkflowExecutionCanceledEventAttributes (Prelude.Maybe Prelude.Text)
workflowExecutionCanceledEventAttributes_details = Lens.lens (\WorkflowExecutionCanceledEventAttributes' {details} -> details) (\s@WorkflowExecutionCanceledEventAttributes' {} a -> s {details = a} :: WorkflowExecutionCanceledEventAttributes)

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @CancelWorkflowExecution@ decision
-- for this cancellation request. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
workflowExecutionCanceledEventAttributes_decisionTaskCompletedEventId :: Lens.Lens' WorkflowExecutionCanceledEventAttributes Prelude.Integer
workflowExecutionCanceledEventAttributes_decisionTaskCompletedEventId = Lens.lens (\WorkflowExecutionCanceledEventAttributes' {decisionTaskCompletedEventId} -> decisionTaskCompletedEventId) (\s@WorkflowExecutionCanceledEventAttributes' {} a -> s {decisionTaskCompletedEventId = a} :: WorkflowExecutionCanceledEventAttributes)

instance
  Data.FromJSON
    WorkflowExecutionCanceledEventAttributes
  where
  parseJSON =
    Data.withObject
      "WorkflowExecutionCanceledEventAttributes"
      ( \x ->
          WorkflowExecutionCanceledEventAttributes'
            Prelude.<$> (x Data..:? "details")
            Prelude.<*> (x Data..: "decisionTaskCompletedEventId")
      )

instance
  Prelude.Hashable
    WorkflowExecutionCanceledEventAttributes
  where
  hashWithSalt
    _salt
    WorkflowExecutionCanceledEventAttributes' {..} =
      _salt `Prelude.hashWithSalt` details
        `Prelude.hashWithSalt` decisionTaskCompletedEventId

instance
  Prelude.NFData
    WorkflowExecutionCanceledEventAttributes
  where
  rnf WorkflowExecutionCanceledEventAttributes' {..} =
    Prelude.rnf details
      `Prelude.seq` Prelude.rnf decisionTaskCompletedEventId
