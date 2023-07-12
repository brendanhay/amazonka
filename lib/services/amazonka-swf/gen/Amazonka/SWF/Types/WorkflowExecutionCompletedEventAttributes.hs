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
-- Module      : Amazonka.SWF.Types.WorkflowExecutionCompletedEventAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.WorkflowExecutionCompletedEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the details of the @WorkflowExecutionCompleted@ event.
--
-- /See:/ 'newWorkflowExecutionCompletedEventAttributes' smart constructor.
data WorkflowExecutionCompletedEventAttributes = WorkflowExecutionCompletedEventAttributes'
  { -- | The result produced by the workflow execution upon successful
    -- completion.
    result :: Prelude.Maybe Prelude.Text,
    -- | The ID of the @DecisionTaskCompleted@ event corresponding to the
    -- decision task that resulted in the @CompleteWorkflowExecution@ decision
    -- to complete this execution. This information can be useful for
    -- diagnosing problems by tracing back the chain of events leading up to
    -- this event.
    decisionTaskCompletedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowExecutionCompletedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'result', 'workflowExecutionCompletedEventAttributes_result' - The result produced by the workflow execution upon successful
-- completion.
--
-- 'decisionTaskCompletedEventId', 'workflowExecutionCompletedEventAttributes_decisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @CompleteWorkflowExecution@ decision
-- to complete this execution. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
newWorkflowExecutionCompletedEventAttributes ::
  -- | 'decisionTaskCompletedEventId'
  Prelude.Integer ->
  WorkflowExecutionCompletedEventAttributes
newWorkflowExecutionCompletedEventAttributes
  pDecisionTaskCompletedEventId_ =
    WorkflowExecutionCompletedEventAttributes'
      { result =
          Prelude.Nothing,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | The result produced by the workflow execution upon successful
-- completion.
workflowExecutionCompletedEventAttributes_result :: Lens.Lens' WorkflowExecutionCompletedEventAttributes (Prelude.Maybe Prelude.Text)
workflowExecutionCompletedEventAttributes_result = Lens.lens (\WorkflowExecutionCompletedEventAttributes' {result} -> result) (\s@WorkflowExecutionCompletedEventAttributes' {} a -> s {result = a} :: WorkflowExecutionCompletedEventAttributes)

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @CompleteWorkflowExecution@ decision
-- to complete this execution. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
workflowExecutionCompletedEventAttributes_decisionTaskCompletedEventId :: Lens.Lens' WorkflowExecutionCompletedEventAttributes Prelude.Integer
workflowExecutionCompletedEventAttributes_decisionTaskCompletedEventId = Lens.lens (\WorkflowExecutionCompletedEventAttributes' {decisionTaskCompletedEventId} -> decisionTaskCompletedEventId) (\s@WorkflowExecutionCompletedEventAttributes' {} a -> s {decisionTaskCompletedEventId = a} :: WorkflowExecutionCompletedEventAttributes)

instance
  Data.FromJSON
    WorkflowExecutionCompletedEventAttributes
  where
  parseJSON =
    Data.withObject
      "WorkflowExecutionCompletedEventAttributes"
      ( \x ->
          WorkflowExecutionCompletedEventAttributes'
            Prelude.<$> (x Data..:? "result")
            Prelude.<*> (x Data..: "decisionTaskCompletedEventId")
      )

instance
  Prelude.Hashable
    WorkflowExecutionCompletedEventAttributes
  where
  hashWithSalt
    _salt
    WorkflowExecutionCompletedEventAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` result
        `Prelude.hashWithSalt` decisionTaskCompletedEventId

instance
  Prelude.NFData
    WorkflowExecutionCompletedEventAttributes
  where
  rnf WorkflowExecutionCompletedEventAttributes' {..} =
    Prelude.rnf result
      `Prelude.seq` Prelude.rnf decisionTaskCompletedEventId
