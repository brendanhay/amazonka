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
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionCompletedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionCompletedEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.FromJSON
    WorkflowExecutionCompletedEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "WorkflowExecutionCompletedEventAttributes"
      ( \x ->
          WorkflowExecutionCompletedEventAttributes'
            Prelude.<$> (x Prelude..:? "result")
              Prelude.<*> (x Prelude..: "decisionTaskCompletedEventId")
      )

instance
  Prelude.Hashable
    WorkflowExecutionCompletedEventAttributes

instance
  Prelude.NFData
    WorkflowExecutionCompletedEventAttributes
