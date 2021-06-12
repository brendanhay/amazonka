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
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionCanceledEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionCanceledEventAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides the details of the @WorkflowExecutionCanceled@ event.
--
-- /See:/ 'newWorkflowExecutionCanceledEventAttributes' smart constructor.
data WorkflowExecutionCanceledEventAttributes = WorkflowExecutionCanceledEventAttributes'
  { -- | The details of the cancellation.
    details :: Core.Maybe Core.Text,
    -- | The ID of the @DecisionTaskCompleted@ event corresponding to the
    -- decision task that resulted in the @CancelWorkflowExecution@ decision
    -- for this cancellation request. This information can be useful for
    -- diagnosing problems by tracing back the chain of events leading up to
    -- this event.
    decisionTaskCompletedEventId :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Integer ->
  WorkflowExecutionCanceledEventAttributes
newWorkflowExecutionCanceledEventAttributes
  pDecisionTaskCompletedEventId_ =
    WorkflowExecutionCanceledEventAttributes'
      { details =
          Core.Nothing,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | The details of the cancellation.
workflowExecutionCanceledEventAttributes_details :: Lens.Lens' WorkflowExecutionCanceledEventAttributes (Core.Maybe Core.Text)
workflowExecutionCanceledEventAttributes_details = Lens.lens (\WorkflowExecutionCanceledEventAttributes' {details} -> details) (\s@WorkflowExecutionCanceledEventAttributes' {} a -> s {details = a} :: WorkflowExecutionCanceledEventAttributes)

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @CancelWorkflowExecution@ decision
-- for this cancellation request. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
workflowExecutionCanceledEventAttributes_decisionTaskCompletedEventId :: Lens.Lens' WorkflowExecutionCanceledEventAttributes Core.Integer
workflowExecutionCanceledEventAttributes_decisionTaskCompletedEventId = Lens.lens (\WorkflowExecutionCanceledEventAttributes' {decisionTaskCompletedEventId} -> decisionTaskCompletedEventId) (\s@WorkflowExecutionCanceledEventAttributes' {} a -> s {decisionTaskCompletedEventId = a} :: WorkflowExecutionCanceledEventAttributes)

instance
  Core.FromJSON
    WorkflowExecutionCanceledEventAttributes
  where
  parseJSON =
    Core.withObject
      "WorkflowExecutionCanceledEventAttributes"
      ( \x ->
          WorkflowExecutionCanceledEventAttributes'
            Core.<$> (x Core..:? "details")
            Core.<*> (x Core..: "decisionTaskCompletedEventId")
      )

instance
  Core.Hashable
    WorkflowExecutionCanceledEventAttributes

instance
  Core.NFData
    WorkflowExecutionCanceledEventAttributes
