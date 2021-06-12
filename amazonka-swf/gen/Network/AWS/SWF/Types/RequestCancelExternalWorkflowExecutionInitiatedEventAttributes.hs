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
-- Module      : Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionInitiatedEventAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides the details of the
-- @RequestCancelExternalWorkflowExecutionInitiated@ event.
--
-- /See:/ 'newRequestCancelExternalWorkflowExecutionInitiatedEventAttributes' smart constructor.
data RequestCancelExternalWorkflowExecutionInitiatedEventAttributes = RequestCancelExternalWorkflowExecutionInitiatedEventAttributes'
  { -- | The @runId@ of the external workflow execution to be canceled.
    runId :: Core.Maybe Core.Text,
    -- | Data attached to the event that can be used by the decider in subsequent
    -- workflow tasks.
    control :: Core.Maybe Core.Text,
    -- | The @workflowId@ of the external workflow execution to be canceled.
    workflowId :: Core.Text,
    -- | The ID of the @DecisionTaskCompleted@ event corresponding to the
    -- decision task that resulted in the
    -- @RequestCancelExternalWorkflowExecution@ decision for this cancellation
    -- request. This information can be useful for diagnosing problems by
    -- tracing back the chain of events leading up to this event.
    decisionTaskCompletedEventId :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RequestCancelExternalWorkflowExecutionInitiatedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runId', 'requestCancelExternalWorkflowExecutionInitiatedEventAttributes_runId' - The @runId@ of the external workflow execution to be canceled.
--
-- 'control', 'requestCancelExternalWorkflowExecutionInitiatedEventAttributes_control' - Data attached to the event that can be used by the decider in subsequent
-- workflow tasks.
--
-- 'workflowId', 'requestCancelExternalWorkflowExecutionInitiatedEventAttributes_workflowId' - The @workflowId@ of the external workflow execution to be canceled.
--
-- 'decisionTaskCompletedEventId', 'requestCancelExternalWorkflowExecutionInitiatedEventAttributes_decisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the
-- @RequestCancelExternalWorkflowExecution@ decision for this cancellation
-- request. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
newRequestCancelExternalWorkflowExecutionInitiatedEventAttributes ::
  -- | 'workflowId'
  Core.Text ->
  -- | 'decisionTaskCompletedEventId'
  Core.Integer ->
  RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
newRequestCancelExternalWorkflowExecutionInitiatedEventAttributes
  pWorkflowId_
  pDecisionTaskCompletedEventId_ =
    RequestCancelExternalWorkflowExecutionInitiatedEventAttributes'
      { runId =
          Core.Nothing,
        control =
          Core.Nothing,
        workflowId =
          pWorkflowId_,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | The @runId@ of the external workflow execution to be canceled.
requestCancelExternalWorkflowExecutionInitiatedEventAttributes_runId :: Lens.Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (Core.Maybe Core.Text)
requestCancelExternalWorkflowExecutionInitiatedEventAttributes_runId = Lens.lens (\RequestCancelExternalWorkflowExecutionInitiatedEventAttributes' {runId} -> runId) (\s@RequestCancelExternalWorkflowExecutionInitiatedEventAttributes' {} a -> s {runId = a} :: RequestCancelExternalWorkflowExecutionInitiatedEventAttributes)

-- | Data attached to the event that can be used by the decider in subsequent
-- workflow tasks.
requestCancelExternalWorkflowExecutionInitiatedEventAttributes_control :: Lens.Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (Core.Maybe Core.Text)
requestCancelExternalWorkflowExecutionInitiatedEventAttributes_control = Lens.lens (\RequestCancelExternalWorkflowExecutionInitiatedEventAttributes' {control} -> control) (\s@RequestCancelExternalWorkflowExecutionInitiatedEventAttributes' {} a -> s {control = a} :: RequestCancelExternalWorkflowExecutionInitiatedEventAttributes)

-- | The @workflowId@ of the external workflow execution to be canceled.
requestCancelExternalWorkflowExecutionInitiatedEventAttributes_workflowId :: Lens.Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes Core.Text
requestCancelExternalWorkflowExecutionInitiatedEventAttributes_workflowId = Lens.lens (\RequestCancelExternalWorkflowExecutionInitiatedEventAttributes' {workflowId} -> workflowId) (\s@RequestCancelExternalWorkflowExecutionInitiatedEventAttributes' {} a -> s {workflowId = a} :: RequestCancelExternalWorkflowExecutionInitiatedEventAttributes)

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the
-- @RequestCancelExternalWorkflowExecution@ decision for this cancellation
-- request. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
requestCancelExternalWorkflowExecutionInitiatedEventAttributes_decisionTaskCompletedEventId :: Lens.Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes Core.Integer
requestCancelExternalWorkflowExecutionInitiatedEventAttributes_decisionTaskCompletedEventId = Lens.lens (\RequestCancelExternalWorkflowExecutionInitiatedEventAttributes' {decisionTaskCompletedEventId} -> decisionTaskCompletedEventId) (\s@RequestCancelExternalWorkflowExecutionInitiatedEventAttributes' {} a -> s {decisionTaskCompletedEventId = a} :: RequestCancelExternalWorkflowExecutionInitiatedEventAttributes)

instance
  Core.FromJSON
    RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
  where
  parseJSON =
    Core.withObject
      "RequestCancelExternalWorkflowExecutionInitiatedEventAttributes"
      ( \x ->
          RequestCancelExternalWorkflowExecutionInitiatedEventAttributes'
            Core.<$> (x Core..:? "runId") Core.<*> (x Core..:? "control")
              Core.<*> (x Core..: "workflowId")
              Core.<*> (x Core..: "decisionTaskCompletedEventId")
      )

instance
  Core.Hashable
    RequestCancelExternalWorkflowExecutionInitiatedEventAttributes

instance
  Core.NFData
    RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
