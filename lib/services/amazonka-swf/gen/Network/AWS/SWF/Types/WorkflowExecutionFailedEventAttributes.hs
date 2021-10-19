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
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionFailedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionFailedEventAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the details of the @WorkflowExecutionFailed@ event.
--
-- /See:/ 'newWorkflowExecutionFailedEventAttributes' smart constructor.
data WorkflowExecutionFailedEventAttributes = WorkflowExecutionFailedEventAttributes'
  { -- | The descriptive reason provided for the failure.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The details of the failure.
    details :: Prelude.Maybe Prelude.Text,
    -- | The ID of the @DecisionTaskCompleted@ event corresponding to the
    -- decision task that resulted in the @FailWorkflowExecution@ decision to
    -- fail this execution. This information can be useful for diagnosing
    -- problems by tracing back the chain of events leading up to this event.
    decisionTaskCompletedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowExecutionFailedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reason', 'workflowExecutionFailedEventAttributes_reason' - The descriptive reason provided for the failure.
--
-- 'details', 'workflowExecutionFailedEventAttributes_details' - The details of the failure.
--
-- 'decisionTaskCompletedEventId', 'workflowExecutionFailedEventAttributes_decisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @FailWorkflowExecution@ decision to
-- fail this execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
newWorkflowExecutionFailedEventAttributes ::
  -- | 'decisionTaskCompletedEventId'
  Prelude.Integer ->
  WorkflowExecutionFailedEventAttributes
newWorkflowExecutionFailedEventAttributes
  pDecisionTaskCompletedEventId_ =
    WorkflowExecutionFailedEventAttributes'
      { reason =
          Prelude.Nothing,
        details = Prelude.Nothing,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | The descriptive reason provided for the failure.
workflowExecutionFailedEventAttributes_reason :: Lens.Lens' WorkflowExecutionFailedEventAttributes (Prelude.Maybe Prelude.Text)
workflowExecutionFailedEventAttributes_reason = Lens.lens (\WorkflowExecutionFailedEventAttributes' {reason} -> reason) (\s@WorkflowExecutionFailedEventAttributes' {} a -> s {reason = a} :: WorkflowExecutionFailedEventAttributes)

-- | The details of the failure.
workflowExecutionFailedEventAttributes_details :: Lens.Lens' WorkflowExecutionFailedEventAttributes (Prelude.Maybe Prelude.Text)
workflowExecutionFailedEventAttributes_details = Lens.lens (\WorkflowExecutionFailedEventAttributes' {details} -> details) (\s@WorkflowExecutionFailedEventAttributes' {} a -> s {details = a} :: WorkflowExecutionFailedEventAttributes)

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @FailWorkflowExecution@ decision to
-- fail this execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
workflowExecutionFailedEventAttributes_decisionTaskCompletedEventId :: Lens.Lens' WorkflowExecutionFailedEventAttributes Prelude.Integer
workflowExecutionFailedEventAttributes_decisionTaskCompletedEventId = Lens.lens (\WorkflowExecutionFailedEventAttributes' {decisionTaskCompletedEventId} -> decisionTaskCompletedEventId) (\s@WorkflowExecutionFailedEventAttributes' {} a -> s {decisionTaskCompletedEventId = a} :: WorkflowExecutionFailedEventAttributes)

instance
  Core.FromJSON
    WorkflowExecutionFailedEventAttributes
  where
  parseJSON =
    Core.withObject
      "WorkflowExecutionFailedEventAttributes"
      ( \x ->
          WorkflowExecutionFailedEventAttributes'
            Prelude.<$> (x Core..:? "reason")
            Prelude.<*> (x Core..:? "details")
            Prelude.<*> (x Core..: "decisionTaskCompletedEventId")
      )

instance
  Prelude.Hashable
    WorkflowExecutionFailedEventAttributes

instance
  Prelude.NFData
    WorkflowExecutionFailedEventAttributes
