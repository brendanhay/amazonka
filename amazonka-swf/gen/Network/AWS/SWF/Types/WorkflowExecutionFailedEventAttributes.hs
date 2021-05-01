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
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionFailedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionFailedEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the details of the @WorkflowExecutionFailed@ event.
--
-- /See:/ 'newWorkflowExecutionFailedEventAttributes' smart constructor.
data WorkflowExecutionFailedEventAttributes = WorkflowExecutionFailedEventAttributes'
  { -- | The details of the failure.
    details :: Prelude.Maybe Prelude.Text,
    -- | The descriptive reason provided for the failure.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The ID of the @DecisionTaskCompleted@ event corresponding to the
    -- decision task that resulted in the @FailWorkflowExecution@ decision to
    -- fail this execution. This information can be useful for diagnosing
    -- problems by tracing back the chain of events leading up to this event.
    decisionTaskCompletedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WorkflowExecutionFailedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'workflowExecutionFailedEventAttributes_details' - The details of the failure.
--
-- 'reason', 'workflowExecutionFailedEventAttributes_reason' - The descriptive reason provided for the failure.
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
      { details =
          Prelude.Nothing,
        reason = Prelude.Nothing,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | The details of the failure.
workflowExecutionFailedEventAttributes_details :: Lens.Lens' WorkflowExecutionFailedEventAttributes (Prelude.Maybe Prelude.Text)
workflowExecutionFailedEventAttributes_details = Lens.lens (\WorkflowExecutionFailedEventAttributes' {details} -> details) (\s@WorkflowExecutionFailedEventAttributes' {} a -> s {details = a} :: WorkflowExecutionFailedEventAttributes)

-- | The descriptive reason provided for the failure.
workflowExecutionFailedEventAttributes_reason :: Lens.Lens' WorkflowExecutionFailedEventAttributes (Prelude.Maybe Prelude.Text)
workflowExecutionFailedEventAttributes_reason = Lens.lens (\WorkflowExecutionFailedEventAttributes' {reason} -> reason) (\s@WorkflowExecutionFailedEventAttributes' {} a -> s {reason = a} :: WorkflowExecutionFailedEventAttributes)

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @FailWorkflowExecution@ decision to
-- fail this execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
workflowExecutionFailedEventAttributes_decisionTaskCompletedEventId :: Lens.Lens' WorkflowExecutionFailedEventAttributes Prelude.Integer
workflowExecutionFailedEventAttributes_decisionTaskCompletedEventId = Lens.lens (\WorkflowExecutionFailedEventAttributes' {decisionTaskCompletedEventId} -> decisionTaskCompletedEventId) (\s@WorkflowExecutionFailedEventAttributes' {} a -> s {decisionTaskCompletedEventId = a} :: WorkflowExecutionFailedEventAttributes)

instance
  Prelude.FromJSON
    WorkflowExecutionFailedEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "WorkflowExecutionFailedEventAttributes"
      ( \x ->
          WorkflowExecutionFailedEventAttributes'
            Prelude.<$> (x Prelude..:? "details")
            Prelude.<*> (x Prelude..:? "reason")
            Prelude.<*> (x Prelude..: "decisionTaskCompletedEventId")
      )

instance
  Prelude.Hashable
    WorkflowExecutionFailedEventAttributes

instance
  Prelude.NFData
    WorkflowExecutionFailedEventAttributes
