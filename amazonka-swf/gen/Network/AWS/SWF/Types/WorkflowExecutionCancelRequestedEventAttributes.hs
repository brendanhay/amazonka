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
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionCancelRequestedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionCancelRequestedEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SWF.Types.WorkflowExecution
import Network.AWS.SWF.Types.WorkflowExecutionCancelRequestedCause

-- | Provides the details of the @WorkflowExecutionCancelRequested@ event.
--
-- /See:/ 'newWorkflowExecutionCancelRequestedEventAttributes' smart constructor.
data WorkflowExecutionCancelRequestedEventAttributes = WorkflowExecutionCancelRequestedEventAttributes'
  { -- | The ID of the @RequestCancelExternalWorkflowExecutionInitiated@ event
    -- corresponding to the @RequestCancelExternalWorkflowExecution@ decision
    -- to cancel this workflow execution.The source event with this ID can be
    -- found in the history of the source workflow execution. This information
    -- can be useful for diagnosing problems by tracing back the chain of
    -- events leading up to this event.
    externalInitiatedEventId :: Prelude.Maybe Prelude.Integer,
    -- | The external workflow execution for which the cancellation was
    -- requested.
    externalWorkflowExecution :: Prelude.Maybe WorkflowExecution,
    -- | If set, indicates that the request to cancel the workflow execution was
    -- automatically generated, and specifies the cause. This happens if the
    -- parent workflow execution times out or is terminated, and the child
    -- policy is set to cancel child executions.
    cause :: Prelude.Maybe WorkflowExecutionCancelRequestedCause
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WorkflowExecutionCancelRequestedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'externalInitiatedEventId', 'workflowExecutionCancelRequestedEventAttributes_externalInitiatedEventId' - The ID of the @RequestCancelExternalWorkflowExecutionInitiated@ event
-- corresponding to the @RequestCancelExternalWorkflowExecution@ decision
-- to cancel this workflow execution.The source event with this ID can be
-- found in the history of the source workflow execution. This information
-- can be useful for diagnosing problems by tracing back the chain of
-- events leading up to this event.
--
-- 'externalWorkflowExecution', 'workflowExecutionCancelRequestedEventAttributes_externalWorkflowExecution' - The external workflow execution for which the cancellation was
-- requested.
--
-- 'cause', 'workflowExecutionCancelRequestedEventAttributes_cause' - If set, indicates that the request to cancel the workflow execution was
-- automatically generated, and specifies the cause. This happens if the
-- parent workflow execution times out or is terminated, and the child
-- policy is set to cancel child executions.
newWorkflowExecutionCancelRequestedEventAttributes ::
  WorkflowExecutionCancelRequestedEventAttributes
newWorkflowExecutionCancelRequestedEventAttributes =
  WorkflowExecutionCancelRequestedEventAttributes'
    { externalInitiatedEventId =
        Prelude.Nothing,
      externalWorkflowExecution =
        Prelude.Nothing,
      cause = Prelude.Nothing
    }

-- | The ID of the @RequestCancelExternalWorkflowExecutionInitiated@ event
-- corresponding to the @RequestCancelExternalWorkflowExecution@ decision
-- to cancel this workflow execution.The source event with this ID can be
-- found in the history of the source workflow execution. This information
-- can be useful for diagnosing problems by tracing back the chain of
-- events leading up to this event.
workflowExecutionCancelRequestedEventAttributes_externalInitiatedEventId :: Lens.Lens' WorkflowExecutionCancelRequestedEventAttributes (Prelude.Maybe Prelude.Integer)
workflowExecutionCancelRequestedEventAttributes_externalInitiatedEventId = Lens.lens (\WorkflowExecutionCancelRequestedEventAttributes' {externalInitiatedEventId} -> externalInitiatedEventId) (\s@WorkflowExecutionCancelRequestedEventAttributes' {} a -> s {externalInitiatedEventId = a} :: WorkflowExecutionCancelRequestedEventAttributes)

-- | The external workflow execution for which the cancellation was
-- requested.
workflowExecutionCancelRequestedEventAttributes_externalWorkflowExecution :: Lens.Lens' WorkflowExecutionCancelRequestedEventAttributes (Prelude.Maybe WorkflowExecution)
workflowExecutionCancelRequestedEventAttributes_externalWorkflowExecution = Lens.lens (\WorkflowExecutionCancelRequestedEventAttributes' {externalWorkflowExecution} -> externalWorkflowExecution) (\s@WorkflowExecutionCancelRequestedEventAttributes' {} a -> s {externalWorkflowExecution = a} :: WorkflowExecutionCancelRequestedEventAttributes)

-- | If set, indicates that the request to cancel the workflow execution was
-- automatically generated, and specifies the cause. This happens if the
-- parent workflow execution times out or is terminated, and the child
-- policy is set to cancel child executions.
workflowExecutionCancelRequestedEventAttributes_cause :: Lens.Lens' WorkflowExecutionCancelRequestedEventAttributes (Prelude.Maybe WorkflowExecutionCancelRequestedCause)
workflowExecutionCancelRequestedEventAttributes_cause = Lens.lens (\WorkflowExecutionCancelRequestedEventAttributes' {cause} -> cause) (\s@WorkflowExecutionCancelRequestedEventAttributes' {} a -> s {cause = a} :: WorkflowExecutionCancelRequestedEventAttributes)

instance
  Prelude.FromJSON
    WorkflowExecutionCancelRequestedEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "WorkflowExecutionCancelRequestedEventAttributes"
      ( \x ->
          WorkflowExecutionCancelRequestedEventAttributes'
            Prelude.<$> (x Prelude..:? "externalInitiatedEventId")
              Prelude.<*> (x Prelude..:? "externalWorkflowExecution")
              Prelude.<*> (x Prelude..:? "cause")
      )

instance
  Prelude.Hashable
    WorkflowExecutionCancelRequestedEventAttributes

instance
  Prelude.NFData
    WorkflowExecutionCancelRequestedEventAttributes
