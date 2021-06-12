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
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionSignaledEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionSignaledEventAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SWF.Types.WorkflowExecution

-- | Provides the details of the @WorkflowExecutionSignaled@ event.
--
-- /See:/ 'newWorkflowExecutionSignaledEventAttributes' smart constructor.
data WorkflowExecutionSignaledEventAttributes = WorkflowExecutionSignaledEventAttributes'
  { -- | The inputs provided with the signal. The decider can use the signal name
    -- and inputs to determine how to process the signal.
    input :: Core.Maybe Core.Text,
    -- | The ID of the @SignalExternalWorkflowExecutionInitiated@ event
    -- corresponding to the @SignalExternalWorkflow@ decision to signal this
    -- workflow execution.The source event with this ID can be found in the
    -- history of the source workflow execution. This information can be useful
    -- for diagnosing problems by tracing back the chain of events leading up
    -- to this event. This field is set only if the signal was initiated by
    -- another workflow execution.
    externalInitiatedEventId :: Core.Maybe Core.Integer,
    -- | The workflow execution that sent the signal. This is set only of the
    -- signal was sent by another workflow execution.
    externalWorkflowExecution :: Core.Maybe WorkflowExecution,
    -- | The name of the signal received. The decider can use the signal name and
    -- inputs to determine how to the process the signal.
    signalName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'WorkflowExecutionSignaledEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'input', 'workflowExecutionSignaledEventAttributes_input' - The inputs provided with the signal. The decider can use the signal name
-- and inputs to determine how to process the signal.
--
-- 'externalInitiatedEventId', 'workflowExecutionSignaledEventAttributes_externalInitiatedEventId' - The ID of the @SignalExternalWorkflowExecutionInitiated@ event
-- corresponding to the @SignalExternalWorkflow@ decision to signal this
-- workflow execution.The source event with this ID can be found in the
-- history of the source workflow execution. This information can be useful
-- for diagnosing problems by tracing back the chain of events leading up
-- to this event. This field is set only if the signal was initiated by
-- another workflow execution.
--
-- 'externalWorkflowExecution', 'workflowExecutionSignaledEventAttributes_externalWorkflowExecution' - The workflow execution that sent the signal. This is set only of the
-- signal was sent by another workflow execution.
--
-- 'signalName', 'workflowExecutionSignaledEventAttributes_signalName' - The name of the signal received. The decider can use the signal name and
-- inputs to determine how to the process the signal.
newWorkflowExecutionSignaledEventAttributes ::
  -- | 'signalName'
  Core.Text ->
  WorkflowExecutionSignaledEventAttributes
newWorkflowExecutionSignaledEventAttributes
  pSignalName_ =
    WorkflowExecutionSignaledEventAttributes'
      { input =
          Core.Nothing,
        externalInitiatedEventId =
          Core.Nothing,
        externalWorkflowExecution =
          Core.Nothing,
        signalName = pSignalName_
      }

-- | The inputs provided with the signal. The decider can use the signal name
-- and inputs to determine how to process the signal.
workflowExecutionSignaledEventAttributes_input :: Lens.Lens' WorkflowExecutionSignaledEventAttributes (Core.Maybe Core.Text)
workflowExecutionSignaledEventAttributes_input = Lens.lens (\WorkflowExecutionSignaledEventAttributes' {input} -> input) (\s@WorkflowExecutionSignaledEventAttributes' {} a -> s {input = a} :: WorkflowExecutionSignaledEventAttributes)

-- | The ID of the @SignalExternalWorkflowExecutionInitiated@ event
-- corresponding to the @SignalExternalWorkflow@ decision to signal this
-- workflow execution.The source event with this ID can be found in the
-- history of the source workflow execution. This information can be useful
-- for diagnosing problems by tracing back the chain of events leading up
-- to this event. This field is set only if the signal was initiated by
-- another workflow execution.
workflowExecutionSignaledEventAttributes_externalInitiatedEventId :: Lens.Lens' WorkflowExecutionSignaledEventAttributes (Core.Maybe Core.Integer)
workflowExecutionSignaledEventAttributes_externalInitiatedEventId = Lens.lens (\WorkflowExecutionSignaledEventAttributes' {externalInitiatedEventId} -> externalInitiatedEventId) (\s@WorkflowExecutionSignaledEventAttributes' {} a -> s {externalInitiatedEventId = a} :: WorkflowExecutionSignaledEventAttributes)

-- | The workflow execution that sent the signal. This is set only of the
-- signal was sent by another workflow execution.
workflowExecutionSignaledEventAttributes_externalWorkflowExecution :: Lens.Lens' WorkflowExecutionSignaledEventAttributes (Core.Maybe WorkflowExecution)
workflowExecutionSignaledEventAttributes_externalWorkflowExecution = Lens.lens (\WorkflowExecutionSignaledEventAttributes' {externalWorkflowExecution} -> externalWorkflowExecution) (\s@WorkflowExecutionSignaledEventAttributes' {} a -> s {externalWorkflowExecution = a} :: WorkflowExecutionSignaledEventAttributes)

-- | The name of the signal received. The decider can use the signal name and
-- inputs to determine how to the process the signal.
workflowExecutionSignaledEventAttributes_signalName :: Lens.Lens' WorkflowExecutionSignaledEventAttributes Core.Text
workflowExecutionSignaledEventAttributes_signalName = Lens.lens (\WorkflowExecutionSignaledEventAttributes' {signalName} -> signalName) (\s@WorkflowExecutionSignaledEventAttributes' {} a -> s {signalName = a} :: WorkflowExecutionSignaledEventAttributes)

instance
  Core.FromJSON
    WorkflowExecutionSignaledEventAttributes
  where
  parseJSON =
    Core.withObject
      "WorkflowExecutionSignaledEventAttributes"
      ( \x ->
          WorkflowExecutionSignaledEventAttributes'
            Core.<$> (x Core..:? "input")
            Core.<*> (x Core..:? "externalInitiatedEventId")
            Core.<*> (x Core..:? "externalWorkflowExecution")
            Core.<*> (x Core..: "signalName")
      )

instance
  Core.Hashable
    WorkflowExecutionSignaledEventAttributes

instance
  Core.NFData
    WorkflowExecutionSignaledEventAttributes
