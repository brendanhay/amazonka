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
-- Module      : Amazonka.SWF.Types.WorkflowExecutionSignaledEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.WorkflowExecutionSignaledEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SWF.Types.WorkflowExecution

-- | Provides the details of the @WorkflowExecutionSignaled@ event.
--
-- /See:/ 'newWorkflowExecutionSignaledEventAttributes' smart constructor.
data WorkflowExecutionSignaledEventAttributes = WorkflowExecutionSignaledEventAttributes'
  { -- | The workflow execution that sent the signal. This is set only of the
    -- signal was sent by another workflow execution.
    externalWorkflowExecution :: Prelude.Maybe WorkflowExecution,
    -- | The ID of the @SignalExternalWorkflowExecutionInitiated@ event
    -- corresponding to the @SignalExternalWorkflow@ decision to signal this
    -- workflow execution.The source event with this ID can be found in the
    -- history of the source workflow execution. This information can be useful
    -- for diagnosing problems by tracing back the chain of events leading up
    -- to this event. This field is set only if the signal was initiated by
    -- another workflow execution.
    externalInitiatedEventId :: Prelude.Maybe Prelude.Integer,
    -- | The inputs provided with the signal. The decider can use the signal name
    -- and inputs to determine how to process the signal.
    input :: Prelude.Maybe Prelude.Text,
    -- | The name of the signal received. The decider can use the signal name and
    -- inputs to determine how to the process the signal.
    signalName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowExecutionSignaledEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'externalWorkflowExecution', 'workflowExecutionSignaledEventAttributes_externalWorkflowExecution' - The workflow execution that sent the signal. This is set only of the
-- signal was sent by another workflow execution.
--
-- 'externalInitiatedEventId', 'workflowExecutionSignaledEventAttributes_externalInitiatedEventId' - The ID of the @SignalExternalWorkflowExecutionInitiated@ event
-- corresponding to the @SignalExternalWorkflow@ decision to signal this
-- workflow execution.The source event with this ID can be found in the
-- history of the source workflow execution. This information can be useful
-- for diagnosing problems by tracing back the chain of events leading up
-- to this event. This field is set only if the signal was initiated by
-- another workflow execution.
--
-- 'input', 'workflowExecutionSignaledEventAttributes_input' - The inputs provided with the signal. The decider can use the signal name
-- and inputs to determine how to process the signal.
--
-- 'signalName', 'workflowExecutionSignaledEventAttributes_signalName' - The name of the signal received. The decider can use the signal name and
-- inputs to determine how to the process the signal.
newWorkflowExecutionSignaledEventAttributes ::
  -- | 'signalName'
  Prelude.Text ->
  WorkflowExecutionSignaledEventAttributes
newWorkflowExecutionSignaledEventAttributes
  pSignalName_ =
    WorkflowExecutionSignaledEventAttributes'
      { externalWorkflowExecution =
          Prelude.Nothing,
        externalInitiatedEventId =
          Prelude.Nothing,
        input = Prelude.Nothing,
        signalName = pSignalName_
      }

-- | The workflow execution that sent the signal. This is set only of the
-- signal was sent by another workflow execution.
workflowExecutionSignaledEventAttributes_externalWorkflowExecution :: Lens.Lens' WorkflowExecutionSignaledEventAttributes (Prelude.Maybe WorkflowExecution)
workflowExecutionSignaledEventAttributes_externalWorkflowExecution = Lens.lens (\WorkflowExecutionSignaledEventAttributes' {externalWorkflowExecution} -> externalWorkflowExecution) (\s@WorkflowExecutionSignaledEventAttributes' {} a -> s {externalWorkflowExecution = a} :: WorkflowExecutionSignaledEventAttributes)

-- | The ID of the @SignalExternalWorkflowExecutionInitiated@ event
-- corresponding to the @SignalExternalWorkflow@ decision to signal this
-- workflow execution.The source event with this ID can be found in the
-- history of the source workflow execution. This information can be useful
-- for diagnosing problems by tracing back the chain of events leading up
-- to this event. This field is set only if the signal was initiated by
-- another workflow execution.
workflowExecutionSignaledEventAttributes_externalInitiatedEventId :: Lens.Lens' WorkflowExecutionSignaledEventAttributes (Prelude.Maybe Prelude.Integer)
workflowExecutionSignaledEventAttributes_externalInitiatedEventId = Lens.lens (\WorkflowExecutionSignaledEventAttributes' {externalInitiatedEventId} -> externalInitiatedEventId) (\s@WorkflowExecutionSignaledEventAttributes' {} a -> s {externalInitiatedEventId = a} :: WorkflowExecutionSignaledEventAttributes)

-- | The inputs provided with the signal. The decider can use the signal name
-- and inputs to determine how to process the signal.
workflowExecutionSignaledEventAttributes_input :: Lens.Lens' WorkflowExecutionSignaledEventAttributes (Prelude.Maybe Prelude.Text)
workflowExecutionSignaledEventAttributes_input = Lens.lens (\WorkflowExecutionSignaledEventAttributes' {input} -> input) (\s@WorkflowExecutionSignaledEventAttributes' {} a -> s {input = a} :: WorkflowExecutionSignaledEventAttributes)

-- | The name of the signal received. The decider can use the signal name and
-- inputs to determine how to the process the signal.
workflowExecutionSignaledEventAttributes_signalName :: Lens.Lens' WorkflowExecutionSignaledEventAttributes Prelude.Text
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
            Prelude.<$> (x Core..:? "externalWorkflowExecution")
            Prelude.<*> (x Core..:? "externalInitiatedEventId")
            Prelude.<*> (x Core..:? "input")
            Prelude.<*> (x Core..: "signalName")
      )

instance
  Prelude.Hashable
    WorkflowExecutionSignaledEventAttributes
  where
  hashWithSalt
    salt'
    WorkflowExecutionSignaledEventAttributes' {..} =
      salt' `Prelude.hashWithSalt` signalName
        `Prelude.hashWithSalt` input
        `Prelude.hashWithSalt` externalInitiatedEventId
        `Prelude.hashWithSalt` externalWorkflowExecution

instance
  Prelude.NFData
    WorkflowExecutionSignaledEventAttributes
  where
  rnf WorkflowExecutionSignaledEventAttributes' {..} =
    Prelude.rnf externalWorkflowExecution
      `Prelude.seq` Prelude.rnf signalName
      `Prelude.seq` Prelude.rnf input
      `Prelude.seq` Prelude.rnf externalInitiatedEventId
