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
-- Module      : Amazonka.SWF.Types.SignalExternalWorkflowExecutionInitiatedEventAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.SignalExternalWorkflowExecutionInitiatedEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the details of the @SignalExternalWorkflowExecutionInitiated@
-- event.
--
-- /See:/ 'newSignalExternalWorkflowExecutionInitiatedEventAttributes' smart constructor.
data SignalExternalWorkflowExecutionInitiatedEventAttributes = SignalExternalWorkflowExecutionInitiatedEventAttributes'
  { -- | Data attached to the event that can be used by the decider in subsequent
    -- decision tasks.
    control :: Prelude.Maybe Prelude.Text,
    -- | The input provided to the signal.
    input :: Prelude.Maybe Prelude.Text,
    -- | The @runId@ of the external workflow execution to send the signal to.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The @workflowId@ of the external workflow execution.
    workflowId :: Prelude.Text,
    -- | The name of the signal.
    signalName :: Prelude.Text,
    -- | The ID of the @DecisionTaskCompleted@ event corresponding to the
    -- decision task that resulted in the @SignalExternalWorkflowExecution@
    -- decision for this signal. This information can be useful for diagnosing
    -- problems by tracing back the chain of events leading up to this event.
    decisionTaskCompletedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SignalExternalWorkflowExecutionInitiatedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'control', 'signalExternalWorkflowExecutionInitiatedEventAttributes_control' - Data attached to the event that can be used by the decider in subsequent
-- decision tasks.
--
-- 'input', 'signalExternalWorkflowExecutionInitiatedEventAttributes_input' - The input provided to the signal.
--
-- 'runId', 'signalExternalWorkflowExecutionInitiatedEventAttributes_runId' - The @runId@ of the external workflow execution to send the signal to.
--
-- 'workflowId', 'signalExternalWorkflowExecutionInitiatedEventAttributes_workflowId' - The @workflowId@ of the external workflow execution.
--
-- 'signalName', 'signalExternalWorkflowExecutionInitiatedEventAttributes_signalName' - The name of the signal.
--
-- 'decisionTaskCompletedEventId', 'signalExternalWorkflowExecutionInitiatedEventAttributes_decisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @SignalExternalWorkflowExecution@
-- decision for this signal. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
newSignalExternalWorkflowExecutionInitiatedEventAttributes ::
  -- | 'workflowId'
  Prelude.Text ->
  -- | 'signalName'
  Prelude.Text ->
  -- | 'decisionTaskCompletedEventId'
  Prelude.Integer ->
  SignalExternalWorkflowExecutionInitiatedEventAttributes
newSignalExternalWorkflowExecutionInitiatedEventAttributes
  pWorkflowId_
  pSignalName_
  pDecisionTaskCompletedEventId_ =
    SignalExternalWorkflowExecutionInitiatedEventAttributes'
      { control =
          Prelude.Nothing,
        input =
          Prelude.Nothing,
        runId =
          Prelude.Nothing,
        workflowId =
          pWorkflowId_,
        signalName =
          pSignalName_,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | Data attached to the event that can be used by the decider in subsequent
-- decision tasks.
signalExternalWorkflowExecutionInitiatedEventAttributes_control :: Lens.Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Prelude.Maybe Prelude.Text)
signalExternalWorkflowExecutionInitiatedEventAttributes_control = Lens.lens (\SignalExternalWorkflowExecutionInitiatedEventAttributes' {control} -> control) (\s@SignalExternalWorkflowExecutionInitiatedEventAttributes' {} a -> s {control = a} :: SignalExternalWorkflowExecutionInitiatedEventAttributes)

-- | The input provided to the signal.
signalExternalWorkflowExecutionInitiatedEventAttributes_input :: Lens.Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Prelude.Maybe Prelude.Text)
signalExternalWorkflowExecutionInitiatedEventAttributes_input = Lens.lens (\SignalExternalWorkflowExecutionInitiatedEventAttributes' {input} -> input) (\s@SignalExternalWorkflowExecutionInitiatedEventAttributes' {} a -> s {input = a} :: SignalExternalWorkflowExecutionInitiatedEventAttributes)

-- | The @runId@ of the external workflow execution to send the signal to.
signalExternalWorkflowExecutionInitiatedEventAttributes_runId :: Lens.Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Prelude.Maybe Prelude.Text)
signalExternalWorkflowExecutionInitiatedEventAttributes_runId = Lens.lens (\SignalExternalWorkflowExecutionInitiatedEventAttributes' {runId} -> runId) (\s@SignalExternalWorkflowExecutionInitiatedEventAttributes' {} a -> s {runId = a} :: SignalExternalWorkflowExecutionInitiatedEventAttributes)

-- | The @workflowId@ of the external workflow execution.
signalExternalWorkflowExecutionInitiatedEventAttributes_workflowId :: Lens.Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes Prelude.Text
signalExternalWorkflowExecutionInitiatedEventAttributes_workflowId = Lens.lens (\SignalExternalWorkflowExecutionInitiatedEventAttributes' {workflowId} -> workflowId) (\s@SignalExternalWorkflowExecutionInitiatedEventAttributes' {} a -> s {workflowId = a} :: SignalExternalWorkflowExecutionInitiatedEventAttributes)

-- | The name of the signal.
signalExternalWorkflowExecutionInitiatedEventAttributes_signalName :: Lens.Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes Prelude.Text
signalExternalWorkflowExecutionInitiatedEventAttributes_signalName = Lens.lens (\SignalExternalWorkflowExecutionInitiatedEventAttributes' {signalName} -> signalName) (\s@SignalExternalWorkflowExecutionInitiatedEventAttributes' {} a -> s {signalName = a} :: SignalExternalWorkflowExecutionInitiatedEventAttributes)

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @SignalExternalWorkflowExecution@
-- decision for this signal. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
signalExternalWorkflowExecutionInitiatedEventAttributes_decisionTaskCompletedEventId :: Lens.Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes Prelude.Integer
signalExternalWorkflowExecutionInitiatedEventAttributes_decisionTaskCompletedEventId = Lens.lens (\SignalExternalWorkflowExecutionInitiatedEventAttributes' {decisionTaskCompletedEventId} -> decisionTaskCompletedEventId) (\s@SignalExternalWorkflowExecutionInitiatedEventAttributes' {} a -> s {decisionTaskCompletedEventId = a} :: SignalExternalWorkflowExecutionInitiatedEventAttributes)

instance
  Data.FromJSON
    SignalExternalWorkflowExecutionInitiatedEventAttributes
  where
  parseJSON =
    Data.withObject
      "SignalExternalWorkflowExecutionInitiatedEventAttributes"
      ( \x ->
          SignalExternalWorkflowExecutionInitiatedEventAttributes'
            Prelude.<$> (x Data..:? "control")
            Prelude.<*> (x Data..:? "input")
            Prelude.<*> (x Data..:? "runId")
            Prelude.<*> (x Data..: "workflowId")
            Prelude.<*> (x Data..: "signalName")
            Prelude.<*> (x Data..: "decisionTaskCompletedEventId")
      )

instance
  Prelude.Hashable
    SignalExternalWorkflowExecutionInitiatedEventAttributes
  where
  hashWithSalt
    _salt
    SignalExternalWorkflowExecutionInitiatedEventAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` control
        `Prelude.hashWithSalt` input
        `Prelude.hashWithSalt` runId
        `Prelude.hashWithSalt` workflowId
        `Prelude.hashWithSalt` signalName
        `Prelude.hashWithSalt` decisionTaskCompletedEventId

instance
  Prelude.NFData
    SignalExternalWorkflowExecutionInitiatedEventAttributes
  where
  rnf
    SignalExternalWorkflowExecutionInitiatedEventAttributes' {..} =
      Prelude.rnf control `Prelude.seq`
        Prelude.rnf input `Prelude.seq`
          Prelude.rnf runId `Prelude.seq`
            Prelude.rnf workflowId `Prelude.seq`
              Prelude.rnf signalName `Prelude.seq`
                Prelude.rnf decisionTaskCompletedEventId
