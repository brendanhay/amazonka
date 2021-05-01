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
-- Module      : Network.AWS.SWF.Types.SignalExternalWorkflowExecutionDecisionAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.SignalExternalWorkflowExecutionDecisionAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the details of the @SignalExternalWorkflowExecution@ decision.
--
-- __Access Control__
--
-- You can use IAM policies to control this decision\'s access to Amazon
-- SWF resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
--
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
--
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller doesn\'t have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s @cause@ parameter is set
-- to @OPERATION_NOT_PERMITTED@. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
--
-- /See:/ 'newSignalExternalWorkflowExecutionDecisionAttributes' smart constructor.
data SignalExternalWorkflowExecutionDecisionAttributes = SignalExternalWorkflowExecutionDecisionAttributes'
  { -- | The @runId@ of the workflow execution to be signaled.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The input data to be provided with the signal. The target workflow
    -- execution uses the signal name and input data to process the signal.
    input :: Prelude.Maybe Prelude.Text,
    -- | The data attached to the event that can be used by the decider in
    -- subsequent decision tasks.
    control :: Prelude.Maybe Prelude.Text,
    -- | The @workflowId@ of the workflow execution to be signaled.
    workflowId :: Prelude.Text,
    -- | The name of the signal.The target workflow execution uses the signal
    -- name and input to process the signal.
    signalName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SignalExternalWorkflowExecutionDecisionAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runId', 'signalExternalWorkflowExecutionDecisionAttributes_runId' - The @runId@ of the workflow execution to be signaled.
--
-- 'input', 'signalExternalWorkflowExecutionDecisionAttributes_input' - The input data to be provided with the signal. The target workflow
-- execution uses the signal name and input data to process the signal.
--
-- 'control', 'signalExternalWorkflowExecutionDecisionAttributes_control' - The data attached to the event that can be used by the decider in
-- subsequent decision tasks.
--
-- 'workflowId', 'signalExternalWorkflowExecutionDecisionAttributes_workflowId' - The @workflowId@ of the workflow execution to be signaled.
--
-- 'signalName', 'signalExternalWorkflowExecutionDecisionAttributes_signalName' - The name of the signal.The target workflow execution uses the signal
-- name and input to process the signal.
newSignalExternalWorkflowExecutionDecisionAttributes ::
  -- | 'workflowId'
  Prelude.Text ->
  -- | 'signalName'
  Prelude.Text ->
  SignalExternalWorkflowExecutionDecisionAttributes
newSignalExternalWorkflowExecutionDecisionAttributes
  pWorkflowId_
  pSignalName_ =
    SignalExternalWorkflowExecutionDecisionAttributes'
      { runId =
          Prelude.Nothing,
        input = Prelude.Nothing,
        control =
          Prelude.Nothing,
        workflowId =
          pWorkflowId_,
        signalName =
          pSignalName_
      }

-- | The @runId@ of the workflow execution to be signaled.
signalExternalWorkflowExecutionDecisionAttributes_runId :: Lens.Lens' SignalExternalWorkflowExecutionDecisionAttributes (Prelude.Maybe Prelude.Text)
signalExternalWorkflowExecutionDecisionAttributes_runId = Lens.lens (\SignalExternalWorkflowExecutionDecisionAttributes' {runId} -> runId) (\s@SignalExternalWorkflowExecutionDecisionAttributes' {} a -> s {runId = a} :: SignalExternalWorkflowExecutionDecisionAttributes)

-- | The input data to be provided with the signal. The target workflow
-- execution uses the signal name and input data to process the signal.
signalExternalWorkflowExecutionDecisionAttributes_input :: Lens.Lens' SignalExternalWorkflowExecutionDecisionAttributes (Prelude.Maybe Prelude.Text)
signalExternalWorkflowExecutionDecisionAttributes_input = Lens.lens (\SignalExternalWorkflowExecutionDecisionAttributes' {input} -> input) (\s@SignalExternalWorkflowExecutionDecisionAttributes' {} a -> s {input = a} :: SignalExternalWorkflowExecutionDecisionAttributes)

-- | The data attached to the event that can be used by the decider in
-- subsequent decision tasks.
signalExternalWorkflowExecutionDecisionAttributes_control :: Lens.Lens' SignalExternalWorkflowExecutionDecisionAttributes (Prelude.Maybe Prelude.Text)
signalExternalWorkflowExecutionDecisionAttributes_control = Lens.lens (\SignalExternalWorkflowExecutionDecisionAttributes' {control} -> control) (\s@SignalExternalWorkflowExecutionDecisionAttributes' {} a -> s {control = a} :: SignalExternalWorkflowExecutionDecisionAttributes)

-- | The @workflowId@ of the workflow execution to be signaled.
signalExternalWorkflowExecutionDecisionAttributes_workflowId :: Lens.Lens' SignalExternalWorkflowExecutionDecisionAttributes Prelude.Text
signalExternalWorkflowExecutionDecisionAttributes_workflowId = Lens.lens (\SignalExternalWorkflowExecutionDecisionAttributes' {workflowId} -> workflowId) (\s@SignalExternalWorkflowExecutionDecisionAttributes' {} a -> s {workflowId = a} :: SignalExternalWorkflowExecutionDecisionAttributes)

-- | The name of the signal.The target workflow execution uses the signal
-- name and input to process the signal.
signalExternalWorkflowExecutionDecisionAttributes_signalName :: Lens.Lens' SignalExternalWorkflowExecutionDecisionAttributes Prelude.Text
signalExternalWorkflowExecutionDecisionAttributes_signalName = Lens.lens (\SignalExternalWorkflowExecutionDecisionAttributes' {signalName} -> signalName) (\s@SignalExternalWorkflowExecutionDecisionAttributes' {} a -> s {signalName = a} :: SignalExternalWorkflowExecutionDecisionAttributes)

instance
  Prelude.Hashable
    SignalExternalWorkflowExecutionDecisionAttributes

instance
  Prelude.NFData
    SignalExternalWorkflowExecutionDecisionAttributes

instance
  Prelude.ToJSON
    SignalExternalWorkflowExecutionDecisionAttributes
  where
  toJSON
    SignalExternalWorkflowExecutionDecisionAttributes' {..} =
      Prelude.object
        ( Prelude.catMaybes
            [ ("runId" Prelude..=) Prelude.<$> runId,
              ("input" Prelude..=) Prelude.<$> input,
              ("control" Prelude..=) Prelude.<$> control,
              Prelude.Just ("workflowId" Prelude..= workflowId),
              Prelude.Just ("signalName" Prelude..= signalName)
            ]
        )
