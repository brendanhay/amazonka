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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

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
    runId :: Core.Maybe Core.Text,
    -- | The input data to be provided with the signal. The target workflow
    -- execution uses the signal name and input data to process the signal.
    input :: Core.Maybe Core.Text,
    -- | The data attached to the event that can be used by the decider in
    -- subsequent decision tasks.
    control :: Core.Maybe Core.Text,
    -- | The @workflowId@ of the workflow execution to be signaled.
    workflowId :: Core.Text,
    -- | The name of the signal.The target workflow execution uses the signal
    -- name and input to process the signal.
    signalName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'signalName'
  Core.Text ->
  SignalExternalWorkflowExecutionDecisionAttributes
newSignalExternalWorkflowExecutionDecisionAttributes
  pWorkflowId_
  pSignalName_ =
    SignalExternalWorkflowExecutionDecisionAttributes'
      { runId =
          Core.Nothing,
        input = Core.Nothing,
        control = Core.Nothing,
        workflowId =
          pWorkflowId_,
        signalName =
          pSignalName_
      }

-- | The @runId@ of the workflow execution to be signaled.
signalExternalWorkflowExecutionDecisionAttributes_runId :: Lens.Lens' SignalExternalWorkflowExecutionDecisionAttributes (Core.Maybe Core.Text)
signalExternalWorkflowExecutionDecisionAttributes_runId = Lens.lens (\SignalExternalWorkflowExecutionDecisionAttributes' {runId} -> runId) (\s@SignalExternalWorkflowExecutionDecisionAttributes' {} a -> s {runId = a} :: SignalExternalWorkflowExecutionDecisionAttributes)

-- | The input data to be provided with the signal. The target workflow
-- execution uses the signal name and input data to process the signal.
signalExternalWorkflowExecutionDecisionAttributes_input :: Lens.Lens' SignalExternalWorkflowExecutionDecisionAttributes (Core.Maybe Core.Text)
signalExternalWorkflowExecutionDecisionAttributes_input = Lens.lens (\SignalExternalWorkflowExecutionDecisionAttributes' {input} -> input) (\s@SignalExternalWorkflowExecutionDecisionAttributes' {} a -> s {input = a} :: SignalExternalWorkflowExecutionDecisionAttributes)

-- | The data attached to the event that can be used by the decider in
-- subsequent decision tasks.
signalExternalWorkflowExecutionDecisionAttributes_control :: Lens.Lens' SignalExternalWorkflowExecutionDecisionAttributes (Core.Maybe Core.Text)
signalExternalWorkflowExecutionDecisionAttributes_control = Lens.lens (\SignalExternalWorkflowExecutionDecisionAttributes' {control} -> control) (\s@SignalExternalWorkflowExecutionDecisionAttributes' {} a -> s {control = a} :: SignalExternalWorkflowExecutionDecisionAttributes)

-- | The @workflowId@ of the workflow execution to be signaled.
signalExternalWorkflowExecutionDecisionAttributes_workflowId :: Lens.Lens' SignalExternalWorkflowExecutionDecisionAttributes Core.Text
signalExternalWorkflowExecutionDecisionAttributes_workflowId = Lens.lens (\SignalExternalWorkflowExecutionDecisionAttributes' {workflowId} -> workflowId) (\s@SignalExternalWorkflowExecutionDecisionAttributes' {} a -> s {workflowId = a} :: SignalExternalWorkflowExecutionDecisionAttributes)

-- | The name of the signal.The target workflow execution uses the signal
-- name and input to process the signal.
signalExternalWorkflowExecutionDecisionAttributes_signalName :: Lens.Lens' SignalExternalWorkflowExecutionDecisionAttributes Core.Text
signalExternalWorkflowExecutionDecisionAttributes_signalName = Lens.lens (\SignalExternalWorkflowExecutionDecisionAttributes' {signalName} -> signalName) (\s@SignalExternalWorkflowExecutionDecisionAttributes' {} a -> s {signalName = a} :: SignalExternalWorkflowExecutionDecisionAttributes)

instance
  Core.Hashable
    SignalExternalWorkflowExecutionDecisionAttributes

instance
  Core.NFData
    SignalExternalWorkflowExecutionDecisionAttributes

instance
  Core.ToJSON
    SignalExternalWorkflowExecutionDecisionAttributes
  where
  toJSON
    SignalExternalWorkflowExecutionDecisionAttributes' {..} =
      Core.object
        ( Core.catMaybes
            [ ("runId" Core..=) Core.<$> runId,
              ("input" Core..=) Core.<$> input,
              ("control" Core..=) Core.<$> control,
              Core.Just ("workflowId" Core..= workflowId),
              Core.Just ("signalName" Core..= signalName)
            ]
        )
