{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.SignalWorkflowExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records a @WorkflowExecutionSignaled@ event in the workflow execution
-- history and creates a decision task for the workflow execution
-- identified by the given domain, workflowId and runId. The event is
-- recorded with the specified user defined signalName and input (if
-- provided).
--
-- If a runId isn\'t specified, then the @WorkflowExecutionSignaled@ event
-- is recorded in the history of the current open workflow with the
-- matching workflowId in the domain.
--
-- If the specified workflow execution isn\'t open, this method fails with
-- @UnknownResource@.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
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
module Network.AWS.SWF.SignalWorkflowExecution
  ( -- * Creating a Request
    SignalWorkflowExecution (..),
    newSignalWorkflowExecution,

    -- * Request Lenses
    signalWorkflowExecution_runId,
    signalWorkflowExecution_input,
    signalWorkflowExecution_domain,
    signalWorkflowExecution_workflowId,
    signalWorkflowExecution_signalName,

    -- * Destructuring the Response
    SignalWorkflowExecutionResponse (..),
    newSignalWorkflowExecutionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SWF.Types

-- | /See:/ 'newSignalWorkflowExecution' smart constructor.
data SignalWorkflowExecution = SignalWorkflowExecution'
  { -- | The runId of the workflow execution to signal.
    runId :: Prelude.Maybe Prelude.Text,
    -- | Data to attach to the @WorkflowExecutionSignaled@ event in the target
    -- workflow execution\'s history.
    input :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain containing the workflow execution to signal.
    domain :: Prelude.Text,
    -- | The workflowId of the workflow execution to signal.
    workflowId :: Prelude.Text,
    -- | The name of the signal. This name must be meaningful to the target
    -- workflow.
    signalName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SignalWorkflowExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runId', 'signalWorkflowExecution_runId' - The runId of the workflow execution to signal.
--
-- 'input', 'signalWorkflowExecution_input' - Data to attach to the @WorkflowExecutionSignaled@ event in the target
-- workflow execution\'s history.
--
-- 'domain', 'signalWorkflowExecution_domain' - The name of the domain containing the workflow execution to signal.
--
-- 'workflowId', 'signalWorkflowExecution_workflowId' - The workflowId of the workflow execution to signal.
--
-- 'signalName', 'signalWorkflowExecution_signalName' - The name of the signal. This name must be meaningful to the target
-- workflow.
newSignalWorkflowExecution ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'workflowId'
  Prelude.Text ->
  -- | 'signalName'
  Prelude.Text ->
  SignalWorkflowExecution
newSignalWorkflowExecution
  pDomain_
  pWorkflowId_
  pSignalName_ =
    SignalWorkflowExecution'
      { runId = Prelude.Nothing,
        input = Prelude.Nothing,
        domain = pDomain_,
        workflowId = pWorkflowId_,
        signalName = pSignalName_
      }

-- | The runId of the workflow execution to signal.
signalWorkflowExecution_runId :: Lens.Lens' SignalWorkflowExecution (Prelude.Maybe Prelude.Text)
signalWorkflowExecution_runId = Lens.lens (\SignalWorkflowExecution' {runId} -> runId) (\s@SignalWorkflowExecution' {} a -> s {runId = a} :: SignalWorkflowExecution)

-- | Data to attach to the @WorkflowExecutionSignaled@ event in the target
-- workflow execution\'s history.
signalWorkflowExecution_input :: Lens.Lens' SignalWorkflowExecution (Prelude.Maybe Prelude.Text)
signalWorkflowExecution_input = Lens.lens (\SignalWorkflowExecution' {input} -> input) (\s@SignalWorkflowExecution' {} a -> s {input = a} :: SignalWorkflowExecution)

-- | The name of the domain containing the workflow execution to signal.
signalWorkflowExecution_domain :: Lens.Lens' SignalWorkflowExecution Prelude.Text
signalWorkflowExecution_domain = Lens.lens (\SignalWorkflowExecution' {domain} -> domain) (\s@SignalWorkflowExecution' {} a -> s {domain = a} :: SignalWorkflowExecution)

-- | The workflowId of the workflow execution to signal.
signalWorkflowExecution_workflowId :: Lens.Lens' SignalWorkflowExecution Prelude.Text
signalWorkflowExecution_workflowId = Lens.lens (\SignalWorkflowExecution' {workflowId} -> workflowId) (\s@SignalWorkflowExecution' {} a -> s {workflowId = a} :: SignalWorkflowExecution)

-- | The name of the signal. This name must be meaningful to the target
-- workflow.
signalWorkflowExecution_signalName :: Lens.Lens' SignalWorkflowExecution Prelude.Text
signalWorkflowExecution_signalName = Lens.lens (\SignalWorkflowExecution' {signalName} -> signalName) (\s@SignalWorkflowExecution' {} a -> s {signalName = a} :: SignalWorkflowExecution)

instance Prelude.AWSRequest SignalWorkflowExecution where
  type
    Rs SignalWorkflowExecution =
      SignalWorkflowExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      SignalWorkflowExecutionResponse'

instance Prelude.Hashable SignalWorkflowExecution

instance Prelude.NFData SignalWorkflowExecution

instance Prelude.ToHeaders SignalWorkflowExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SimpleWorkflowService.SignalWorkflowExecution" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON SignalWorkflowExecution where
  toJSON SignalWorkflowExecution' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("runId" Prelude..=) Prelude.<$> runId,
            ("input" Prelude..=) Prelude.<$> input,
            Prelude.Just ("domain" Prelude..= domain),
            Prelude.Just ("workflowId" Prelude..= workflowId),
            Prelude.Just ("signalName" Prelude..= signalName)
          ]
      )

instance Prelude.ToPath SignalWorkflowExecution where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SignalWorkflowExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSignalWorkflowExecutionResponse' smart constructor.
data SignalWorkflowExecutionResponse = SignalWorkflowExecutionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SignalWorkflowExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSignalWorkflowExecutionResponse ::
  SignalWorkflowExecutionResponse
newSignalWorkflowExecutionResponse =
  SignalWorkflowExecutionResponse'

instance
  Prelude.NFData
    SignalWorkflowExecutionResponse
