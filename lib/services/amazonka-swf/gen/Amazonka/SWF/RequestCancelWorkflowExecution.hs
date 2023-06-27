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
-- Module      : Amazonka.SWF.RequestCancelWorkflowExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records a @WorkflowExecutionCancelRequested@ event in the currently
-- running workflow execution identified by the given domain, workflowId,
-- and runId. This logically requests the cancellation of the workflow
-- execution as a whole. It is up to the decider to take appropriate
-- actions when it receives an execution history with this event.
--
-- If the runId isn\'t specified, the @WorkflowExecutionCancelRequested@
-- event is recorded in the history of the current open workflow execution
-- with the specified workflowId in the domain.
--
-- Because this action allows the workflow to properly clean up and
-- gracefully close, it should be used instead of
-- TerminateWorkflowExecution when possible.
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
module Amazonka.SWF.RequestCancelWorkflowExecution
  ( -- * Creating a Request
    RequestCancelWorkflowExecution (..),
    newRequestCancelWorkflowExecution,

    -- * Request Lenses
    requestCancelWorkflowExecution_runId,
    requestCancelWorkflowExecution_domain,
    requestCancelWorkflowExecution_workflowId,

    -- * Destructuring the Response
    RequestCancelWorkflowExecutionResponse (..),
    newRequestCancelWorkflowExecutionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SWF.Types

-- | /See:/ 'newRequestCancelWorkflowExecution' smart constructor.
data RequestCancelWorkflowExecution = RequestCancelWorkflowExecution'
  { -- | The runId of the workflow execution to cancel.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain containing the workflow execution to cancel.
    domain :: Prelude.Text,
    -- | The workflowId of the workflow execution to cancel.
    workflowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestCancelWorkflowExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runId', 'requestCancelWorkflowExecution_runId' - The runId of the workflow execution to cancel.
--
-- 'domain', 'requestCancelWorkflowExecution_domain' - The name of the domain containing the workflow execution to cancel.
--
-- 'workflowId', 'requestCancelWorkflowExecution_workflowId' - The workflowId of the workflow execution to cancel.
newRequestCancelWorkflowExecution ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'workflowId'
  Prelude.Text ->
  RequestCancelWorkflowExecution
newRequestCancelWorkflowExecution
  pDomain_
  pWorkflowId_ =
    RequestCancelWorkflowExecution'
      { runId =
          Prelude.Nothing,
        domain = pDomain_,
        workflowId = pWorkflowId_
      }

-- | The runId of the workflow execution to cancel.
requestCancelWorkflowExecution_runId :: Lens.Lens' RequestCancelWorkflowExecution (Prelude.Maybe Prelude.Text)
requestCancelWorkflowExecution_runId = Lens.lens (\RequestCancelWorkflowExecution' {runId} -> runId) (\s@RequestCancelWorkflowExecution' {} a -> s {runId = a} :: RequestCancelWorkflowExecution)

-- | The name of the domain containing the workflow execution to cancel.
requestCancelWorkflowExecution_domain :: Lens.Lens' RequestCancelWorkflowExecution Prelude.Text
requestCancelWorkflowExecution_domain = Lens.lens (\RequestCancelWorkflowExecution' {domain} -> domain) (\s@RequestCancelWorkflowExecution' {} a -> s {domain = a} :: RequestCancelWorkflowExecution)

-- | The workflowId of the workflow execution to cancel.
requestCancelWorkflowExecution_workflowId :: Lens.Lens' RequestCancelWorkflowExecution Prelude.Text
requestCancelWorkflowExecution_workflowId = Lens.lens (\RequestCancelWorkflowExecution' {workflowId} -> workflowId) (\s@RequestCancelWorkflowExecution' {} a -> s {workflowId = a} :: RequestCancelWorkflowExecution)

instance
  Core.AWSRequest
    RequestCancelWorkflowExecution
  where
  type
    AWSResponse RequestCancelWorkflowExecution =
      RequestCancelWorkflowExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      RequestCancelWorkflowExecutionResponse'

instance
  Prelude.Hashable
    RequestCancelWorkflowExecution
  where
  hashWithSalt
    _salt
    RequestCancelWorkflowExecution' {..} =
      _salt
        `Prelude.hashWithSalt` runId
        `Prelude.hashWithSalt` domain
        `Prelude.hashWithSalt` workflowId

instance
  Prelude.NFData
    RequestCancelWorkflowExecution
  where
  rnf RequestCancelWorkflowExecution' {..} =
    Prelude.rnf runId
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf workflowId

instance
  Data.ToHeaders
    RequestCancelWorkflowExecution
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SimpleWorkflowService.RequestCancelWorkflowExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RequestCancelWorkflowExecution where
  toJSON RequestCancelWorkflowExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("runId" Data..=) Prelude.<$> runId,
            Prelude.Just ("domain" Data..= domain),
            Prelude.Just ("workflowId" Data..= workflowId)
          ]
      )

instance Data.ToPath RequestCancelWorkflowExecution where
  toPath = Prelude.const "/"

instance Data.ToQuery RequestCancelWorkflowExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRequestCancelWorkflowExecutionResponse' smart constructor.
data RequestCancelWorkflowExecutionResponse = RequestCancelWorkflowExecutionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestCancelWorkflowExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRequestCancelWorkflowExecutionResponse ::
  RequestCancelWorkflowExecutionResponse
newRequestCancelWorkflowExecutionResponse =
  RequestCancelWorkflowExecutionResponse'

instance
  Prelude.NFData
    RequestCancelWorkflowExecutionResponse
  where
  rnf _ = ()
