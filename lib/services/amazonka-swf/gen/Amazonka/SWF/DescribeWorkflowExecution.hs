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
-- Module      : Amazonka.SWF.DescribeWorkflowExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified workflow execution including its
-- type and some statistics.
--
-- This operation is eventually consistent. The results are best effort and
-- may not exactly reflect recent updates and changes.
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
module Amazonka.SWF.DescribeWorkflowExecution
  ( -- * Creating a Request
    DescribeWorkflowExecution (..),
    newDescribeWorkflowExecution,

    -- * Request Lenses
    describeWorkflowExecution_domain,
    describeWorkflowExecution_execution,

    -- * Destructuring the Response
    DescribeWorkflowExecutionResponse (..),
    newDescribeWorkflowExecutionResponse,

    -- * Response Lenses
    describeWorkflowExecutionResponse_latestActivityTaskTimestamp,
    describeWorkflowExecutionResponse_latestExecutionContext,
    describeWorkflowExecutionResponse_httpStatus,
    describeWorkflowExecutionResponse_executionInfo,
    describeWorkflowExecutionResponse_executionConfiguration,
    describeWorkflowExecutionResponse_openCounts,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SWF.Types

-- | /See:/ 'newDescribeWorkflowExecution' smart constructor.
data DescribeWorkflowExecution = DescribeWorkflowExecution'
  { -- | The name of the domain containing the workflow execution.
    domain :: Prelude.Text,
    -- | The workflow execution to describe.
    execution :: WorkflowExecution
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkflowExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'describeWorkflowExecution_domain' - The name of the domain containing the workflow execution.
--
-- 'execution', 'describeWorkflowExecution_execution' - The workflow execution to describe.
newDescribeWorkflowExecution ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'execution'
  WorkflowExecution ->
  DescribeWorkflowExecution
newDescribeWorkflowExecution pDomain_ pExecution_ =
  DescribeWorkflowExecution'
    { domain = pDomain_,
      execution = pExecution_
    }

-- | The name of the domain containing the workflow execution.
describeWorkflowExecution_domain :: Lens.Lens' DescribeWorkflowExecution Prelude.Text
describeWorkflowExecution_domain = Lens.lens (\DescribeWorkflowExecution' {domain} -> domain) (\s@DescribeWorkflowExecution' {} a -> s {domain = a} :: DescribeWorkflowExecution)

-- | The workflow execution to describe.
describeWorkflowExecution_execution :: Lens.Lens' DescribeWorkflowExecution WorkflowExecution
describeWorkflowExecution_execution = Lens.lens (\DescribeWorkflowExecution' {execution} -> execution) (\s@DescribeWorkflowExecution' {} a -> s {execution = a} :: DescribeWorkflowExecution)

instance Core.AWSRequest DescribeWorkflowExecution where
  type
    AWSResponse DescribeWorkflowExecution =
      DescribeWorkflowExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkflowExecutionResponse'
            Prelude.<$> (x Data..?> "latestActivityTaskTimestamp")
            Prelude.<*> (x Data..?> "latestExecutionContext")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "executionInfo")
            Prelude.<*> (x Data..:> "executionConfiguration")
            Prelude.<*> (x Data..:> "openCounts")
      )

instance Prelude.Hashable DescribeWorkflowExecution where
  hashWithSalt _salt DescribeWorkflowExecution' {..} =
    _salt
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` execution

instance Prelude.NFData DescribeWorkflowExecution where
  rnf DescribeWorkflowExecution' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf execution

instance Data.ToHeaders DescribeWorkflowExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SimpleWorkflowService.DescribeWorkflowExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeWorkflowExecution where
  toJSON DescribeWorkflowExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("domain" Data..= domain),
            Prelude.Just ("execution" Data..= execution)
          ]
      )

instance Data.ToPath DescribeWorkflowExecution where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeWorkflowExecution where
  toQuery = Prelude.const Prelude.mempty

-- | Contains details about a workflow execution.
--
-- /See:/ 'newDescribeWorkflowExecutionResponse' smart constructor.
data DescribeWorkflowExecutionResponse = DescribeWorkflowExecutionResponse'
  { -- | The time when the last activity task was scheduled for this workflow
    -- execution. You can use this information to determine if the workflow has
    -- not made progress for an unusually long period of time and might require
    -- a corrective action.
    latestActivityTaskTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The latest executionContext provided by the decider for this workflow
    -- execution. A decider can provide an executionContext (a free-form
    -- string) when closing a decision task using RespondDecisionTaskCompleted.
    latestExecutionContext :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the workflow execution.
    executionInfo :: WorkflowExecutionInfo,
    -- | The configuration settings for this workflow execution including timeout
    -- values, tasklist etc.
    executionConfiguration :: WorkflowExecutionConfiguration,
    -- | The number of tasks for this workflow execution. This includes open and
    -- closed tasks of all types.
    openCounts :: WorkflowExecutionOpenCounts
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkflowExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'latestActivityTaskTimestamp', 'describeWorkflowExecutionResponse_latestActivityTaskTimestamp' - The time when the last activity task was scheduled for this workflow
-- execution. You can use this information to determine if the workflow has
-- not made progress for an unusually long period of time and might require
-- a corrective action.
--
-- 'latestExecutionContext', 'describeWorkflowExecutionResponse_latestExecutionContext' - The latest executionContext provided by the decider for this workflow
-- execution. A decider can provide an executionContext (a free-form
-- string) when closing a decision task using RespondDecisionTaskCompleted.
--
-- 'httpStatus', 'describeWorkflowExecutionResponse_httpStatus' - The response's http status code.
--
-- 'executionInfo', 'describeWorkflowExecutionResponse_executionInfo' - Information about the workflow execution.
--
-- 'executionConfiguration', 'describeWorkflowExecutionResponse_executionConfiguration' - The configuration settings for this workflow execution including timeout
-- values, tasklist etc.
--
-- 'openCounts', 'describeWorkflowExecutionResponse_openCounts' - The number of tasks for this workflow execution. This includes open and
-- closed tasks of all types.
newDescribeWorkflowExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'executionInfo'
  WorkflowExecutionInfo ->
  -- | 'executionConfiguration'
  WorkflowExecutionConfiguration ->
  -- | 'openCounts'
  WorkflowExecutionOpenCounts ->
  DescribeWorkflowExecutionResponse
newDescribeWorkflowExecutionResponse
  pHttpStatus_
  pExecutionInfo_
  pExecutionConfiguration_
  pOpenCounts_ =
    DescribeWorkflowExecutionResponse'
      { latestActivityTaskTimestamp =
          Prelude.Nothing,
        latestExecutionContext = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        executionInfo = pExecutionInfo_,
        executionConfiguration =
          pExecutionConfiguration_,
        openCounts = pOpenCounts_
      }

-- | The time when the last activity task was scheduled for this workflow
-- execution. You can use this information to determine if the workflow has
-- not made progress for an unusually long period of time and might require
-- a corrective action.
describeWorkflowExecutionResponse_latestActivityTaskTimestamp :: Lens.Lens' DescribeWorkflowExecutionResponse (Prelude.Maybe Prelude.UTCTime)
describeWorkflowExecutionResponse_latestActivityTaskTimestamp = Lens.lens (\DescribeWorkflowExecutionResponse' {latestActivityTaskTimestamp} -> latestActivityTaskTimestamp) (\s@DescribeWorkflowExecutionResponse' {} a -> s {latestActivityTaskTimestamp = a} :: DescribeWorkflowExecutionResponse) Prelude.. Lens.mapping Data._Time

-- | The latest executionContext provided by the decider for this workflow
-- execution. A decider can provide an executionContext (a free-form
-- string) when closing a decision task using RespondDecisionTaskCompleted.
describeWorkflowExecutionResponse_latestExecutionContext :: Lens.Lens' DescribeWorkflowExecutionResponse (Prelude.Maybe Prelude.Text)
describeWorkflowExecutionResponse_latestExecutionContext = Lens.lens (\DescribeWorkflowExecutionResponse' {latestExecutionContext} -> latestExecutionContext) (\s@DescribeWorkflowExecutionResponse' {} a -> s {latestExecutionContext = a} :: DescribeWorkflowExecutionResponse)

-- | The response's http status code.
describeWorkflowExecutionResponse_httpStatus :: Lens.Lens' DescribeWorkflowExecutionResponse Prelude.Int
describeWorkflowExecutionResponse_httpStatus = Lens.lens (\DescribeWorkflowExecutionResponse' {httpStatus} -> httpStatus) (\s@DescribeWorkflowExecutionResponse' {} a -> s {httpStatus = a} :: DescribeWorkflowExecutionResponse)

-- | Information about the workflow execution.
describeWorkflowExecutionResponse_executionInfo :: Lens.Lens' DescribeWorkflowExecutionResponse WorkflowExecutionInfo
describeWorkflowExecutionResponse_executionInfo = Lens.lens (\DescribeWorkflowExecutionResponse' {executionInfo} -> executionInfo) (\s@DescribeWorkflowExecutionResponse' {} a -> s {executionInfo = a} :: DescribeWorkflowExecutionResponse)

-- | The configuration settings for this workflow execution including timeout
-- values, tasklist etc.
describeWorkflowExecutionResponse_executionConfiguration :: Lens.Lens' DescribeWorkflowExecutionResponse WorkflowExecutionConfiguration
describeWorkflowExecutionResponse_executionConfiguration = Lens.lens (\DescribeWorkflowExecutionResponse' {executionConfiguration} -> executionConfiguration) (\s@DescribeWorkflowExecutionResponse' {} a -> s {executionConfiguration = a} :: DescribeWorkflowExecutionResponse)

-- | The number of tasks for this workflow execution. This includes open and
-- closed tasks of all types.
describeWorkflowExecutionResponse_openCounts :: Lens.Lens' DescribeWorkflowExecutionResponse WorkflowExecutionOpenCounts
describeWorkflowExecutionResponse_openCounts = Lens.lens (\DescribeWorkflowExecutionResponse' {openCounts} -> openCounts) (\s@DescribeWorkflowExecutionResponse' {} a -> s {openCounts = a} :: DescribeWorkflowExecutionResponse)

instance
  Prelude.NFData
    DescribeWorkflowExecutionResponse
  where
  rnf DescribeWorkflowExecutionResponse' {..} =
    Prelude.rnf latestActivityTaskTimestamp
      `Prelude.seq` Prelude.rnf latestExecutionContext
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf executionInfo
      `Prelude.seq` Prelude.rnf executionConfiguration
      `Prelude.seq` Prelude.rnf openCounts
