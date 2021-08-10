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
-- Module      : Network.AWS.SWF.DescribeWorkflowExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.SWF.DescribeWorkflowExecution
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
    describeWorkflowExecutionResponse_latestExecutionContext,
    describeWorkflowExecutionResponse_latestActivityTaskTimestamp,
    describeWorkflowExecutionResponse_httpStatus,
    describeWorkflowExecutionResponse_executionInfo,
    describeWorkflowExecutionResponse_executionConfiguration,
    describeWorkflowExecutionResponse_openCounts,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SWF.Types

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkflowExecutionResponse'
            Prelude.<$> (x Core..?> "latestExecutionContext")
            Prelude.<*> (x Core..?> "latestActivityTaskTimestamp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "executionInfo")
            Prelude.<*> (x Core..:> "executionConfiguration")
            Prelude.<*> (x Core..:> "openCounts")
      )

instance Prelude.Hashable DescribeWorkflowExecution

instance Prelude.NFData DescribeWorkflowExecution

instance Core.ToHeaders DescribeWorkflowExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SimpleWorkflowService.DescribeWorkflowExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeWorkflowExecution where
  toJSON DescribeWorkflowExecution' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("domain" Core..= domain),
            Prelude.Just ("execution" Core..= execution)
          ]
      )

instance Core.ToPath DescribeWorkflowExecution where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeWorkflowExecution where
  toQuery = Prelude.const Prelude.mempty

-- | Contains details about a workflow execution.
--
-- /See:/ 'newDescribeWorkflowExecutionResponse' smart constructor.
data DescribeWorkflowExecutionResponse = DescribeWorkflowExecutionResponse'
  { -- | The latest executionContext provided by the decider for this workflow
    -- execution. A decider can provide an executionContext (a free-form
    -- string) when closing a decision task using RespondDecisionTaskCompleted.
    latestExecutionContext :: Prelude.Maybe Prelude.Text,
    -- | The time when the last activity task was scheduled for this workflow
    -- execution. You can use this information to determine if the workflow has
    -- not made progress for an unusually long period of time and might require
    -- a corrective action.
    latestActivityTaskTimestamp :: Prelude.Maybe Core.POSIX,
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
-- 'latestExecutionContext', 'describeWorkflowExecutionResponse_latestExecutionContext' - The latest executionContext provided by the decider for this workflow
-- execution. A decider can provide an executionContext (a free-form
-- string) when closing a decision task using RespondDecisionTaskCompleted.
--
-- 'latestActivityTaskTimestamp', 'describeWorkflowExecutionResponse_latestActivityTaskTimestamp' - The time when the last activity task was scheduled for this workflow
-- execution. You can use this information to determine if the workflow has
-- not made progress for an unusually long period of time and might require
-- a corrective action.
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
      { latestExecutionContext =
          Prelude.Nothing,
        latestActivityTaskTimestamp =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        executionInfo = pExecutionInfo_,
        executionConfiguration =
          pExecutionConfiguration_,
        openCounts = pOpenCounts_
      }

-- | The latest executionContext provided by the decider for this workflow
-- execution. A decider can provide an executionContext (a free-form
-- string) when closing a decision task using RespondDecisionTaskCompleted.
describeWorkflowExecutionResponse_latestExecutionContext :: Lens.Lens' DescribeWorkflowExecutionResponse (Prelude.Maybe Prelude.Text)
describeWorkflowExecutionResponse_latestExecutionContext = Lens.lens (\DescribeWorkflowExecutionResponse' {latestExecutionContext} -> latestExecutionContext) (\s@DescribeWorkflowExecutionResponse' {} a -> s {latestExecutionContext = a} :: DescribeWorkflowExecutionResponse)

-- | The time when the last activity task was scheduled for this workflow
-- execution. You can use this information to determine if the workflow has
-- not made progress for an unusually long period of time and might require
-- a corrective action.
describeWorkflowExecutionResponse_latestActivityTaskTimestamp :: Lens.Lens' DescribeWorkflowExecutionResponse (Prelude.Maybe Prelude.UTCTime)
describeWorkflowExecutionResponse_latestActivityTaskTimestamp = Lens.lens (\DescribeWorkflowExecutionResponse' {latestActivityTaskTimestamp} -> latestActivityTaskTimestamp) (\s@DescribeWorkflowExecutionResponse' {} a -> s {latestActivityTaskTimestamp = a} :: DescribeWorkflowExecutionResponse) Prelude.. Lens.mapping Core._Time

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
