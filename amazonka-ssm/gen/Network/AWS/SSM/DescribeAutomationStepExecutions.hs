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
-- Module      : Network.AWS.SSM.DescribeAutomationStepExecutions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information about all active and terminated step executions in an
-- Automation workflow.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeAutomationStepExecutions
  ( -- * Creating a Request
    DescribeAutomationStepExecutions (..),
    newDescribeAutomationStepExecutions,

    -- * Request Lenses
    describeAutomationStepExecutions_nextToken,
    describeAutomationStepExecutions_maxResults,
    describeAutomationStepExecutions_reverseOrder,
    describeAutomationStepExecutions_filters,
    describeAutomationStepExecutions_automationExecutionId,

    -- * Destructuring the Response
    DescribeAutomationStepExecutionsResponse (..),
    newDescribeAutomationStepExecutionsResponse,

    -- * Response Lenses
    describeAutomationStepExecutionsResponse_nextToken,
    describeAutomationStepExecutionsResponse_stepExecutions,
    describeAutomationStepExecutionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeAutomationStepExecutions' smart constructor.
data DescribeAutomationStepExecutions = DescribeAutomationStepExecutions'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | A boolean that indicates whether to list step executions in reverse
    -- order by start time. The default value is false.
    reverseOrder :: Core.Maybe Core.Bool,
    -- | One or more filters to limit the number of step executions returned by
    -- the request.
    filters :: Core.Maybe (Core.NonEmpty StepExecutionFilter),
    -- | The Automation execution ID for which you want step execution
    -- descriptions.
    automationExecutionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAutomationStepExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAutomationStepExecutions_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'describeAutomationStepExecutions_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'reverseOrder', 'describeAutomationStepExecutions_reverseOrder' - A boolean that indicates whether to list step executions in reverse
-- order by start time. The default value is false.
--
-- 'filters', 'describeAutomationStepExecutions_filters' - One or more filters to limit the number of step executions returned by
-- the request.
--
-- 'automationExecutionId', 'describeAutomationStepExecutions_automationExecutionId' - The Automation execution ID for which you want step execution
-- descriptions.
newDescribeAutomationStepExecutions ::
  -- | 'automationExecutionId'
  Core.Text ->
  DescribeAutomationStepExecutions
newDescribeAutomationStepExecutions
  pAutomationExecutionId_ =
    DescribeAutomationStepExecutions'
      { nextToken =
          Core.Nothing,
        maxResults = Core.Nothing,
        reverseOrder = Core.Nothing,
        filters = Core.Nothing,
        automationExecutionId =
          pAutomationExecutionId_
      }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeAutomationStepExecutions_nextToken :: Lens.Lens' DescribeAutomationStepExecutions (Core.Maybe Core.Text)
describeAutomationStepExecutions_nextToken = Lens.lens (\DescribeAutomationStepExecutions' {nextToken} -> nextToken) (\s@DescribeAutomationStepExecutions' {} a -> s {nextToken = a} :: DescribeAutomationStepExecutions)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeAutomationStepExecutions_maxResults :: Lens.Lens' DescribeAutomationStepExecutions (Core.Maybe Core.Natural)
describeAutomationStepExecutions_maxResults = Lens.lens (\DescribeAutomationStepExecutions' {maxResults} -> maxResults) (\s@DescribeAutomationStepExecutions' {} a -> s {maxResults = a} :: DescribeAutomationStepExecutions)

-- | A boolean that indicates whether to list step executions in reverse
-- order by start time. The default value is false.
describeAutomationStepExecutions_reverseOrder :: Lens.Lens' DescribeAutomationStepExecutions (Core.Maybe Core.Bool)
describeAutomationStepExecutions_reverseOrder = Lens.lens (\DescribeAutomationStepExecutions' {reverseOrder} -> reverseOrder) (\s@DescribeAutomationStepExecutions' {} a -> s {reverseOrder = a} :: DescribeAutomationStepExecutions)

-- | One or more filters to limit the number of step executions returned by
-- the request.
describeAutomationStepExecutions_filters :: Lens.Lens' DescribeAutomationStepExecutions (Core.Maybe (Core.NonEmpty StepExecutionFilter))
describeAutomationStepExecutions_filters = Lens.lens (\DescribeAutomationStepExecutions' {filters} -> filters) (\s@DescribeAutomationStepExecutions' {} a -> s {filters = a} :: DescribeAutomationStepExecutions) Core.. Lens.mapping Lens._Coerce

-- | The Automation execution ID for which you want step execution
-- descriptions.
describeAutomationStepExecutions_automationExecutionId :: Lens.Lens' DescribeAutomationStepExecutions Core.Text
describeAutomationStepExecutions_automationExecutionId = Lens.lens (\DescribeAutomationStepExecutions' {automationExecutionId} -> automationExecutionId) (\s@DescribeAutomationStepExecutions' {} a -> s {automationExecutionId = a} :: DescribeAutomationStepExecutions)

instance
  Core.AWSPager
    DescribeAutomationStepExecutions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAutomationStepExecutionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAutomationStepExecutionsResponse_stepExecutions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeAutomationStepExecutions_nextToken
          Lens..~ rs
          Lens.^? describeAutomationStepExecutionsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeAutomationStepExecutions
  where
  type
    AWSResponse DescribeAutomationStepExecutions =
      DescribeAutomationStepExecutionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAutomationStepExecutionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "StepExecutions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeAutomationStepExecutions

instance Core.NFData DescribeAutomationStepExecutions

instance
  Core.ToHeaders
    DescribeAutomationStepExecutions
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeAutomationStepExecutions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeAutomationStepExecutions where
  toJSON DescribeAutomationStepExecutions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("ReverseOrder" Core..=) Core.<$> reverseOrder,
            ("Filters" Core..=) Core.<$> filters,
            Core.Just
              ( "AutomationExecutionId"
                  Core..= automationExecutionId
              )
          ]
      )

instance Core.ToPath DescribeAutomationStepExecutions where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeAutomationStepExecutions
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeAutomationStepExecutionsResponse' smart constructor.
data DescribeAutomationStepExecutionsResponse = DescribeAutomationStepExecutionsResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of details about the current state of all steps that make up an
    -- execution.
    stepExecutions :: Core.Maybe [StepExecution],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAutomationStepExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAutomationStepExecutionsResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'stepExecutions', 'describeAutomationStepExecutionsResponse_stepExecutions' - A list of details about the current state of all steps that make up an
-- execution.
--
-- 'httpStatus', 'describeAutomationStepExecutionsResponse_httpStatus' - The response's http status code.
newDescribeAutomationStepExecutionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeAutomationStepExecutionsResponse
newDescribeAutomationStepExecutionsResponse
  pHttpStatus_ =
    DescribeAutomationStepExecutionsResponse'
      { nextToken =
          Core.Nothing,
        stepExecutions = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeAutomationStepExecutionsResponse_nextToken :: Lens.Lens' DescribeAutomationStepExecutionsResponse (Core.Maybe Core.Text)
describeAutomationStepExecutionsResponse_nextToken = Lens.lens (\DescribeAutomationStepExecutionsResponse' {nextToken} -> nextToken) (\s@DescribeAutomationStepExecutionsResponse' {} a -> s {nextToken = a} :: DescribeAutomationStepExecutionsResponse)

-- | A list of details about the current state of all steps that make up an
-- execution.
describeAutomationStepExecutionsResponse_stepExecutions :: Lens.Lens' DescribeAutomationStepExecutionsResponse (Core.Maybe [StepExecution])
describeAutomationStepExecutionsResponse_stepExecutions = Lens.lens (\DescribeAutomationStepExecutionsResponse' {stepExecutions} -> stepExecutions) (\s@DescribeAutomationStepExecutionsResponse' {} a -> s {stepExecutions = a} :: DescribeAutomationStepExecutionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAutomationStepExecutionsResponse_httpStatus :: Lens.Lens' DescribeAutomationStepExecutionsResponse Core.Int
describeAutomationStepExecutionsResponse_httpStatus = Lens.lens (\DescribeAutomationStepExecutionsResponse' {httpStatus} -> httpStatus) (\s@DescribeAutomationStepExecutionsResponse' {} a -> s {httpStatus = a} :: DescribeAutomationStepExecutionsResponse)

instance
  Core.NFData
    DescribeAutomationStepExecutionsResponse
