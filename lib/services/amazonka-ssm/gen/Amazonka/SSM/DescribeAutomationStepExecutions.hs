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
-- Module      : Amazonka.SSM.DescribeAutomationStepExecutions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information about all active and terminated step executions in an
-- Automation workflow.
--
-- This operation returns paginated results.
module Amazonka.SSM.DescribeAutomationStepExecutions
  ( -- * Creating a Request
    DescribeAutomationStepExecutions (..),
    newDescribeAutomationStepExecutions,

    -- * Request Lenses
    describeAutomationStepExecutions_filters,
    describeAutomationStepExecutions_maxResults,
    describeAutomationStepExecutions_nextToken,
    describeAutomationStepExecutions_reverseOrder,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDescribeAutomationStepExecutions' smart constructor.
data DescribeAutomationStepExecutions = DescribeAutomationStepExecutions'
  { -- | One or more filters to limit the number of step executions returned by
    -- the request.
    filters :: Prelude.Maybe (Prelude.NonEmpty StepExecutionFilter),
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to list step executions in reverse order by start
    -- time. The default value is \'false\'.
    reverseOrder :: Prelude.Maybe Prelude.Bool,
    -- | The Automation execution ID for which you want step execution
    -- descriptions.
    automationExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAutomationStepExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeAutomationStepExecutions_filters' - One or more filters to limit the number of step executions returned by
-- the request.
--
-- 'maxResults', 'describeAutomationStepExecutions_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'nextToken', 'describeAutomationStepExecutions_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'reverseOrder', 'describeAutomationStepExecutions_reverseOrder' - Indicates whether to list step executions in reverse order by start
-- time. The default value is \'false\'.
--
-- 'automationExecutionId', 'describeAutomationStepExecutions_automationExecutionId' - The Automation execution ID for which you want step execution
-- descriptions.
newDescribeAutomationStepExecutions ::
  -- | 'automationExecutionId'
  Prelude.Text ->
  DescribeAutomationStepExecutions
newDescribeAutomationStepExecutions
  pAutomationExecutionId_ =
    DescribeAutomationStepExecutions'
      { filters =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        reverseOrder = Prelude.Nothing,
        automationExecutionId =
          pAutomationExecutionId_
      }

-- | One or more filters to limit the number of step executions returned by
-- the request.
describeAutomationStepExecutions_filters :: Lens.Lens' DescribeAutomationStepExecutions (Prelude.Maybe (Prelude.NonEmpty StepExecutionFilter))
describeAutomationStepExecutions_filters = Lens.lens (\DescribeAutomationStepExecutions' {filters} -> filters) (\s@DescribeAutomationStepExecutions' {} a -> s {filters = a} :: DescribeAutomationStepExecutions) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeAutomationStepExecutions_maxResults :: Lens.Lens' DescribeAutomationStepExecutions (Prelude.Maybe Prelude.Natural)
describeAutomationStepExecutions_maxResults = Lens.lens (\DescribeAutomationStepExecutions' {maxResults} -> maxResults) (\s@DescribeAutomationStepExecutions' {} a -> s {maxResults = a} :: DescribeAutomationStepExecutions)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeAutomationStepExecutions_nextToken :: Lens.Lens' DescribeAutomationStepExecutions (Prelude.Maybe Prelude.Text)
describeAutomationStepExecutions_nextToken = Lens.lens (\DescribeAutomationStepExecutions' {nextToken} -> nextToken) (\s@DescribeAutomationStepExecutions' {} a -> s {nextToken = a} :: DescribeAutomationStepExecutions)

-- | Indicates whether to list step executions in reverse order by start
-- time. The default value is \'false\'.
describeAutomationStepExecutions_reverseOrder :: Lens.Lens' DescribeAutomationStepExecutions (Prelude.Maybe Prelude.Bool)
describeAutomationStepExecutions_reverseOrder = Lens.lens (\DescribeAutomationStepExecutions' {reverseOrder} -> reverseOrder) (\s@DescribeAutomationStepExecutions' {} a -> s {reverseOrder = a} :: DescribeAutomationStepExecutions)

-- | The Automation execution ID for which you want step execution
-- descriptions.
describeAutomationStepExecutions_automationExecutionId :: Lens.Lens' DescribeAutomationStepExecutions Prelude.Text
describeAutomationStepExecutions_automationExecutionId = Lens.lens (\DescribeAutomationStepExecutions' {automationExecutionId} -> automationExecutionId) (\s@DescribeAutomationStepExecutions' {} a -> s {automationExecutionId = a} :: DescribeAutomationStepExecutions)

instance
  Core.AWSPager
    DescribeAutomationStepExecutions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAutomationStepExecutionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAutomationStepExecutionsResponse_stepExecutions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeAutomationStepExecutions_nextToken
          Lens..~ rs
          Lens.^? describeAutomationStepExecutionsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeAutomationStepExecutions
  where
  type
    AWSResponse DescribeAutomationStepExecutions =
      DescribeAutomationStepExecutionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAutomationStepExecutionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "StepExecutions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAutomationStepExecutions
  where
  hashWithSalt
    _salt
    DescribeAutomationStepExecutions' {..} =
      _salt `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` reverseOrder
        `Prelude.hashWithSalt` automationExecutionId

instance
  Prelude.NFData
    DescribeAutomationStepExecutions
  where
  rnf DescribeAutomationStepExecutions' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf reverseOrder
      `Prelude.seq` Prelude.rnf automationExecutionId

instance
  Data.ToHeaders
    DescribeAutomationStepExecutions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.DescribeAutomationStepExecutions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAutomationStepExecutions where
  toJSON DescribeAutomationStepExecutions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ReverseOrder" Data..=) Prelude.<$> reverseOrder,
            Prelude.Just
              ( "AutomationExecutionId"
                  Data..= automationExecutionId
              )
          ]
      )

instance Data.ToPath DescribeAutomationStepExecutions where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeAutomationStepExecutions
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAutomationStepExecutionsResponse' smart constructor.
data DescribeAutomationStepExecutionsResponse = DescribeAutomationStepExecutionsResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of details about the current state of all steps that make up an
    -- execution.
    stepExecutions :: Prelude.Maybe [StepExecution],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeAutomationStepExecutionsResponse
newDescribeAutomationStepExecutionsResponse
  pHttpStatus_ =
    DescribeAutomationStepExecutionsResponse'
      { nextToken =
          Prelude.Nothing,
        stepExecutions = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeAutomationStepExecutionsResponse_nextToken :: Lens.Lens' DescribeAutomationStepExecutionsResponse (Prelude.Maybe Prelude.Text)
describeAutomationStepExecutionsResponse_nextToken = Lens.lens (\DescribeAutomationStepExecutionsResponse' {nextToken} -> nextToken) (\s@DescribeAutomationStepExecutionsResponse' {} a -> s {nextToken = a} :: DescribeAutomationStepExecutionsResponse)

-- | A list of details about the current state of all steps that make up an
-- execution.
describeAutomationStepExecutionsResponse_stepExecutions :: Lens.Lens' DescribeAutomationStepExecutionsResponse (Prelude.Maybe [StepExecution])
describeAutomationStepExecutionsResponse_stepExecutions = Lens.lens (\DescribeAutomationStepExecutionsResponse' {stepExecutions} -> stepExecutions) (\s@DescribeAutomationStepExecutionsResponse' {} a -> s {stepExecutions = a} :: DescribeAutomationStepExecutionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAutomationStepExecutionsResponse_httpStatus :: Lens.Lens' DescribeAutomationStepExecutionsResponse Prelude.Int
describeAutomationStepExecutionsResponse_httpStatus = Lens.lens (\DescribeAutomationStepExecutionsResponse' {httpStatus} -> httpStatus) (\s@DescribeAutomationStepExecutionsResponse' {} a -> s {httpStatus = a} :: DescribeAutomationStepExecutionsResponse)

instance
  Prelude.NFData
    DescribeAutomationStepExecutionsResponse
  where
  rnf DescribeAutomationStepExecutionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf stepExecutions
      `Prelude.seq` Prelude.rnf httpStatus
