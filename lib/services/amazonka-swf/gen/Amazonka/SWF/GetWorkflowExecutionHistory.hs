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
-- Module      : Amazonka.SWF.GetWorkflowExecutionHistory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the history of the specified workflow execution. The results may
-- be split into multiple pages. To retrieve subsequent pages, make the
-- call again using the @nextPageToken@ returned by the initial call.
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
--
-- This operation returns paginated results.
module Amazonka.SWF.GetWorkflowExecutionHistory
  ( -- * Creating a Request
    GetWorkflowExecutionHistory (..),
    newGetWorkflowExecutionHistory,

    -- * Request Lenses
    getWorkflowExecutionHistory_maximumPageSize,
    getWorkflowExecutionHistory_nextPageToken,
    getWorkflowExecutionHistory_reverseOrder,
    getWorkflowExecutionHistory_domain,
    getWorkflowExecutionHistory_execution,

    -- * Destructuring the Response
    GetWorkflowExecutionHistoryResponse (..),
    newGetWorkflowExecutionHistoryResponse,

    -- * Response Lenses
    getWorkflowExecutionHistoryResponse_nextPageToken,
    getWorkflowExecutionHistoryResponse_httpStatus,
    getWorkflowExecutionHistoryResponse_events,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SWF.Types

-- | /See:/ 'newGetWorkflowExecutionHistory' smart constructor.
data GetWorkflowExecutionHistory = GetWorkflowExecutionHistory'
  { -- | The maximum number of results that are returned per call. Use
    -- @nextPageToken@ to obtain further pages of results.
    maximumPageSize :: Prelude.Maybe Prelude.Natural,
    -- | If @NextPageToken@ is returned there are more results available. The
    -- value of @NextPageToken@ is a unique pagination token for each page.
    -- Make the call again using the returned token to retrieve the next page.
    -- Keep all other arguments unchanged. Each pagination token expires after
    -- 60 seconds. Using an expired pagination token will return a @400@ error:
    -- \"@Specified token has exceeded its maximum lifetime@\".
    --
    -- The configured @maximumPageSize@ determines how many results can be
    -- returned in a single call.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | When set to @true@, returns the events in reverse order. By default the
    -- results are returned in ascending order of the @eventTimeStamp@ of the
    -- events.
    reverseOrder :: Prelude.Maybe Prelude.Bool,
    -- | The name of the domain containing the workflow execution.
    domain :: Prelude.Text,
    -- | Specifies the workflow execution for which to return the history.
    execution :: WorkflowExecution
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflowExecutionHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumPageSize', 'getWorkflowExecutionHistory_maximumPageSize' - The maximum number of results that are returned per call. Use
-- @nextPageToken@ to obtain further pages of results.
--
-- 'nextPageToken', 'getWorkflowExecutionHistory_nextPageToken' - If @NextPageToken@ is returned there are more results available. The
-- value of @NextPageToken@ is a unique pagination token for each page.
-- Make the call again using the returned token to retrieve the next page.
-- Keep all other arguments unchanged. Each pagination token expires after
-- 60 seconds. Using an expired pagination token will return a @400@ error:
-- \"@Specified token has exceeded its maximum lifetime@\".
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
--
-- 'reverseOrder', 'getWorkflowExecutionHistory_reverseOrder' - When set to @true@, returns the events in reverse order. By default the
-- results are returned in ascending order of the @eventTimeStamp@ of the
-- events.
--
-- 'domain', 'getWorkflowExecutionHistory_domain' - The name of the domain containing the workflow execution.
--
-- 'execution', 'getWorkflowExecutionHistory_execution' - Specifies the workflow execution for which to return the history.
newGetWorkflowExecutionHistory ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'execution'
  WorkflowExecution ->
  GetWorkflowExecutionHistory
newGetWorkflowExecutionHistory pDomain_ pExecution_ =
  GetWorkflowExecutionHistory'
    { maximumPageSize =
        Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      reverseOrder = Prelude.Nothing,
      domain = pDomain_,
      execution = pExecution_
    }

-- | The maximum number of results that are returned per call. Use
-- @nextPageToken@ to obtain further pages of results.
getWorkflowExecutionHistory_maximumPageSize :: Lens.Lens' GetWorkflowExecutionHistory (Prelude.Maybe Prelude.Natural)
getWorkflowExecutionHistory_maximumPageSize = Lens.lens (\GetWorkflowExecutionHistory' {maximumPageSize} -> maximumPageSize) (\s@GetWorkflowExecutionHistory' {} a -> s {maximumPageSize = a} :: GetWorkflowExecutionHistory)

-- | If @NextPageToken@ is returned there are more results available. The
-- value of @NextPageToken@ is a unique pagination token for each page.
-- Make the call again using the returned token to retrieve the next page.
-- Keep all other arguments unchanged. Each pagination token expires after
-- 60 seconds. Using an expired pagination token will return a @400@ error:
-- \"@Specified token has exceeded its maximum lifetime@\".
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
getWorkflowExecutionHistory_nextPageToken :: Lens.Lens' GetWorkflowExecutionHistory (Prelude.Maybe Prelude.Text)
getWorkflowExecutionHistory_nextPageToken = Lens.lens (\GetWorkflowExecutionHistory' {nextPageToken} -> nextPageToken) (\s@GetWorkflowExecutionHistory' {} a -> s {nextPageToken = a} :: GetWorkflowExecutionHistory)

-- | When set to @true@, returns the events in reverse order. By default the
-- results are returned in ascending order of the @eventTimeStamp@ of the
-- events.
getWorkflowExecutionHistory_reverseOrder :: Lens.Lens' GetWorkflowExecutionHistory (Prelude.Maybe Prelude.Bool)
getWorkflowExecutionHistory_reverseOrder = Lens.lens (\GetWorkflowExecutionHistory' {reverseOrder} -> reverseOrder) (\s@GetWorkflowExecutionHistory' {} a -> s {reverseOrder = a} :: GetWorkflowExecutionHistory)

-- | The name of the domain containing the workflow execution.
getWorkflowExecutionHistory_domain :: Lens.Lens' GetWorkflowExecutionHistory Prelude.Text
getWorkflowExecutionHistory_domain = Lens.lens (\GetWorkflowExecutionHistory' {domain} -> domain) (\s@GetWorkflowExecutionHistory' {} a -> s {domain = a} :: GetWorkflowExecutionHistory)

-- | Specifies the workflow execution for which to return the history.
getWorkflowExecutionHistory_execution :: Lens.Lens' GetWorkflowExecutionHistory WorkflowExecution
getWorkflowExecutionHistory_execution = Lens.lens (\GetWorkflowExecutionHistory' {execution} -> execution) (\s@GetWorkflowExecutionHistory' {} a -> s {execution = a} :: GetWorkflowExecutionHistory)

instance Core.AWSPager GetWorkflowExecutionHistory where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getWorkflowExecutionHistoryResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. getWorkflowExecutionHistoryResponse_events
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getWorkflowExecutionHistory_nextPageToken
          Lens..~ rs
          Lens.^? getWorkflowExecutionHistoryResponse_nextPageToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetWorkflowExecutionHistory where
  type
    AWSResponse GetWorkflowExecutionHistory =
      GetWorkflowExecutionHistoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkflowExecutionHistoryResponse'
            Prelude.<$> (x Core..?> "nextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "events" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable GetWorkflowExecutionHistory where
  hashWithSalt _salt GetWorkflowExecutionHistory' {..} =
    _salt `Prelude.hashWithSalt` maximumPageSize
      `Prelude.hashWithSalt` nextPageToken
      `Prelude.hashWithSalt` reverseOrder
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` execution

instance Prelude.NFData GetWorkflowExecutionHistory where
  rnf GetWorkflowExecutionHistory' {..} =
    Prelude.rnf maximumPageSize
      `Prelude.seq` Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf reverseOrder
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf execution

instance Core.ToHeaders GetWorkflowExecutionHistory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SimpleWorkflowService.GetWorkflowExecutionHistory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetWorkflowExecutionHistory where
  toJSON GetWorkflowExecutionHistory' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("maximumPageSize" Core..=)
              Prelude.<$> maximumPageSize,
            ("nextPageToken" Core..=) Prelude.<$> nextPageToken,
            ("reverseOrder" Core..=) Prelude.<$> reverseOrder,
            Prelude.Just ("domain" Core..= domain),
            Prelude.Just ("execution" Core..= execution)
          ]
      )

instance Core.ToPath GetWorkflowExecutionHistory where
  toPath = Prelude.const "/"

instance Core.ToQuery GetWorkflowExecutionHistory where
  toQuery = Prelude.const Prelude.mempty

-- | Paginated representation of a workflow history for a workflow execution.
-- This is the up to date, complete and authoritative record of the events
-- related to all tasks and events in the life of the workflow execution.
--
-- /See:/ 'newGetWorkflowExecutionHistoryResponse' smart constructor.
data GetWorkflowExecutionHistoryResponse = GetWorkflowExecutionHistoryResponse'
  { -- | If a @NextPageToken@ was returned by a previous call, there are more
    -- results available. To retrieve the next page of results, make the call
    -- again using the returned token in @nextPageToken@. Keep all other
    -- arguments unchanged.
    --
    -- The configured @maximumPageSize@ determines how many results can be
    -- returned in a single call.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of history events.
    events :: [HistoryEvent]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflowExecutionHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getWorkflowExecutionHistoryResponse_nextPageToken' - If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
--
-- 'httpStatus', 'getWorkflowExecutionHistoryResponse_httpStatus' - The response's http status code.
--
-- 'events', 'getWorkflowExecutionHistoryResponse_events' - The list of history events.
newGetWorkflowExecutionHistoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWorkflowExecutionHistoryResponse
newGetWorkflowExecutionHistoryResponse pHttpStatus_ =
  GetWorkflowExecutionHistoryResponse'
    { nextPageToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      events = Prelude.mempty
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
getWorkflowExecutionHistoryResponse_nextPageToken :: Lens.Lens' GetWorkflowExecutionHistoryResponse (Prelude.Maybe Prelude.Text)
getWorkflowExecutionHistoryResponse_nextPageToken = Lens.lens (\GetWorkflowExecutionHistoryResponse' {nextPageToken} -> nextPageToken) (\s@GetWorkflowExecutionHistoryResponse' {} a -> s {nextPageToken = a} :: GetWorkflowExecutionHistoryResponse)

-- | The response's http status code.
getWorkflowExecutionHistoryResponse_httpStatus :: Lens.Lens' GetWorkflowExecutionHistoryResponse Prelude.Int
getWorkflowExecutionHistoryResponse_httpStatus = Lens.lens (\GetWorkflowExecutionHistoryResponse' {httpStatus} -> httpStatus) (\s@GetWorkflowExecutionHistoryResponse' {} a -> s {httpStatus = a} :: GetWorkflowExecutionHistoryResponse)

-- | The list of history events.
getWorkflowExecutionHistoryResponse_events :: Lens.Lens' GetWorkflowExecutionHistoryResponse [HistoryEvent]
getWorkflowExecutionHistoryResponse_events = Lens.lens (\GetWorkflowExecutionHistoryResponse' {events} -> events) (\s@GetWorkflowExecutionHistoryResponse' {} a -> s {events = a} :: GetWorkflowExecutionHistoryResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    GetWorkflowExecutionHistoryResponse
  where
  rnf GetWorkflowExecutionHistoryResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf events
