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
-- Module      : Network.AWS.StepFunctions.ListExecutions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the executions of a state machine that meet the filtering
-- criteria. Results are sorted by time, with the most recent execution
-- first.
--
-- If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
--
-- This operation is eventually consistent. The results are best effort and
-- may not reflect very recent updates and changes.
--
-- This API action is not supported by @EXPRESS@ state machines.
--
-- This operation returns paginated results.
module Network.AWS.StepFunctions.ListExecutions
  ( -- * Creating a Request
    ListExecutions (..),
    newListExecutions,

    -- * Request Lenses
    listExecutions_nextToken,
    listExecutions_maxResults,
    listExecutions_statusFilter,
    listExecutions_stateMachineArn,

    -- * Destructuring the Response
    ListExecutionsResponse (..),
    newListExecutionsResponse,

    -- * Response Lenses
    listExecutionsResponse_nextToken,
    listExecutionsResponse_httpStatus,
    listExecutionsResponse_executions,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'newListExecutions' smart constructor.
data ListExecutions = ListExecutions'
  { -- | If @nextToken@ is returned, there are more results available. The value
    -- of @nextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours. Using
    -- an expired pagination token will return an /HTTP 400 InvalidToken/
    -- error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results that are returned per call. You can use
    -- @nextToken@ to obtain further pages of results. The default is 100 and
    -- the maximum allowed page size is 1000. A value of 0 uses the default.
    --
    -- This is only an upper limit. The actual number of results returned per
    -- call might be fewer than the specified maximum.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If specified, only list the executions whose current execution status
    -- matches the given filter.
    statusFilter :: Prelude.Maybe ExecutionStatus,
    -- | The Amazon Resource Name (ARN) of the state machine whose executions is
    -- listed.
    stateMachineArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listExecutions_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
--
-- 'maxResults', 'listExecutions_maxResults' - The maximum number of results that are returned per call. You can use
-- @nextToken@ to obtain further pages of results. The default is 100 and
-- the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per
-- call might be fewer than the specified maximum.
--
-- 'statusFilter', 'listExecutions_statusFilter' - If specified, only list the executions whose current execution status
-- matches the given filter.
--
-- 'stateMachineArn', 'listExecutions_stateMachineArn' - The Amazon Resource Name (ARN) of the state machine whose executions is
-- listed.
newListExecutions ::
  -- | 'stateMachineArn'
  Prelude.Text ->
  ListExecutions
newListExecutions pStateMachineArn_ =
  ListExecutions'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      statusFilter = Prelude.Nothing,
      stateMachineArn = pStateMachineArn_
    }

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
listExecutions_nextToken :: Lens.Lens' ListExecutions (Prelude.Maybe Prelude.Text)
listExecutions_nextToken = Lens.lens (\ListExecutions' {nextToken} -> nextToken) (\s@ListExecutions' {} a -> s {nextToken = a} :: ListExecutions)

-- | The maximum number of results that are returned per call. You can use
-- @nextToken@ to obtain further pages of results. The default is 100 and
-- the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per
-- call might be fewer than the specified maximum.
listExecutions_maxResults :: Lens.Lens' ListExecutions (Prelude.Maybe Prelude.Natural)
listExecutions_maxResults = Lens.lens (\ListExecutions' {maxResults} -> maxResults) (\s@ListExecutions' {} a -> s {maxResults = a} :: ListExecutions)

-- | If specified, only list the executions whose current execution status
-- matches the given filter.
listExecutions_statusFilter :: Lens.Lens' ListExecutions (Prelude.Maybe ExecutionStatus)
listExecutions_statusFilter = Lens.lens (\ListExecutions' {statusFilter} -> statusFilter) (\s@ListExecutions' {} a -> s {statusFilter = a} :: ListExecutions)

-- | The Amazon Resource Name (ARN) of the state machine whose executions is
-- listed.
listExecutions_stateMachineArn :: Lens.Lens' ListExecutions Prelude.Text
listExecutions_stateMachineArn = Lens.lens (\ListExecutions' {stateMachineArn} -> stateMachineArn) (\s@ListExecutions' {} a -> s {stateMachineArn = a} :: ListExecutions)

instance Core.AWSPager ListExecutions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listExecutionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listExecutionsResponse_executions) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listExecutions_nextToken
          Lens..~ rs
          Lens.^? listExecutionsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListExecutions where
  type
    AWSResponse ListExecutions =
      ListExecutionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListExecutionsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "executions" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListExecutions

instance Prelude.NFData ListExecutions

instance Core.ToHeaders ListExecutions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSStepFunctions.ListExecutions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListExecutions where
  toJSON ListExecutions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("statusFilter" Core..=) Prelude.<$> statusFilter,
            Prelude.Just
              ("stateMachineArn" Core..= stateMachineArn)
          ]
      )

instance Core.ToPath ListExecutions where
  toPath = Prelude.const "/"

instance Core.ToQuery ListExecutions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListExecutionsResponse' smart constructor.
data ListExecutionsResponse = ListExecutionsResponse'
  { -- | If @nextToken@ is returned, there are more results available. The value
    -- of @nextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours. Using
    -- an expired pagination token will return an /HTTP 400 InvalidToken/
    -- error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of matching executions.
    executions :: [ExecutionListItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listExecutionsResponse_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
--
-- 'httpStatus', 'listExecutionsResponse_httpStatus' - The response's http status code.
--
-- 'executions', 'listExecutionsResponse_executions' - The list of matching executions.
newListExecutionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListExecutionsResponse
newListExecutionsResponse pHttpStatus_ =
  ListExecutionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      executions = Prelude.mempty
    }

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
listExecutionsResponse_nextToken :: Lens.Lens' ListExecutionsResponse (Prelude.Maybe Prelude.Text)
listExecutionsResponse_nextToken = Lens.lens (\ListExecutionsResponse' {nextToken} -> nextToken) (\s@ListExecutionsResponse' {} a -> s {nextToken = a} :: ListExecutionsResponse)

-- | The response's http status code.
listExecutionsResponse_httpStatus :: Lens.Lens' ListExecutionsResponse Prelude.Int
listExecutionsResponse_httpStatus = Lens.lens (\ListExecutionsResponse' {httpStatus} -> httpStatus) (\s@ListExecutionsResponse' {} a -> s {httpStatus = a} :: ListExecutionsResponse)

-- | The list of matching executions.
listExecutionsResponse_executions :: Lens.Lens' ListExecutionsResponse [ExecutionListItem]
listExecutionsResponse_executions = Lens.lens (\ListExecutionsResponse' {executions} -> executions) (\s@ListExecutionsResponse' {} a -> s {executions = a} :: ListExecutionsResponse) Prelude.. Lens._Coerce

instance Prelude.NFData ListExecutionsResponse
