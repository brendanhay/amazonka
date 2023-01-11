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
-- Module      : Amazonka.StepFunctions.ListExecutions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all executions of a state machine or a Map Run. You can list all
-- executions related to a state machine by specifying a state machine
-- Amazon Resource Name (ARN), or those related to a Map Run by specifying
-- a Map Run ARN.
--
-- Results are sorted by time, with the most recent execution first.
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
module Amazonka.StepFunctions.ListExecutions
  ( -- * Creating a Request
    ListExecutions (..),
    newListExecutions,

    -- * Request Lenses
    listExecutions_mapRunArn,
    listExecutions_maxResults,
    listExecutions_nextToken,
    listExecutions_stateMachineArn,
    listExecutions_statusFilter,

    -- * Destructuring the Response
    ListExecutionsResponse (..),
    newListExecutionsResponse,

    -- * Response Lenses
    listExecutionsResponse_nextToken,
    listExecutionsResponse_httpStatus,
    listExecutionsResponse_executions,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newListExecutions' smart constructor.
data ListExecutions = ListExecutions'
  { -- | The Amazon Resource Name (ARN) of the Map Run that started the child
    -- workflow executions. If the @mapRunArn@ field is specified, a list of
    -- all of the child workflow executions started by a Map Run is returned.
    -- For more information, see
    -- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-examine-map-run.html Examining Map Run>
    -- in the /Step Functions Developer Guide/.
    --
    -- You can specify either a @mapRunArn@ or a @stateMachineArn@, but not
    -- both.
    mapRunArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results that are returned per call. You can use
    -- @nextToken@ to obtain further pages of results. The default is 100 and
    -- the maximum allowed page size is 1000. A value of 0 uses the default.
    --
    -- This is only an upper limit. The actual number of results returned per
    -- call might be fewer than the specified maximum.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If @nextToken@ is returned, there are more results available. The value
    -- of @nextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours. Using
    -- an expired pagination token will return an /HTTP 400 InvalidToken/
    -- error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the state machine whose executions is
    -- listed.
    --
    -- You can specify either a @mapRunArn@ or a @stateMachineArn@, but not
    -- both.
    stateMachineArn :: Prelude.Maybe Prelude.Text,
    -- | If specified, only list the executions whose current execution status
    -- matches the given filter.
    statusFilter :: Prelude.Maybe ExecutionStatus
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
-- 'mapRunArn', 'listExecutions_mapRunArn' - The Amazon Resource Name (ARN) of the Map Run that started the child
-- workflow executions. If the @mapRunArn@ field is specified, a list of
-- all of the child workflow executions started by a Map Run is returned.
-- For more information, see
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-examine-map-run.html Examining Map Run>
-- in the /Step Functions Developer Guide/.
--
-- You can specify either a @mapRunArn@ or a @stateMachineArn@, but not
-- both.
--
-- 'maxResults', 'listExecutions_maxResults' - The maximum number of results that are returned per call. You can use
-- @nextToken@ to obtain further pages of results. The default is 100 and
-- the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per
-- call might be fewer than the specified maximum.
--
-- 'nextToken', 'listExecutions_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
--
-- 'stateMachineArn', 'listExecutions_stateMachineArn' - The Amazon Resource Name (ARN) of the state machine whose executions is
-- listed.
--
-- You can specify either a @mapRunArn@ or a @stateMachineArn@, but not
-- both.
--
-- 'statusFilter', 'listExecutions_statusFilter' - If specified, only list the executions whose current execution status
-- matches the given filter.
newListExecutions ::
  ListExecutions
newListExecutions =
  ListExecutions'
    { mapRunArn = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      stateMachineArn = Prelude.Nothing,
      statusFilter = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Map Run that started the child
-- workflow executions. If the @mapRunArn@ field is specified, a list of
-- all of the child workflow executions started by a Map Run is returned.
-- For more information, see
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-examine-map-run.html Examining Map Run>
-- in the /Step Functions Developer Guide/.
--
-- You can specify either a @mapRunArn@ or a @stateMachineArn@, but not
-- both.
listExecutions_mapRunArn :: Lens.Lens' ListExecutions (Prelude.Maybe Prelude.Text)
listExecutions_mapRunArn = Lens.lens (\ListExecutions' {mapRunArn} -> mapRunArn) (\s@ListExecutions' {} a -> s {mapRunArn = a} :: ListExecutions)

-- | The maximum number of results that are returned per call. You can use
-- @nextToken@ to obtain further pages of results. The default is 100 and
-- the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per
-- call might be fewer than the specified maximum.
listExecutions_maxResults :: Lens.Lens' ListExecutions (Prelude.Maybe Prelude.Natural)
listExecutions_maxResults = Lens.lens (\ListExecutions' {maxResults} -> maxResults) (\s@ListExecutions' {} a -> s {maxResults = a} :: ListExecutions)

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
listExecutions_nextToken :: Lens.Lens' ListExecutions (Prelude.Maybe Prelude.Text)
listExecutions_nextToken = Lens.lens (\ListExecutions' {nextToken} -> nextToken) (\s@ListExecutions' {} a -> s {nextToken = a} :: ListExecutions)

-- | The Amazon Resource Name (ARN) of the state machine whose executions is
-- listed.
--
-- You can specify either a @mapRunArn@ or a @stateMachineArn@, but not
-- both.
listExecutions_stateMachineArn :: Lens.Lens' ListExecutions (Prelude.Maybe Prelude.Text)
listExecutions_stateMachineArn = Lens.lens (\ListExecutions' {stateMachineArn} -> stateMachineArn) (\s@ListExecutions' {} a -> s {stateMachineArn = a} :: ListExecutions)

-- | If specified, only list the executions whose current execution status
-- matches the given filter.
listExecutions_statusFilter :: Lens.Lens' ListExecutions (Prelude.Maybe ExecutionStatus)
listExecutions_statusFilter = Lens.lens (\ListExecutions' {statusFilter} -> statusFilter) (\s@ListExecutions' {} a -> s {statusFilter = a} :: ListExecutions)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListExecutionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "executions" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListExecutions where
  hashWithSalt _salt ListExecutions' {..} =
    _salt `Prelude.hashWithSalt` mapRunArn
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` stateMachineArn
      `Prelude.hashWithSalt` statusFilter

instance Prelude.NFData ListExecutions where
  rnf ListExecutions' {..} =
    Prelude.rnf mapRunArn
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf stateMachineArn
      `Prelude.seq` Prelude.rnf statusFilter

instance Data.ToHeaders ListExecutions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.ListExecutions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListExecutions where
  toJSON ListExecutions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("mapRunArn" Data..=) Prelude.<$> mapRunArn,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("stateMachineArn" Data..=)
              Prelude.<$> stateMachineArn,
            ("statusFilter" Data..=) Prelude.<$> statusFilter
          ]
      )

instance Data.ToPath ListExecutions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListExecutions where
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
listExecutionsResponse_executions = Lens.lens (\ListExecutionsResponse' {executions} -> executions) (\s@ListExecutionsResponse' {} a -> s {executions = a} :: ListExecutionsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListExecutionsResponse where
  rnf ListExecutionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf executions
