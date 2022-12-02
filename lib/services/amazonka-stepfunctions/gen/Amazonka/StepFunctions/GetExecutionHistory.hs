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
-- Module      : Amazonka.StepFunctions.GetExecutionHistory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the history of the specified execution as a list of events. By
-- default, the results are returned in ascending order of the @timeStamp@
-- of the events. Use the @reverseOrder@ parameter to get the latest events
-- first.
--
-- If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
--
-- This API action is not supported by @EXPRESS@ state machines.
--
-- This operation returns paginated results.
module Amazonka.StepFunctions.GetExecutionHistory
  ( -- * Creating a Request
    GetExecutionHistory (..),
    newGetExecutionHistory,

    -- * Request Lenses
    getExecutionHistory_nextToken,
    getExecutionHistory_maxResults,
    getExecutionHistory_includeExecutionData,
    getExecutionHistory_reverseOrder,
    getExecutionHistory_executionArn,

    -- * Destructuring the Response
    GetExecutionHistoryResponse (..),
    newGetExecutionHistoryResponse,

    -- * Response Lenses
    getExecutionHistoryResponse_nextToken,
    getExecutionHistoryResponse_httpStatus,
    getExecutionHistoryResponse_events,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newGetExecutionHistory' smart constructor.
data GetExecutionHistory = GetExecutionHistory'
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
    -- | You can select whether execution data (input or output of a history
    -- event) is returned. The default is @true@.
    includeExecutionData :: Prelude.Maybe Prelude.Bool,
    -- | Lists events in descending order of their @timeStamp@.
    reverseOrder :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the execution.
    executionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExecutionHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getExecutionHistory_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
--
-- 'maxResults', 'getExecutionHistory_maxResults' - The maximum number of results that are returned per call. You can use
-- @nextToken@ to obtain further pages of results. The default is 100 and
-- the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per
-- call might be fewer than the specified maximum.
--
-- 'includeExecutionData', 'getExecutionHistory_includeExecutionData' - You can select whether execution data (input or output of a history
-- event) is returned. The default is @true@.
--
-- 'reverseOrder', 'getExecutionHistory_reverseOrder' - Lists events in descending order of their @timeStamp@.
--
-- 'executionArn', 'getExecutionHistory_executionArn' - The Amazon Resource Name (ARN) of the execution.
newGetExecutionHistory ::
  -- | 'executionArn'
  Prelude.Text ->
  GetExecutionHistory
newGetExecutionHistory pExecutionArn_ =
  GetExecutionHistory'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      includeExecutionData = Prelude.Nothing,
      reverseOrder = Prelude.Nothing,
      executionArn = pExecutionArn_
    }

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
getExecutionHistory_nextToken :: Lens.Lens' GetExecutionHistory (Prelude.Maybe Prelude.Text)
getExecutionHistory_nextToken = Lens.lens (\GetExecutionHistory' {nextToken} -> nextToken) (\s@GetExecutionHistory' {} a -> s {nextToken = a} :: GetExecutionHistory)

-- | The maximum number of results that are returned per call. You can use
-- @nextToken@ to obtain further pages of results. The default is 100 and
-- the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per
-- call might be fewer than the specified maximum.
getExecutionHistory_maxResults :: Lens.Lens' GetExecutionHistory (Prelude.Maybe Prelude.Natural)
getExecutionHistory_maxResults = Lens.lens (\GetExecutionHistory' {maxResults} -> maxResults) (\s@GetExecutionHistory' {} a -> s {maxResults = a} :: GetExecutionHistory)

-- | You can select whether execution data (input or output of a history
-- event) is returned. The default is @true@.
getExecutionHistory_includeExecutionData :: Lens.Lens' GetExecutionHistory (Prelude.Maybe Prelude.Bool)
getExecutionHistory_includeExecutionData = Lens.lens (\GetExecutionHistory' {includeExecutionData} -> includeExecutionData) (\s@GetExecutionHistory' {} a -> s {includeExecutionData = a} :: GetExecutionHistory)

-- | Lists events in descending order of their @timeStamp@.
getExecutionHistory_reverseOrder :: Lens.Lens' GetExecutionHistory (Prelude.Maybe Prelude.Bool)
getExecutionHistory_reverseOrder = Lens.lens (\GetExecutionHistory' {reverseOrder} -> reverseOrder) (\s@GetExecutionHistory' {} a -> s {reverseOrder = a} :: GetExecutionHistory)

-- | The Amazon Resource Name (ARN) of the execution.
getExecutionHistory_executionArn :: Lens.Lens' GetExecutionHistory Prelude.Text
getExecutionHistory_executionArn = Lens.lens (\GetExecutionHistory' {executionArn} -> executionArn) (\s@GetExecutionHistory' {} a -> s {executionArn = a} :: GetExecutionHistory)

instance Core.AWSPager GetExecutionHistory where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getExecutionHistoryResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. getExecutionHistoryResponse_events) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getExecutionHistory_nextToken
          Lens..~ rs
          Lens.^? getExecutionHistoryResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetExecutionHistory where
  type
    AWSResponse GetExecutionHistory =
      GetExecutionHistoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetExecutionHistoryResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "events" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable GetExecutionHistory where
  hashWithSalt _salt GetExecutionHistory' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` includeExecutionData
      `Prelude.hashWithSalt` reverseOrder
      `Prelude.hashWithSalt` executionArn

instance Prelude.NFData GetExecutionHistory where
  rnf GetExecutionHistory' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf includeExecutionData
      `Prelude.seq` Prelude.rnf reverseOrder
      `Prelude.seq` Prelude.rnf executionArn

instance Data.ToHeaders GetExecutionHistory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.GetExecutionHistory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetExecutionHistory where
  toJSON GetExecutionHistory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("includeExecutionData" Data..=)
              Prelude.<$> includeExecutionData,
            ("reverseOrder" Data..=) Prelude.<$> reverseOrder,
            Prelude.Just ("executionArn" Data..= executionArn)
          ]
      )

instance Data.ToPath GetExecutionHistory where
  toPath = Prelude.const "/"

instance Data.ToQuery GetExecutionHistory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetExecutionHistoryResponse' smart constructor.
data GetExecutionHistoryResponse = GetExecutionHistoryResponse'
  { -- | If @nextToken@ is returned, there are more results available. The value
    -- of @nextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours. Using
    -- an expired pagination token will return an /HTTP 400 InvalidToken/
    -- error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of events that occurred in the execution.
    events :: [HistoryEvent]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExecutionHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getExecutionHistoryResponse_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
--
-- 'httpStatus', 'getExecutionHistoryResponse_httpStatus' - The response's http status code.
--
-- 'events', 'getExecutionHistoryResponse_events' - The list of events that occurred in the execution.
newGetExecutionHistoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetExecutionHistoryResponse
newGetExecutionHistoryResponse pHttpStatus_ =
  GetExecutionHistoryResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      events = Prelude.mempty
    }

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
getExecutionHistoryResponse_nextToken :: Lens.Lens' GetExecutionHistoryResponse (Prelude.Maybe Prelude.Text)
getExecutionHistoryResponse_nextToken = Lens.lens (\GetExecutionHistoryResponse' {nextToken} -> nextToken) (\s@GetExecutionHistoryResponse' {} a -> s {nextToken = a} :: GetExecutionHistoryResponse)

-- | The response's http status code.
getExecutionHistoryResponse_httpStatus :: Lens.Lens' GetExecutionHistoryResponse Prelude.Int
getExecutionHistoryResponse_httpStatus = Lens.lens (\GetExecutionHistoryResponse' {httpStatus} -> httpStatus) (\s@GetExecutionHistoryResponse' {} a -> s {httpStatus = a} :: GetExecutionHistoryResponse)

-- | The list of events that occurred in the execution.
getExecutionHistoryResponse_events :: Lens.Lens' GetExecutionHistoryResponse [HistoryEvent]
getExecutionHistoryResponse_events = Lens.lens (\GetExecutionHistoryResponse' {events} -> events) (\s@GetExecutionHistoryResponse' {} a -> s {events = a} :: GetExecutionHistoryResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetExecutionHistoryResponse where
  rnf GetExecutionHistoryResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf events
