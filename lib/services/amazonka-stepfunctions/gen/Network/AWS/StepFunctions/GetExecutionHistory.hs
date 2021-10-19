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
-- Module      : Network.AWS.StepFunctions.GetExecutionHistory
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.StepFunctions.GetExecutionHistory
  ( -- * Creating a Request
    GetExecutionHistory (..),
    newGetExecutionHistory,

    -- * Request Lenses
    getExecutionHistory_reverseOrder,
    getExecutionHistory_includeExecutionData,
    getExecutionHistory_nextToken,
    getExecutionHistory_maxResults,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'newGetExecutionHistory' smart constructor.
data GetExecutionHistory = GetExecutionHistory'
  { -- | Lists events in descending order of their @timeStamp@.
    reverseOrder :: Prelude.Maybe Prelude.Bool,
    -- | You can select whether execution data (input or output of a history
    -- event) is returned. The default is @true@.
    includeExecutionData :: Prelude.Maybe Prelude.Bool,
    -- | If @nextToken@ is returned, there are more results available. The value
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
-- 'reverseOrder', 'getExecutionHistory_reverseOrder' - Lists events in descending order of their @timeStamp@.
--
-- 'includeExecutionData', 'getExecutionHistory_includeExecutionData' - You can select whether execution data (input or output of a history
-- event) is returned. The default is @true@.
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
-- 'executionArn', 'getExecutionHistory_executionArn' - The Amazon Resource Name (ARN) of the execution.
newGetExecutionHistory ::
  -- | 'executionArn'
  Prelude.Text ->
  GetExecutionHistory
newGetExecutionHistory pExecutionArn_ =
  GetExecutionHistory'
    { reverseOrder =
        Prelude.Nothing,
      includeExecutionData = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      executionArn = pExecutionArn_
    }

-- | Lists events in descending order of their @timeStamp@.
getExecutionHistory_reverseOrder :: Lens.Lens' GetExecutionHistory (Prelude.Maybe Prelude.Bool)
getExecutionHistory_reverseOrder = Lens.lens (\GetExecutionHistory' {reverseOrder} -> reverseOrder) (\s@GetExecutionHistory' {} a -> s {reverseOrder = a} :: GetExecutionHistory)

-- | You can select whether execution data (input or output of a history
-- event) is returned. The default is @true@.
getExecutionHistory_includeExecutionData :: Lens.Lens' GetExecutionHistory (Prelude.Maybe Prelude.Bool)
getExecutionHistory_includeExecutionData = Lens.lens (\GetExecutionHistory' {includeExecutionData} -> includeExecutionData) (\s@GetExecutionHistory' {} a -> s {includeExecutionData = a} :: GetExecutionHistory)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetExecutionHistoryResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "events" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable GetExecutionHistory

instance Prelude.NFData GetExecutionHistory

instance Core.ToHeaders GetExecutionHistory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSStepFunctions.GetExecutionHistory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetExecutionHistory where
  toJSON GetExecutionHistory' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("reverseOrder" Core..=) Prelude.<$> reverseOrder,
            ("includeExecutionData" Core..=)
              Prelude.<$> includeExecutionData,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("executionArn" Core..= executionArn)
          ]
      )

instance Core.ToPath GetExecutionHistory where
  toPath = Prelude.const "/"

instance Core.ToQuery GetExecutionHistory where
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

instance Prelude.NFData GetExecutionHistoryResponse
