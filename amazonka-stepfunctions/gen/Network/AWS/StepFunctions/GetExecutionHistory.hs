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
    getExecutionHistory_nextToken,
    getExecutionHistory_maxResults,
    getExecutionHistory_reverseOrder,
    getExecutionHistory_includeExecutionData,
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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'newGetExecutionHistory' smart constructor.
data GetExecutionHistory = GetExecutionHistory'
  { -- | If @nextToken@ is returned, there are more results available. The value
    -- of @nextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours. Using
    -- an expired pagination token will return an /HTTP 400 InvalidToken/
    -- error.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results that are returned per call. You can use
    -- @nextToken@ to obtain further pages of results. The default is 100 and
    -- the maximum allowed page size is 1000. A value of 0 uses the default.
    --
    -- This is only an upper limit. The actual number of results returned per
    -- call might be fewer than the specified maximum.
    maxResults :: Core.Maybe Core.Natural,
    -- | Lists events in descending order of their @timeStamp@.
    reverseOrder :: Core.Maybe Core.Bool,
    -- | You can select whether execution data (input or output of a history
    -- event) is returned. The default is @true@.
    includeExecutionData :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the execution.
    executionArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'reverseOrder', 'getExecutionHistory_reverseOrder' - Lists events in descending order of their @timeStamp@.
--
-- 'includeExecutionData', 'getExecutionHistory_includeExecutionData' - You can select whether execution data (input or output of a history
-- event) is returned. The default is @true@.
--
-- 'executionArn', 'getExecutionHistory_executionArn' - The Amazon Resource Name (ARN) of the execution.
newGetExecutionHistory ::
  -- | 'executionArn'
  Core.Text ->
  GetExecutionHistory
newGetExecutionHistory pExecutionArn_ =
  GetExecutionHistory'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      reverseOrder = Core.Nothing,
      includeExecutionData = Core.Nothing,
      executionArn = pExecutionArn_
    }

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
getExecutionHistory_nextToken :: Lens.Lens' GetExecutionHistory (Core.Maybe Core.Text)
getExecutionHistory_nextToken = Lens.lens (\GetExecutionHistory' {nextToken} -> nextToken) (\s@GetExecutionHistory' {} a -> s {nextToken = a} :: GetExecutionHistory)

-- | The maximum number of results that are returned per call. You can use
-- @nextToken@ to obtain further pages of results. The default is 100 and
-- the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per
-- call might be fewer than the specified maximum.
getExecutionHistory_maxResults :: Lens.Lens' GetExecutionHistory (Core.Maybe Core.Natural)
getExecutionHistory_maxResults = Lens.lens (\GetExecutionHistory' {maxResults} -> maxResults) (\s@GetExecutionHistory' {} a -> s {maxResults = a} :: GetExecutionHistory)

-- | Lists events in descending order of their @timeStamp@.
getExecutionHistory_reverseOrder :: Lens.Lens' GetExecutionHistory (Core.Maybe Core.Bool)
getExecutionHistory_reverseOrder = Lens.lens (\GetExecutionHistory' {reverseOrder} -> reverseOrder) (\s@GetExecutionHistory' {} a -> s {reverseOrder = a} :: GetExecutionHistory)

-- | You can select whether execution data (input or output of a history
-- event) is returned. The default is @true@.
getExecutionHistory_includeExecutionData :: Lens.Lens' GetExecutionHistory (Core.Maybe Core.Bool)
getExecutionHistory_includeExecutionData = Lens.lens (\GetExecutionHistory' {includeExecutionData} -> includeExecutionData) (\s@GetExecutionHistory' {} a -> s {includeExecutionData = a} :: GetExecutionHistory)

-- | The Amazon Resource Name (ARN) of the execution.
getExecutionHistory_executionArn :: Lens.Lens' GetExecutionHistory Core.Text
getExecutionHistory_executionArn = Lens.lens (\GetExecutionHistory' {executionArn} -> executionArn) (\s@GetExecutionHistory' {} a -> s {executionArn = a} :: GetExecutionHistory)

instance Core.AWSPager GetExecutionHistory where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getExecutionHistoryResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^. getExecutionHistoryResponse_events) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getExecutionHistory_nextToken
          Lens..~ rs
          Lens.^? getExecutionHistoryResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest GetExecutionHistory where
  type
    AWSResponse GetExecutionHistory =
      GetExecutionHistoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetExecutionHistoryResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "events" Core..!@ Core.mempty)
      )

instance Core.Hashable GetExecutionHistory

instance Core.NFData GetExecutionHistory

instance Core.ToHeaders GetExecutionHistory where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSStepFunctions.GetExecutionHistory" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetExecutionHistory where
  toJSON GetExecutionHistory' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("reverseOrder" Core..=) Core.<$> reverseOrder,
            ("includeExecutionData" Core..=)
              Core.<$> includeExecutionData,
            Core.Just ("executionArn" Core..= executionArn)
          ]
      )

instance Core.ToPath GetExecutionHistory where
  toPath = Core.const "/"

instance Core.ToQuery GetExecutionHistory where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetExecutionHistoryResponse' smart constructor.
data GetExecutionHistoryResponse = GetExecutionHistoryResponse'
  { -- | If @nextToken@ is returned, there are more results available. The value
    -- of @nextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours. Using
    -- an expired pagination token will return an /HTTP 400 InvalidToken/
    -- error.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The list of events that occurred in the execution.
    events :: [HistoryEvent]
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Int ->
  GetExecutionHistoryResponse
newGetExecutionHistoryResponse pHttpStatus_ =
  GetExecutionHistoryResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      events = Core.mempty
    }

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
getExecutionHistoryResponse_nextToken :: Lens.Lens' GetExecutionHistoryResponse (Core.Maybe Core.Text)
getExecutionHistoryResponse_nextToken = Lens.lens (\GetExecutionHistoryResponse' {nextToken} -> nextToken) (\s@GetExecutionHistoryResponse' {} a -> s {nextToken = a} :: GetExecutionHistoryResponse)

-- | The response's http status code.
getExecutionHistoryResponse_httpStatus :: Lens.Lens' GetExecutionHistoryResponse Core.Int
getExecutionHistoryResponse_httpStatus = Lens.lens (\GetExecutionHistoryResponse' {httpStatus} -> httpStatus) (\s@GetExecutionHistoryResponse' {} a -> s {httpStatus = a} :: GetExecutionHistoryResponse)

-- | The list of events that occurred in the execution.
getExecutionHistoryResponse_events :: Lens.Lens' GetExecutionHistoryResponse [HistoryEvent]
getExecutionHistoryResponse_events = Lens.lens (\GetExecutionHistoryResponse' {events} -> events) (\s@GetExecutionHistoryResponse' {} a -> s {events = a} :: GetExecutionHistoryResponse) Core.. Lens._Coerce

instance Core.NFData GetExecutionHistoryResponse
