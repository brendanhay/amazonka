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
-- Module      : Network.AWS.SSM.DescribeSessions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of all active sessions (both connected and
-- disconnected) or terminated sessions from the past 30 days.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeSessions
  ( -- * Creating a Request
    DescribeSessions (..),
    newDescribeSessions,

    -- * Request Lenses
    describeSessions_nextToken,
    describeSessions_maxResults,
    describeSessions_filters,
    describeSessions_state,

    -- * Destructuring the Response
    DescribeSessionsResponse (..),
    newDescribeSessionsResponse,

    -- * Response Lenses
    describeSessionsResponse_nextToken,
    describeSessionsResponse_sessions,
    describeSessionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeSessions' smart constructor.
data DescribeSessions = DescribeSessions'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | One or more filters to limit the type of sessions returned by the
    -- request.
    filters :: Core.Maybe (Core.NonEmpty SessionFilter),
    -- | The session status to retrieve a list of sessions for. For example,
    -- \"Active\".
    state :: SessionState
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSessions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSessions_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'describeSessions_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'filters', 'describeSessions_filters' - One or more filters to limit the type of sessions returned by the
-- request.
--
-- 'state', 'describeSessions_state' - The session status to retrieve a list of sessions for. For example,
-- \"Active\".
newDescribeSessions ::
  -- | 'state'
  SessionState ->
  DescribeSessions
newDescribeSessions pState_ =
  DescribeSessions'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing,
      state = pState_
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeSessions_nextToken :: Lens.Lens' DescribeSessions (Core.Maybe Core.Text)
describeSessions_nextToken = Lens.lens (\DescribeSessions' {nextToken} -> nextToken) (\s@DescribeSessions' {} a -> s {nextToken = a} :: DescribeSessions)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeSessions_maxResults :: Lens.Lens' DescribeSessions (Core.Maybe Core.Natural)
describeSessions_maxResults = Lens.lens (\DescribeSessions' {maxResults} -> maxResults) (\s@DescribeSessions' {} a -> s {maxResults = a} :: DescribeSessions)

-- | One or more filters to limit the type of sessions returned by the
-- request.
describeSessions_filters :: Lens.Lens' DescribeSessions (Core.Maybe (Core.NonEmpty SessionFilter))
describeSessions_filters = Lens.lens (\DescribeSessions' {filters} -> filters) (\s@DescribeSessions' {} a -> s {filters = a} :: DescribeSessions) Core.. Lens.mapping Lens._Coerce

-- | The session status to retrieve a list of sessions for. For example,
-- \"Active\".
describeSessions_state :: Lens.Lens' DescribeSessions SessionState
describeSessions_state = Lens.lens (\DescribeSessions' {state} -> state) (\s@DescribeSessions' {} a -> s {state = a} :: DescribeSessions)

instance Core.AWSPager DescribeSessions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSessionsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSessionsResponse_sessions Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeSessions_nextToken
          Lens..~ rs
          Lens.^? describeSessionsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeSessions where
  type
    AWSResponse DescribeSessions =
      DescribeSessionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSessionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Sessions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeSessions

instance Core.NFData DescribeSessions

instance Core.ToHeaders DescribeSessions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.DescribeSessions" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeSessions where
  toJSON DescribeSessions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters,
            Core.Just ("State" Core..= state)
          ]
      )

instance Core.ToPath DescribeSessions where
  toPath = Core.const "/"

instance Core.ToQuery DescribeSessions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeSessionsResponse' smart constructor.
data DescribeSessionsResponse = DescribeSessionsResponse'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | A list of sessions meeting the request parameters.
    sessions :: Core.Maybe [Session],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSessionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSessionsResponse_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'sessions', 'describeSessionsResponse_sessions' - A list of sessions meeting the request parameters.
--
-- 'httpStatus', 'describeSessionsResponse_httpStatus' - The response's http status code.
newDescribeSessionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeSessionsResponse
newDescribeSessionsResponse pHttpStatus_ =
  DescribeSessionsResponse'
    { nextToken = Core.Nothing,
      sessions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeSessionsResponse_nextToken :: Lens.Lens' DescribeSessionsResponse (Core.Maybe Core.Text)
describeSessionsResponse_nextToken = Lens.lens (\DescribeSessionsResponse' {nextToken} -> nextToken) (\s@DescribeSessionsResponse' {} a -> s {nextToken = a} :: DescribeSessionsResponse)

-- | A list of sessions meeting the request parameters.
describeSessionsResponse_sessions :: Lens.Lens' DescribeSessionsResponse (Core.Maybe [Session])
describeSessionsResponse_sessions = Lens.lens (\DescribeSessionsResponse' {sessions} -> sessions) (\s@DescribeSessionsResponse' {} a -> s {sessions = a} :: DescribeSessionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeSessionsResponse_httpStatus :: Lens.Lens' DescribeSessionsResponse Core.Int
describeSessionsResponse_httpStatus = Lens.lens (\DescribeSessionsResponse' {httpStatus} -> httpStatus) (\s@DescribeSessionsResponse' {} a -> s {httpStatus = a} :: DescribeSessionsResponse)

instance Core.NFData DescribeSessionsResponse
