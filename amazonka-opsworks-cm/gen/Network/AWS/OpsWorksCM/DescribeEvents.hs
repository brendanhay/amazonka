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
-- Module      : Network.AWS.OpsWorksCM.DescribeEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes events for a specified server. Results are ordered by time,
-- with newest events first.
--
-- This operation is synchronous.
--
-- A @ResourceNotFoundException@ is thrown when the server does not exist.
-- A @ValidationException@ is raised when parameters of the request are not
-- valid.
--
-- This operation returns paginated results.
module Network.AWS.OpsWorksCM.DescribeEvents
  ( -- * Creating a Request
    DescribeEvents (..),
    newDescribeEvents,

    -- * Request Lenses
    describeEvents_nextToken,
    describeEvents_maxResults,
    describeEvents_serverName,

    -- * Destructuring the Response
    DescribeEventsResponse (..),
    newDescribeEventsResponse,

    -- * Response Lenses
    describeEventsResponse_nextToken,
    describeEventsResponse_serverEvents,
    describeEventsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { -- | NextToken is a string that is returned in some command responses. It
    -- indicates that not all entries have been returned, and that you must run
    -- at least one more request to get remaining items. To get remaining
    -- results, call @DescribeEvents@ again, and assign the token from the
    -- previous results as the value of the @nextToken@ parameter. If there are
    -- no more results, the response object\'s @nextToken@ parameter value is
    -- @null@. Setting a @nextToken@ value that was not returned in your
    -- previous results causes an @InvalidNextTokenException@ to occur.
    nextToken :: Core.Maybe Core.Text,
    -- | To receive a paginated response, use this parameter to specify the
    -- maximum number of results to be returned with a single call. If the
    -- number of available results exceeds this maximum, the response includes
    -- a @NextToken@ value that you can assign to the @NextToken@ request
    -- parameter to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The name of the server for which you want to view events.
    serverName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEvents_nextToken' - NextToken is a string that is returned in some command responses. It
-- indicates that not all entries have been returned, and that you must run
-- at least one more request to get remaining items. To get remaining
-- results, call @DescribeEvents@ again, and assign the token from the
-- previous results as the value of the @nextToken@ parameter. If there are
-- no more results, the response object\'s @nextToken@ parameter value is
-- @null@. Setting a @nextToken@ value that was not returned in your
-- previous results causes an @InvalidNextTokenException@ to occur.
--
-- 'maxResults', 'describeEvents_maxResults' - To receive a paginated response, use this parameter to specify the
-- maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
--
-- 'serverName', 'describeEvents_serverName' - The name of the server for which you want to view events.
newDescribeEvents ::
  -- | 'serverName'
  Core.Text ->
  DescribeEvents
newDescribeEvents pServerName_ =
  DescribeEvents'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      serverName = pServerName_
    }

-- | NextToken is a string that is returned in some command responses. It
-- indicates that not all entries have been returned, and that you must run
-- at least one more request to get remaining items. To get remaining
-- results, call @DescribeEvents@ again, and assign the token from the
-- previous results as the value of the @nextToken@ parameter. If there are
-- no more results, the response object\'s @nextToken@ parameter value is
-- @null@. Setting a @nextToken@ value that was not returned in your
-- previous results causes an @InvalidNextTokenException@ to occur.
describeEvents_nextToken :: Lens.Lens' DescribeEvents (Core.Maybe Core.Text)
describeEvents_nextToken = Lens.lens (\DescribeEvents' {nextToken} -> nextToken) (\s@DescribeEvents' {} a -> s {nextToken = a} :: DescribeEvents)

-- | To receive a paginated response, use this parameter to specify the
-- maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
describeEvents_maxResults :: Lens.Lens' DescribeEvents (Core.Maybe Core.Natural)
describeEvents_maxResults = Lens.lens (\DescribeEvents' {maxResults} -> maxResults) (\s@DescribeEvents' {} a -> s {maxResults = a} :: DescribeEvents)

-- | The name of the server for which you want to view events.
describeEvents_serverName :: Lens.Lens' DescribeEvents Core.Text
describeEvents_serverName = Lens.lens (\DescribeEvents' {serverName} -> serverName) (\s@DescribeEvents' {} a -> s {serverName = a} :: DescribeEvents)

instance Core.AWSPager DescribeEvents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEventsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEventsResponse_serverEvents
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeEvents_nextToken
          Lens..~ rs
          Lens.^? describeEventsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeEvents where
  type
    AWSResponse DescribeEvents =
      DescribeEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "ServerEvents" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEvents

instance Core.NFData DescribeEvents

instance Core.ToHeaders DescribeEvents where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorksCM_V2016_11_01.DescribeEvents" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeEvents where
  toJSON DescribeEvents' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("ServerName" Core..= serverName)
          ]
      )

instance Core.ToPath DescribeEvents where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEvents where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { -- | NextToken is a string that is returned in some command responses. It
    -- indicates that not all entries have been returned, and that you must run
    -- at least one more request to get remaining items. To get remaining
    -- results, call @DescribeEvents@ again, and assign the token from the
    -- previous results as the value of the @nextToken@ parameter. If there are
    -- no more results, the response object\'s @nextToken@ parameter value is
    -- @null@. Setting a @nextToken@ value that was not returned in your
    -- previous results causes an @InvalidNextTokenException@ to occur.
    nextToken :: Core.Maybe Core.Text,
    -- | Contains the response to a @DescribeEvents@ request.
    serverEvents :: Core.Maybe [ServerEvent],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEventsResponse_nextToken' - NextToken is a string that is returned in some command responses. It
-- indicates that not all entries have been returned, and that you must run
-- at least one more request to get remaining items. To get remaining
-- results, call @DescribeEvents@ again, and assign the token from the
-- previous results as the value of the @nextToken@ parameter. If there are
-- no more results, the response object\'s @nextToken@ parameter value is
-- @null@. Setting a @nextToken@ value that was not returned in your
-- previous results causes an @InvalidNextTokenException@ to occur.
--
-- 'serverEvents', 'describeEventsResponse_serverEvents' - Contains the response to a @DescribeEvents@ request.
--
-- 'httpStatus', 'describeEventsResponse_httpStatus' - The response's http status code.
newDescribeEventsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeEventsResponse
newDescribeEventsResponse pHttpStatus_ =
  DescribeEventsResponse'
    { nextToken = Core.Nothing,
      serverEvents = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | NextToken is a string that is returned in some command responses. It
-- indicates that not all entries have been returned, and that you must run
-- at least one more request to get remaining items. To get remaining
-- results, call @DescribeEvents@ again, and assign the token from the
-- previous results as the value of the @nextToken@ parameter. If there are
-- no more results, the response object\'s @nextToken@ parameter value is
-- @null@. Setting a @nextToken@ value that was not returned in your
-- previous results causes an @InvalidNextTokenException@ to occur.
describeEventsResponse_nextToken :: Lens.Lens' DescribeEventsResponse (Core.Maybe Core.Text)
describeEventsResponse_nextToken = Lens.lens (\DescribeEventsResponse' {nextToken} -> nextToken) (\s@DescribeEventsResponse' {} a -> s {nextToken = a} :: DescribeEventsResponse)

-- | Contains the response to a @DescribeEvents@ request.
describeEventsResponse_serverEvents :: Lens.Lens' DescribeEventsResponse (Core.Maybe [ServerEvent])
describeEventsResponse_serverEvents = Lens.lens (\DescribeEventsResponse' {serverEvents} -> serverEvents) (\s@DescribeEventsResponse' {} a -> s {serverEvents = a} :: DescribeEventsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeEventsResponse_httpStatus :: Lens.Lens' DescribeEventsResponse Core.Int
describeEventsResponse_httpStatus = Lens.lens (\DescribeEventsResponse' {httpStatus} -> httpStatus) (\s@DescribeEventsResponse' {} a -> s {httpStatus = a} :: DescribeEventsResponse)

instance Core.NFData DescribeEventsResponse
