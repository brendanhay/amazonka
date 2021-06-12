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
-- Module      : Network.AWS.AWSHealth.DescribeEventAggregates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of events of each event type (issue, scheduled
-- change, and account notification). If no filter is specified, the counts
-- of all events in each category are returned.
--
-- This API operation uses pagination. Specify the @nextToken@ parameter in
-- the next request to return more results.
--
-- This operation returns paginated results.
module Network.AWS.AWSHealth.DescribeEventAggregates
  ( -- * Creating a Request
    DescribeEventAggregates (..),
    newDescribeEventAggregates,

    -- * Request Lenses
    describeEventAggregates_nextToken,
    describeEventAggregates_maxResults,
    describeEventAggregates_filter,
    describeEventAggregates_aggregateField,

    -- * Destructuring the Response
    DescribeEventAggregatesResponse (..),
    newDescribeEventAggregatesResponse,

    -- * Response Lenses
    describeEventAggregatesResponse_nextToken,
    describeEventAggregatesResponse_eventAggregates,
    describeEventAggregatesResponse_httpStatus,
  )
where

import Network.AWS.AWSHealth.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEventAggregates' smart constructor.
data DescribeEventAggregates = DescribeEventAggregates'
  { -- | If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next batch of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return in one batch, between 10 and 100,
    -- inclusive.
    maxResults :: Core.Maybe Core.Natural,
    -- | Values to narrow the results returned.
    filter' :: Core.Maybe EventFilter,
    -- | The only currently supported value is @eventTypeCategory@.
    aggregateField :: EventAggregateField
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEventAggregates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEventAggregates_nextToken' - If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'maxResults', 'describeEventAggregates_maxResults' - The maximum number of items to return in one batch, between 10 and 100,
-- inclusive.
--
-- 'filter'', 'describeEventAggregates_filter' - Values to narrow the results returned.
--
-- 'aggregateField', 'describeEventAggregates_aggregateField' - The only currently supported value is @eventTypeCategory@.
newDescribeEventAggregates ::
  -- | 'aggregateField'
  EventAggregateField ->
  DescribeEventAggregates
newDescribeEventAggregates pAggregateField_ =
  DescribeEventAggregates'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filter' = Core.Nothing,
      aggregateField = pAggregateField_
    }

-- | If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
describeEventAggregates_nextToken :: Lens.Lens' DescribeEventAggregates (Core.Maybe Core.Text)
describeEventAggregates_nextToken = Lens.lens (\DescribeEventAggregates' {nextToken} -> nextToken) (\s@DescribeEventAggregates' {} a -> s {nextToken = a} :: DescribeEventAggregates)

-- | The maximum number of items to return in one batch, between 10 and 100,
-- inclusive.
describeEventAggregates_maxResults :: Lens.Lens' DescribeEventAggregates (Core.Maybe Core.Natural)
describeEventAggregates_maxResults = Lens.lens (\DescribeEventAggregates' {maxResults} -> maxResults) (\s@DescribeEventAggregates' {} a -> s {maxResults = a} :: DescribeEventAggregates)

-- | Values to narrow the results returned.
describeEventAggregates_filter :: Lens.Lens' DescribeEventAggregates (Core.Maybe EventFilter)
describeEventAggregates_filter = Lens.lens (\DescribeEventAggregates' {filter'} -> filter') (\s@DescribeEventAggregates' {} a -> s {filter' = a} :: DescribeEventAggregates)

-- | The only currently supported value is @eventTypeCategory@.
describeEventAggregates_aggregateField :: Lens.Lens' DescribeEventAggregates EventAggregateField
describeEventAggregates_aggregateField = Lens.lens (\DescribeEventAggregates' {aggregateField} -> aggregateField) (\s@DescribeEventAggregates' {} a -> s {aggregateField = a} :: DescribeEventAggregates)

instance Core.AWSPager DescribeEventAggregates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEventAggregatesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEventAggregatesResponse_eventAggregates
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeEventAggregates_nextToken
          Lens..~ rs
          Lens.^? describeEventAggregatesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeEventAggregates where
  type
    AWSResponse DescribeEventAggregates =
      DescribeEventAggregatesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventAggregatesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "eventAggregates" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEventAggregates

instance Core.NFData DescribeEventAggregates

instance Core.ToHeaders DescribeEventAggregates where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHealth_20160804.DescribeEventAggregates" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeEventAggregates where
  toJSON DescribeEventAggregates' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("filter" Core..=) Core.<$> filter',
            Core.Just ("aggregateField" Core..= aggregateField)
          ]
      )

instance Core.ToPath DescribeEventAggregates where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEventAggregates where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeEventAggregatesResponse' smart constructor.
data DescribeEventAggregatesResponse = DescribeEventAggregatesResponse'
  { -- | If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next batch of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Core.Maybe Core.Text,
    -- | The number of events in each category that meet the optional filter
    -- criteria.
    eventAggregates :: Core.Maybe [EventAggregate],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEventAggregatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEventAggregatesResponse_nextToken' - If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'eventAggregates', 'describeEventAggregatesResponse_eventAggregates' - The number of events in each category that meet the optional filter
-- criteria.
--
-- 'httpStatus', 'describeEventAggregatesResponse_httpStatus' - The response's http status code.
newDescribeEventAggregatesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeEventAggregatesResponse
newDescribeEventAggregatesResponse pHttpStatus_ =
  DescribeEventAggregatesResponse'
    { nextToken =
        Core.Nothing,
      eventAggregates = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
describeEventAggregatesResponse_nextToken :: Lens.Lens' DescribeEventAggregatesResponse (Core.Maybe Core.Text)
describeEventAggregatesResponse_nextToken = Lens.lens (\DescribeEventAggregatesResponse' {nextToken} -> nextToken) (\s@DescribeEventAggregatesResponse' {} a -> s {nextToken = a} :: DescribeEventAggregatesResponse)

-- | The number of events in each category that meet the optional filter
-- criteria.
describeEventAggregatesResponse_eventAggregates :: Lens.Lens' DescribeEventAggregatesResponse (Core.Maybe [EventAggregate])
describeEventAggregatesResponse_eventAggregates = Lens.lens (\DescribeEventAggregatesResponse' {eventAggregates} -> eventAggregates) (\s@DescribeEventAggregatesResponse' {} a -> s {eventAggregates = a} :: DescribeEventAggregatesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeEventAggregatesResponse_httpStatus :: Lens.Lens' DescribeEventAggregatesResponse Core.Int
describeEventAggregatesResponse_httpStatus = Lens.lens (\DescribeEventAggregatesResponse' {httpStatus} -> httpStatus) (\s@DescribeEventAggregatesResponse' {} a -> s {httpStatus = a} :: DescribeEventAggregatesResponse)

instance Core.NFData DescribeEventAggregatesResponse
