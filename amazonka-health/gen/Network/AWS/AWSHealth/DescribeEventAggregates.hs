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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEventAggregates' smart constructor.
data DescribeEventAggregates = DescribeEventAggregates'
  { -- | If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next batch of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return in one batch, between 10 and 100,
    -- inclusive.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Values to narrow the results returned.
    filter' :: Prelude.Maybe EventFilter,
    -- | The only currently supported value is @eventTypeCategory@.
    aggregateField :: EventAggregateField
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filter' = Prelude.Nothing,
      aggregateField = pAggregateField_
    }

-- | If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
describeEventAggregates_nextToken :: Lens.Lens' DescribeEventAggregates (Prelude.Maybe Prelude.Text)
describeEventAggregates_nextToken = Lens.lens (\DescribeEventAggregates' {nextToken} -> nextToken) (\s@DescribeEventAggregates' {} a -> s {nextToken = a} :: DescribeEventAggregates)

-- | The maximum number of items to return in one batch, between 10 and 100,
-- inclusive.
describeEventAggregates_maxResults :: Lens.Lens' DescribeEventAggregates (Prelude.Maybe Prelude.Natural)
describeEventAggregates_maxResults = Lens.lens (\DescribeEventAggregates' {maxResults} -> maxResults) (\s@DescribeEventAggregates' {} a -> s {maxResults = a} :: DescribeEventAggregates)

-- | Values to narrow the results returned.
describeEventAggregates_filter :: Lens.Lens' DescribeEventAggregates (Prelude.Maybe EventFilter)
describeEventAggregates_filter = Lens.lens (\DescribeEventAggregates' {filter'} -> filter') (\s@DescribeEventAggregates' {} a -> s {filter' = a} :: DescribeEventAggregates)

-- | The only currently supported value is @eventTypeCategory@.
describeEventAggregates_aggregateField :: Lens.Lens' DescribeEventAggregates EventAggregateField
describeEventAggregates_aggregateField = Lens.lens (\DescribeEventAggregates' {aggregateField} -> aggregateField) (\s@DescribeEventAggregates' {} a -> s {aggregateField = a} :: DescribeEventAggregates)

instance Core.AWSPager DescribeEventAggregates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEventAggregatesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEventAggregatesResponse_eventAggregates
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeEventAggregates_nextToken
          Lens..~ rs
          Lens.^? describeEventAggregatesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeEventAggregates where
  type
    AWSResponse DescribeEventAggregates =
      DescribeEventAggregatesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventAggregatesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "eventAggregates"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEventAggregates

instance Prelude.NFData DescribeEventAggregates

instance Core.ToHeaders DescribeEventAggregates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHealth_20160804.DescribeEventAggregates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeEventAggregates where
  toJSON DescribeEventAggregates' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("filter" Core..=) Prelude.<$> filter',
            Prelude.Just
              ("aggregateField" Core..= aggregateField)
          ]
      )

instance Core.ToPath DescribeEventAggregates where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeEventAggregates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEventAggregatesResponse' smart constructor.
data DescribeEventAggregatesResponse = DescribeEventAggregatesResponse'
  { -- | If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next batch of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of events in each category that meet the optional filter
    -- criteria.
    eventAggregates :: Prelude.Maybe [EventAggregate],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeEventAggregatesResponse
newDescribeEventAggregatesResponse pHttpStatus_ =
  DescribeEventAggregatesResponse'
    { nextToken =
        Prelude.Nothing,
      eventAggregates = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
describeEventAggregatesResponse_nextToken :: Lens.Lens' DescribeEventAggregatesResponse (Prelude.Maybe Prelude.Text)
describeEventAggregatesResponse_nextToken = Lens.lens (\DescribeEventAggregatesResponse' {nextToken} -> nextToken) (\s@DescribeEventAggregatesResponse' {} a -> s {nextToken = a} :: DescribeEventAggregatesResponse)

-- | The number of events in each category that meet the optional filter
-- criteria.
describeEventAggregatesResponse_eventAggregates :: Lens.Lens' DescribeEventAggregatesResponse (Prelude.Maybe [EventAggregate])
describeEventAggregatesResponse_eventAggregates = Lens.lens (\DescribeEventAggregatesResponse' {eventAggregates} -> eventAggregates) (\s@DescribeEventAggregatesResponse' {} a -> s {eventAggregates = a} :: DescribeEventAggregatesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeEventAggregatesResponse_httpStatus :: Lens.Lens' DescribeEventAggregatesResponse Prelude.Int
describeEventAggregatesResponse_httpStatus = Lens.lens (\DescribeEventAggregatesResponse' {httpStatus} -> httpStatus) (\s@DescribeEventAggregatesResponse' {} a -> s {httpStatus = a} :: DescribeEventAggregatesResponse)

instance
  Prelude.NFData
    DescribeEventAggregatesResponse
