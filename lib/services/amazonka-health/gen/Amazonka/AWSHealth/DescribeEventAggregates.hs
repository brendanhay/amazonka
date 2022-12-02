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
-- Module      : Amazonka.AWSHealth.DescribeEventAggregates
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.AWSHealth.DescribeEventAggregates
  ( -- * Creating a Request
    DescribeEventAggregates (..),
    newDescribeEventAggregates,

    -- * Request Lenses
    describeEventAggregates_nextToken,
    describeEventAggregates_filter,
    describeEventAggregates_maxResults,
    describeEventAggregates_aggregateField,

    -- * Destructuring the Response
    DescribeEventAggregatesResponse (..),
    newDescribeEventAggregatesResponse,

    -- * Response Lenses
    describeEventAggregatesResponse_eventAggregates,
    describeEventAggregatesResponse_nextToken,
    describeEventAggregatesResponse_httpStatus,
  )
where

import Amazonka.AWSHealth.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEventAggregates' smart constructor.
data DescribeEventAggregates = DescribeEventAggregates'
  { -- | If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next batch of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Values to narrow the results returned.
    filter' :: Prelude.Maybe EventFilter,
    -- | The maximum number of items to return in one batch, between 10 and 100,
    -- inclusive.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'filter'', 'describeEventAggregates_filter' - Values to narrow the results returned.
--
-- 'maxResults', 'describeEventAggregates_maxResults' - The maximum number of items to return in one batch, between 10 and 100,
-- inclusive.
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
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      aggregateField = pAggregateField_
    }

-- | If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
describeEventAggregates_nextToken :: Lens.Lens' DescribeEventAggregates (Prelude.Maybe Prelude.Text)
describeEventAggregates_nextToken = Lens.lens (\DescribeEventAggregates' {nextToken} -> nextToken) (\s@DescribeEventAggregates' {} a -> s {nextToken = a} :: DescribeEventAggregates)

-- | Values to narrow the results returned.
describeEventAggregates_filter :: Lens.Lens' DescribeEventAggregates (Prelude.Maybe EventFilter)
describeEventAggregates_filter = Lens.lens (\DescribeEventAggregates' {filter'} -> filter') (\s@DescribeEventAggregates' {} a -> s {filter' = a} :: DescribeEventAggregates)

-- | The maximum number of items to return in one batch, between 10 and 100,
-- inclusive.
describeEventAggregates_maxResults :: Lens.Lens' DescribeEventAggregates (Prelude.Maybe Prelude.Natural)
describeEventAggregates_maxResults = Lens.lens (\DescribeEventAggregates' {maxResults} -> maxResults) (\s@DescribeEventAggregates' {} a -> s {maxResults = a} :: DescribeEventAggregates)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventAggregatesResponse'
            Prelude.<$> ( x Data..?> "eventAggregates"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEventAggregates where
  hashWithSalt _salt DescribeEventAggregates' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` aggregateField

instance Prelude.NFData DescribeEventAggregates where
  rnf DescribeEventAggregates' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf aggregateField

instance Data.ToHeaders DescribeEventAggregates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHealth_20160804.DescribeEventAggregates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEventAggregates where
  toJSON DescribeEventAggregates' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("filter" Data..=) Prelude.<$> filter',
            ("maxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just
              ("aggregateField" Data..= aggregateField)
          ]
      )

instance Data.ToPath DescribeEventAggregates where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEventAggregates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEventAggregatesResponse' smart constructor.
data DescribeEventAggregatesResponse = DescribeEventAggregatesResponse'
  { -- | The number of events in each category that meet the optional filter
    -- criteria.
    eventAggregates :: Prelude.Maybe [EventAggregate],
    -- | If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next batch of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'eventAggregates', 'describeEventAggregatesResponse_eventAggregates' - The number of events in each category that meet the optional filter
-- criteria.
--
-- 'nextToken', 'describeEventAggregatesResponse_nextToken' - If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'httpStatus', 'describeEventAggregatesResponse_httpStatus' - The response's http status code.
newDescribeEventAggregatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEventAggregatesResponse
newDescribeEventAggregatesResponse pHttpStatus_ =
  DescribeEventAggregatesResponse'
    { eventAggregates =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of events in each category that meet the optional filter
-- criteria.
describeEventAggregatesResponse_eventAggregates :: Lens.Lens' DescribeEventAggregatesResponse (Prelude.Maybe [EventAggregate])
describeEventAggregatesResponse_eventAggregates = Lens.lens (\DescribeEventAggregatesResponse' {eventAggregates} -> eventAggregates) (\s@DescribeEventAggregatesResponse' {} a -> s {eventAggregates = a} :: DescribeEventAggregatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
describeEventAggregatesResponse_nextToken :: Lens.Lens' DescribeEventAggregatesResponse (Prelude.Maybe Prelude.Text)
describeEventAggregatesResponse_nextToken = Lens.lens (\DescribeEventAggregatesResponse' {nextToken} -> nextToken) (\s@DescribeEventAggregatesResponse' {} a -> s {nextToken = a} :: DescribeEventAggregatesResponse)

-- | The response's http status code.
describeEventAggregatesResponse_httpStatus :: Lens.Lens' DescribeEventAggregatesResponse Prelude.Int
describeEventAggregatesResponse_httpStatus = Lens.lens (\DescribeEventAggregatesResponse' {httpStatus} -> httpStatus) (\s@DescribeEventAggregatesResponse' {} a -> s {httpStatus = a} :: DescribeEventAggregatesResponse)

instance
  Prelude.NFData
    DescribeEventAggregatesResponse
  where
  rnf DescribeEventAggregatesResponse' {..} =
    Prelude.rnf eventAggregates
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
