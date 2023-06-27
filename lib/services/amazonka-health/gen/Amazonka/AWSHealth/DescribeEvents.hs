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
-- Module      : Amazonka.AWSHealth.DescribeEvents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about events that meet the specified filter
-- criteria. Events are returned in a summary form and do not include the
-- detailed description, any additional metadata that depends on the event
-- type, or any affected resources. To retrieve that information, use the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetails.html DescribeEventDetails>
-- and
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedEntities.html DescribeAffectedEntities>
-- operations.
--
-- If no filter criteria are specified, all events are returned. Results
-- are sorted by @lastModifiedTime@, starting with the most recent event.
--
-- -   When you call the @DescribeEvents@ operation and specify an entity
--     for the @entityValues@ parameter, Health might return public events
--     that aren\'t specific to that resource. For example, if you call
--     @DescribeEvents@ and specify an ID for an Amazon Elastic Compute
--     Cloud (Amazon EC2) instance, Health might return events that aren\'t
--     specific to that resource or service. To get events that are
--     specific to a service, use the @services@ parameter in the @filter@
--     object. For more information, see
--     <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html Event>.
--
-- -   This API operation uses pagination. Specify the @nextToken@
--     parameter in the next request to return more results.
--
-- This operation returns paginated results.
module Amazonka.AWSHealth.DescribeEvents
  ( -- * Creating a Request
    DescribeEvents (..),
    newDescribeEvents,

    -- * Request Lenses
    describeEvents_filter,
    describeEvents_locale,
    describeEvents_maxResults,
    describeEvents_nextToken,

    -- * Destructuring the Response
    DescribeEventsResponse (..),
    newDescribeEventsResponse,

    -- * Response Lenses
    describeEventsResponse_events,
    describeEventsResponse_nextToken,
    describeEventsResponse_httpStatus,
  )
where

import Amazonka.AWSHealth.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { -- | Values to narrow the results returned.
    filter' :: Prelude.Maybe EventFilter,
    -- | The locale (language) to return information in. English (en) is the
    -- default and the only supported value at this time.
    locale :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return in one batch, between 10 and 100,
    -- inclusive.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next batch of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'describeEvents_filter' - Values to narrow the results returned.
--
-- 'locale', 'describeEvents_locale' - The locale (language) to return information in. English (en) is the
-- default and the only supported value at this time.
--
-- 'maxResults', 'describeEvents_maxResults' - The maximum number of items to return in one batch, between 10 and 100,
-- inclusive.
--
-- 'nextToken', 'describeEvents_nextToken' - If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
newDescribeEvents ::
  DescribeEvents
newDescribeEvents =
  DescribeEvents'
    { filter' = Prelude.Nothing,
      locale = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Values to narrow the results returned.
describeEvents_filter :: Lens.Lens' DescribeEvents (Prelude.Maybe EventFilter)
describeEvents_filter = Lens.lens (\DescribeEvents' {filter'} -> filter') (\s@DescribeEvents' {} a -> s {filter' = a} :: DescribeEvents)

-- | The locale (language) to return information in. English (en) is the
-- default and the only supported value at this time.
describeEvents_locale :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.Text)
describeEvents_locale = Lens.lens (\DescribeEvents' {locale} -> locale) (\s@DescribeEvents' {} a -> s {locale = a} :: DescribeEvents)

-- | The maximum number of items to return in one batch, between 10 and 100,
-- inclusive.
describeEvents_maxResults :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.Natural)
describeEvents_maxResults = Lens.lens (\DescribeEvents' {maxResults} -> maxResults) (\s@DescribeEvents' {} a -> s {maxResults = a} :: DescribeEvents)

-- | If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
describeEvents_nextToken :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.Text)
describeEvents_nextToken = Lens.lens (\DescribeEvents' {nextToken} -> nextToken) (\s@DescribeEvents' {} a -> s {nextToken = a} :: DescribeEvents)

instance Core.AWSPager DescribeEvents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEventsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEventsResponse_events
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeEvents_nextToken
          Lens..~ rs
          Lens.^? describeEventsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeEvents where
  type
    AWSResponse DescribeEvents =
      DescribeEventsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventsResponse'
            Prelude.<$> (x Data..?> "events" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEvents where
  hashWithSalt _salt DescribeEvents' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeEvents where
  rnf DescribeEvents' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf locale
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHealth_20160804.DescribeEvents" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEvents where
  toJSON DescribeEvents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filter" Data..=) Prelude.<$> filter',
            ("locale" Data..=) Prelude.<$> locale,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeEvents where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEvents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { -- | The events that match the specified filter criteria.
    events :: Prelude.Maybe [Event],
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
-- Create a value of 'DescribeEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'events', 'describeEventsResponse_events' - The events that match the specified filter criteria.
--
-- 'nextToken', 'describeEventsResponse_nextToken' - If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'httpStatus', 'describeEventsResponse_httpStatus' - The response's http status code.
newDescribeEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEventsResponse
newDescribeEventsResponse pHttpStatus_ =
  DescribeEventsResponse'
    { events = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The events that match the specified filter criteria.
describeEventsResponse_events :: Lens.Lens' DescribeEventsResponse (Prelude.Maybe [Event])
describeEventsResponse_events = Lens.lens (\DescribeEventsResponse' {events} -> events) (\s@DescribeEventsResponse' {} a -> s {events = a} :: DescribeEventsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
describeEventsResponse_nextToken :: Lens.Lens' DescribeEventsResponse (Prelude.Maybe Prelude.Text)
describeEventsResponse_nextToken = Lens.lens (\DescribeEventsResponse' {nextToken} -> nextToken) (\s@DescribeEventsResponse' {} a -> s {nextToken = a} :: DescribeEventsResponse)

-- | The response's http status code.
describeEventsResponse_httpStatus :: Lens.Lens' DescribeEventsResponse Prelude.Int
describeEventsResponse_httpStatus = Lens.lens (\DescribeEventsResponse' {httpStatus} -> httpStatus) (\s@DescribeEventsResponse' {} a -> s {httpStatus = a} :: DescribeEventsResponse)

instance Prelude.NFData DescribeEventsResponse where
  rnf DescribeEventsResponse' {..} =
    Prelude.rnf events
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
