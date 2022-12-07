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
-- Module      : Amazonka.AWSHealth.DescribeEventsForOrganization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about events across your organization in
-- Organizations. You can use the@filters@ parameter to specify the events
-- that you want to return. Events are returned in a summary form and
-- don\'t include the affected accounts, detailed description, any
-- additional metadata that depends on the event type, or any affected
-- resources. To retrieve that information, use the following operations:
--
-- -   <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedAccountsForOrganization.html DescribeAffectedAccountsForOrganization>
--
-- -   <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization>
--
-- -   <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedEntitiesForOrganization.html DescribeAffectedEntitiesForOrganization>
--
-- If you don\'t specify a @filter@, the @DescribeEventsForOrganizations@
-- returns all events across your organization. Results are sorted by
-- @lastModifiedTime@, starting with the most recent event.
--
-- For more information about the different types of Health events, see
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html Event>.
--
-- Before you can call this operation, you must first enable Health to work
-- with Organizations. To do this, call the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_EnableHealthServiceAccessForOrganization.html EnableHealthServiceAccessForOrganization>
-- operation from your organization\'s management account.
--
-- This API operation uses pagination. Specify the @nextToken@ parameter in
-- the next request to return more results.
--
-- This operation returns paginated results.
module Amazonka.AWSHealth.DescribeEventsForOrganization
  ( -- * Creating a Request
    DescribeEventsForOrganization (..),
    newDescribeEventsForOrganization,

    -- * Request Lenses
    describeEventsForOrganization_nextToken,
    describeEventsForOrganization_locale,
    describeEventsForOrganization_filter,
    describeEventsForOrganization_maxResults,

    -- * Destructuring the Response
    DescribeEventsForOrganizationResponse (..),
    newDescribeEventsForOrganizationResponse,

    -- * Response Lenses
    describeEventsForOrganizationResponse_nextToken,
    describeEventsForOrganizationResponse_events,
    describeEventsForOrganizationResponse_httpStatus,
  )
where

import Amazonka.AWSHealth.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEventsForOrganization' smart constructor.
data DescribeEventsForOrganization = DescribeEventsForOrganization'
  { -- | If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next batch of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The locale (language) to return information in. English (en) is the
    -- default and the only supported value at this time.
    locale :: Prelude.Maybe Prelude.Text,
    -- | Values to narrow the results returned.
    filter' :: Prelude.Maybe OrganizationEventFilter,
    -- | The maximum number of items to return in one batch, between 10 and 100,
    -- inclusive.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEventsForOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEventsForOrganization_nextToken' - If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'locale', 'describeEventsForOrganization_locale' - The locale (language) to return information in. English (en) is the
-- default and the only supported value at this time.
--
-- 'filter'', 'describeEventsForOrganization_filter' - Values to narrow the results returned.
--
-- 'maxResults', 'describeEventsForOrganization_maxResults' - The maximum number of items to return in one batch, between 10 and 100,
-- inclusive.
newDescribeEventsForOrganization ::
  DescribeEventsForOrganization
newDescribeEventsForOrganization =
  DescribeEventsForOrganization'
    { nextToken =
        Prelude.Nothing,
      locale = Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
describeEventsForOrganization_nextToken :: Lens.Lens' DescribeEventsForOrganization (Prelude.Maybe Prelude.Text)
describeEventsForOrganization_nextToken = Lens.lens (\DescribeEventsForOrganization' {nextToken} -> nextToken) (\s@DescribeEventsForOrganization' {} a -> s {nextToken = a} :: DescribeEventsForOrganization)

-- | The locale (language) to return information in. English (en) is the
-- default and the only supported value at this time.
describeEventsForOrganization_locale :: Lens.Lens' DescribeEventsForOrganization (Prelude.Maybe Prelude.Text)
describeEventsForOrganization_locale = Lens.lens (\DescribeEventsForOrganization' {locale} -> locale) (\s@DescribeEventsForOrganization' {} a -> s {locale = a} :: DescribeEventsForOrganization)

-- | Values to narrow the results returned.
describeEventsForOrganization_filter :: Lens.Lens' DescribeEventsForOrganization (Prelude.Maybe OrganizationEventFilter)
describeEventsForOrganization_filter = Lens.lens (\DescribeEventsForOrganization' {filter'} -> filter') (\s@DescribeEventsForOrganization' {} a -> s {filter' = a} :: DescribeEventsForOrganization)

-- | The maximum number of items to return in one batch, between 10 and 100,
-- inclusive.
describeEventsForOrganization_maxResults :: Lens.Lens' DescribeEventsForOrganization (Prelude.Maybe Prelude.Natural)
describeEventsForOrganization_maxResults = Lens.lens (\DescribeEventsForOrganization' {maxResults} -> maxResults) (\s@DescribeEventsForOrganization' {} a -> s {maxResults = a} :: DescribeEventsForOrganization)

instance Core.AWSPager DescribeEventsForOrganization where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEventsForOrganizationResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEventsForOrganizationResponse_events
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeEventsForOrganization_nextToken
          Lens..~ rs
          Lens.^? describeEventsForOrganizationResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeEventsForOrganization
  where
  type
    AWSResponse DescribeEventsForOrganization =
      DescribeEventsForOrganizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventsForOrganizationResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "events" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeEventsForOrganization
  where
  hashWithSalt _salt DescribeEventsForOrganization' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeEventsForOrganization where
  rnf DescribeEventsForOrganization' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf locale
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders DescribeEventsForOrganization where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHealth_20160804.DescribeEventsForOrganization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEventsForOrganization where
  toJSON DescribeEventsForOrganization' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("locale" Data..=) Prelude.<$> locale,
            ("filter" Data..=) Prelude.<$> filter',
            ("maxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath DescribeEventsForOrganization where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEventsForOrganization where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEventsForOrganizationResponse' smart constructor.
data DescribeEventsForOrganizationResponse = DescribeEventsForOrganizationResponse'
  { -- | If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next batch of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The events that match the specified filter criteria.
    events :: Prelude.Maybe [OrganizationEvent],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEventsForOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEventsForOrganizationResponse_nextToken' - If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'events', 'describeEventsForOrganizationResponse_events' - The events that match the specified filter criteria.
--
-- 'httpStatus', 'describeEventsForOrganizationResponse_httpStatus' - The response's http status code.
newDescribeEventsForOrganizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEventsForOrganizationResponse
newDescribeEventsForOrganizationResponse pHttpStatus_ =
  DescribeEventsForOrganizationResponse'
    { nextToken =
        Prelude.Nothing,
      events = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
describeEventsForOrganizationResponse_nextToken :: Lens.Lens' DescribeEventsForOrganizationResponse (Prelude.Maybe Prelude.Text)
describeEventsForOrganizationResponse_nextToken = Lens.lens (\DescribeEventsForOrganizationResponse' {nextToken} -> nextToken) (\s@DescribeEventsForOrganizationResponse' {} a -> s {nextToken = a} :: DescribeEventsForOrganizationResponse)

-- | The events that match the specified filter criteria.
describeEventsForOrganizationResponse_events :: Lens.Lens' DescribeEventsForOrganizationResponse (Prelude.Maybe [OrganizationEvent])
describeEventsForOrganizationResponse_events = Lens.lens (\DescribeEventsForOrganizationResponse' {events} -> events) (\s@DescribeEventsForOrganizationResponse' {} a -> s {events = a} :: DescribeEventsForOrganizationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeEventsForOrganizationResponse_httpStatus :: Lens.Lens' DescribeEventsForOrganizationResponse Prelude.Int
describeEventsForOrganizationResponse_httpStatus = Lens.lens (\DescribeEventsForOrganizationResponse' {httpStatus} -> httpStatus) (\s@DescribeEventsForOrganizationResponse' {} a -> s {httpStatus = a} :: DescribeEventsForOrganizationResponse)

instance
  Prelude.NFData
    DescribeEventsForOrganizationResponse
  where
  rnf DescribeEventsForOrganizationResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf events
      `Prelude.seq` Prelude.rnf httpStatus
