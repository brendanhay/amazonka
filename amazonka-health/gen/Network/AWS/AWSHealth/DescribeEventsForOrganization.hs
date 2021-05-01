{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AWSHealth.DescribeEventsForOrganization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about events across your organization in AWS
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
-- For more information about the different types of AWS Health events, see
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html Event>.
--
-- Before you can call this operation, you must first enable AWS Health to
-- work with AWS Organizations. To do this, call the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_EnableHealthServiceAccessForOrganization.html EnableHealthServiceAccessForOrganization>
-- operation from your organization\'s management account.
--
-- This API operation uses pagination. Specify the @nextToken@ parameter in
-- the next request to return more results.
--
-- This operation returns paginated results.
module Network.AWS.AWSHealth.DescribeEventsForOrganization
  ( -- * Creating a Request
    DescribeEventsForOrganization (..),
    newDescribeEventsForOrganization,

    -- * Request Lenses
    describeEventsForOrganization_nextToken,
    describeEventsForOrganization_maxResults,
    describeEventsForOrganization_locale,
    describeEventsForOrganization_filter,

    -- * Destructuring the Response
    DescribeEventsForOrganizationResponse (..),
    newDescribeEventsForOrganizationResponse,

    -- * Response Lenses
    describeEventsForOrganizationResponse_nextToken,
    describeEventsForOrganizationResponse_events,
    describeEventsForOrganizationResponse_httpStatus,
  )
where

import Network.AWS.AWSHealth.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEventsForOrganization' smart constructor.
data DescribeEventsForOrganization = DescribeEventsForOrganization'
  { -- | If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next batch of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return in one batch, between 10 and 100,
    -- inclusive.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The locale (language) to return information in. English (en) is the
    -- default and the only supported value at this time.
    locale :: Prelude.Maybe Prelude.Text,
    -- | Values to narrow the results returned.
    filter' :: Prelude.Maybe OrganizationEventFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'maxResults', 'describeEventsForOrganization_maxResults' - The maximum number of items to return in one batch, between 10 and 100,
-- inclusive.
--
-- 'locale', 'describeEventsForOrganization_locale' - The locale (language) to return information in. English (en) is the
-- default and the only supported value at this time.
--
-- 'filter'', 'describeEventsForOrganization_filter' - Values to narrow the results returned.
newDescribeEventsForOrganization ::
  DescribeEventsForOrganization
newDescribeEventsForOrganization =
  DescribeEventsForOrganization'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      locale = Prelude.Nothing,
      filter' = Prelude.Nothing
    }

-- | If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
describeEventsForOrganization_nextToken :: Lens.Lens' DescribeEventsForOrganization (Prelude.Maybe Prelude.Text)
describeEventsForOrganization_nextToken = Lens.lens (\DescribeEventsForOrganization' {nextToken} -> nextToken) (\s@DescribeEventsForOrganization' {} a -> s {nextToken = a} :: DescribeEventsForOrganization)

-- | The maximum number of items to return in one batch, between 10 and 100,
-- inclusive.
describeEventsForOrganization_maxResults :: Lens.Lens' DescribeEventsForOrganization (Prelude.Maybe Prelude.Natural)
describeEventsForOrganization_maxResults = Lens.lens (\DescribeEventsForOrganization' {maxResults} -> maxResults) (\s@DescribeEventsForOrganization' {} a -> s {maxResults = a} :: DescribeEventsForOrganization)

-- | The locale (language) to return information in. English (en) is the
-- default and the only supported value at this time.
describeEventsForOrganization_locale :: Lens.Lens' DescribeEventsForOrganization (Prelude.Maybe Prelude.Text)
describeEventsForOrganization_locale = Lens.lens (\DescribeEventsForOrganization' {locale} -> locale) (\s@DescribeEventsForOrganization' {} a -> s {locale = a} :: DescribeEventsForOrganization)

-- | Values to narrow the results returned.
describeEventsForOrganization_filter :: Lens.Lens' DescribeEventsForOrganization (Prelude.Maybe OrganizationEventFilter)
describeEventsForOrganization_filter = Lens.lens (\DescribeEventsForOrganization' {filter'} -> filter') (\s@DescribeEventsForOrganization' {} a -> s {filter' = a} :: DescribeEventsForOrganization)

instance Pager.AWSPager DescribeEventsForOrganization where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeEventsForOrganizationResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeEventsForOrganizationResponse_events
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeEventsForOrganization_nextToken
          Lens..~ rs
          Lens.^? describeEventsForOrganizationResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeEventsForOrganization
  where
  type
    Rs DescribeEventsForOrganization =
      DescribeEventsForOrganizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventsForOrganizationResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> (x Prelude..?> "events" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeEventsForOrganization

instance Prelude.NFData DescribeEventsForOrganization

instance
  Prelude.ToHeaders
    DescribeEventsForOrganization
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSHealth_20160804.DescribeEventsForOrganization" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeEventsForOrganization where
  toJSON DescribeEventsForOrganization' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
            ("maxResults" Prelude..=) Prelude.<$> maxResults,
            ("locale" Prelude..=) Prelude.<$> locale,
            ("filter" Prelude..=) Prelude.<$> filter'
          ]
      )

instance Prelude.ToPath DescribeEventsForOrganization where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeEventsForOrganization
  where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
describeEventsForOrganizationResponse_events = Lens.lens (\DescribeEventsForOrganizationResponse' {events} -> events) (\s@DescribeEventsForOrganizationResponse' {} a -> s {events = a} :: DescribeEventsForOrganizationResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeEventsForOrganizationResponse_httpStatus :: Lens.Lens' DescribeEventsForOrganizationResponse Prelude.Int
describeEventsForOrganizationResponse_httpStatus = Lens.lens (\DescribeEventsForOrganizationResponse' {httpStatus} -> httpStatus) (\s@DescribeEventsForOrganizationResponse' {} a -> s {httpStatus = a} :: DescribeEventsForOrganizationResponse)

instance
  Prelude.NFData
    DescribeEventsForOrganizationResponse
