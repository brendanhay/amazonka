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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEventsForOrganization' smart constructor.
data DescribeEventsForOrganization = DescribeEventsForOrganization'
  { -- | If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next batch of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return in one batch, between 10 and 100,
    -- inclusive.
    maxResults :: Core.Maybe Core.Natural,
    -- | The locale (language) to return information in. English (en) is the
    -- default and the only supported value at this time.
    locale :: Core.Maybe Core.Text,
    -- | Values to narrow the results returned.
    filter' :: Core.Maybe OrganizationEventFilter
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      maxResults = Core.Nothing,
      locale = Core.Nothing,
      filter' = Core.Nothing
    }

-- | If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
describeEventsForOrganization_nextToken :: Lens.Lens' DescribeEventsForOrganization (Core.Maybe Core.Text)
describeEventsForOrganization_nextToken = Lens.lens (\DescribeEventsForOrganization' {nextToken} -> nextToken) (\s@DescribeEventsForOrganization' {} a -> s {nextToken = a} :: DescribeEventsForOrganization)

-- | The maximum number of items to return in one batch, between 10 and 100,
-- inclusive.
describeEventsForOrganization_maxResults :: Lens.Lens' DescribeEventsForOrganization (Core.Maybe Core.Natural)
describeEventsForOrganization_maxResults = Lens.lens (\DescribeEventsForOrganization' {maxResults} -> maxResults) (\s@DescribeEventsForOrganization' {} a -> s {maxResults = a} :: DescribeEventsForOrganization)

-- | The locale (language) to return information in. English (en) is the
-- default and the only supported value at this time.
describeEventsForOrganization_locale :: Lens.Lens' DescribeEventsForOrganization (Core.Maybe Core.Text)
describeEventsForOrganization_locale = Lens.lens (\DescribeEventsForOrganization' {locale} -> locale) (\s@DescribeEventsForOrganization' {} a -> s {locale = a} :: DescribeEventsForOrganization)

-- | Values to narrow the results returned.
describeEventsForOrganization_filter :: Lens.Lens' DescribeEventsForOrganization (Core.Maybe OrganizationEventFilter)
describeEventsForOrganization_filter = Lens.lens (\DescribeEventsForOrganization' {filter'} -> filter') (\s@DescribeEventsForOrganization' {} a -> s {filter' = a} :: DescribeEventsForOrganization)

instance Core.AWSPager DescribeEventsForOrganization where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEventsForOrganizationResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEventsForOrganizationResponse_events
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeEventsForOrganization_nextToken
          Lens..~ rs
          Lens.^? describeEventsForOrganizationResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeEventsForOrganization
  where
  type
    AWSResponse DescribeEventsForOrganization =
      DescribeEventsForOrganizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventsForOrganizationResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "events" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEventsForOrganization

instance Core.NFData DescribeEventsForOrganization

instance Core.ToHeaders DescribeEventsForOrganization where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHealth_20160804.DescribeEventsForOrganization" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeEventsForOrganization where
  toJSON DescribeEventsForOrganization' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("locale" Core..=) Core.<$> locale,
            ("filter" Core..=) Core.<$> filter'
          ]
      )

instance Core.ToPath DescribeEventsForOrganization where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEventsForOrganization where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeEventsForOrganizationResponse' smart constructor.
data DescribeEventsForOrganizationResponse = DescribeEventsForOrganizationResponse'
  { -- | If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next batch of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Core.Maybe Core.Text,
    -- | The events that match the specified filter criteria.
    events :: Core.Maybe [OrganizationEvent],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeEventsForOrganizationResponse
newDescribeEventsForOrganizationResponse pHttpStatus_ =
  DescribeEventsForOrganizationResponse'
    { nextToken =
        Core.Nothing,
      events = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
describeEventsForOrganizationResponse_nextToken :: Lens.Lens' DescribeEventsForOrganizationResponse (Core.Maybe Core.Text)
describeEventsForOrganizationResponse_nextToken = Lens.lens (\DescribeEventsForOrganizationResponse' {nextToken} -> nextToken) (\s@DescribeEventsForOrganizationResponse' {} a -> s {nextToken = a} :: DescribeEventsForOrganizationResponse)

-- | The events that match the specified filter criteria.
describeEventsForOrganizationResponse_events :: Lens.Lens' DescribeEventsForOrganizationResponse (Core.Maybe [OrganizationEvent])
describeEventsForOrganizationResponse_events = Lens.lens (\DescribeEventsForOrganizationResponse' {events} -> events) (\s@DescribeEventsForOrganizationResponse' {} a -> s {events = a} :: DescribeEventsForOrganizationResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeEventsForOrganizationResponse_httpStatus :: Lens.Lens' DescribeEventsForOrganizationResponse Core.Int
describeEventsForOrganizationResponse_httpStatus = Lens.lens (\DescribeEventsForOrganizationResponse' {httpStatus} -> httpStatus) (\s@DescribeEventsForOrganizationResponse' {} a -> s {httpStatus = a} :: DescribeEventsForOrganizationResponse)

instance
  Core.NFData
    DescribeEventsForOrganizationResponse
