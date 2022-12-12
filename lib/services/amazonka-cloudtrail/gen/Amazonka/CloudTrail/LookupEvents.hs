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
-- Module      : Amazonka.CloudTrail.LookupEvents
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Looks up
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-concepts.html#cloudtrail-concepts-management-events management events>
-- or
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-concepts.html#cloudtrail-concepts-insights-events CloudTrail Insights events>
-- that are captured by CloudTrail. You can look up events that occurred in
-- a region within the last 90 days. Lookup supports the following
-- attributes for management events:
--
-- -   Amazon Web Services access key
--
-- -   Event ID
--
-- -   Event name
--
-- -   Event source
--
-- -   Read only
--
-- -   Resource name
--
-- -   Resource type
--
-- -   User name
--
-- Lookup supports the following attributes for Insights events:
--
-- -   Event ID
--
-- -   Event name
--
-- -   Event source
--
-- All attributes are optional. The default number of results returned is
-- 50, with a maximum of 50 possible. The response includes a token that
-- you can use to get the next page of results.
--
-- The rate of lookup requests is limited to two per second, per account,
-- per region. If this limit is exceeded, a throttling error occurs.
--
-- This operation returns paginated results.
module Amazonka.CloudTrail.LookupEvents
  ( -- * Creating a Request
    LookupEvents (..),
    newLookupEvents,

    -- * Request Lenses
    lookupEvents_endTime,
    lookupEvents_eventCategory,
    lookupEvents_lookupAttributes,
    lookupEvents_maxResults,
    lookupEvents_nextToken,
    lookupEvents_startTime,

    -- * Destructuring the Response
    LookupEventsResponse (..),
    newLookupEventsResponse,

    -- * Response Lenses
    lookupEventsResponse_events,
    lookupEventsResponse_nextToken,
    lookupEventsResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains a request for LookupEvents.
--
-- /See:/ 'newLookupEvents' smart constructor.
data LookupEvents = LookupEvents'
  { -- | Specifies that only events that occur before or at the specified time
    -- are returned. If the specified end time is before the specified start
    -- time, an error is returned.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | Specifies the event category. If you do not specify an event category,
    -- events of the category are not returned in the response. For example, if
    -- you do not specify @insight@ as the value of @EventCategory@, no
    -- Insights events are returned.
    eventCategory :: Prelude.Maybe EventCategory,
    -- | Contains a list of lookup attributes. Currently the list can contain
    -- only one item.
    lookupAttributes :: Prelude.Maybe [LookupAttribute],
    -- | The number of events to return. Possible values are 1 through 50. The
    -- default is 50.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to use to get the next page of results after a previous API
    -- call. This token must be passed in with the same parameters that were
    -- specified in the original call. For example, if the original call
    -- specified an AttributeKey of \'Username\' with a value of \'root\', the
    -- call with NextToken should include those same parameters.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies that only events that occur after or at the specified time are
    -- returned. If the specified start time is after the specified end time,
    -- an error is returned.
    startTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LookupEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'lookupEvents_endTime' - Specifies that only events that occur before or at the specified time
-- are returned. If the specified end time is before the specified start
-- time, an error is returned.
--
-- 'eventCategory', 'lookupEvents_eventCategory' - Specifies the event category. If you do not specify an event category,
-- events of the category are not returned in the response. For example, if
-- you do not specify @insight@ as the value of @EventCategory@, no
-- Insights events are returned.
--
-- 'lookupAttributes', 'lookupEvents_lookupAttributes' - Contains a list of lookup attributes. Currently the list can contain
-- only one item.
--
-- 'maxResults', 'lookupEvents_maxResults' - The number of events to return. Possible values are 1 through 50. The
-- default is 50.
--
-- 'nextToken', 'lookupEvents_nextToken' - The token to use to get the next page of results after a previous API
-- call. This token must be passed in with the same parameters that were
-- specified in the original call. For example, if the original call
-- specified an AttributeKey of \'Username\' with a value of \'root\', the
-- call with NextToken should include those same parameters.
--
-- 'startTime', 'lookupEvents_startTime' - Specifies that only events that occur after or at the specified time are
-- returned. If the specified start time is after the specified end time,
-- an error is returned.
newLookupEvents ::
  LookupEvents
newLookupEvents =
  LookupEvents'
    { endTime = Prelude.Nothing,
      eventCategory = Prelude.Nothing,
      lookupAttributes = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | Specifies that only events that occur before or at the specified time
-- are returned. If the specified end time is before the specified start
-- time, an error is returned.
lookupEvents_endTime :: Lens.Lens' LookupEvents (Prelude.Maybe Prelude.UTCTime)
lookupEvents_endTime = Lens.lens (\LookupEvents' {endTime} -> endTime) (\s@LookupEvents' {} a -> s {endTime = a} :: LookupEvents) Prelude.. Lens.mapping Data._Time

-- | Specifies the event category. If you do not specify an event category,
-- events of the category are not returned in the response. For example, if
-- you do not specify @insight@ as the value of @EventCategory@, no
-- Insights events are returned.
lookupEvents_eventCategory :: Lens.Lens' LookupEvents (Prelude.Maybe EventCategory)
lookupEvents_eventCategory = Lens.lens (\LookupEvents' {eventCategory} -> eventCategory) (\s@LookupEvents' {} a -> s {eventCategory = a} :: LookupEvents)

-- | Contains a list of lookup attributes. Currently the list can contain
-- only one item.
lookupEvents_lookupAttributes :: Lens.Lens' LookupEvents (Prelude.Maybe [LookupAttribute])
lookupEvents_lookupAttributes = Lens.lens (\LookupEvents' {lookupAttributes} -> lookupAttributes) (\s@LookupEvents' {} a -> s {lookupAttributes = a} :: LookupEvents) Prelude.. Lens.mapping Lens.coerced

-- | The number of events to return. Possible values are 1 through 50. The
-- default is 50.
lookupEvents_maxResults :: Lens.Lens' LookupEvents (Prelude.Maybe Prelude.Natural)
lookupEvents_maxResults = Lens.lens (\LookupEvents' {maxResults} -> maxResults) (\s@LookupEvents' {} a -> s {maxResults = a} :: LookupEvents)

-- | The token to use to get the next page of results after a previous API
-- call. This token must be passed in with the same parameters that were
-- specified in the original call. For example, if the original call
-- specified an AttributeKey of \'Username\' with a value of \'root\', the
-- call with NextToken should include those same parameters.
lookupEvents_nextToken :: Lens.Lens' LookupEvents (Prelude.Maybe Prelude.Text)
lookupEvents_nextToken = Lens.lens (\LookupEvents' {nextToken} -> nextToken) (\s@LookupEvents' {} a -> s {nextToken = a} :: LookupEvents)

-- | Specifies that only events that occur after or at the specified time are
-- returned. If the specified start time is after the specified end time,
-- an error is returned.
lookupEvents_startTime :: Lens.Lens' LookupEvents (Prelude.Maybe Prelude.UTCTime)
lookupEvents_startTime = Lens.lens (\LookupEvents' {startTime} -> startTime) (\s@LookupEvents' {} a -> s {startTime = a} :: LookupEvents) Prelude.. Lens.mapping Data._Time

instance Core.AWSPager LookupEvents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? lookupEventsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? lookupEventsResponse_events Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& lookupEvents_nextToken
          Lens..~ rs
          Lens.^? lookupEventsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest LookupEvents where
  type AWSResponse LookupEvents = LookupEventsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          LookupEventsResponse'
            Prelude.<$> (x Data..?> "Events" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable LookupEvents where
  hashWithSalt _salt LookupEvents' {..} =
    _salt `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` eventCategory
      `Prelude.hashWithSalt` lookupAttributes
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData LookupEvents where
  rnf LookupEvents' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf eventCategory
      `Prelude.seq` Prelude.rnf lookupAttributes
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf startTime

instance Data.ToHeaders LookupEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.LookupEvents" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON LookupEvents where
  toJSON LookupEvents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndTime" Data..=) Prelude.<$> endTime,
            ("EventCategory" Data..=) Prelude.<$> eventCategory,
            ("LookupAttributes" Data..=)
              Prelude.<$> lookupAttributes,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("StartTime" Data..=) Prelude.<$> startTime
          ]
      )

instance Data.ToPath LookupEvents where
  toPath = Prelude.const "/"

instance Data.ToQuery LookupEvents where
  toQuery = Prelude.const Prelude.mempty

-- | Contains a response to a LookupEvents action.
--
-- /See:/ 'newLookupEventsResponse' smart constructor.
data LookupEventsResponse = LookupEventsResponse'
  { -- | A list of events returned based on the lookup attributes specified and
    -- the CloudTrail event. The events list is sorted by time. The most recent
    -- event is listed first.
    events :: Prelude.Maybe [Event],
    -- | The token to use to get the next page of results after a previous API
    -- call. If the token does not appear, there are no more results to return.
    -- The token must be passed in with the same parameters as the previous
    -- call. For example, if the original call specified an AttributeKey of
    -- \'Username\' with a value of \'root\', the call with NextToken should
    -- include those same parameters.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LookupEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'events', 'lookupEventsResponse_events' - A list of events returned based on the lookup attributes specified and
-- the CloudTrail event. The events list is sorted by time. The most recent
-- event is listed first.
--
-- 'nextToken', 'lookupEventsResponse_nextToken' - The token to use to get the next page of results after a previous API
-- call. If the token does not appear, there are no more results to return.
-- The token must be passed in with the same parameters as the previous
-- call. For example, if the original call specified an AttributeKey of
-- \'Username\' with a value of \'root\', the call with NextToken should
-- include those same parameters.
--
-- 'httpStatus', 'lookupEventsResponse_httpStatus' - The response's http status code.
newLookupEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  LookupEventsResponse
newLookupEventsResponse pHttpStatus_ =
  LookupEventsResponse'
    { events = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of events returned based on the lookup attributes specified and
-- the CloudTrail event. The events list is sorted by time. The most recent
-- event is listed first.
lookupEventsResponse_events :: Lens.Lens' LookupEventsResponse (Prelude.Maybe [Event])
lookupEventsResponse_events = Lens.lens (\LookupEventsResponse' {events} -> events) (\s@LookupEventsResponse' {} a -> s {events = a} :: LookupEventsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to get the next page of results after a previous API
-- call. If the token does not appear, there are no more results to return.
-- The token must be passed in with the same parameters as the previous
-- call. For example, if the original call specified an AttributeKey of
-- \'Username\' with a value of \'root\', the call with NextToken should
-- include those same parameters.
lookupEventsResponse_nextToken :: Lens.Lens' LookupEventsResponse (Prelude.Maybe Prelude.Text)
lookupEventsResponse_nextToken = Lens.lens (\LookupEventsResponse' {nextToken} -> nextToken) (\s@LookupEventsResponse' {} a -> s {nextToken = a} :: LookupEventsResponse)

-- | The response's http status code.
lookupEventsResponse_httpStatus :: Lens.Lens' LookupEventsResponse Prelude.Int
lookupEventsResponse_httpStatus = Lens.lens (\LookupEventsResponse' {httpStatus} -> httpStatus) (\s@LookupEventsResponse' {} a -> s {httpStatus = a} :: LookupEventsResponse)

instance Prelude.NFData LookupEventsResponse where
  rnf LookupEventsResponse' {..} =
    Prelude.rnf events
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
