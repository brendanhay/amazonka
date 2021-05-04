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
-- Module      : Network.AWS.CloudTrail.LookupEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- -   AWS access key
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
module Network.AWS.CloudTrail.LookupEvents
  ( -- * Creating a Request
    LookupEvents (..),
    newLookupEvents,

    -- * Request Lenses
    lookupEvents_nextToken,
    lookupEvents_maxResults,
    lookupEvents_startTime,
    lookupEvents_endTime,
    lookupEvents_eventCategory,
    lookupEvents_lookupAttributes,

    -- * Destructuring the Response
    LookupEventsResponse (..),
    newLookupEventsResponse,

    -- * Response Lenses
    lookupEventsResponse_nextToken,
    lookupEventsResponse_events,
    lookupEventsResponse_httpStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains a request for LookupEvents.
--
-- /See:/ 'newLookupEvents' smart constructor.
data LookupEvents = LookupEvents'
  { -- | The token to use to get the next page of results after a previous API
    -- call. This token must be passed in with the same parameters that were
    -- specified in the the original call. For example, if the original call
    -- specified an AttributeKey of \'Username\' with a value of \'root\', the
    -- call with NextToken should include those same parameters.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of events to return. Possible values are 1 through 50. The
    -- default is 50.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specifies that only events that occur after or at the specified time are
    -- returned. If the specified start time is after the specified end time,
    -- an error is returned.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | Specifies that only events that occur before or at the specified time
    -- are returned. If the specified end time is before the specified start
    -- time, an error is returned.
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | Specifies the event category. If you do not specify an event category,
    -- events of the category are not returned in the response. For example, if
    -- you do not specify @insight@ as the value of @EventCategory@, no
    -- Insights events are returned.
    eventCategory :: Prelude.Maybe EventCategory,
    -- | Contains a list of lookup attributes. Currently the list can contain
    -- only one item.
    lookupAttributes :: Prelude.Maybe [LookupAttribute]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LookupEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'lookupEvents_nextToken' - The token to use to get the next page of results after a previous API
-- call. This token must be passed in with the same parameters that were
-- specified in the the original call. For example, if the original call
-- specified an AttributeKey of \'Username\' with a value of \'root\', the
-- call with NextToken should include those same parameters.
--
-- 'maxResults', 'lookupEvents_maxResults' - The number of events to return. Possible values are 1 through 50. The
-- default is 50.
--
-- 'startTime', 'lookupEvents_startTime' - Specifies that only events that occur after or at the specified time are
-- returned. If the specified start time is after the specified end time,
-- an error is returned.
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
newLookupEvents ::
  LookupEvents
newLookupEvents =
  LookupEvents'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      startTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      eventCategory = Prelude.Nothing,
      lookupAttributes = Prelude.Nothing
    }

-- | The token to use to get the next page of results after a previous API
-- call. This token must be passed in with the same parameters that were
-- specified in the the original call. For example, if the original call
-- specified an AttributeKey of \'Username\' with a value of \'root\', the
-- call with NextToken should include those same parameters.
lookupEvents_nextToken :: Lens.Lens' LookupEvents (Prelude.Maybe Prelude.Text)
lookupEvents_nextToken = Lens.lens (\LookupEvents' {nextToken} -> nextToken) (\s@LookupEvents' {} a -> s {nextToken = a} :: LookupEvents)

-- | The number of events to return. Possible values are 1 through 50. The
-- default is 50.
lookupEvents_maxResults :: Lens.Lens' LookupEvents (Prelude.Maybe Prelude.Natural)
lookupEvents_maxResults = Lens.lens (\LookupEvents' {maxResults} -> maxResults) (\s@LookupEvents' {} a -> s {maxResults = a} :: LookupEvents)

-- | Specifies that only events that occur after or at the specified time are
-- returned. If the specified start time is after the specified end time,
-- an error is returned.
lookupEvents_startTime :: Lens.Lens' LookupEvents (Prelude.Maybe Prelude.UTCTime)
lookupEvents_startTime = Lens.lens (\LookupEvents' {startTime} -> startTime) (\s@LookupEvents' {} a -> s {startTime = a} :: LookupEvents) Prelude.. Lens.mapping Prelude._Time

-- | Specifies that only events that occur before or at the specified time
-- are returned. If the specified end time is before the specified start
-- time, an error is returned.
lookupEvents_endTime :: Lens.Lens' LookupEvents (Prelude.Maybe Prelude.UTCTime)
lookupEvents_endTime = Lens.lens (\LookupEvents' {endTime} -> endTime) (\s@LookupEvents' {} a -> s {endTime = a} :: LookupEvents) Prelude.. Lens.mapping Prelude._Time

-- | Specifies the event category. If you do not specify an event category,
-- events of the category are not returned in the response. For example, if
-- you do not specify @insight@ as the value of @EventCategory@, no
-- Insights events are returned.
lookupEvents_eventCategory :: Lens.Lens' LookupEvents (Prelude.Maybe EventCategory)
lookupEvents_eventCategory = Lens.lens (\LookupEvents' {eventCategory} -> eventCategory) (\s@LookupEvents' {} a -> s {eventCategory = a} :: LookupEvents)

-- | Contains a list of lookup attributes. Currently the list can contain
-- only one item.
lookupEvents_lookupAttributes :: Lens.Lens' LookupEvents (Prelude.Maybe [LookupAttribute])
lookupEvents_lookupAttributes = Lens.lens (\LookupEvents' {lookupAttributes} -> lookupAttributes) (\s@LookupEvents' {} a -> s {lookupAttributes = a} :: LookupEvents) Prelude.. Lens.mapping Prelude._Coerce

instance Pager.AWSPager LookupEvents where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? lookupEventsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? lookupEventsResponse_events Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& lookupEvents_nextToken
          Lens..~ rs
          Lens.^? lookupEventsResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest LookupEvents where
  type Rs LookupEvents = LookupEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          LookupEventsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (x Prelude..?> "Events" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable LookupEvents

instance Prelude.NFData LookupEvents

instance Prelude.ToHeaders LookupEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.LookupEvents" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON LookupEvents where
  toJSON LookupEvents' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("StartTime" Prelude..=) Prelude.<$> startTime,
            ("EndTime" Prelude..=) Prelude.<$> endTime,
            ("EventCategory" Prelude..=)
              Prelude.<$> eventCategory,
            ("LookupAttributes" Prelude..=)
              Prelude.<$> lookupAttributes
          ]
      )

instance Prelude.ToPath LookupEvents where
  toPath = Prelude.const "/"

instance Prelude.ToQuery LookupEvents where
  toQuery = Prelude.const Prelude.mempty

-- | Contains a response to a LookupEvents action.
--
-- /See:/ 'newLookupEventsResponse' smart constructor.
data LookupEventsResponse = LookupEventsResponse'
  { -- | The token to use to get the next page of results after a previous API
    -- call. If the token does not appear, there are no more results to return.
    -- The token must be passed in with the same parameters as the previous
    -- call. For example, if the original call specified an AttributeKey of
    -- \'Username\' with a value of \'root\', the call with NextToken should
    -- include those same parameters.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of events returned based on the lookup attributes specified and
    -- the CloudTrail event. The events list is sorted by time. The most recent
    -- event is listed first.
    events :: Prelude.Maybe [Event],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LookupEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'lookupEventsResponse_nextToken' - The token to use to get the next page of results after a previous API
-- call. If the token does not appear, there are no more results to return.
-- The token must be passed in with the same parameters as the previous
-- call. For example, if the original call specified an AttributeKey of
-- \'Username\' with a value of \'root\', the call with NextToken should
-- include those same parameters.
--
-- 'events', 'lookupEventsResponse_events' - A list of events returned based on the lookup attributes specified and
-- the CloudTrail event. The events list is sorted by time. The most recent
-- event is listed first.
--
-- 'httpStatus', 'lookupEventsResponse_httpStatus' - The response's http status code.
newLookupEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  LookupEventsResponse
newLookupEventsResponse pHttpStatus_ =
  LookupEventsResponse'
    { nextToken = Prelude.Nothing,
      events = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next page of results after a previous API
-- call. If the token does not appear, there are no more results to return.
-- The token must be passed in with the same parameters as the previous
-- call. For example, if the original call specified an AttributeKey of
-- \'Username\' with a value of \'root\', the call with NextToken should
-- include those same parameters.
lookupEventsResponse_nextToken :: Lens.Lens' LookupEventsResponse (Prelude.Maybe Prelude.Text)
lookupEventsResponse_nextToken = Lens.lens (\LookupEventsResponse' {nextToken} -> nextToken) (\s@LookupEventsResponse' {} a -> s {nextToken = a} :: LookupEventsResponse)

-- | A list of events returned based on the lookup attributes specified and
-- the CloudTrail event. The events list is sorted by time. The most recent
-- event is listed first.
lookupEventsResponse_events :: Lens.Lens' LookupEventsResponse (Prelude.Maybe [Event])
lookupEventsResponse_events = Lens.lens (\LookupEventsResponse' {events} -> events) (\s@LookupEventsResponse' {} a -> s {events = a} :: LookupEventsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
lookupEventsResponse_httpStatus :: Lens.Lens' LookupEventsResponse Prelude.Int
lookupEventsResponse_httpStatus = Lens.lens (\LookupEventsResponse' {httpStatus} -> httpStatus) (\s@LookupEventsResponse' {} a -> s {httpStatus = a} :: LookupEventsResponse)

instance Prelude.NFData LookupEventsResponse
