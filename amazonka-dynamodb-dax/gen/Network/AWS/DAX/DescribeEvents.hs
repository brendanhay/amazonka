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
-- Module      : Network.AWS.DAX.DescribeEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns events related to DAX clusters and parameter groups. You can
-- obtain events specific to a particular DAX cluster or parameter group by
-- providing the name as a parameter.
--
-- By default, only the events occurring within the last 24 hours are
-- returned; however, you can retrieve up to 14 days\' worth of events if
-- necessary.
--
-- This operation returns paginated results.
module Network.AWS.DAX.DescribeEvents
  ( -- * Creating a Request
    DescribeEvents (..),
    newDescribeEvents,

    -- * Request Lenses
    describeEvents_nextToken,
    describeEvents_duration,
    describeEvents_maxResults,
    describeEvents_startTime,
    describeEvents_sourceName,
    describeEvents_endTime,
    describeEvents_sourceType,

    -- * Destructuring the Response
    DescribeEventsResponse (..),
    newDescribeEventsResponse,

    -- * Response Lenses
    describeEventsResponse_nextToken,
    describeEventsResponse_events,
    describeEventsResponse_httpStatus,
  )
where

import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of minutes\' worth of events to retrieve.
    duration :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    --
    -- The value for @MaxResults@ must be between 20 and 100.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The beginning of the time interval to retrieve events for, specified in
    -- ISO 8601 format.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The identifier of the event source for which events will be returned. If
    -- not specified, then all sources are included in the response.
    sourceName :: Prelude.Maybe Prelude.Text,
    -- | The end of the time interval for which to retrieve events, specified in
    -- ISO 8601 format.
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | The event source to retrieve events for. If no value is specified, all
    -- events are returned.
    sourceType :: Prelude.Maybe SourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEvents_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
--
-- 'duration', 'describeEvents_duration' - The number of minutes\' worth of events to retrieve.
--
-- 'maxResults', 'describeEvents_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
--
-- 'startTime', 'describeEvents_startTime' - The beginning of the time interval to retrieve events for, specified in
-- ISO 8601 format.
--
-- 'sourceName', 'describeEvents_sourceName' - The identifier of the event source for which events will be returned. If
-- not specified, then all sources are included in the response.
--
-- 'endTime', 'describeEvents_endTime' - The end of the time interval for which to retrieve events, specified in
-- ISO 8601 format.
--
-- 'sourceType', 'describeEvents_sourceType' - The event source to retrieve events for. If no value is specified, all
-- events are returned.
newDescribeEvents ::
  DescribeEvents
newDescribeEvents =
  DescribeEvents'
    { nextToken = Prelude.Nothing,
      duration = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      startTime = Prelude.Nothing,
      sourceName = Prelude.Nothing,
      endTime = Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
describeEvents_nextToken :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.Text)
describeEvents_nextToken = Lens.lens (\DescribeEvents' {nextToken} -> nextToken) (\s@DescribeEvents' {} a -> s {nextToken = a} :: DescribeEvents)

-- | The number of minutes\' worth of events to retrieve.
describeEvents_duration :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.Int)
describeEvents_duration = Lens.lens (\DescribeEvents' {duration} -> duration) (\s@DescribeEvents' {} a -> s {duration = a} :: DescribeEvents)

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
describeEvents_maxResults :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.Int)
describeEvents_maxResults = Lens.lens (\DescribeEvents' {maxResults} -> maxResults) (\s@DescribeEvents' {} a -> s {maxResults = a} :: DescribeEvents)

-- | The beginning of the time interval to retrieve events for, specified in
-- ISO 8601 format.
describeEvents_startTime :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.UTCTime)
describeEvents_startTime = Lens.lens (\DescribeEvents' {startTime} -> startTime) (\s@DescribeEvents' {} a -> s {startTime = a} :: DescribeEvents) Prelude.. Lens.mapping Prelude._Time

-- | The identifier of the event source for which events will be returned. If
-- not specified, then all sources are included in the response.
describeEvents_sourceName :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.Text)
describeEvents_sourceName = Lens.lens (\DescribeEvents' {sourceName} -> sourceName) (\s@DescribeEvents' {} a -> s {sourceName = a} :: DescribeEvents)

-- | The end of the time interval for which to retrieve events, specified in
-- ISO 8601 format.
describeEvents_endTime :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.UTCTime)
describeEvents_endTime = Lens.lens (\DescribeEvents' {endTime} -> endTime) (\s@DescribeEvents' {} a -> s {endTime = a} :: DescribeEvents) Prelude.. Lens.mapping Prelude._Time

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned.
describeEvents_sourceType :: Lens.Lens' DescribeEvents (Prelude.Maybe SourceType)
describeEvents_sourceType = Lens.lens (\DescribeEvents' {sourceType} -> sourceType) (\s@DescribeEvents' {} a -> s {sourceType = a} :: DescribeEvents)

instance Pager.AWSPager DescribeEvents where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeEventsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeEventsResponse_events Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeEvents_nextToken
          Lens..~ rs
          Lens.^? describeEventsResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeEvents where
  type Rs DescribeEvents = DescribeEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (x Prelude..?> "Events" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEvents

instance Prelude.NFData DescribeEvents

instance Prelude.ToHeaders DescribeEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AmazonDAXV3.DescribeEvents" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeEvents where
  toJSON DescribeEvents' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("Duration" Prelude..=) Prelude.<$> duration,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("StartTime" Prelude..=) Prelude.<$> startTime,
            ("SourceName" Prelude..=) Prelude.<$> sourceName,
            ("EndTime" Prelude..=) Prelude.<$> endTime,
            ("SourceType" Prelude..=) Prelude.<$> sourceType
          ]
      )

instance Prelude.ToPath DescribeEvents where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeEvents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of events. Each element in the array represents one event.
    events :: Prelude.Maybe [Event],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEventsResponse_nextToken' - Provides an identifier to allow retrieval of paginated results.
--
-- 'events', 'describeEventsResponse_events' - An array of events. Each element in the array represents one event.
--
-- 'httpStatus', 'describeEventsResponse_httpStatus' - The response's http status code.
newDescribeEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEventsResponse
newDescribeEventsResponse pHttpStatus_ =
  DescribeEventsResponse'
    { nextToken =
        Prelude.Nothing,
      events = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Provides an identifier to allow retrieval of paginated results.
describeEventsResponse_nextToken :: Lens.Lens' DescribeEventsResponse (Prelude.Maybe Prelude.Text)
describeEventsResponse_nextToken = Lens.lens (\DescribeEventsResponse' {nextToken} -> nextToken) (\s@DescribeEventsResponse' {} a -> s {nextToken = a} :: DescribeEventsResponse)

-- | An array of events. Each element in the array represents one event.
describeEventsResponse_events :: Lens.Lens' DescribeEventsResponse (Prelude.Maybe [Event])
describeEventsResponse_events = Lens.lens (\DescribeEventsResponse' {events} -> events) (\s@DescribeEventsResponse' {} a -> s {events = a} :: DescribeEventsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeEventsResponse_httpStatus :: Lens.Lens' DescribeEventsResponse Prelude.Int
describeEventsResponse_httpStatus = Lens.lens (\DescribeEventsResponse' {httpStatus} -> httpStatus) (\s@DescribeEventsResponse' {} a -> s {httpStatus = a} :: DescribeEventsResponse)

instance Prelude.NFData DescribeEventsResponse
