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
-- Module      : Network.AWS.CloudWatchLogs.DescribeLogStreams
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the log streams for the specified log group. You can list all the
-- log streams or filter the results by prefix. You can also control how
-- the results are ordered.
--
-- This operation has a limit of five transactions per second, after which
-- transactions are throttled.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeLogStreams
  ( -- * Creating a Request
    DescribeLogStreams (..),
    newDescribeLogStreams,

    -- * Request Lenses
    describeLogStreams_logStreamNamePrefix,
    describeLogStreams_nextToken,
    describeLogStreams_orderBy,
    describeLogStreams_descending,
    describeLogStreams_limit,
    describeLogStreams_logGroupName,

    -- * Destructuring the Response
    DescribeLogStreamsResponse (..),
    newDescribeLogStreamsResponse,

    -- * Response Lenses
    describeLogStreamsResponse_nextToken,
    describeLogStreamsResponse_logStreams,
    describeLogStreamsResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLogStreams' smart constructor.
data DescribeLogStreams = DescribeLogStreams'
  { -- | The prefix to match.
    --
    -- If @orderBy@ is @LastEventTime@, you cannot specify this parameter.
    logStreamNamePrefix :: Core.Maybe Core.Text,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | If the value is @LogStreamName@, the results are ordered by log stream
    -- name. If the value is @LastEventTime@, the results are ordered by the
    -- event time. The default value is @LogStreamName@.
    --
    -- If you order the results by event time, you cannot specify the
    -- @logStreamNamePrefix@ parameter.
    --
    -- @lastEventTimeStamp@ represents the time of the most recent log event in
    -- the log stream in CloudWatch Logs. This number is expressed as the
    -- number of milliseconds after Jan 1, 1970 00:00:00 UTC.
    -- @lastEventTimeStamp@ updates on an eventual consistency basis. It
    -- typically updates in less than an hour from ingestion, but in rare
    -- situations might take longer.
    orderBy :: Core.Maybe OrderBy,
    -- | If the value is true, results are returned in descending order. If the
    -- value is to false, results are returned in ascending order. The default
    -- value is false.
    descending :: Core.Maybe Core.Bool,
    -- | The maximum number of items returned. If you don\'t specify a value, the
    -- default is up to 50 items.
    limit :: Core.Maybe Core.Natural,
    -- | The name of the log group.
    logGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLogStreams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logStreamNamePrefix', 'describeLogStreams_logStreamNamePrefix' - The prefix to match.
--
-- If @orderBy@ is @LastEventTime@, you cannot specify this parameter.
--
-- 'nextToken', 'describeLogStreams_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'orderBy', 'describeLogStreams_orderBy' - If the value is @LogStreamName@, the results are ordered by log stream
-- name. If the value is @LastEventTime@, the results are ordered by the
-- event time. The default value is @LogStreamName@.
--
-- If you order the results by event time, you cannot specify the
-- @logStreamNamePrefix@ parameter.
--
-- @lastEventTimeStamp@ represents the time of the most recent log event in
-- the log stream in CloudWatch Logs. This number is expressed as the
-- number of milliseconds after Jan 1, 1970 00:00:00 UTC.
-- @lastEventTimeStamp@ updates on an eventual consistency basis. It
-- typically updates in less than an hour from ingestion, but in rare
-- situations might take longer.
--
-- 'descending', 'describeLogStreams_descending' - If the value is true, results are returned in descending order. If the
-- value is to false, results are returned in ascending order. The default
-- value is false.
--
-- 'limit', 'describeLogStreams_limit' - The maximum number of items returned. If you don\'t specify a value, the
-- default is up to 50 items.
--
-- 'logGroupName', 'describeLogStreams_logGroupName' - The name of the log group.
newDescribeLogStreams ::
  -- | 'logGroupName'
  Core.Text ->
  DescribeLogStreams
newDescribeLogStreams pLogGroupName_ =
  DescribeLogStreams'
    { logStreamNamePrefix =
        Core.Nothing,
      nextToken = Core.Nothing,
      orderBy = Core.Nothing,
      descending = Core.Nothing,
      limit = Core.Nothing,
      logGroupName = pLogGroupName_
    }

-- | The prefix to match.
--
-- If @orderBy@ is @LastEventTime@, you cannot specify this parameter.
describeLogStreams_logStreamNamePrefix :: Lens.Lens' DescribeLogStreams (Core.Maybe Core.Text)
describeLogStreams_logStreamNamePrefix = Lens.lens (\DescribeLogStreams' {logStreamNamePrefix} -> logStreamNamePrefix) (\s@DescribeLogStreams' {} a -> s {logStreamNamePrefix = a} :: DescribeLogStreams)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeLogStreams_nextToken :: Lens.Lens' DescribeLogStreams (Core.Maybe Core.Text)
describeLogStreams_nextToken = Lens.lens (\DescribeLogStreams' {nextToken} -> nextToken) (\s@DescribeLogStreams' {} a -> s {nextToken = a} :: DescribeLogStreams)

-- | If the value is @LogStreamName@, the results are ordered by log stream
-- name. If the value is @LastEventTime@, the results are ordered by the
-- event time. The default value is @LogStreamName@.
--
-- If you order the results by event time, you cannot specify the
-- @logStreamNamePrefix@ parameter.
--
-- @lastEventTimeStamp@ represents the time of the most recent log event in
-- the log stream in CloudWatch Logs. This number is expressed as the
-- number of milliseconds after Jan 1, 1970 00:00:00 UTC.
-- @lastEventTimeStamp@ updates on an eventual consistency basis. It
-- typically updates in less than an hour from ingestion, but in rare
-- situations might take longer.
describeLogStreams_orderBy :: Lens.Lens' DescribeLogStreams (Core.Maybe OrderBy)
describeLogStreams_orderBy = Lens.lens (\DescribeLogStreams' {orderBy} -> orderBy) (\s@DescribeLogStreams' {} a -> s {orderBy = a} :: DescribeLogStreams)

-- | If the value is true, results are returned in descending order. If the
-- value is to false, results are returned in ascending order. The default
-- value is false.
describeLogStreams_descending :: Lens.Lens' DescribeLogStreams (Core.Maybe Core.Bool)
describeLogStreams_descending = Lens.lens (\DescribeLogStreams' {descending} -> descending) (\s@DescribeLogStreams' {} a -> s {descending = a} :: DescribeLogStreams)

-- | The maximum number of items returned. If you don\'t specify a value, the
-- default is up to 50 items.
describeLogStreams_limit :: Lens.Lens' DescribeLogStreams (Core.Maybe Core.Natural)
describeLogStreams_limit = Lens.lens (\DescribeLogStreams' {limit} -> limit) (\s@DescribeLogStreams' {} a -> s {limit = a} :: DescribeLogStreams)

-- | The name of the log group.
describeLogStreams_logGroupName :: Lens.Lens' DescribeLogStreams Core.Text
describeLogStreams_logGroupName = Lens.lens (\DescribeLogStreams' {logGroupName} -> logGroupName) (\s@DescribeLogStreams' {} a -> s {logGroupName = a} :: DescribeLogStreams)

instance Core.AWSPager DescribeLogStreams where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeLogStreamsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeLogStreamsResponse_logStreams
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeLogStreams_nextToken
          Lens..~ rs
          Lens.^? describeLogStreamsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeLogStreams where
  type
    AWSResponse DescribeLogStreams =
      DescribeLogStreamsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLogStreamsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "logStreams" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeLogStreams

instance Core.NFData DescribeLogStreams

instance Core.ToHeaders DescribeLogStreams where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.DescribeLogStreams" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeLogStreams where
  toJSON DescribeLogStreams' {..} =
    Core.object
      ( Core.catMaybes
          [ ("logStreamNamePrefix" Core..=)
              Core.<$> logStreamNamePrefix,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("orderBy" Core..=) Core.<$> orderBy,
            ("descending" Core..=) Core.<$> descending,
            ("limit" Core..=) Core.<$> limit,
            Core.Just ("logGroupName" Core..= logGroupName)
          ]
      )

instance Core.ToPath DescribeLogStreams where
  toPath = Core.const "/"

instance Core.ToQuery DescribeLogStreams where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeLogStreamsResponse' smart constructor.
data DescribeLogStreamsResponse = DescribeLogStreamsResponse'
  { nextToken :: Core.Maybe Core.Text,
    -- | The log streams.
    logStreams :: Core.Maybe [LogStream],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLogStreamsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeLogStreamsResponse_nextToken' - Undocumented member.
--
-- 'logStreams', 'describeLogStreamsResponse_logStreams' - The log streams.
--
-- 'httpStatus', 'describeLogStreamsResponse_httpStatus' - The response's http status code.
newDescribeLogStreamsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeLogStreamsResponse
newDescribeLogStreamsResponse pHttpStatus_ =
  DescribeLogStreamsResponse'
    { nextToken =
        Core.Nothing,
      logStreams = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeLogStreamsResponse_nextToken :: Lens.Lens' DescribeLogStreamsResponse (Core.Maybe Core.Text)
describeLogStreamsResponse_nextToken = Lens.lens (\DescribeLogStreamsResponse' {nextToken} -> nextToken) (\s@DescribeLogStreamsResponse' {} a -> s {nextToken = a} :: DescribeLogStreamsResponse)

-- | The log streams.
describeLogStreamsResponse_logStreams :: Lens.Lens' DescribeLogStreamsResponse (Core.Maybe [LogStream])
describeLogStreamsResponse_logStreams = Lens.lens (\DescribeLogStreamsResponse' {logStreams} -> logStreams) (\s@DescribeLogStreamsResponse' {} a -> s {logStreams = a} :: DescribeLogStreamsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeLogStreamsResponse_httpStatus :: Lens.Lens' DescribeLogStreamsResponse Core.Int
describeLogStreamsResponse_httpStatus = Lens.lens (\DescribeLogStreamsResponse' {httpStatus} -> httpStatus) (\s@DescribeLogStreamsResponse' {} a -> s {httpStatus = a} :: DescribeLogStreamsResponse)

instance Core.NFData DescribeLogStreamsResponse
