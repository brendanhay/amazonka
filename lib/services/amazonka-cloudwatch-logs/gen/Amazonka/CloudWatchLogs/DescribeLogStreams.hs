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
-- Module      : Amazonka.CloudWatchLogs.DescribeLogStreams
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- If you are using CloudWatch cross-account observability, you can use
-- this operation in a monitoring account and view data from the linked
-- source accounts. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Unified-Cross-Account.html CloudWatch cross-account observability>.
--
-- This operation returns paginated results.
module Amazonka.CloudWatchLogs.DescribeLogStreams
  ( -- * Creating a Request
    DescribeLogStreams (..),
    newDescribeLogStreams,

    -- * Request Lenses
    describeLogStreams_descending,
    describeLogStreams_limit,
    describeLogStreams_logGroupIdentifier,
    describeLogStreams_logStreamNamePrefix,
    describeLogStreams_nextToken,
    describeLogStreams_orderBy,
    describeLogStreams_logGroupName,

    -- * Destructuring the Response
    DescribeLogStreamsResponse (..),
    newDescribeLogStreamsResponse,

    -- * Response Lenses
    describeLogStreamsResponse_logStreams,
    describeLogStreamsResponse_nextToken,
    describeLogStreamsResponse_httpStatus,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLogStreams' smart constructor.
data DescribeLogStreams = DescribeLogStreams'
  { -- | If the value is true, results are returned in descending order. If the
    -- value is to false, results are returned in ascending order. The default
    -- value is false.
    descending :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of items returned. If you don\'t specify a value, the
    -- default is up to 50 items.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Specify either the name or ARN of the log group to view. If the log
    -- group is in a source account and you are using a monitoring account, you
    -- must use the log group ARN.
    --
    -- If you specify values for both @logGroupName@ and @logGroupIdentifier@,
    -- the action returns an @InvalidParameterException@ error.
    logGroupIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The prefix to match.
    --
    -- If @orderBy@ is @LastEventTime@, you cannot specify this parameter.
    logStreamNamePrefix :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | If the value is @LogStreamName@, the results are ordered by log stream
    -- name. If the value is @LastEventTime@, the results are ordered by the
    -- event time. The default value is @LogStreamName@.
    --
    -- If you order the results by event time, you cannot specify the
    -- @logStreamNamePrefix@ parameter.
    --
    -- @lastEventTimestamp@ represents the time of the most recent log event in
    -- the log stream in CloudWatch Logs. This number is expressed as the
    -- number of milliseconds after @Jan 1, 1970 00:00:00 UTC@.
    -- @lastEventTimestamp@ updates on an eventual consistency basis. It
    -- typically updates in less than an hour from ingestion, but in rare
    -- situations might take longer.
    orderBy :: Prelude.Maybe OrderBy,
    -- | The name of the log group.
    --
    -- If you specify values for both @logGroupName@ and @logGroupIdentifier@,
    -- the action returns an @InvalidParameterException@ error.
    logGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLogStreams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'descending', 'describeLogStreams_descending' - If the value is true, results are returned in descending order. If the
-- value is to false, results are returned in ascending order. The default
-- value is false.
--
-- 'limit', 'describeLogStreams_limit' - The maximum number of items returned. If you don\'t specify a value, the
-- default is up to 50 items.
--
-- 'logGroupIdentifier', 'describeLogStreams_logGroupIdentifier' - Specify either the name or ARN of the log group to view. If the log
-- group is in a source account and you are using a monitoring account, you
-- must use the log group ARN.
--
-- If you specify values for both @logGroupName@ and @logGroupIdentifier@,
-- the action returns an @InvalidParameterException@ error.
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
-- @lastEventTimestamp@ represents the time of the most recent log event in
-- the log stream in CloudWatch Logs. This number is expressed as the
-- number of milliseconds after @Jan 1, 1970 00:00:00 UTC@.
-- @lastEventTimestamp@ updates on an eventual consistency basis. It
-- typically updates in less than an hour from ingestion, but in rare
-- situations might take longer.
--
-- 'logGroupName', 'describeLogStreams_logGroupName' - The name of the log group.
--
-- If you specify values for both @logGroupName@ and @logGroupIdentifier@,
-- the action returns an @InvalidParameterException@ error.
newDescribeLogStreams ::
  -- | 'logGroupName'
  Prelude.Text ->
  DescribeLogStreams
newDescribeLogStreams pLogGroupName_ =
  DescribeLogStreams'
    { descending = Prelude.Nothing,
      limit = Prelude.Nothing,
      logGroupIdentifier = Prelude.Nothing,
      logStreamNamePrefix = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      orderBy = Prelude.Nothing,
      logGroupName = pLogGroupName_
    }

-- | If the value is true, results are returned in descending order. If the
-- value is to false, results are returned in ascending order. The default
-- value is false.
describeLogStreams_descending :: Lens.Lens' DescribeLogStreams (Prelude.Maybe Prelude.Bool)
describeLogStreams_descending = Lens.lens (\DescribeLogStreams' {descending} -> descending) (\s@DescribeLogStreams' {} a -> s {descending = a} :: DescribeLogStreams)

-- | The maximum number of items returned. If you don\'t specify a value, the
-- default is up to 50 items.
describeLogStreams_limit :: Lens.Lens' DescribeLogStreams (Prelude.Maybe Prelude.Natural)
describeLogStreams_limit = Lens.lens (\DescribeLogStreams' {limit} -> limit) (\s@DescribeLogStreams' {} a -> s {limit = a} :: DescribeLogStreams)

-- | Specify either the name or ARN of the log group to view. If the log
-- group is in a source account and you are using a monitoring account, you
-- must use the log group ARN.
--
-- If you specify values for both @logGroupName@ and @logGroupIdentifier@,
-- the action returns an @InvalidParameterException@ error.
describeLogStreams_logGroupIdentifier :: Lens.Lens' DescribeLogStreams (Prelude.Maybe Prelude.Text)
describeLogStreams_logGroupIdentifier = Lens.lens (\DescribeLogStreams' {logGroupIdentifier} -> logGroupIdentifier) (\s@DescribeLogStreams' {} a -> s {logGroupIdentifier = a} :: DescribeLogStreams)

-- | The prefix to match.
--
-- If @orderBy@ is @LastEventTime@, you cannot specify this parameter.
describeLogStreams_logStreamNamePrefix :: Lens.Lens' DescribeLogStreams (Prelude.Maybe Prelude.Text)
describeLogStreams_logStreamNamePrefix = Lens.lens (\DescribeLogStreams' {logStreamNamePrefix} -> logStreamNamePrefix) (\s@DescribeLogStreams' {} a -> s {logStreamNamePrefix = a} :: DescribeLogStreams)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeLogStreams_nextToken :: Lens.Lens' DescribeLogStreams (Prelude.Maybe Prelude.Text)
describeLogStreams_nextToken = Lens.lens (\DescribeLogStreams' {nextToken} -> nextToken) (\s@DescribeLogStreams' {} a -> s {nextToken = a} :: DescribeLogStreams)

-- | If the value is @LogStreamName@, the results are ordered by log stream
-- name. If the value is @LastEventTime@, the results are ordered by the
-- event time. The default value is @LogStreamName@.
--
-- If you order the results by event time, you cannot specify the
-- @logStreamNamePrefix@ parameter.
--
-- @lastEventTimestamp@ represents the time of the most recent log event in
-- the log stream in CloudWatch Logs. This number is expressed as the
-- number of milliseconds after @Jan 1, 1970 00:00:00 UTC@.
-- @lastEventTimestamp@ updates on an eventual consistency basis. It
-- typically updates in less than an hour from ingestion, but in rare
-- situations might take longer.
describeLogStreams_orderBy :: Lens.Lens' DescribeLogStreams (Prelude.Maybe OrderBy)
describeLogStreams_orderBy = Lens.lens (\DescribeLogStreams' {orderBy} -> orderBy) (\s@DescribeLogStreams' {} a -> s {orderBy = a} :: DescribeLogStreams)

-- | The name of the log group.
--
-- If you specify values for both @logGroupName@ and @logGroupIdentifier@,
-- the action returns an @InvalidParameterException@ error.
describeLogStreams_logGroupName :: Lens.Lens' DescribeLogStreams Prelude.Text
describeLogStreams_logGroupName = Lens.lens (\DescribeLogStreams' {logGroupName} -> logGroupName) (\s@DescribeLogStreams' {} a -> s {logGroupName = a} :: DescribeLogStreams)

instance Core.AWSPager DescribeLogStreams where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeLogStreamsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeLogStreamsResponse_logStreams
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeLogStreams_nextToken
          Lens..~ rs
          Lens.^? describeLogStreamsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeLogStreams where
  type
    AWSResponse DescribeLogStreams =
      DescribeLogStreamsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLogStreamsResponse'
            Prelude.<$> (x Data..?> "logStreams" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLogStreams where
  hashWithSalt _salt DescribeLogStreams' {..} =
    _salt
      `Prelude.hashWithSalt` descending
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` logGroupIdentifier
      `Prelude.hashWithSalt` logStreamNamePrefix
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` orderBy
      `Prelude.hashWithSalt` logGroupName

instance Prelude.NFData DescribeLogStreams where
  rnf DescribeLogStreams' {..} =
    Prelude.rnf descending
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf logGroupIdentifier
      `Prelude.seq` Prelude.rnf logStreamNamePrefix
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf orderBy
      `Prelude.seq` Prelude.rnf logGroupName

instance Data.ToHeaders DescribeLogStreams where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.DescribeLogStreams" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeLogStreams where
  toJSON DescribeLogStreams' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("descending" Data..=) Prelude.<$> descending,
            ("limit" Data..=) Prelude.<$> limit,
            ("logGroupIdentifier" Data..=)
              Prelude.<$> logGroupIdentifier,
            ("logStreamNamePrefix" Data..=)
              Prelude.<$> logStreamNamePrefix,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("orderBy" Data..=) Prelude.<$> orderBy,
            Prelude.Just ("logGroupName" Data..= logGroupName)
          ]
      )

instance Data.ToPath DescribeLogStreams where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLogStreams where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeLogStreamsResponse' smart constructor.
data DescribeLogStreamsResponse = DescribeLogStreamsResponse'
  { -- | The log streams.
    logStreams :: Prelude.Maybe [LogStream],
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLogStreamsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logStreams', 'describeLogStreamsResponse_logStreams' - The log streams.
--
-- 'nextToken', 'describeLogStreamsResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'describeLogStreamsResponse_httpStatus' - The response's http status code.
newDescribeLogStreamsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLogStreamsResponse
newDescribeLogStreamsResponse pHttpStatus_ =
  DescribeLogStreamsResponse'
    { logStreams =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The log streams.
describeLogStreamsResponse_logStreams :: Lens.Lens' DescribeLogStreamsResponse (Prelude.Maybe [LogStream])
describeLogStreamsResponse_logStreams = Lens.lens (\DescribeLogStreamsResponse' {logStreams} -> logStreams) (\s@DescribeLogStreamsResponse' {} a -> s {logStreams = a} :: DescribeLogStreamsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describeLogStreamsResponse_nextToken :: Lens.Lens' DescribeLogStreamsResponse (Prelude.Maybe Prelude.Text)
describeLogStreamsResponse_nextToken = Lens.lens (\DescribeLogStreamsResponse' {nextToken} -> nextToken) (\s@DescribeLogStreamsResponse' {} a -> s {nextToken = a} :: DescribeLogStreamsResponse)

-- | The response's http status code.
describeLogStreamsResponse_httpStatus :: Lens.Lens' DescribeLogStreamsResponse Prelude.Int
describeLogStreamsResponse_httpStatus = Lens.lens (\DescribeLogStreamsResponse' {httpStatus} -> httpStatus) (\s@DescribeLogStreamsResponse' {} a -> s {httpStatus = a} :: DescribeLogStreamsResponse)

instance Prelude.NFData DescribeLogStreamsResponse where
  rnf DescribeLogStreamsResponse' {..} =
    Prelude.rnf logStreams
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
