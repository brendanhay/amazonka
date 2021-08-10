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
-- Module      : Network.AWS.CloudWatchLogs.StartQuery
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules a query of a log group using CloudWatch Logs Insights. You
-- specify the log group and time range to query and the query string to
-- use.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax>.
--
-- Queries time out after 15 minutes of execution. If your queries are
-- timing out, reduce the time range being searched or partition your query
-- into a number of queries.
module Network.AWS.CloudWatchLogs.StartQuery
  ( -- * Creating a Request
    StartQuery (..),
    newStartQuery,

    -- * Request Lenses
    startQuery_logGroupNames,
    startQuery_logGroupName,
    startQuery_limit,
    startQuery_startTime,
    startQuery_endTime,
    startQuery_queryString,

    -- * Destructuring the Response
    StartQueryResponse (..),
    newStartQueryResponse,

    -- * Response Lenses
    startQueryResponse_queryId,
    startQueryResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartQuery' smart constructor.
data StartQuery = StartQuery'
  { -- | The list of log groups to be queried. You can include up to 20 log
    -- groups.
    --
    -- A @StartQuery@ operation must include a @logGroupNames@ or a
    -- @logGroupName@ parameter, but not both.
    logGroupNames :: Prelude.Maybe [Prelude.Text],
    -- | The log group on which to perform the query.
    --
    -- A @StartQuery@ operation must include a @logGroupNames@ or a
    -- @logGroupName@ parameter, but not both.
    logGroupName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of log events to return in the query. If the query
    -- string uses the @fields@ command, only the specified fields and their
    -- values are returned. The default is 1000.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The beginning of the time range to query. The range is inclusive, so the
    -- specified start time is included in the query. Specified as epoch time,
    -- the number of seconds since January 1, 1970, 00:00:00 UTC.
    startTime :: Prelude.Natural,
    -- | The end of the time range to query. The range is inclusive, so the
    -- specified end time is included in the query. Specified as epoch time,
    -- the number of seconds since January 1, 1970, 00:00:00 UTC.
    endTime :: Prelude.Natural,
    -- | The query string to use. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax>.
    queryString :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupNames', 'startQuery_logGroupNames' - The list of log groups to be queried. You can include up to 20 log
-- groups.
--
-- A @StartQuery@ operation must include a @logGroupNames@ or a
-- @logGroupName@ parameter, but not both.
--
-- 'logGroupName', 'startQuery_logGroupName' - The log group on which to perform the query.
--
-- A @StartQuery@ operation must include a @logGroupNames@ or a
-- @logGroupName@ parameter, but not both.
--
-- 'limit', 'startQuery_limit' - The maximum number of log events to return in the query. If the query
-- string uses the @fields@ command, only the specified fields and their
-- values are returned. The default is 1000.
--
-- 'startTime', 'startQuery_startTime' - The beginning of the time range to query. The range is inclusive, so the
-- specified start time is included in the query. Specified as epoch time,
-- the number of seconds since January 1, 1970, 00:00:00 UTC.
--
-- 'endTime', 'startQuery_endTime' - The end of the time range to query. The range is inclusive, so the
-- specified end time is included in the query. Specified as epoch time,
-- the number of seconds since January 1, 1970, 00:00:00 UTC.
--
-- 'queryString', 'startQuery_queryString' - The query string to use. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax>.
newStartQuery ::
  -- | 'startTime'
  Prelude.Natural ->
  -- | 'endTime'
  Prelude.Natural ->
  -- | 'queryString'
  Prelude.Text ->
  StartQuery
newStartQuery pStartTime_ pEndTime_ pQueryString_ =
  StartQuery'
    { logGroupNames = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      limit = Prelude.Nothing,
      startTime = pStartTime_,
      endTime = pEndTime_,
      queryString = pQueryString_
    }

-- | The list of log groups to be queried. You can include up to 20 log
-- groups.
--
-- A @StartQuery@ operation must include a @logGroupNames@ or a
-- @logGroupName@ parameter, but not both.
startQuery_logGroupNames :: Lens.Lens' StartQuery (Prelude.Maybe [Prelude.Text])
startQuery_logGroupNames = Lens.lens (\StartQuery' {logGroupNames} -> logGroupNames) (\s@StartQuery' {} a -> s {logGroupNames = a} :: StartQuery) Prelude.. Lens.mapping Lens._Coerce

-- | The log group on which to perform the query.
--
-- A @StartQuery@ operation must include a @logGroupNames@ or a
-- @logGroupName@ parameter, but not both.
startQuery_logGroupName :: Lens.Lens' StartQuery (Prelude.Maybe Prelude.Text)
startQuery_logGroupName = Lens.lens (\StartQuery' {logGroupName} -> logGroupName) (\s@StartQuery' {} a -> s {logGroupName = a} :: StartQuery)

-- | The maximum number of log events to return in the query. If the query
-- string uses the @fields@ command, only the specified fields and their
-- values are returned. The default is 1000.
startQuery_limit :: Lens.Lens' StartQuery (Prelude.Maybe Prelude.Natural)
startQuery_limit = Lens.lens (\StartQuery' {limit} -> limit) (\s@StartQuery' {} a -> s {limit = a} :: StartQuery)

-- | The beginning of the time range to query. The range is inclusive, so the
-- specified start time is included in the query. Specified as epoch time,
-- the number of seconds since January 1, 1970, 00:00:00 UTC.
startQuery_startTime :: Lens.Lens' StartQuery Prelude.Natural
startQuery_startTime = Lens.lens (\StartQuery' {startTime} -> startTime) (\s@StartQuery' {} a -> s {startTime = a} :: StartQuery)

-- | The end of the time range to query. The range is inclusive, so the
-- specified end time is included in the query. Specified as epoch time,
-- the number of seconds since January 1, 1970, 00:00:00 UTC.
startQuery_endTime :: Lens.Lens' StartQuery Prelude.Natural
startQuery_endTime = Lens.lens (\StartQuery' {endTime} -> endTime) (\s@StartQuery' {} a -> s {endTime = a} :: StartQuery)

-- | The query string to use. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax>.
startQuery_queryString :: Lens.Lens' StartQuery Prelude.Text
startQuery_queryString = Lens.lens (\StartQuery' {queryString} -> queryString) (\s@StartQuery' {} a -> s {queryString = a} :: StartQuery)

instance Core.AWSRequest StartQuery where
  type AWSResponse StartQuery = StartQueryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartQueryResponse'
            Prelude.<$> (x Core..?> "queryId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartQuery

instance Prelude.NFData StartQuery

instance Core.ToHeaders StartQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("Logs_20140328.StartQuery" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartQuery where
  toJSON StartQuery' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("logGroupNames" Core..=) Prelude.<$> logGroupNames,
            ("logGroupName" Core..=) Prelude.<$> logGroupName,
            ("limit" Core..=) Prelude.<$> limit,
            Prelude.Just ("startTime" Core..= startTime),
            Prelude.Just ("endTime" Core..= endTime),
            Prelude.Just ("queryString" Core..= queryString)
          ]
      )

instance Core.ToPath StartQuery where
  toPath = Prelude.const "/"

instance Core.ToQuery StartQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartQueryResponse' smart constructor.
data StartQueryResponse = StartQueryResponse'
  { -- | The unique ID of the query.
    queryId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryId', 'startQueryResponse_queryId' - The unique ID of the query.
--
-- 'httpStatus', 'startQueryResponse_httpStatus' - The response's http status code.
newStartQueryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartQueryResponse
newStartQueryResponse pHttpStatus_ =
  StartQueryResponse'
    { queryId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ID of the query.
startQueryResponse_queryId :: Lens.Lens' StartQueryResponse (Prelude.Maybe Prelude.Text)
startQueryResponse_queryId = Lens.lens (\StartQueryResponse' {queryId} -> queryId) (\s@StartQueryResponse' {} a -> s {queryId = a} :: StartQueryResponse)

-- | The response's http status code.
startQueryResponse_httpStatus :: Lens.Lens' StartQueryResponse Prelude.Int
startQueryResponse_httpStatus = Lens.lens (\StartQueryResponse' {httpStatus} -> httpStatus) (\s@StartQueryResponse' {} a -> s {httpStatus = a} :: StartQueryResponse)

instance Prelude.NFData StartQueryResponse
