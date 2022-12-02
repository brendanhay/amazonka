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
-- Module      : Amazonka.CloudWatchLogs.StartQuery
-- Copyright   : (c) 2013-2022 Brendan Hay
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
--
-- You are limited to 20 concurrent CloudWatch Logs insights queries,
-- including queries that have been added to dashboards.
module Amazonka.CloudWatchLogs.StartQuery
  ( -- * Creating a Request
    StartQuery (..),
    newStartQuery,

    -- * Request Lenses
    startQuery_limit,
    startQuery_logGroupNames,
    startQuery_logGroupName,
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

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartQuery' smart constructor.
data StartQuery = StartQuery'
  { -- | The maximum number of log events to return in the query. If the query
    -- string uses the @fields@ command, only the specified fields and their
    -- values are returned. The default is 1000.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The list of log groups to be queried. You can include up to 20 log
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
-- 'limit', 'startQuery_limit' - The maximum number of log events to return in the query. If the query
-- string uses the @fields@ command, only the specified fields and their
-- values are returned. The default is 1000.
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
    { limit = Prelude.Nothing,
      logGroupNames = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      startTime = pStartTime_,
      endTime = pEndTime_,
      queryString = pQueryString_
    }

-- | The maximum number of log events to return in the query. If the query
-- string uses the @fields@ command, only the specified fields and their
-- values are returned. The default is 1000.
startQuery_limit :: Lens.Lens' StartQuery (Prelude.Maybe Prelude.Natural)
startQuery_limit = Lens.lens (\StartQuery' {limit} -> limit) (\s@StartQuery' {} a -> s {limit = a} :: StartQuery)

-- | The list of log groups to be queried. You can include up to 20 log
-- groups.
--
-- A @StartQuery@ operation must include a @logGroupNames@ or a
-- @logGroupName@ parameter, but not both.
startQuery_logGroupNames :: Lens.Lens' StartQuery (Prelude.Maybe [Prelude.Text])
startQuery_logGroupNames = Lens.lens (\StartQuery' {logGroupNames} -> logGroupNames) (\s@StartQuery' {} a -> s {logGroupNames = a} :: StartQuery) Prelude.. Lens.mapping Lens.coerced

-- | The log group on which to perform the query.
--
-- A @StartQuery@ operation must include a @logGroupNames@ or a
-- @logGroupName@ parameter, but not both.
startQuery_logGroupName :: Lens.Lens' StartQuery (Prelude.Maybe Prelude.Text)
startQuery_logGroupName = Lens.lens (\StartQuery' {logGroupName} -> logGroupName) (\s@StartQuery' {} a -> s {logGroupName = a} :: StartQuery)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartQueryResponse'
            Prelude.<$> (x Data..?> "queryId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartQuery where
  hashWithSalt _salt StartQuery' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` logGroupNames
      `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` queryString

instance Prelude.NFData StartQuery where
  rnf StartQuery' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf logGroupNames
      `Prelude.seq` Prelude.rnf logGroupName
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf queryString

instance Data.ToHeaders StartQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("Logs_20140328.StartQuery" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartQuery where
  toJSON StartQuery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("limit" Data..=) Prelude.<$> limit,
            ("logGroupNames" Data..=) Prelude.<$> logGroupNames,
            ("logGroupName" Data..=) Prelude.<$> logGroupName,
            Prelude.Just ("startTime" Data..= startTime),
            Prelude.Just ("endTime" Data..= endTime),
            Prelude.Just ("queryString" Data..= queryString)
          ]
      )

instance Data.ToPath StartQuery where
  toPath = Prelude.const "/"

instance Data.ToQuery StartQuery where
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

instance Prelude.NFData StartQueryResponse where
  rnf StartQueryResponse' {..} =
    Prelude.rnf queryId
      `Prelude.seq` Prelude.rnf httpStatus
