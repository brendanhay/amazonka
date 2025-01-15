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
-- Module      : Amazonka.CloudWatchLogs.DescribeMetricFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified metric filters. You can list all of the metric
-- filters or filter the results by log name, prefix, metric name, or
-- metric namespace. The results are ASCII-sorted by filter name.
--
-- This operation returns paginated results.
module Amazonka.CloudWatchLogs.DescribeMetricFilters
  ( -- * Creating a Request
    DescribeMetricFilters (..),
    newDescribeMetricFilters,

    -- * Request Lenses
    describeMetricFilters_filterNamePrefix,
    describeMetricFilters_limit,
    describeMetricFilters_logGroupName,
    describeMetricFilters_metricName,
    describeMetricFilters_metricNamespace,
    describeMetricFilters_nextToken,

    -- * Destructuring the Response
    DescribeMetricFiltersResponse (..),
    newDescribeMetricFiltersResponse,

    -- * Response Lenses
    describeMetricFiltersResponse_metricFilters,
    describeMetricFiltersResponse_nextToken,
    describeMetricFiltersResponse_httpStatus,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeMetricFilters' smart constructor.
data DescribeMetricFilters = DescribeMetricFilters'
  { -- | The prefix to match. CloudWatch Logs uses the value that you set here
    -- only if you also include the @logGroupName@ parameter in your request.
    filterNamePrefix :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items returned. If you don\'t specify a value, the
    -- default is up to 50 items.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The name of the log group.
    logGroupName :: Prelude.Maybe Prelude.Text,
    -- | Filters results to include only those with the specified metric name. If
    -- you include this parameter in your request, you must also include the
    -- @metricNamespace@ parameter.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | Filters results to include only those in the specified namespace. If you
    -- include this parameter in your request, you must also include the
    -- @metricName@ parameter.
    metricNamespace :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMetricFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterNamePrefix', 'describeMetricFilters_filterNamePrefix' - The prefix to match. CloudWatch Logs uses the value that you set here
-- only if you also include the @logGroupName@ parameter in your request.
--
-- 'limit', 'describeMetricFilters_limit' - The maximum number of items returned. If you don\'t specify a value, the
-- default is up to 50 items.
--
-- 'logGroupName', 'describeMetricFilters_logGroupName' - The name of the log group.
--
-- 'metricName', 'describeMetricFilters_metricName' - Filters results to include only those with the specified metric name. If
-- you include this parameter in your request, you must also include the
-- @metricNamespace@ parameter.
--
-- 'metricNamespace', 'describeMetricFilters_metricNamespace' - Filters results to include only those in the specified namespace. If you
-- include this parameter in your request, you must also include the
-- @metricName@ parameter.
--
-- 'nextToken', 'describeMetricFilters_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
newDescribeMetricFilters ::
  DescribeMetricFilters
newDescribeMetricFilters =
  DescribeMetricFilters'
    { filterNamePrefix =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      metricName = Prelude.Nothing,
      metricNamespace = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The prefix to match. CloudWatch Logs uses the value that you set here
-- only if you also include the @logGroupName@ parameter in your request.
describeMetricFilters_filterNamePrefix :: Lens.Lens' DescribeMetricFilters (Prelude.Maybe Prelude.Text)
describeMetricFilters_filterNamePrefix = Lens.lens (\DescribeMetricFilters' {filterNamePrefix} -> filterNamePrefix) (\s@DescribeMetricFilters' {} a -> s {filterNamePrefix = a} :: DescribeMetricFilters)

-- | The maximum number of items returned. If you don\'t specify a value, the
-- default is up to 50 items.
describeMetricFilters_limit :: Lens.Lens' DescribeMetricFilters (Prelude.Maybe Prelude.Natural)
describeMetricFilters_limit = Lens.lens (\DescribeMetricFilters' {limit} -> limit) (\s@DescribeMetricFilters' {} a -> s {limit = a} :: DescribeMetricFilters)

-- | The name of the log group.
describeMetricFilters_logGroupName :: Lens.Lens' DescribeMetricFilters (Prelude.Maybe Prelude.Text)
describeMetricFilters_logGroupName = Lens.lens (\DescribeMetricFilters' {logGroupName} -> logGroupName) (\s@DescribeMetricFilters' {} a -> s {logGroupName = a} :: DescribeMetricFilters)

-- | Filters results to include only those with the specified metric name. If
-- you include this parameter in your request, you must also include the
-- @metricNamespace@ parameter.
describeMetricFilters_metricName :: Lens.Lens' DescribeMetricFilters (Prelude.Maybe Prelude.Text)
describeMetricFilters_metricName = Lens.lens (\DescribeMetricFilters' {metricName} -> metricName) (\s@DescribeMetricFilters' {} a -> s {metricName = a} :: DescribeMetricFilters)

-- | Filters results to include only those in the specified namespace. If you
-- include this parameter in your request, you must also include the
-- @metricName@ parameter.
describeMetricFilters_metricNamespace :: Lens.Lens' DescribeMetricFilters (Prelude.Maybe Prelude.Text)
describeMetricFilters_metricNamespace = Lens.lens (\DescribeMetricFilters' {metricNamespace} -> metricNamespace) (\s@DescribeMetricFilters' {} a -> s {metricNamespace = a} :: DescribeMetricFilters)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeMetricFilters_nextToken :: Lens.Lens' DescribeMetricFilters (Prelude.Maybe Prelude.Text)
describeMetricFilters_nextToken = Lens.lens (\DescribeMetricFilters' {nextToken} -> nextToken) (\s@DescribeMetricFilters' {} a -> s {nextToken = a} :: DescribeMetricFilters)

instance Core.AWSPager DescribeMetricFilters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMetricFiltersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMetricFiltersResponse_metricFilters
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeMetricFilters_nextToken
              Lens..~ rs
              Lens.^? describeMetricFiltersResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeMetricFilters where
  type
    AWSResponse DescribeMetricFilters =
      DescribeMetricFiltersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMetricFiltersResponse'
            Prelude.<$> (x Data..?> "metricFilters" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeMetricFilters where
  hashWithSalt _salt DescribeMetricFilters' {..} =
    _salt
      `Prelude.hashWithSalt` filterNamePrefix
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` metricNamespace
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeMetricFilters where
  rnf DescribeMetricFilters' {..} =
    Prelude.rnf filterNamePrefix `Prelude.seq`
      Prelude.rnf limit `Prelude.seq`
        Prelude.rnf logGroupName `Prelude.seq`
          Prelude.rnf metricName `Prelude.seq`
            Prelude.rnf metricNamespace `Prelude.seq`
              Prelude.rnf nextToken

instance Data.ToHeaders DescribeMetricFilters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.DescribeMetricFilters" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeMetricFilters where
  toJSON DescribeMetricFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filterNamePrefix" Data..=)
              Prelude.<$> filterNamePrefix,
            ("limit" Data..=) Prelude.<$> limit,
            ("logGroupName" Data..=) Prelude.<$> logGroupName,
            ("metricName" Data..=) Prelude.<$> metricName,
            ("metricNamespace" Data..=)
              Prelude.<$> metricNamespace,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeMetricFilters where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeMetricFilters where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMetricFiltersResponse' smart constructor.
data DescribeMetricFiltersResponse = DescribeMetricFiltersResponse'
  { -- | The metric filters.
    metricFilters :: Prelude.Maybe [MetricFilter],
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMetricFiltersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricFilters', 'describeMetricFiltersResponse_metricFilters' - The metric filters.
--
-- 'nextToken', 'describeMetricFiltersResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'describeMetricFiltersResponse_httpStatus' - The response's http status code.
newDescribeMetricFiltersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMetricFiltersResponse
newDescribeMetricFiltersResponse pHttpStatus_ =
  DescribeMetricFiltersResponse'
    { metricFilters =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The metric filters.
describeMetricFiltersResponse_metricFilters :: Lens.Lens' DescribeMetricFiltersResponse (Prelude.Maybe [MetricFilter])
describeMetricFiltersResponse_metricFilters = Lens.lens (\DescribeMetricFiltersResponse' {metricFilters} -> metricFilters) (\s@DescribeMetricFiltersResponse' {} a -> s {metricFilters = a} :: DescribeMetricFiltersResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describeMetricFiltersResponse_nextToken :: Lens.Lens' DescribeMetricFiltersResponse (Prelude.Maybe Prelude.Text)
describeMetricFiltersResponse_nextToken = Lens.lens (\DescribeMetricFiltersResponse' {nextToken} -> nextToken) (\s@DescribeMetricFiltersResponse' {} a -> s {nextToken = a} :: DescribeMetricFiltersResponse)

-- | The response's http status code.
describeMetricFiltersResponse_httpStatus :: Lens.Lens' DescribeMetricFiltersResponse Prelude.Int
describeMetricFiltersResponse_httpStatus = Lens.lens (\DescribeMetricFiltersResponse' {httpStatus} -> httpStatus) (\s@DescribeMetricFiltersResponse' {} a -> s {httpStatus = a} :: DescribeMetricFiltersResponse)

instance Prelude.NFData DescribeMetricFiltersResponse where
  rnf DescribeMetricFiltersResponse' {..} =
    Prelude.rnf metricFilters `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
