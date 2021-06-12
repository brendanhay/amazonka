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
-- Module      : Network.AWS.CloudWatchLogs.DescribeMetricFilters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified metric filters. You can list all of the metric
-- filters or filter the results by log name, prefix, metric name, or
-- metric namespace. The results are ASCII-sorted by filter name.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeMetricFilters
  ( -- * Creating a Request
    DescribeMetricFilters (..),
    newDescribeMetricFilters,

    -- * Request Lenses
    describeMetricFilters_filterNamePrefix,
    describeMetricFilters_metricNamespace,
    describeMetricFilters_nextToken,
    describeMetricFilters_metricName,
    describeMetricFilters_logGroupName,
    describeMetricFilters_limit,

    -- * Destructuring the Response
    DescribeMetricFiltersResponse (..),
    newDescribeMetricFiltersResponse,

    -- * Response Lenses
    describeMetricFiltersResponse_metricFilters,
    describeMetricFiltersResponse_nextToken,
    describeMetricFiltersResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeMetricFilters' smart constructor.
data DescribeMetricFilters = DescribeMetricFilters'
  { -- | The prefix to match. CloudWatch Logs uses the value you set here only if
    -- you also include the @logGroupName@ parameter in your request.
    filterNamePrefix :: Core.Maybe Core.Text,
    -- | Filters results to include only those in the specified namespace. If you
    -- include this parameter in your request, you must also include the
    -- @metricName@ parameter.
    metricNamespace :: Core.Maybe Core.Text,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | Filters results to include only those with the specified metric name. If
    -- you include this parameter in your request, you must also include the
    -- @metricNamespace@ parameter.
    metricName :: Core.Maybe Core.Text,
    -- | The name of the log group.
    logGroupName :: Core.Maybe Core.Text,
    -- | The maximum number of items returned. If you don\'t specify a value, the
    -- default is up to 50 items.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMetricFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterNamePrefix', 'describeMetricFilters_filterNamePrefix' - The prefix to match. CloudWatch Logs uses the value you set here only if
-- you also include the @logGroupName@ parameter in your request.
--
-- 'metricNamespace', 'describeMetricFilters_metricNamespace' - Filters results to include only those in the specified namespace. If you
-- include this parameter in your request, you must also include the
-- @metricName@ parameter.
--
-- 'nextToken', 'describeMetricFilters_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'metricName', 'describeMetricFilters_metricName' - Filters results to include only those with the specified metric name. If
-- you include this parameter in your request, you must also include the
-- @metricNamespace@ parameter.
--
-- 'logGroupName', 'describeMetricFilters_logGroupName' - The name of the log group.
--
-- 'limit', 'describeMetricFilters_limit' - The maximum number of items returned. If you don\'t specify a value, the
-- default is up to 50 items.
newDescribeMetricFilters ::
  DescribeMetricFilters
newDescribeMetricFilters =
  DescribeMetricFilters'
    { filterNamePrefix =
        Core.Nothing,
      metricNamespace = Core.Nothing,
      nextToken = Core.Nothing,
      metricName = Core.Nothing,
      logGroupName = Core.Nothing,
      limit = Core.Nothing
    }

-- | The prefix to match. CloudWatch Logs uses the value you set here only if
-- you also include the @logGroupName@ parameter in your request.
describeMetricFilters_filterNamePrefix :: Lens.Lens' DescribeMetricFilters (Core.Maybe Core.Text)
describeMetricFilters_filterNamePrefix = Lens.lens (\DescribeMetricFilters' {filterNamePrefix} -> filterNamePrefix) (\s@DescribeMetricFilters' {} a -> s {filterNamePrefix = a} :: DescribeMetricFilters)

-- | Filters results to include only those in the specified namespace. If you
-- include this parameter in your request, you must also include the
-- @metricName@ parameter.
describeMetricFilters_metricNamespace :: Lens.Lens' DescribeMetricFilters (Core.Maybe Core.Text)
describeMetricFilters_metricNamespace = Lens.lens (\DescribeMetricFilters' {metricNamespace} -> metricNamespace) (\s@DescribeMetricFilters' {} a -> s {metricNamespace = a} :: DescribeMetricFilters)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeMetricFilters_nextToken :: Lens.Lens' DescribeMetricFilters (Core.Maybe Core.Text)
describeMetricFilters_nextToken = Lens.lens (\DescribeMetricFilters' {nextToken} -> nextToken) (\s@DescribeMetricFilters' {} a -> s {nextToken = a} :: DescribeMetricFilters)

-- | Filters results to include only those with the specified metric name. If
-- you include this parameter in your request, you must also include the
-- @metricNamespace@ parameter.
describeMetricFilters_metricName :: Lens.Lens' DescribeMetricFilters (Core.Maybe Core.Text)
describeMetricFilters_metricName = Lens.lens (\DescribeMetricFilters' {metricName} -> metricName) (\s@DescribeMetricFilters' {} a -> s {metricName = a} :: DescribeMetricFilters)

-- | The name of the log group.
describeMetricFilters_logGroupName :: Lens.Lens' DescribeMetricFilters (Core.Maybe Core.Text)
describeMetricFilters_logGroupName = Lens.lens (\DescribeMetricFilters' {logGroupName} -> logGroupName) (\s@DescribeMetricFilters' {} a -> s {logGroupName = a} :: DescribeMetricFilters)

-- | The maximum number of items returned. If you don\'t specify a value, the
-- default is up to 50 items.
describeMetricFilters_limit :: Lens.Lens' DescribeMetricFilters (Core.Maybe Core.Natural)
describeMetricFilters_limit = Lens.lens (\DescribeMetricFilters' {limit} -> limit) (\s@DescribeMetricFilters' {} a -> s {limit = a} :: DescribeMetricFilters)

instance Core.AWSPager DescribeMetricFilters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMetricFiltersResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMetricFiltersResponse_metricFilters
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeMetricFilters_nextToken
          Lens..~ rs
          Lens.^? describeMetricFiltersResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeMetricFilters where
  type
    AWSResponse DescribeMetricFilters =
      DescribeMetricFiltersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMetricFiltersResponse'
            Core.<$> (x Core..?> "metricFilters" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeMetricFilters

instance Core.NFData DescribeMetricFilters

instance Core.ToHeaders DescribeMetricFilters where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.DescribeMetricFilters" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeMetricFilters where
  toJSON DescribeMetricFilters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("filterNamePrefix" Core..=)
              Core.<$> filterNamePrefix,
            ("metricNamespace" Core..=) Core.<$> metricNamespace,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("metricName" Core..=) Core.<$> metricName,
            ("logGroupName" Core..=) Core.<$> logGroupName,
            ("limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath DescribeMetricFilters where
  toPath = Core.const "/"

instance Core.ToQuery DescribeMetricFilters where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeMetricFiltersResponse' smart constructor.
data DescribeMetricFiltersResponse = DescribeMetricFiltersResponse'
  { -- | The metric filters.
    metricFilters :: Core.Maybe [MetricFilter],
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeMetricFiltersResponse
newDescribeMetricFiltersResponse pHttpStatus_ =
  DescribeMetricFiltersResponse'
    { metricFilters =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The metric filters.
describeMetricFiltersResponse_metricFilters :: Lens.Lens' DescribeMetricFiltersResponse (Core.Maybe [MetricFilter])
describeMetricFiltersResponse_metricFilters = Lens.lens (\DescribeMetricFiltersResponse' {metricFilters} -> metricFilters) (\s@DescribeMetricFiltersResponse' {} a -> s {metricFilters = a} :: DescribeMetricFiltersResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
describeMetricFiltersResponse_nextToken :: Lens.Lens' DescribeMetricFiltersResponse (Core.Maybe Core.Text)
describeMetricFiltersResponse_nextToken = Lens.lens (\DescribeMetricFiltersResponse' {nextToken} -> nextToken) (\s@DescribeMetricFiltersResponse' {} a -> s {nextToken = a} :: DescribeMetricFiltersResponse)

-- | The response's http status code.
describeMetricFiltersResponse_httpStatus :: Lens.Lens' DescribeMetricFiltersResponse Core.Int
describeMetricFiltersResponse_httpStatus = Lens.lens (\DescribeMetricFiltersResponse' {httpStatus} -> httpStatus) (\s@DescribeMetricFiltersResponse' {} a -> s {httpStatus = a} :: DescribeMetricFiltersResponse)

instance Core.NFData DescribeMetricFiltersResponse
