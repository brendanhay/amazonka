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
-- Module      : Network.AWS.CloudWatchLogs.DescribeSubscriptionFilters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the subscription filters for the specified log group. You can list
-- all the subscription filters or filter the results by prefix. The
-- results are ASCII-sorted by filter name.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeSubscriptionFilters
  ( -- * Creating a Request
    DescribeSubscriptionFilters (..),
    newDescribeSubscriptionFilters,

    -- * Request Lenses
    describeSubscriptionFilters_filterNamePrefix,
    describeSubscriptionFilters_nextToken,
    describeSubscriptionFilters_limit,
    describeSubscriptionFilters_logGroupName,

    -- * Destructuring the Response
    DescribeSubscriptionFiltersResponse (..),
    newDescribeSubscriptionFiltersResponse,

    -- * Response Lenses
    describeSubscriptionFiltersResponse_nextToken,
    describeSubscriptionFiltersResponse_subscriptionFilters,
    describeSubscriptionFiltersResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeSubscriptionFilters' smart constructor.
data DescribeSubscriptionFilters = DescribeSubscriptionFilters'
  { -- | The prefix to match. If you don\'t specify a value, no prefix filter is
    -- applied.
    filterNamePrefix :: Core.Maybe Core.Text,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items returned. If you don\'t specify a value, the
    -- default is up to 50 items.
    limit :: Core.Maybe Core.Natural,
    -- | The name of the log group.
    logGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSubscriptionFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterNamePrefix', 'describeSubscriptionFilters_filterNamePrefix' - The prefix to match. If you don\'t specify a value, no prefix filter is
-- applied.
--
-- 'nextToken', 'describeSubscriptionFilters_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'limit', 'describeSubscriptionFilters_limit' - The maximum number of items returned. If you don\'t specify a value, the
-- default is up to 50 items.
--
-- 'logGroupName', 'describeSubscriptionFilters_logGroupName' - The name of the log group.
newDescribeSubscriptionFilters ::
  -- | 'logGroupName'
  Core.Text ->
  DescribeSubscriptionFilters
newDescribeSubscriptionFilters pLogGroupName_ =
  DescribeSubscriptionFilters'
    { filterNamePrefix =
        Core.Nothing,
      nextToken = Core.Nothing,
      limit = Core.Nothing,
      logGroupName = pLogGroupName_
    }

-- | The prefix to match. If you don\'t specify a value, no prefix filter is
-- applied.
describeSubscriptionFilters_filterNamePrefix :: Lens.Lens' DescribeSubscriptionFilters (Core.Maybe Core.Text)
describeSubscriptionFilters_filterNamePrefix = Lens.lens (\DescribeSubscriptionFilters' {filterNamePrefix} -> filterNamePrefix) (\s@DescribeSubscriptionFilters' {} a -> s {filterNamePrefix = a} :: DescribeSubscriptionFilters)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeSubscriptionFilters_nextToken :: Lens.Lens' DescribeSubscriptionFilters (Core.Maybe Core.Text)
describeSubscriptionFilters_nextToken = Lens.lens (\DescribeSubscriptionFilters' {nextToken} -> nextToken) (\s@DescribeSubscriptionFilters' {} a -> s {nextToken = a} :: DescribeSubscriptionFilters)

-- | The maximum number of items returned. If you don\'t specify a value, the
-- default is up to 50 items.
describeSubscriptionFilters_limit :: Lens.Lens' DescribeSubscriptionFilters (Core.Maybe Core.Natural)
describeSubscriptionFilters_limit = Lens.lens (\DescribeSubscriptionFilters' {limit} -> limit) (\s@DescribeSubscriptionFilters' {} a -> s {limit = a} :: DescribeSubscriptionFilters)

-- | The name of the log group.
describeSubscriptionFilters_logGroupName :: Lens.Lens' DescribeSubscriptionFilters Core.Text
describeSubscriptionFilters_logGroupName = Lens.lens (\DescribeSubscriptionFilters' {logGroupName} -> logGroupName) (\s@DescribeSubscriptionFilters' {} a -> s {logGroupName = a} :: DescribeSubscriptionFilters)

instance Core.AWSPager DescribeSubscriptionFilters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSubscriptionFiltersResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSubscriptionFiltersResponse_subscriptionFilters
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeSubscriptionFilters_nextToken
          Lens..~ rs
          Lens.^? describeSubscriptionFiltersResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeSubscriptionFilters where
  type
    AWSResponse DescribeSubscriptionFilters =
      DescribeSubscriptionFiltersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSubscriptionFiltersResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> ( x Core..?> "subscriptionFilters"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeSubscriptionFilters

instance Core.NFData DescribeSubscriptionFilters

instance Core.ToHeaders DescribeSubscriptionFilters where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.DescribeSubscriptionFilters" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeSubscriptionFilters where
  toJSON DescribeSubscriptionFilters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("filterNamePrefix" Core..=)
              Core.<$> filterNamePrefix,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("limit" Core..=) Core.<$> limit,
            Core.Just ("logGroupName" Core..= logGroupName)
          ]
      )

instance Core.ToPath DescribeSubscriptionFilters where
  toPath = Core.const "/"

instance Core.ToQuery DescribeSubscriptionFilters where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeSubscriptionFiltersResponse' smart constructor.
data DescribeSubscriptionFiltersResponse = DescribeSubscriptionFiltersResponse'
  { nextToken :: Core.Maybe Core.Text,
    -- | The subscription filters.
    subscriptionFilters :: Core.Maybe [SubscriptionFilter],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSubscriptionFiltersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSubscriptionFiltersResponse_nextToken' - Undocumented member.
--
-- 'subscriptionFilters', 'describeSubscriptionFiltersResponse_subscriptionFilters' - The subscription filters.
--
-- 'httpStatus', 'describeSubscriptionFiltersResponse_httpStatus' - The response's http status code.
newDescribeSubscriptionFiltersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeSubscriptionFiltersResponse
newDescribeSubscriptionFiltersResponse pHttpStatus_ =
  DescribeSubscriptionFiltersResponse'
    { nextToken =
        Core.Nothing,
      subscriptionFilters = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeSubscriptionFiltersResponse_nextToken :: Lens.Lens' DescribeSubscriptionFiltersResponse (Core.Maybe Core.Text)
describeSubscriptionFiltersResponse_nextToken = Lens.lens (\DescribeSubscriptionFiltersResponse' {nextToken} -> nextToken) (\s@DescribeSubscriptionFiltersResponse' {} a -> s {nextToken = a} :: DescribeSubscriptionFiltersResponse)

-- | The subscription filters.
describeSubscriptionFiltersResponse_subscriptionFilters :: Lens.Lens' DescribeSubscriptionFiltersResponse (Core.Maybe [SubscriptionFilter])
describeSubscriptionFiltersResponse_subscriptionFilters = Lens.lens (\DescribeSubscriptionFiltersResponse' {subscriptionFilters} -> subscriptionFilters) (\s@DescribeSubscriptionFiltersResponse' {} a -> s {subscriptionFilters = a} :: DescribeSubscriptionFiltersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeSubscriptionFiltersResponse_httpStatus :: Lens.Lens' DescribeSubscriptionFiltersResponse Core.Int
describeSubscriptionFiltersResponse_httpStatus = Lens.lens (\DescribeSubscriptionFiltersResponse' {httpStatus} -> httpStatus) (\s@DescribeSubscriptionFiltersResponse' {} a -> s {httpStatus = a} :: DescribeSubscriptionFiltersResponse)

instance
  Core.NFData
    DescribeSubscriptionFiltersResponse
