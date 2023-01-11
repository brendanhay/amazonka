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
-- Module      : Amazonka.CloudWatchLogs.DescribeSubscriptionFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.CloudWatchLogs.DescribeSubscriptionFilters
  ( -- * Creating a Request
    DescribeSubscriptionFilters (..),
    newDescribeSubscriptionFilters,

    -- * Request Lenses
    describeSubscriptionFilters_filterNamePrefix,
    describeSubscriptionFilters_limit,
    describeSubscriptionFilters_nextToken,
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

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSubscriptionFilters' smart constructor.
data DescribeSubscriptionFilters = DescribeSubscriptionFilters'
  { -- | The prefix to match. If you don\'t specify a value, no prefix filter is
    -- applied.
    filterNamePrefix :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items returned. If you don\'t specify a value, the
    -- default is up to 50 items.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the log group.
    logGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'limit', 'describeSubscriptionFilters_limit' - The maximum number of items returned. If you don\'t specify a value, the
-- default is up to 50 items.
--
-- 'nextToken', 'describeSubscriptionFilters_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'logGroupName', 'describeSubscriptionFilters_logGroupName' - The name of the log group.
newDescribeSubscriptionFilters ::
  -- | 'logGroupName'
  Prelude.Text ->
  DescribeSubscriptionFilters
newDescribeSubscriptionFilters pLogGroupName_ =
  DescribeSubscriptionFilters'
    { filterNamePrefix =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      logGroupName = pLogGroupName_
    }

-- | The prefix to match. If you don\'t specify a value, no prefix filter is
-- applied.
describeSubscriptionFilters_filterNamePrefix :: Lens.Lens' DescribeSubscriptionFilters (Prelude.Maybe Prelude.Text)
describeSubscriptionFilters_filterNamePrefix = Lens.lens (\DescribeSubscriptionFilters' {filterNamePrefix} -> filterNamePrefix) (\s@DescribeSubscriptionFilters' {} a -> s {filterNamePrefix = a} :: DescribeSubscriptionFilters)

-- | The maximum number of items returned. If you don\'t specify a value, the
-- default is up to 50 items.
describeSubscriptionFilters_limit :: Lens.Lens' DescribeSubscriptionFilters (Prelude.Maybe Prelude.Natural)
describeSubscriptionFilters_limit = Lens.lens (\DescribeSubscriptionFilters' {limit} -> limit) (\s@DescribeSubscriptionFilters' {} a -> s {limit = a} :: DescribeSubscriptionFilters)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeSubscriptionFilters_nextToken :: Lens.Lens' DescribeSubscriptionFilters (Prelude.Maybe Prelude.Text)
describeSubscriptionFilters_nextToken = Lens.lens (\DescribeSubscriptionFilters' {nextToken} -> nextToken) (\s@DescribeSubscriptionFilters' {} a -> s {nextToken = a} :: DescribeSubscriptionFilters)

-- | The name of the log group.
describeSubscriptionFilters_logGroupName :: Lens.Lens' DescribeSubscriptionFilters Prelude.Text
describeSubscriptionFilters_logGroupName = Lens.lens (\DescribeSubscriptionFilters' {logGroupName} -> logGroupName) (\s@DescribeSubscriptionFilters' {} a -> s {logGroupName = a} :: DescribeSubscriptionFilters)

instance Core.AWSPager DescribeSubscriptionFilters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSubscriptionFiltersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSubscriptionFiltersResponse_subscriptionFilters
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeSubscriptionFilters_nextToken
          Lens..~ rs
          Lens.^? describeSubscriptionFiltersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeSubscriptionFilters where
  type
    AWSResponse DescribeSubscriptionFilters =
      DescribeSubscriptionFiltersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSubscriptionFiltersResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "subscriptionFilters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSubscriptionFilters where
  hashWithSalt _salt DescribeSubscriptionFilters' {..} =
    _salt `Prelude.hashWithSalt` filterNamePrefix
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` logGroupName

instance Prelude.NFData DescribeSubscriptionFilters where
  rnf DescribeSubscriptionFilters' {..} =
    Prelude.rnf filterNamePrefix
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf logGroupName

instance Data.ToHeaders DescribeSubscriptionFilters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.DescribeSubscriptionFilters" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSubscriptionFilters where
  toJSON DescribeSubscriptionFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filterNamePrefix" Data..=)
              Prelude.<$> filterNamePrefix,
            ("limit" Data..=) Prelude.<$> limit,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("logGroupName" Data..= logGroupName)
          ]
      )

instance Data.ToPath DescribeSubscriptionFilters where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSubscriptionFilters where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSubscriptionFiltersResponse' smart constructor.
data DescribeSubscriptionFiltersResponse = DescribeSubscriptionFiltersResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | The subscription filters.
    subscriptionFilters :: Prelude.Maybe [SubscriptionFilter],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeSubscriptionFiltersResponse
newDescribeSubscriptionFiltersResponse pHttpStatus_ =
  DescribeSubscriptionFiltersResponse'
    { nextToken =
        Prelude.Nothing,
      subscriptionFilters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeSubscriptionFiltersResponse_nextToken :: Lens.Lens' DescribeSubscriptionFiltersResponse (Prelude.Maybe Prelude.Text)
describeSubscriptionFiltersResponse_nextToken = Lens.lens (\DescribeSubscriptionFiltersResponse' {nextToken} -> nextToken) (\s@DescribeSubscriptionFiltersResponse' {} a -> s {nextToken = a} :: DescribeSubscriptionFiltersResponse)

-- | The subscription filters.
describeSubscriptionFiltersResponse_subscriptionFilters :: Lens.Lens' DescribeSubscriptionFiltersResponse (Prelude.Maybe [SubscriptionFilter])
describeSubscriptionFiltersResponse_subscriptionFilters = Lens.lens (\DescribeSubscriptionFiltersResponse' {subscriptionFilters} -> subscriptionFilters) (\s@DescribeSubscriptionFiltersResponse' {} a -> s {subscriptionFilters = a} :: DescribeSubscriptionFiltersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSubscriptionFiltersResponse_httpStatus :: Lens.Lens' DescribeSubscriptionFiltersResponse Prelude.Int
describeSubscriptionFiltersResponse_httpStatus = Lens.lens (\DescribeSubscriptionFiltersResponse' {httpStatus} -> httpStatus) (\s@DescribeSubscriptionFiltersResponse' {} a -> s {httpStatus = a} :: DescribeSubscriptionFiltersResponse)

instance
  Prelude.NFData
    DescribeSubscriptionFiltersResponse
  where
  rnf DescribeSubscriptionFiltersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf subscriptionFilters
      `Prelude.seq` Prelude.rnf httpStatus
