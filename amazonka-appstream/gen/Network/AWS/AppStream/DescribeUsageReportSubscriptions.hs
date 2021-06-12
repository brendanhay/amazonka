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
-- Module      : Network.AWS.AppStream.DescribeUsageReportSubscriptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more usage report subscriptions.
module Network.AWS.AppStream.DescribeUsageReportSubscriptions
  ( -- * Creating a Request
    DescribeUsageReportSubscriptions (..),
    newDescribeUsageReportSubscriptions,

    -- * Request Lenses
    describeUsageReportSubscriptions_nextToken,
    describeUsageReportSubscriptions_maxResults,

    -- * Destructuring the Response
    DescribeUsageReportSubscriptionsResponse (..),
    newDescribeUsageReportSubscriptionsResponse,

    -- * Response Lenses
    describeUsageReportSubscriptionsResponse_nextToken,
    describeUsageReportSubscriptionsResponse_usageReportSubscriptions,
    describeUsageReportSubscriptionsResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeUsageReportSubscriptions' smart constructor.
data DescribeUsageReportSubscriptions = DescribeUsageReportSubscriptions'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum size of each page of results.
    maxResults :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeUsageReportSubscriptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeUsageReportSubscriptions_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'maxResults', 'describeUsageReportSubscriptions_maxResults' - The maximum size of each page of results.
newDescribeUsageReportSubscriptions ::
  DescribeUsageReportSubscriptions
newDescribeUsageReportSubscriptions =
  DescribeUsageReportSubscriptions'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
describeUsageReportSubscriptions_nextToken :: Lens.Lens' DescribeUsageReportSubscriptions (Core.Maybe Core.Text)
describeUsageReportSubscriptions_nextToken = Lens.lens (\DescribeUsageReportSubscriptions' {nextToken} -> nextToken) (\s@DescribeUsageReportSubscriptions' {} a -> s {nextToken = a} :: DescribeUsageReportSubscriptions)

-- | The maximum size of each page of results.
describeUsageReportSubscriptions_maxResults :: Lens.Lens' DescribeUsageReportSubscriptions (Core.Maybe Core.Int)
describeUsageReportSubscriptions_maxResults = Lens.lens (\DescribeUsageReportSubscriptions' {maxResults} -> maxResults) (\s@DescribeUsageReportSubscriptions' {} a -> s {maxResults = a} :: DescribeUsageReportSubscriptions)

instance
  Core.AWSRequest
    DescribeUsageReportSubscriptions
  where
  type
    AWSResponse DescribeUsageReportSubscriptions =
      DescribeUsageReportSubscriptionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUsageReportSubscriptionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "UsageReportSubscriptions"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeUsageReportSubscriptions

instance Core.NFData DescribeUsageReportSubscriptions

instance
  Core.ToHeaders
    DescribeUsageReportSubscriptions
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.DescribeUsageReportSubscriptions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeUsageReportSubscriptions where
  toJSON DescribeUsageReportSubscriptions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath DescribeUsageReportSubscriptions where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeUsageReportSubscriptions
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeUsageReportSubscriptionsResponse' smart constructor.
data DescribeUsageReportSubscriptionsResponse = DescribeUsageReportSubscriptionsResponse'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the usage report subscription.
    usageReportSubscriptions :: Core.Maybe [UsageReportSubscription],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeUsageReportSubscriptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeUsageReportSubscriptionsResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'usageReportSubscriptions', 'describeUsageReportSubscriptionsResponse_usageReportSubscriptions' - Information about the usage report subscription.
--
-- 'httpStatus', 'describeUsageReportSubscriptionsResponse_httpStatus' - The response's http status code.
newDescribeUsageReportSubscriptionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeUsageReportSubscriptionsResponse
newDescribeUsageReportSubscriptionsResponse
  pHttpStatus_ =
    DescribeUsageReportSubscriptionsResponse'
      { nextToken =
          Core.Nothing,
        usageReportSubscriptions =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
describeUsageReportSubscriptionsResponse_nextToken :: Lens.Lens' DescribeUsageReportSubscriptionsResponse (Core.Maybe Core.Text)
describeUsageReportSubscriptionsResponse_nextToken = Lens.lens (\DescribeUsageReportSubscriptionsResponse' {nextToken} -> nextToken) (\s@DescribeUsageReportSubscriptionsResponse' {} a -> s {nextToken = a} :: DescribeUsageReportSubscriptionsResponse)

-- | Information about the usage report subscription.
describeUsageReportSubscriptionsResponse_usageReportSubscriptions :: Lens.Lens' DescribeUsageReportSubscriptionsResponse (Core.Maybe [UsageReportSubscription])
describeUsageReportSubscriptionsResponse_usageReportSubscriptions = Lens.lens (\DescribeUsageReportSubscriptionsResponse' {usageReportSubscriptions} -> usageReportSubscriptions) (\s@DescribeUsageReportSubscriptionsResponse' {} a -> s {usageReportSubscriptions = a} :: DescribeUsageReportSubscriptionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeUsageReportSubscriptionsResponse_httpStatus :: Lens.Lens' DescribeUsageReportSubscriptionsResponse Core.Int
describeUsageReportSubscriptionsResponse_httpStatus = Lens.lens (\DescribeUsageReportSubscriptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeUsageReportSubscriptionsResponse' {} a -> s {httpStatus = a} :: DescribeUsageReportSubscriptionsResponse)

instance
  Core.NFData
    DescribeUsageReportSubscriptionsResponse
