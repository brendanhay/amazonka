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
-- Module      : Amazonka.AppStream.DescribeUsageReportSubscriptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more usage report subscriptions.
module Amazonka.AppStream.DescribeUsageReportSubscriptions
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
    describeUsageReportSubscriptionsResponse_usageReportSubscriptions,
    describeUsageReportSubscriptionsResponse_nextToken,
    describeUsageReportSubscriptionsResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeUsageReportSubscriptions' smart constructor.
data DescribeUsageReportSubscriptions = DescribeUsageReportSubscriptions'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum size of each page of results.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
describeUsageReportSubscriptions_nextToken :: Lens.Lens' DescribeUsageReportSubscriptions (Prelude.Maybe Prelude.Text)
describeUsageReportSubscriptions_nextToken = Lens.lens (\DescribeUsageReportSubscriptions' {nextToken} -> nextToken) (\s@DescribeUsageReportSubscriptions' {} a -> s {nextToken = a} :: DescribeUsageReportSubscriptions)

-- | The maximum size of each page of results.
describeUsageReportSubscriptions_maxResults :: Lens.Lens' DescribeUsageReportSubscriptions (Prelude.Maybe Prelude.Int)
describeUsageReportSubscriptions_maxResults = Lens.lens (\DescribeUsageReportSubscriptions' {maxResults} -> maxResults) (\s@DescribeUsageReportSubscriptions' {} a -> s {maxResults = a} :: DescribeUsageReportSubscriptions)

instance
  Core.AWSRequest
    DescribeUsageReportSubscriptions
  where
  type
    AWSResponse DescribeUsageReportSubscriptions =
      DescribeUsageReportSubscriptionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUsageReportSubscriptionsResponse'
            Prelude.<$> ( x Data..?> "UsageReportSubscriptions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeUsageReportSubscriptions
  where
  hashWithSalt
    _salt
    DescribeUsageReportSubscriptions' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults

instance
  Prelude.NFData
    DescribeUsageReportSubscriptions
  where
  rnf DescribeUsageReportSubscriptions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance
  Data.ToHeaders
    DescribeUsageReportSubscriptions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.DescribeUsageReportSubscriptions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeUsageReportSubscriptions where
  toJSON DescribeUsageReportSubscriptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath DescribeUsageReportSubscriptions where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeUsageReportSubscriptions
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeUsageReportSubscriptionsResponse' smart constructor.
data DescribeUsageReportSubscriptionsResponse = DescribeUsageReportSubscriptionsResponse'
  { -- | Information about the usage report subscription.
    usageReportSubscriptions :: Prelude.Maybe [UsageReportSubscription],
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUsageReportSubscriptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usageReportSubscriptions', 'describeUsageReportSubscriptionsResponse_usageReportSubscriptions' - Information about the usage report subscription.
--
-- 'nextToken', 'describeUsageReportSubscriptionsResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'httpStatus', 'describeUsageReportSubscriptionsResponse_httpStatus' - The response's http status code.
newDescribeUsageReportSubscriptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeUsageReportSubscriptionsResponse
newDescribeUsageReportSubscriptionsResponse
  pHttpStatus_ =
    DescribeUsageReportSubscriptionsResponse'
      { usageReportSubscriptions =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the usage report subscription.
describeUsageReportSubscriptionsResponse_usageReportSubscriptions :: Lens.Lens' DescribeUsageReportSubscriptionsResponse (Prelude.Maybe [UsageReportSubscription])
describeUsageReportSubscriptionsResponse_usageReportSubscriptions = Lens.lens (\DescribeUsageReportSubscriptionsResponse' {usageReportSubscriptions} -> usageReportSubscriptions) (\s@DescribeUsageReportSubscriptionsResponse' {} a -> s {usageReportSubscriptions = a} :: DescribeUsageReportSubscriptionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
describeUsageReportSubscriptionsResponse_nextToken :: Lens.Lens' DescribeUsageReportSubscriptionsResponse (Prelude.Maybe Prelude.Text)
describeUsageReportSubscriptionsResponse_nextToken = Lens.lens (\DescribeUsageReportSubscriptionsResponse' {nextToken} -> nextToken) (\s@DescribeUsageReportSubscriptionsResponse' {} a -> s {nextToken = a} :: DescribeUsageReportSubscriptionsResponse)

-- | The response's http status code.
describeUsageReportSubscriptionsResponse_httpStatus :: Lens.Lens' DescribeUsageReportSubscriptionsResponse Prelude.Int
describeUsageReportSubscriptionsResponse_httpStatus = Lens.lens (\DescribeUsageReportSubscriptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeUsageReportSubscriptionsResponse' {} a -> s {httpStatus = a} :: DescribeUsageReportSubscriptionsResponse)

instance
  Prelude.NFData
    DescribeUsageReportSubscriptionsResponse
  where
  rnf DescribeUsageReportSubscriptionsResponse' {..} =
    Prelude.rnf usageReportSubscriptions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
