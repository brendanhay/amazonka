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
-- Module      : Amazonka.EC2.DescribeAwsNetworkPerformanceMetricSubscriptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the current Infrastructure Performance metric subscriptions.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeAwsNetworkPerformanceMetricSubscriptions
  ( -- * Creating a Request
    DescribeAwsNetworkPerformanceMetricSubscriptions (..),
    newDescribeAwsNetworkPerformanceMetricSubscriptions,

    -- * Request Lenses
    describeAwsNetworkPerformanceMetricSubscriptions_dryRun,
    describeAwsNetworkPerformanceMetricSubscriptions_filters,
    describeAwsNetworkPerformanceMetricSubscriptions_maxResults,
    describeAwsNetworkPerformanceMetricSubscriptions_nextToken,

    -- * Destructuring the Response
    DescribeAwsNetworkPerformanceMetricSubscriptionsResponse (..),
    newDescribeAwsNetworkPerformanceMetricSubscriptionsResponse,

    -- * Response Lenses
    describeAwsNetworkPerformanceMetricSubscriptionsResponse_nextToken,
    describeAwsNetworkPerformanceMetricSubscriptionsResponse_subscriptions,
    describeAwsNetworkPerformanceMetricSubscriptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAwsNetworkPerformanceMetricSubscriptions' smart constructor.
data DescribeAwsNetworkPerformanceMetricSubscriptions = DescribeAwsNetworkPerformanceMetricSubscriptions'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAwsNetworkPerformanceMetricSubscriptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeAwsNetworkPerformanceMetricSubscriptions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeAwsNetworkPerformanceMetricSubscriptions_filters' - One or more filters.
--
-- 'maxResults', 'describeAwsNetworkPerformanceMetricSubscriptions_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeAwsNetworkPerformanceMetricSubscriptions_nextToken' - The token for the next page of results.
newDescribeAwsNetworkPerformanceMetricSubscriptions ::
  DescribeAwsNetworkPerformanceMetricSubscriptions
newDescribeAwsNetworkPerformanceMetricSubscriptions =
  DescribeAwsNetworkPerformanceMetricSubscriptions'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults =
        Prelude.Nothing,
      nextToken =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeAwsNetworkPerformanceMetricSubscriptions_dryRun :: Lens.Lens' DescribeAwsNetworkPerformanceMetricSubscriptions (Prelude.Maybe Prelude.Bool)
describeAwsNetworkPerformanceMetricSubscriptions_dryRun = Lens.lens (\DescribeAwsNetworkPerformanceMetricSubscriptions' {dryRun} -> dryRun) (\s@DescribeAwsNetworkPerformanceMetricSubscriptions' {} a -> s {dryRun = a} :: DescribeAwsNetworkPerformanceMetricSubscriptions)

-- | One or more filters.
describeAwsNetworkPerformanceMetricSubscriptions_filters :: Lens.Lens' DescribeAwsNetworkPerformanceMetricSubscriptions (Prelude.Maybe [Filter])
describeAwsNetworkPerformanceMetricSubscriptions_filters = Lens.lens (\DescribeAwsNetworkPerformanceMetricSubscriptions' {filters} -> filters) (\s@DescribeAwsNetworkPerformanceMetricSubscriptions' {} a -> s {filters = a} :: DescribeAwsNetworkPerformanceMetricSubscriptions) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeAwsNetworkPerformanceMetricSubscriptions_maxResults :: Lens.Lens' DescribeAwsNetworkPerformanceMetricSubscriptions (Prelude.Maybe Prelude.Natural)
describeAwsNetworkPerformanceMetricSubscriptions_maxResults = Lens.lens (\DescribeAwsNetworkPerformanceMetricSubscriptions' {maxResults} -> maxResults) (\s@DescribeAwsNetworkPerformanceMetricSubscriptions' {} a -> s {maxResults = a} :: DescribeAwsNetworkPerformanceMetricSubscriptions)

-- | The token for the next page of results.
describeAwsNetworkPerformanceMetricSubscriptions_nextToken :: Lens.Lens' DescribeAwsNetworkPerformanceMetricSubscriptions (Prelude.Maybe Prelude.Text)
describeAwsNetworkPerformanceMetricSubscriptions_nextToken = Lens.lens (\DescribeAwsNetworkPerformanceMetricSubscriptions' {nextToken} -> nextToken) (\s@DescribeAwsNetworkPerformanceMetricSubscriptions' {} a -> s {nextToken = a} :: DescribeAwsNetworkPerformanceMetricSubscriptions)

instance
  Core.AWSPager
    DescribeAwsNetworkPerformanceMetricSubscriptions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAwsNetworkPerformanceMetricSubscriptionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAwsNetworkPerformanceMetricSubscriptionsResponse_subscriptions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeAwsNetworkPerformanceMetricSubscriptions_nextToken
          Lens..~ rs
            Lens.^? describeAwsNetworkPerformanceMetricSubscriptionsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeAwsNetworkPerformanceMetricSubscriptions
  where
  type
    AWSResponse
      DescribeAwsNetworkPerformanceMetricSubscriptions =
      DescribeAwsNetworkPerformanceMetricSubscriptionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeAwsNetworkPerformanceMetricSubscriptionsResponse'
            Prelude.<$> (x Data..@? "nextToken")
              Prelude.<*> ( x Data..@? "subscriptionSet" Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Data.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAwsNetworkPerformanceMetricSubscriptions
  where
  hashWithSalt
    _salt
    DescribeAwsNetworkPerformanceMetricSubscriptions' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeAwsNetworkPerformanceMetricSubscriptions
  where
  rnf
    DescribeAwsNetworkPerformanceMetricSubscriptions' {..} =
      Prelude.rnf dryRun
        `Prelude.seq` Prelude.rnf filters
        `Prelude.seq` Prelude.rnf maxResults
        `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeAwsNetworkPerformanceMetricSubscriptions
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeAwsNetworkPerformanceMetricSubscriptions
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeAwsNetworkPerformanceMetricSubscriptions
  where
  toQuery
    DescribeAwsNetworkPerformanceMetricSubscriptions' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "DescribeAwsNetworkPerformanceMetricSubscriptions" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Data.=: dryRun,
          Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
          "MaxResults" Data.=: maxResults,
          "NextToken" Data.=: nextToken
        ]

-- | /See:/ 'newDescribeAwsNetworkPerformanceMetricSubscriptionsResponse' smart constructor.
data DescribeAwsNetworkPerformanceMetricSubscriptionsResponse = DescribeAwsNetworkPerformanceMetricSubscriptionsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Describes the current Infrastructure Performance subscriptions.
    subscriptions :: Prelude.Maybe [Subscription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAwsNetworkPerformanceMetricSubscriptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAwsNetworkPerformanceMetricSubscriptionsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'subscriptions', 'describeAwsNetworkPerformanceMetricSubscriptionsResponse_subscriptions' - Describes the current Infrastructure Performance subscriptions.
--
-- 'httpStatus', 'describeAwsNetworkPerformanceMetricSubscriptionsResponse_httpStatus' - The response's http status code.
newDescribeAwsNetworkPerformanceMetricSubscriptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAwsNetworkPerformanceMetricSubscriptionsResponse
newDescribeAwsNetworkPerformanceMetricSubscriptionsResponse
  pHttpStatus_ =
    DescribeAwsNetworkPerformanceMetricSubscriptionsResponse'
      { nextToken =
          Prelude.Nothing,
        subscriptions =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeAwsNetworkPerformanceMetricSubscriptionsResponse_nextToken :: Lens.Lens' DescribeAwsNetworkPerformanceMetricSubscriptionsResponse (Prelude.Maybe Prelude.Text)
describeAwsNetworkPerformanceMetricSubscriptionsResponse_nextToken = Lens.lens (\DescribeAwsNetworkPerformanceMetricSubscriptionsResponse' {nextToken} -> nextToken) (\s@DescribeAwsNetworkPerformanceMetricSubscriptionsResponse' {} a -> s {nextToken = a} :: DescribeAwsNetworkPerformanceMetricSubscriptionsResponse)

-- | Describes the current Infrastructure Performance subscriptions.
describeAwsNetworkPerformanceMetricSubscriptionsResponse_subscriptions :: Lens.Lens' DescribeAwsNetworkPerformanceMetricSubscriptionsResponse (Prelude.Maybe [Subscription])
describeAwsNetworkPerformanceMetricSubscriptionsResponse_subscriptions = Lens.lens (\DescribeAwsNetworkPerformanceMetricSubscriptionsResponse' {subscriptions} -> subscriptions) (\s@DescribeAwsNetworkPerformanceMetricSubscriptionsResponse' {} a -> s {subscriptions = a} :: DescribeAwsNetworkPerformanceMetricSubscriptionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAwsNetworkPerformanceMetricSubscriptionsResponse_httpStatus :: Lens.Lens' DescribeAwsNetworkPerformanceMetricSubscriptionsResponse Prelude.Int
describeAwsNetworkPerformanceMetricSubscriptionsResponse_httpStatus = Lens.lens (\DescribeAwsNetworkPerformanceMetricSubscriptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeAwsNetworkPerformanceMetricSubscriptionsResponse' {} a -> s {httpStatus = a} :: DescribeAwsNetworkPerformanceMetricSubscriptionsResponse)

instance
  Prelude.NFData
    DescribeAwsNetworkPerformanceMetricSubscriptionsResponse
  where
  rnf
    DescribeAwsNetworkPerformanceMetricSubscriptionsResponse' {..} =
      Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf subscriptions
        `Prelude.seq` Prelude.rnf httpStatus
