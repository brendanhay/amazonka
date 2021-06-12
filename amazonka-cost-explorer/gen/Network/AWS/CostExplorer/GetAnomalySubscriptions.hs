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
-- Module      : Network.AWS.CostExplorer.GetAnomalySubscriptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the cost anomaly subscription objects for your account. You
-- can filter using a list of cost anomaly monitor Amazon Resource Names
-- (ARNs).
module Network.AWS.CostExplorer.GetAnomalySubscriptions
  ( -- * Creating a Request
    GetAnomalySubscriptions (..),
    newGetAnomalySubscriptions,

    -- * Request Lenses
    getAnomalySubscriptions_maxResults,
    getAnomalySubscriptions_nextPageToken,
    getAnomalySubscriptions_subscriptionArnList,
    getAnomalySubscriptions_monitorArn,

    -- * Destructuring the Response
    GetAnomalySubscriptionsResponse (..),
    newGetAnomalySubscriptionsResponse,

    -- * Response Lenses
    getAnomalySubscriptionsResponse_nextPageToken,
    getAnomalySubscriptionsResponse_httpStatus,
    getAnomalySubscriptionsResponse_anomalySubscriptions,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAnomalySubscriptions' smart constructor.
data GetAnomalySubscriptions = GetAnomalySubscriptions'
  { -- | The number of entries a paginated response contains.
    maxResults :: Core.Maybe Core.Int,
    -- | The token to retrieve the next set of results. AWS provides the token
    -- when the response from a previous call has more results than the maximum
    -- page size.
    nextPageToken :: Core.Maybe Core.Text,
    -- | A list of cost anomaly subscription ARNs.
    subscriptionArnList :: Core.Maybe [Core.Text],
    -- | Cost anomaly monitor ARNs.
    monitorArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAnomalySubscriptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getAnomalySubscriptions_maxResults' - The number of entries a paginated response contains.
--
-- 'nextPageToken', 'getAnomalySubscriptions_nextPageToken' - The token to retrieve the next set of results. AWS provides the token
-- when the response from a previous call has more results than the maximum
-- page size.
--
-- 'subscriptionArnList', 'getAnomalySubscriptions_subscriptionArnList' - A list of cost anomaly subscription ARNs.
--
-- 'monitorArn', 'getAnomalySubscriptions_monitorArn' - Cost anomaly monitor ARNs.
newGetAnomalySubscriptions ::
  GetAnomalySubscriptions
newGetAnomalySubscriptions =
  GetAnomalySubscriptions'
    { maxResults = Core.Nothing,
      nextPageToken = Core.Nothing,
      subscriptionArnList = Core.Nothing,
      monitorArn = Core.Nothing
    }

-- | The number of entries a paginated response contains.
getAnomalySubscriptions_maxResults :: Lens.Lens' GetAnomalySubscriptions (Core.Maybe Core.Int)
getAnomalySubscriptions_maxResults = Lens.lens (\GetAnomalySubscriptions' {maxResults} -> maxResults) (\s@GetAnomalySubscriptions' {} a -> s {maxResults = a} :: GetAnomalySubscriptions)

-- | The token to retrieve the next set of results. AWS provides the token
-- when the response from a previous call has more results than the maximum
-- page size.
getAnomalySubscriptions_nextPageToken :: Lens.Lens' GetAnomalySubscriptions (Core.Maybe Core.Text)
getAnomalySubscriptions_nextPageToken = Lens.lens (\GetAnomalySubscriptions' {nextPageToken} -> nextPageToken) (\s@GetAnomalySubscriptions' {} a -> s {nextPageToken = a} :: GetAnomalySubscriptions)

-- | A list of cost anomaly subscription ARNs.
getAnomalySubscriptions_subscriptionArnList :: Lens.Lens' GetAnomalySubscriptions (Core.Maybe [Core.Text])
getAnomalySubscriptions_subscriptionArnList = Lens.lens (\GetAnomalySubscriptions' {subscriptionArnList} -> subscriptionArnList) (\s@GetAnomalySubscriptions' {} a -> s {subscriptionArnList = a} :: GetAnomalySubscriptions) Core.. Lens.mapping Lens._Coerce

-- | Cost anomaly monitor ARNs.
getAnomalySubscriptions_monitorArn :: Lens.Lens' GetAnomalySubscriptions (Core.Maybe Core.Text)
getAnomalySubscriptions_monitorArn = Lens.lens (\GetAnomalySubscriptions' {monitorArn} -> monitorArn) (\s@GetAnomalySubscriptions' {} a -> s {monitorArn = a} :: GetAnomalySubscriptions)

instance Core.AWSRequest GetAnomalySubscriptions where
  type
    AWSResponse GetAnomalySubscriptions =
      GetAnomalySubscriptionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAnomalySubscriptionsResponse'
            Core.<$> (x Core..?> "NextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "AnomalySubscriptions"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable GetAnomalySubscriptions

instance Core.NFData GetAnomalySubscriptions

instance Core.ToHeaders GetAnomalySubscriptions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.GetAnomalySubscriptions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetAnomalySubscriptions where
  toJSON GetAnomalySubscriptions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextPageToken" Core..=) Core.<$> nextPageToken,
            ("SubscriptionArnList" Core..=)
              Core.<$> subscriptionArnList,
            ("MonitorArn" Core..=) Core.<$> monitorArn
          ]
      )

instance Core.ToPath GetAnomalySubscriptions where
  toPath = Core.const "/"

instance Core.ToQuery GetAnomalySubscriptions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAnomalySubscriptionsResponse' smart constructor.
data GetAnomalySubscriptionsResponse = GetAnomalySubscriptionsResponse'
  { -- | The token to retrieve the next set of results. AWS provides the token
    -- when the response from a previous call has more results than the maximum
    -- page size.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of cost anomaly subscriptions that includes the detailed metadata
    -- for each one.
    anomalySubscriptions :: [AnomalySubscription]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAnomalySubscriptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getAnomalySubscriptionsResponse_nextPageToken' - The token to retrieve the next set of results. AWS provides the token
-- when the response from a previous call has more results than the maximum
-- page size.
--
-- 'httpStatus', 'getAnomalySubscriptionsResponse_httpStatus' - The response's http status code.
--
-- 'anomalySubscriptions', 'getAnomalySubscriptionsResponse_anomalySubscriptions' - A list of cost anomaly subscriptions that includes the detailed metadata
-- for each one.
newGetAnomalySubscriptionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAnomalySubscriptionsResponse
newGetAnomalySubscriptionsResponse pHttpStatus_ =
  GetAnomalySubscriptionsResponse'
    { nextPageToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      anomalySubscriptions = Core.mempty
    }

-- | The token to retrieve the next set of results. AWS provides the token
-- when the response from a previous call has more results than the maximum
-- page size.
getAnomalySubscriptionsResponse_nextPageToken :: Lens.Lens' GetAnomalySubscriptionsResponse (Core.Maybe Core.Text)
getAnomalySubscriptionsResponse_nextPageToken = Lens.lens (\GetAnomalySubscriptionsResponse' {nextPageToken} -> nextPageToken) (\s@GetAnomalySubscriptionsResponse' {} a -> s {nextPageToken = a} :: GetAnomalySubscriptionsResponse)

-- | The response's http status code.
getAnomalySubscriptionsResponse_httpStatus :: Lens.Lens' GetAnomalySubscriptionsResponse Core.Int
getAnomalySubscriptionsResponse_httpStatus = Lens.lens (\GetAnomalySubscriptionsResponse' {httpStatus} -> httpStatus) (\s@GetAnomalySubscriptionsResponse' {} a -> s {httpStatus = a} :: GetAnomalySubscriptionsResponse)

-- | A list of cost anomaly subscriptions that includes the detailed metadata
-- for each one.
getAnomalySubscriptionsResponse_anomalySubscriptions :: Lens.Lens' GetAnomalySubscriptionsResponse [AnomalySubscription]
getAnomalySubscriptionsResponse_anomalySubscriptions = Lens.lens (\GetAnomalySubscriptionsResponse' {anomalySubscriptions} -> anomalySubscriptions) (\s@GetAnomalySubscriptionsResponse' {} a -> s {anomalySubscriptions = a} :: GetAnomalySubscriptionsResponse) Core.. Lens._Coerce

instance Core.NFData GetAnomalySubscriptionsResponse
