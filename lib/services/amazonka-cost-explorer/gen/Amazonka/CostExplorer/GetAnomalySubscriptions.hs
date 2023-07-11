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
-- Module      : Amazonka.CostExplorer.GetAnomalySubscriptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the cost anomaly subscription objects for your account. You
-- can filter using a list of cost anomaly monitor Amazon Resource Names
-- (ARNs).
module Amazonka.CostExplorer.GetAnomalySubscriptions
  ( -- * Creating a Request
    GetAnomalySubscriptions (..),
    newGetAnomalySubscriptions,

    -- * Request Lenses
    getAnomalySubscriptions_maxResults,
    getAnomalySubscriptions_monitorArn,
    getAnomalySubscriptions_nextPageToken,
    getAnomalySubscriptions_subscriptionArnList,

    -- * Destructuring the Response
    GetAnomalySubscriptionsResponse (..),
    newGetAnomalySubscriptionsResponse,

    -- * Response Lenses
    getAnomalySubscriptionsResponse_nextPageToken,
    getAnomalySubscriptionsResponse_httpStatus,
    getAnomalySubscriptionsResponse_anomalySubscriptions,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAnomalySubscriptions' smart constructor.
data GetAnomalySubscriptions = GetAnomalySubscriptions'
  { -- | The number of entries a paginated response contains.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Cost anomaly monitor ARNs.
    monitorArn :: Prelude.Maybe Prelude.Text,
    -- | The token to retrieve the next set of results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | A list of cost anomaly subscription ARNs.
    subscriptionArnList :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'monitorArn', 'getAnomalySubscriptions_monitorArn' - Cost anomaly monitor ARNs.
--
-- 'nextPageToken', 'getAnomalySubscriptions_nextPageToken' - The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
--
-- 'subscriptionArnList', 'getAnomalySubscriptions_subscriptionArnList' - A list of cost anomaly subscription ARNs.
newGetAnomalySubscriptions ::
  GetAnomalySubscriptions
newGetAnomalySubscriptions =
  GetAnomalySubscriptions'
    { maxResults =
        Prelude.Nothing,
      monitorArn = Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      subscriptionArnList = Prelude.Nothing
    }

-- | The number of entries a paginated response contains.
getAnomalySubscriptions_maxResults :: Lens.Lens' GetAnomalySubscriptions (Prelude.Maybe Prelude.Int)
getAnomalySubscriptions_maxResults = Lens.lens (\GetAnomalySubscriptions' {maxResults} -> maxResults) (\s@GetAnomalySubscriptions' {} a -> s {maxResults = a} :: GetAnomalySubscriptions)

-- | Cost anomaly monitor ARNs.
getAnomalySubscriptions_monitorArn :: Lens.Lens' GetAnomalySubscriptions (Prelude.Maybe Prelude.Text)
getAnomalySubscriptions_monitorArn = Lens.lens (\GetAnomalySubscriptions' {monitorArn} -> monitorArn) (\s@GetAnomalySubscriptions' {} a -> s {monitorArn = a} :: GetAnomalySubscriptions)

-- | The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
getAnomalySubscriptions_nextPageToken :: Lens.Lens' GetAnomalySubscriptions (Prelude.Maybe Prelude.Text)
getAnomalySubscriptions_nextPageToken = Lens.lens (\GetAnomalySubscriptions' {nextPageToken} -> nextPageToken) (\s@GetAnomalySubscriptions' {} a -> s {nextPageToken = a} :: GetAnomalySubscriptions)

-- | A list of cost anomaly subscription ARNs.
getAnomalySubscriptions_subscriptionArnList :: Lens.Lens' GetAnomalySubscriptions (Prelude.Maybe [Prelude.Text])
getAnomalySubscriptions_subscriptionArnList = Lens.lens (\GetAnomalySubscriptions' {subscriptionArnList} -> subscriptionArnList) (\s@GetAnomalySubscriptions' {} a -> s {subscriptionArnList = a} :: GetAnomalySubscriptions) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest GetAnomalySubscriptions where
  type
    AWSResponse GetAnomalySubscriptions =
      GetAnomalySubscriptionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAnomalySubscriptionsResponse'
            Prelude.<$> (x Data..?> "NextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "AnomalySubscriptions"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable GetAnomalySubscriptions where
  hashWithSalt _salt GetAnomalySubscriptions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` monitorArn
      `Prelude.hashWithSalt` nextPageToken
      `Prelude.hashWithSalt` subscriptionArnList

instance Prelude.NFData GetAnomalySubscriptions where
  rnf GetAnomalySubscriptions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf monitorArn
      `Prelude.seq` Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf subscriptionArnList

instance Data.ToHeaders GetAnomalySubscriptions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSInsightsIndexService.GetAnomalySubscriptions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetAnomalySubscriptions where
  toJSON GetAnomalySubscriptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("MonitorArn" Data..=) Prelude.<$> monitorArn,
            ("NextPageToken" Data..=) Prelude.<$> nextPageToken,
            ("SubscriptionArnList" Data..=)
              Prelude.<$> subscriptionArnList
          ]
      )

instance Data.ToPath GetAnomalySubscriptions where
  toPath = Prelude.const "/"

instance Data.ToQuery GetAnomalySubscriptions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAnomalySubscriptionsResponse' smart constructor.
data GetAnomalySubscriptionsResponse = GetAnomalySubscriptionsResponse'
  { -- | The token to retrieve the next set of results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of cost anomaly subscriptions that includes the detailed metadata
    -- for each one.
    anomalySubscriptions :: [AnomalySubscription]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAnomalySubscriptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getAnomalySubscriptionsResponse_nextPageToken' - The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
--
-- 'httpStatus', 'getAnomalySubscriptionsResponse_httpStatus' - The response's http status code.
--
-- 'anomalySubscriptions', 'getAnomalySubscriptionsResponse_anomalySubscriptions' - A list of cost anomaly subscriptions that includes the detailed metadata
-- for each one.
newGetAnomalySubscriptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAnomalySubscriptionsResponse
newGetAnomalySubscriptionsResponse pHttpStatus_ =
  GetAnomalySubscriptionsResponse'
    { nextPageToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      anomalySubscriptions = Prelude.mempty
    }

-- | The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
getAnomalySubscriptionsResponse_nextPageToken :: Lens.Lens' GetAnomalySubscriptionsResponse (Prelude.Maybe Prelude.Text)
getAnomalySubscriptionsResponse_nextPageToken = Lens.lens (\GetAnomalySubscriptionsResponse' {nextPageToken} -> nextPageToken) (\s@GetAnomalySubscriptionsResponse' {} a -> s {nextPageToken = a} :: GetAnomalySubscriptionsResponse)

-- | The response's http status code.
getAnomalySubscriptionsResponse_httpStatus :: Lens.Lens' GetAnomalySubscriptionsResponse Prelude.Int
getAnomalySubscriptionsResponse_httpStatus = Lens.lens (\GetAnomalySubscriptionsResponse' {httpStatus} -> httpStatus) (\s@GetAnomalySubscriptionsResponse' {} a -> s {httpStatus = a} :: GetAnomalySubscriptionsResponse)

-- | A list of cost anomaly subscriptions that includes the detailed metadata
-- for each one.
getAnomalySubscriptionsResponse_anomalySubscriptions :: Lens.Lens' GetAnomalySubscriptionsResponse [AnomalySubscription]
getAnomalySubscriptionsResponse_anomalySubscriptions = Lens.lens (\GetAnomalySubscriptionsResponse' {anomalySubscriptions} -> anomalySubscriptions) (\s@GetAnomalySubscriptionsResponse' {} a -> s {anomalySubscriptions = a} :: GetAnomalySubscriptionsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    GetAnomalySubscriptionsResponse
  where
  rnf GetAnomalySubscriptionsResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf anomalySubscriptions
