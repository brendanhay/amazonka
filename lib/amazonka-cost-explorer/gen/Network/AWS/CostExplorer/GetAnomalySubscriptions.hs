{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetAnomalySubscriptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the cost anomaly subscription objects for your account. You can filter using a list of cost anomaly monitor Amazon Resource Names (ARNs).
module Network.AWS.CostExplorer.GetAnomalySubscriptions
  ( -- * Creating a request
    GetAnomalySubscriptions (..),
    mkGetAnomalySubscriptions,

    -- ** Request lenses
    gasSubscriptionARNList,
    gasNextPageToken,
    gasMaxResults,
    gasMonitorARN,

    -- * Destructuring the response
    GetAnomalySubscriptionsResponse (..),
    mkGetAnomalySubscriptionsResponse,

    -- ** Response lenses
    gasrsNextPageToken,
    gasrsResponseStatus,
    gasrsAnomalySubscriptions,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAnomalySubscriptions' smart constructor.
data GetAnomalySubscriptions = GetAnomalySubscriptions'
  { subscriptionARNList ::
      Lude.Maybe [Lude.Text],
    nextPageToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int,
    monitorARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAnomalySubscriptions' with the minimum fields required to make a request.
--
-- * 'maxResults' - The number of entries a paginated response contains.
-- * 'monitorARN' - Cost anomaly monitor ARNs.
-- * 'nextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
-- * 'subscriptionARNList' - A list of cost anomaly subscription ARNs.
mkGetAnomalySubscriptions ::
  GetAnomalySubscriptions
mkGetAnomalySubscriptions =
  GetAnomalySubscriptions'
    { subscriptionARNList = Lude.Nothing,
      nextPageToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      monitorARN = Lude.Nothing
    }

-- | A list of cost anomaly subscription ARNs.
--
-- /Note:/ Consider using 'subscriptionARNList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasSubscriptionARNList :: Lens.Lens' GetAnomalySubscriptions (Lude.Maybe [Lude.Text])
gasSubscriptionARNList = Lens.lens (subscriptionARNList :: GetAnomalySubscriptions -> Lude.Maybe [Lude.Text]) (\s a -> s {subscriptionARNList = a} :: GetAnomalySubscriptions)
{-# DEPRECATED gasSubscriptionARNList "Use generic-lens or generic-optics with 'subscriptionARNList' instead." #-}

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasNextPageToken :: Lens.Lens' GetAnomalySubscriptions (Lude.Maybe Lude.Text)
gasNextPageToken = Lens.lens (nextPageToken :: GetAnomalySubscriptions -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetAnomalySubscriptions)
{-# DEPRECATED gasNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The number of entries a paginated response contains.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasMaxResults :: Lens.Lens' GetAnomalySubscriptions (Lude.Maybe Lude.Int)
gasMaxResults = Lens.lens (maxResults :: GetAnomalySubscriptions -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: GetAnomalySubscriptions)
{-# DEPRECATED gasMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Cost anomaly monitor ARNs.
--
-- /Note:/ Consider using 'monitorARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasMonitorARN :: Lens.Lens' GetAnomalySubscriptions (Lude.Maybe Lude.Text)
gasMonitorARN = Lens.lens (monitorARN :: GetAnomalySubscriptions -> Lude.Maybe Lude.Text) (\s a -> s {monitorARN = a} :: GetAnomalySubscriptions)
{-# DEPRECATED gasMonitorARN "Use generic-lens or generic-optics with 'monitorARN' instead." #-}

instance Lude.AWSRequest GetAnomalySubscriptions where
  type Rs GetAnomalySubscriptions = GetAnomalySubscriptionsResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAnomalySubscriptionsResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "AnomalySubscriptions" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders GetAnomalySubscriptions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSInsightsIndexService.GetAnomalySubscriptions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAnomalySubscriptions where
  toJSON GetAnomalySubscriptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SubscriptionArnList" Lude..=) Lude.<$> subscriptionARNList,
            ("NextPageToken" Lude..=) Lude.<$> nextPageToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("MonitorArn" Lude..=) Lude.<$> monitorARN
          ]
      )

instance Lude.ToPath GetAnomalySubscriptions where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAnomalySubscriptions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAnomalySubscriptionsResponse' smart constructor.
data GetAnomalySubscriptionsResponse = GetAnomalySubscriptionsResponse'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    anomalySubscriptions ::
      [AnomalySubscription]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAnomalySubscriptionsResponse' with the minimum fields required to make a request.
--
-- * 'anomalySubscriptions' - A list of cost anomaly subscriptions that includes the detailed metadata for each one.
-- * 'nextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
-- * 'responseStatus' - The response status code.
mkGetAnomalySubscriptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAnomalySubscriptionsResponse
mkGetAnomalySubscriptionsResponse pResponseStatus_ =
  GetAnomalySubscriptionsResponse'
    { nextPageToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      anomalySubscriptions = Lude.mempty
    }

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrsNextPageToken :: Lens.Lens' GetAnomalySubscriptionsResponse (Lude.Maybe Lude.Text)
gasrsNextPageToken = Lens.lens (nextPageToken :: GetAnomalySubscriptionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetAnomalySubscriptionsResponse)
{-# DEPRECATED gasrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrsResponseStatus :: Lens.Lens' GetAnomalySubscriptionsResponse Lude.Int
gasrsResponseStatus = Lens.lens (responseStatus :: GetAnomalySubscriptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAnomalySubscriptionsResponse)
{-# DEPRECATED gasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of cost anomaly subscriptions that includes the detailed metadata for each one.
--
-- /Note:/ Consider using 'anomalySubscriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrsAnomalySubscriptions :: Lens.Lens' GetAnomalySubscriptionsResponse [AnomalySubscription]
gasrsAnomalySubscriptions = Lens.lens (anomalySubscriptions :: GetAnomalySubscriptionsResponse -> [AnomalySubscription]) (\s a -> s {anomalySubscriptions = a} :: GetAnomalySubscriptionsResponse)
{-# DEPRECATED gasrsAnomalySubscriptions "Use generic-lens or generic-optics with 'anomalySubscriptions' instead." #-}
