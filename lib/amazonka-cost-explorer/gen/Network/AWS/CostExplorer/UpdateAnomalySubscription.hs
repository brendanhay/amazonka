{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.UpdateAnomalySubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing cost anomaly monitor subscription.
module Network.AWS.CostExplorer.UpdateAnomalySubscription
  ( -- * Creating a request
    UpdateAnomalySubscription (..),
    mkUpdateAnomalySubscription,

    -- ** Request lenses
    uasSubscriptionName,
    uasFrequency,
    uasThreshold,
    uasMonitorARNList,
    uasSubscribers,
    uasSubscriptionARN,

    -- * Destructuring the response
    UpdateAnomalySubscriptionResponse (..),
    mkUpdateAnomalySubscriptionResponse,

    -- ** Response lenses
    uasrsResponseStatus,
    uasrsSubscriptionARN,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateAnomalySubscription' smart constructor.
data UpdateAnomalySubscription = UpdateAnomalySubscription'
  { subscriptionName ::
      Lude.Maybe Lude.Text,
    frequency ::
      Lude.Maybe AnomalySubscriptionFrequency,
    threshold :: Lude.Maybe Lude.Double,
    monitorARNList ::
      Lude.Maybe [Lude.Text],
    subscribers :: Lude.Maybe [Subscriber],
    subscriptionARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAnomalySubscription' with the minimum fields required to make a request.
--
-- * 'frequency' - The update to the frequency value at which subscribers will receive notifications.
-- * 'monitorARNList' - A list of cost anomaly subscription ARNs.
-- * 'subscribers' - The update to the subscriber list.
-- * 'subscriptionARN' - A cost anomaly subscription Amazon Resource Name (ARN).
-- * 'subscriptionName' - The subscription's new name.
-- * 'threshold' - The update to the threshold value for receiving notifications.
mkUpdateAnomalySubscription ::
  -- | 'subscriptionARN'
  Lude.Text ->
  UpdateAnomalySubscription
mkUpdateAnomalySubscription pSubscriptionARN_ =
  UpdateAnomalySubscription'
    { subscriptionName = Lude.Nothing,
      frequency = Lude.Nothing,
      threshold = Lude.Nothing,
      monitorARNList = Lude.Nothing,
      subscribers = Lude.Nothing,
      subscriptionARN = pSubscriptionARN_
    }

-- | The subscription's new name.
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasSubscriptionName :: Lens.Lens' UpdateAnomalySubscription (Lude.Maybe Lude.Text)
uasSubscriptionName = Lens.lens (subscriptionName :: UpdateAnomalySubscription -> Lude.Maybe Lude.Text) (\s a -> s {subscriptionName = a} :: UpdateAnomalySubscription)
{-# DEPRECATED uasSubscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead." #-}

-- | The update to the frequency value at which subscribers will receive notifications.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasFrequency :: Lens.Lens' UpdateAnomalySubscription (Lude.Maybe AnomalySubscriptionFrequency)
uasFrequency = Lens.lens (frequency :: UpdateAnomalySubscription -> Lude.Maybe AnomalySubscriptionFrequency) (\s a -> s {frequency = a} :: UpdateAnomalySubscription)
{-# DEPRECATED uasFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

-- | The update to the threshold value for receiving notifications.
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasThreshold :: Lens.Lens' UpdateAnomalySubscription (Lude.Maybe Lude.Double)
uasThreshold = Lens.lens (threshold :: UpdateAnomalySubscription -> Lude.Maybe Lude.Double) (\s a -> s {threshold = a} :: UpdateAnomalySubscription)
{-# DEPRECATED uasThreshold "Use generic-lens or generic-optics with 'threshold' instead." #-}

-- | A list of cost anomaly subscription ARNs.
--
-- /Note:/ Consider using 'monitorARNList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasMonitorARNList :: Lens.Lens' UpdateAnomalySubscription (Lude.Maybe [Lude.Text])
uasMonitorARNList = Lens.lens (monitorARNList :: UpdateAnomalySubscription -> Lude.Maybe [Lude.Text]) (\s a -> s {monitorARNList = a} :: UpdateAnomalySubscription)
{-# DEPRECATED uasMonitorARNList "Use generic-lens or generic-optics with 'monitorARNList' instead." #-}

-- | The update to the subscriber list.
--
-- /Note:/ Consider using 'subscribers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasSubscribers :: Lens.Lens' UpdateAnomalySubscription (Lude.Maybe [Subscriber])
uasSubscribers = Lens.lens (subscribers :: UpdateAnomalySubscription -> Lude.Maybe [Subscriber]) (\s a -> s {subscribers = a} :: UpdateAnomalySubscription)
{-# DEPRECATED uasSubscribers "Use generic-lens or generic-optics with 'subscribers' instead." #-}

-- | A cost anomaly subscription Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'subscriptionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasSubscriptionARN :: Lens.Lens' UpdateAnomalySubscription Lude.Text
uasSubscriptionARN = Lens.lens (subscriptionARN :: UpdateAnomalySubscription -> Lude.Text) (\s a -> s {subscriptionARN = a} :: UpdateAnomalySubscription)
{-# DEPRECATED uasSubscriptionARN "Use generic-lens or generic-optics with 'subscriptionARN' instead." #-}

instance Lude.AWSRequest UpdateAnomalySubscription where
  type
    Rs UpdateAnomalySubscription =
      UpdateAnomalySubscriptionResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateAnomalySubscriptionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "SubscriptionArn")
      )

instance Lude.ToHeaders UpdateAnomalySubscription where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSInsightsIndexService.UpdateAnomalySubscription" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateAnomalySubscription where
  toJSON UpdateAnomalySubscription' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SubscriptionName" Lude..=) Lude.<$> subscriptionName,
            ("Frequency" Lude..=) Lude.<$> frequency,
            ("Threshold" Lude..=) Lude.<$> threshold,
            ("MonitorArnList" Lude..=) Lude.<$> monitorARNList,
            ("Subscribers" Lude..=) Lude.<$> subscribers,
            Lude.Just ("SubscriptionArn" Lude..= subscriptionARN)
          ]
      )

instance Lude.ToPath UpdateAnomalySubscription where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateAnomalySubscription where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateAnomalySubscriptionResponse' smart constructor.
data UpdateAnomalySubscriptionResponse = UpdateAnomalySubscriptionResponse'
  { responseStatus ::
      Lude.Int,
    subscriptionARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAnomalySubscriptionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'subscriptionARN' - A cost anomaly subscription ARN.
mkUpdateAnomalySubscriptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'subscriptionARN'
  Lude.Text ->
  UpdateAnomalySubscriptionResponse
mkUpdateAnomalySubscriptionResponse
  pResponseStatus_
  pSubscriptionARN_ =
    UpdateAnomalySubscriptionResponse'
      { responseStatus =
          pResponseStatus_,
        subscriptionARN = pSubscriptionARN_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrsResponseStatus :: Lens.Lens' UpdateAnomalySubscriptionResponse Lude.Int
uasrsResponseStatus = Lens.lens (responseStatus :: UpdateAnomalySubscriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateAnomalySubscriptionResponse)
{-# DEPRECATED uasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A cost anomaly subscription ARN.
--
-- /Note:/ Consider using 'subscriptionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrsSubscriptionARN :: Lens.Lens' UpdateAnomalySubscriptionResponse Lude.Text
uasrsSubscriptionARN = Lens.lens (subscriptionARN :: UpdateAnomalySubscriptionResponse -> Lude.Text) (\s a -> s {subscriptionARN = a} :: UpdateAnomalySubscriptionResponse)
{-# DEPRECATED uasrsSubscriptionARN "Use generic-lens or generic-optics with 'subscriptionARN' instead." #-}
