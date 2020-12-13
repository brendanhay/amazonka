{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.AnomalySubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.AnomalySubscription
  ( AnomalySubscription (..),

    -- * Smart constructor
    mkAnomalySubscription,

    -- * Lenses
    asSubscriptionName,
    asFrequency,
    asAccountId,
    asThreshold,
    asMonitorARNList,
    asSubscriptionARN,
    asSubscribers,
  )
where

import Network.AWS.CostExplorer.Types.AnomalySubscriptionFrequency
import Network.AWS.CostExplorer.Types.Subscriber
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The association between a monitor, threshold, and list of subscribers used to deliver notifications about anomalies detected by a monitor that exceeds a threshold. The content consists of the detailed metadata and the current status of the @AnomalySubscription@ object.
--
-- /See:/ 'mkAnomalySubscription' smart constructor.
data AnomalySubscription = AnomalySubscription'
  { -- | The name for the subscription.
    subscriptionName :: Lude.Text,
    -- | The frequency at which anomaly reports are sent over email.
    frequency :: AnomalySubscriptionFrequency,
    -- | Your unique account identifier.
    accountId :: Lude.Maybe Lude.Text,
    -- | The dollar value that triggers a notification if the threshold is exceeded.
    threshold :: Lude.Double,
    -- | A list of cost anomaly monitors.
    monitorARNList :: [Lude.Text],
    -- | The @AnomalySubscription@ Amazon Resource Name (ARN).
    subscriptionARN :: Lude.Maybe Lude.Text,
    -- | A list of subscribers to notify.
    subscribers :: [Subscriber]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AnomalySubscription' with the minimum fields required to make a request.
--
-- * 'subscriptionName' - The name for the subscription.
-- * 'frequency' - The frequency at which anomaly reports are sent over email.
-- * 'accountId' - Your unique account identifier.
-- * 'threshold' - The dollar value that triggers a notification if the threshold is exceeded.
-- * 'monitorARNList' - A list of cost anomaly monitors.
-- * 'subscriptionARN' - The @AnomalySubscription@ Amazon Resource Name (ARN).
-- * 'subscribers' - A list of subscribers to notify.
mkAnomalySubscription ::
  -- | 'subscriptionName'
  Lude.Text ->
  -- | 'frequency'
  AnomalySubscriptionFrequency ->
  -- | 'threshold'
  Lude.Double ->
  AnomalySubscription
mkAnomalySubscription pSubscriptionName_ pFrequency_ pThreshold_ =
  AnomalySubscription'
    { subscriptionName = pSubscriptionName_,
      frequency = pFrequency_,
      accountId = Lude.Nothing,
      threshold = pThreshold_,
      monitorARNList = Lude.mempty,
      subscriptionARN = Lude.Nothing,
      subscribers = Lude.mempty
    }

-- | The name for the subscription.
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSubscriptionName :: Lens.Lens' AnomalySubscription Lude.Text
asSubscriptionName = Lens.lens (subscriptionName :: AnomalySubscription -> Lude.Text) (\s a -> s {subscriptionName = a} :: AnomalySubscription)
{-# DEPRECATED asSubscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead." #-}

-- | The frequency at which anomaly reports are sent over email.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asFrequency :: Lens.Lens' AnomalySubscription AnomalySubscriptionFrequency
asFrequency = Lens.lens (frequency :: AnomalySubscription -> AnomalySubscriptionFrequency) (\s a -> s {frequency = a} :: AnomalySubscription)
{-# DEPRECATED asFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

-- | Your unique account identifier.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAccountId :: Lens.Lens' AnomalySubscription (Lude.Maybe Lude.Text)
asAccountId = Lens.lens (accountId :: AnomalySubscription -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: AnomalySubscription)
{-# DEPRECATED asAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The dollar value that triggers a notification if the threshold is exceeded.
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asThreshold :: Lens.Lens' AnomalySubscription Lude.Double
asThreshold = Lens.lens (threshold :: AnomalySubscription -> Lude.Double) (\s a -> s {threshold = a} :: AnomalySubscription)
{-# DEPRECATED asThreshold "Use generic-lens or generic-optics with 'threshold' instead." #-}

-- | A list of cost anomaly monitors.
--
-- /Note:/ Consider using 'monitorARNList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asMonitorARNList :: Lens.Lens' AnomalySubscription [Lude.Text]
asMonitorARNList = Lens.lens (monitorARNList :: AnomalySubscription -> [Lude.Text]) (\s a -> s {monitorARNList = a} :: AnomalySubscription)
{-# DEPRECATED asMonitorARNList "Use generic-lens or generic-optics with 'monitorARNList' instead." #-}

-- | The @AnomalySubscription@ Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'subscriptionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSubscriptionARN :: Lens.Lens' AnomalySubscription (Lude.Maybe Lude.Text)
asSubscriptionARN = Lens.lens (subscriptionARN :: AnomalySubscription -> Lude.Maybe Lude.Text) (\s a -> s {subscriptionARN = a} :: AnomalySubscription)
{-# DEPRECATED asSubscriptionARN "Use generic-lens or generic-optics with 'subscriptionARN' instead." #-}

-- | A list of subscribers to notify.
--
-- /Note:/ Consider using 'subscribers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSubscribers :: Lens.Lens' AnomalySubscription [Subscriber]
asSubscribers = Lens.lens (subscribers :: AnomalySubscription -> [Subscriber]) (\s a -> s {subscribers = a} :: AnomalySubscription)
{-# DEPRECATED asSubscribers "Use generic-lens or generic-optics with 'subscribers' instead." #-}

instance Lude.FromJSON AnomalySubscription where
  parseJSON =
    Lude.withObject
      "AnomalySubscription"
      ( \x ->
          AnomalySubscription'
            Lude.<$> (x Lude..: "SubscriptionName")
            Lude.<*> (x Lude..: "Frequency")
            Lude.<*> (x Lude..:? "AccountId")
            Lude.<*> (x Lude..: "Threshold")
            Lude.<*> (x Lude..:? "MonitorArnList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "SubscriptionArn")
            Lude.<*> (x Lude..:? "Subscribers" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON AnomalySubscription where
  toJSON AnomalySubscription' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SubscriptionName" Lude..= subscriptionName),
            Lude.Just ("Frequency" Lude..= frequency),
            ("AccountId" Lude..=) Lude.<$> accountId,
            Lude.Just ("Threshold" Lude..= threshold),
            Lude.Just ("MonitorArnList" Lude..= monitorARNList),
            ("SubscriptionArn" Lude..=) Lude.<$> subscriptionARN,
            Lude.Just ("Subscribers" Lude..= subscribers)
          ]
      )
