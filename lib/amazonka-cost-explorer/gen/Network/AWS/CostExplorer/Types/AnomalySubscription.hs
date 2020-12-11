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
    asAccountId,
    asSubscriptionARN,
    asMonitorARNList,
    asSubscribers,
    asThreshold,
    asFrequency,
    asSubscriptionName,
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
  { accountId ::
      Lude.Maybe Lude.Text,
    subscriptionARN :: Lude.Maybe Lude.Text,
    monitorARNList :: [Lude.Text],
    subscribers :: [Subscriber],
    threshold :: Lude.Double,
    frequency :: AnomalySubscriptionFrequency,
    subscriptionName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AnomalySubscription' with the minimum fields required to make a request.
--
-- * 'accountId' - Your unique account identifier.
-- * 'frequency' - The frequency at which anomaly reports are sent over email.
-- * 'monitorARNList' - A list of cost anomaly monitors.
-- * 'subscribers' - A list of subscribers to notify.
-- * 'subscriptionARN' - The @AnomalySubscription@ Amazon Resource Name (ARN).
-- * 'subscriptionName' - The name for the subscription.
-- * 'threshold' - The dollar value that triggers a notification if the threshold is exceeded.
mkAnomalySubscription ::
  -- | 'threshold'
  Lude.Double ->
  -- | 'frequency'
  AnomalySubscriptionFrequency ->
  -- | 'subscriptionName'
  Lude.Text ->
  AnomalySubscription
mkAnomalySubscription pThreshold_ pFrequency_ pSubscriptionName_ =
  AnomalySubscription'
    { accountId = Lude.Nothing,
      subscriptionARN = Lude.Nothing,
      monitorARNList = Lude.mempty,
      subscribers = Lude.mempty,
      threshold = pThreshold_,
      frequency = pFrequency_,
      subscriptionName = pSubscriptionName_
    }

-- | Your unique account identifier.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAccountId :: Lens.Lens' AnomalySubscription (Lude.Maybe Lude.Text)
asAccountId = Lens.lens (accountId :: AnomalySubscription -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: AnomalySubscription)
{-# DEPRECATED asAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The @AnomalySubscription@ Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'subscriptionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSubscriptionARN :: Lens.Lens' AnomalySubscription (Lude.Maybe Lude.Text)
asSubscriptionARN = Lens.lens (subscriptionARN :: AnomalySubscription -> Lude.Maybe Lude.Text) (\s a -> s {subscriptionARN = a} :: AnomalySubscription)
{-# DEPRECATED asSubscriptionARN "Use generic-lens or generic-optics with 'subscriptionARN' instead." #-}

-- | A list of cost anomaly monitors.
--
-- /Note:/ Consider using 'monitorARNList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asMonitorARNList :: Lens.Lens' AnomalySubscription [Lude.Text]
asMonitorARNList = Lens.lens (monitorARNList :: AnomalySubscription -> [Lude.Text]) (\s a -> s {monitorARNList = a} :: AnomalySubscription)
{-# DEPRECATED asMonitorARNList "Use generic-lens or generic-optics with 'monitorARNList' instead." #-}

-- | A list of subscribers to notify.
--
-- /Note:/ Consider using 'subscribers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSubscribers :: Lens.Lens' AnomalySubscription [Subscriber]
asSubscribers = Lens.lens (subscribers :: AnomalySubscription -> [Subscriber]) (\s a -> s {subscribers = a} :: AnomalySubscription)
{-# DEPRECATED asSubscribers "Use generic-lens or generic-optics with 'subscribers' instead." #-}

-- | The dollar value that triggers a notification if the threshold is exceeded.
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asThreshold :: Lens.Lens' AnomalySubscription Lude.Double
asThreshold = Lens.lens (threshold :: AnomalySubscription -> Lude.Double) (\s a -> s {threshold = a} :: AnomalySubscription)
{-# DEPRECATED asThreshold "Use generic-lens or generic-optics with 'threshold' instead." #-}

-- | The frequency at which anomaly reports are sent over email.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asFrequency :: Lens.Lens' AnomalySubscription AnomalySubscriptionFrequency
asFrequency = Lens.lens (frequency :: AnomalySubscription -> AnomalySubscriptionFrequency) (\s a -> s {frequency = a} :: AnomalySubscription)
{-# DEPRECATED asFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

-- | The name for the subscription.
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSubscriptionName :: Lens.Lens' AnomalySubscription Lude.Text
asSubscriptionName = Lens.lens (subscriptionName :: AnomalySubscription -> Lude.Text) (\s a -> s {subscriptionName = a} :: AnomalySubscription)
{-# DEPRECATED asSubscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead." #-}

instance Lude.FromJSON AnomalySubscription where
  parseJSON =
    Lude.withObject
      "AnomalySubscription"
      ( \x ->
          AnomalySubscription'
            Lude.<$> (x Lude..:? "AccountId")
            Lude.<*> (x Lude..:? "SubscriptionArn")
            Lude.<*> (x Lude..:? "MonitorArnList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Subscribers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "Threshold")
            Lude.<*> (x Lude..: "Frequency")
            Lude.<*> (x Lude..: "SubscriptionName")
      )

instance Lude.ToJSON AnomalySubscription where
  toJSON AnomalySubscription' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AccountId" Lude..=) Lude.<$> accountId,
            ("SubscriptionArn" Lude..=) Lude.<$> subscriptionARN,
            Lude.Just ("MonitorArnList" Lude..= monitorARNList),
            Lude.Just ("Subscribers" Lude..= subscribers),
            Lude.Just ("Threshold" Lude..= threshold),
            Lude.Just ("Frequency" Lude..= frequency),
            Lude.Just ("SubscriptionName" Lude..= subscriptionName)
          ]
      )
