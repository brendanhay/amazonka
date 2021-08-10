{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.AnomalySubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.AnomalySubscription where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.AnomalySubscriptionFrequency
import Network.AWS.CostExplorer.Types.Subscriber
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The association between a monitor, threshold, and list of subscribers
-- used to deliver notifications about anomalies detected by a monitor that
-- exceeds a threshold. The content consists of the detailed metadata and
-- the current status of the @AnomalySubscription@ object.
--
-- /See:/ 'newAnomalySubscription' smart constructor.
data AnomalySubscription = AnomalySubscription'
  { -- | Your unique account identifier.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The @AnomalySubscription@ Amazon Resource Name (ARN).
    subscriptionArn :: Prelude.Maybe Prelude.Text,
    -- | A list of cost anomaly monitors.
    monitorArnList :: [Prelude.Text],
    -- | A list of subscribers to notify.
    subscribers :: [Subscriber],
    -- | The dollar value that triggers a notification if the threshold is
    -- exceeded.
    threshold :: Prelude.Double,
    -- | The frequency at which anomaly reports are sent over email.
    frequency :: AnomalySubscriptionFrequency,
    -- | The name for the subscription.
    subscriptionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnomalySubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'anomalySubscription_accountId' - Your unique account identifier.
--
-- 'subscriptionArn', 'anomalySubscription_subscriptionArn' - The @AnomalySubscription@ Amazon Resource Name (ARN).
--
-- 'monitorArnList', 'anomalySubscription_monitorArnList' - A list of cost anomaly monitors.
--
-- 'subscribers', 'anomalySubscription_subscribers' - A list of subscribers to notify.
--
-- 'threshold', 'anomalySubscription_threshold' - The dollar value that triggers a notification if the threshold is
-- exceeded.
--
-- 'frequency', 'anomalySubscription_frequency' - The frequency at which anomaly reports are sent over email.
--
-- 'subscriptionName', 'anomalySubscription_subscriptionName' - The name for the subscription.
newAnomalySubscription ::
  -- | 'threshold'
  Prelude.Double ->
  -- | 'frequency'
  AnomalySubscriptionFrequency ->
  -- | 'subscriptionName'
  Prelude.Text ->
  AnomalySubscription
newAnomalySubscription
  pThreshold_
  pFrequency_
  pSubscriptionName_ =
    AnomalySubscription'
      { accountId = Prelude.Nothing,
        subscriptionArn = Prelude.Nothing,
        monitorArnList = Prelude.mempty,
        subscribers = Prelude.mempty,
        threshold = pThreshold_,
        frequency = pFrequency_,
        subscriptionName = pSubscriptionName_
      }

-- | Your unique account identifier.
anomalySubscription_accountId :: Lens.Lens' AnomalySubscription (Prelude.Maybe Prelude.Text)
anomalySubscription_accountId = Lens.lens (\AnomalySubscription' {accountId} -> accountId) (\s@AnomalySubscription' {} a -> s {accountId = a} :: AnomalySubscription)

-- | The @AnomalySubscription@ Amazon Resource Name (ARN).
anomalySubscription_subscriptionArn :: Lens.Lens' AnomalySubscription (Prelude.Maybe Prelude.Text)
anomalySubscription_subscriptionArn = Lens.lens (\AnomalySubscription' {subscriptionArn} -> subscriptionArn) (\s@AnomalySubscription' {} a -> s {subscriptionArn = a} :: AnomalySubscription)

-- | A list of cost anomaly monitors.
anomalySubscription_monitorArnList :: Lens.Lens' AnomalySubscription [Prelude.Text]
anomalySubscription_monitorArnList = Lens.lens (\AnomalySubscription' {monitorArnList} -> monitorArnList) (\s@AnomalySubscription' {} a -> s {monitorArnList = a} :: AnomalySubscription) Prelude.. Lens._Coerce

-- | A list of subscribers to notify.
anomalySubscription_subscribers :: Lens.Lens' AnomalySubscription [Subscriber]
anomalySubscription_subscribers = Lens.lens (\AnomalySubscription' {subscribers} -> subscribers) (\s@AnomalySubscription' {} a -> s {subscribers = a} :: AnomalySubscription) Prelude.. Lens._Coerce

-- | The dollar value that triggers a notification if the threshold is
-- exceeded.
anomalySubscription_threshold :: Lens.Lens' AnomalySubscription Prelude.Double
anomalySubscription_threshold = Lens.lens (\AnomalySubscription' {threshold} -> threshold) (\s@AnomalySubscription' {} a -> s {threshold = a} :: AnomalySubscription)

-- | The frequency at which anomaly reports are sent over email.
anomalySubscription_frequency :: Lens.Lens' AnomalySubscription AnomalySubscriptionFrequency
anomalySubscription_frequency = Lens.lens (\AnomalySubscription' {frequency} -> frequency) (\s@AnomalySubscription' {} a -> s {frequency = a} :: AnomalySubscription)

-- | The name for the subscription.
anomalySubscription_subscriptionName :: Lens.Lens' AnomalySubscription Prelude.Text
anomalySubscription_subscriptionName = Lens.lens (\AnomalySubscription' {subscriptionName} -> subscriptionName) (\s@AnomalySubscription' {} a -> s {subscriptionName = a} :: AnomalySubscription)

instance Core.FromJSON AnomalySubscription where
  parseJSON =
    Core.withObject
      "AnomalySubscription"
      ( \x ->
          AnomalySubscription'
            Prelude.<$> (x Core..:? "AccountId")
            Prelude.<*> (x Core..:? "SubscriptionArn")
            Prelude.<*> (x Core..:? "MonitorArnList" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Subscribers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "Threshold")
            Prelude.<*> (x Core..: "Frequency")
            Prelude.<*> (x Core..: "SubscriptionName")
      )

instance Prelude.Hashable AnomalySubscription

instance Prelude.NFData AnomalySubscription

instance Core.ToJSON AnomalySubscription where
  toJSON AnomalySubscription' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AccountId" Core..=) Prelude.<$> accountId,
            ("SubscriptionArn" Core..=)
              Prelude.<$> subscriptionArn,
            Prelude.Just
              ("MonitorArnList" Core..= monitorArnList),
            Prelude.Just ("Subscribers" Core..= subscribers),
            Prelude.Just ("Threshold" Core..= threshold),
            Prelude.Just ("Frequency" Core..= frequency),
            Prelude.Just
              ("SubscriptionName" Core..= subscriptionName)
          ]
      )
