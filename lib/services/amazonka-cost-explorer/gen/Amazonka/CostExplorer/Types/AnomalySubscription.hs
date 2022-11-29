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
-- Module      : Amazonka.CostExplorer.Types.AnomalySubscription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.AnomalySubscription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.AnomalySubscriptionFrequency
import Amazonka.CostExplorer.Types.Subscriber
import qualified Amazonka.Prelude as Prelude

-- | The association between a monitor, threshold, and list of subscribers
-- used to deliver notifications about anomalies detected by a monitor that
-- exceeds a threshold. The content consists of the detailed metadata and
-- the current status of the @AnomalySubscription@ object.
--
-- /See:/ 'newAnomalySubscription' smart constructor.
data AnomalySubscription = AnomalySubscription'
  { -- | The @AnomalySubscription@ Amazon Resource Name (ARN).
    subscriptionArn :: Prelude.Maybe Prelude.Text,
    -- | Your unique account identifier.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | A list of cost anomaly monitors.
    monitorArnList :: [Prelude.Text],
    -- | A list of subscribers to notify.
    subscribers :: [Subscriber],
    -- | The dollar value that triggers a notification if the threshold is
    -- exceeded.
    threshold :: Prelude.Double,
    -- | The frequency that anomaly reports are sent over email.
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
-- 'subscriptionArn', 'anomalySubscription_subscriptionArn' - The @AnomalySubscription@ Amazon Resource Name (ARN).
--
-- 'accountId', 'anomalySubscription_accountId' - Your unique account identifier.
--
-- 'monitorArnList', 'anomalySubscription_monitorArnList' - A list of cost anomaly monitors.
--
-- 'subscribers', 'anomalySubscription_subscribers' - A list of subscribers to notify.
--
-- 'threshold', 'anomalySubscription_threshold' - The dollar value that triggers a notification if the threshold is
-- exceeded.
--
-- 'frequency', 'anomalySubscription_frequency' - The frequency that anomaly reports are sent over email.
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
      { subscriptionArn =
          Prelude.Nothing,
        accountId = Prelude.Nothing,
        monitorArnList = Prelude.mempty,
        subscribers = Prelude.mempty,
        threshold = pThreshold_,
        frequency = pFrequency_,
        subscriptionName = pSubscriptionName_
      }

-- | The @AnomalySubscription@ Amazon Resource Name (ARN).
anomalySubscription_subscriptionArn :: Lens.Lens' AnomalySubscription (Prelude.Maybe Prelude.Text)
anomalySubscription_subscriptionArn = Lens.lens (\AnomalySubscription' {subscriptionArn} -> subscriptionArn) (\s@AnomalySubscription' {} a -> s {subscriptionArn = a} :: AnomalySubscription)

-- | Your unique account identifier.
anomalySubscription_accountId :: Lens.Lens' AnomalySubscription (Prelude.Maybe Prelude.Text)
anomalySubscription_accountId = Lens.lens (\AnomalySubscription' {accountId} -> accountId) (\s@AnomalySubscription' {} a -> s {accountId = a} :: AnomalySubscription)

-- | A list of cost anomaly monitors.
anomalySubscription_monitorArnList :: Lens.Lens' AnomalySubscription [Prelude.Text]
anomalySubscription_monitorArnList = Lens.lens (\AnomalySubscription' {monitorArnList} -> monitorArnList) (\s@AnomalySubscription' {} a -> s {monitorArnList = a} :: AnomalySubscription) Prelude.. Lens.coerced

-- | A list of subscribers to notify.
anomalySubscription_subscribers :: Lens.Lens' AnomalySubscription [Subscriber]
anomalySubscription_subscribers = Lens.lens (\AnomalySubscription' {subscribers} -> subscribers) (\s@AnomalySubscription' {} a -> s {subscribers = a} :: AnomalySubscription) Prelude.. Lens.coerced

-- | The dollar value that triggers a notification if the threshold is
-- exceeded.
anomalySubscription_threshold :: Lens.Lens' AnomalySubscription Prelude.Double
anomalySubscription_threshold = Lens.lens (\AnomalySubscription' {threshold} -> threshold) (\s@AnomalySubscription' {} a -> s {threshold = a} :: AnomalySubscription)

-- | The frequency that anomaly reports are sent over email.
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
            Prelude.<$> (x Core..:? "SubscriptionArn")
            Prelude.<*> (x Core..:? "AccountId")
            Prelude.<*> (x Core..:? "MonitorArnList" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Subscribers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "Threshold")
            Prelude.<*> (x Core..: "Frequency")
            Prelude.<*> (x Core..: "SubscriptionName")
      )

instance Prelude.Hashable AnomalySubscription where
  hashWithSalt _salt AnomalySubscription' {..} =
    _salt `Prelude.hashWithSalt` subscriptionArn
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` monitorArnList
      `Prelude.hashWithSalt` subscribers
      `Prelude.hashWithSalt` threshold
      `Prelude.hashWithSalt` frequency
      `Prelude.hashWithSalt` subscriptionName

instance Prelude.NFData AnomalySubscription where
  rnf AnomalySubscription' {..} =
    Prelude.rnf subscriptionArn
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf monitorArnList
      `Prelude.seq` Prelude.rnf subscribers
      `Prelude.seq` Prelude.rnf threshold
      `Prelude.seq` Prelude.rnf frequency
      `Prelude.seq` Prelude.rnf subscriptionName

instance Core.ToJSON AnomalySubscription where
  toJSON AnomalySubscription' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SubscriptionArn" Core..=)
              Prelude.<$> subscriptionArn,
            ("AccountId" Core..=) Prelude.<$> accountId,
            Prelude.Just
              ("MonitorArnList" Core..= monitorArnList),
            Prelude.Just ("Subscribers" Core..= subscribers),
            Prelude.Just ("Threshold" Core..= threshold),
            Prelude.Just ("Frequency" Core..= frequency),
            Prelude.Just
              ("SubscriptionName" Core..= subscriptionName)
          ]
      )
