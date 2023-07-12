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
-- Module      : Amazonka.EC2.Types.Subscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Subscription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.MetricType
import Amazonka.EC2.Types.PeriodType
import Amazonka.EC2.Types.StatisticType
import qualified Amazonka.Prelude as Prelude

-- | Describes an Infrastructure Performance subscription.
--
-- /See:/ 'newSubscription' smart constructor.
data Subscription = Subscription'
  { -- | The Region or Availability Zone that\'s the target for the subscription.
    -- For example, @eu-west-1@.
    destination :: Prelude.Maybe Prelude.Text,
    -- | The metric used for the subscription.
    metric :: Prelude.Maybe MetricType,
    -- | The data aggregation time for the subscription.
    period :: Prelude.Maybe PeriodType,
    -- | The Region or Availability Zone that\'s the source for the subscription.
    -- For example, @us-east-1@.
    source :: Prelude.Maybe Prelude.Text,
    -- | The statistic used for the subscription.
    statistic :: Prelude.Maybe StatisticType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Subscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'subscription_destination' - The Region or Availability Zone that\'s the target for the subscription.
-- For example, @eu-west-1@.
--
-- 'metric', 'subscription_metric' - The metric used for the subscription.
--
-- 'period', 'subscription_period' - The data aggregation time for the subscription.
--
-- 'source', 'subscription_source' - The Region or Availability Zone that\'s the source for the subscription.
-- For example, @us-east-1@.
--
-- 'statistic', 'subscription_statistic' - The statistic used for the subscription.
newSubscription ::
  Subscription
newSubscription =
  Subscription'
    { destination = Prelude.Nothing,
      metric = Prelude.Nothing,
      period = Prelude.Nothing,
      source = Prelude.Nothing,
      statistic = Prelude.Nothing
    }

-- | The Region or Availability Zone that\'s the target for the subscription.
-- For example, @eu-west-1@.
subscription_destination :: Lens.Lens' Subscription (Prelude.Maybe Prelude.Text)
subscription_destination = Lens.lens (\Subscription' {destination} -> destination) (\s@Subscription' {} a -> s {destination = a} :: Subscription)

-- | The metric used for the subscription.
subscription_metric :: Lens.Lens' Subscription (Prelude.Maybe MetricType)
subscription_metric = Lens.lens (\Subscription' {metric} -> metric) (\s@Subscription' {} a -> s {metric = a} :: Subscription)

-- | The data aggregation time for the subscription.
subscription_period :: Lens.Lens' Subscription (Prelude.Maybe PeriodType)
subscription_period = Lens.lens (\Subscription' {period} -> period) (\s@Subscription' {} a -> s {period = a} :: Subscription)

-- | The Region or Availability Zone that\'s the source for the subscription.
-- For example, @us-east-1@.
subscription_source :: Lens.Lens' Subscription (Prelude.Maybe Prelude.Text)
subscription_source = Lens.lens (\Subscription' {source} -> source) (\s@Subscription' {} a -> s {source = a} :: Subscription)

-- | The statistic used for the subscription.
subscription_statistic :: Lens.Lens' Subscription (Prelude.Maybe StatisticType)
subscription_statistic = Lens.lens (\Subscription' {statistic} -> statistic) (\s@Subscription' {} a -> s {statistic = a} :: Subscription)

instance Data.FromXML Subscription where
  parseXML x =
    Subscription'
      Prelude.<$> (x Data..@? "destination")
      Prelude.<*> (x Data..@? "metric")
      Prelude.<*> (x Data..@? "period")
      Prelude.<*> (x Data..@? "source")
      Prelude.<*> (x Data..@? "statistic")

instance Prelude.Hashable Subscription where
  hashWithSalt _salt Subscription' {..} =
    _salt
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` metric
      `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` statistic

instance Prelude.NFData Subscription where
  rnf Subscription' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf metric
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf statistic
