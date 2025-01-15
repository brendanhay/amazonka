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
-- Module      : Amazonka.Shield.Types.Subscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.Subscription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Shield.Types.AutoRenew
import Amazonka.Shield.Types.Limit
import Amazonka.Shield.Types.ProactiveEngagementStatus
import Amazonka.Shield.Types.SubscriptionLimits

-- | Information about the Shield Advanced subscription for an account.
--
-- /See:/ 'newSubscription' smart constructor.
data Subscription = Subscription'
  { -- | If @ENABLED@, the subscription will be automatically renewed at the end
    -- of the existing subscription period.
    --
    -- When you initally create a subscription, @AutoRenew@ is set to
    -- @ENABLED@. You can change this by submitting an @UpdateSubscription@
    -- request. If the @UpdateSubscription@ request does not included a value
    -- for @AutoRenew@, the existing value for @AutoRenew@ remains unchanged.
    autoRenew :: Prelude.Maybe AutoRenew,
    -- | The date and time your subscription will end.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | Specifies how many protections of a given type you can create.
    limits :: Prelude.Maybe [Limit],
    -- | If @ENABLED@, the Shield Response Team (SRT) will use email and phone to
    -- notify contacts about escalations to the SRT and to initiate proactive
    -- customer support.
    --
    -- If @PENDING@, you have requested proactive engagement and the request is
    -- pending. The status changes to @ENABLED@ when your request is fully
    -- processed.
    --
    -- If @DISABLED@, the SRT will not proactively notify contacts about
    -- escalations or to initiate proactive customer support.
    proactiveEngagementStatus :: Prelude.Maybe ProactiveEngagementStatus,
    -- | The start time of the subscription, in Unix time in seconds.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The ARN (Amazon Resource Name) of the subscription.
    subscriptionArn :: Prelude.Maybe Prelude.Text,
    -- | The length, in seconds, of the Shield Advanced subscription for the
    -- account.
    timeCommitmentInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Limits settings for your subscription.
    subscriptionLimits :: SubscriptionLimits
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
-- 'autoRenew', 'subscription_autoRenew' - If @ENABLED@, the subscription will be automatically renewed at the end
-- of the existing subscription period.
--
-- When you initally create a subscription, @AutoRenew@ is set to
-- @ENABLED@. You can change this by submitting an @UpdateSubscription@
-- request. If the @UpdateSubscription@ request does not included a value
-- for @AutoRenew@, the existing value for @AutoRenew@ remains unchanged.
--
-- 'endTime', 'subscription_endTime' - The date and time your subscription will end.
--
-- 'limits', 'subscription_limits' - Specifies how many protections of a given type you can create.
--
-- 'proactiveEngagementStatus', 'subscription_proactiveEngagementStatus' - If @ENABLED@, the Shield Response Team (SRT) will use email and phone to
-- notify contacts about escalations to the SRT and to initiate proactive
-- customer support.
--
-- If @PENDING@, you have requested proactive engagement and the request is
-- pending. The status changes to @ENABLED@ when your request is fully
-- processed.
--
-- If @DISABLED@, the SRT will not proactively notify contacts about
-- escalations or to initiate proactive customer support.
--
-- 'startTime', 'subscription_startTime' - The start time of the subscription, in Unix time in seconds.
--
-- 'subscriptionArn', 'subscription_subscriptionArn' - The ARN (Amazon Resource Name) of the subscription.
--
-- 'timeCommitmentInSeconds', 'subscription_timeCommitmentInSeconds' - The length, in seconds, of the Shield Advanced subscription for the
-- account.
--
-- 'subscriptionLimits', 'subscription_subscriptionLimits' - Limits settings for your subscription.
newSubscription ::
  -- | 'subscriptionLimits'
  SubscriptionLimits ->
  Subscription
newSubscription pSubscriptionLimits_ =
  Subscription'
    { autoRenew = Prelude.Nothing,
      endTime = Prelude.Nothing,
      limits = Prelude.Nothing,
      proactiveEngagementStatus = Prelude.Nothing,
      startTime = Prelude.Nothing,
      subscriptionArn = Prelude.Nothing,
      timeCommitmentInSeconds = Prelude.Nothing,
      subscriptionLimits = pSubscriptionLimits_
    }

-- | If @ENABLED@, the subscription will be automatically renewed at the end
-- of the existing subscription period.
--
-- When you initally create a subscription, @AutoRenew@ is set to
-- @ENABLED@. You can change this by submitting an @UpdateSubscription@
-- request. If the @UpdateSubscription@ request does not included a value
-- for @AutoRenew@, the existing value for @AutoRenew@ remains unchanged.
subscription_autoRenew :: Lens.Lens' Subscription (Prelude.Maybe AutoRenew)
subscription_autoRenew = Lens.lens (\Subscription' {autoRenew} -> autoRenew) (\s@Subscription' {} a -> s {autoRenew = a} :: Subscription)

-- | The date and time your subscription will end.
subscription_endTime :: Lens.Lens' Subscription (Prelude.Maybe Prelude.UTCTime)
subscription_endTime = Lens.lens (\Subscription' {endTime} -> endTime) (\s@Subscription' {} a -> s {endTime = a} :: Subscription) Prelude.. Lens.mapping Data._Time

-- | Specifies how many protections of a given type you can create.
subscription_limits :: Lens.Lens' Subscription (Prelude.Maybe [Limit])
subscription_limits = Lens.lens (\Subscription' {limits} -> limits) (\s@Subscription' {} a -> s {limits = a} :: Subscription) Prelude.. Lens.mapping Lens.coerced

-- | If @ENABLED@, the Shield Response Team (SRT) will use email and phone to
-- notify contacts about escalations to the SRT and to initiate proactive
-- customer support.
--
-- If @PENDING@, you have requested proactive engagement and the request is
-- pending. The status changes to @ENABLED@ when your request is fully
-- processed.
--
-- If @DISABLED@, the SRT will not proactively notify contacts about
-- escalations or to initiate proactive customer support.
subscription_proactiveEngagementStatus :: Lens.Lens' Subscription (Prelude.Maybe ProactiveEngagementStatus)
subscription_proactiveEngagementStatus = Lens.lens (\Subscription' {proactiveEngagementStatus} -> proactiveEngagementStatus) (\s@Subscription' {} a -> s {proactiveEngagementStatus = a} :: Subscription)

-- | The start time of the subscription, in Unix time in seconds.
subscription_startTime :: Lens.Lens' Subscription (Prelude.Maybe Prelude.UTCTime)
subscription_startTime = Lens.lens (\Subscription' {startTime} -> startTime) (\s@Subscription' {} a -> s {startTime = a} :: Subscription) Prelude.. Lens.mapping Data._Time

-- | The ARN (Amazon Resource Name) of the subscription.
subscription_subscriptionArn :: Lens.Lens' Subscription (Prelude.Maybe Prelude.Text)
subscription_subscriptionArn = Lens.lens (\Subscription' {subscriptionArn} -> subscriptionArn) (\s@Subscription' {} a -> s {subscriptionArn = a} :: Subscription)

-- | The length, in seconds, of the Shield Advanced subscription for the
-- account.
subscription_timeCommitmentInSeconds :: Lens.Lens' Subscription (Prelude.Maybe Prelude.Natural)
subscription_timeCommitmentInSeconds = Lens.lens (\Subscription' {timeCommitmentInSeconds} -> timeCommitmentInSeconds) (\s@Subscription' {} a -> s {timeCommitmentInSeconds = a} :: Subscription)

-- | Limits settings for your subscription.
subscription_subscriptionLimits :: Lens.Lens' Subscription SubscriptionLimits
subscription_subscriptionLimits = Lens.lens (\Subscription' {subscriptionLimits} -> subscriptionLimits) (\s@Subscription' {} a -> s {subscriptionLimits = a} :: Subscription)

instance Data.FromJSON Subscription where
  parseJSON =
    Data.withObject
      "Subscription"
      ( \x ->
          Subscription'
            Prelude.<$> (x Data..:? "AutoRenew")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "Limits" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ProactiveEngagementStatus")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "SubscriptionArn")
            Prelude.<*> (x Data..:? "TimeCommitmentInSeconds")
            Prelude.<*> (x Data..: "SubscriptionLimits")
      )

instance Prelude.Hashable Subscription where
  hashWithSalt _salt Subscription' {..} =
    _salt
      `Prelude.hashWithSalt` autoRenew
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` limits
      `Prelude.hashWithSalt` proactiveEngagementStatus
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` subscriptionArn
      `Prelude.hashWithSalt` timeCommitmentInSeconds
      `Prelude.hashWithSalt` subscriptionLimits

instance Prelude.NFData Subscription where
  rnf Subscription' {..} =
    Prelude.rnf autoRenew `Prelude.seq`
      Prelude.rnf endTime `Prelude.seq`
        Prelude.rnf limits `Prelude.seq`
          Prelude.rnf proactiveEngagementStatus `Prelude.seq`
            Prelude.rnf startTime `Prelude.seq`
              Prelude.rnf subscriptionArn `Prelude.seq`
                Prelude.rnf timeCommitmentInSeconds `Prelude.seq`
                  Prelude.rnf subscriptionLimits
