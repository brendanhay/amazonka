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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.Subscription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Shield.Types.AutoRenew
import Amazonka.Shield.Types.Limit
import Amazonka.Shield.Types.ProactiveEngagementStatus
import Amazonka.Shield.Types.SubscriptionLimits

-- | Information about the Shield Advanced subscription for an account.
--
-- /See:/ 'newSubscription' smart constructor.
data Subscription = Subscription'
  { -- | The length, in seconds, of the Shield Advanced subscription for the
    -- account.
    timeCommitmentInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The start time of the subscription, in Unix time in seconds. For more
    -- information see
    -- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | Specifies how many protections of a given type you can create.
    limits :: Prelude.Maybe [Limit],
    -- | If @ENABLED@, the subscription will be automatically renewed at the end
    -- of the existing subscription period.
    --
    -- When you initally create a subscription, @AutoRenew@ is set to
    -- @ENABLED@. You can change this by submitting an @UpdateSubscription@
    -- request. If the @UpdateSubscription@ request does not included a value
    -- for @AutoRenew@, the existing value for @AutoRenew@ remains unchanged.
    autoRenew :: Prelude.Maybe AutoRenew,
    -- | The date and time your subscription will end.
    endTime :: Prelude.Maybe Core.POSIX,
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
    -- | The ARN (Amazon Resource Name) of the subscription.
    subscriptionArn :: Prelude.Maybe Prelude.Text,
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
-- 'timeCommitmentInSeconds', 'subscription_timeCommitmentInSeconds' - The length, in seconds, of the Shield Advanced subscription for the
-- account.
--
-- 'startTime', 'subscription_startTime' - The start time of the subscription, in Unix time in seconds. For more
-- information see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
--
-- 'limits', 'subscription_limits' - Specifies how many protections of a given type you can create.
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
-- 'subscriptionArn', 'subscription_subscriptionArn' - The ARN (Amazon Resource Name) of the subscription.
--
-- 'subscriptionLimits', 'subscription_subscriptionLimits' - Limits settings for your subscription.
newSubscription ::
  -- | 'subscriptionLimits'
  SubscriptionLimits ->
  Subscription
newSubscription pSubscriptionLimits_ =
  Subscription'
    { timeCommitmentInSeconds =
        Prelude.Nothing,
      startTime = Prelude.Nothing,
      limits = Prelude.Nothing,
      autoRenew = Prelude.Nothing,
      endTime = Prelude.Nothing,
      proactiveEngagementStatus = Prelude.Nothing,
      subscriptionArn = Prelude.Nothing,
      subscriptionLimits = pSubscriptionLimits_
    }

-- | The length, in seconds, of the Shield Advanced subscription for the
-- account.
subscription_timeCommitmentInSeconds :: Lens.Lens' Subscription (Prelude.Maybe Prelude.Natural)
subscription_timeCommitmentInSeconds = Lens.lens (\Subscription' {timeCommitmentInSeconds} -> timeCommitmentInSeconds) (\s@Subscription' {} a -> s {timeCommitmentInSeconds = a} :: Subscription)

-- | The start time of the subscription, in Unix time in seconds. For more
-- information see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
subscription_startTime :: Lens.Lens' Subscription (Prelude.Maybe Prelude.UTCTime)
subscription_startTime = Lens.lens (\Subscription' {startTime} -> startTime) (\s@Subscription' {} a -> s {startTime = a} :: Subscription) Prelude.. Lens.mapping Core._Time

-- | Specifies how many protections of a given type you can create.
subscription_limits :: Lens.Lens' Subscription (Prelude.Maybe [Limit])
subscription_limits = Lens.lens (\Subscription' {limits} -> limits) (\s@Subscription' {} a -> s {limits = a} :: Subscription) Prelude.. Lens.mapping Lens.coerced

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
subscription_endTime = Lens.lens (\Subscription' {endTime} -> endTime) (\s@Subscription' {} a -> s {endTime = a} :: Subscription) Prelude.. Lens.mapping Core._Time

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

-- | The ARN (Amazon Resource Name) of the subscription.
subscription_subscriptionArn :: Lens.Lens' Subscription (Prelude.Maybe Prelude.Text)
subscription_subscriptionArn = Lens.lens (\Subscription' {subscriptionArn} -> subscriptionArn) (\s@Subscription' {} a -> s {subscriptionArn = a} :: Subscription)

-- | Limits settings for your subscription.
subscription_subscriptionLimits :: Lens.Lens' Subscription SubscriptionLimits
subscription_subscriptionLimits = Lens.lens (\Subscription' {subscriptionLimits} -> subscriptionLimits) (\s@Subscription' {} a -> s {subscriptionLimits = a} :: Subscription)

instance Core.FromJSON Subscription where
  parseJSON =
    Core.withObject
      "Subscription"
      ( \x ->
          Subscription'
            Prelude.<$> (x Core..:? "TimeCommitmentInSeconds")
            Prelude.<*> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "Limits" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "AutoRenew")
            Prelude.<*> (x Core..:? "EndTime")
            Prelude.<*> (x Core..:? "ProactiveEngagementStatus")
            Prelude.<*> (x Core..:? "SubscriptionArn")
            Prelude.<*> (x Core..: "SubscriptionLimits")
      )

instance Prelude.Hashable Subscription where
  hashWithSalt salt' Subscription' {..} =
    salt' `Prelude.hashWithSalt` subscriptionLimits
      `Prelude.hashWithSalt` subscriptionArn
      `Prelude.hashWithSalt` proactiveEngagementStatus
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` autoRenew
      `Prelude.hashWithSalt` limits
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` timeCommitmentInSeconds

instance Prelude.NFData Subscription where
  rnf Subscription' {..} =
    Prelude.rnf timeCommitmentInSeconds
      `Prelude.seq` Prelude.rnf subscriptionLimits
      `Prelude.seq` Prelude.rnf subscriptionArn
      `Prelude.seq` Prelude.rnf proactiveEngagementStatus
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf autoRenew
      `Prelude.seq` Prelude.rnf limits
      `Prelude.seq` Prelude.rnf startTime
