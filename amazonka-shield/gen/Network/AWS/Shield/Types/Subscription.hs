{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Shield.Types.Subscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.Subscription where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Shield.Types.AutoRenew
import Network.AWS.Shield.Types.Limit
import Network.AWS.Shield.Types.ProactiveEngagementStatus
import Network.AWS.Shield.Types.SubscriptionLimits

-- | Information about the AWS Shield Advanced subscription for an account.
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
    -- | If @ENABLED@, the DDoS Response Team (DRT) will use email and phone to
    -- notify contacts about escalations to the DRT and to initiate proactive
    -- customer support.
    --
    -- If @PENDING@, you have requested proactive engagement and the request is
    -- pending. The status changes to @ENABLED@ when your request is fully
    -- processed.
    --
    -- If @DISABLED@, the DRT will not proactively notify contacts about
    -- escalations or to initiate proactive customer support.
    proactiveEngagementStatus :: Prelude.Maybe ProactiveEngagementStatus,
    -- | The start time of the subscription, in Unix time in seconds. For more
    -- information see
    -- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The date and time your subscription will end.
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | Specifies how many protections of a given type you can create.
    limits :: Prelude.Maybe [Limit],
    -- | The length, in seconds, of the AWS Shield Advanced subscription for the
    -- account.
    timeCommitmentInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Limits settings for your subscription.
    subscriptionLimits :: SubscriptionLimits
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'proactiveEngagementStatus', 'subscription_proactiveEngagementStatus' - If @ENABLED@, the DDoS Response Team (DRT) will use email and phone to
-- notify contacts about escalations to the DRT and to initiate proactive
-- customer support.
--
-- If @PENDING@, you have requested proactive engagement and the request is
-- pending. The status changes to @ENABLED@ when your request is fully
-- processed.
--
-- If @DISABLED@, the DRT will not proactively notify contacts about
-- escalations or to initiate proactive customer support.
--
-- 'startTime', 'subscription_startTime' - The start time of the subscription, in Unix time in seconds. For more
-- information see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
--
-- 'endTime', 'subscription_endTime' - The date and time your subscription will end.
--
-- 'limits', 'subscription_limits' - Specifies how many protections of a given type you can create.
--
-- 'timeCommitmentInSeconds', 'subscription_timeCommitmentInSeconds' - The length, in seconds, of the AWS Shield Advanced subscription for the
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
      proactiveEngagementStatus = Prelude.Nothing,
      startTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      limits = Prelude.Nothing,
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

-- | If @ENABLED@, the DDoS Response Team (DRT) will use email and phone to
-- notify contacts about escalations to the DRT and to initiate proactive
-- customer support.
--
-- If @PENDING@, you have requested proactive engagement and the request is
-- pending. The status changes to @ENABLED@ when your request is fully
-- processed.
--
-- If @DISABLED@, the DRT will not proactively notify contacts about
-- escalations or to initiate proactive customer support.
subscription_proactiveEngagementStatus :: Lens.Lens' Subscription (Prelude.Maybe ProactiveEngagementStatus)
subscription_proactiveEngagementStatus = Lens.lens (\Subscription' {proactiveEngagementStatus} -> proactiveEngagementStatus) (\s@Subscription' {} a -> s {proactiveEngagementStatus = a} :: Subscription)

-- | The start time of the subscription, in Unix time in seconds. For more
-- information see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
subscription_startTime :: Lens.Lens' Subscription (Prelude.Maybe Prelude.UTCTime)
subscription_startTime = Lens.lens (\Subscription' {startTime} -> startTime) (\s@Subscription' {} a -> s {startTime = a} :: Subscription) Prelude.. Lens.mapping Prelude._Time

-- | The date and time your subscription will end.
subscription_endTime :: Lens.Lens' Subscription (Prelude.Maybe Prelude.UTCTime)
subscription_endTime = Lens.lens (\Subscription' {endTime} -> endTime) (\s@Subscription' {} a -> s {endTime = a} :: Subscription) Prelude.. Lens.mapping Prelude._Time

-- | Specifies how many protections of a given type you can create.
subscription_limits :: Lens.Lens' Subscription (Prelude.Maybe [Limit])
subscription_limits = Lens.lens (\Subscription' {limits} -> limits) (\s@Subscription' {} a -> s {limits = a} :: Subscription) Prelude.. Lens.mapping Prelude._Coerce

-- | The length, in seconds, of the AWS Shield Advanced subscription for the
-- account.
subscription_timeCommitmentInSeconds :: Lens.Lens' Subscription (Prelude.Maybe Prelude.Natural)
subscription_timeCommitmentInSeconds = Lens.lens (\Subscription' {timeCommitmentInSeconds} -> timeCommitmentInSeconds) (\s@Subscription' {} a -> s {timeCommitmentInSeconds = a} :: Subscription)

-- | Limits settings for your subscription.
subscription_subscriptionLimits :: Lens.Lens' Subscription SubscriptionLimits
subscription_subscriptionLimits = Lens.lens (\Subscription' {subscriptionLimits} -> subscriptionLimits) (\s@Subscription' {} a -> s {subscriptionLimits = a} :: Subscription)

instance Prelude.FromJSON Subscription where
  parseJSON =
    Prelude.withObject
      "Subscription"
      ( \x ->
          Subscription'
            Prelude.<$> (x Prelude..:? "AutoRenew")
            Prelude.<*> (x Prelude..:? "ProactiveEngagementStatus")
            Prelude.<*> (x Prelude..:? "StartTime")
            Prelude.<*> (x Prelude..:? "EndTime")
            Prelude.<*> (x Prelude..:? "Limits" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "TimeCommitmentInSeconds")
            Prelude.<*> (x Prelude..: "SubscriptionLimits")
      )

instance Prelude.Hashable Subscription

instance Prelude.NFData Subscription
