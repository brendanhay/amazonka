{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.Subscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.Subscription
  ( Subscription (..),

    -- * Smart constructor
    mkSubscription,

    -- * Lenses
    sTimeCommitmentInSeconds,
    sStartTime,
    sLimits,
    sAutoRenew,
    sEndTime,
    sProactiveEngagementStatus,
    sSubscriptionLimits,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Shield.Types.AutoRenew
import Network.AWS.Shield.Types.Limit
import Network.AWS.Shield.Types.ProactiveEngagementStatus
import Network.AWS.Shield.Types.SubscriptionLimits

-- | Information about the AWS Shield Advanced subscription for an account.
--
-- /See:/ 'mkSubscription' smart constructor.
data Subscription = Subscription'
  { timeCommitmentInSeconds ::
      Lude.Maybe Lude.Natural,
    startTime :: Lude.Maybe Lude.Timestamp,
    limits :: Lude.Maybe [Limit],
    autoRenew :: Lude.Maybe AutoRenew,
    endTime :: Lude.Maybe Lude.Timestamp,
    proactiveEngagementStatus :: Lude.Maybe ProactiveEngagementStatus,
    subscriptionLimits :: SubscriptionLimits
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Subscription' with the minimum fields required to make a request.
--
-- * 'autoRenew' - If @ENABLED@ , the subscription will be automatically renewed at the end of the existing subscription period.
--
-- When you initally create a subscription, @AutoRenew@ is set to @ENABLED@ . You can change this by submitting an @UpdateSubscription@ request. If the @UpdateSubscription@ request does not included a value for @AutoRenew@ , the existing value for @AutoRenew@ remains unchanged.
-- * 'endTime' - The date and time your subscription will end.
-- * 'limits' - Specifies how many protections of a given type you can create.
-- * 'proactiveEngagementStatus' - If @ENABLED@ , the DDoS Response Team (DRT) will use email and phone to notify contacts about escalations to the DRT and to initiate proactive customer support.
--
-- If @PENDING@ , you have requested proactive engagement and the request is pending. The status changes to @ENABLED@ when your request is fully processed.
-- If @DISABLED@ , the DRT will not proactively notify contacts about escalations or to initiate proactive customer support.
-- * 'startTime' - The start time of the subscription, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
-- * 'subscriptionLimits' - Limits settings for your subscription.
-- * 'timeCommitmentInSeconds' - The length, in seconds, of the AWS Shield Advanced subscription for the account.
mkSubscription ::
  -- | 'subscriptionLimits'
  SubscriptionLimits ->
  Subscription
mkSubscription pSubscriptionLimits_ =
  Subscription'
    { timeCommitmentInSeconds = Lude.Nothing,
      startTime = Lude.Nothing,
      limits = Lude.Nothing,
      autoRenew = Lude.Nothing,
      endTime = Lude.Nothing,
      proactiveEngagementStatus = Lude.Nothing,
      subscriptionLimits = pSubscriptionLimits_
    }

-- | The length, in seconds, of the AWS Shield Advanced subscription for the account.
--
-- /Note:/ Consider using 'timeCommitmentInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTimeCommitmentInSeconds :: Lens.Lens' Subscription (Lude.Maybe Lude.Natural)
sTimeCommitmentInSeconds = Lens.lens (timeCommitmentInSeconds :: Subscription -> Lude.Maybe Lude.Natural) (\s a -> s {timeCommitmentInSeconds = a} :: Subscription)
{-# DEPRECATED sTimeCommitmentInSeconds "Use generic-lens or generic-optics with 'timeCommitmentInSeconds' instead." #-}

-- | The start time of the subscription, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStartTime :: Lens.Lens' Subscription (Lude.Maybe Lude.Timestamp)
sStartTime = Lens.lens (startTime :: Subscription -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: Subscription)
{-# DEPRECATED sStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Specifies how many protections of a given type you can create.
--
-- /Note:/ Consider using 'limits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sLimits :: Lens.Lens' Subscription (Lude.Maybe [Limit])
sLimits = Lens.lens (limits :: Subscription -> Lude.Maybe [Limit]) (\s a -> s {limits = a} :: Subscription)
{-# DEPRECATED sLimits "Use generic-lens or generic-optics with 'limits' instead." #-}

-- | If @ENABLED@ , the subscription will be automatically renewed at the end of the existing subscription period.
--
-- When you initally create a subscription, @AutoRenew@ is set to @ENABLED@ . You can change this by submitting an @UpdateSubscription@ request. If the @UpdateSubscription@ request does not included a value for @AutoRenew@ , the existing value for @AutoRenew@ remains unchanged.
--
-- /Note:/ Consider using 'autoRenew' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAutoRenew :: Lens.Lens' Subscription (Lude.Maybe AutoRenew)
sAutoRenew = Lens.lens (autoRenew :: Subscription -> Lude.Maybe AutoRenew) (\s a -> s {autoRenew = a} :: Subscription)
{-# DEPRECATED sAutoRenew "Use generic-lens or generic-optics with 'autoRenew' instead." #-}

-- | The date and time your subscription will end.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEndTime :: Lens.Lens' Subscription (Lude.Maybe Lude.Timestamp)
sEndTime = Lens.lens (endTime :: Subscription -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: Subscription)
{-# DEPRECATED sEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | If @ENABLED@ , the DDoS Response Team (DRT) will use email and phone to notify contacts about escalations to the DRT and to initiate proactive customer support.
--
-- If @PENDING@ , you have requested proactive engagement and the request is pending. The status changes to @ENABLED@ when your request is fully processed.
-- If @DISABLED@ , the DRT will not proactively notify contacts about escalations or to initiate proactive customer support.
--
-- /Note:/ Consider using 'proactiveEngagementStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sProactiveEngagementStatus :: Lens.Lens' Subscription (Lude.Maybe ProactiveEngagementStatus)
sProactiveEngagementStatus = Lens.lens (proactiveEngagementStatus :: Subscription -> Lude.Maybe ProactiveEngagementStatus) (\s a -> s {proactiveEngagementStatus = a} :: Subscription)
{-# DEPRECATED sProactiveEngagementStatus "Use generic-lens or generic-optics with 'proactiveEngagementStatus' instead." #-}

-- | Limits settings for your subscription.
--
-- /Note:/ Consider using 'subscriptionLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubscriptionLimits :: Lens.Lens' Subscription SubscriptionLimits
sSubscriptionLimits = Lens.lens (subscriptionLimits :: Subscription -> SubscriptionLimits) (\s a -> s {subscriptionLimits = a} :: Subscription)
{-# DEPRECATED sSubscriptionLimits "Use generic-lens or generic-optics with 'subscriptionLimits' instead." #-}

instance Lude.FromJSON Subscription where
  parseJSON =
    Lude.withObject
      "Subscription"
      ( \x ->
          Subscription'
            Lude.<$> (x Lude..:? "TimeCommitmentInSeconds")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "Limits" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AutoRenew")
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "ProactiveEngagementStatus")
            Lude.<*> (x Lude..: "SubscriptionLimits")
      )
