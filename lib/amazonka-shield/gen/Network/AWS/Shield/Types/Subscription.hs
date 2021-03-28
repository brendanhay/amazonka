{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.Subscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.Subscription
  ( Subscription (..)
  -- * Smart constructor
  , mkSubscription
  -- * Lenses
  , sSubscriptionLimits
  , sAutoRenew
  , sEndTime
  , sLimits
  , sProactiveEngagementStatus
  , sStartTime
  , sTimeCommitmentInSeconds
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Shield.Types.AutoRenew as Types
import qualified Network.AWS.Shield.Types.Limit as Types
import qualified Network.AWS.Shield.Types.ProactiveEngagementStatus as Types
import qualified Network.AWS.Shield.Types.SubscriptionLimits as Types

-- | Information about the AWS Shield Advanced subscription for an account.
--
-- /See:/ 'mkSubscription' smart constructor.
data Subscription = Subscription'
  { subscriptionLimits :: Types.SubscriptionLimits
    -- ^ Limits settings for your subscription. 
  , autoRenew :: Core.Maybe Types.AutoRenew
    -- ^ If @ENABLED@ , the subscription will be automatically renewed at the end of the existing subscription period.
--
-- When you initally create a subscription, @AutoRenew@ is set to @ENABLED@ . You can change this by submitting an @UpdateSubscription@ request. If the @UpdateSubscription@ request does not included a value for @AutoRenew@ , the existing value for @AutoRenew@ remains unchanged.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time your subscription will end.
  , limits :: Core.Maybe [Types.Limit]
    -- ^ Specifies how many protections of a given type you can create.
  , proactiveEngagementStatus :: Core.Maybe Types.ProactiveEngagementStatus
    -- ^ If @ENABLED@ , the DDoS Response Team (DRT) will use email and phone to notify contacts about escalations to the DRT and to initiate proactive customer support.
--
-- If @PENDING@ , you have requested proactive engagement and the request is pending. The status changes to @ENABLED@ when your request is fully processed.
-- If @DISABLED@ , the DRT will not proactively notify contacts about escalations or to initiate proactive customer support. 
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The start time of the subscription, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
  , timeCommitmentInSeconds :: Core.Maybe Core.Natural
    -- ^ The length, in seconds, of the AWS Shield Advanced subscription for the account.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Subscription' value with any optional fields omitted.
mkSubscription
    :: Types.SubscriptionLimits -- ^ 'subscriptionLimits'
    -> Subscription
mkSubscription subscriptionLimits
  = Subscription'{subscriptionLimits, autoRenew = Core.Nothing,
                  endTime = Core.Nothing, limits = Core.Nothing,
                  proactiveEngagementStatus = Core.Nothing, startTime = Core.Nothing,
                  timeCommitmentInSeconds = Core.Nothing}

-- | Limits settings for your subscription. 
--
-- /Note:/ Consider using 'subscriptionLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubscriptionLimits :: Lens.Lens' Subscription Types.SubscriptionLimits
sSubscriptionLimits = Lens.field @"subscriptionLimits"
{-# INLINEABLE sSubscriptionLimits #-}
{-# DEPRECATED subscriptionLimits "Use generic-lens or generic-optics with 'subscriptionLimits' instead"  #-}

-- | If @ENABLED@ , the subscription will be automatically renewed at the end of the existing subscription period.
--
-- When you initally create a subscription, @AutoRenew@ is set to @ENABLED@ . You can change this by submitting an @UpdateSubscription@ request. If the @UpdateSubscription@ request does not included a value for @AutoRenew@ , the existing value for @AutoRenew@ remains unchanged.
--
-- /Note:/ Consider using 'autoRenew' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAutoRenew :: Lens.Lens' Subscription (Core.Maybe Types.AutoRenew)
sAutoRenew = Lens.field @"autoRenew"
{-# INLINEABLE sAutoRenew #-}
{-# DEPRECATED autoRenew "Use generic-lens or generic-optics with 'autoRenew' instead"  #-}

-- | The date and time your subscription will end.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEndTime :: Lens.Lens' Subscription (Core.Maybe Core.NominalDiffTime)
sEndTime = Lens.field @"endTime"
{-# INLINEABLE sEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | Specifies how many protections of a given type you can create.
--
-- /Note:/ Consider using 'limits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sLimits :: Lens.Lens' Subscription (Core.Maybe [Types.Limit])
sLimits = Lens.field @"limits"
{-# INLINEABLE sLimits #-}
{-# DEPRECATED limits "Use generic-lens or generic-optics with 'limits' instead"  #-}

-- | If @ENABLED@ , the DDoS Response Team (DRT) will use email and phone to notify contacts about escalations to the DRT and to initiate proactive customer support.
--
-- If @PENDING@ , you have requested proactive engagement and the request is pending. The status changes to @ENABLED@ when your request is fully processed.
-- If @DISABLED@ , the DRT will not proactively notify contacts about escalations or to initiate proactive customer support. 
--
-- /Note:/ Consider using 'proactiveEngagementStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sProactiveEngagementStatus :: Lens.Lens' Subscription (Core.Maybe Types.ProactiveEngagementStatus)
sProactiveEngagementStatus = Lens.field @"proactiveEngagementStatus"
{-# INLINEABLE sProactiveEngagementStatus #-}
{-# DEPRECATED proactiveEngagementStatus "Use generic-lens or generic-optics with 'proactiveEngagementStatus' instead"  #-}

-- | The start time of the subscription, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStartTime :: Lens.Lens' Subscription (Core.Maybe Core.NominalDiffTime)
sStartTime = Lens.field @"startTime"
{-# INLINEABLE sStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The length, in seconds, of the AWS Shield Advanced subscription for the account.
--
-- /Note:/ Consider using 'timeCommitmentInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTimeCommitmentInSeconds :: Lens.Lens' Subscription (Core.Maybe Core.Natural)
sTimeCommitmentInSeconds = Lens.field @"timeCommitmentInSeconds"
{-# INLINEABLE sTimeCommitmentInSeconds #-}
{-# DEPRECATED timeCommitmentInSeconds "Use generic-lens or generic-optics with 'timeCommitmentInSeconds' instead"  #-}

instance Core.FromJSON Subscription where
        parseJSON
          = Core.withObject "Subscription" Core.$
              \ x ->
                Subscription' Core.<$>
                  (x Core..: "SubscriptionLimits") Core.<*> x Core..:? "AutoRenew"
                    Core.<*> x Core..:? "EndTime"
                    Core.<*> x Core..:? "Limits"
                    Core.<*> x Core..:? "ProactiveEngagementStatus"
                    Core.<*> x Core..:? "StartTime"
                    Core.<*> x Core..:? "TimeCommitmentInSeconds"
