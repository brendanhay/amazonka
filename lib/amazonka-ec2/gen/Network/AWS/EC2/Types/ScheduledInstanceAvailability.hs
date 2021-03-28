{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstanceAvailability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ScheduledInstanceAvailability
  ( ScheduledInstanceAvailability (..)
  -- * Smart constructor
  , mkScheduledInstanceAvailability
  -- * Lenses
  , siaAvailabilityZone
  , siaAvailableInstanceCount
  , siaFirstSlotStartTime
  , siaHourlyPrice
  , siaInstanceType
  , siaMaxTermDurationInDays
  , siaMinTermDurationInDays
  , siaNetworkPlatform
  , siaPlatform
  , siaPurchaseToken
  , siaRecurrence
  , siaSlotDurationInHours
  , siaTotalScheduledInstanceHours
  ) where

import qualified Network.AWS.EC2.Types.ScheduledInstanceRecurrence as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a schedule that is available for your Scheduled Instances.
--
-- /See:/ 'mkScheduledInstanceAvailability' smart constructor.
data ScheduledInstanceAvailability = ScheduledInstanceAvailability'
  { availabilityZone :: Core.Maybe Core.Text
    -- ^ The Availability Zone.
  , availableInstanceCount :: Core.Maybe Core.Int
    -- ^ The number of available instances.
  , firstSlotStartTime :: Core.Maybe Core.UTCTime
    -- ^ The time period for the first schedule to start.
  , hourlyPrice :: Core.Maybe Core.Text
    -- ^ The hourly price for a single instance.
  , instanceType :: Core.Maybe Core.Text
    -- ^ The instance type. You can specify one of the C3, C4, M4, or R3 instance types.
  , maxTermDurationInDays :: Core.Maybe Core.Int
    -- ^ The maximum term. The only possible value is 365 days.
  , minTermDurationInDays :: Core.Maybe Core.Int
    -- ^ The minimum term. The only possible value is 365 days.
  , networkPlatform :: Core.Maybe Core.Text
    -- ^ The network platform (@EC2-Classic@ or @EC2-VPC@ ).
  , platform :: Core.Maybe Core.Text
    -- ^ The platform (@Linux/UNIX@ or @Windows@ ).
  , purchaseToken :: Core.Maybe Core.Text
    -- ^ The purchase token. This token expires in two hours.
  , recurrence :: Core.Maybe Types.ScheduledInstanceRecurrence
    -- ^ The schedule recurrence.
  , slotDurationInHours :: Core.Maybe Core.Int
    -- ^ The number of hours in the schedule.
  , totalScheduledInstanceHours :: Core.Maybe Core.Int
    -- ^ The total number of hours for a single instance for the entire term.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ScheduledInstanceAvailability' value with any optional fields omitted.
mkScheduledInstanceAvailability
    :: ScheduledInstanceAvailability
mkScheduledInstanceAvailability
  = ScheduledInstanceAvailability'{availabilityZone = Core.Nothing,
                                   availableInstanceCount = Core.Nothing,
                                   firstSlotStartTime = Core.Nothing, hourlyPrice = Core.Nothing,
                                   instanceType = Core.Nothing,
                                   maxTermDurationInDays = Core.Nothing,
                                   minTermDurationInDays = Core.Nothing,
                                   networkPlatform = Core.Nothing, platform = Core.Nothing,
                                   purchaseToken = Core.Nothing, recurrence = Core.Nothing,
                                   slotDurationInHours = Core.Nothing,
                                   totalScheduledInstanceHours = Core.Nothing}

-- | The Availability Zone.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaAvailabilityZone :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.Text)
siaAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE siaAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The number of available instances.
--
-- /Note:/ Consider using 'availableInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaAvailableInstanceCount :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.Int)
siaAvailableInstanceCount = Lens.field @"availableInstanceCount"
{-# INLINEABLE siaAvailableInstanceCount #-}
{-# DEPRECATED availableInstanceCount "Use generic-lens or generic-optics with 'availableInstanceCount' instead"  #-}

-- | The time period for the first schedule to start.
--
-- /Note:/ Consider using 'firstSlotStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaFirstSlotStartTime :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.UTCTime)
siaFirstSlotStartTime = Lens.field @"firstSlotStartTime"
{-# INLINEABLE siaFirstSlotStartTime #-}
{-# DEPRECATED firstSlotStartTime "Use generic-lens or generic-optics with 'firstSlotStartTime' instead"  #-}

-- | The hourly price for a single instance.
--
-- /Note:/ Consider using 'hourlyPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaHourlyPrice :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.Text)
siaHourlyPrice = Lens.field @"hourlyPrice"
{-# INLINEABLE siaHourlyPrice #-}
{-# DEPRECATED hourlyPrice "Use generic-lens or generic-optics with 'hourlyPrice' instead"  #-}

-- | The instance type. You can specify one of the C3, C4, M4, or R3 instance types.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaInstanceType :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.Text)
siaInstanceType = Lens.field @"instanceType"
{-# INLINEABLE siaInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The maximum term. The only possible value is 365 days.
--
-- /Note:/ Consider using 'maxTermDurationInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaMaxTermDurationInDays :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.Int)
siaMaxTermDurationInDays = Lens.field @"maxTermDurationInDays"
{-# INLINEABLE siaMaxTermDurationInDays #-}
{-# DEPRECATED maxTermDurationInDays "Use generic-lens or generic-optics with 'maxTermDurationInDays' instead"  #-}

-- | The minimum term. The only possible value is 365 days.
--
-- /Note:/ Consider using 'minTermDurationInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaMinTermDurationInDays :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.Int)
siaMinTermDurationInDays = Lens.field @"minTermDurationInDays"
{-# INLINEABLE siaMinTermDurationInDays #-}
{-# DEPRECATED minTermDurationInDays "Use generic-lens or generic-optics with 'minTermDurationInDays' instead"  #-}

-- | The network platform (@EC2-Classic@ or @EC2-VPC@ ).
--
-- /Note:/ Consider using 'networkPlatform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaNetworkPlatform :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.Text)
siaNetworkPlatform = Lens.field @"networkPlatform"
{-# INLINEABLE siaNetworkPlatform #-}
{-# DEPRECATED networkPlatform "Use generic-lens or generic-optics with 'networkPlatform' instead"  #-}

-- | The platform (@Linux/UNIX@ or @Windows@ ).
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaPlatform :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.Text)
siaPlatform = Lens.field @"platform"
{-# INLINEABLE siaPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

-- | The purchase token. This token expires in two hours.
--
-- /Note:/ Consider using 'purchaseToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaPurchaseToken :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.Text)
siaPurchaseToken = Lens.field @"purchaseToken"
{-# INLINEABLE siaPurchaseToken #-}
{-# DEPRECATED purchaseToken "Use generic-lens or generic-optics with 'purchaseToken' instead"  #-}

-- | The schedule recurrence.
--
-- /Note:/ Consider using 'recurrence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaRecurrence :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Types.ScheduledInstanceRecurrence)
siaRecurrence = Lens.field @"recurrence"
{-# INLINEABLE siaRecurrence #-}
{-# DEPRECATED recurrence "Use generic-lens or generic-optics with 'recurrence' instead"  #-}

-- | The number of hours in the schedule.
--
-- /Note:/ Consider using 'slotDurationInHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaSlotDurationInHours :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.Int)
siaSlotDurationInHours = Lens.field @"slotDurationInHours"
{-# INLINEABLE siaSlotDurationInHours #-}
{-# DEPRECATED slotDurationInHours "Use generic-lens or generic-optics with 'slotDurationInHours' instead"  #-}

-- | The total number of hours for a single instance for the entire term.
--
-- /Note:/ Consider using 'totalScheduledInstanceHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaTotalScheduledInstanceHours :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.Int)
siaTotalScheduledInstanceHours = Lens.field @"totalScheduledInstanceHours"
{-# INLINEABLE siaTotalScheduledInstanceHours #-}
{-# DEPRECATED totalScheduledInstanceHours "Use generic-lens or generic-optics with 'totalScheduledInstanceHours' instead"  #-}

instance Core.FromXML ScheduledInstanceAvailability where
        parseXML x
          = ScheduledInstanceAvailability' Core.<$>
              (x Core..@? "availabilityZone") Core.<*>
                x Core..@? "availableInstanceCount"
                Core.<*> x Core..@? "firstSlotStartTime"
                Core.<*> x Core..@? "hourlyPrice"
                Core.<*> x Core..@? "instanceType"
                Core.<*> x Core..@? "maxTermDurationInDays"
                Core.<*> x Core..@? "minTermDurationInDays"
                Core.<*> x Core..@? "networkPlatform"
                Core.<*> x Core..@? "platform"
                Core.<*> x Core..@? "purchaseToken"
                Core.<*> x Core..@? "recurrence"
                Core.<*> x Core..@? "slotDurationInHours"
                Core.<*> x Core..@? "totalScheduledInstanceHours"
