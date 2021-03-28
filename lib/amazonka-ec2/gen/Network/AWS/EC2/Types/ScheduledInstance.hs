{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ScheduledInstance
  ( ScheduledInstance (..)
  -- * Smart constructor
  , mkScheduledInstance
  -- * Lenses
  , siAvailabilityZone
  , siCreateDate
  , siHourlyPrice
  , siInstanceCount
  , siInstanceType
  , siNetworkPlatform
  , siNextSlotStartTime
  , siPlatform
  , siPreviousSlotEndTime
  , siRecurrence
  , siScheduledInstanceId
  , siSlotDurationInHours
  , siTermEndDate
  , siTermStartDate
  , siTotalScheduledInstanceHours
  ) where

import qualified Network.AWS.EC2.Types.ScheduledInstanceRecurrence as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Scheduled Instance.
--
-- /See:/ 'mkScheduledInstance' smart constructor.
data ScheduledInstance = ScheduledInstance'
  { availabilityZone :: Core.Maybe Core.Text
    -- ^ The Availability Zone.
  , createDate :: Core.Maybe Core.UTCTime
    -- ^ The date when the Scheduled Instance was purchased.
  , hourlyPrice :: Core.Maybe Core.Text
    -- ^ The hourly price for a single instance.
  , instanceCount :: Core.Maybe Core.Int
    -- ^ The number of instances.
  , instanceType :: Core.Maybe Core.Text
    -- ^ The instance type.
  , networkPlatform :: Core.Maybe Core.Text
    -- ^ The network platform (@EC2-Classic@ or @EC2-VPC@ ).
  , nextSlotStartTime :: Core.Maybe Core.UTCTime
    -- ^ The time for the next schedule to start.
  , platform :: Core.Maybe Core.Text
    -- ^ The platform (@Linux/UNIX@ or @Windows@ ).
  , previousSlotEndTime :: Core.Maybe Core.UTCTime
    -- ^ The time that the previous schedule ended or will end.
  , recurrence :: Core.Maybe Types.ScheduledInstanceRecurrence
    -- ^ The schedule recurrence.
  , scheduledInstanceId :: Core.Maybe Core.Text
    -- ^ The Scheduled Instance ID.
  , slotDurationInHours :: Core.Maybe Core.Int
    -- ^ The number of hours in the schedule.
  , termEndDate :: Core.Maybe Core.UTCTime
    -- ^ The end date for the Scheduled Instance.
  , termStartDate :: Core.Maybe Core.UTCTime
    -- ^ The start date for the Scheduled Instance.
  , totalScheduledInstanceHours :: Core.Maybe Core.Int
    -- ^ The total number of hours for a single instance for the entire term.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ScheduledInstance' value with any optional fields omitted.
mkScheduledInstance
    :: ScheduledInstance
mkScheduledInstance
  = ScheduledInstance'{availabilityZone = Core.Nothing,
                       createDate = Core.Nothing, hourlyPrice = Core.Nothing,
                       instanceCount = Core.Nothing, instanceType = Core.Nothing,
                       networkPlatform = Core.Nothing, nextSlotStartTime = Core.Nothing,
                       platform = Core.Nothing, previousSlotEndTime = Core.Nothing,
                       recurrence = Core.Nothing, scheduledInstanceId = Core.Nothing,
                       slotDurationInHours = Core.Nothing, termEndDate = Core.Nothing,
                       termStartDate = Core.Nothing,
                       totalScheduledInstanceHours = Core.Nothing}

-- | The Availability Zone.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siAvailabilityZone :: Lens.Lens' ScheduledInstance (Core.Maybe Core.Text)
siAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE siAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The date when the Scheduled Instance was purchased.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siCreateDate :: Lens.Lens' ScheduledInstance (Core.Maybe Core.UTCTime)
siCreateDate = Lens.field @"createDate"
{-# INLINEABLE siCreateDate #-}
{-# DEPRECATED createDate "Use generic-lens or generic-optics with 'createDate' instead"  #-}

-- | The hourly price for a single instance.
--
-- /Note:/ Consider using 'hourlyPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siHourlyPrice :: Lens.Lens' ScheduledInstance (Core.Maybe Core.Text)
siHourlyPrice = Lens.field @"hourlyPrice"
{-# INLINEABLE siHourlyPrice #-}
{-# DEPRECATED hourlyPrice "Use generic-lens or generic-optics with 'hourlyPrice' instead"  #-}

-- | The number of instances.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siInstanceCount :: Lens.Lens' ScheduledInstance (Core.Maybe Core.Int)
siInstanceCount = Lens.field @"instanceCount"
{-# INLINEABLE siInstanceCount #-}
{-# DEPRECATED instanceCount "Use generic-lens or generic-optics with 'instanceCount' instead"  #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siInstanceType :: Lens.Lens' ScheduledInstance (Core.Maybe Core.Text)
siInstanceType = Lens.field @"instanceType"
{-# INLINEABLE siInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The network platform (@EC2-Classic@ or @EC2-VPC@ ).
--
-- /Note:/ Consider using 'networkPlatform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siNetworkPlatform :: Lens.Lens' ScheduledInstance (Core.Maybe Core.Text)
siNetworkPlatform = Lens.field @"networkPlatform"
{-# INLINEABLE siNetworkPlatform #-}
{-# DEPRECATED networkPlatform "Use generic-lens or generic-optics with 'networkPlatform' instead"  #-}

-- | The time for the next schedule to start.
--
-- /Note:/ Consider using 'nextSlotStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siNextSlotStartTime :: Lens.Lens' ScheduledInstance (Core.Maybe Core.UTCTime)
siNextSlotStartTime = Lens.field @"nextSlotStartTime"
{-# INLINEABLE siNextSlotStartTime #-}
{-# DEPRECATED nextSlotStartTime "Use generic-lens or generic-optics with 'nextSlotStartTime' instead"  #-}

-- | The platform (@Linux/UNIX@ or @Windows@ ).
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siPlatform :: Lens.Lens' ScheduledInstance (Core.Maybe Core.Text)
siPlatform = Lens.field @"platform"
{-# INLINEABLE siPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

-- | The time that the previous schedule ended or will end.
--
-- /Note:/ Consider using 'previousSlotEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siPreviousSlotEndTime :: Lens.Lens' ScheduledInstance (Core.Maybe Core.UTCTime)
siPreviousSlotEndTime = Lens.field @"previousSlotEndTime"
{-# INLINEABLE siPreviousSlotEndTime #-}
{-# DEPRECATED previousSlotEndTime "Use generic-lens or generic-optics with 'previousSlotEndTime' instead"  #-}

-- | The schedule recurrence.
--
-- /Note:/ Consider using 'recurrence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siRecurrence :: Lens.Lens' ScheduledInstance (Core.Maybe Types.ScheduledInstanceRecurrence)
siRecurrence = Lens.field @"recurrence"
{-# INLINEABLE siRecurrence #-}
{-# DEPRECATED recurrence "Use generic-lens or generic-optics with 'recurrence' instead"  #-}

-- | The Scheduled Instance ID.
--
-- /Note:/ Consider using 'scheduledInstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siScheduledInstanceId :: Lens.Lens' ScheduledInstance (Core.Maybe Core.Text)
siScheduledInstanceId = Lens.field @"scheduledInstanceId"
{-# INLINEABLE siScheduledInstanceId #-}
{-# DEPRECATED scheduledInstanceId "Use generic-lens or generic-optics with 'scheduledInstanceId' instead"  #-}

-- | The number of hours in the schedule.
--
-- /Note:/ Consider using 'slotDurationInHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siSlotDurationInHours :: Lens.Lens' ScheduledInstance (Core.Maybe Core.Int)
siSlotDurationInHours = Lens.field @"slotDurationInHours"
{-# INLINEABLE siSlotDurationInHours #-}
{-# DEPRECATED slotDurationInHours "Use generic-lens or generic-optics with 'slotDurationInHours' instead"  #-}

-- | The end date for the Scheduled Instance.
--
-- /Note:/ Consider using 'termEndDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siTermEndDate :: Lens.Lens' ScheduledInstance (Core.Maybe Core.UTCTime)
siTermEndDate = Lens.field @"termEndDate"
{-# INLINEABLE siTermEndDate #-}
{-# DEPRECATED termEndDate "Use generic-lens or generic-optics with 'termEndDate' instead"  #-}

-- | The start date for the Scheduled Instance.
--
-- /Note:/ Consider using 'termStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siTermStartDate :: Lens.Lens' ScheduledInstance (Core.Maybe Core.UTCTime)
siTermStartDate = Lens.field @"termStartDate"
{-# INLINEABLE siTermStartDate #-}
{-# DEPRECATED termStartDate "Use generic-lens or generic-optics with 'termStartDate' instead"  #-}

-- | The total number of hours for a single instance for the entire term.
--
-- /Note:/ Consider using 'totalScheduledInstanceHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siTotalScheduledInstanceHours :: Lens.Lens' ScheduledInstance (Core.Maybe Core.Int)
siTotalScheduledInstanceHours = Lens.field @"totalScheduledInstanceHours"
{-# INLINEABLE siTotalScheduledInstanceHours #-}
{-# DEPRECATED totalScheduledInstanceHours "Use generic-lens or generic-optics with 'totalScheduledInstanceHours' instead"  #-}

instance Core.FromXML ScheduledInstance where
        parseXML x
          = ScheduledInstance' Core.<$>
              (x Core..@? "availabilityZone") Core.<*> x Core..@? "createDate"
                Core.<*> x Core..@? "hourlyPrice"
                Core.<*> x Core..@? "instanceCount"
                Core.<*> x Core..@? "instanceType"
                Core.<*> x Core..@? "networkPlatform"
                Core.<*> x Core..@? "nextSlotStartTime"
                Core.<*> x Core..@? "platform"
                Core.<*> x Core..@? "previousSlotEndTime"
                Core.<*> x Core..@? "recurrence"
                Core.<*> x Core..@? "scheduledInstanceId"
                Core.<*> x Core..@? "slotDurationInHours"
                Core.<*> x Core..@? "termEndDate"
                Core.<*> x Core..@? "termStartDate"
                Core.<*> x Core..@? "totalScheduledInstanceHours"
