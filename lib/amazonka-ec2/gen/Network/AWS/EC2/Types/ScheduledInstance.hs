-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstance
  ( ScheduledInstance (..),

    -- * Smart constructor
    mkScheduledInstance,

    -- * Lenses
    siPreviousSlotEndTime,
    siPlatform,
    siTermStartDate,
    siInstanceCount,
    siScheduledInstanceId,
    siHourlyPrice,
    siCreateDate,
    siSlotDurationInHours,
    siTotalScheduledInstanceHours,
    siInstanceType,
    siRecurrence,
    siAvailabilityZone,
    siTermEndDate,
    siNextSlotStartTime,
    siNetworkPlatform,
  )
where

import Network.AWS.EC2.Types.ScheduledInstanceRecurrence
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a Scheduled Instance.
--
-- /See:/ 'mkScheduledInstance' smart constructor.
data ScheduledInstance = ScheduledInstance'
  { previousSlotEndTime ::
      Lude.Maybe Lude.ISO8601,
    platform :: Lude.Maybe Lude.Text,
    termStartDate :: Lude.Maybe Lude.ISO8601,
    instanceCount :: Lude.Maybe Lude.Int,
    scheduledInstanceId :: Lude.Maybe Lude.Text,
    hourlyPrice :: Lude.Maybe Lude.Text,
    createDate :: Lude.Maybe Lude.ISO8601,
    slotDurationInHours :: Lude.Maybe Lude.Int,
    totalScheduledInstanceHours :: Lude.Maybe Lude.Int,
    instanceType :: Lude.Maybe Lude.Text,
    recurrence :: Lude.Maybe ScheduledInstanceRecurrence,
    availabilityZone :: Lude.Maybe Lude.Text,
    termEndDate :: Lude.Maybe Lude.ISO8601,
    nextSlotStartTime :: Lude.Maybe Lude.ISO8601,
    networkPlatform :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduledInstance' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone.
-- * 'createDate' - The date when the Scheduled Instance was purchased.
-- * 'hourlyPrice' - The hourly price for a single instance.
-- * 'instanceCount' - The number of instances.
-- * 'instanceType' - The instance type.
-- * 'networkPlatform' - The network platform (@EC2-Classic@ or @EC2-VPC@ ).
-- * 'nextSlotStartTime' - The time for the next schedule to start.
-- * 'platform' - The platform (@Linux/UNIX@ or @Windows@ ).
-- * 'previousSlotEndTime' - The time that the previous schedule ended or will end.
-- * 'recurrence' - The schedule recurrence.
-- * 'scheduledInstanceId' - The Scheduled Instance ID.
-- * 'slotDurationInHours' - The number of hours in the schedule.
-- * 'termEndDate' - The end date for the Scheduled Instance.
-- * 'termStartDate' - The start date for the Scheduled Instance.
-- * 'totalScheduledInstanceHours' - The total number of hours for a single instance for the entire term.
mkScheduledInstance ::
  ScheduledInstance
mkScheduledInstance =
  ScheduledInstance'
    { previousSlotEndTime = Lude.Nothing,
      platform = Lude.Nothing,
      termStartDate = Lude.Nothing,
      instanceCount = Lude.Nothing,
      scheduledInstanceId = Lude.Nothing,
      hourlyPrice = Lude.Nothing,
      createDate = Lude.Nothing,
      slotDurationInHours = Lude.Nothing,
      totalScheduledInstanceHours = Lude.Nothing,
      instanceType = Lude.Nothing,
      recurrence = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      termEndDate = Lude.Nothing,
      nextSlotStartTime = Lude.Nothing,
      networkPlatform = Lude.Nothing
    }

-- | The time that the previous schedule ended or will end.
--
-- /Note:/ Consider using 'previousSlotEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siPreviousSlotEndTime :: Lens.Lens' ScheduledInstance (Lude.Maybe Lude.ISO8601)
siPreviousSlotEndTime = Lens.lens (previousSlotEndTime :: ScheduledInstance -> Lude.Maybe Lude.ISO8601) (\s a -> s {previousSlotEndTime = a} :: ScheduledInstance)
{-# DEPRECATED siPreviousSlotEndTime "Use generic-lens or generic-optics with 'previousSlotEndTime' instead." #-}

-- | The platform (@Linux/UNIX@ or @Windows@ ).
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siPlatform :: Lens.Lens' ScheduledInstance (Lude.Maybe Lude.Text)
siPlatform = Lens.lens (platform :: ScheduledInstance -> Lude.Maybe Lude.Text) (\s a -> s {platform = a} :: ScheduledInstance)
{-# DEPRECATED siPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The start date for the Scheduled Instance.
--
-- /Note:/ Consider using 'termStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siTermStartDate :: Lens.Lens' ScheduledInstance (Lude.Maybe Lude.ISO8601)
siTermStartDate = Lens.lens (termStartDate :: ScheduledInstance -> Lude.Maybe Lude.ISO8601) (\s a -> s {termStartDate = a} :: ScheduledInstance)
{-# DEPRECATED siTermStartDate "Use generic-lens or generic-optics with 'termStartDate' instead." #-}

-- | The number of instances.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siInstanceCount :: Lens.Lens' ScheduledInstance (Lude.Maybe Lude.Int)
siInstanceCount = Lens.lens (instanceCount :: ScheduledInstance -> Lude.Maybe Lude.Int) (\s a -> s {instanceCount = a} :: ScheduledInstance)
{-# DEPRECATED siInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The Scheduled Instance ID.
--
-- /Note:/ Consider using 'scheduledInstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siScheduledInstanceId :: Lens.Lens' ScheduledInstance (Lude.Maybe Lude.Text)
siScheduledInstanceId = Lens.lens (scheduledInstanceId :: ScheduledInstance -> Lude.Maybe Lude.Text) (\s a -> s {scheduledInstanceId = a} :: ScheduledInstance)
{-# DEPRECATED siScheduledInstanceId "Use generic-lens or generic-optics with 'scheduledInstanceId' instead." #-}

-- | The hourly price for a single instance.
--
-- /Note:/ Consider using 'hourlyPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siHourlyPrice :: Lens.Lens' ScheduledInstance (Lude.Maybe Lude.Text)
siHourlyPrice = Lens.lens (hourlyPrice :: ScheduledInstance -> Lude.Maybe Lude.Text) (\s a -> s {hourlyPrice = a} :: ScheduledInstance)
{-# DEPRECATED siHourlyPrice "Use generic-lens or generic-optics with 'hourlyPrice' instead." #-}

-- | The date when the Scheduled Instance was purchased.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siCreateDate :: Lens.Lens' ScheduledInstance (Lude.Maybe Lude.ISO8601)
siCreateDate = Lens.lens (createDate :: ScheduledInstance -> Lude.Maybe Lude.ISO8601) (\s a -> s {createDate = a} :: ScheduledInstance)
{-# DEPRECATED siCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The number of hours in the schedule.
--
-- /Note:/ Consider using 'slotDurationInHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siSlotDurationInHours :: Lens.Lens' ScheduledInstance (Lude.Maybe Lude.Int)
siSlotDurationInHours = Lens.lens (slotDurationInHours :: ScheduledInstance -> Lude.Maybe Lude.Int) (\s a -> s {slotDurationInHours = a} :: ScheduledInstance)
{-# DEPRECATED siSlotDurationInHours "Use generic-lens or generic-optics with 'slotDurationInHours' instead." #-}

-- | The total number of hours for a single instance for the entire term.
--
-- /Note:/ Consider using 'totalScheduledInstanceHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siTotalScheduledInstanceHours :: Lens.Lens' ScheduledInstance (Lude.Maybe Lude.Int)
siTotalScheduledInstanceHours = Lens.lens (totalScheduledInstanceHours :: ScheduledInstance -> Lude.Maybe Lude.Int) (\s a -> s {totalScheduledInstanceHours = a} :: ScheduledInstance)
{-# DEPRECATED siTotalScheduledInstanceHours "Use generic-lens or generic-optics with 'totalScheduledInstanceHours' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siInstanceType :: Lens.Lens' ScheduledInstance (Lude.Maybe Lude.Text)
siInstanceType = Lens.lens (instanceType :: ScheduledInstance -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: ScheduledInstance)
{-# DEPRECATED siInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The schedule recurrence.
--
-- /Note:/ Consider using 'recurrence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siRecurrence :: Lens.Lens' ScheduledInstance (Lude.Maybe ScheduledInstanceRecurrence)
siRecurrence = Lens.lens (recurrence :: ScheduledInstance -> Lude.Maybe ScheduledInstanceRecurrence) (\s a -> s {recurrence = a} :: ScheduledInstance)
{-# DEPRECATED siRecurrence "Use generic-lens or generic-optics with 'recurrence' instead." #-}

-- | The Availability Zone.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siAvailabilityZone :: Lens.Lens' ScheduledInstance (Lude.Maybe Lude.Text)
siAvailabilityZone = Lens.lens (availabilityZone :: ScheduledInstance -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: ScheduledInstance)
{-# DEPRECATED siAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The end date for the Scheduled Instance.
--
-- /Note:/ Consider using 'termEndDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siTermEndDate :: Lens.Lens' ScheduledInstance (Lude.Maybe Lude.ISO8601)
siTermEndDate = Lens.lens (termEndDate :: ScheduledInstance -> Lude.Maybe Lude.ISO8601) (\s a -> s {termEndDate = a} :: ScheduledInstance)
{-# DEPRECATED siTermEndDate "Use generic-lens or generic-optics with 'termEndDate' instead." #-}

-- | The time for the next schedule to start.
--
-- /Note:/ Consider using 'nextSlotStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siNextSlotStartTime :: Lens.Lens' ScheduledInstance (Lude.Maybe Lude.ISO8601)
siNextSlotStartTime = Lens.lens (nextSlotStartTime :: ScheduledInstance -> Lude.Maybe Lude.ISO8601) (\s a -> s {nextSlotStartTime = a} :: ScheduledInstance)
{-# DEPRECATED siNextSlotStartTime "Use generic-lens or generic-optics with 'nextSlotStartTime' instead." #-}

-- | The network platform (@EC2-Classic@ or @EC2-VPC@ ).
--
-- /Note:/ Consider using 'networkPlatform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siNetworkPlatform :: Lens.Lens' ScheduledInstance (Lude.Maybe Lude.Text)
siNetworkPlatform = Lens.lens (networkPlatform :: ScheduledInstance -> Lude.Maybe Lude.Text) (\s a -> s {networkPlatform = a} :: ScheduledInstance)
{-# DEPRECATED siNetworkPlatform "Use generic-lens or generic-optics with 'networkPlatform' instead." #-}

instance Lude.FromXML ScheduledInstance where
  parseXML x =
    ScheduledInstance'
      Lude.<$> (x Lude..@? "previousSlotEndTime")
      Lude.<*> (x Lude..@? "platform")
      Lude.<*> (x Lude..@? "termStartDate")
      Lude.<*> (x Lude..@? "instanceCount")
      Lude.<*> (x Lude..@? "scheduledInstanceId")
      Lude.<*> (x Lude..@? "hourlyPrice")
      Lude.<*> (x Lude..@? "createDate")
      Lude.<*> (x Lude..@? "slotDurationInHours")
      Lude.<*> (x Lude..@? "totalScheduledInstanceHours")
      Lude.<*> (x Lude..@? "instanceType")
      Lude.<*> (x Lude..@? "recurrence")
      Lude.<*> (x Lude..@? "availabilityZone")
      Lude.<*> (x Lude..@? "termEndDate")
      Lude.<*> (x Lude..@? "nextSlotStartTime")
      Lude.<*> (x Lude..@? "networkPlatform")
