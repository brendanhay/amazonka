{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstanceAvailability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstanceAvailability
  ( ScheduledInstanceAvailability (..),

    -- * Smart constructor
    mkScheduledInstanceAvailability,

    -- * Lenses
    siaMaxTermDurationInDays,
    siaPlatform,
    siaPurchaseToken,
    siaHourlyPrice,
    siaAvailableInstanceCount,
    siaSlotDurationInHours,
    siaTotalScheduledInstanceHours,
    siaInstanceType,
    siaRecurrence,
    siaAvailabilityZone,
    siaMinTermDurationInDays,
    siaFirstSlotStartTime,
    siaNetworkPlatform,
  )
where

import Network.AWS.EC2.Types.ScheduledInstanceRecurrence
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a schedule that is available for your Scheduled Instances.
--
-- /See:/ 'mkScheduledInstanceAvailability' smart constructor.
data ScheduledInstanceAvailability = ScheduledInstanceAvailability'
  { maxTermDurationInDays ::
      Lude.Maybe Lude.Int,
    platform ::
      Lude.Maybe Lude.Text,
    purchaseToken ::
      Lude.Maybe Lude.Text,
    hourlyPrice ::
      Lude.Maybe Lude.Text,
    availableInstanceCount ::
      Lude.Maybe Lude.Int,
    slotDurationInHours ::
      Lude.Maybe Lude.Int,
    totalScheduledInstanceHours ::
      Lude.Maybe Lude.Int,
    instanceType ::
      Lude.Maybe Lude.Text,
    recurrence ::
      Lude.Maybe
        ScheduledInstanceRecurrence,
    availabilityZone ::
      Lude.Maybe Lude.Text,
    minTermDurationInDays ::
      Lude.Maybe Lude.Int,
    firstSlotStartTime ::
      Lude.Maybe Lude.DateTime,
    networkPlatform ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduledInstanceAvailability' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone.
-- * 'availableInstanceCount' - The number of available instances.
-- * 'firstSlotStartTime' - The time period for the first schedule to start.
-- * 'hourlyPrice' - The hourly price for a single instance.
-- * 'instanceType' - The instance type. You can specify one of the C3, C4, M4, or R3 instance types.
-- * 'maxTermDurationInDays' - The maximum term. The only possible value is 365 days.
-- * 'minTermDurationInDays' - The minimum term. The only possible value is 365 days.
-- * 'networkPlatform' - The network platform (@EC2-Classic@ or @EC2-VPC@ ).
-- * 'platform' - The platform (@Linux/UNIX@ or @Windows@ ).
-- * 'purchaseToken' - The purchase token. This token expires in two hours.
-- * 'recurrence' - The schedule recurrence.
-- * 'slotDurationInHours' - The number of hours in the schedule.
-- * 'totalScheduledInstanceHours' - The total number of hours for a single instance for the entire term.
mkScheduledInstanceAvailability ::
  ScheduledInstanceAvailability
mkScheduledInstanceAvailability =
  ScheduledInstanceAvailability'
    { maxTermDurationInDays =
        Lude.Nothing,
      platform = Lude.Nothing,
      purchaseToken = Lude.Nothing,
      hourlyPrice = Lude.Nothing,
      availableInstanceCount = Lude.Nothing,
      slotDurationInHours = Lude.Nothing,
      totalScheduledInstanceHours = Lude.Nothing,
      instanceType = Lude.Nothing,
      recurrence = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      minTermDurationInDays = Lude.Nothing,
      firstSlotStartTime = Lude.Nothing,
      networkPlatform = Lude.Nothing
    }

-- | The maximum term. The only possible value is 365 days.
--
-- /Note:/ Consider using 'maxTermDurationInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaMaxTermDurationInDays :: Lens.Lens' ScheduledInstanceAvailability (Lude.Maybe Lude.Int)
siaMaxTermDurationInDays = Lens.lens (maxTermDurationInDays :: ScheduledInstanceAvailability -> Lude.Maybe Lude.Int) (\s a -> s {maxTermDurationInDays = a} :: ScheduledInstanceAvailability)
{-# DEPRECATED siaMaxTermDurationInDays "Use generic-lens or generic-optics with 'maxTermDurationInDays' instead." #-}

-- | The platform (@Linux/UNIX@ or @Windows@ ).
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaPlatform :: Lens.Lens' ScheduledInstanceAvailability (Lude.Maybe Lude.Text)
siaPlatform = Lens.lens (platform :: ScheduledInstanceAvailability -> Lude.Maybe Lude.Text) (\s a -> s {platform = a} :: ScheduledInstanceAvailability)
{-# DEPRECATED siaPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The purchase token. This token expires in two hours.
--
-- /Note:/ Consider using 'purchaseToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaPurchaseToken :: Lens.Lens' ScheduledInstanceAvailability (Lude.Maybe Lude.Text)
siaPurchaseToken = Lens.lens (purchaseToken :: ScheduledInstanceAvailability -> Lude.Maybe Lude.Text) (\s a -> s {purchaseToken = a} :: ScheduledInstanceAvailability)
{-# DEPRECATED siaPurchaseToken "Use generic-lens or generic-optics with 'purchaseToken' instead." #-}

-- | The hourly price for a single instance.
--
-- /Note:/ Consider using 'hourlyPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaHourlyPrice :: Lens.Lens' ScheduledInstanceAvailability (Lude.Maybe Lude.Text)
siaHourlyPrice = Lens.lens (hourlyPrice :: ScheduledInstanceAvailability -> Lude.Maybe Lude.Text) (\s a -> s {hourlyPrice = a} :: ScheduledInstanceAvailability)
{-# DEPRECATED siaHourlyPrice "Use generic-lens or generic-optics with 'hourlyPrice' instead." #-}

-- | The number of available instances.
--
-- /Note:/ Consider using 'availableInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaAvailableInstanceCount :: Lens.Lens' ScheduledInstanceAvailability (Lude.Maybe Lude.Int)
siaAvailableInstanceCount = Lens.lens (availableInstanceCount :: ScheduledInstanceAvailability -> Lude.Maybe Lude.Int) (\s a -> s {availableInstanceCount = a} :: ScheduledInstanceAvailability)
{-# DEPRECATED siaAvailableInstanceCount "Use generic-lens or generic-optics with 'availableInstanceCount' instead." #-}

-- | The number of hours in the schedule.
--
-- /Note:/ Consider using 'slotDurationInHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaSlotDurationInHours :: Lens.Lens' ScheduledInstanceAvailability (Lude.Maybe Lude.Int)
siaSlotDurationInHours = Lens.lens (slotDurationInHours :: ScheduledInstanceAvailability -> Lude.Maybe Lude.Int) (\s a -> s {slotDurationInHours = a} :: ScheduledInstanceAvailability)
{-# DEPRECATED siaSlotDurationInHours "Use generic-lens or generic-optics with 'slotDurationInHours' instead." #-}

-- | The total number of hours for a single instance for the entire term.
--
-- /Note:/ Consider using 'totalScheduledInstanceHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaTotalScheduledInstanceHours :: Lens.Lens' ScheduledInstanceAvailability (Lude.Maybe Lude.Int)
siaTotalScheduledInstanceHours = Lens.lens (totalScheduledInstanceHours :: ScheduledInstanceAvailability -> Lude.Maybe Lude.Int) (\s a -> s {totalScheduledInstanceHours = a} :: ScheduledInstanceAvailability)
{-# DEPRECATED siaTotalScheduledInstanceHours "Use generic-lens or generic-optics with 'totalScheduledInstanceHours' instead." #-}

-- | The instance type. You can specify one of the C3, C4, M4, or R3 instance types.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaInstanceType :: Lens.Lens' ScheduledInstanceAvailability (Lude.Maybe Lude.Text)
siaInstanceType = Lens.lens (instanceType :: ScheduledInstanceAvailability -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: ScheduledInstanceAvailability)
{-# DEPRECATED siaInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The schedule recurrence.
--
-- /Note:/ Consider using 'recurrence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaRecurrence :: Lens.Lens' ScheduledInstanceAvailability (Lude.Maybe ScheduledInstanceRecurrence)
siaRecurrence = Lens.lens (recurrence :: ScheduledInstanceAvailability -> Lude.Maybe ScheduledInstanceRecurrence) (\s a -> s {recurrence = a} :: ScheduledInstanceAvailability)
{-# DEPRECATED siaRecurrence "Use generic-lens or generic-optics with 'recurrence' instead." #-}

-- | The Availability Zone.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaAvailabilityZone :: Lens.Lens' ScheduledInstanceAvailability (Lude.Maybe Lude.Text)
siaAvailabilityZone = Lens.lens (availabilityZone :: ScheduledInstanceAvailability -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: ScheduledInstanceAvailability)
{-# DEPRECATED siaAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The minimum term. The only possible value is 365 days.
--
-- /Note:/ Consider using 'minTermDurationInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaMinTermDurationInDays :: Lens.Lens' ScheduledInstanceAvailability (Lude.Maybe Lude.Int)
siaMinTermDurationInDays = Lens.lens (minTermDurationInDays :: ScheduledInstanceAvailability -> Lude.Maybe Lude.Int) (\s a -> s {minTermDurationInDays = a} :: ScheduledInstanceAvailability)
{-# DEPRECATED siaMinTermDurationInDays "Use generic-lens or generic-optics with 'minTermDurationInDays' instead." #-}

-- | The time period for the first schedule to start.
--
-- /Note:/ Consider using 'firstSlotStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaFirstSlotStartTime :: Lens.Lens' ScheduledInstanceAvailability (Lude.Maybe Lude.DateTime)
siaFirstSlotStartTime = Lens.lens (firstSlotStartTime :: ScheduledInstanceAvailability -> Lude.Maybe Lude.DateTime) (\s a -> s {firstSlotStartTime = a} :: ScheduledInstanceAvailability)
{-# DEPRECATED siaFirstSlotStartTime "Use generic-lens or generic-optics with 'firstSlotStartTime' instead." #-}

-- | The network platform (@EC2-Classic@ or @EC2-VPC@ ).
--
-- /Note:/ Consider using 'networkPlatform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siaNetworkPlatform :: Lens.Lens' ScheduledInstanceAvailability (Lude.Maybe Lude.Text)
siaNetworkPlatform = Lens.lens (networkPlatform :: ScheduledInstanceAvailability -> Lude.Maybe Lude.Text) (\s a -> s {networkPlatform = a} :: ScheduledInstanceAvailability)
{-# DEPRECATED siaNetworkPlatform "Use generic-lens or generic-optics with 'networkPlatform' instead." #-}

instance Lude.FromXML ScheduledInstanceAvailability where
  parseXML x =
    ScheduledInstanceAvailability'
      Lude.<$> (x Lude..@? "maxTermDurationInDays")
      Lude.<*> (x Lude..@? "platform")
      Lude.<*> (x Lude..@? "purchaseToken")
      Lude.<*> (x Lude..@? "hourlyPrice")
      Lude.<*> (x Lude..@? "availableInstanceCount")
      Lude.<*> (x Lude..@? "slotDurationInHours")
      Lude.<*> (x Lude..@? "totalScheduledInstanceHours")
      Lude.<*> (x Lude..@? "instanceType")
      Lude.<*> (x Lude..@? "recurrence")
      Lude.<*> (x Lude..@? "availabilityZone")
      Lude.<*> (x Lude..@? "minTermDurationInDays")
      Lude.<*> (x Lude..@? "firstSlotStartTime")
      Lude.<*> (x Lude..@? "networkPlatform")
