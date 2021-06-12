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
-- Module      : Network.AWS.EC2.Types.ScheduledInstanceAvailability
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstanceAvailability where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ScheduledInstanceRecurrence
import qualified Network.AWS.Lens as Lens

-- | Describes a schedule that is available for your Scheduled Instances.
--
-- /See:/ 'newScheduledInstanceAvailability' smart constructor.
data ScheduledInstanceAvailability = ScheduledInstanceAvailability'
  { -- | The platform (@Linux\/UNIX@ or @Windows@).
    platform :: Core.Maybe Core.Text,
    -- | The instance type. You can specify one of the C3, C4, M4, or R3 instance
    -- types.
    instanceType :: Core.Maybe Core.Text,
    -- | The network platform (@EC2-Classic@ or @EC2-VPC@).
    networkPlatform :: Core.Maybe Core.Text,
    -- | The number of hours in the schedule.
    slotDurationInHours :: Core.Maybe Core.Int,
    -- | The number of available instances.
    availableInstanceCount :: Core.Maybe Core.Int,
    -- | The minimum term. The only possible value is 365 days.
    minTermDurationInDays :: Core.Maybe Core.Int,
    -- | The Availability Zone.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The schedule recurrence.
    recurrence :: Core.Maybe ScheduledInstanceRecurrence,
    -- | The maximum term. The only possible value is 365 days.
    maxTermDurationInDays :: Core.Maybe Core.Int,
    -- | The total number of hours for a single instance for the entire term.
    totalScheduledInstanceHours :: Core.Maybe Core.Int,
    -- | The time period for the first schedule to start.
    firstSlotStartTime :: Core.Maybe Core.ISO8601,
    -- | The hourly price for a single instance.
    hourlyPrice :: Core.Maybe Core.Text,
    -- | The purchase token. This token expires in two hours.
    purchaseToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ScheduledInstanceAvailability' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platform', 'scheduledInstanceAvailability_platform' - The platform (@Linux\/UNIX@ or @Windows@).
--
-- 'instanceType', 'scheduledInstanceAvailability_instanceType' - The instance type. You can specify one of the C3, C4, M4, or R3 instance
-- types.
--
-- 'networkPlatform', 'scheduledInstanceAvailability_networkPlatform' - The network platform (@EC2-Classic@ or @EC2-VPC@).
--
-- 'slotDurationInHours', 'scheduledInstanceAvailability_slotDurationInHours' - The number of hours in the schedule.
--
-- 'availableInstanceCount', 'scheduledInstanceAvailability_availableInstanceCount' - The number of available instances.
--
-- 'minTermDurationInDays', 'scheduledInstanceAvailability_minTermDurationInDays' - The minimum term. The only possible value is 365 days.
--
-- 'availabilityZone', 'scheduledInstanceAvailability_availabilityZone' - The Availability Zone.
--
-- 'recurrence', 'scheduledInstanceAvailability_recurrence' - The schedule recurrence.
--
-- 'maxTermDurationInDays', 'scheduledInstanceAvailability_maxTermDurationInDays' - The maximum term. The only possible value is 365 days.
--
-- 'totalScheduledInstanceHours', 'scheduledInstanceAvailability_totalScheduledInstanceHours' - The total number of hours for a single instance for the entire term.
--
-- 'firstSlotStartTime', 'scheduledInstanceAvailability_firstSlotStartTime' - The time period for the first schedule to start.
--
-- 'hourlyPrice', 'scheduledInstanceAvailability_hourlyPrice' - The hourly price for a single instance.
--
-- 'purchaseToken', 'scheduledInstanceAvailability_purchaseToken' - The purchase token. This token expires in two hours.
newScheduledInstanceAvailability ::
  ScheduledInstanceAvailability
newScheduledInstanceAvailability =
  ScheduledInstanceAvailability'
    { platform =
        Core.Nothing,
      instanceType = Core.Nothing,
      networkPlatform = Core.Nothing,
      slotDurationInHours = Core.Nothing,
      availableInstanceCount = Core.Nothing,
      minTermDurationInDays = Core.Nothing,
      availabilityZone = Core.Nothing,
      recurrence = Core.Nothing,
      maxTermDurationInDays = Core.Nothing,
      totalScheduledInstanceHours = Core.Nothing,
      firstSlotStartTime = Core.Nothing,
      hourlyPrice = Core.Nothing,
      purchaseToken = Core.Nothing
    }

-- | The platform (@Linux\/UNIX@ or @Windows@).
scheduledInstanceAvailability_platform :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.Text)
scheduledInstanceAvailability_platform = Lens.lens (\ScheduledInstanceAvailability' {platform} -> platform) (\s@ScheduledInstanceAvailability' {} a -> s {platform = a} :: ScheduledInstanceAvailability)

-- | The instance type. You can specify one of the C3, C4, M4, or R3 instance
-- types.
scheduledInstanceAvailability_instanceType :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.Text)
scheduledInstanceAvailability_instanceType = Lens.lens (\ScheduledInstanceAvailability' {instanceType} -> instanceType) (\s@ScheduledInstanceAvailability' {} a -> s {instanceType = a} :: ScheduledInstanceAvailability)

-- | The network platform (@EC2-Classic@ or @EC2-VPC@).
scheduledInstanceAvailability_networkPlatform :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.Text)
scheduledInstanceAvailability_networkPlatform = Lens.lens (\ScheduledInstanceAvailability' {networkPlatform} -> networkPlatform) (\s@ScheduledInstanceAvailability' {} a -> s {networkPlatform = a} :: ScheduledInstanceAvailability)

-- | The number of hours in the schedule.
scheduledInstanceAvailability_slotDurationInHours :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.Int)
scheduledInstanceAvailability_slotDurationInHours = Lens.lens (\ScheduledInstanceAvailability' {slotDurationInHours} -> slotDurationInHours) (\s@ScheduledInstanceAvailability' {} a -> s {slotDurationInHours = a} :: ScheduledInstanceAvailability)

-- | The number of available instances.
scheduledInstanceAvailability_availableInstanceCount :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.Int)
scheduledInstanceAvailability_availableInstanceCount = Lens.lens (\ScheduledInstanceAvailability' {availableInstanceCount} -> availableInstanceCount) (\s@ScheduledInstanceAvailability' {} a -> s {availableInstanceCount = a} :: ScheduledInstanceAvailability)

-- | The minimum term. The only possible value is 365 days.
scheduledInstanceAvailability_minTermDurationInDays :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.Int)
scheduledInstanceAvailability_minTermDurationInDays = Lens.lens (\ScheduledInstanceAvailability' {minTermDurationInDays} -> minTermDurationInDays) (\s@ScheduledInstanceAvailability' {} a -> s {minTermDurationInDays = a} :: ScheduledInstanceAvailability)

-- | The Availability Zone.
scheduledInstanceAvailability_availabilityZone :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.Text)
scheduledInstanceAvailability_availabilityZone = Lens.lens (\ScheduledInstanceAvailability' {availabilityZone} -> availabilityZone) (\s@ScheduledInstanceAvailability' {} a -> s {availabilityZone = a} :: ScheduledInstanceAvailability)

-- | The schedule recurrence.
scheduledInstanceAvailability_recurrence :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe ScheduledInstanceRecurrence)
scheduledInstanceAvailability_recurrence = Lens.lens (\ScheduledInstanceAvailability' {recurrence} -> recurrence) (\s@ScheduledInstanceAvailability' {} a -> s {recurrence = a} :: ScheduledInstanceAvailability)

-- | The maximum term. The only possible value is 365 days.
scheduledInstanceAvailability_maxTermDurationInDays :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.Int)
scheduledInstanceAvailability_maxTermDurationInDays = Lens.lens (\ScheduledInstanceAvailability' {maxTermDurationInDays} -> maxTermDurationInDays) (\s@ScheduledInstanceAvailability' {} a -> s {maxTermDurationInDays = a} :: ScheduledInstanceAvailability)

-- | The total number of hours for a single instance for the entire term.
scheduledInstanceAvailability_totalScheduledInstanceHours :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.Int)
scheduledInstanceAvailability_totalScheduledInstanceHours = Lens.lens (\ScheduledInstanceAvailability' {totalScheduledInstanceHours} -> totalScheduledInstanceHours) (\s@ScheduledInstanceAvailability' {} a -> s {totalScheduledInstanceHours = a} :: ScheduledInstanceAvailability)

-- | The time period for the first schedule to start.
scheduledInstanceAvailability_firstSlotStartTime :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.UTCTime)
scheduledInstanceAvailability_firstSlotStartTime = Lens.lens (\ScheduledInstanceAvailability' {firstSlotStartTime} -> firstSlotStartTime) (\s@ScheduledInstanceAvailability' {} a -> s {firstSlotStartTime = a} :: ScheduledInstanceAvailability) Core.. Lens.mapping Core._Time

-- | The hourly price for a single instance.
scheduledInstanceAvailability_hourlyPrice :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.Text)
scheduledInstanceAvailability_hourlyPrice = Lens.lens (\ScheduledInstanceAvailability' {hourlyPrice} -> hourlyPrice) (\s@ScheduledInstanceAvailability' {} a -> s {hourlyPrice = a} :: ScheduledInstanceAvailability)

-- | The purchase token. This token expires in two hours.
scheduledInstanceAvailability_purchaseToken :: Lens.Lens' ScheduledInstanceAvailability (Core.Maybe Core.Text)
scheduledInstanceAvailability_purchaseToken = Lens.lens (\ScheduledInstanceAvailability' {purchaseToken} -> purchaseToken) (\s@ScheduledInstanceAvailability' {} a -> s {purchaseToken = a} :: ScheduledInstanceAvailability)

instance Core.FromXML ScheduledInstanceAvailability where
  parseXML x =
    ScheduledInstanceAvailability'
      Core.<$> (x Core..@? "platform")
      Core.<*> (x Core..@? "instanceType")
      Core.<*> (x Core..@? "networkPlatform")
      Core.<*> (x Core..@? "slotDurationInHours")
      Core.<*> (x Core..@? "availableInstanceCount")
      Core.<*> (x Core..@? "minTermDurationInDays")
      Core.<*> (x Core..@? "availabilityZone")
      Core.<*> (x Core..@? "recurrence")
      Core.<*> (x Core..@? "maxTermDurationInDays")
      Core.<*> (x Core..@? "totalScheduledInstanceHours")
      Core.<*> (x Core..@? "firstSlotStartTime")
      Core.<*> (x Core..@? "hourlyPrice")
      Core.<*> (x Core..@? "purchaseToken")

instance Core.Hashable ScheduledInstanceAvailability

instance Core.NFData ScheduledInstanceAvailability
