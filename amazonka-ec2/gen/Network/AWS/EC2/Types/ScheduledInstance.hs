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
-- Module      : Network.AWS.EC2.Types.ScheduledInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstance where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ScheduledInstanceRecurrence
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a Scheduled Instance.
--
-- /See:/ 'newScheduledInstance' smart constructor.
data ScheduledInstance = ScheduledInstance'
  { -- | The platform (@Linux\/UNIX@ or @Windows@).
    platform :: Prelude.Maybe Prelude.Text,
    -- | The instance type.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The network platform (@EC2-Classic@ or @EC2-VPC@).
    networkPlatform :: Prelude.Maybe Prelude.Text,
    -- | The number of hours in the schedule.
    slotDurationInHours :: Prelude.Maybe Prelude.Int,
    -- | The date when the Scheduled Instance was purchased.
    createDate :: Prelude.Maybe Prelude.ISO8601,
    -- | The Scheduled Instance ID.
    scheduledInstanceId :: Prelude.Maybe Prelude.Text,
    -- | The time that the previous schedule ended or will end.
    previousSlotEndTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The Availability Zone.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The schedule recurrence.
    recurrence :: Prelude.Maybe ScheduledInstanceRecurrence,
    -- | The total number of hours for a single instance for the entire term.
    totalScheduledInstanceHours :: Prelude.Maybe Prelude.Int,
    -- | The time for the next schedule to start.
    nextSlotStartTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The hourly price for a single instance.
    hourlyPrice :: Prelude.Maybe Prelude.Text,
    -- | The end date for the Scheduled Instance.
    termEndDate :: Prelude.Maybe Prelude.ISO8601,
    -- | The start date for the Scheduled Instance.
    termStartDate :: Prelude.Maybe Prelude.ISO8601,
    -- | The number of instances.
    instanceCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ScheduledInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platform', 'scheduledInstance_platform' - The platform (@Linux\/UNIX@ or @Windows@).
--
-- 'instanceType', 'scheduledInstance_instanceType' - The instance type.
--
-- 'networkPlatform', 'scheduledInstance_networkPlatform' - The network platform (@EC2-Classic@ or @EC2-VPC@).
--
-- 'slotDurationInHours', 'scheduledInstance_slotDurationInHours' - The number of hours in the schedule.
--
-- 'createDate', 'scheduledInstance_createDate' - The date when the Scheduled Instance was purchased.
--
-- 'scheduledInstanceId', 'scheduledInstance_scheduledInstanceId' - The Scheduled Instance ID.
--
-- 'previousSlotEndTime', 'scheduledInstance_previousSlotEndTime' - The time that the previous schedule ended or will end.
--
-- 'availabilityZone', 'scheduledInstance_availabilityZone' - The Availability Zone.
--
-- 'recurrence', 'scheduledInstance_recurrence' - The schedule recurrence.
--
-- 'totalScheduledInstanceHours', 'scheduledInstance_totalScheduledInstanceHours' - The total number of hours for a single instance for the entire term.
--
-- 'nextSlotStartTime', 'scheduledInstance_nextSlotStartTime' - The time for the next schedule to start.
--
-- 'hourlyPrice', 'scheduledInstance_hourlyPrice' - The hourly price for a single instance.
--
-- 'termEndDate', 'scheduledInstance_termEndDate' - The end date for the Scheduled Instance.
--
-- 'termStartDate', 'scheduledInstance_termStartDate' - The start date for the Scheduled Instance.
--
-- 'instanceCount', 'scheduledInstance_instanceCount' - The number of instances.
newScheduledInstance ::
  ScheduledInstance
newScheduledInstance =
  ScheduledInstance'
    { platform = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      networkPlatform = Prelude.Nothing,
      slotDurationInHours = Prelude.Nothing,
      createDate = Prelude.Nothing,
      scheduledInstanceId = Prelude.Nothing,
      previousSlotEndTime = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      recurrence = Prelude.Nothing,
      totalScheduledInstanceHours = Prelude.Nothing,
      nextSlotStartTime = Prelude.Nothing,
      hourlyPrice = Prelude.Nothing,
      termEndDate = Prelude.Nothing,
      termStartDate = Prelude.Nothing,
      instanceCount = Prelude.Nothing
    }

-- | The platform (@Linux\/UNIX@ or @Windows@).
scheduledInstance_platform :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Text)
scheduledInstance_platform = Lens.lens (\ScheduledInstance' {platform} -> platform) (\s@ScheduledInstance' {} a -> s {platform = a} :: ScheduledInstance)

-- | The instance type.
scheduledInstance_instanceType :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Text)
scheduledInstance_instanceType = Lens.lens (\ScheduledInstance' {instanceType} -> instanceType) (\s@ScheduledInstance' {} a -> s {instanceType = a} :: ScheduledInstance)

-- | The network platform (@EC2-Classic@ or @EC2-VPC@).
scheduledInstance_networkPlatform :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Text)
scheduledInstance_networkPlatform = Lens.lens (\ScheduledInstance' {networkPlatform} -> networkPlatform) (\s@ScheduledInstance' {} a -> s {networkPlatform = a} :: ScheduledInstance)

-- | The number of hours in the schedule.
scheduledInstance_slotDurationInHours :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Int)
scheduledInstance_slotDurationInHours = Lens.lens (\ScheduledInstance' {slotDurationInHours} -> slotDurationInHours) (\s@ScheduledInstance' {} a -> s {slotDurationInHours = a} :: ScheduledInstance)

-- | The date when the Scheduled Instance was purchased.
scheduledInstance_createDate :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.UTCTime)
scheduledInstance_createDate = Lens.lens (\ScheduledInstance' {createDate} -> createDate) (\s@ScheduledInstance' {} a -> s {createDate = a} :: ScheduledInstance) Prelude.. Lens.mapping Prelude._Time

-- | The Scheduled Instance ID.
scheduledInstance_scheduledInstanceId :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Text)
scheduledInstance_scheduledInstanceId = Lens.lens (\ScheduledInstance' {scheduledInstanceId} -> scheduledInstanceId) (\s@ScheduledInstance' {} a -> s {scheduledInstanceId = a} :: ScheduledInstance)

-- | The time that the previous schedule ended or will end.
scheduledInstance_previousSlotEndTime :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.UTCTime)
scheduledInstance_previousSlotEndTime = Lens.lens (\ScheduledInstance' {previousSlotEndTime} -> previousSlotEndTime) (\s@ScheduledInstance' {} a -> s {previousSlotEndTime = a} :: ScheduledInstance) Prelude.. Lens.mapping Prelude._Time

-- | The Availability Zone.
scheduledInstance_availabilityZone :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Text)
scheduledInstance_availabilityZone = Lens.lens (\ScheduledInstance' {availabilityZone} -> availabilityZone) (\s@ScheduledInstance' {} a -> s {availabilityZone = a} :: ScheduledInstance)

-- | The schedule recurrence.
scheduledInstance_recurrence :: Lens.Lens' ScheduledInstance (Prelude.Maybe ScheduledInstanceRecurrence)
scheduledInstance_recurrence = Lens.lens (\ScheduledInstance' {recurrence} -> recurrence) (\s@ScheduledInstance' {} a -> s {recurrence = a} :: ScheduledInstance)

-- | The total number of hours for a single instance for the entire term.
scheduledInstance_totalScheduledInstanceHours :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Int)
scheduledInstance_totalScheduledInstanceHours = Lens.lens (\ScheduledInstance' {totalScheduledInstanceHours} -> totalScheduledInstanceHours) (\s@ScheduledInstance' {} a -> s {totalScheduledInstanceHours = a} :: ScheduledInstance)

-- | The time for the next schedule to start.
scheduledInstance_nextSlotStartTime :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.UTCTime)
scheduledInstance_nextSlotStartTime = Lens.lens (\ScheduledInstance' {nextSlotStartTime} -> nextSlotStartTime) (\s@ScheduledInstance' {} a -> s {nextSlotStartTime = a} :: ScheduledInstance) Prelude.. Lens.mapping Prelude._Time

-- | The hourly price for a single instance.
scheduledInstance_hourlyPrice :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Text)
scheduledInstance_hourlyPrice = Lens.lens (\ScheduledInstance' {hourlyPrice} -> hourlyPrice) (\s@ScheduledInstance' {} a -> s {hourlyPrice = a} :: ScheduledInstance)

-- | The end date for the Scheduled Instance.
scheduledInstance_termEndDate :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.UTCTime)
scheduledInstance_termEndDate = Lens.lens (\ScheduledInstance' {termEndDate} -> termEndDate) (\s@ScheduledInstance' {} a -> s {termEndDate = a} :: ScheduledInstance) Prelude.. Lens.mapping Prelude._Time

-- | The start date for the Scheduled Instance.
scheduledInstance_termStartDate :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.UTCTime)
scheduledInstance_termStartDate = Lens.lens (\ScheduledInstance' {termStartDate} -> termStartDate) (\s@ScheduledInstance' {} a -> s {termStartDate = a} :: ScheduledInstance) Prelude.. Lens.mapping Prelude._Time

-- | The number of instances.
scheduledInstance_instanceCount :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Int)
scheduledInstance_instanceCount = Lens.lens (\ScheduledInstance' {instanceCount} -> instanceCount) (\s@ScheduledInstance' {} a -> s {instanceCount = a} :: ScheduledInstance)

instance Prelude.FromXML ScheduledInstance where
  parseXML x =
    ScheduledInstance'
      Prelude.<$> (x Prelude..@? "platform")
      Prelude.<*> (x Prelude..@? "instanceType")
      Prelude.<*> (x Prelude..@? "networkPlatform")
      Prelude.<*> (x Prelude..@? "slotDurationInHours")
      Prelude.<*> (x Prelude..@? "createDate")
      Prelude.<*> (x Prelude..@? "scheduledInstanceId")
      Prelude.<*> (x Prelude..@? "previousSlotEndTime")
      Prelude.<*> (x Prelude..@? "availabilityZone")
      Prelude.<*> (x Prelude..@? "recurrence")
      Prelude.<*> (x Prelude..@? "totalScheduledInstanceHours")
      Prelude.<*> (x Prelude..@? "nextSlotStartTime")
      Prelude.<*> (x Prelude..@? "hourlyPrice")
      Prelude.<*> (x Prelude..@? "termEndDate")
      Prelude.<*> (x Prelude..@? "termStartDate")
      Prelude.<*> (x Prelude..@? "instanceCount")

instance Prelude.Hashable ScheduledInstance

instance Prelude.NFData ScheduledInstance
