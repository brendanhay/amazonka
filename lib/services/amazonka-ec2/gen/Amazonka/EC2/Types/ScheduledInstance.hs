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
-- Module      : Amazonka.EC2.Types.ScheduledInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ScheduledInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ScheduledInstanceRecurrence
import qualified Amazonka.Prelude as Prelude

-- | Describes a Scheduled Instance.
--
-- /See:/ 'newScheduledInstance' smart constructor.
data ScheduledInstance = ScheduledInstance'
  { -- | The start date for the Scheduled Instance.
    termStartDate :: Prelude.Maybe Data.ISO8601,
    -- | The time for the next schedule to start.
    nextSlotStartTime :: Prelude.Maybe Data.ISO8601,
    -- | The hourly price for a single instance.
    hourlyPrice :: Prelude.Maybe Prelude.Text,
    -- | The total number of hours for a single instance for the entire term.
    totalScheduledInstanceHours :: Prelude.Maybe Prelude.Int,
    -- | The Scheduled Instance ID.
    scheduledInstanceId :: Prelude.Maybe Prelude.Text,
    -- | The network platform (@EC2-Classic@ or @EC2-VPC@).
    networkPlatform :: Prelude.Maybe Prelude.Text,
    -- | The platform (@Linux\/UNIX@ or @Windows@).
    platform :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The instance type.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The number of instances.
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | The date when the Scheduled Instance was purchased.
    createDate :: Prelude.Maybe Data.ISO8601,
    -- | The schedule recurrence.
    recurrence :: Prelude.Maybe ScheduledInstanceRecurrence,
    -- | The time that the previous schedule ended or will end.
    previousSlotEndTime :: Prelude.Maybe Data.ISO8601,
    -- | The end date for the Scheduled Instance.
    termEndDate :: Prelude.Maybe Data.ISO8601,
    -- | The number of hours in the schedule.
    slotDurationInHours :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'termStartDate', 'scheduledInstance_termStartDate' - The start date for the Scheduled Instance.
--
-- 'nextSlotStartTime', 'scheduledInstance_nextSlotStartTime' - The time for the next schedule to start.
--
-- 'hourlyPrice', 'scheduledInstance_hourlyPrice' - The hourly price for a single instance.
--
-- 'totalScheduledInstanceHours', 'scheduledInstance_totalScheduledInstanceHours' - The total number of hours for a single instance for the entire term.
--
-- 'scheduledInstanceId', 'scheduledInstance_scheduledInstanceId' - The Scheduled Instance ID.
--
-- 'networkPlatform', 'scheduledInstance_networkPlatform' - The network platform (@EC2-Classic@ or @EC2-VPC@).
--
-- 'platform', 'scheduledInstance_platform' - The platform (@Linux\/UNIX@ or @Windows@).
--
-- 'availabilityZone', 'scheduledInstance_availabilityZone' - The Availability Zone.
--
-- 'instanceType', 'scheduledInstance_instanceType' - The instance type.
--
-- 'instanceCount', 'scheduledInstance_instanceCount' - The number of instances.
--
-- 'createDate', 'scheduledInstance_createDate' - The date when the Scheduled Instance was purchased.
--
-- 'recurrence', 'scheduledInstance_recurrence' - The schedule recurrence.
--
-- 'previousSlotEndTime', 'scheduledInstance_previousSlotEndTime' - The time that the previous schedule ended or will end.
--
-- 'termEndDate', 'scheduledInstance_termEndDate' - The end date for the Scheduled Instance.
--
-- 'slotDurationInHours', 'scheduledInstance_slotDurationInHours' - The number of hours in the schedule.
newScheduledInstance ::
  ScheduledInstance
newScheduledInstance =
  ScheduledInstance'
    { termStartDate = Prelude.Nothing,
      nextSlotStartTime = Prelude.Nothing,
      hourlyPrice = Prelude.Nothing,
      totalScheduledInstanceHours = Prelude.Nothing,
      scheduledInstanceId = Prelude.Nothing,
      networkPlatform = Prelude.Nothing,
      platform = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      instanceCount = Prelude.Nothing,
      createDate = Prelude.Nothing,
      recurrence = Prelude.Nothing,
      previousSlotEndTime = Prelude.Nothing,
      termEndDate = Prelude.Nothing,
      slotDurationInHours = Prelude.Nothing
    }

-- | The start date for the Scheduled Instance.
scheduledInstance_termStartDate :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.UTCTime)
scheduledInstance_termStartDate = Lens.lens (\ScheduledInstance' {termStartDate} -> termStartDate) (\s@ScheduledInstance' {} a -> s {termStartDate = a} :: ScheduledInstance) Prelude.. Lens.mapping Data._Time

-- | The time for the next schedule to start.
scheduledInstance_nextSlotStartTime :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.UTCTime)
scheduledInstance_nextSlotStartTime = Lens.lens (\ScheduledInstance' {nextSlotStartTime} -> nextSlotStartTime) (\s@ScheduledInstance' {} a -> s {nextSlotStartTime = a} :: ScheduledInstance) Prelude.. Lens.mapping Data._Time

-- | The hourly price for a single instance.
scheduledInstance_hourlyPrice :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Text)
scheduledInstance_hourlyPrice = Lens.lens (\ScheduledInstance' {hourlyPrice} -> hourlyPrice) (\s@ScheduledInstance' {} a -> s {hourlyPrice = a} :: ScheduledInstance)

-- | The total number of hours for a single instance for the entire term.
scheduledInstance_totalScheduledInstanceHours :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Int)
scheduledInstance_totalScheduledInstanceHours = Lens.lens (\ScheduledInstance' {totalScheduledInstanceHours} -> totalScheduledInstanceHours) (\s@ScheduledInstance' {} a -> s {totalScheduledInstanceHours = a} :: ScheduledInstance)

-- | The Scheduled Instance ID.
scheduledInstance_scheduledInstanceId :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Text)
scheduledInstance_scheduledInstanceId = Lens.lens (\ScheduledInstance' {scheduledInstanceId} -> scheduledInstanceId) (\s@ScheduledInstance' {} a -> s {scheduledInstanceId = a} :: ScheduledInstance)

-- | The network platform (@EC2-Classic@ or @EC2-VPC@).
scheduledInstance_networkPlatform :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Text)
scheduledInstance_networkPlatform = Lens.lens (\ScheduledInstance' {networkPlatform} -> networkPlatform) (\s@ScheduledInstance' {} a -> s {networkPlatform = a} :: ScheduledInstance)

-- | The platform (@Linux\/UNIX@ or @Windows@).
scheduledInstance_platform :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Text)
scheduledInstance_platform = Lens.lens (\ScheduledInstance' {platform} -> platform) (\s@ScheduledInstance' {} a -> s {platform = a} :: ScheduledInstance)

-- | The Availability Zone.
scheduledInstance_availabilityZone :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Text)
scheduledInstance_availabilityZone = Lens.lens (\ScheduledInstance' {availabilityZone} -> availabilityZone) (\s@ScheduledInstance' {} a -> s {availabilityZone = a} :: ScheduledInstance)

-- | The instance type.
scheduledInstance_instanceType :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Text)
scheduledInstance_instanceType = Lens.lens (\ScheduledInstance' {instanceType} -> instanceType) (\s@ScheduledInstance' {} a -> s {instanceType = a} :: ScheduledInstance)

-- | The number of instances.
scheduledInstance_instanceCount :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Int)
scheduledInstance_instanceCount = Lens.lens (\ScheduledInstance' {instanceCount} -> instanceCount) (\s@ScheduledInstance' {} a -> s {instanceCount = a} :: ScheduledInstance)

-- | The date when the Scheduled Instance was purchased.
scheduledInstance_createDate :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.UTCTime)
scheduledInstance_createDate = Lens.lens (\ScheduledInstance' {createDate} -> createDate) (\s@ScheduledInstance' {} a -> s {createDate = a} :: ScheduledInstance) Prelude.. Lens.mapping Data._Time

-- | The schedule recurrence.
scheduledInstance_recurrence :: Lens.Lens' ScheduledInstance (Prelude.Maybe ScheduledInstanceRecurrence)
scheduledInstance_recurrence = Lens.lens (\ScheduledInstance' {recurrence} -> recurrence) (\s@ScheduledInstance' {} a -> s {recurrence = a} :: ScheduledInstance)

-- | The time that the previous schedule ended or will end.
scheduledInstance_previousSlotEndTime :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.UTCTime)
scheduledInstance_previousSlotEndTime = Lens.lens (\ScheduledInstance' {previousSlotEndTime} -> previousSlotEndTime) (\s@ScheduledInstance' {} a -> s {previousSlotEndTime = a} :: ScheduledInstance) Prelude.. Lens.mapping Data._Time

-- | The end date for the Scheduled Instance.
scheduledInstance_termEndDate :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.UTCTime)
scheduledInstance_termEndDate = Lens.lens (\ScheduledInstance' {termEndDate} -> termEndDate) (\s@ScheduledInstance' {} a -> s {termEndDate = a} :: ScheduledInstance) Prelude.. Lens.mapping Data._Time

-- | The number of hours in the schedule.
scheduledInstance_slotDurationInHours :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Int)
scheduledInstance_slotDurationInHours = Lens.lens (\ScheduledInstance' {slotDurationInHours} -> slotDurationInHours) (\s@ScheduledInstance' {} a -> s {slotDurationInHours = a} :: ScheduledInstance)

instance Data.FromXML ScheduledInstance where
  parseXML x =
    ScheduledInstance'
      Prelude.<$> (x Data..@? "termStartDate")
      Prelude.<*> (x Data..@? "nextSlotStartTime")
      Prelude.<*> (x Data..@? "hourlyPrice")
      Prelude.<*> (x Data..@? "totalScheduledInstanceHours")
      Prelude.<*> (x Data..@? "scheduledInstanceId")
      Prelude.<*> (x Data..@? "networkPlatform")
      Prelude.<*> (x Data..@? "platform")
      Prelude.<*> (x Data..@? "availabilityZone")
      Prelude.<*> (x Data..@? "instanceType")
      Prelude.<*> (x Data..@? "instanceCount")
      Prelude.<*> (x Data..@? "createDate")
      Prelude.<*> (x Data..@? "recurrence")
      Prelude.<*> (x Data..@? "previousSlotEndTime")
      Prelude.<*> (x Data..@? "termEndDate")
      Prelude.<*> (x Data..@? "slotDurationInHours")

instance Prelude.Hashable ScheduledInstance where
  hashWithSalt _salt ScheduledInstance' {..} =
    _salt `Prelude.hashWithSalt` termStartDate
      `Prelude.hashWithSalt` nextSlotStartTime
      `Prelude.hashWithSalt` hourlyPrice
      `Prelude.hashWithSalt` totalScheduledInstanceHours
      `Prelude.hashWithSalt` scheduledInstanceId
      `Prelude.hashWithSalt` networkPlatform
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` recurrence
      `Prelude.hashWithSalt` previousSlotEndTime
      `Prelude.hashWithSalt` termEndDate
      `Prelude.hashWithSalt` slotDurationInHours

instance Prelude.NFData ScheduledInstance where
  rnf ScheduledInstance' {..} =
    Prelude.rnf termStartDate
      `Prelude.seq` Prelude.rnf nextSlotStartTime
      `Prelude.seq` Prelude.rnf hourlyPrice
      `Prelude.seq` Prelude.rnf totalScheduledInstanceHours
      `Prelude.seq` Prelude.rnf scheduledInstanceId
      `Prelude.seq` Prelude.rnf networkPlatform
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf instanceCount
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf recurrence
      `Prelude.seq` Prelude.rnf previousSlotEndTime
      `Prelude.seq` Prelude.rnf termEndDate
      `Prelude.seq` Prelude.rnf slotDurationInHours
