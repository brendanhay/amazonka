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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | The Availability Zone.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The date when the Scheduled Instance was purchased.
    createDate :: Prelude.Maybe Data.ISO8601,
    -- | The hourly price for a single instance.
    hourlyPrice :: Prelude.Maybe Prelude.Text,
    -- | The number of instances.
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | The instance type.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The network platform (@EC2-Classic@ or @EC2-VPC@).
    networkPlatform :: Prelude.Maybe Prelude.Text,
    -- | The time for the next schedule to start.
    nextSlotStartTime :: Prelude.Maybe Data.ISO8601,
    -- | The platform (@Linux\/UNIX@ or @Windows@).
    platform :: Prelude.Maybe Prelude.Text,
    -- | The time that the previous schedule ended or will end.
    previousSlotEndTime :: Prelude.Maybe Data.ISO8601,
    -- | The schedule recurrence.
    recurrence :: Prelude.Maybe ScheduledInstanceRecurrence,
    -- | The Scheduled Instance ID.
    scheduledInstanceId :: Prelude.Maybe Prelude.Text,
    -- | The number of hours in the schedule.
    slotDurationInHours :: Prelude.Maybe Prelude.Int,
    -- | The end date for the Scheduled Instance.
    termEndDate :: Prelude.Maybe Data.ISO8601,
    -- | The start date for the Scheduled Instance.
    termStartDate :: Prelude.Maybe Data.ISO8601,
    -- | The total number of hours for a single instance for the entire term.
    totalScheduledInstanceHours :: Prelude.Maybe Prelude.Int
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
-- 'availabilityZone', 'scheduledInstance_availabilityZone' - The Availability Zone.
--
-- 'createDate', 'scheduledInstance_createDate' - The date when the Scheduled Instance was purchased.
--
-- 'hourlyPrice', 'scheduledInstance_hourlyPrice' - The hourly price for a single instance.
--
-- 'instanceCount', 'scheduledInstance_instanceCount' - The number of instances.
--
-- 'instanceType', 'scheduledInstance_instanceType' - The instance type.
--
-- 'networkPlatform', 'scheduledInstance_networkPlatform' - The network platform (@EC2-Classic@ or @EC2-VPC@).
--
-- 'nextSlotStartTime', 'scheduledInstance_nextSlotStartTime' - The time for the next schedule to start.
--
-- 'platform', 'scheduledInstance_platform' - The platform (@Linux\/UNIX@ or @Windows@).
--
-- 'previousSlotEndTime', 'scheduledInstance_previousSlotEndTime' - The time that the previous schedule ended or will end.
--
-- 'recurrence', 'scheduledInstance_recurrence' - The schedule recurrence.
--
-- 'scheduledInstanceId', 'scheduledInstance_scheduledInstanceId' - The Scheduled Instance ID.
--
-- 'slotDurationInHours', 'scheduledInstance_slotDurationInHours' - The number of hours in the schedule.
--
-- 'termEndDate', 'scheduledInstance_termEndDate' - The end date for the Scheduled Instance.
--
-- 'termStartDate', 'scheduledInstance_termStartDate' - The start date for the Scheduled Instance.
--
-- 'totalScheduledInstanceHours', 'scheduledInstance_totalScheduledInstanceHours' - The total number of hours for a single instance for the entire term.
newScheduledInstance ::
  ScheduledInstance
newScheduledInstance =
  ScheduledInstance'
    { availabilityZone =
        Prelude.Nothing,
      createDate = Prelude.Nothing,
      hourlyPrice = Prelude.Nothing,
      instanceCount = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      networkPlatform = Prelude.Nothing,
      nextSlotStartTime = Prelude.Nothing,
      platform = Prelude.Nothing,
      previousSlotEndTime = Prelude.Nothing,
      recurrence = Prelude.Nothing,
      scheduledInstanceId = Prelude.Nothing,
      slotDurationInHours = Prelude.Nothing,
      termEndDate = Prelude.Nothing,
      termStartDate = Prelude.Nothing,
      totalScheduledInstanceHours = Prelude.Nothing
    }

-- | The Availability Zone.
scheduledInstance_availabilityZone :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Text)
scheduledInstance_availabilityZone = Lens.lens (\ScheduledInstance' {availabilityZone} -> availabilityZone) (\s@ScheduledInstance' {} a -> s {availabilityZone = a} :: ScheduledInstance)

-- | The date when the Scheduled Instance was purchased.
scheduledInstance_createDate :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.UTCTime)
scheduledInstance_createDate = Lens.lens (\ScheduledInstance' {createDate} -> createDate) (\s@ScheduledInstance' {} a -> s {createDate = a} :: ScheduledInstance) Prelude.. Lens.mapping Data._Time

-- | The hourly price for a single instance.
scheduledInstance_hourlyPrice :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Text)
scheduledInstance_hourlyPrice = Lens.lens (\ScheduledInstance' {hourlyPrice} -> hourlyPrice) (\s@ScheduledInstance' {} a -> s {hourlyPrice = a} :: ScheduledInstance)

-- | The number of instances.
scheduledInstance_instanceCount :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Int)
scheduledInstance_instanceCount = Lens.lens (\ScheduledInstance' {instanceCount} -> instanceCount) (\s@ScheduledInstance' {} a -> s {instanceCount = a} :: ScheduledInstance)

-- | The instance type.
scheduledInstance_instanceType :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Text)
scheduledInstance_instanceType = Lens.lens (\ScheduledInstance' {instanceType} -> instanceType) (\s@ScheduledInstance' {} a -> s {instanceType = a} :: ScheduledInstance)

-- | The network platform (@EC2-Classic@ or @EC2-VPC@).
scheduledInstance_networkPlatform :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Text)
scheduledInstance_networkPlatform = Lens.lens (\ScheduledInstance' {networkPlatform} -> networkPlatform) (\s@ScheduledInstance' {} a -> s {networkPlatform = a} :: ScheduledInstance)

-- | The time for the next schedule to start.
scheduledInstance_nextSlotStartTime :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.UTCTime)
scheduledInstance_nextSlotStartTime = Lens.lens (\ScheduledInstance' {nextSlotStartTime} -> nextSlotStartTime) (\s@ScheduledInstance' {} a -> s {nextSlotStartTime = a} :: ScheduledInstance) Prelude.. Lens.mapping Data._Time

-- | The platform (@Linux\/UNIX@ or @Windows@).
scheduledInstance_platform :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Text)
scheduledInstance_platform = Lens.lens (\ScheduledInstance' {platform} -> platform) (\s@ScheduledInstance' {} a -> s {platform = a} :: ScheduledInstance)

-- | The time that the previous schedule ended or will end.
scheduledInstance_previousSlotEndTime :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.UTCTime)
scheduledInstance_previousSlotEndTime = Lens.lens (\ScheduledInstance' {previousSlotEndTime} -> previousSlotEndTime) (\s@ScheduledInstance' {} a -> s {previousSlotEndTime = a} :: ScheduledInstance) Prelude.. Lens.mapping Data._Time

-- | The schedule recurrence.
scheduledInstance_recurrence :: Lens.Lens' ScheduledInstance (Prelude.Maybe ScheduledInstanceRecurrence)
scheduledInstance_recurrence = Lens.lens (\ScheduledInstance' {recurrence} -> recurrence) (\s@ScheduledInstance' {} a -> s {recurrence = a} :: ScheduledInstance)

-- | The Scheduled Instance ID.
scheduledInstance_scheduledInstanceId :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Text)
scheduledInstance_scheduledInstanceId = Lens.lens (\ScheduledInstance' {scheduledInstanceId} -> scheduledInstanceId) (\s@ScheduledInstance' {} a -> s {scheduledInstanceId = a} :: ScheduledInstance)

-- | The number of hours in the schedule.
scheduledInstance_slotDurationInHours :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Int)
scheduledInstance_slotDurationInHours = Lens.lens (\ScheduledInstance' {slotDurationInHours} -> slotDurationInHours) (\s@ScheduledInstance' {} a -> s {slotDurationInHours = a} :: ScheduledInstance)

-- | The end date for the Scheduled Instance.
scheduledInstance_termEndDate :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.UTCTime)
scheduledInstance_termEndDate = Lens.lens (\ScheduledInstance' {termEndDate} -> termEndDate) (\s@ScheduledInstance' {} a -> s {termEndDate = a} :: ScheduledInstance) Prelude.. Lens.mapping Data._Time

-- | The start date for the Scheduled Instance.
scheduledInstance_termStartDate :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.UTCTime)
scheduledInstance_termStartDate = Lens.lens (\ScheduledInstance' {termStartDate} -> termStartDate) (\s@ScheduledInstance' {} a -> s {termStartDate = a} :: ScheduledInstance) Prelude.. Lens.mapping Data._Time

-- | The total number of hours for a single instance for the entire term.
scheduledInstance_totalScheduledInstanceHours :: Lens.Lens' ScheduledInstance (Prelude.Maybe Prelude.Int)
scheduledInstance_totalScheduledInstanceHours = Lens.lens (\ScheduledInstance' {totalScheduledInstanceHours} -> totalScheduledInstanceHours) (\s@ScheduledInstance' {} a -> s {totalScheduledInstanceHours = a} :: ScheduledInstance)

instance Data.FromXML ScheduledInstance where
  parseXML x =
    ScheduledInstance'
      Prelude.<$> (x Data..@? "availabilityZone")
      Prelude.<*> (x Data..@? "createDate")
      Prelude.<*> (x Data..@? "hourlyPrice")
      Prelude.<*> (x Data..@? "instanceCount")
      Prelude.<*> (x Data..@? "instanceType")
      Prelude.<*> (x Data..@? "networkPlatform")
      Prelude.<*> (x Data..@? "nextSlotStartTime")
      Prelude.<*> (x Data..@? "platform")
      Prelude.<*> (x Data..@? "previousSlotEndTime")
      Prelude.<*> (x Data..@? "recurrence")
      Prelude.<*> (x Data..@? "scheduledInstanceId")
      Prelude.<*> (x Data..@? "slotDurationInHours")
      Prelude.<*> (x Data..@? "termEndDate")
      Prelude.<*> (x Data..@? "termStartDate")
      Prelude.<*> (x Data..@? "totalScheduledInstanceHours")

instance Prelude.Hashable ScheduledInstance where
  hashWithSalt _salt ScheduledInstance' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` hourlyPrice
      `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` networkPlatform
      `Prelude.hashWithSalt` nextSlotStartTime
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` previousSlotEndTime
      `Prelude.hashWithSalt` recurrence
      `Prelude.hashWithSalt` scheduledInstanceId
      `Prelude.hashWithSalt` slotDurationInHours
      `Prelude.hashWithSalt` termEndDate
      `Prelude.hashWithSalt` termStartDate
      `Prelude.hashWithSalt` totalScheduledInstanceHours

instance Prelude.NFData ScheduledInstance where
  rnf ScheduledInstance' {..} =
    Prelude.rnf availabilityZone `Prelude.seq`
      Prelude.rnf createDate `Prelude.seq`
        Prelude.rnf hourlyPrice `Prelude.seq`
          Prelude.rnf instanceCount `Prelude.seq`
            Prelude.rnf instanceType `Prelude.seq`
              Prelude.rnf networkPlatform `Prelude.seq`
                Prelude.rnf nextSlotStartTime `Prelude.seq`
                  Prelude.rnf platform `Prelude.seq`
                    Prelude.rnf previousSlotEndTime `Prelude.seq`
                      Prelude.rnf recurrence `Prelude.seq`
                        Prelude.rnf scheduledInstanceId `Prelude.seq`
                          Prelude.rnf slotDurationInHours `Prelude.seq`
                            Prelude.rnf termEndDate `Prelude.seq`
                              Prelude.rnf termStartDate `Prelude.seq`
                                Prelude.rnf totalScheduledInstanceHours
