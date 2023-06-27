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
-- Module      : Amazonka.EC2.Types.ScheduledInstanceAvailability
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ScheduledInstanceAvailability where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ScheduledInstanceRecurrence
import qualified Amazonka.Prelude as Prelude

-- | Describes a schedule that is available for your Scheduled Instances.
--
-- /See:/ 'newScheduledInstanceAvailability' smart constructor.
data ScheduledInstanceAvailability = ScheduledInstanceAvailability'
  { -- | The Availability Zone.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The number of available instances.
    availableInstanceCount :: Prelude.Maybe Prelude.Int,
    -- | The time period for the first schedule to start.
    firstSlotStartTime :: Prelude.Maybe Data.ISO8601,
    -- | The hourly price for a single instance.
    hourlyPrice :: Prelude.Maybe Prelude.Text,
    -- | The instance type. You can specify one of the C3, C4, M4, or R3 instance
    -- types.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The maximum term. The only possible value is 365 days.
    maxTermDurationInDays :: Prelude.Maybe Prelude.Int,
    -- | The minimum term. The only possible value is 365 days.
    minTermDurationInDays :: Prelude.Maybe Prelude.Int,
    -- | The network platform.
    networkPlatform :: Prelude.Maybe Prelude.Text,
    -- | The platform (@Linux\/UNIX@ or @Windows@).
    platform :: Prelude.Maybe Prelude.Text,
    -- | The purchase token. This token expires in two hours.
    purchaseToken :: Prelude.Maybe Prelude.Text,
    -- | The schedule recurrence.
    recurrence :: Prelude.Maybe ScheduledInstanceRecurrence,
    -- | The number of hours in the schedule.
    slotDurationInHours :: Prelude.Maybe Prelude.Int,
    -- | The total number of hours for a single instance for the entire term.
    totalScheduledInstanceHours :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledInstanceAvailability' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'scheduledInstanceAvailability_availabilityZone' - The Availability Zone.
--
-- 'availableInstanceCount', 'scheduledInstanceAvailability_availableInstanceCount' - The number of available instances.
--
-- 'firstSlotStartTime', 'scheduledInstanceAvailability_firstSlotStartTime' - The time period for the first schedule to start.
--
-- 'hourlyPrice', 'scheduledInstanceAvailability_hourlyPrice' - The hourly price for a single instance.
--
-- 'instanceType', 'scheduledInstanceAvailability_instanceType' - The instance type. You can specify one of the C3, C4, M4, or R3 instance
-- types.
--
-- 'maxTermDurationInDays', 'scheduledInstanceAvailability_maxTermDurationInDays' - The maximum term. The only possible value is 365 days.
--
-- 'minTermDurationInDays', 'scheduledInstanceAvailability_minTermDurationInDays' - The minimum term. The only possible value is 365 days.
--
-- 'networkPlatform', 'scheduledInstanceAvailability_networkPlatform' - The network platform.
--
-- 'platform', 'scheduledInstanceAvailability_platform' - The platform (@Linux\/UNIX@ or @Windows@).
--
-- 'purchaseToken', 'scheduledInstanceAvailability_purchaseToken' - The purchase token. This token expires in two hours.
--
-- 'recurrence', 'scheduledInstanceAvailability_recurrence' - The schedule recurrence.
--
-- 'slotDurationInHours', 'scheduledInstanceAvailability_slotDurationInHours' - The number of hours in the schedule.
--
-- 'totalScheduledInstanceHours', 'scheduledInstanceAvailability_totalScheduledInstanceHours' - The total number of hours for a single instance for the entire term.
newScheduledInstanceAvailability ::
  ScheduledInstanceAvailability
newScheduledInstanceAvailability =
  ScheduledInstanceAvailability'
    { availabilityZone =
        Prelude.Nothing,
      availableInstanceCount = Prelude.Nothing,
      firstSlotStartTime = Prelude.Nothing,
      hourlyPrice = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      maxTermDurationInDays = Prelude.Nothing,
      minTermDurationInDays = Prelude.Nothing,
      networkPlatform = Prelude.Nothing,
      platform = Prelude.Nothing,
      purchaseToken = Prelude.Nothing,
      recurrence = Prelude.Nothing,
      slotDurationInHours = Prelude.Nothing,
      totalScheduledInstanceHours =
        Prelude.Nothing
    }

-- | The Availability Zone.
scheduledInstanceAvailability_availabilityZone :: Lens.Lens' ScheduledInstanceAvailability (Prelude.Maybe Prelude.Text)
scheduledInstanceAvailability_availabilityZone = Lens.lens (\ScheduledInstanceAvailability' {availabilityZone} -> availabilityZone) (\s@ScheduledInstanceAvailability' {} a -> s {availabilityZone = a} :: ScheduledInstanceAvailability)

-- | The number of available instances.
scheduledInstanceAvailability_availableInstanceCount :: Lens.Lens' ScheduledInstanceAvailability (Prelude.Maybe Prelude.Int)
scheduledInstanceAvailability_availableInstanceCount = Lens.lens (\ScheduledInstanceAvailability' {availableInstanceCount} -> availableInstanceCount) (\s@ScheduledInstanceAvailability' {} a -> s {availableInstanceCount = a} :: ScheduledInstanceAvailability)

-- | The time period for the first schedule to start.
scheduledInstanceAvailability_firstSlotStartTime :: Lens.Lens' ScheduledInstanceAvailability (Prelude.Maybe Prelude.UTCTime)
scheduledInstanceAvailability_firstSlotStartTime = Lens.lens (\ScheduledInstanceAvailability' {firstSlotStartTime} -> firstSlotStartTime) (\s@ScheduledInstanceAvailability' {} a -> s {firstSlotStartTime = a} :: ScheduledInstanceAvailability) Prelude.. Lens.mapping Data._Time

-- | The hourly price for a single instance.
scheduledInstanceAvailability_hourlyPrice :: Lens.Lens' ScheduledInstanceAvailability (Prelude.Maybe Prelude.Text)
scheduledInstanceAvailability_hourlyPrice = Lens.lens (\ScheduledInstanceAvailability' {hourlyPrice} -> hourlyPrice) (\s@ScheduledInstanceAvailability' {} a -> s {hourlyPrice = a} :: ScheduledInstanceAvailability)

-- | The instance type. You can specify one of the C3, C4, M4, or R3 instance
-- types.
scheduledInstanceAvailability_instanceType :: Lens.Lens' ScheduledInstanceAvailability (Prelude.Maybe Prelude.Text)
scheduledInstanceAvailability_instanceType = Lens.lens (\ScheduledInstanceAvailability' {instanceType} -> instanceType) (\s@ScheduledInstanceAvailability' {} a -> s {instanceType = a} :: ScheduledInstanceAvailability)

-- | The maximum term. The only possible value is 365 days.
scheduledInstanceAvailability_maxTermDurationInDays :: Lens.Lens' ScheduledInstanceAvailability (Prelude.Maybe Prelude.Int)
scheduledInstanceAvailability_maxTermDurationInDays = Lens.lens (\ScheduledInstanceAvailability' {maxTermDurationInDays} -> maxTermDurationInDays) (\s@ScheduledInstanceAvailability' {} a -> s {maxTermDurationInDays = a} :: ScheduledInstanceAvailability)

-- | The minimum term. The only possible value is 365 days.
scheduledInstanceAvailability_minTermDurationInDays :: Lens.Lens' ScheduledInstanceAvailability (Prelude.Maybe Prelude.Int)
scheduledInstanceAvailability_minTermDurationInDays = Lens.lens (\ScheduledInstanceAvailability' {minTermDurationInDays} -> minTermDurationInDays) (\s@ScheduledInstanceAvailability' {} a -> s {minTermDurationInDays = a} :: ScheduledInstanceAvailability)

-- | The network platform.
scheduledInstanceAvailability_networkPlatform :: Lens.Lens' ScheduledInstanceAvailability (Prelude.Maybe Prelude.Text)
scheduledInstanceAvailability_networkPlatform = Lens.lens (\ScheduledInstanceAvailability' {networkPlatform} -> networkPlatform) (\s@ScheduledInstanceAvailability' {} a -> s {networkPlatform = a} :: ScheduledInstanceAvailability)

-- | The platform (@Linux\/UNIX@ or @Windows@).
scheduledInstanceAvailability_platform :: Lens.Lens' ScheduledInstanceAvailability (Prelude.Maybe Prelude.Text)
scheduledInstanceAvailability_platform = Lens.lens (\ScheduledInstanceAvailability' {platform} -> platform) (\s@ScheduledInstanceAvailability' {} a -> s {platform = a} :: ScheduledInstanceAvailability)

-- | The purchase token. This token expires in two hours.
scheduledInstanceAvailability_purchaseToken :: Lens.Lens' ScheduledInstanceAvailability (Prelude.Maybe Prelude.Text)
scheduledInstanceAvailability_purchaseToken = Lens.lens (\ScheduledInstanceAvailability' {purchaseToken} -> purchaseToken) (\s@ScheduledInstanceAvailability' {} a -> s {purchaseToken = a} :: ScheduledInstanceAvailability)

-- | The schedule recurrence.
scheduledInstanceAvailability_recurrence :: Lens.Lens' ScheduledInstanceAvailability (Prelude.Maybe ScheduledInstanceRecurrence)
scheduledInstanceAvailability_recurrence = Lens.lens (\ScheduledInstanceAvailability' {recurrence} -> recurrence) (\s@ScheduledInstanceAvailability' {} a -> s {recurrence = a} :: ScheduledInstanceAvailability)

-- | The number of hours in the schedule.
scheduledInstanceAvailability_slotDurationInHours :: Lens.Lens' ScheduledInstanceAvailability (Prelude.Maybe Prelude.Int)
scheduledInstanceAvailability_slotDurationInHours = Lens.lens (\ScheduledInstanceAvailability' {slotDurationInHours} -> slotDurationInHours) (\s@ScheduledInstanceAvailability' {} a -> s {slotDurationInHours = a} :: ScheduledInstanceAvailability)

-- | The total number of hours for a single instance for the entire term.
scheduledInstanceAvailability_totalScheduledInstanceHours :: Lens.Lens' ScheduledInstanceAvailability (Prelude.Maybe Prelude.Int)
scheduledInstanceAvailability_totalScheduledInstanceHours = Lens.lens (\ScheduledInstanceAvailability' {totalScheduledInstanceHours} -> totalScheduledInstanceHours) (\s@ScheduledInstanceAvailability' {} a -> s {totalScheduledInstanceHours = a} :: ScheduledInstanceAvailability)

instance Data.FromXML ScheduledInstanceAvailability where
  parseXML x =
    ScheduledInstanceAvailability'
      Prelude.<$> (x Data..@? "availabilityZone")
      Prelude.<*> (x Data..@? "availableInstanceCount")
      Prelude.<*> (x Data..@? "firstSlotStartTime")
      Prelude.<*> (x Data..@? "hourlyPrice")
      Prelude.<*> (x Data..@? "instanceType")
      Prelude.<*> (x Data..@? "maxTermDurationInDays")
      Prelude.<*> (x Data..@? "minTermDurationInDays")
      Prelude.<*> (x Data..@? "networkPlatform")
      Prelude.<*> (x Data..@? "platform")
      Prelude.<*> (x Data..@? "purchaseToken")
      Prelude.<*> (x Data..@? "recurrence")
      Prelude.<*> (x Data..@? "slotDurationInHours")
      Prelude.<*> (x Data..@? "totalScheduledInstanceHours")

instance
  Prelude.Hashable
    ScheduledInstanceAvailability
  where
  hashWithSalt _salt ScheduledInstanceAvailability' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` availableInstanceCount
      `Prelude.hashWithSalt` firstSlotStartTime
      `Prelude.hashWithSalt` hourlyPrice
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` maxTermDurationInDays
      `Prelude.hashWithSalt` minTermDurationInDays
      `Prelude.hashWithSalt` networkPlatform
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` purchaseToken
      `Prelude.hashWithSalt` recurrence
      `Prelude.hashWithSalt` slotDurationInHours
      `Prelude.hashWithSalt` totalScheduledInstanceHours

instance Prelude.NFData ScheduledInstanceAvailability where
  rnf ScheduledInstanceAvailability' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf availableInstanceCount
      `Prelude.seq` Prelude.rnf firstSlotStartTime
      `Prelude.seq` Prelude.rnf hourlyPrice
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf maxTermDurationInDays
      `Prelude.seq` Prelude.rnf minTermDurationInDays
      `Prelude.seq` Prelude.rnf networkPlatform
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf purchaseToken
      `Prelude.seq` Prelude.rnf recurrence
      `Prelude.seq` Prelude.rnf slotDurationInHours
      `Prelude.seq` Prelude.rnf totalScheduledInstanceHours
