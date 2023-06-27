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
-- Module      : Amazonka.QuickSight.Types.RefreshFrequency
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RefreshFrequency where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.RefreshInterval
import Amazonka.QuickSight.Types.ScheduleRefreshOnEntity

-- | Specifies the interval between each scheduled refresh of a dataset.
--
-- /See:/ 'newRefreshFrequency' smart constructor.
data RefreshFrequency = RefreshFrequency'
  { -- | The day of the week that you want to schedule the refresh on. This value
    -- is required for weekly and monthly refresh intervals.
    refreshOnDay :: Prelude.Maybe ScheduleRefreshOnEntity,
    -- | The time of day that you want the datset to refresh. This value is
    -- expressed in HH:MM format. This field is not required for schedules that
    -- refresh hourly.
    timeOfTheDay :: Prelude.Maybe Prelude.Text,
    -- | The timezone that you want the refresh schedule to use. The timezone ID
    -- must match a corresponding ID found on
    -- @java.util.time.getAvailableIDs()@.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | The interval between scheduled refreshes. Valid values are as follows:
    --
    -- -   @MINUTE15@: The dataset refreshes every 15 minutes. This value is
    --     only supported for incremental refreshes. This interval can only be
    --     used for one schedule per dataset.
    --
    -- -   @MINUTE30@:The dataset refreshes every 30 minutes. This value is
    --     only supported for incremental refreshes. This interval can only be
    --     used for one schedule per dataset.
    --
    -- -   @HOURLY@: The dataset refreshes every hour. This interval can only
    --     be used for one schedule per dataset.
    --
    -- -   @DAILY@: The dataset refreshes every day.
    --
    -- -   @WEEKLY@: The dataset refreshes every week.
    --
    -- -   @MONTHLY@: The dataset refreshes every month.
    interval :: RefreshInterval
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RefreshFrequency' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'refreshOnDay', 'refreshFrequency_refreshOnDay' - The day of the week that you want to schedule the refresh on. This value
-- is required for weekly and monthly refresh intervals.
--
-- 'timeOfTheDay', 'refreshFrequency_timeOfTheDay' - The time of day that you want the datset to refresh. This value is
-- expressed in HH:MM format. This field is not required for schedules that
-- refresh hourly.
--
-- 'timezone', 'refreshFrequency_timezone' - The timezone that you want the refresh schedule to use. The timezone ID
-- must match a corresponding ID found on
-- @java.util.time.getAvailableIDs()@.
--
-- 'interval', 'refreshFrequency_interval' - The interval between scheduled refreshes. Valid values are as follows:
--
-- -   @MINUTE15@: The dataset refreshes every 15 minutes. This value is
--     only supported for incremental refreshes. This interval can only be
--     used for one schedule per dataset.
--
-- -   @MINUTE30@:The dataset refreshes every 30 minutes. This value is
--     only supported for incremental refreshes. This interval can only be
--     used for one schedule per dataset.
--
-- -   @HOURLY@: The dataset refreshes every hour. This interval can only
--     be used for one schedule per dataset.
--
-- -   @DAILY@: The dataset refreshes every day.
--
-- -   @WEEKLY@: The dataset refreshes every week.
--
-- -   @MONTHLY@: The dataset refreshes every month.
newRefreshFrequency ::
  -- | 'interval'
  RefreshInterval ->
  RefreshFrequency
newRefreshFrequency pInterval_ =
  RefreshFrequency'
    { refreshOnDay = Prelude.Nothing,
      timeOfTheDay = Prelude.Nothing,
      timezone = Prelude.Nothing,
      interval = pInterval_
    }

-- | The day of the week that you want to schedule the refresh on. This value
-- is required for weekly and monthly refresh intervals.
refreshFrequency_refreshOnDay :: Lens.Lens' RefreshFrequency (Prelude.Maybe ScheduleRefreshOnEntity)
refreshFrequency_refreshOnDay = Lens.lens (\RefreshFrequency' {refreshOnDay} -> refreshOnDay) (\s@RefreshFrequency' {} a -> s {refreshOnDay = a} :: RefreshFrequency)

-- | The time of day that you want the datset to refresh. This value is
-- expressed in HH:MM format. This field is not required for schedules that
-- refresh hourly.
refreshFrequency_timeOfTheDay :: Lens.Lens' RefreshFrequency (Prelude.Maybe Prelude.Text)
refreshFrequency_timeOfTheDay = Lens.lens (\RefreshFrequency' {timeOfTheDay} -> timeOfTheDay) (\s@RefreshFrequency' {} a -> s {timeOfTheDay = a} :: RefreshFrequency)

-- | The timezone that you want the refresh schedule to use. The timezone ID
-- must match a corresponding ID found on
-- @java.util.time.getAvailableIDs()@.
refreshFrequency_timezone :: Lens.Lens' RefreshFrequency (Prelude.Maybe Prelude.Text)
refreshFrequency_timezone = Lens.lens (\RefreshFrequency' {timezone} -> timezone) (\s@RefreshFrequency' {} a -> s {timezone = a} :: RefreshFrequency)

-- | The interval between scheduled refreshes. Valid values are as follows:
--
-- -   @MINUTE15@: The dataset refreshes every 15 minutes. This value is
--     only supported for incremental refreshes. This interval can only be
--     used for one schedule per dataset.
--
-- -   @MINUTE30@:The dataset refreshes every 30 minutes. This value is
--     only supported for incremental refreshes. This interval can only be
--     used for one schedule per dataset.
--
-- -   @HOURLY@: The dataset refreshes every hour. This interval can only
--     be used for one schedule per dataset.
--
-- -   @DAILY@: The dataset refreshes every day.
--
-- -   @WEEKLY@: The dataset refreshes every week.
--
-- -   @MONTHLY@: The dataset refreshes every month.
refreshFrequency_interval :: Lens.Lens' RefreshFrequency RefreshInterval
refreshFrequency_interval = Lens.lens (\RefreshFrequency' {interval} -> interval) (\s@RefreshFrequency' {} a -> s {interval = a} :: RefreshFrequency)

instance Data.FromJSON RefreshFrequency where
  parseJSON =
    Data.withObject
      "RefreshFrequency"
      ( \x ->
          RefreshFrequency'
            Prelude.<$> (x Data..:? "RefreshOnDay")
            Prelude.<*> (x Data..:? "TimeOfTheDay")
            Prelude.<*> (x Data..:? "Timezone")
            Prelude.<*> (x Data..: "Interval")
      )

instance Prelude.Hashable RefreshFrequency where
  hashWithSalt _salt RefreshFrequency' {..} =
    _salt
      `Prelude.hashWithSalt` refreshOnDay
      `Prelude.hashWithSalt` timeOfTheDay
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` interval

instance Prelude.NFData RefreshFrequency where
  rnf RefreshFrequency' {..} =
    Prelude.rnf refreshOnDay
      `Prelude.seq` Prelude.rnf timeOfTheDay
      `Prelude.seq` Prelude.rnf timezone
      `Prelude.seq` Prelude.rnf interval

instance Data.ToJSON RefreshFrequency where
  toJSON RefreshFrequency' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RefreshOnDay" Data..=) Prelude.<$> refreshOnDay,
            ("TimeOfTheDay" Data..=) Prelude.<$> timeOfTheDay,
            ("Timezone" Data..=) Prelude.<$> timezone,
            Prelude.Just ("Interval" Data..= interval)
          ]
      )
