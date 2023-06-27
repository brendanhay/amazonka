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
-- Module      : Amazonka.SecurityHub.Types.AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The scheduled time period (UTC) during which Amazon MQ begins to apply
-- pending updates or patches to the broker.
--
-- /See:/ 'newAwsAmazonMqBrokerMaintenanceWindowStartTimeDetails' smart constructor.
data AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails = AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails'
  { -- | The day of the week on which the maintenance window falls.
    dayOfWeek :: Prelude.Maybe Prelude.Text,
    -- | The time, in 24-hour format, on which the maintenance window falls.
    timeOfDay :: Prelude.Maybe Prelude.Text,
    -- | The time zone in either the Country\/City format or the UTC offset
    -- format. UTC is the default format.
    timeZone :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dayOfWeek', 'awsAmazonMqBrokerMaintenanceWindowStartTimeDetails_dayOfWeek' - The day of the week on which the maintenance window falls.
--
-- 'timeOfDay', 'awsAmazonMqBrokerMaintenanceWindowStartTimeDetails_timeOfDay' - The time, in 24-hour format, on which the maintenance window falls.
--
-- 'timeZone', 'awsAmazonMqBrokerMaintenanceWindowStartTimeDetails_timeZone' - The time zone in either the Country\/City format or the UTC offset
-- format. UTC is the default format.
newAwsAmazonMqBrokerMaintenanceWindowStartTimeDetails ::
  AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails
newAwsAmazonMqBrokerMaintenanceWindowStartTimeDetails =
  AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails'
    { dayOfWeek =
        Prelude.Nothing,
      timeOfDay =
        Prelude.Nothing,
      timeZone =
        Prelude.Nothing
    }

-- | The day of the week on which the maintenance window falls.
awsAmazonMqBrokerMaintenanceWindowStartTimeDetails_dayOfWeek :: Lens.Lens' AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerMaintenanceWindowStartTimeDetails_dayOfWeek = Lens.lens (\AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails' {dayOfWeek} -> dayOfWeek) (\s@AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails' {} a -> s {dayOfWeek = a} :: AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails)

-- | The time, in 24-hour format, on which the maintenance window falls.
awsAmazonMqBrokerMaintenanceWindowStartTimeDetails_timeOfDay :: Lens.Lens' AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerMaintenanceWindowStartTimeDetails_timeOfDay = Lens.lens (\AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails' {timeOfDay} -> timeOfDay) (\s@AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails' {} a -> s {timeOfDay = a} :: AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails)

-- | The time zone in either the Country\/City format or the UTC offset
-- format. UTC is the default format.
awsAmazonMqBrokerMaintenanceWindowStartTimeDetails_timeZone :: Lens.Lens' AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerMaintenanceWindowStartTimeDetails_timeZone = Lens.lens (\AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails' {timeZone} -> timeZone) (\s@AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails' {} a -> s {timeZone = a} :: AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails)

instance
  Data.FromJSON
    AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails
  where
  parseJSON =
    Data.withObject
      "AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails"
      ( \x ->
          AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails'
            Prelude.<$> (x Data..:? "DayOfWeek")
            Prelude.<*> (x Data..:? "TimeOfDay")
            Prelude.<*> (x Data..:? "TimeZone")
      )

instance
  Prelude.Hashable
    AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails
  where
  hashWithSalt
    _salt
    AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails' {..} =
      _salt
        `Prelude.hashWithSalt` dayOfWeek
        `Prelude.hashWithSalt` timeOfDay
        `Prelude.hashWithSalt` timeZone

instance
  Prelude.NFData
    AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails
  where
  rnf
    AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails' {..} =
      Prelude.rnf dayOfWeek
        `Prelude.seq` Prelude.rnf timeOfDay
        `Prelude.seq` Prelude.rnf timeZone

instance
  Data.ToJSON
    AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails
  where
  toJSON
    AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("DayOfWeek" Data..=) Prelude.<$> dayOfWeek,
              ("TimeOfDay" Data..=) Prelude.<$> timeOfDay,
              ("TimeZone" Data..=) Prelude.<$> timeZone
            ]
        )
