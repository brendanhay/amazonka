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
-- Module      : Amazonka.IotTwinMaker.Types.PropertyValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.PropertyValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.DataValue
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information about a value for a time series
-- property.
--
-- /See:/ 'newPropertyValue' smart constructor.
data PropertyValue = PropertyValue'
  { -- | ISO8601 DateTime of a value for a time series property.
    --
    -- The time for when the property value was recorded in ISO 8601 format:
    -- /YYYY-MM-DDThh:mm:ss[.SSSSSSSSS][Z\/±HH:mm]/.
    --
    -- -   /[YYYY]/: year
    --
    -- -   /[MM]/: month
    --
    -- -   /[DD]/: day
    --
    -- -   /[hh]/: hour
    --
    -- -   /[mm]/: minute
    --
    -- -   /[ss]/: seconds
    --
    -- -   /[.SSSSSSSSS]/: additional precision, where precedence is
    --     maintained. For example: [.573123] is equal to 573123000
    --     nanoseconds.
    --
    -- -   /Z/: default timezone UTC
    --
    -- -   /± HH:mm/: time zone offset in Hours and Minutes.
    --
    -- /Required sub-fields/: YYYY-MM-DDThh:mm:ss and [Z\/±HH:mm]
    time :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of a value for a time series property.
    timestamp :: Prelude.Maybe Data.POSIX,
    -- | An object that specifies a value for a time series property.
    value :: DataValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PropertyValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'time', 'propertyValue_time' - ISO8601 DateTime of a value for a time series property.
--
-- The time for when the property value was recorded in ISO 8601 format:
-- /YYYY-MM-DDThh:mm:ss[.SSSSSSSSS][Z\/±HH:mm]/.
--
-- -   /[YYYY]/: year
--
-- -   /[MM]/: month
--
-- -   /[DD]/: day
--
-- -   /[hh]/: hour
--
-- -   /[mm]/: minute
--
-- -   /[ss]/: seconds
--
-- -   /[.SSSSSSSSS]/: additional precision, where precedence is
--     maintained. For example: [.573123] is equal to 573123000
--     nanoseconds.
--
-- -   /Z/: default timezone UTC
--
-- -   /± HH:mm/: time zone offset in Hours and Minutes.
--
-- /Required sub-fields/: YYYY-MM-DDThh:mm:ss and [Z\/±HH:mm]
--
-- 'timestamp', 'propertyValue_timestamp' - The timestamp of a value for a time series property.
--
-- 'value', 'propertyValue_value' - An object that specifies a value for a time series property.
newPropertyValue ::
  -- | 'value'
  DataValue ->
  PropertyValue
newPropertyValue pValue_ =
  PropertyValue'
    { time = Prelude.Nothing,
      timestamp = Prelude.Nothing,
      value = pValue_
    }

-- | ISO8601 DateTime of a value for a time series property.
--
-- The time for when the property value was recorded in ISO 8601 format:
-- /YYYY-MM-DDThh:mm:ss[.SSSSSSSSS][Z\/±HH:mm]/.
--
-- -   /[YYYY]/: year
--
-- -   /[MM]/: month
--
-- -   /[DD]/: day
--
-- -   /[hh]/: hour
--
-- -   /[mm]/: minute
--
-- -   /[ss]/: seconds
--
-- -   /[.SSSSSSSSS]/: additional precision, where precedence is
--     maintained. For example: [.573123] is equal to 573123000
--     nanoseconds.
--
-- -   /Z/: default timezone UTC
--
-- -   /± HH:mm/: time zone offset in Hours and Minutes.
--
-- /Required sub-fields/: YYYY-MM-DDThh:mm:ss and [Z\/±HH:mm]
propertyValue_time :: Lens.Lens' PropertyValue (Prelude.Maybe Prelude.Text)
propertyValue_time = Lens.lens (\PropertyValue' {time} -> time) (\s@PropertyValue' {} a -> s {time = a} :: PropertyValue)

-- | The timestamp of a value for a time series property.
propertyValue_timestamp :: Lens.Lens' PropertyValue (Prelude.Maybe Prelude.UTCTime)
propertyValue_timestamp = Lens.lens (\PropertyValue' {timestamp} -> timestamp) (\s@PropertyValue' {} a -> s {timestamp = a} :: PropertyValue) Prelude.. Lens.mapping Data._Time

-- | An object that specifies a value for a time series property.
propertyValue_value :: Lens.Lens' PropertyValue DataValue
propertyValue_value = Lens.lens (\PropertyValue' {value} -> value) (\s@PropertyValue' {} a -> s {value = a} :: PropertyValue)

instance Data.FromJSON PropertyValue where
  parseJSON =
    Data.withObject
      "PropertyValue"
      ( \x ->
          PropertyValue'
            Prelude.<$> (x Data..:? "time")
            Prelude.<*> (x Data..:? "timestamp")
            Prelude.<*> (x Data..: "value")
      )

instance Prelude.Hashable PropertyValue where
  hashWithSalt _salt PropertyValue' {..} =
    _salt
      `Prelude.hashWithSalt` time
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` value

instance Prelude.NFData PropertyValue where
  rnf PropertyValue' {..} =
    Prelude.rnf time
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON PropertyValue where
  toJSON PropertyValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("time" Data..=) Prelude.<$> time,
            ("timestamp" Data..=) Prelude.<$> timestamp,
            Prelude.Just ("value" Data..= value)
          ]
      )
