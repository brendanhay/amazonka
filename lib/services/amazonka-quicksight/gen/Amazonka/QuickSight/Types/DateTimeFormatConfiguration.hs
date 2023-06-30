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
-- Module      : Amazonka.QuickSight.Types.DateTimeFormatConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DateTimeFormatConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.NullValueFormatConfiguration
import Amazonka.QuickSight.Types.NumericFormatConfiguration

-- | Formatting configuration for @DateTime@ fields.
--
-- /See:/ 'newDateTimeFormatConfiguration' smart constructor.
data DateTimeFormatConfiguration = DateTimeFormatConfiguration'
  { -- | Determines the @DateTime@ format.
    dateTimeFormat :: Prelude.Maybe Prelude.Text,
    -- | The options that determine the null value format configuration.
    nullValueFormatConfiguration :: Prelude.Maybe NullValueFormatConfiguration,
    -- | The formatting configuration for numeric @DateTime@ fields.
    numericFormatConfiguration :: Prelude.Maybe NumericFormatConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DateTimeFormatConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateTimeFormat', 'dateTimeFormatConfiguration_dateTimeFormat' - Determines the @DateTime@ format.
--
-- 'nullValueFormatConfiguration', 'dateTimeFormatConfiguration_nullValueFormatConfiguration' - The options that determine the null value format configuration.
--
-- 'numericFormatConfiguration', 'dateTimeFormatConfiguration_numericFormatConfiguration' - The formatting configuration for numeric @DateTime@ fields.
newDateTimeFormatConfiguration ::
  DateTimeFormatConfiguration
newDateTimeFormatConfiguration =
  DateTimeFormatConfiguration'
    { dateTimeFormat =
        Prelude.Nothing,
      nullValueFormatConfiguration = Prelude.Nothing,
      numericFormatConfiguration = Prelude.Nothing
    }

-- | Determines the @DateTime@ format.
dateTimeFormatConfiguration_dateTimeFormat :: Lens.Lens' DateTimeFormatConfiguration (Prelude.Maybe Prelude.Text)
dateTimeFormatConfiguration_dateTimeFormat = Lens.lens (\DateTimeFormatConfiguration' {dateTimeFormat} -> dateTimeFormat) (\s@DateTimeFormatConfiguration' {} a -> s {dateTimeFormat = a} :: DateTimeFormatConfiguration)

-- | The options that determine the null value format configuration.
dateTimeFormatConfiguration_nullValueFormatConfiguration :: Lens.Lens' DateTimeFormatConfiguration (Prelude.Maybe NullValueFormatConfiguration)
dateTimeFormatConfiguration_nullValueFormatConfiguration = Lens.lens (\DateTimeFormatConfiguration' {nullValueFormatConfiguration} -> nullValueFormatConfiguration) (\s@DateTimeFormatConfiguration' {} a -> s {nullValueFormatConfiguration = a} :: DateTimeFormatConfiguration)

-- | The formatting configuration for numeric @DateTime@ fields.
dateTimeFormatConfiguration_numericFormatConfiguration :: Lens.Lens' DateTimeFormatConfiguration (Prelude.Maybe NumericFormatConfiguration)
dateTimeFormatConfiguration_numericFormatConfiguration = Lens.lens (\DateTimeFormatConfiguration' {numericFormatConfiguration} -> numericFormatConfiguration) (\s@DateTimeFormatConfiguration' {} a -> s {numericFormatConfiguration = a} :: DateTimeFormatConfiguration)

instance Data.FromJSON DateTimeFormatConfiguration where
  parseJSON =
    Data.withObject
      "DateTimeFormatConfiguration"
      ( \x ->
          DateTimeFormatConfiguration'
            Prelude.<$> (x Data..:? "DateTimeFormat")
            Prelude.<*> (x Data..:? "NullValueFormatConfiguration")
            Prelude.<*> (x Data..:? "NumericFormatConfiguration")
      )

instance Prelude.Hashable DateTimeFormatConfiguration where
  hashWithSalt _salt DateTimeFormatConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` dateTimeFormat
      `Prelude.hashWithSalt` nullValueFormatConfiguration
      `Prelude.hashWithSalt` numericFormatConfiguration

instance Prelude.NFData DateTimeFormatConfiguration where
  rnf DateTimeFormatConfiguration' {..} =
    Prelude.rnf dateTimeFormat
      `Prelude.seq` Prelude.rnf nullValueFormatConfiguration
      `Prelude.seq` Prelude.rnf numericFormatConfiguration

instance Data.ToJSON DateTimeFormatConfiguration where
  toJSON DateTimeFormatConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DateTimeFormat" Data..=)
              Prelude.<$> dateTimeFormat,
            ("NullValueFormatConfiguration" Data..=)
              Prelude.<$> nullValueFormatConfiguration,
            ("NumericFormatConfiguration" Data..=)
              Prelude.<$> numericFormatConfiguration
          ]
      )
