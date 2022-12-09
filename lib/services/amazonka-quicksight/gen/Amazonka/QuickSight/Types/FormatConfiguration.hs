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
-- Module      : Amazonka.QuickSight.Types.FormatConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FormatConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DateTimeFormatConfiguration
import Amazonka.QuickSight.Types.NumberFormatConfiguration
import Amazonka.QuickSight.Types.StringFormatConfiguration

-- | The formatting configuration for all types of field.
--
-- /See:/ 'newFormatConfiguration' smart constructor.
data FormatConfiguration = FormatConfiguration'
  { -- | Formatting configuration for @DateTime@ fields.
    dateTimeFormatConfiguration :: Prelude.Maybe DateTimeFormatConfiguration,
    -- | Formatting configuration for number fields.
    numberFormatConfiguration :: Prelude.Maybe NumberFormatConfiguration,
    -- | Formatting configuration for string fields.
    stringFormatConfiguration :: Prelude.Maybe StringFormatConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FormatConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateTimeFormatConfiguration', 'formatConfiguration_dateTimeFormatConfiguration' - Formatting configuration for @DateTime@ fields.
--
-- 'numberFormatConfiguration', 'formatConfiguration_numberFormatConfiguration' - Formatting configuration for number fields.
--
-- 'stringFormatConfiguration', 'formatConfiguration_stringFormatConfiguration' - Formatting configuration for string fields.
newFormatConfiguration ::
  FormatConfiguration
newFormatConfiguration =
  FormatConfiguration'
    { dateTimeFormatConfiguration =
        Prelude.Nothing,
      numberFormatConfiguration = Prelude.Nothing,
      stringFormatConfiguration = Prelude.Nothing
    }

-- | Formatting configuration for @DateTime@ fields.
formatConfiguration_dateTimeFormatConfiguration :: Lens.Lens' FormatConfiguration (Prelude.Maybe DateTimeFormatConfiguration)
formatConfiguration_dateTimeFormatConfiguration = Lens.lens (\FormatConfiguration' {dateTimeFormatConfiguration} -> dateTimeFormatConfiguration) (\s@FormatConfiguration' {} a -> s {dateTimeFormatConfiguration = a} :: FormatConfiguration)

-- | Formatting configuration for number fields.
formatConfiguration_numberFormatConfiguration :: Lens.Lens' FormatConfiguration (Prelude.Maybe NumberFormatConfiguration)
formatConfiguration_numberFormatConfiguration = Lens.lens (\FormatConfiguration' {numberFormatConfiguration} -> numberFormatConfiguration) (\s@FormatConfiguration' {} a -> s {numberFormatConfiguration = a} :: FormatConfiguration)

-- | Formatting configuration for string fields.
formatConfiguration_stringFormatConfiguration :: Lens.Lens' FormatConfiguration (Prelude.Maybe StringFormatConfiguration)
formatConfiguration_stringFormatConfiguration = Lens.lens (\FormatConfiguration' {stringFormatConfiguration} -> stringFormatConfiguration) (\s@FormatConfiguration' {} a -> s {stringFormatConfiguration = a} :: FormatConfiguration)

instance Data.FromJSON FormatConfiguration where
  parseJSON =
    Data.withObject
      "FormatConfiguration"
      ( \x ->
          FormatConfiguration'
            Prelude.<$> (x Data..:? "DateTimeFormatConfiguration")
            Prelude.<*> (x Data..:? "NumberFormatConfiguration")
            Prelude.<*> (x Data..:? "StringFormatConfiguration")
      )

instance Prelude.Hashable FormatConfiguration where
  hashWithSalt _salt FormatConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` dateTimeFormatConfiguration
      `Prelude.hashWithSalt` numberFormatConfiguration
      `Prelude.hashWithSalt` stringFormatConfiguration

instance Prelude.NFData FormatConfiguration where
  rnf FormatConfiguration' {..} =
    Prelude.rnf dateTimeFormatConfiguration
      `Prelude.seq` Prelude.rnf numberFormatConfiguration
      `Prelude.seq` Prelude.rnf stringFormatConfiguration

instance Data.ToJSON FormatConfiguration where
  toJSON FormatConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DateTimeFormatConfiguration" Data..=)
              Prelude.<$> dateTimeFormatConfiguration,
            ("NumberFormatConfiguration" Data..=)
              Prelude.<$> numberFormatConfiguration,
            ("StringFormatConfiguration" Data..=)
              Prelude.<$> stringFormatConfiguration
          ]
      )
