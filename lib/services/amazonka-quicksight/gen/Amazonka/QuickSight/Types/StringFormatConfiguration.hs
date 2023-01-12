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
-- Module      : Amazonka.QuickSight.Types.StringFormatConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.StringFormatConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.NullValueFormatConfiguration
import Amazonka.QuickSight.Types.NumericFormatConfiguration

-- | Formatting configuration for string fields.
--
-- /See:/ 'newStringFormatConfiguration' smart constructor.
data StringFormatConfiguration = StringFormatConfiguration'
  { -- | The options that determine the null value format configuration.
    nullValueFormatConfiguration :: Prelude.Maybe NullValueFormatConfiguration,
    -- | The formatting configuration for numeric strings.
    numericFormatConfiguration :: Prelude.Maybe NumericFormatConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StringFormatConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nullValueFormatConfiguration', 'stringFormatConfiguration_nullValueFormatConfiguration' - The options that determine the null value format configuration.
--
-- 'numericFormatConfiguration', 'stringFormatConfiguration_numericFormatConfiguration' - The formatting configuration for numeric strings.
newStringFormatConfiguration ::
  StringFormatConfiguration
newStringFormatConfiguration =
  StringFormatConfiguration'
    { nullValueFormatConfiguration =
        Prelude.Nothing,
      numericFormatConfiguration = Prelude.Nothing
    }

-- | The options that determine the null value format configuration.
stringFormatConfiguration_nullValueFormatConfiguration :: Lens.Lens' StringFormatConfiguration (Prelude.Maybe NullValueFormatConfiguration)
stringFormatConfiguration_nullValueFormatConfiguration = Lens.lens (\StringFormatConfiguration' {nullValueFormatConfiguration} -> nullValueFormatConfiguration) (\s@StringFormatConfiguration' {} a -> s {nullValueFormatConfiguration = a} :: StringFormatConfiguration)

-- | The formatting configuration for numeric strings.
stringFormatConfiguration_numericFormatConfiguration :: Lens.Lens' StringFormatConfiguration (Prelude.Maybe NumericFormatConfiguration)
stringFormatConfiguration_numericFormatConfiguration = Lens.lens (\StringFormatConfiguration' {numericFormatConfiguration} -> numericFormatConfiguration) (\s@StringFormatConfiguration' {} a -> s {numericFormatConfiguration = a} :: StringFormatConfiguration)

instance Data.FromJSON StringFormatConfiguration where
  parseJSON =
    Data.withObject
      "StringFormatConfiguration"
      ( \x ->
          StringFormatConfiguration'
            Prelude.<$> (x Data..:? "NullValueFormatConfiguration")
            Prelude.<*> (x Data..:? "NumericFormatConfiguration")
      )

instance Prelude.Hashable StringFormatConfiguration where
  hashWithSalt _salt StringFormatConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` nullValueFormatConfiguration
      `Prelude.hashWithSalt` numericFormatConfiguration

instance Prelude.NFData StringFormatConfiguration where
  rnf StringFormatConfiguration' {..} =
    Prelude.rnf nullValueFormatConfiguration
      `Prelude.seq` Prelude.rnf numericFormatConfiguration

instance Data.ToJSON StringFormatConfiguration where
  toJSON StringFormatConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NullValueFormatConfiguration" Data..=)
              Prelude.<$> nullValueFormatConfiguration,
            ("NumericFormatConfiguration" Data..=)
              Prelude.<$> numericFormatConfiguration
          ]
      )
