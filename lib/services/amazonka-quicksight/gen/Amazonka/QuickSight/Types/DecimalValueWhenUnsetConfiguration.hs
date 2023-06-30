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
-- Module      : Amazonka.QuickSight.Types.DecimalValueWhenUnsetConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DecimalValueWhenUnsetConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ValueWhenUnsetOption

-- | The configuration that defines the default value of a @Decimal@
-- parameter when a value has not been set.
--
-- /See:/ 'newDecimalValueWhenUnsetConfiguration' smart constructor.
data DecimalValueWhenUnsetConfiguration = DecimalValueWhenUnsetConfiguration'
  { -- | A custom value that\'s used when the value of a parameter isn\'t set.
    customValue :: Prelude.Maybe (Data.Sensitive Prelude.Double),
    -- | The built-in options for default values. The value can be one of the
    -- following:
    --
    -- -   @RECOMMENDED@: The recommended value.
    --
    -- -   @NULL@: The @NULL@ value.
    valueWhenUnsetOption :: Prelude.Maybe ValueWhenUnsetOption
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DecimalValueWhenUnsetConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customValue', 'decimalValueWhenUnsetConfiguration_customValue' - A custom value that\'s used when the value of a parameter isn\'t set.
--
-- 'valueWhenUnsetOption', 'decimalValueWhenUnsetConfiguration_valueWhenUnsetOption' - The built-in options for default values. The value can be one of the
-- following:
--
-- -   @RECOMMENDED@: The recommended value.
--
-- -   @NULL@: The @NULL@ value.
newDecimalValueWhenUnsetConfiguration ::
  DecimalValueWhenUnsetConfiguration
newDecimalValueWhenUnsetConfiguration =
  DecimalValueWhenUnsetConfiguration'
    { customValue =
        Prelude.Nothing,
      valueWhenUnsetOption = Prelude.Nothing
    }

-- | A custom value that\'s used when the value of a parameter isn\'t set.
decimalValueWhenUnsetConfiguration_customValue :: Lens.Lens' DecimalValueWhenUnsetConfiguration (Prelude.Maybe Prelude.Double)
decimalValueWhenUnsetConfiguration_customValue = Lens.lens (\DecimalValueWhenUnsetConfiguration' {customValue} -> customValue) (\s@DecimalValueWhenUnsetConfiguration' {} a -> s {customValue = a} :: DecimalValueWhenUnsetConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | The built-in options for default values. The value can be one of the
-- following:
--
-- -   @RECOMMENDED@: The recommended value.
--
-- -   @NULL@: The @NULL@ value.
decimalValueWhenUnsetConfiguration_valueWhenUnsetOption :: Lens.Lens' DecimalValueWhenUnsetConfiguration (Prelude.Maybe ValueWhenUnsetOption)
decimalValueWhenUnsetConfiguration_valueWhenUnsetOption = Lens.lens (\DecimalValueWhenUnsetConfiguration' {valueWhenUnsetOption} -> valueWhenUnsetOption) (\s@DecimalValueWhenUnsetConfiguration' {} a -> s {valueWhenUnsetOption = a} :: DecimalValueWhenUnsetConfiguration)

instance
  Data.FromJSON
    DecimalValueWhenUnsetConfiguration
  where
  parseJSON =
    Data.withObject
      "DecimalValueWhenUnsetConfiguration"
      ( \x ->
          DecimalValueWhenUnsetConfiguration'
            Prelude.<$> (x Data..:? "CustomValue")
            Prelude.<*> (x Data..:? "ValueWhenUnsetOption")
      )

instance
  Prelude.Hashable
    DecimalValueWhenUnsetConfiguration
  where
  hashWithSalt
    _salt
    DecimalValueWhenUnsetConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` customValue
        `Prelude.hashWithSalt` valueWhenUnsetOption

instance
  Prelude.NFData
    DecimalValueWhenUnsetConfiguration
  where
  rnf DecimalValueWhenUnsetConfiguration' {..} =
    Prelude.rnf customValue
      `Prelude.seq` Prelude.rnf valueWhenUnsetOption

instance
  Data.ToJSON
    DecimalValueWhenUnsetConfiguration
  where
  toJSON DecimalValueWhenUnsetConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomValue" Data..=) Prelude.<$> customValue,
            ("ValueWhenUnsetOption" Data..=)
              Prelude.<$> valueWhenUnsetOption
          ]
      )
