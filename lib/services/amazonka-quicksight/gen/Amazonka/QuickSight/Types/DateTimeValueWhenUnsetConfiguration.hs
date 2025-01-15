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
-- Module      : Amazonka.QuickSight.Types.DateTimeValueWhenUnsetConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DateTimeValueWhenUnsetConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ValueWhenUnsetOption

-- | The configuration that defines the default value of a @DateTime@
-- parameter when a value has not been set.
--
-- /See:/ 'newDateTimeValueWhenUnsetConfiguration' smart constructor.
data DateTimeValueWhenUnsetConfiguration = DateTimeValueWhenUnsetConfiguration'
  { -- | A custom value that\'s used when the value of a parameter isn\'t set.
    customValue :: Prelude.Maybe (Data.Sensitive Data.POSIX),
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
-- Create a value of 'DateTimeValueWhenUnsetConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customValue', 'dateTimeValueWhenUnsetConfiguration_customValue' - A custom value that\'s used when the value of a parameter isn\'t set.
--
-- 'valueWhenUnsetOption', 'dateTimeValueWhenUnsetConfiguration_valueWhenUnsetOption' - The built-in options for default values. The value can be one of the
-- following:
--
-- -   @RECOMMENDED@: The recommended value.
--
-- -   @NULL@: The @NULL@ value.
newDateTimeValueWhenUnsetConfiguration ::
  DateTimeValueWhenUnsetConfiguration
newDateTimeValueWhenUnsetConfiguration =
  DateTimeValueWhenUnsetConfiguration'
    { customValue =
        Prelude.Nothing,
      valueWhenUnsetOption = Prelude.Nothing
    }

-- | A custom value that\'s used when the value of a parameter isn\'t set.
dateTimeValueWhenUnsetConfiguration_customValue :: Lens.Lens' DateTimeValueWhenUnsetConfiguration (Prelude.Maybe Prelude.UTCTime)
dateTimeValueWhenUnsetConfiguration_customValue = Lens.lens (\DateTimeValueWhenUnsetConfiguration' {customValue} -> customValue) (\s@DateTimeValueWhenUnsetConfiguration' {} a -> s {customValue = a} :: DateTimeValueWhenUnsetConfiguration) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Data._Time)

-- | The built-in options for default values. The value can be one of the
-- following:
--
-- -   @RECOMMENDED@: The recommended value.
--
-- -   @NULL@: The @NULL@ value.
dateTimeValueWhenUnsetConfiguration_valueWhenUnsetOption :: Lens.Lens' DateTimeValueWhenUnsetConfiguration (Prelude.Maybe ValueWhenUnsetOption)
dateTimeValueWhenUnsetConfiguration_valueWhenUnsetOption = Lens.lens (\DateTimeValueWhenUnsetConfiguration' {valueWhenUnsetOption} -> valueWhenUnsetOption) (\s@DateTimeValueWhenUnsetConfiguration' {} a -> s {valueWhenUnsetOption = a} :: DateTimeValueWhenUnsetConfiguration)

instance
  Data.FromJSON
    DateTimeValueWhenUnsetConfiguration
  where
  parseJSON =
    Data.withObject
      "DateTimeValueWhenUnsetConfiguration"
      ( \x ->
          DateTimeValueWhenUnsetConfiguration'
            Prelude.<$> (x Data..:? "CustomValue")
            Prelude.<*> (x Data..:? "ValueWhenUnsetOption")
      )

instance
  Prelude.Hashable
    DateTimeValueWhenUnsetConfiguration
  where
  hashWithSalt
    _salt
    DateTimeValueWhenUnsetConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` customValue
        `Prelude.hashWithSalt` valueWhenUnsetOption

instance
  Prelude.NFData
    DateTimeValueWhenUnsetConfiguration
  where
  rnf DateTimeValueWhenUnsetConfiguration' {..} =
    Prelude.rnf customValue `Prelude.seq`
      Prelude.rnf valueWhenUnsetOption

instance
  Data.ToJSON
    DateTimeValueWhenUnsetConfiguration
  where
  toJSON DateTimeValueWhenUnsetConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomValue" Data..=) Prelude.<$> customValue,
            ("ValueWhenUnsetOption" Data..=)
              Prelude.<$> valueWhenUnsetOption
          ]
      )
