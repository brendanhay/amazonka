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
-- Module      : Amazonka.QuickSight.Types.StringValueWhenUnsetConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.StringValueWhenUnsetConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ValueWhenUnsetOption

-- | The configuration that defines the default value of a @String@ parameter
-- when a value has not been set.
--
-- /See:/ 'newStringValueWhenUnsetConfiguration' smart constructor.
data StringValueWhenUnsetConfiguration = StringValueWhenUnsetConfiguration'
  { -- | A custom value that\'s used when the value of a parameter isn\'t set.
    customValue :: Prelude.Maybe (Data.Sensitive Prelude.Text),
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
-- Create a value of 'StringValueWhenUnsetConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customValue', 'stringValueWhenUnsetConfiguration_customValue' - A custom value that\'s used when the value of a parameter isn\'t set.
--
-- 'valueWhenUnsetOption', 'stringValueWhenUnsetConfiguration_valueWhenUnsetOption' - The built-in options for default values. The value can be one of the
-- following:
--
-- -   @RECOMMENDED@: The recommended value.
--
-- -   @NULL@: The @NULL@ value.
newStringValueWhenUnsetConfiguration ::
  StringValueWhenUnsetConfiguration
newStringValueWhenUnsetConfiguration =
  StringValueWhenUnsetConfiguration'
    { customValue =
        Prelude.Nothing,
      valueWhenUnsetOption = Prelude.Nothing
    }

-- | A custom value that\'s used when the value of a parameter isn\'t set.
stringValueWhenUnsetConfiguration_customValue :: Lens.Lens' StringValueWhenUnsetConfiguration (Prelude.Maybe Prelude.Text)
stringValueWhenUnsetConfiguration_customValue = Lens.lens (\StringValueWhenUnsetConfiguration' {customValue} -> customValue) (\s@StringValueWhenUnsetConfiguration' {} a -> s {customValue = a} :: StringValueWhenUnsetConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | The built-in options for default values. The value can be one of the
-- following:
--
-- -   @RECOMMENDED@: The recommended value.
--
-- -   @NULL@: The @NULL@ value.
stringValueWhenUnsetConfiguration_valueWhenUnsetOption :: Lens.Lens' StringValueWhenUnsetConfiguration (Prelude.Maybe ValueWhenUnsetOption)
stringValueWhenUnsetConfiguration_valueWhenUnsetOption = Lens.lens (\StringValueWhenUnsetConfiguration' {valueWhenUnsetOption} -> valueWhenUnsetOption) (\s@StringValueWhenUnsetConfiguration' {} a -> s {valueWhenUnsetOption = a} :: StringValueWhenUnsetConfiguration)

instance
  Data.FromJSON
    StringValueWhenUnsetConfiguration
  where
  parseJSON =
    Data.withObject
      "StringValueWhenUnsetConfiguration"
      ( \x ->
          StringValueWhenUnsetConfiguration'
            Prelude.<$> (x Data..:? "CustomValue")
            Prelude.<*> (x Data..:? "ValueWhenUnsetOption")
      )

instance
  Prelude.Hashable
    StringValueWhenUnsetConfiguration
  where
  hashWithSalt
    _salt
    StringValueWhenUnsetConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` customValue
        `Prelude.hashWithSalt` valueWhenUnsetOption

instance
  Prelude.NFData
    StringValueWhenUnsetConfiguration
  where
  rnf StringValueWhenUnsetConfiguration' {..} =
    Prelude.rnf customValue
      `Prelude.seq` Prelude.rnf valueWhenUnsetOption

instance
  Data.ToJSON
    StringValueWhenUnsetConfiguration
  where
  toJSON StringValueWhenUnsetConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomValue" Data..=) Prelude.<$> customValue,
            ("ValueWhenUnsetOption" Data..=)
              Prelude.<$> valueWhenUnsetOption
          ]
      )
