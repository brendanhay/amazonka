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
-- Module      : Amazonka.QuickSight.Types.IntegerValueWhenUnsetConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.IntegerValueWhenUnsetConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ValueWhenUnsetOption

-- | A parameter declaration for the @Integer@ data type.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newIntegerValueWhenUnsetConfiguration' smart constructor.
data IntegerValueWhenUnsetConfiguration = IntegerValueWhenUnsetConfiguration'
  { -- | A custom value that\'s used when the value of a parameter isn\'t set.
    customValue :: Prelude.Maybe (Data.Sensitive Prelude.Integer),
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
-- Create a value of 'IntegerValueWhenUnsetConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customValue', 'integerValueWhenUnsetConfiguration_customValue' - A custom value that\'s used when the value of a parameter isn\'t set.
--
-- 'valueWhenUnsetOption', 'integerValueWhenUnsetConfiguration_valueWhenUnsetOption' - The built-in options for default values. The value can be one of the
-- following:
--
-- -   @RECOMMENDED@: The recommended value.
--
-- -   @NULL@: The @NULL@ value.
newIntegerValueWhenUnsetConfiguration ::
  IntegerValueWhenUnsetConfiguration
newIntegerValueWhenUnsetConfiguration =
  IntegerValueWhenUnsetConfiguration'
    { customValue =
        Prelude.Nothing,
      valueWhenUnsetOption = Prelude.Nothing
    }

-- | A custom value that\'s used when the value of a parameter isn\'t set.
integerValueWhenUnsetConfiguration_customValue :: Lens.Lens' IntegerValueWhenUnsetConfiguration (Prelude.Maybe Prelude.Integer)
integerValueWhenUnsetConfiguration_customValue = Lens.lens (\IntegerValueWhenUnsetConfiguration' {customValue} -> customValue) (\s@IntegerValueWhenUnsetConfiguration' {} a -> s {customValue = a} :: IntegerValueWhenUnsetConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | The built-in options for default values. The value can be one of the
-- following:
--
-- -   @RECOMMENDED@: The recommended value.
--
-- -   @NULL@: The @NULL@ value.
integerValueWhenUnsetConfiguration_valueWhenUnsetOption :: Lens.Lens' IntegerValueWhenUnsetConfiguration (Prelude.Maybe ValueWhenUnsetOption)
integerValueWhenUnsetConfiguration_valueWhenUnsetOption = Lens.lens (\IntegerValueWhenUnsetConfiguration' {valueWhenUnsetOption} -> valueWhenUnsetOption) (\s@IntegerValueWhenUnsetConfiguration' {} a -> s {valueWhenUnsetOption = a} :: IntegerValueWhenUnsetConfiguration)

instance
  Data.FromJSON
    IntegerValueWhenUnsetConfiguration
  where
  parseJSON =
    Data.withObject
      "IntegerValueWhenUnsetConfiguration"
      ( \x ->
          IntegerValueWhenUnsetConfiguration'
            Prelude.<$> (x Data..:? "CustomValue")
            Prelude.<*> (x Data..:? "ValueWhenUnsetOption")
      )

instance
  Prelude.Hashable
    IntegerValueWhenUnsetConfiguration
  where
  hashWithSalt
    _salt
    IntegerValueWhenUnsetConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` customValue
        `Prelude.hashWithSalt` valueWhenUnsetOption

instance
  Prelude.NFData
    IntegerValueWhenUnsetConfiguration
  where
  rnf IntegerValueWhenUnsetConfiguration' {..} =
    Prelude.rnf customValue
      `Prelude.seq` Prelude.rnf valueWhenUnsetOption

instance
  Data.ToJSON
    IntegerValueWhenUnsetConfiguration
  where
  toJSON IntegerValueWhenUnsetConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomValue" Data..=) Prelude.<$> customValue,
            ("ValueWhenUnsetOption" Data..=)
              Prelude.<$> valueWhenUnsetOption
          ]
      )
