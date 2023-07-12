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
-- Module      : Amazonka.AmplifyUiBuilder.Types.ValueMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.ValueMapping where

import Amazonka.AmplifyUiBuilder.Types.FormInputValueProperty
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Associates a complex object with a display value. Use @ValueMapping@ to
-- store how to represent complex objects when they are displayed.
--
-- /See:/ 'newValueMapping' smart constructor.
data ValueMapping = ValueMapping'
  { -- | The value to display for the complex object.
    displayValue :: Prelude.Maybe FormInputValueProperty,
    -- | The complex object.
    value :: FormInputValueProperty
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValueMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayValue', 'valueMapping_displayValue' - The value to display for the complex object.
--
-- 'value', 'valueMapping_value' - The complex object.
newValueMapping ::
  -- | 'value'
  FormInputValueProperty ->
  ValueMapping
newValueMapping pValue_ =
  ValueMapping'
    { displayValue = Prelude.Nothing,
      value = pValue_
    }

-- | The value to display for the complex object.
valueMapping_displayValue :: Lens.Lens' ValueMapping (Prelude.Maybe FormInputValueProperty)
valueMapping_displayValue = Lens.lens (\ValueMapping' {displayValue} -> displayValue) (\s@ValueMapping' {} a -> s {displayValue = a} :: ValueMapping)

-- | The complex object.
valueMapping_value :: Lens.Lens' ValueMapping FormInputValueProperty
valueMapping_value = Lens.lens (\ValueMapping' {value} -> value) (\s@ValueMapping' {} a -> s {value = a} :: ValueMapping)

instance Data.FromJSON ValueMapping where
  parseJSON =
    Data.withObject
      "ValueMapping"
      ( \x ->
          ValueMapping'
            Prelude.<$> (x Data..:? "displayValue")
            Prelude.<*> (x Data..: "value")
      )

instance Prelude.Hashable ValueMapping where
  hashWithSalt _salt ValueMapping' {..} =
    _salt
      `Prelude.hashWithSalt` displayValue
      `Prelude.hashWithSalt` value

instance Prelude.NFData ValueMapping where
  rnf ValueMapping' {..} =
    Prelude.rnf displayValue
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON ValueMapping where
  toJSON ValueMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("displayValue" Data..=) Prelude.<$> displayValue,
            Prelude.Just ("value" Data..= value)
          ]
      )
