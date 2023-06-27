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
-- Module      : Amazonka.AmplifyUiBuilder.Types.ValueMappings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.ValueMappings where

import Amazonka.AmplifyUiBuilder.Types.FormInputBindingPropertiesValue
import Amazonka.AmplifyUiBuilder.Types.ValueMapping
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the data binding configuration for a value map.
--
-- /See:/ 'newValueMappings' smart constructor.
data ValueMappings = ValueMappings'
  { -- | The information to bind fields to data at runtime.
    bindingProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text FormInputBindingPropertiesValue),
    -- | The value and display value pairs.
    values :: [ValueMapping]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValueMappings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bindingProperties', 'valueMappings_bindingProperties' - The information to bind fields to data at runtime.
--
-- 'values', 'valueMappings_values' - The value and display value pairs.
newValueMappings ::
  ValueMappings
newValueMappings =
  ValueMappings'
    { bindingProperties = Prelude.Nothing,
      values = Prelude.mempty
    }

-- | The information to bind fields to data at runtime.
valueMappings_bindingProperties :: Lens.Lens' ValueMappings (Prelude.Maybe (Prelude.HashMap Prelude.Text FormInputBindingPropertiesValue))
valueMappings_bindingProperties = Lens.lens (\ValueMappings' {bindingProperties} -> bindingProperties) (\s@ValueMappings' {} a -> s {bindingProperties = a} :: ValueMappings) Prelude.. Lens.mapping Lens.coerced

-- | The value and display value pairs.
valueMappings_values :: Lens.Lens' ValueMappings [ValueMapping]
valueMappings_values = Lens.lens (\ValueMappings' {values} -> values) (\s@ValueMappings' {} a -> s {values = a} :: ValueMappings) Prelude.. Lens.coerced

instance Data.FromJSON ValueMappings where
  parseJSON =
    Data.withObject
      "ValueMappings"
      ( \x ->
          ValueMappings'
            Prelude.<$> ( x
                            Data..:? "bindingProperties"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ValueMappings where
  hashWithSalt _salt ValueMappings' {..} =
    _salt
      `Prelude.hashWithSalt` bindingProperties
      `Prelude.hashWithSalt` values

instance Prelude.NFData ValueMappings where
  rnf ValueMappings' {..} =
    Prelude.rnf bindingProperties
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON ValueMappings where
  toJSON ValueMappings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bindingProperties" Data..=)
              Prelude.<$> bindingProperties,
            Prelude.Just ("values" Data..= values)
          ]
      )
