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
-- Module      : Amazonka.AmplifyUiBuilder.Types.ComponentBindingPropertiesValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.ComponentBindingPropertiesValue where

import Amazonka.AmplifyUiBuilder.Types.ComponentBindingPropertiesValueProperties
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the data binding configuration for a component at runtime.
-- You can use @ComponentBindingPropertiesValue@ to add exposed properties
-- to a component to allow different values to be entered when a component
-- is reused in different places in an app.
--
-- /See:/ 'newComponentBindingPropertiesValue' smart constructor.
data ComponentBindingPropertiesValue = ComponentBindingPropertiesValue'
  { -- | The property type.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The default value of the property.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | Describes the properties to customize with data at runtime.
    bindingProperties :: Prelude.Maybe ComponentBindingPropertiesValueProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentBindingPropertiesValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'componentBindingPropertiesValue_type' - The property type.
--
-- 'defaultValue', 'componentBindingPropertiesValue_defaultValue' - The default value of the property.
--
-- 'bindingProperties', 'componentBindingPropertiesValue_bindingProperties' - Describes the properties to customize with data at runtime.
newComponentBindingPropertiesValue ::
  ComponentBindingPropertiesValue
newComponentBindingPropertiesValue =
  ComponentBindingPropertiesValue'
    { type' =
        Prelude.Nothing,
      defaultValue = Prelude.Nothing,
      bindingProperties = Prelude.Nothing
    }

-- | The property type.
componentBindingPropertiesValue_type :: Lens.Lens' ComponentBindingPropertiesValue (Prelude.Maybe Prelude.Text)
componentBindingPropertiesValue_type = Lens.lens (\ComponentBindingPropertiesValue' {type'} -> type') (\s@ComponentBindingPropertiesValue' {} a -> s {type' = a} :: ComponentBindingPropertiesValue)

-- | The default value of the property.
componentBindingPropertiesValue_defaultValue :: Lens.Lens' ComponentBindingPropertiesValue (Prelude.Maybe Prelude.Text)
componentBindingPropertiesValue_defaultValue = Lens.lens (\ComponentBindingPropertiesValue' {defaultValue} -> defaultValue) (\s@ComponentBindingPropertiesValue' {} a -> s {defaultValue = a} :: ComponentBindingPropertiesValue)

-- | Describes the properties to customize with data at runtime.
componentBindingPropertiesValue_bindingProperties :: Lens.Lens' ComponentBindingPropertiesValue (Prelude.Maybe ComponentBindingPropertiesValueProperties)
componentBindingPropertiesValue_bindingProperties = Lens.lens (\ComponentBindingPropertiesValue' {bindingProperties} -> bindingProperties) (\s@ComponentBindingPropertiesValue' {} a -> s {bindingProperties = a} :: ComponentBindingPropertiesValue)

instance
  Data.FromJSON
    ComponentBindingPropertiesValue
  where
  parseJSON =
    Data.withObject
      "ComponentBindingPropertiesValue"
      ( \x ->
          ComponentBindingPropertiesValue'
            Prelude.<$> (x Data..:? "type")
            Prelude.<*> (x Data..:? "defaultValue")
            Prelude.<*> (x Data..:? "bindingProperties")
      )

instance
  Prelude.Hashable
    ComponentBindingPropertiesValue
  where
  hashWithSalt
    _salt
    ComponentBindingPropertiesValue' {..} =
      _salt `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` defaultValue
        `Prelude.hashWithSalt` bindingProperties

instance
  Prelude.NFData
    ComponentBindingPropertiesValue
  where
  rnf ComponentBindingPropertiesValue' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf bindingProperties

instance Data.ToJSON ComponentBindingPropertiesValue where
  toJSON ComponentBindingPropertiesValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("type" Data..=) Prelude.<$> type',
            ("defaultValue" Data..=) Prelude.<$> defaultValue,
            ("bindingProperties" Data..=)
              Prelude.<$> bindingProperties
          ]
      )
