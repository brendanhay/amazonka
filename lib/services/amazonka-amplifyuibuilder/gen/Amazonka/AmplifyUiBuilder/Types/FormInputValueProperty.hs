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
-- Module      : Amazonka.AmplifyUiBuilder.Types.FormInputValueProperty
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.FormInputValueProperty where

import Amazonka.AmplifyUiBuilder.Types.FormInputValuePropertyBindingProperties
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration for an input field on a form. Use
-- @FormInputValueProperty@ to specify the values to render or bind by
-- default.
--
-- /See:/ 'newFormInputValueProperty' smart constructor.
data FormInputValueProperty = FormInputValueProperty'
  { -- | The information to bind fields to data at runtime.
    bindingProperties :: Prelude.Maybe FormInputValuePropertyBindingProperties,
    -- | A list of form properties to concatenate to create the value to assign
    -- to this field property.
    concat :: Prelude.Maybe [FormInputValueProperty],
    -- | The value to assign to the input field.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FormInputValueProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bindingProperties', 'formInputValueProperty_bindingProperties' - The information to bind fields to data at runtime.
--
-- 'concat', 'formInputValueProperty_concat' - A list of form properties to concatenate to create the value to assign
-- to this field property.
--
-- 'value', 'formInputValueProperty_value' - The value to assign to the input field.
newFormInputValueProperty ::
  FormInputValueProperty
newFormInputValueProperty =
  FormInputValueProperty'
    { bindingProperties =
        Prelude.Nothing,
      concat = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The information to bind fields to data at runtime.
formInputValueProperty_bindingProperties :: Lens.Lens' FormInputValueProperty (Prelude.Maybe FormInputValuePropertyBindingProperties)
formInputValueProperty_bindingProperties = Lens.lens (\FormInputValueProperty' {bindingProperties} -> bindingProperties) (\s@FormInputValueProperty' {} a -> s {bindingProperties = a} :: FormInputValueProperty)

-- | A list of form properties to concatenate to create the value to assign
-- to this field property.
formInputValueProperty_concat :: Lens.Lens' FormInputValueProperty (Prelude.Maybe [FormInputValueProperty])
formInputValueProperty_concat = Lens.lens (\FormInputValueProperty' {concat} -> concat) (\s@FormInputValueProperty' {} a -> s {concat = a} :: FormInputValueProperty) Prelude.. Lens.mapping Lens.coerced

-- | The value to assign to the input field.
formInputValueProperty_value :: Lens.Lens' FormInputValueProperty (Prelude.Maybe Prelude.Text)
formInputValueProperty_value = Lens.lens (\FormInputValueProperty' {value} -> value) (\s@FormInputValueProperty' {} a -> s {value = a} :: FormInputValueProperty)

instance Data.FromJSON FormInputValueProperty where
  parseJSON =
    Data.withObject
      "FormInputValueProperty"
      ( \x ->
          FormInputValueProperty'
            Prelude.<$> (x Data..:? "bindingProperties")
            Prelude.<*> (x Data..:? "concat" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable FormInputValueProperty where
  hashWithSalt _salt FormInputValueProperty' {..} =
    _salt
      `Prelude.hashWithSalt` bindingProperties
      `Prelude.hashWithSalt` concat
      `Prelude.hashWithSalt` value

instance Prelude.NFData FormInputValueProperty where
  rnf FormInputValueProperty' {..} =
    Prelude.rnf bindingProperties
      `Prelude.seq` Prelude.rnf concat
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON FormInputValueProperty where
  toJSON FormInputValueProperty' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bindingProperties" Data..=)
              Prelude.<$> bindingProperties,
            ("concat" Data..=) Prelude.<$> concat,
            ("value" Data..=) Prelude.<$> value
          ]
      )
