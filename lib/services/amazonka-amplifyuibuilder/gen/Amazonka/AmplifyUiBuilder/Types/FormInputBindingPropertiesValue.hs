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
-- Module      : Amazonka.AmplifyUiBuilder.Types.FormInputBindingPropertiesValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.FormInputBindingPropertiesValue where

import Amazonka.AmplifyUiBuilder.Types.FormInputBindingPropertiesValueProperties
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the data binding configuration for a form\'s input fields at
-- runtime.You can use @FormInputBindingPropertiesValue@ to add exposed
-- properties to a form to allow different values to be entered when a form
-- is reused in different places in an app.
--
-- /See:/ 'newFormInputBindingPropertiesValue' smart constructor.
data FormInputBindingPropertiesValue = FormInputBindingPropertiesValue'
  { -- | Describes the properties to customize with data at runtime.
    bindingProperties :: Prelude.Maybe FormInputBindingPropertiesValueProperties,
    -- | The property type.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FormInputBindingPropertiesValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bindingProperties', 'formInputBindingPropertiesValue_bindingProperties' - Describes the properties to customize with data at runtime.
--
-- 'type'', 'formInputBindingPropertiesValue_type' - The property type.
newFormInputBindingPropertiesValue ::
  FormInputBindingPropertiesValue
newFormInputBindingPropertiesValue =
  FormInputBindingPropertiesValue'
    { bindingProperties =
        Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Describes the properties to customize with data at runtime.
formInputBindingPropertiesValue_bindingProperties :: Lens.Lens' FormInputBindingPropertiesValue (Prelude.Maybe FormInputBindingPropertiesValueProperties)
formInputBindingPropertiesValue_bindingProperties = Lens.lens (\FormInputBindingPropertiesValue' {bindingProperties} -> bindingProperties) (\s@FormInputBindingPropertiesValue' {} a -> s {bindingProperties = a} :: FormInputBindingPropertiesValue)

-- | The property type.
formInputBindingPropertiesValue_type :: Lens.Lens' FormInputBindingPropertiesValue (Prelude.Maybe Prelude.Text)
formInputBindingPropertiesValue_type = Lens.lens (\FormInputBindingPropertiesValue' {type'} -> type') (\s@FormInputBindingPropertiesValue' {} a -> s {type' = a} :: FormInputBindingPropertiesValue)

instance
  Data.FromJSON
    FormInputBindingPropertiesValue
  where
  parseJSON =
    Data.withObject
      "FormInputBindingPropertiesValue"
      ( \x ->
          FormInputBindingPropertiesValue'
            Prelude.<$> (x Data..:? "bindingProperties")
            Prelude.<*> (x Data..:? "type")
      )

instance
  Prelude.Hashable
    FormInputBindingPropertiesValue
  where
  hashWithSalt
    _salt
    FormInputBindingPropertiesValue' {..} =
      _salt
        `Prelude.hashWithSalt` bindingProperties
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    FormInputBindingPropertiesValue
  where
  rnf FormInputBindingPropertiesValue' {..} =
    Prelude.rnf bindingProperties
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON FormInputBindingPropertiesValue where
  toJSON FormInputBindingPropertiesValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bindingProperties" Data..=)
              Prelude.<$> bindingProperties,
            ("type" Data..=) Prelude.<$> type'
          ]
      )
