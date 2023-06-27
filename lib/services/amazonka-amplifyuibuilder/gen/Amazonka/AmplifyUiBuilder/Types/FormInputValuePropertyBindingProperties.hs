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
-- Module      : Amazonka.AmplifyUiBuilder.Types.FormInputValuePropertyBindingProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.FormInputValuePropertyBindingProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Associates a form property to a binding property. This enables exposed
-- properties on the top level form to propagate data to the form\'s
-- property values.
--
-- /See:/ 'newFormInputValuePropertyBindingProperties' smart constructor.
data FormInputValuePropertyBindingProperties = FormInputValuePropertyBindingProperties'
  { -- | The data field to bind the property to.
    field :: Prelude.Maybe Prelude.Text,
    -- | The form property to bind to the data field.
    property :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FormInputValuePropertyBindingProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'field', 'formInputValuePropertyBindingProperties_field' - The data field to bind the property to.
--
-- 'property', 'formInputValuePropertyBindingProperties_property' - The form property to bind to the data field.
newFormInputValuePropertyBindingProperties ::
  -- | 'property'
  Prelude.Text ->
  FormInputValuePropertyBindingProperties
newFormInputValuePropertyBindingProperties pProperty_ =
  FormInputValuePropertyBindingProperties'
    { field =
        Prelude.Nothing,
      property = pProperty_
    }

-- | The data field to bind the property to.
formInputValuePropertyBindingProperties_field :: Lens.Lens' FormInputValuePropertyBindingProperties (Prelude.Maybe Prelude.Text)
formInputValuePropertyBindingProperties_field = Lens.lens (\FormInputValuePropertyBindingProperties' {field} -> field) (\s@FormInputValuePropertyBindingProperties' {} a -> s {field = a} :: FormInputValuePropertyBindingProperties)

-- | The form property to bind to the data field.
formInputValuePropertyBindingProperties_property :: Lens.Lens' FormInputValuePropertyBindingProperties Prelude.Text
formInputValuePropertyBindingProperties_property = Lens.lens (\FormInputValuePropertyBindingProperties' {property} -> property) (\s@FormInputValuePropertyBindingProperties' {} a -> s {property = a} :: FormInputValuePropertyBindingProperties)

instance
  Data.FromJSON
    FormInputValuePropertyBindingProperties
  where
  parseJSON =
    Data.withObject
      "FormInputValuePropertyBindingProperties"
      ( \x ->
          FormInputValuePropertyBindingProperties'
            Prelude.<$> (x Data..:? "field")
            Prelude.<*> (x Data..: "property")
      )

instance
  Prelude.Hashable
    FormInputValuePropertyBindingProperties
  where
  hashWithSalt
    _salt
    FormInputValuePropertyBindingProperties' {..} =
      _salt
        `Prelude.hashWithSalt` field
        `Prelude.hashWithSalt` property

instance
  Prelude.NFData
    FormInputValuePropertyBindingProperties
  where
  rnf FormInputValuePropertyBindingProperties' {..} =
    Prelude.rnf field
      `Prelude.seq` Prelude.rnf property

instance
  Data.ToJSON
    FormInputValuePropertyBindingProperties
  where
  toJSON FormInputValuePropertyBindingProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("field" Data..=) Prelude.<$> field,
            Prelude.Just ("property" Data..= property)
          ]
      )
