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
-- Module      : Amazonka.AmplifyUiBuilder.Types.ComponentPropertyBindingProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.ComponentPropertyBindingProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Associates a component property to a binding property. This enables
-- exposed properties on the top level component to propagate data to the
-- component\'s property values.
--
-- /See:/ 'newComponentPropertyBindingProperties' smart constructor.
data ComponentPropertyBindingProperties = ComponentPropertyBindingProperties'
  { -- | The data field to bind the property to.
    field :: Prelude.Maybe Prelude.Text,
    -- | The component property to bind to the data field.
    property :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentPropertyBindingProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'field', 'componentPropertyBindingProperties_field' - The data field to bind the property to.
--
-- 'property', 'componentPropertyBindingProperties_property' - The component property to bind to the data field.
newComponentPropertyBindingProperties ::
  -- | 'property'
  Prelude.Text ->
  ComponentPropertyBindingProperties
newComponentPropertyBindingProperties pProperty_ =
  ComponentPropertyBindingProperties'
    { field =
        Prelude.Nothing,
      property = pProperty_
    }

-- | The data field to bind the property to.
componentPropertyBindingProperties_field :: Lens.Lens' ComponentPropertyBindingProperties (Prelude.Maybe Prelude.Text)
componentPropertyBindingProperties_field = Lens.lens (\ComponentPropertyBindingProperties' {field} -> field) (\s@ComponentPropertyBindingProperties' {} a -> s {field = a} :: ComponentPropertyBindingProperties)

-- | The component property to bind to the data field.
componentPropertyBindingProperties_property :: Lens.Lens' ComponentPropertyBindingProperties Prelude.Text
componentPropertyBindingProperties_property = Lens.lens (\ComponentPropertyBindingProperties' {property} -> property) (\s@ComponentPropertyBindingProperties' {} a -> s {property = a} :: ComponentPropertyBindingProperties)

instance
  Data.FromJSON
    ComponentPropertyBindingProperties
  where
  parseJSON =
    Data.withObject
      "ComponentPropertyBindingProperties"
      ( \x ->
          ComponentPropertyBindingProperties'
            Prelude.<$> (x Data..:? "field")
            Prelude.<*> (x Data..: "property")
      )

instance
  Prelude.Hashable
    ComponentPropertyBindingProperties
  where
  hashWithSalt
    _salt
    ComponentPropertyBindingProperties' {..} =
      _salt `Prelude.hashWithSalt` field
        `Prelude.hashWithSalt` property

instance
  Prelude.NFData
    ComponentPropertyBindingProperties
  where
  rnf ComponentPropertyBindingProperties' {..} =
    Prelude.rnf field
      `Prelude.seq` Prelude.rnf property

instance
  Data.ToJSON
    ComponentPropertyBindingProperties
  where
  toJSON ComponentPropertyBindingProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("field" Data..=) Prelude.<$> field,
            Prelude.Just ("property" Data..= property)
          ]
      )
