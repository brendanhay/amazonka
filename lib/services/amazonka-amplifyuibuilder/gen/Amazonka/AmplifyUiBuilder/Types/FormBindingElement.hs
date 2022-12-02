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
-- Module      : Amazonka.AmplifyUiBuilder.Types.FormBindingElement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.FormBindingElement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes how to bind a component property to form data.
--
-- /See:/ 'newFormBindingElement' smart constructor.
data FormBindingElement = FormBindingElement'
  { -- | The name of the component to retrieve a value from.
    element :: Prelude.Text,
    -- | The property to retrieve a value from.
    property :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FormBindingElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'element', 'formBindingElement_element' - The name of the component to retrieve a value from.
--
-- 'property', 'formBindingElement_property' - The property to retrieve a value from.
newFormBindingElement ::
  -- | 'element'
  Prelude.Text ->
  -- | 'property'
  Prelude.Text ->
  FormBindingElement
newFormBindingElement pElement_ pProperty_ =
  FormBindingElement'
    { element = pElement_,
      property = pProperty_
    }

-- | The name of the component to retrieve a value from.
formBindingElement_element :: Lens.Lens' FormBindingElement Prelude.Text
formBindingElement_element = Lens.lens (\FormBindingElement' {element} -> element) (\s@FormBindingElement' {} a -> s {element = a} :: FormBindingElement)

-- | The property to retrieve a value from.
formBindingElement_property :: Lens.Lens' FormBindingElement Prelude.Text
formBindingElement_property = Lens.lens (\FormBindingElement' {property} -> property) (\s@FormBindingElement' {} a -> s {property = a} :: FormBindingElement)

instance Data.FromJSON FormBindingElement where
  parseJSON =
    Data.withObject
      "FormBindingElement"
      ( \x ->
          FormBindingElement'
            Prelude.<$> (x Data..: "element")
            Prelude.<*> (x Data..: "property")
      )

instance Prelude.Hashable FormBindingElement where
  hashWithSalt _salt FormBindingElement' {..} =
    _salt `Prelude.hashWithSalt` element
      `Prelude.hashWithSalt` property

instance Prelude.NFData FormBindingElement where
  rnf FormBindingElement' {..} =
    Prelude.rnf element
      `Prelude.seq` Prelude.rnf property

instance Data.ToJSON FormBindingElement where
  toJSON FormBindingElement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("element" Data..= element),
            Prelude.Just ("property" Data..= property)
          ]
      )
