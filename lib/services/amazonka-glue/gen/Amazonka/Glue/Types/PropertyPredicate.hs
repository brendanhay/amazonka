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
-- Module      : Amazonka.Glue.Types.PropertyPredicate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.PropertyPredicate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.Comparator
import qualified Amazonka.Prelude as Prelude

-- | Defines a property predicate.
--
-- /See:/ 'newPropertyPredicate' smart constructor.
data PropertyPredicate = PropertyPredicate'
  { -- | The comparator used to compare this property to others.
    comparator :: Prelude.Maybe Comparator,
    -- | The key of the property.
    key :: Prelude.Maybe Prelude.Text,
    -- | The value of the property.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PropertyPredicate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comparator', 'propertyPredicate_comparator' - The comparator used to compare this property to others.
--
-- 'key', 'propertyPredicate_key' - The key of the property.
--
-- 'value', 'propertyPredicate_value' - The value of the property.
newPropertyPredicate ::
  PropertyPredicate
newPropertyPredicate =
  PropertyPredicate'
    { comparator = Prelude.Nothing,
      key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The comparator used to compare this property to others.
propertyPredicate_comparator :: Lens.Lens' PropertyPredicate (Prelude.Maybe Comparator)
propertyPredicate_comparator = Lens.lens (\PropertyPredicate' {comparator} -> comparator) (\s@PropertyPredicate' {} a -> s {comparator = a} :: PropertyPredicate)

-- | The key of the property.
propertyPredicate_key :: Lens.Lens' PropertyPredicate (Prelude.Maybe Prelude.Text)
propertyPredicate_key = Lens.lens (\PropertyPredicate' {key} -> key) (\s@PropertyPredicate' {} a -> s {key = a} :: PropertyPredicate)

-- | The value of the property.
propertyPredicate_value :: Lens.Lens' PropertyPredicate (Prelude.Maybe Prelude.Text)
propertyPredicate_value = Lens.lens (\PropertyPredicate' {value} -> value) (\s@PropertyPredicate' {} a -> s {value = a} :: PropertyPredicate)

instance Prelude.Hashable PropertyPredicate where
  hashWithSalt _salt PropertyPredicate' {..} =
    _salt `Prelude.hashWithSalt` comparator
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData PropertyPredicate where
  rnf PropertyPredicate' {..} =
    Prelude.rnf comparator
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON PropertyPredicate where
  toJSON PropertyPredicate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Comparator" Data..=) Prelude.<$> comparator,
            ("Key" Data..=) Prelude.<$> key,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
