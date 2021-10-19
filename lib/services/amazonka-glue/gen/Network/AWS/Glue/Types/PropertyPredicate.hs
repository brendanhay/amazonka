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
-- Module      : Network.AWS.Glue.Types.PropertyPredicate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PropertyPredicate where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.Comparator
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Defines a property predicate.
--
-- /See:/ 'newPropertyPredicate' smart constructor.
data PropertyPredicate = PropertyPredicate'
  { -- | The value of the property.
    value :: Prelude.Maybe Prelude.Text,
    -- | The key of the property.
    key :: Prelude.Maybe Prelude.Text,
    -- | The comparator used to compare this property to others.
    comparator :: Prelude.Maybe Comparator
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
-- 'value', 'propertyPredicate_value' - The value of the property.
--
-- 'key', 'propertyPredicate_key' - The key of the property.
--
-- 'comparator', 'propertyPredicate_comparator' - The comparator used to compare this property to others.
newPropertyPredicate ::
  PropertyPredicate
newPropertyPredicate =
  PropertyPredicate'
    { value = Prelude.Nothing,
      key = Prelude.Nothing,
      comparator = Prelude.Nothing
    }

-- | The value of the property.
propertyPredicate_value :: Lens.Lens' PropertyPredicate (Prelude.Maybe Prelude.Text)
propertyPredicate_value = Lens.lens (\PropertyPredicate' {value} -> value) (\s@PropertyPredicate' {} a -> s {value = a} :: PropertyPredicate)

-- | The key of the property.
propertyPredicate_key :: Lens.Lens' PropertyPredicate (Prelude.Maybe Prelude.Text)
propertyPredicate_key = Lens.lens (\PropertyPredicate' {key} -> key) (\s@PropertyPredicate' {} a -> s {key = a} :: PropertyPredicate)

-- | The comparator used to compare this property to others.
propertyPredicate_comparator :: Lens.Lens' PropertyPredicate (Prelude.Maybe Comparator)
propertyPredicate_comparator = Lens.lens (\PropertyPredicate' {comparator} -> comparator) (\s@PropertyPredicate' {} a -> s {comparator = a} :: PropertyPredicate)

instance Prelude.Hashable PropertyPredicate

instance Prelude.NFData PropertyPredicate

instance Core.ToJSON PropertyPredicate where
  toJSON PropertyPredicate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Value" Core..=) Prelude.<$> value,
            ("Key" Core..=) Prelude.<$> key,
            ("Comparator" Core..=) Prelude.<$> comparator
          ]
      )
