{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Glue.Types.Comparator
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Defines a property predicate.
--
-- /See:/ 'newPropertyPredicate' smart constructor.
data PropertyPredicate = PropertyPredicate'
  { -- | The key of the property.
    key :: Prelude.Maybe Prelude.Text,
    -- | The value of the property.
    value :: Prelude.Maybe Prelude.Text,
    -- | The comparator used to compare this property to others.
    comparator :: Prelude.Maybe Comparator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PropertyPredicate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'propertyPredicate_key' - The key of the property.
--
-- 'value', 'propertyPredicate_value' - The value of the property.
--
-- 'comparator', 'propertyPredicate_comparator' - The comparator used to compare this property to others.
newPropertyPredicate ::
  PropertyPredicate
newPropertyPredicate =
  PropertyPredicate'
    { key = Prelude.Nothing,
      value = Prelude.Nothing,
      comparator = Prelude.Nothing
    }

-- | The key of the property.
propertyPredicate_key :: Lens.Lens' PropertyPredicate (Prelude.Maybe Prelude.Text)
propertyPredicate_key = Lens.lens (\PropertyPredicate' {key} -> key) (\s@PropertyPredicate' {} a -> s {key = a} :: PropertyPredicate)

-- | The value of the property.
propertyPredicate_value :: Lens.Lens' PropertyPredicate (Prelude.Maybe Prelude.Text)
propertyPredicate_value = Lens.lens (\PropertyPredicate' {value} -> value) (\s@PropertyPredicate' {} a -> s {value = a} :: PropertyPredicate)

-- | The comparator used to compare this property to others.
propertyPredicate_comparator :: Lens.Lens' PropertyPredicate (Prelude.Maybe Comparator)
propertyPredicate_comparator = Lens.lens (\PropertyPredicate' {comparator} -> comparator) (\s@PropertyPredicate' {} a -> s {comparator = a} :: PropertyPredicate)

instance Prelude.Hashable PropertyPredicate

instance Prelude.NFData PropertyPredicate

instance Prelude.ToJSON PropertyPredicate where
  toJSON PropertyPredicate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Key" Prelude..=) Prelude.<$> key,
            ("Value" Prelude..=) Prelude.<$> value,
            ("Comparator" Prelude..=) Prelude.<$> comparator
          ]
      )
