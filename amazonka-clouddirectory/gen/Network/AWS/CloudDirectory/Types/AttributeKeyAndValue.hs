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
-- Module      : Network.AWS.CloudDirectory.Types.AttributeKeyAndValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.AttributeKeyAndValue where

import Network.AWS.CloudDirectory.Types.AttributeKey
import Network.AWS.CloudDirectory.Types.TypedAttributeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The combination of an attribute key and an attribute value.
--
-- /See:/ 'newAttributeKeyAndValue' smart constructor.
data AttributeKeyAndValue = AttributeKeyAndValue'
  { -- | The key of the attribute.
    key :: AttributeKey,
    -- | The value of the attribute.
    value :: TypedAttributeValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AttributeKeyAndValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'attributeKeyAndValue_key' - The key of the attribute.
--
-- 'value', 'attributeKeyAndValue_value' - The value of the attribute.
newAttributeKeyAndValue ::
  -- | 'key'
  AttributeKey ->
  -- | 'value'
  TypedAttributeValue ->
  AttributeKeyAndValue
newAttributeKeyAndValue pKey_ pValue_ =
  AttributeKeyAndValue' {key = pKey_, value = pValue_}

-- | The key of the attribute.
attributeKeyAndValue_key :: Lens.Lens' AttributeKeyAndValue AttributeKey
attributeKeyAndValue_key = Lens.lens (\AttributeKeyAndValue' {key} -> key) (\s@AttributeKeyAndValue' {} a -> s {key = a} :: AttributeKeyAndValue)

-- | The value of the attribute.
attributeKeyAndValue_value :: Lens.Lens' AttributeKeyAndValue TypedAttributeValue
attributeKeyAndValue_value = Lens.lens (\AttributeKeyAndValue' {value} -> value) (\s@AttributeKeyAndValue' {} a -> s {value = a} :: AttributeKeyAndValue)

instance Prelude.FromJSON AttributeKeyAndValue where
  parseJSON =
    Prelude.withObject
      "AttributeKeyAndValue"
      ( \x ->
          AttributeKeyAndValue'
            Prelude.<$> (x Prelude..: "Key")
            Prelude.<*> (x Prelude..: "Value")
      )

instance Prelude.Hashable AttributeKeyAndValue

instance Prelude.NFData AttributeKeyAndValue

instance Prelude.ToJSON AttributeKeyAndValue where
  toJSON AttributeKeyAndValue' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Prelude..= key),
            Prelude.Just ("Value" Prelude..= value)
          ]
      )
