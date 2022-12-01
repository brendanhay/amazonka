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
-- Module      : Amazonka.Kendra.Types.AdditionalResultAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.AdditionalResultAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types.AdditionalResultAttributeValue
import Amazonka.Kendra.Types.AdditionalResultAttributeValueType
import qualified Amazonka.Prelude as Prelude

-- | An attribute returned from an index query.
--
-- /See:/ 'newAdditionalResultAttribute' smart constructor.
data AdditionalResultAttribute = AdditionalResultAttribute'
  { -- | The key that identifies the attribute.
    key :: Prelude.Text,
    -- | The data type of the @Value@ property.
    valueType :: AdditionalResultAttributeValueType,
    -- | An object that contains the attribute value.
    value :: AdditionalResultAttributeValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdditionalResultAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'additionalResultAttribute_key' - The key that identifies the attribute.
--
-- 'valueType', 'additionalResultAttribute_valueType' - The data type of the @Value@ property.
--
-- 'value', 'additionalResultAttribute_value' - An object that contains the attribute value.
newAdditionalResultAttribute ::
  -- | 'key'
  Prelude.Text ->
  -- | 'valueType'
  AdditionalResultAttributeValueType ->
  -- | 'value'
  AdditionalResultAttributeValue ->
  AdditionalResultAttribute
newAdditionalResultAttribute
  pKey_
  pValueType_
  pValue_ =
    AdditionalResultAttribute'
      { key = pKey_,
        valueType = pValueType_,
        value = pValue_
      }

-- | The key that identifies the attribute.
additionalResultAttribute_key :: Lens.Lens' AdditionalResultAttribute Prelude.Text
additionalResultAttribute_key = Lens.lens (\AdditionalResultAttribute' {key} -> key) (\s@AdditionalResultAttribute' {} a -> s {key = a} :: AdditionalResultAttribute)

-- | The data type of the @Value@ property.
additionalResultAttribute_valueType :: Lens.Lens' AdditionalResultAttribute AdditionalResultAttributeValueType
additionalResultAttribute_valueType = Lens.lens (\AdditionalResultAttribute' {valueType} -> valueType) (\s@AdditionalResultAttribute' {} a -> s {valueType = a} :: AdditionalResultAttribute)

-- | An object that contains the attribute value.
additionalResultAttribute_value :: Lens.Lens' AdditionalResultAttribute AdditionalResultAttributeValue
additionalResultAttribute_value = Lens.lens (\AdditionalResultAttribute' {value} -> value) (\s@AdditionalResultAttribute' {} a -> s {value = a} :: AdditionalResultAttribute)

instance Core.FromJSON AdditionalResultAttribute where
  parseJSON =
    Core.withObject
      "AdditionalResultAttribute"
      ( \x ->
          AdditionalResultAttribute'
            Prelude.<$> (x Core..: "Key")
            Prelude.<*> (x Core..: "ValueType")
            Prelude.<*> (x Core..: "Value")
      )

instance Prelude.Hashable AdditionalResultAttribute where
  hashWithSalt _salt AdditionalResultAttribute' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` valueType
      `Prelude.hashWithSalt` value

instance Prelude.NFData AdditionalResultAttribute where
  rnf AdditionalResultAttribute' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf valueType
      `Prelude.seq` Prelude.rnf value
