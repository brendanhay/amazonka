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
-- Module      : Amazonka.CloudDirectory.Types.AttributeNameAndValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.AttributeNameAndValue where

import Amazonka.CloudDirectory.Types.TypedAttributeValue
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Identifies the attribute name and value for a typed link.
--
-- /See:/ 'newAttributeNameAndValue' smart constructor.
data AttributeNameAndValue = AttributeNameAndValue'
  { -- | The attribute name of the typed link.
    attributeName :: Prelude.Text,
    -- | The value for the typed link.
    value :: TypedAttributeValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttributeNameAndValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeName', 'attributeNameAndValue_attributeName' - The attribute name of the typed link.
--
-- 'value', 'attributeNameAndValue_value' - The value for the typed link.
newAttributeNameAndValue ::
  -- | 'attributeName'
  Prelude.Text ->
  -- | 'value'
  TypedAttributeValue ->
  AttributeNameAndValue
newAttributeNameAndValue pAttributeName_ pValue_ =
  AttributeNameAndValue'
    { attributeName =
        pAttributeName_,
      value = pValue_
    }

-- | The attribute name of the typed link.
attributeNameAndValue_attributeName :: Lens.Lens' AttributeNameAndValue Prelude.Text
attributeNameAndValue_attributeName = Lens.lens (\AttributeNameAndValue' {attributeName} -> attributeName) (\s@AttributeNameAndValue' {} a -> s {attributeName = a} :: AttributeNameAndValue)

-- | The value for the typed link.
attributeNameAndValue_value :: Lens.Lens' AttributeNameAndValue TypedAttributeValue
attributeNameAndValue_value = Lens.lens (\AttributeNameAndValue' {value} -> value) (\s@AttributeNameAndValue' {} a -> s {value = a} :: AttributeNameAndValue)

instance Data.FromJSON AttributeNameAndValue where
  parseJSON =
    Data.withObject
      "AttributeNameAndValue"
      ( \x ->
          AttributeNameAndValue'
            Prelude.<$> (x Data..: "AttributeName")
            Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable AttributeNameAndValue where
  hashWithSalt _salt AttributeNameAndValue' {..} =
    _salt
      `Prelude.hashWithSalt` attributeName
      `Prelude.hashWithSalt` value

instance Prelude.NFData AttributeNameAndValue where
  rnf AttributeNameAndValue' {..} =
    Prelude.rnf attributeName
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON AttributeNameAndValue where
  toJSON AttributeNameAndValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AttributeName" Data..= attributeName),
            Prelude.Just ("Value" Data..= value)
          ]
      )
