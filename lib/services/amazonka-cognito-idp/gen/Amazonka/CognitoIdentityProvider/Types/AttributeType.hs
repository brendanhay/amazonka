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
-- Module      : Amazonka.CognitoIdentityProvider.Types.AttributeType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.AttributeType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies whether the attribute is standard or custom.
--
-- /See:/ 'newAttributeType' smart constructor.
data AttributeType = AttributeType'
  { -- | The value of the attribute.
    value :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the attribute.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttributeType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'attributeType_value' - The value of the attribute.
--
-- 'name', 'attributeType_name' - The name of the attribute.
newAttributeType ::
  -- | 'name'
  Prelude.Text ->
  AttributeType
newAttributeType pName_ =
  AttributeType'
    { value = Prelude.Nothing,
      name = pName_
    }

-- | The value of the attribute.
attributeType_value :: Lens.Lens' AttributeType (Prelude.Maybe Prelude.Text)
attributeType_value = Lens.lens (\AttributeType' {value} -> value) (\s@AttributeType' {} a -> s {value = a} :: AttributeType) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the attribute.
attributeType_name :: Lens.Lens' AttributeType Prelude.Text
attributeType_name = Lens.lens (\AttributeType' {name} -> name) (\s@AttributeType' {} a -> s {name = a} :: AttributeType)

instance Data.FromJSON AttributeType where
  parseJSON =
    Data.withObject
      "AttributeType"
      ( \x ->
          AttributeType'
            Prelude.<$> (x Data..:? "Value") Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable AttributeType where
  hashWithSalt _salt AttributeType' {..} =
    _salt `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` name

instance Prelude.NFData AttributeType where
  rnf AttributeType' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf name

instance Data.ToJSON AttributeType where
  toJSON AttributeType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Value" Data..=) Prelude.<$> value,
            Prelude.Just ("Name" Data..= name)
          ]
      )
