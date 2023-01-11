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
-- Module      : Amazonka.IdentityStore.Types.UniqueAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IdentityStore.Types.UniqueAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IdentityStore.Types.AttributeValue
import qualified Amazonka.Prelude as Prelude

-- | An entity attribute that\'s unique to a specific entity.
--
-- /See:/ 'newUniqueAttribute' smart constructor.
data UniqueAttribute = UniqueAttribute'
  { -- | A string representation of the path to a given attribute or
    -- sub-attribute. Supports JMESPath.
    attributePath :: Prelude.Text,
    -- | The value of the attribute. This is a @Document@ type. This type is not
    -- supported by Java V1, Go V1, and older versions of the AWS CLI.
    attributeValue :: AttributeValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UniqueAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributePath', 'uniqueAttribute_attributePath' - A string representation of the path to a given attribute or
-- sub-attribute. Supports JMESPath.
--
-- 'attributeValue', 'uniqueAttribute_attributeValue' - The value of the attribute. This is a @Document@ type. This type is not
-- supported by Java V1, Go V1, and older versions of the AWS CLI.
newUniqueAttribute ::
  -- | 'attributePath'
  Prelude.Text ->
  -- | 'attributeValue'
  AttributeValue ->
  UniqueAttribute
newUniqueAttribute pAttributePath_ pAttributeValue_ =
  UniqueAttribute'
    { attributePath = pAttributePath_,
      attributeValue = pAttributeValue_
    }

-- | A string representation of the path to a given attribute or
-- sub-attribute. Supports JMESPath.
uniqueAttribute_attributePath :: Lens.Lens' UniqueAttribute Prelude.Text
uniqueAttribute_attributePath = Lens.lens (\UniqueAttribute' {attributePath} -> attributePath) (\s@UniqueAttribute' {} a -> s {attributePath = a} :: UniqueAttribute)

-- | The value of the attribute. This is a @Document@ type. This type is not
-- supported by Java V1, Go V1, and older versions of the AWS CLI.
uniqueAttribute_attributeValue :: Lens.Lens' UniqueAttribute AttributeValue
uniqueAttribute_attributeValue = Lens.lens (\UniqueAttribute' {attributeValue} -> attributeValue) (\s@UniqueAttribute' {} a -> s {attributeValue = a} :: UniqueAttribute)

instance Prelude.Hashable UniqueAttribute where
  hashWithSalt _salt UniqueAttribute' {..} =
    _salt `Prelude.hashWithSalt` attributePath
      `Prelude.hashWithSalt` attributeValue

instance Prelude.NFData UniqueAttribute where
  rnf UniqueAttribute' {..} =
    Prelude.rnf attributePath
      `Prelude.seq` Prelude.rnf attributeValue

instance Data.ToJSON UniqueAttribute where
  toJSON UniqueAttribute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AttributePath" Data..= attributePath),
            Prelude.Just
              ("AttributeValue" Data..= attributeValue)
          ]
      )
