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
-- Module      : Amazonka.IdentityStore.Types.AttributeOperation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IdentityStore.Types.AttributeOperation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IdentityStore.Types.AttributeValue
import qualified Amazonka.Prelude as Prelude

-- | An operation that applies to the requested group. This operation might
-- add, replace, or remove an attribute.
--
-- /See:/ 'newAttributeOperation' smart constructor.
data AttributeOperation = AttributeOperation'
  { -- | The value of the attribute. This is a @Document@ type. This type is not
    -- supported by Java V1, Go V1, and older versions of the AWS CLI.
    attributeValue :: Prelude.Maybe AttributeValue,
    -- | A string representation of the path to a given attribute or
    -- sub-attribute. Supports JMESPath.
    attributePath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttributeOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeValue', 'attributeOperation_attributeValue' - The value of the attribute. This is a @Document@ type. This type is not
-- supported by Java V1, Go V1, and older versions of the AWS CLI.
--
-- 'attributePath', 'attributeOperation_attributePath' - A string representation of the path to a given attribute or
-- sub-attribute. Supports JMESPath.
newAttributeOperation ::
  -- | 'attributePath'
  Prelude.Text ->
  AttributeOperation
newAttributeOperation pAttributePath_ =
  AttributeOperation'
    { attributeValue =
        Prelude.Nothing,
      attributePath = pAttributePath_
    }

-- | The value of the attribute. This is a @Document@ type. This type is not
-- supported by Java V1, Go V1, and older versions of the AWS CLI.
attributeOperation_attributeValue :: Lens.Lens' AttributeOperation (Prelude.Maybe AttributeValue)
attributeOperation_attributeValue = Lens.lens (\AttributeOperation' {attributeValue} -> attributeValue) (\s@AttributeOperation' {} a -> s {attributeValue = a} :: AttributeOperation)

-- | A string representation of the path to a given attribute or
-- sub-attribute. Supports JMESPath.
attributeOperation_attributePath :: Lens.Lens' AttributeOperation Prelude.Text
attributeOperation_attributePath = Lens.lens (\AttributeOperation' {attributePath} -> attributePath) (\s@AttributeOperation' {} a -> s {attributePath = a} :: AttributeOperation)

instance Prelude.Hashable AttributeOperation where
  hashWithSalt _salt AttributeOperation' {..} =
    _salt `Prelude.hashWithSalt` attributeValue
      `Prelude.hashWithSalt` attributePath

instance Prelude.NFData AttributeOperation where
  rnf AttributeOperation' {..} =
    Prelude.rnf attributeValue
      `Prelude.seq` Prelude.rnf attributePath

instance Core.ToJSON AttributeOperation where
  toJSON AttributeOperation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AttributeValue" Core..=)
              Prelude.<$> attributeValue,
            Prelude.Just
              ("AttributePath" Core..= attributePath)
          ]
      )
