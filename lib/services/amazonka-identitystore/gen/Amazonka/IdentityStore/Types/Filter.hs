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
-- Module      : Amazonka.IdentityStore.Types.Filter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IdentityStore.Types.Filter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A query filter used by @ListUsers@ and @ListGroups@. This filter object
-- provides the attribute name and attribute value to search users or
-- groups.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | The attribute path that is used to specify which attribute name to
    -- search. Length limit is 255 characters. For example, @UserName@ is a
    -- valid attribute path for the @ListUsers@ API, and @DisplayName@ is a
    -- valid attribute path for the @ListGroups@ API.
    attributePath :: Prelude.Text,
    -- | Represents the data for an attribute. Each attribute value is described
    -- as a name-value pair.
    attributeValue :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Filter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributePath', 'filter_attributePath' - The attribute path that is used to specify which attribute name to
-- search. Length limit is 255 characters. For example, @UserName@ is a
-- valid attribute path for the @ListUsers@ API, and @DisplayName@ is a
-- valid attribute path for the @ListGroups@ API.
--
-- 'attributeValue', 'filter_attributeValue' - Represents the data for an attribute. Each attribute value is described
-- as a name-value pair.
newFilter ::
  -- | 'attributePath'
  Prelude.Text ->
  -- | 'attributeValue'
  Prelude.Text ->
  Filter
newFilter pAttributePath_ pAttributeValue_ =
  Filter'
    { attributePath = pAttributePath_,
      attributeValue =
        Data._Sensitive Lens.# pAttributeValue_
    }

-- | The attribute path that is used to specify which attribute name to
-- search. Length limit is 255 characters. For example, @UserName@ is a
-- valid attribute path for the @ListUsers@ API, and @DisplayName@ is a
-- valid attribute path for the @ListGroups@ API.
filter_attributePath :: Lens.Lens' Filter Prelude.Text
filter_attributePath = Lens.lens (\Filter' {attributePath} -> attributePath) (\s@Filter' {} a -> s {attributePath = a} :: Filter)

-- | Represents the data for an attribute. Each attribute value is described
-- as a name-value pair.
filter_attributeValue :: Lens.Lens' Filter Prelude.Text
filter_attributeValue = Lens.lens (\Filter' {attributeValue} -> attributeValue) (\s@Filter' {} a -> s {attributeValue = a} :: Filter) Prelude.. Data._Sensitive

instance Prelude.Hashable Filter where
  hashWithSalt _salt Filter' {..} =
    _salt
      `Prelude.hashWithSalt` attributePath
      `Prelude.hashWithSalt` attributeValue

instance Prelude.NFData Filter where
  rnf Filter' {..} =
    Prelude.rnf attributePath
      `Prelude.seq` Prelude.rnf attributeValue

instance Data.ToJSON Filter where
  toJSON Filter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AttributePath" Data..= attributePath),
            Prelude.Just
              ("AttributeValue" Data..= attributeValue)
          ]
      )
