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
-- Module      : Amazonka.SSM.Types.OpsItemRelatedItemsFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.OpsItemRelatedItemsFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.OpsItemRelatedItemsFilterKey
import Amazonka.SSM.Types.OpsItemRelatedItemsFilterOperator

-- | Describes a filter for a specific list of related-item resources.
--
-- /See:/ 'newOpsItemRelatedItemsFilter' smart constructor.
data OpsItemRelatedItemsFilter = OpsItemRelatedItemsFilter'
  { -- | The name of the filter key. Supported values include @ResourceUri@,
    -- @ResourceType@, or @AssociationId@.
    key :: OpsItemRelatedItemsFilterKey,
    -- | The values for the filter.
    values :: [Prelude.Text],
    -- | The operator used by the filter call. The only supported operator is
    -- @EQUAL@.
    operator :: OpsItemRelatedItemsFilterOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpsItemRelatedItemsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'opsItemRelatedItemsFilter_key' - The name of the filter key. Supported values include @ResourceUri@,
-- @ResourceType@, or @AssociationId@.
--
-- 'values', 'opsItemRelatedItemsFilter_values' - The values for the filter.
--
-- 'operator', 'opsItemRelatedItemsFilter_operator' - The operator used by the filter call. The only supported operator is
-- @EQUAL@.
newOpsItemRelatedItemsFilter ::
  -- | 'key'
  OpsItemRelatedItemsFilterKey ->
  -- | 'operator'
  OpsItemRelatedItemsFilterOperator ->
  OpsItemRelatedItemsFilter
newOpsItemRelatedItemsFilter pKey_ pOperator_ =
  OpsItemRelatedItemsFilter'
    { key = pKey_,
      values = Prelude.mempty,
      operator = pOperator_
    }

-- | The name of the filter key. Supported values include @ResourceUri@,
-- @ResourceType@, or @AssociationId@.
opsItemRelatedItemsFilter_key :: Lens.Lens' OpsItemRelatedItemsFilter OpsItemRelatedItemsFilterKey
opsItemRelatedItemsFilter_key = Lens.lens (\OpsItemRelatedItemsFilter' {key} -> key) (\s@OpsItemRelatedItemsFilter' {} a -> s {key = a} :: OpsItemRelatedItemsFilter)

-- | The values for the filter.
opsItemRelatedItemsFilter_values :: Lens.Lens' OpsItemRelatedItemsFilter [Prelude.Text]
opsItemRelatedItemsFilter_values = Lens.lens (\OpsItemRelatedItemsFilter' {values} -> values) (\s@OpsItemRelatedItemsFilter' {} a -> s {values = a} :: OpsItemRelatedItemsFilter) Prelude.. Lens.coerced

-- | The operator used by the filter call. The only supported operator is
-- @EQUAL@.
opsItemRelatedItemsFilter_operator :: Lens.Lens' OpsItemRelatedItemsFilter OpsItemRelatedItemsFilterOperator
opsItemRelatedItemsFilter_operator = Lens.lens (\OpsItemRelatedItemsFilter' {operator} -> operator) (\s@OpsItemRelatedItemsFilter' {} a -> s {operator = a} :: OpsItemRelatedItemsFilter)

instance Prelude.Hashable OpsItemRelatedItemsFilter where
  hashWithSalt _salt OpsItemRelatedItemsFilter' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` operator

instance Prelude.NFData OpsItemRelatedItemsFilter where
  rnf OpsItemRelatedItemsFilter' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf values
      `Prelude.seq` Prelude.rnf operator

instance Data.ToJSON OpsItemRelatedItemsFilter where
  toJSON OpsItemRelatedItemsFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Data..= key),
            Prelude.Just ("Values" Data..= values),
            Prelude.Just ("Operator" Data..= operator)
          ]
      )
