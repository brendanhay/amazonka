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
-- Module      : Amazonka.LexV2Models.Types.BuiltInSlotTypeSortBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BuiltInSlotTypeSortBy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.BuiltInSlotTypeSortAttribute
import Amazonka.LexV2Models.Types.SortOrder
import qualified Amazonka.Prelude as Prelude

-- | Specifies attributes for sorting a list of built-in slot types.
--
-- /See:/ 'newBuiltInSlotTypeSortBy' smart constructor.
data BuiltInSlotTypeSortBy = BuiltInSlotTypeSortBy'
  { -- | The attribute to use to sort the list of built-in intents.
    attribute :: BuiltInSlotTypeSortAttribute,
    -- | The order to sort the list. You can choose ascending or descending.
    order :: SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BuiltInSlotTypeSortBy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'builtInSlotTypeSortBy_attribute' - The attribute to use to sort the list of built-in intents.
--
-- 'order', 'builtInSlotTypeSortBy_order' - The order to sort the list. You can choose ascending or descending.
newBuiltInSlotTypeSortBy ::
  -- | 'attribute'
  BuiltInSlotTypeSortAttribute ->
  -- | 'order'
  SortOrder ->
  BuiltInSlotTypeSortBy
newBuiltInSlotTypeSortBy pAttribute_ pOrder_ =
  BuiltInSlotTypeSortBy'
    { attribute = pAttribute_,
      order = pOrder_
    }

-- | The attribute to use to sort the list of built-in intents.
builtInSlotTypeSortBy_attribute :: Lens.Lens' BuiltInSlotTypeSortBy BuiltInSlotTypeSortAttribute
builtInSlotTypeSortBy_attribute = Lens.lens (\BuiltInSlotTypeSortBy' {attribute} -> attribute) (\s@BuiltInSlotTypeSortBy' {} a -> s {attribute = a} :: BuiltInSlotTypeSortBy)

-- | The order to sort the list. You can choose ascending or descending.
builtInSlotTypeSortBy_order :: Lens.Lens' BuiltInSlotTypeSortBy SortOrder
builtInSlotTypeSortBy_order = Lens.lens (\BuiltInSlotTypeSortBy' {order} -> order) (\s@BuiltInSlotTypeSortBy' {} a -> s {order = a} :: BuiltInSlotTypeSortBy)

instance Prelude.Hashable BuiltInSlotTypeSortBy where
  hashWithSalt _salt BuiltInSlotTypeSortBy' {..} =
    _salt
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` order

instance Prelude.NFData BuiltInSlotTypeSortBy where
  rnf BuiltInSlotTypeSortBy' {..} =
    Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf order

instance Data.ToJSON BuiltInSlotTypeSortBy where
  toJSON BuiltInSlotTypeSortBy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("attribute" Data..= attribute),
            Prelude.Just ("order" Data..= order)
          ]
      )
