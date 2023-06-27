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
-- Module      : Amazonka.LexV2Models.Types.BuiltInIntentSortBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BuiltInIntentSortBy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.BuiltInIntentSortAttribute
import Amazonka.LexV2Models.Types.SortOrder
import qualified Amazonka.Prelude as Prelude

-- | Specifies attributes for sorting a list of built-in intents.
--
-- /See:/ 'newBuiltInIntentSortBy' smart constructor.
data BuiltInIntentSortBy = BuiltInIntentSortBy'
  { -- | The attribute to use to sort the list of built-in intents.
    attribute :: BuiltInIntentSortAttribute,
    -- | The order to sort the list. You can specify ascending or descending
    -- order.
    order :: SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BuiltInIntentSortBy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'builtInIntentSortBy_attribute' - The attribute to use to sort the list of built-in intents.
--
-- 'order', 'builtInIntentSortBy_order' - The order to sort the list. You can specify ascending or descending
-- order.
newBuiltInIntentSortBy ::
  -- | 'attribute'
  BuiltInIntentSortAttribute ->
  -- | 'order'
  SortOrder ->
  BuiltInIntentSortBy
newBuiltInIntentSortBy pAttribute_ pOrder_ =
  BuiltInIntentSortBy'
    { attribute = pAttribute_,
      order = pOrder_
    }

-- | The attribute to use to sort the list of built-in intents.
builtInIntentSortBy_attribute :: Lens.Lens' BuiltInIntentSortBy BuiltInIntentSortAttribute
builtInIntentSortBy_attribute = Lens.lens (\BuiltInIntentSortBy' {attribute} -> attribute) (\s@BuiltInIntentSortBy' {} a -> s {attribute = a} :: BuiltInIntentSortBy)

-- | The order to sort the list. You can specify ascending or descending
-- order.
builtInIntentSortBy_order :: Lens.Lens' BuiltInIntentSortBy SortOrder
builtInIntentSortBy_order = Lens.lens (\BuiltInIntentSortBy' {order} -> order) (\s@BuiltInIntentSortBy' {} a -> s {order = a} :: BuiltInIntentSortBy)

instance Prelude.Hashable BuiltInIntentSortBy where
  hashWithSalt _salt BuiltInIntentSortBy' {..} =
    _salt
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` order

instance Prelude.NFData BuiltInIntentSortBy where
  rnf BuiltInIntentSortBy' {..} =
    Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf order

instance Data.ToJSON BuiltInIntentSortBy where
  toJSON BuiltInIntentSortBy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("attribute" Data..= attribute),
            Prelude.Just ("order" Data..= order)
          ]
      )
