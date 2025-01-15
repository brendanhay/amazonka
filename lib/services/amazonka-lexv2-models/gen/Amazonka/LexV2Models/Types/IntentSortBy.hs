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
-- Module      : Amazonka.LexV2Models.Types.IntentSortBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.IntentSortBy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.IntentSortAttribute
import Amazonka.LexV2Models.Types.SortOrder
import qualified Amazonka.Prelude as Prelude

-- | Specifies attributes for sorting a list of intents.
--
-- /See:/ 'newIntentSortBy' smart constructor.
data IntentSortBy = IntentSortBy'
  { -- | The attribute to use to sort the list of intents.
    attribute :: IntentSortAttribute,
    -- | The order to sort the list. You can choose ascending or descending.
    order :: SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntentSortBy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'intentSortBy_attribute' - The attribute to use to sort the list of intents.
--
-- 'order', 'intentSortBy_order' - The order to sort the list. You can choose ascending or descending.
newIntentSortBy ::
  -- | 'attribute'
  IntentSortAttribute ->
  -- | 'order'
  SortOrder ->
  IntentSortBy
newIntentSortBy pAttribute_ pOrder_ =
  IntentSortBy'
    { attribute = pAttribute_,
      order = pOrder_
    }

-- | The attribute to use to sort the list of intents.
intentSortBy_attribute :: Lens.Lens' IntentSortBy IntentSortAttribute
intentSortBy_attribute = Lens.lens (\IntentSortBy' {attribute} -> attribute) (\s@IntentSortBy' {} a -> s {attribute = a} :: IntentSortBy)

-- | The order to sort the list. You can choose ascending or descending.
intentSortBy_order :: Lens.Lens' IntentSortBy SortOrder
intentSortBy_order = Lens.lens (\IntentSortBy' {order} -> order) (\s@IntentSortBy' {} a -> s {order = a} :: IntentSortBy)

instance Prelude.Hashable IntentSortBy where
  hashWithSalt _salt IntentSortBy' {..} =
    _salt
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` order

instance Prelude.NFData IntentSortBy where
  rnf IntentSortBy' {..} =
    Prelude.rnf attribute `Prelude.seq`
      Prelude.rnf order

instance Data.ToJSON IntentSortBy where
  toJSON IntentSortBy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("attribute" Data..= attribute),
            Prelude.Just ("order" Data..= order)
          ]
      )
