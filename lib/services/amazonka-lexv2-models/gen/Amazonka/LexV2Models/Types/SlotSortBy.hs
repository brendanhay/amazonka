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
-- Module      : Amazonka.LexV2Models.Types.SlotSortBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SlotSortBy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.SlotSortAttribute
import Amazonka.LexV2Models.Types.SortOrder
import qualified Amazonka.Prelude as Prelude

-- | Specifies attributes for sorting a list of bots.
--
-- /See:/ 'newSlotSortBy' smart constructor.
data SlotSortBy = SlotSortBy'
  { -- | The attribute to use to sort the list.
    attribute :: SlotSortAttribute,
    -- | The order to sort the list. You can choose ascending or descending.
    order :: SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlotSortBy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'slotSortBy_attribute' - The attribute to use to sort the list.
--
-- 'order', 'slotSortBy_order' - The order to sort the list. You can choose ascending or descending.
newSlotSortBy ::
  -- | 'attribute'
  SlotSortAttribute ->
  -- | 'order'
  SortOrder ->
  SlotSortBy
newSlotSortBy pAttribute_ pOrder_ =
  SlotSortBy'
    { attribute = pAttribute_,
      order = pOrder_
    }

-- | The attribute to use to sort the list.
slotSortBy_attribute :: Lens.Lens' SlotSortBy SlotSortAttribute
slotSortBy_attribute = Lens.lens (\SlotSortBy' {attribute} -> attribute) (\s@SlotSortBy' {} a -> s {attribute = a} :: SlotSortBy)

-- | The order to sort the list. You can choose ascending or descending.
slotSortBy_order :: Lens.Lens' SlotSortBy SortOrder
slotSortBy_order = Lens.lens (\SlotSortBy' {order} -> order) (\s@SlotSortBy' {} a -> s {order = a} :: SlotSortBy)

instance Prelude.Hashable SlotSortBy where
  hashWithSalt _salt SlotSortBy' {..} =
    _salt
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` order

instance Prelude.NFData SlotSortBy where
  rnf SlotSortBy' {..} =
    Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf order

instance Data.ToJSON SlotSortBy where
  toJSON SlotSortBy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("attribute" Data..= attribute),
            Prelude.Just ("order" Data..= order)
          ]
      )
