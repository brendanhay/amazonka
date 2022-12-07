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
-- Module      : Amazonka.LexV2Models.Types.SlotTypeSortBy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SlotTypeSortBy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.SlotTypeSortAttribute
import Amazonka.LexV2Models.Types.SortOrder
import qualified Amazonka.Prelude as Prelude

-- | Specifies attributes for sorting a list of slot types.
--
-- /See:/ 'newSlotTypeSortBy' smart constructor.
data SlotTypeSortBy = SlotTypeSortBy'
  { -- | The attribute to use to sort the list of slot types.
    attribute :: SlotTypeSortAttribute,
    -- | The order to sort the list. You can say ascending or descending.
    order :: SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlotTypeSortBy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'slotTypeSortBy_attribute' - The attribute to use to sort the list of slot types.
--
-- 'order', 'slotTypeSortBy_order' - The order to sort the list. You can say ascending or descending.
newSlotTypeSortBy ::
  -- | 'attribute'
  SlotTypeSortAttribute ->
  -- | 'order'
  SortOrder ->
  SlotTypeSortBy
newSlotTypeSortBy pAttribute_ pOrder_ =
  SlotTypeSortBy'
    { attribute = pAttribute_,
      order = pOrder_
    }

-- | The attribute to use to sort the list of slot types.
slotTypeSortBy_attribute :: Lens.Lens' SlotTypeSortBy SlotTypeSortAttribute
slotTypeSortBy_attribute = Lens.lens (\SlotTypeSortBy' {attribute} -> attribute) (\s@SlotTypeSortBy' {} a -> s {attribute = a} :: SlotTypeSortBy)

-- | The order to sort the list. You can say ascending or descending.
slotTypeSortBy_order :: Lens.Lens' SlotTypeSortBy SortOrder
slotTypeSortBy_order = Lens.lens (\SlotTypeSortBy' {order} -> order) (\s@SlotTypeSortBy' {} a -> s {order = a} :: SlotTypeSortBy)

instance Prelude.Hashable SlotTypeSortBy where
  hashWithSalt _salt SlotTypeSortBy' {..} =
    _salt `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` order

instance Prelude.NFData SlotTypeSortBy where
  rnf SlotTypeSortBy' {..} =
    Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf order

instance Data.ToJSON SlotTypeSortBy where
  toJSON SlotTypeSortBy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("attribute" Data..= attribute),
            Prelude.Just ("order" Data..= order)
          ]
      )
