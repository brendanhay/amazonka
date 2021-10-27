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
-- Module      : Network.AWS.LexV2Models.Types.SlotTypeSortBy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.SlotTypeSortBy where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types.SlotTypeSortAttribute
import Network.AWS.LexV2Models.Types.SortOrder
import qualified Network.AWS.Prelude as Prelude

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

instance Prelude.Hashable SlotTypeSortBy

instance Prelude.NFData SlotTypeSortBy

instance Core.ToJSON SlotTypeSortBy where
  toJSON SlotTypeSortBy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("attribute" Core..= attribute),
            Prelude.Just ("order" Core..= order)
          ]
      )
