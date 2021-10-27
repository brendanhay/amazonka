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
-- Module      : Network.AWS.LexV2Models.Types.BotSortBy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.BotSortBy where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types.BotSortAttribute
import Network.AWS.LexV2Models.Types.SortOrder
import qualified Network.AWS.Prelude as Prelude

-- | Specifies attributes for sorting a list of bots.
--
-- /See:/ 'newBotSortBy' smart constructor.
data BotSortBy = BotSortBy'
  { -- | The attribute to use to sort the list of bots.
    attribute :: BotSortAttribute,
    -- | The order to sort the list. You can choose ascending or descending.
    order :: SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotSortBy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'botSortBy_attribute' - The attribute to use to sort the list of bots.
--
-- 'order', 'botSortBy_order' - The order to sort the list. You can choose ascending or descending.
newBotSortBy ::
  -- | 'attribute'
  BotSortAttribute ->
  -- | 'order'
  SortOrder ->
  BotSortBy
newBotSortBy pAttribute_ pOrder_ =
  BotSortBy'
    { attribute = pAttribute_,
      order = pOrder_
    }

-- | The attribute to use to sort the list of bots.
botSortBy_attribute :: Lens.Lens' BotSortBy BotSortAttribute
botSortBy_attribute = Lens.lens (\BotSortBy' {attribute} -> attribute) (\s@BotSortBy' {} a -> s {attribute = a} :: BotSortBy)

-- | The order to sort the list. You can choose ascending or descending.
botSortBy_order :: Lens.Lens' BotSortBy SortOrder
botSortBy_order = Lens.lens (\BotSortBy' {order} -> order) (\s@BotSortBy' {} a -> s {order = a} :: BotSortBy)

instance Prelude.Hashable BotSortBy

instance Prelude.NFData BotSortBy

instance Core.ToJSON BotSortBy where
  toJSON BotSortBy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("attribute" Core..= attribute),
            Prelude.Just ("order" Core..= order)
          ]
      )
