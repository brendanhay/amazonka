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
-- Module      : Network.AWS.LexV2Models.Types.BotLocaleSortBy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.BotLocaleSortBy where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types.BotLocaleSortAttribute
import Network.AWS.LexV2Models.Types.SortOrder
import qualified Network.AWS.Prelude as Prelude

-- | Specifies attributes for sorting a list of bot locales.
--
-- /See:/ 'newBotLocaleSortBy' smart constructor.
data BotLocaleSortBy = BotLocaleSortBy'
  { -- | The bot locale attribute to sort by.
    attribute :: BotLocaleSortAttribute,
    -- | Specifies whether to sort the bot locales in ascending or descending
    -- order.
    order :: SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotLocaleSortBy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'botLocaleSortBy_attribute' - The bot locale attribute to sort by.
--
-- 'order', 'botLocaleSortBy_order' - Specifies whether to sort the bot locales in ascending or descending
-- order.
newBotLocaleSortBy ::
  -- | 'attribute'
  BotLocaleSortAttribute ->
  -- | 'order'
  SortOrder ->
  BotLocaleSortBy
newBotLocaleSortBy pAttribute_ pOrder_ =
  BotLocaleSortBy'
    { attribute = pAttribute_,
      order = pOrder_
    }

-- | The bot locale attribute to sort by.
botLocaleSortBy_attribute :: Lens.Lens' BotLocaleSortBy BotLocaleSortAttribute
botLocaleSortBy_attribute = Lens.lens (\BotLocaleSortBy' {attribute} -> attribute) (\s@BotLocaleSortBy' {} a -> s {attribute = a} :: BotLocaleSortBy)

-- | Specifies whether to sort the bot locales in ascending or descending
-- order.
botLocaleSortBy_order :: Lens.Lens' BotLocaleSortBy SortOrder
botLocaleSortBy_order = Lens.lens (\BotLocaleSortBy' {order} -> order) (\s@BotLocaleSortBy' {} a -> s {order = a} :: BotLocaleSortBy)

instance Prelude.Hashable BotLocaleSortBy

instance Prelude.NFData BotLocaleSortBy

instance Core.ToJSON BotLocaleSortBy where
  toJSON BotLocaleSortBy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("attribute" Core..= attribute),
            Prelude.Just ("order" Core..= order)
          ]
      )
