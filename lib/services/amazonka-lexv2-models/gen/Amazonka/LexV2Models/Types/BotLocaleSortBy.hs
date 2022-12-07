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
-- Module      : Amazonka.LexV2Models.Types.BotLocaleSortBy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotLocaleSortBy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.BotLocaleSortAttribute
import Amazonka.LexV2Models.Types.SortOrder
import qualified Amazonka.Prelude as Prelude

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

instance Prelude.Hashable BotLocaleSortBy where
  hashWithSalt _salt BotLocaleSortBy' {..} =
    _salt `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` order

instance Prelude.NFData BotLocaleSortBy where
  rnf BotLocaleSortBy' {..} =
    Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf order

instance Data.ToJSON BotLocaleSortBy where
  toJSON BotLocaleSortBy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("attribute" Data..= attribute),
            Prelude.Just ("order" Data..= order)
          ]
      )
