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
-- Module      : Amazonka.LexV2Models.Types.BotVersionSortBy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotVersionSortBy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types.BotVersionSortAttribute
import Amazonka.LexV2Models.Types.SortOrder
import qualified Amazonka.Prelude as Prelude

-- | Specifies attributes for sorting a list of bot versions.
--
-- /See:/ 'newBotVersionSortBy' smart constructor.
data BotVersionSortBy = BotVersionSortBy'
  { -- | The attribute to use to sort the list of versions.
    attribute :: BotVersionSortAttribute,
    -- | The order to sort the list. You can specify ascending or descending
    -- order.
    order :: SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotVersionSortBy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'botVersionSortBy_attribute' - The attribute to use to sort the list of versions.
--
-- 'order', 'botVersionSortBy_order' - The order to sort the list. You can specify ascending or descending
-- order.
newBotVersionSortBy ::
  -- | 'attribute'
  BotVersionSortAttribute ->
  -- | 'order'
  SortOrder ->
  BotVersionSortBy
newBotVersionSortBy pAttribute_ pOrder_ =
  BotVersionSortBy'
    { attribute = pAttribute_,
      order = pOrder_
    }

-- | The attribute to use to sort the list of versions.
botVersionSortBy_attribute :: Lens.Lens' BotVersionSortBy BotVersionSortAttribute
botVersionSortBy_attribute = Lens.lens (\BotVersionSortBy' {attribute} -> attribute) (\s@BotVersionSortBy' {} a -> s {attribute = a} :: BotVersionSortBy)

-- | The order to sort the list. You can specify ascending or descending
-- order.
botVersionSortBy_order :: Lens.Lens' BotVersionSortBy SortOrder
botVersionSortBy_order = Lens.lens (\BotVersionSortBy' {order} -> order) (\s@BotVersionSortBy' {} a -> s {order = a} :: BotVersionSortBy)

instance Prelude.Hashable BotVersionSortBy where
  hashWithSalt _salt BotVersionSortBy' {..} =
    _salt `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` order

instance Prelude.NFData BotVersionSortBy where
  rnf BotVersionSortBy' {..} =
    Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf order

instance Core.ToJSON BotVersionSortBy where
  toJSON BotVersionSortBy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("attribute" Core..= attribute),
            Prelude.Just ("order" Core..= order)
          ]
      )
