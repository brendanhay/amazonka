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
-- Module      : Network.AWS.LexV2Models.Types.ImportSortBy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.ImportSortBy where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types.ImportSortAttribute
import Network.AWS.LexV2Models.Types.SortOrder
import qualified Network.AWS.Prelude as Prelude

-- | Provides information for sorting a list of imports.
--
-- /See:/ 'newImportSortBy' smart constructor.
data ImportSortBy = ImportSortBy'
  { -- | The export field to use for sorting.
    attribute :: ImportSortAttribute,
    -- | The order to sort the list.
    order :: SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportSortBy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'importSortBy_attribute' - The export field to use for sorting.
--
-- 'order', 'importSortBy_order' - The order to sort the list.
newImportSortBy ::
  -- | 'attribute'
  ImportSortAttribute ->
  -- | 'order'
  SortOrder ->
  ImportSortBy
newImportSortBy pAttribute_ pOrder_ =
  ImportSortBy'
    { attribute = pAttribute_,
      order = pOrder_
    }

-- | The export field to use for sorting.
importSortBy_attribute :: Lens.Lens' ImportSortBy ImportSortAttribute
importSortBy_attribute = Lens.lens (\ImportSortBy' {attribute} -> attribute) (\s@ImportSortBy' {} a -> s {attribute = a} :: ImportSortBy)

-- | The order to sort the list.
importSortBy_order :: Lens.Lens' ImportSortBy SortOrder
importSortBy_order = Lens.lens (\ImportSortBy' {order} -> order) (\s@ImportSortBy' {} a -> s {order = a} :: ImportSortBy)

instance Prelude.Hashable ImportSortBy

instance Prelude.NFData ImportSortBy

instance Core.ToJSON ImportSortBy where
  toJSON ImportSortBy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("attribute" Core..= attribute),
            Prelude.Just ("order" Core..= order)
          ]
      )
