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
-- Module      : Network.AWS.MarketplaceCatalog.Types.Sort
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceCatalog.Types.Sort where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceCatalog.Types.SortOrder
import qualified Network.AWS.Prelude as Prelude

-- | An object that contains two attributes, @SortBy@ and @SortOrder@.
--
-- /See:/ 'newSort' smart constructor.
data Sort = Sort'
  { -- | The sorting order. Can be @ASCENDING@ or @DESCENDING@. The default value
    -- is @DESCENDING@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | For @ListEntities@, supported attributes include @LastModifiedDate@
    -- (default), @Visibility@, @EntityId@, and @Name@.
    --
    -- For @ListChangeSets@, supported attributes include @StartTime@ and
    -- @EndTime@.
    sortBy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Sort' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'sort_sortOrder' - The sorting order. Can be @ASCENDING@ or @DESCENDING@. The default value
-- is @DESCENDING@.
--
-- 'sortBy', 'sort_sortBy' - For @ListEntities@, supported attributes include @LastModifiedDate@
-- (default), @Visibility@, @EntityId@, and @Name@.
--
-- For @ListChangeSets@, supported attributes include @StartTime@ and
-- @EndTime@.
newSort ::
  Sort
newSort =
  Sort'
    { sortOrder = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | The sorting order. Can be @ASCENDING@ or @DESCENDING@. The default value
-- is @DESCENDING@.
sort_sortOrder :: Lens.Lens' Sort (Prelude.Maybe SortOrder)
sort_sortOrder = Lens.lens (\Sort' {sortOrder} -> sortOrder) (\s@Sort' {} a -> s {sortOrder = a} :: Sort)

-- | For @ListEntities@, supported attributes include @LastModifiedDate@
-- (default), @Visibility@, @EntityId@, and @Name@.
--
-- For @ListChangeSets@, supported attributes include @StartTime@ and
-- @EndTime@.
sort_sortBy :: Lens.Lens' Sort (Prelude.Maybe Prelude.Text)
sort_sortBy = Lens.lens (\Sort' {sortBy} -> sortBy) (\s@Sort' {} a -> s {sortBy = a} :: Sort)

instance Prelude.Hashable Sort

instance Prelude.NFData Sort

instance Core.ToJSON Sort where
  toJSON Sort' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("SortBy" Core..=) Prelude.<$> sortBy
          ]
      )
