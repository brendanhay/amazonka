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
-- Module      : Amazonka.MarketplaceCatalog.Types.Sort
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MarketplaceCatalog.Types.Sort where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MarketplaceCatalog.Types.SortOrder
import qualified Amazonka.Prelude as Prelude

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

instance Prelude.Hashable Sort where
  hashWithSalt _salt Sort' {..} =
    _salt `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` sortBy

instance Prelude.NFData Sort where
  rnf Sort' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf sortBy

instance Data.ToJSON Sort where
  toJSON Sort' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("SortBy" Data..=) Prelude.<$> sortBy
          ]
      )
