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
-- Module      : Amazonka.WorkDocs.Types.SearchSortResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.SearchSortResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.OrderByFieldType
import Amazonka.WorkDocs.Types.SortOrder

-- | The result of the sort operation.
--
-- /See:/ 'newSearchSortResult' smart constructor.
data SearchSortResult = SearchSortResult'
  { -- | Sort search results based on this field name.
    field :: Prelude.Maybe OrderByFieldType,
    -- | Sort direction.
    order :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchSortResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'field', 'searchSortResult_field' - Sort search results based on this field name.
--
-- 'order', 'searchSortResult_order' - Sort direction.
newSearchSortResult ::
  SearchSortResult
newSearchSortResult =
  SearchSortResult'
    { field = Prelude.Nothing,
      order = Prelude.Nothing
    }

-- | Sort search results based on this field name.
searchSortResult_field :: Lens.Lens' SearchSortResult (Prelude.Maybe OrderByFieldType)
searchSortResult_field = Lens.lens (\SearchSortResult' {field} -> field) (\s@SearchSortResult' {} a -> s {field = a} :: SearchSortResult)

-- | Sort direction.
searchSortResult_order :: Lens.Lens' SearchSortResult (Prelude.Maybe SortOrder)
searchSortResult_order = Lens.lens (\SearchSortResult' {order} -> order) (\s@SearchSortResult' {} a -> s {order = a} :: SearchSortResult)

instance Prelude.Hashable SearchSortResult where
  hashWithSalt _salt SearchSortResult' {..} =
    _salt
      `Prelude.hashWithSalt` field
      `Prelude.hashWithSalt` order

instance Prelude.NFData SearchSortResult where
  rnf SearchSortResult' {..} =
    Prelude.rnf field `Prelude.seq` Prelude.rnf order

instance Data.ToJSON SearchSortResult where
  toJSON SearchSortResult' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Field" Data..=) Prelude.<$> field,
            ("Order" Data..=) Prelude.<$> order
          ]
      )
