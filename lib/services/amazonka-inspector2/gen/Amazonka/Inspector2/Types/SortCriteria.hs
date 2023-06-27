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
-- Module      : Amazonka.Inspector2.Types.SortCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.SortCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.SortField
import Amazonka.Inspector2.Types.SortOrder
import qualified Amazonka.Prelude as Prelude

-- | Details about the criteria used to sort finding results.
--
-- /See:/ 'newSortCriteria' smart constructor.
data SortCriteria = SortCriteria'
  { -- | The finding detail field by which results are sorted.
    field :: SortField,
    -- | The order by which findings are sorted.
    sortOrder :: SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SortCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'field', 'sortCriteria_field' - The finding detail field by which results are sorted.
--
-- 'sortOrder', 'sortCriteria_sortOrder' - The order by which findings are sorted.
newSortCriteria ::
  -- | 'field'
  SortField ->
  -- | 'sortOrder'
  SortOrder ->
  SortCriteria
newSortCriteria pField_ pSortOrder_ =
  SortCriteria'
    { field = pField_,
      sortOrder = pSortOrder_
    }

-- | The finding detail field by which results are sorted.
sortCriteria_field :: Lens.Lens' SortCriteria SortField
sortCriteria_field = Lens.lens (\SortCriteria' {field} -> field) (\s@SortCriteria' {} a -> s {field = a} :: SortCriteria)

-- | The order by which findings are sorted.
sortCriteria_sortOrder :: Lens.Lens' SortCriteria SortOrder
sortCriteria_sortOrder = Lens.lens (\SortCriteria' {sortOrder} -> sortOrder) (\s@SortCriteria' {} a -> s {sortOrder = a} :: SortCriteria)

instance Prelude.Hashable SortCriteria where
  hashWithSalt _salt SortCriteria' {..} =
    _salt
      `Prelude.hashWithSalt` field
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData SortCriteria where
  rnf SortCriteria' {..} =
    Prelude.rnf field
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToJSON SortCriteria where
  toJSON SortCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("field" Data..= field),
            Prelude.Just ("sortOrder" Data..= sortOrder)
          ]
      )
