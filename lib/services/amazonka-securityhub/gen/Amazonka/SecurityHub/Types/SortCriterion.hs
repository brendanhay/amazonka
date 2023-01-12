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
-- Module      : Amazonka.SecurityHub.Types.SortCriterion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.SortCriterion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.SortOrder

-- | A collection of finding attributes used to sort findings.
--
-- /See:/ 'newSortCriterion' smart constructor.
data SortCriterion = SortCriterion'
  { -- | The finding attribute used to sort findings.
    field :: Prelude.Maybe Prelude.Text,
    -- | The order used to sort findings.
    sortOrder :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SortCriterion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'field', 'sortCriterion_field' - The finding attribute used to sort findings.
--
-- 'sortOrder', 'sortCriterion_sortOrder' - The order used to sort findings.
newSortCriterion ::
  SortCriterion
newSortCriterion =
  SortCriterion'
    { field = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | The finding attribute used to sort findings.
sortCriterion_field :: Lens.Lens' SortCriterion (Prelude.Maybe Prelude.Text)
sortCriterion_field = Lens.lens (\SortCriterion' {field} -> field) (\s@SortCriterion' {} a -> s {field = a} :: SortCriterion)

-- | The order used to sort findings.
sortCriterion_sortOrder :: Lens.Lens' SortCriterion (Prelude.Maybe SortOrder)
sortCriterion_sortOrder = Lens.lens (\SortCriterion' {sortOrder} -> sortOrder) (\s@SortCriterion' {} a -> s {sortOrder = a} :: SortCriterion)

instance Prelude.Hashable SortCriterion where
  hashWithSalt _salt SortCriterion' {..} =
    _salt `Prelude.hashWithSalt` field
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData SortCriterion where
  rnf SortCriterion' {..} =
    Prelude.rnf field
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToJSON SortCriterion where
  toJSON SortCriterion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Field" Data..=) Prelude.<$> field,
            ("SortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )
