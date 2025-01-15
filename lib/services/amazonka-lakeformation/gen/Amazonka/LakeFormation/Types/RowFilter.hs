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
-- Module      : Amazonka.LakeFormation.Types.RowFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.RowFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types.AllRowsWildcard
import qualified Amazonka.Prelude as Prelude

-- | A PartiQL predicate.
--
-- /See:/ 'newRowFilter' smart constructor.
data RowFilter = RowFilter'
  { -- | A wildcard for all rows.
    allRowsWildcard :: Prelude.Maybe AllRowsWildcard,
    -- | A filter expression.
    filterExpression :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RowFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allRowsWildcard', 'rowFilter_allRowsWildcard' - A wildcard for all rows.
--
-- 'filterExpression', 'rowFilter_filterExpression' - A filter expression.
newRowFilter ::
  RowFilter
newRowFilter =
  RowFilter'
    { allRowsWildcard = Prelude.Nothing,
      filterExpression = Prelude.Nothing
    }

-- | A wildcard for all rows.
rowFilter_allRowsWildcard :: Lens.Lens' RowFilter (Prelude.Maybe AllRowsWildcard)
rowFilter_allRowsWildcard = Lens.lens (\RowFilter' {allRowsWildcard} -> allRowsWildcard) (\s@RowFilter' {} a -> s {allRowsWildcard = a} :: RowFilter)

-- | A filter expression.
rowFilter_filterExpression :: Lens.Lens' RowFilter (Prelude.Maybe Prelude.Text)
rowFilter_filterExpression = Lens.lens (\RowFilter' {filterExpression} -> filterExpression) (\s@RowFilter' {} a -> s {filterExpression = a} :: RowFilter)

instance Data.FromJSON RowFilter where
  parseJSON =
    Data.withObject
      "RowFilter"
      ( \x ->
          RowFilter'
            Prelude.<$> (x Data..:? "AllRowsWildcard")
            Prelude.<*> (x Data..:? "FilterExpression")
      )

instance Prelude.Hashable RowFilter where
  hashWithSalt _salt RowFilter' {..} =
    _salt
      `Prelude.hashWithSalt` allRowsWildcard
      `Prelude.hashWithSalt` filterExpression

instance Prelude.NFData RowFilter where
  rnf RowFilter' {..} =
    Prelude.rnf allRowsWildcard `Prelude.seq`
      Prelude.rnf filterExpression

instance Data.ToJSON RowFilter where
  toJSON RowFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllRowsWildcard" Data..=)
              Prelude.<$> allRowsWildcard,
            ("FilterExpression" Data..=)
              Prelude.<$> filterExpression
          ]
      )
