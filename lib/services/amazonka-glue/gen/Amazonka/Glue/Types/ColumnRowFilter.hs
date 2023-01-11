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
-- Module      : Amazonka.Glue.Types.ColumnRowFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.ColumnRowFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newColumnRowFilter' smart constructor.
data ColumnRowFilter = ColumnRowFilter'
  { columnName :: Prelude.Maybe Prelude.Text,
    rowFilterExpression :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnRowFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnName', 'columnRowFilter_columnName' - Undocumented member.
--
-- 'rowFilterExpression', 'columnRowFilter_rowFilterExpression' - Undocumented member.
newColumnRowFilter ::
  ColumnRowFilter
newColumnRowFilter =
  ColumnRowFilter'
    { columnName = Prelude.Nothing,
      rowFilterExpression = Prelude.Nothing
    }

-- | Undocumented member.
columnRowFilter_columnName :: Lens.Lens' ColumnRowFilter (Prelude.Maybe Prelude.Text)
columnRowFilter_columnName = Lens.lens (\ColumnRowFilter' {columnName} -> columnName) (\s@ColumnRowFilter' {} a -> s {columnName = a} :: ColumnRowFilter)

-- | Undocumented member.
columnRowFilter_rowFilterExpression :: Lens.Lens' ColumnRowFilter (Prelude.Maybe Prelude.Text)
columnRowFilter_rowFilterExpression = Lens.lens (\ColumnRowFilter' {rowFilterExpression} -> rowFilterExpression) (\s@ColumnRowFilter' {} a -> s {rowFilterExpression = a} :: ColumnRowFilter)

instance Data.FromJSON ColumnRowFilter where
  parseJSON =
    Data.withObject
      "ColumnRowFilter"
      ( \x ->
          ColumnRowFilter'
            Prelude.<$> (x Data..:? "ColumnName")
            Prelude.<*> (x Data..:? "RowFilterExpression")
      )

instance Prelude.Hashable ColumnRowFilter where
  hashWithSalt _salt ColumnRowFilter' {..} =
    _salt `Prelude.hashWithSalt` columnName
      `Prelude.hashWithSalt` rowFilterExpression

instance Prelude.NFData ColumnRowFilter where
  rnf ColumnRowFilter' {..} =
    Prelude.rnf columnName
      `Prelude.seq` Prelude.rnf rowFilterExpression
