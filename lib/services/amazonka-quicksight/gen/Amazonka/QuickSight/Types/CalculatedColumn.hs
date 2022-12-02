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
-- Module      : Amazonka.QuickSight.Types.CalculatedColumn
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CalculatedColumn where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A calculated column for a dataset.
--
-- /See:/ 'newCalculatedColumn' smart constructor.
data CalculatedColumn = CalculatedColumn'
  { -- | Column name.
    columnName :: Prelude.Text,
    -- | A unique ID to identify a calculated column. During a dataset update, if
    -- the column ID of a calculated column matches that of an existing
    -- calculated column, Amazon QuickSight preserves the existing calculated
    -- column.
    columnId :: Prelude.Text,
    -- | An expression that defines the calculated column.
    expression :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CalculatedColumn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnName', 'calculatedColumn_columnName' - Column name.
--
-- 'columnId', 'calculatedColumn_columnId' - A unique ID to identify a calculated column. During a dataset update, if
-- the column ID of a calculated column matches that of an existing
-- calculated column, Amazon QuickSight preserves the existing calculated
-- column.
--
-- 'expression', 'calculatedColumn_expression' - An expression that defines the calculated column.
newCalculatedColumn ::
  -- | 'columnName'
  Prelude.Text ->
  -- | 'columnId'
  Prelude.Text ->
  -- | 'expression'
  Prelude.Text ->
  CalculatedColumn
newCalculatedColumn
  pColumnName_
  pColumnId_
  pExpression_ =
    CalculatedColumn'
      { columnName = pColumnName_,
        columnId = pColumnId_,
        expression = pExpression_
      }

-- | Column name.
calculatedColumn_columnName :: Lens.Lens' CalculatedColumn Prelude.Text
calculatedColumn_columnName = Lens.lens (\CalculatedColumn' {columnName} -> columnName) (\s@CalculatedColumn' {} a -> s {columnName = a} :: CalculatedColumn)

-- | A unique ID to identify a calculated column. During a dataset update, if
-- the column ID of a calculated column matches that of an existing
-- calculated column, Amazon QuickSight preserves the existing calculated
-- column.
calculatedColumn_columnId :: Lens.Lens' CalculatedColumn Prelude.Text
calculatedColumn_columnId = Lens.lens (\CalculatedColumn' {columnId} -> columnId) (\s@CalculatedColumn' {} a -> s {columnId = a} :: CalculatedColumn)

-- | An expression that defines the calculated column.
calculatedColumn_expression :: Lens.Lens' CalculatedColumn Prelude.Text
calculatedColumn_expression = Lens.lens (\CalculatedColumn' {expression} -> expression) (\s@CalculatedColumn' {} a -> s {expression = a} :: CalculatedColumn)

instance Data.FromJSON CalculatedColumn where
  parseJSON =
    Data.withObject
      "CalculatedColumn"
      ( \x ->
          CalculatedColumn'
            Prelude.<$> (x Data..: "ColumnName")
            Prelude.<*> (x Data..: "ColumnId")
            Prelude.<*> (x Data..: "Expression")
      )

instance Prelude.Hashable CalculatedColumn where
  hashWithSalt _salt CalculatedColumn' {..} =
    _salt `Prelude.hashWithSalt` columnName
      `Prelude.hashWithSalt` columnId
      `Prelude.hashWithSalt` expression

instance Prelude.NFData CalculatedColumn where
  rnf CalculatedColumn' {..} =
    Prelude.rnf columnName
      `Prelude.seq` Prelude.rnf columnId
      `Prelude.seq` Prelude.rnf expression

instance Data.ToJSON CalculatedColumn where
  toJSON CalculatedColumn' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ColumnName" Data..= columnName),
            Prelude.Just ("ColumnId" Data..= columnId),
            Prelude.Just ("Expression" Data..= expression)
          ]
      )
