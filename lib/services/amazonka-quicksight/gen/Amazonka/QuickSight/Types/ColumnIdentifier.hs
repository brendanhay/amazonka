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
-- Module      : Amazonka.QuickSight.Types.ColumnIdentifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ColumnIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A column of a data set.
--
-- /See:/ 'newColumnIdentifier' smart constructor.
data ColumnIdentifier = ColumnIdentifier'
  { -- | The data set that the column belongs to.
    dataSetIdentifier :: Prelude.Text,
    -- | The name of the column.
    columnName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetIdentifier', 'columnIdentifier_dataSetIdentifier' - The data set that the column belongs to.
--
-- 'columnName', 'columnIdentifier_columnName' - The name of the column.
newColumnIdentifier ::
  -- | 'dataSetIdentifier'
  Prelude.Text ->
  -- | 'columnName'
  Prelude.Text ->
  ColumnIdentifier
newColumnIdentifier pDataSetIdentifier_ pColumnName_ =
  ColumnIdentifier'
    { dataSetIdentifier =
        pDataSetIdentifier_,
      columnName = pColumnName_
    }

-- | The data set that the column belongs to.
columnIdentifier_dataSetIdentifier :: Lens.Lens' ColumnIdentifier Prelude.Text
columnIdentifier_dataSetIdentifier = Lens.lens (\ColumnIdentifier' {dataSetIdentifier} -> dataSetIdentifier) (\s@ColumnIdentifier' {} a -> s {dataSetIdentifier = a} :: ColumnIdentifier)

-- | The name of the column.
columnIdentifier_columnName :: Lens.Lens' ColumnIdentifier Prelude.Text
columnIdentifier_columnName = Lens.lens (\ColumnIdentifier' {columnName} -> columnName) (\s@ColumnIdentifier' {} a -> s {columnName = a} :: ColumnIdentifier)

instance Data.FromJSON ColumnIdentifier where
  parseJSON =
    Data.withObject
      "ColumnIdentifier"
      ( \x ->
          ColumnIdentifier'
            Prelude.<$> (x Data..: "DataSetIdentifier")
            Prelude.<*> (x Data..: "ColumnName")
      )

instance Prelude.Hashable ColumnIdentifier where
  hashWithSalt _salt ColumnIdentifier' {..} =
    _salt `Prelude.hashWithSalt` dataSetIdentifier
      `Prelude.hashWithSalt` columnName

instance Prelude.NFData ColumnIdentifier where
  rnf ColumnIdentifier' {..} =
    Prelude.rnf dataSetIdentifier
      `Prelude.seq` Prelude.rnf columnName

instance Data.ToJSON ColumnIdentifier where
  toJSON ColumnIdentifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DataSetIdentifier" Data..= dataSetIdentifier),
            Prelude.Just ("ColumnName" Data..= columnName)
          ]
      )
