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
-- Module      : Amazonka.QuickSight.Types.RowInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RowInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about rows for a data set SPICE ingestion.
--
-- /See:/ 'newRowInfo' smart constructor.
data RowInfo = RowInfo'
  { -- | The number of rows that were not ingested.
    rowsDropped :: Prelude.Maybe Prelude.Integer,
    -- | The number of rows that were ingested.
    rowsIngested :: Prelude.Maybe Prelude.Integer,
    -- | The total number of rows in the dataset.
    totalRowsInDataset :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RowInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rowsDropped', 'rowInfo_rowsDropped' - The number of rows that were not ingested.
--
-- 'rowsIngested', 'rowInfo_rowsIngested' - The number of rows that were ingested.
--
-- 'totalRowsInDataset', 'rowInfo_totalRowsInDataset' - The total number of rows in the dataset.
newRowInfo ::
  RowInfo
newRowInfo =
  RowInfo'
    { rowsDropped = Prelude.Nothing,
      rowsIngested = Prelude.Nothing,
      totalRowsInDataset = Prelude.Nothing
    }

-- | The number of rows that were not ingested.
rowInfo_rowsDropped :: Lens.Lens' RowInfo (Prelude.Maybe Prelude.Integer)
rowInfo_rowsDropped = Lens.lens (\RowInfo' {rowsDropped} -> rowsDropped) (\s@RowInfo' {} a -> s {rowsDropped = a} :: RowInfo)

-- | The number of rows that were ingested.
rowInfo_rowsIngested :: Lens.Lens' RowInfo (Prelude.Maybe Prelude.Integer)
rowInfo_rowsIngested = Lens.lens (\RowInfo' {rowsIngested} -> rowsIngested) (\s@RowInfo' {} a -> s {rowsIngested = a} :: RowInfo)

-- | The total number of rows in the dataset.
rowInfo_totalRowsInDataset :: Lens.Lens' RowInfo (Prelude.Maybe Prelude.Integer)
rowInfo_totalRowsInDataset = Lens.lens (\RowInfo' {totalRowsInDataset} -> totalRowsInDataset) (\s@RowInfo' {} a -> s {totalRowsInDataset = a} :: RowInfo)

instance Data.FromJSON RowInfo where
  parseJSON =
    Data.withObject
      "RowInfo"
      ( \x ->
          RowInfo'
            Prelude.<$> (x Data..:? "RowsDropped")
            Prelude.<*> (x Data..:? "RowsIngested")
            Prelude.<*> (x Data..:? "TotalRowsInDataset")
      )

instance Prelude.Hashable RowInfo where
  hashWithSalt _salt RowInfo' {..} =
    _salt `Prelude.hashWithSalt` rowsDropped
      `Prelude.hashWithSalt` rowsIngested
      `Prelude.hashWithSalt` totalRowsInDataset

instance Prelude.NFData RowInfo where
  rnf RowInfo' {..} =
    Prelude.rnf rowsDropped
      `Prelude.seq` Prelude.rnf rowsIngested
      `Prelude.seq` Prelude.rnf totalRowsInDataset
