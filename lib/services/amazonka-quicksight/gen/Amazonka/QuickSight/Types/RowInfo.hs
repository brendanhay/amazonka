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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RowInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about rows for a data set SPICE ingestion.
--
-- /See:/ 'newRowInfo' smart constructor.
data RowInfo = RowInfo'
  { -- | The total number of rows in the dataset.
    totalRowsInDataset :: Prelude.Maybe Prelude.Integer,
    -- | The number of rows that were not ingested.
    rowsDropped :: Prelude.Maybe Prelude.Integer,
    -- | The number of rows that were ingested.
    rowsIngested :: Prelude.Maybe Prelude.Integer
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
-- 'totalRowsInDataset', 'rowInfo_totalRowsInDataset' - The total number of rows in the dataset.
--
-- 'rowsDropped', 'rowInfo_rowsDropped' - The number of rows that were not ingested.
--
-- 'rowsIngested', 'rowInfo_rowsIngested' - The number of rows that were ingested.
newRowInfo ::
  RowInfo
newRowInfo =
  RowInfo'
    { totalRowsInDataset = Prelude.Nothing,
      rowsDropped = Prelude.Nothing,
      rowsIngested = Prelude.Nothing
    }

-- | The total number of rows in the dataset.
rowInfo_totalRowsInDataset :: Lens.Lens' RowInfo (Prelude.Maybe Prelude.Integer)
rowInfo_totalRowsInDataset = Lens.lens (\RowInfo' {totalRowsInDataset} -> totalRowsInDataset) (\s@RowInfo' {} a -> s {totalRowsInDataset = a} :: RowInfo)

-- | The number of rows that were not ingested.
rowInfo_rowsDropped :: Lens.Lens' RowInfo (Prelude.Maybe Prelude.Integer)
rowInfo_rowsDropped = Lens.lens (\RowInfo' {rowsDropped} -> rowsDropped) (\s@RowInfo' {} a -> s {rowsDropped = a} :: RowInfo)

-- | The number of rows that were ingested.
rowInfo_rowsIngested :: Lens.Lens' RowInfo (Prelude.Maybe Prelude.Integer)
rowInfo_rowsIngested = Lens.lens (\RowInfo' {rowsIngested} -> rowsIngested) (\s@RowInfo' {} a -> s {rowsIngested = a} :: RowInfo)

instance Core.FromJSON RowInfo where
  parseJSON =
    Core.withObject
      "RowInfo"
      ( \x ->
          RowInfo'
            Prelude.<$> (x Core..:? "TotalRowsInDataset")
            Prelude.<*> (x Core..:? "RowsDropped")
            Prelude.<*> (x Core..:? "RowsIngested")
      )

instance Prelude.Hashable RowInfo where
  hashWithSalt _salt RowInfo' {..} =
    _salt `Prelude.hashWithSalt` totalRowsInDataset
      `Prelude.hashWithSalt` rowsDropped
      `Prelude.hashWithSalt` rowsIngested

instance Prelude.NFData RowInfo where
  rnf RowInfo' {..} =
    Prelude.rnf totalRowsInDataset
      `Prelude.seq` Prelude.rnf rowsDropped
      `Prelude.seq` Prelude.rnf rowsIngested
