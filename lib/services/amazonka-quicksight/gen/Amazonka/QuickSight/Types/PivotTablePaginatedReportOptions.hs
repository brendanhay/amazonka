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
-- Module      : Amazonka.QuickSight.Types.PivotTablePaginatedReportOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PivotTablePaginatedReportOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Visibility

-- | The paginated report options for a pivot table visual.
--
-- /See:/ 'newPivotTablePaginatedReportOptions' smart constructor.
data PivotTablePaginatedReportOptions = PivotTablePaginatedReportOptions'
  { -- | The visibility of the repeating header rows on each page.
    overflowColumnHeaderVisibility :: Prelude.Maybe Visibility,
    -- | The visibility of the printing table overflow across pages.
    verticalOverflowVisibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PivotTablePaginatedReportOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'overflowColumnHeaderVisibility', 'pivotTablePaginatedReportOptions_overflowColumnHeaderVisibility' - The visibility of the repeating header rows on each page.
--
-- 'verticalOverflowVisibility', 'pivotTablePaginatedReportOptions_verticalOverflowVisibility' - The visibility of the printing table overflow across pages.
newPivotTablePaginatedReportOptions ::
  PivotTablePaginatedReportOptions
newPivotTablePaginatedReportOptions =
  PivotTablePaginatedReportOptions'
    { overflowColumnHeaderVisibility =
        Prelude.Nothing,
      verticalOverflowVisibility =
        Prelude.Nothing
    }

-- | The visibility of the repeating header rows on each page.
pivotTablePaginatedReportOptions_overflowColumnHeaderVisibility :: Lens.Lens' PivotTablePaginatedReportOptions (Prelude.Maybe Visibility)
pivotTablePaginatedReportOptions_overflowColumnHeaderVisibility = Lens.lens (\PivotTablePaginatedReportOptions' {overflowColumnHeaderVisibility} -> overflowColumnHeaderVisibility) (\s@PivotTablePaginatedReportOptions' {} a -> s {overflowColumnHeaderVisibility = a} :: PivotTablePaginatedReportOptions)

-- | The visibility of the printing table overflow across pages.
pivotTablePaginatedReportOptions_verticalOverflowVisibility :: Lens.Lens' PivotTablePaginatedReportOptions (Prelude.Maybe Visibility)
pivotTablePaginatedReportOptions_verticalOverflowVisibility = Lens.lens (\PivotTablePaginatedReportOptions' {verticalOverflowVisibility} -> verticalOverflowVisibility) (\s@PivotTablePaginatedReportOptions' {} a -> s {verticalOverflowVisibility = a} :: PivotTablePaginatedReportOptions)

instance
  Data.FromJSON
    PivotTablePaginatedReportOptions
  where
  parseJSON =
    Data.withObject
      "PivotTablePaginatedReportOptions"
      ( \x ->
          PivotTablePaginatedReportOptions'
            Prelude.<$> (x Data..:? "OverflowColumnHeaderVisibility")
            Prelude.<*> (x Data..:? "VerticalOverflowVisibility")
      )

instance
  Prelude.Hashable
    PivotTablePaginatedReportOptions
  where
  hashWithSalt
    _salt
    PivotTablePaginatedReportOptions' {..} =
      _salt
        `Prelude.hashWithSalt` overflowColumnHeaderVisibility
        `Prelude.hashWithSalt` verticalOverflowVisibility

instance
  Prelude.NFData
    PivotTablePaginatedReportOptions
  where
  rnf PivotTablePaginatedReportOptions' {..} =
    Prelude.rnf overflowColumnHeaderVisibility
      `Prelude.seq` Prelude.rnf verticalOverflowVisibility

instance Data.ToJSON PivotTablePaginatedReportOptions where
  toJSON PivotTablePaginatedReportOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OverflowColumnHeaderVisibility" Data..=)
              Prelude.<$> overflowColumnHeaderVisibility,
            ("VerticalOverflowVisibility" Data..=)
              Prelude.<$> verticalOverflowVisibility
          ]
      )
