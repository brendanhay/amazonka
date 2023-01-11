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
-- Module      : Amazonka.QuickSight.Types.TablePaginatedReportOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TablePaginatedReportOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Visibility

-- | The paginated report options for a table visual.
--
-- /See:/ 'newTablePaginatedReportOptions' smart constructor.
data TablePaginatedReportOptions = TablePaginatedReportOptions'
  { -- | The visibility of repeating header rows on each page.
    overflowColumnHeaderVisibility :: Prelude.Maybe Visibility,
    -- | The visibility of printing table overflow across pages.
    verticalOverflowVisibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TablePaginatedReportOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'overflowColumnHeaderVisibility', 'tablePaginatedReportOptions_overflowColumnHeaderVisibility' - The visibility of repeating header rows on each page.
--
-- 'verticalOverflowVisibility', 'tablePaginatedReportOptions_verticalOverflowVisibility' - The visibility of printing table overflow across pages.
newTablePaginatedReportOptions ::
  TablePaginatedReportOptions
newTablePaginatedReportOptions =
  TablePaginatedReportOptions'
    { overflowColumnHeaderVisibility =
        Prelude.Nothing,
      verticalOverflowVisibility = Prelude.Nothing
    }

-- | The visibility of repeating header rows on each page.
tablePaginatedReportOptions_overflowColumnHeaderVisibility :: Lens.Lens' TablePaginatedReportOptions (Prelude.Maybe Visibility)
tablePaginatedReportOptions_overflowColumnHeaderVisibility = Lens.lens (\TablePaginatedReportOptions' {overflowColumnHeaderVisibility} -> overflowColumnHeaderVisibility) (\s@TablePaginatedReportOptions' {} a -> s {overflowColumnHeaderVisibility = a} :: TablePaginatedReportOptions)

-- | The visibility of printing table overflow across pages.
tablePaginatedReportOptions_verticalOverflowVisibility :: Lens.Lens' TablePaginatedReportOptions (Prelude.Maybe Visibility)
tablePaginatedReportOptions_verticalOverflowVisibility = Lens.lens (\TablePaginatedReportOptions' {verticalOverflowVisibility} -> verticalOverflowVisibility) (\s@TablePaginatedReportOptions' {} a -> s {verticalOverflowVisibility = a} :: TablePaginatedReportOptions)

instance Data.FromJSON TablePaginatedReportOptions where
  parseJSON =
    Data.withObject
      "TablePaginatedReportOptions"
      ( \x ->
          TablePaginatedReportOptions'
            Prelude.<$> (x Data..:? "OverflowColumnHeaderVisibility")
            Prelude.<*> (x Data..:? "VerticalOverflowVisibility")
      )

instance Prelude.Hashable TablePaginatedReportOptions where
  hashWithSalt _salt TablePaginatedReportOptions' {..} =
    _salt
      `Prelude.hashWithSalt` overflowColumnHeaderVisibility
      `Prelude.hashWithSalt` verticalOverflowVisibility

instance Prelude.NFData TablePaginatedReportOptions where
  rnf TablePaginatedReportOptions' {..} =
    Prelude.rnf overflowColumnHeaderVisibility
      `Prelude.seq` Prelude.rnf verticalOverflowVisibility

instance Data.ToJSON TablePaginatedReportOptions where
  toJSON TablePaginatedReportOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OverflowColumnHeaderVisibility" Data..=)
              Prelude.<$> overflowColumnHeaderVisibility,
            ("VerticalOverflowVisibility" Data..=)
              Prelude.<$> verticalOverflowVisibility
          ]
      )
