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
-- Module      : Amazonka.DataBrew.Types.ViewFrame
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.ViewFrame where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types.AnalyticsMode
import qualified Amazonka.Prelude as Prelude

-- | Represents the data being transformed during an action.
--
-- /See:/ 'newViewFrame' smart constructor.
data ViewFrame = ViewFrame'
  { -- | Controls if analytics computation is enabled or disabled. Enabled by
    -- default.
    analytics :: Prelude.Maybe AnalyticsMode,
    -- | The starting index for the range of rows to return in the view frame.
    startRowIndex :: Prelude.Maybe Prelude.Natural,
    -- | The number of columns to include in the view frame, beginning with the
    -- @StartColumnIndex@ value and ignoring any columns in the @HiddenColumns@
    -- list.
    columnRange :: Prelude.Maybe Prelude.Natural,
    -- | The number of rows to include in the view frame, beginning with the
    -- @StartRowIndex@ value.
    rowRange :: Prelude.Maybe Prelude.Int,
    -- | A list of columns to hide in the view frame.
    hiddenColumns :: Prelude.Maybe [Prelude.Text],
    -- | The starting index for the range of columns to return in the view frame.
    startColumnIndex :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ViewFrame' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analytics', 'viewFrame_analytics' - Controls if analytics computation is enabled or disabled. Enabled by
-- default.
--
-- 'startRowIndex', 'viewFrame_startRowIndex' - The starting index for the range of rows to return in the view frame.
--
-- 'columnRange', 'viewFrame_columnRange' - The number of columns to include in the view frame, beginning with the
-- @StartColumnIndex@ value and ignoring any columns in the @HiddenColumns@
-- list.
--
-- 'rowRange', 'viewFrame_rowRange' - The number of rows to include in the view frame, beginning with the
-- @StartRowIndex@ value.
--
-- 'hiddenColumns', 'viewFrame_hiddenColumns' - A list of columns to hide in the view frame.
--
-- 'startColumnIndex', 'viewFrame_startColumnIndex' - The starting index for the range of columns to return in the view frame.
newViewFrame ::
  -- | 'startColumnIndex'
  Prelude.Natural ->
  ViewFrame
newViewFrame pStartColumnIndex_ =
  ViewFrame'
    { analytics = Prelude.Nothing,
      startRowIndex = Prelude.Nothing,
      columnRange = Prelude.Nothing,
      rowRange = Prelude.Nothing,
      hiddenColumns = Prelude.Nothing,
      startColumnIndex = pStartColumnIndex_
    }

-- | Controls if analytics computation is enabled or disabled. Enabled by
-- default.
viewFrame_analytics :: Lens.Lens' ViewFrame (Prelude.Maybe AnalyticsMode)
viewFrame_analytics = Lens.lens (\ViewFrame' {analytics} -> analytics) (\s@ViewFrame' {} a -> s {analytics = a} :: ViewFrame)

-- | The starting index for the range of rows to return in the view frame.
viewFrame_startRowIndex :: Lens.Lens' ViewFrame (Prelude.Maybe Prelude.Natural)
viewFrame_startRowIndex = Lens.lens (\ViewFrame' {startRowIndex} -> startRowIndex) (\s@ViewFrame' {} a -> s {startRowIndex = a} :: ViewFrame)

-- | The number of columns to include in the view frame, beginning with the
-- @StartColumnIndex@ value and ignoring any columns in the @HiddenColumns@
-- list.
viewFrame_columnRange :: Lens.Lens' ViewFrame (Prelude.Maybe Prelude.Natural)
viewFrame_columnRange = Lens.lens (\ViewFrame' {columnRange} -> columnRange) (\s@ViewFrame' {} a -> s {columnRange = a} :: ViewFrame)

-- | The number of rows to include in the view frame, beginning with the
-- @StartRowIndex@ value.
viewFrame_rowRange :: Lens.Lens' ViewFrame (Prelude.Maybe Prelude.Int)
viewFrame_rowRange = Lens.lens (\ViewFrame' {rowRange} -> rowRange) (\s@ViewFrame' {} a -> s {rowRange = a} :: ViewFrame)

-- | A list of columns to hide in the view frame.
viewFrame_hiddenColumns :: Lens.Lens' ViewFrame (Prelude.Maybe [Prelude.Text])
viewFrame_hiddenColumns = Lens.lens (\ViewFrame' {hiddenColumns} -> hiddenColumns) (\s@ViewFrame' {} a -> s {hiddenColumns = a} :: ViewFrame) Prelude.. Lens.mapping Lens.coerced

-- | The starting index for the range of columns to return in the view frame.
viewFrame_startColumnIndex :: Lens.Lens' ViewFrame Prelude.Natural
viewFrame_startColumnIndex = Lens.lens (\ViewFrame' {startColumnIndex} -> startColumnIndex) (\s@ViewFrame' {} a -> s {startColumnIndex = a} :: ViewFrame)

instance Prelude.Hashable ViewFrame where
  hashWithSalt _salt ViewFrame' {..} =
    _salt `Prelude.hashWithSalt` analytics
      `Prelude.hashWithSalt` startRowIndex
      `Prelude.hashWithSalt` columnRange
      `Prelude.hashWithSalt` rowRange
      `Prelude.hashWithSalt` hiddenColumns
      `Prelude.hashWithSalt` startColumnIndex

instance Prelude.NFData ViewFrame where
  rnf ViewFrame' {..} =
    Prelude.rnf analytics
      `Prelude.seq` Prelude.rnf startRowIndex
      `Prelude.seq` Prelude.rnf columnRange
      `Prelude.seq` Prelude.rnf rowRange
      `Prelude.seq` Prelude.rnf hiddenColumns
      `Prelude.seq` Prelude.rnf startColumnIndex

instance Data.ToJSON ViewFrame where
  toJSON ViewFrame' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Analytics" Data..=) Prelude.<$> analytics,
            ("StartRowIndex" Data..=) Prelude.<$> startRowIndex,
            ("ColumnRange" Data..=) Prelude.<$> columnRange,
            ("RowRange" Data..=) Prelude.<$> rowRange,
            ("HiddenColumns" Data..=) Prelude.<$> hiddenColumns,
            Prelude.Just
              ("StartColumnIndex" Data..= startColumnIndex)
          ]
      )
