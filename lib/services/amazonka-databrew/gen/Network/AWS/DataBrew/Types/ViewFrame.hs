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
-- Module      : Network.AWS.DataBrew.Types.ViewFrame
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataBrew.Types.ViewFrame where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the data being transformed during an action.
--
-- /See:/ 'newViewFrame' smart constructor.
data ViewFrame = ViewFrame'
  { -- | A list of columns to hide in the view frame.
    hiddenColumns :: Prelude.Maybe [Prelude.Text],
    -- | The number of columns to include in the view frame, beginning with the
    -- @StartColumnIndex@ value and ignoring any columns in the @HiddenColumns@
    -- list.
    columnRange :: Prelude.Maybe Prelude.Natural,
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
-- 'hiddenColumns', 'viewFrame_hiddenColumns' - A list of columns to hide in the view frame.
--
-- 'columnRange', 'viewFrame_columnRange' - The number of columns to include in the view frame, beginning with the
-- @StartColumnIndex@ value and ignoring any columns in the @HiddenColumns@
-- list.
--
-- 'startColumnIndex', 'viewFrame_startColumnIndex' - The starting index for the range of columns to return in the view frame.
newViewFrame ::
  -- | 'startColumnIndex'
  Prelude.Natural ->
  ViewFrame
newViewFrame pStartColumnIndex_ =
  ViewFrame'
    { hiddenColumns = Prelude.Nothing,
      columnRange = Prelude.Nothing,
      startColumnIndex = pStartColumnIndex_
    }

-- | A list of columns to hide in the view frame.
viewFrame_hiddenColumns :: Lens.Lens' ViewFrame (Prelude.Maybe [Prelude.Text])
viewFrame_hiddenColumns = Lens.lens (\ViewFrame' {hiddenColumns} -> hiddenColumns) (\s@ViewFrame' {} a -> s {hiddenColumns = a} :: ViewFrame) Prelude.. Lens.mapping Lens.coerced

-- | The number of columns to include in the view frame, beginning with the
-- @StartColumnIndex@ value and ignoring any columns in the @HiddenColumns@
-- list.
viewFrame_columnRange :: Lens.Lens' ViewFrame (Prelude.Maybe Prelude.Natural)
viewFrame_columnRange = Lens.lens (\ViewFrame' {columnRange} -> columnRange) (\s@ViewFrame' {} a -> s {columnRange = a} :: ViewFrame)

-- | The starting index for the range of columns to return in the view frame.
viewFrame_startColumnIndex :: Lens.Lens' ViewFrame Prelude.Natural
viewFrame_startColumnIndex = Lens.lens (\ViewFrame' {startColumnIndex} -> startColumnIndex) (\s@ViewFrame' {} a -> s {startColumnIndex = a} :: ViewFrame)

instance Prelude.Hashable ViewFrame

instance Prelude.NFData ViewFrame

instance Core.ToJSON ViewFrame where
  toJSON ViewFrame' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("HiddenColumns" Core..=) Prelude.<$> hiddenColumns,
            ("ColumnRange" Core..=) Prelude.<$> columnRange,
            Prelude.Just
              ("StartColumnIndex" Core..= startColumnIndex)
          ]
      )
