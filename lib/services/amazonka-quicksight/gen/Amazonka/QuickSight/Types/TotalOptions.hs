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
-- Module      : Amazonka.QuickSight.Types.TotalOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TotalOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TableCellStyle
import Amazonka.QuickSight.Types.TableTotalsPlacement
import Amazonka.QuickSight.Types.TableTotalsScrollStatus
import Amazonka.QuickSight.Types.Visibility

-- | The total options for a table visual.
--
-- /See:/ 'newTotalOptions' smart constructor.
data TotalOptions = TotalOptions'
  { -- | The custom label string for the total cells.
    customLabel :: Prelude.Maybe Prelude.Text,
    -- | The placement (start, end) for the total cells.
    placement :: Prelude.Maybe TableTotalsPlacement,
    -- | The scroll status (pinned, scrolled) for the total cells.
    scrollStatus :: Prelude.Maybe TableTotalsScrollStatus,
    -- | Cell styling options for the total cells.
    totalCellStyle :: Prelude.Maybe TableCellStyle,
    -- | The visibility configuration for the total cells.
    totalsVisibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TotalOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customLabel', 'totalOptions_customLabel' - The custom label string for the total cells.
--
-- 'placement', 'totalOptions_placement' - The placement (start, end) for the total cells.
--
-- 'scrollStatus', 'totalOptions_scrollStatus' - The scroll status (pinned, scrolled) for the total cells.
--
-- 'totalCellStyle', 'totalOptions_totalCellStyle' - Cell styling options for the total cells.
--
-- 'totalsVisibility', 'totalOptions_totalsVisibility' - The visibility configuration for the total cells.
newTotalOptions ::
  TotalOptions
newTotalOptions =
  TotalOptions'
    { customLabel = Prelude.Nothing,
      placement = Prelude.Nothing,
      scrollStatus = Prelude.Nothing,
      totalCellStyle = Prelude.Nothing,
      totalsVisibility = Prelude.Nothing
    }

-- | The custom label string for the total cells.
totalOptions_customLabel :: Lens.Lens' TotalOptions (Prelude.Maybe Prelude.Text)
totalOptions_customLabel = Lens.lens (\TotalOptions' {customLabel} -> customLabel) (\s@TotalOptions' {} a -> s {customLabel = a} :: TotalOptions)

-- | The placement (start, end) for the total cells.
totalOptions_placement :: Lens.Lens' TotalOptions (Prelude.Maybe TableTotalsPlacement)
totalOptions_placement = Lens.lens (\TotalOptions' {placement} -> placement) (\s@TotalOptions' {} a -> s {placement = a} :: TotalOptions)

-- | The scroll status (pinned, scrolled) for the total cells.
totalOptions_scrollStatus :: Lens.Lens' TotalOptions (Prelude.Maybe TableTotalsScrollStatus)
totalOptions_scrollStatus = Lens.lens (\TotalOptions' {scrollStatus} -> scrollStatus) (\s@TotalOptions' {} a -> s {scrollStatus = a} :: TotalOptions)

-- | Cell styling options for the total cells.
totalOptions_totalCellStyle :: Lens.Lens' TotalOptions (Prelude.Maybe TableCellStyle)
totalOptions_totalCellStyle = Lens.lens (\TotalOptions' {totalCellStyle} -> totalCellStyle) (\s@TotalOptions' {} a -> s {totalCellStyle = a} :: TotalOptions)

-- | The visibility configuration for the total cells.
totalOptions_totalsVisibility :: Lens.Lens' TotalOptions (Prelude.Maybe Visibility)
totalOptions_totalsVisibility = Lens.lens (\TotalOptions' {totalsVisibility} -> totalsVisibility) (\s@TotalOptions' {} a -> s {totalsVisibility = a} :: TotalOptions)

instance Data.FromJSON TotalOptions where
  parseJSON =
    Data.withObject
      "TotalOptions"
      ( \x ->
          TotalOptions'
            Prelude.<$> (x Data..:? "CustomLabel")
            Prelude.<*> (x Data..:? "Placement")
            Prelude.<*> (x Data..:? "ScrollStatus")
            Prelude.<*> (x Data..:? "TotalCellStyle")
            Prelude.<*> (x Data..:? "TotalsVisibility")
      )

instance Prelude.Hashable TotalOptions where
  hashWithSalt _salt TotalOptions' {..} =
    _salt `Prelude.hashWithSalt` customLabel
      `Prelude.hashWithSalt` placement
      `Prelude.hashWithSalt` scrollStatus
      `Prelude.hashWithSalt` totalCellStyle
      `Prelude.hashWithSalt` totalsVisibility

instance Prelude.NFData TotalOptions where
  rnf TotalOptions' {..} =
    Prelude.rnf customLabel
      `Prelude.seq` Prelude.rnf placement
      `Prelude.seq` Prelude.rnf scrollStatus
      `Prelude.seq` Prelude.rnf totalCellStyle
      `Prelude.seq` Prelude.rnf totalsVisibility

instance Data.ToJSON TotalOptions where
  toJSON TotalOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomLabel" Data..=) Prelude.<$> customLabel,
            ("Placement" Data..=) Prelude.<$> placement,
            ("ScrollStatus" Data..=) Prelude.<$> scrollStatus,
            ("TotalCellStyle" Data..=)
              Prelude.<$> totalCellStyle,
            ("TotalsVisibility" Data..=)
              Prelude.<$> totalsVisibility
          ]
      )
