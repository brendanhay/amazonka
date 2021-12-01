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
-- Module      : Amazonka.QuickSight.Types.SheetStyle
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SheetStyle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TileLayoutStyle
import Amazonka.QuickSight.Types.TileStyle

-- | The theme display options for sheets.
--
-- /See:/ 'newSheetStyle' smart constructor.
data SheetStyle = SheetStyle'
  { -- | The layout options for tiles.
    tileLayout :: Prelude.Maybe TileLayoutStyle,
    -- | The display options for tiles.
    tile :: Prelude.Maybe TileStyle
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SheetStyle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tileLayout', 'sheetStyle_tileLayout' - The layout options for tiles.
--
-- 'tile', 'sheetStyle_tile' - The display options for tiles.
newSheetStyle ::
  SheetStyle
newSheetStyle =
  SheetStyle'
    { tileLayout = Prelude.Nothing,
      tile = Prelude.Nothing
    }

-- | The layout options for tiles.
sheetStyle_tileLayout :: Lens.Lens' SheetStyle (Prelude.Maybe TileLayoutStyle)
sheetStyle_tileLayout = Lens.lens (\SheetStyle' {tileLayout} -> tileLayout) (\s@SheetStyle' {} a -> s {tileLayout = a} :: SheetStyle)

-- | The display options for tiles.
sheetStyle_tile :: Lens.Lens' SheetStyle (Prelude.Maybe TileStyle)
sheetStyle_tile = Lens.lens (\SheetStyle' {tile} -> tile) (\s@SheetStyle' {} a -> s {tile = a} :: SheetStyle)

instance Core.FromJSON SheetStyle where
  parseJSON =
    Core.withObject
      "SheetStyle"
      ( \x ->
          SheetStyle'
            Prelude.<$> (x Core..:? "TileLayout")
            Prelude.<*> (x Core..:? "Tile")
      )

instance Prelude.Hashable SheetStyle where
  hashWithSalt salt' SheetStyle' {..} =
    salt' `Prelude.hashWithSalt` tile
      `Prelude.hashWithSalt` tileLayout

instance Prelude.NFData SheetStyle where
  rnf SheetStyle' {..} =
    Prelude.rnf tileLayout
      `Prelude.seq` Prelude.rnf tile

instance Core.ToJSON SheetStyle where
  toJSON SheetStyle' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TileLayout" Core..=) Prelude.<$> tileLayout,
            ("Tile" Core..=) Prelude.<$> tile
          ]
      )
