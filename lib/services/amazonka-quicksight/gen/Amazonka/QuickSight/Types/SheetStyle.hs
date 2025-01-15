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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SheetStyle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TileLayoutStyle
import Amazonka.QuickSight.Types.TileStyle

-- | The theme display options for sheets.
--
-- /See:/ 'newSheetStyle' smart constructor.
data SheetStyle = SheetStyle'
  { -- | The display options for tiles.
    tile :: Prelude.Maybe TileStyle,
    -- | The layout options for tiles.
    tileLayout :: Prelude.Maybe TileLayoutStyle
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
-- 'tile', 'sheetStyle_tile' - The display options for tiles.
--
-- 'tileLayout', 'sheetStyle_tileLayout' - The layout options for tiles.
newSheetStyle ::
  SheetStyle
newSheetStyle =
  SheetStyle'
    { tile = Prelude.Nothing,
      tileLayout = Prelude.Nothing
    }

-- | The display options for tiles.
sheetStyle_tile :: Lens.Lens' SheetStyle (Prelude.Maybe TileStyle)
sheetStyle_tile = Lens.lens (\SheetStyle' {tile} -> tile) (\s@SheetStyle' {} a -> s {tile = a} :: SheetStyle)

-- | The layout options for tiles.
sheetStyle_tileLayout :: Lens.Lens' SheetStyle (Prelude.Maybe TileLayoutStyle)
sheetStyle_tileLayout = Lens.lens (\SheetStyle' {tileLayout} -> tileLayout) (\s@SheetStyle' {} a -> s {tileLayout = a} :: SheetStyle)

instance Data.FromJSON SheetStyle where
  parseJSON =
    Data.withObject
      "SheetStyle"
      ( \x ->
          SheetStyle'
            Prelude.<$> (x Data..:? "Tile")
            Prelude.<*> (x Data..:? "TileLayout")
      )

instance Prelude.Hashable SheetStyle where
  hashWithSalt _salt SheetStyle' {..} =
    _salt
      `Prelude.hashWithSalt` tile
      `Prelude.hashWithSalt` tileLayout

instance Prelude.NFData SheetStyle where
  rnf SheetStyle' {..} =
    Prelude.rnf tile `Prelude.seq`
      Prelude.rnf tileLayout

instance Data.ToJSON SheetStyle where
  toJSON SheetStyle' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tile" Data..=) Prelude.<$> tile,
            ("TileLayout" Data..=) Prelude.<$> tileLayout
          ]
      )
