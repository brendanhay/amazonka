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
-- Module      : Amazonka.QuickSight.Types.TileLayoutStyle
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TileLayoutStyle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.GutterStyle
import Amazonka.QuickSight.Types.MarginStyle

-- | The display options for the layout of tiles on a sheet.
--
-- /See:/ 'newTileLayoutStyle' smart constructor.
data TileLayoutStyle = TileLayoutStyle'
  { -- | The margin settings that apply around the outside edge of sheets.
    margin :: Prelude.Maybe MarginStyle,
    -- | The gutter settings that apply between tiles.
    gutter :: Prelude.Maybe GutterStyle
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TileLayoutStyle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'margin', 'tileLayoutStyle_margin' - The margin settings that apply around the outside edge of sheets.
--
-- 'gutter', 'tileLayoutStyle_gutter' - The gutter settings that apply between tiles.
newTileLayoutStyle ::
  TileLayoutStyle
newTileLayoutStyle =
  TileLayoutStyle'
    { margin = Prelude.Nothing,
      gutter = Prelude.Nothing
    }

-- | The margin settings that apply around the outside edge of sheets.
tileLayoutStyle_margin :: Lens.Lens' TileLayoutStyle (Prelude.Maybe MarginStyle)
tileLayoutStyle_margin = Lens.lens (\TileLayoutStyle' {margin} -> margin) (\s@TileLayoutStyle' {} a -> s {margin = a} :: TileLayoutStyle)

-- | The gutter settings that apply between tiles.
tileLayoutStyle_gutter :: Lens.Lens' TileLayoutStyle (Prelude.Maybe GutterStyle)
tileLayoutStyle_gutter = Lens.lens (\TileLayoutStyle' {gutter} -> gutter) (\s@TileLayoutStyle' {} a -> s {gutter = a} :: TileLayoutStyle)

instance Core.FromJSON TileLayoutStyle where
  parseJSON =
    Core.withObject
      "TileLayoutStyle"
      ( \x ->
          TileLayoutStyle'
            Prelude.<$> (x Core..:? "Margin")
            Prelude.<*> (x Core..:? "Gutter")
      )

instance Prelude.Hashable TileLayoutStyle where
  hashWithSalt _salt TileLayoutStyle' {..} =
    _salt `Prelude.hashWithSalt` margin
      `Prelude.hashWithSalt` gutter

instance Prelude.NFData TileLayoutStyle where
  rnf TileLayoutStyle' {..} =
    Prelude.rnf margin `Prelude.seq` Prelude.rnf gutter

instance Core.ToJSON TileLayoutStyle where
  toJSON TileLayoutStyle' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Margin" Core..=) Prelude.<$> margin,
            ("Gutter" Core..=) Prelude.<$> gutter
          ]
      )
