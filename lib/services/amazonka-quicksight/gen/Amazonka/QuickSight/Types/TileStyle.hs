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
-- Module      : Amazonka.QuickSight.Types.TileStyle
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TileStyle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.BorderStyle

-- | Display options related to tiles on a sheet.
--
-- /See:/ 'newTileStyle' smart constructor.
data TileStyle = TileStyle'
  { -- | The border around a tile.
    border :: Prelude.Maybe BorderStyle
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TileStyle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'border', 'tileStyle_border' - The border around a tile.
newTileStyle ::
  TileStyle
newTileStyle = TileStyle' {border = Prelude.Nothing}

-- | The border around a tile.
tileStyle_border :: Lens.Lens' TileStyle (Prelude.Maybe BorderStyle)
tileStyle_border = Lens.lens (\TileStyle' {border} -> border) (\s@TileStyle' {} a -> s {border = a} :: TileStyle)

instance Data.FromJSON TileStyle where
  parseJSON =
    Data.withObject
      "TileStyle"
      (\x -> TileStyle' Prelude.<$> (x Data..:? "Border"))

instance Prelude.Hashable TileStyle where
  hashWithSalt _salt TileStyle' {..} =
    _salt `Prelude.hashWithSalt` border

instance Prelude.NFData TileStyle where
  rnf TileStyle' {..} = Prelude.rnf border

instance Data.ToJSON TileStyle where
  toJSON TileStyle' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Border" Data..=) Prelude.<$> border]
      )
