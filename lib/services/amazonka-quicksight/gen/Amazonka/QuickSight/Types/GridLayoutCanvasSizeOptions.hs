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
-- Module      : Amazonka.QuickSight.Types.GridLayoutCanvasSizeOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GridLayoutCanvasSizeOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.GridLayoutScreenCanvasSizeOptions

-- | Configuration options for the canvas of a grid layout.
--
-- /See:/ 'newGridLayoutCanvasSizeOptions' smart constructor.
data GridLayoutCanvasSizeOptions = GridLayoutCanvasSizeOptions'
  { -- | The options that determine the sizing of the canvas used in a grid
    -- layout.
    screenCanvasSizeOptions :: Prelude.Maybe GridLayoutScreenCanvasSizeOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GridLayoutCanvasSizeOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'screenCanvasSizeOptions', 'gridLayoutCanvasSizeOptions_screenCanvasSizeOptions' - The options that determine the sizing of the canvas used in a grid
-- layout.
newGridLayoutCanvasSizeOptions ::
  GridLayoutCanvasSizeOptions
newGridLayoutCanvasSizeOptions =
  GridLayoutCanvasSizeOptions'
    { screenCanvasSizeOptions =
        Prelude.Nothing
    }

-- | The options that determine the sizing of the canvas used in a grid
-- layout.
gridLayoutCanvasSizeOptions_screenCanvasSizeOptions :: Lens.Lens' GridLayoutCanvasSizeOptions (Prelude.Maybe GridLayoutScreenCanvasSizeOptions)
gridLayoutCanvasSizeOptions_screenCanvasSizeOptions = Lens.lens (\GridLayoutCanvasSizeOptions' {screenCanvasSizeOptions} -> screenCanvasSizeOptions) (\s@GridLayoutCanvasSizeOptions' {} a -> s {screenCanvasSizeOptions = a} :: GridLayoutCanvasSizeOptions)

instance Data.FromJSON GridLayoutCanvasSizeOptions where
  parseJSON =
    Data.withObject
      "GridLayoutCanvasSizeOptions"
      ( \x ->
          GridLayoutCanvasSizeOptions'
            Prelude.<$> (x Data..:? "ScreenCanvasSizeOptions")
      )

instance Prelude.Hashable GridLayoutCanvasSizeOptions where
  hashWithSalt _salt GridLayoutCanvasSizeOptions' {..} =
    _salt
      `Prelude.hashWithSalt` screenCanvasSizeOptions

instance Prelude.NFData GridLayoutCanvasSizeOptions where
  rnf GridLayoutCanvasSizeOptions' {..} =
    Prelude.rnf screenCanvasSizeOptions

instance Data.ToJSON GridLayoutCanvasSizeOptions where
  toJSON GridLayoutCanvasSizeOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ScreenCanvasSizeOptions" Data..=)
              Prelude.<$> screenCanvasSizeOptions
          ]
      )
