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
-- Module      : Amazonka.QuickSight.Types.GridLayoutScreenCanvasSizeOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GridLayoutScreenCanvasSizeOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ResizeOption

-- | The options that determine the sizing of the canvas used in a grid
-- layout.
--
-- /See:/ 'newGridLayoutScreenCanvasSizeOptions' smart constructor.
data GridLayoutScreenCanvasSizeOptions = GridLayoutScreenCanvasSizeOptions'
  { -- | The width that the view port will be optimized for when the layout
    -- renders.
    optimizedViewPortWidth :: Prelude.Maybe Prelude.Text,
    -- | This value determines the layout behavior when the viewport is resized.
    --
    -- -   @FIXED@: A fixed width will be used when optimizing the layout. In
    --     the Amazon QuickSight console, this option is called @Classic@.
    --
    -- -   @RESPONSIVE@: The width of the canvas will be responsive and
    --     optimized to the view port. In the Amazon QuickSight console, this
    --     option is called @Tiled@.
    resizeOption :: ResizeOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GridLayoutScreenCanvasSizeOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optimizedViewPortWidth', 'gridLayoutScreenCanvasSizeOptions_optimizedViewPortWidth' - The width that the view port will be optimized for when the layout
-- renders.
--
-- 'resizeOption', 'gridLayoutScreenCanvasSizeOptions_resizeOption' - This value determines the layout behavior when the viewport is resized.
--
-- -   @FIXED@: A fixed width will be used when optimizing the layout. In
--     the Amazon QuickSight console, this option is called @Classic@.
--
-- -   @RESPONSIVE@: The width of the canvas will be responsive and
--     optimized to the view port. In the Amazon QuickSight console, this
--     option is called @Tiled@.
newGridLayoutScreenCanvasSizeOptions ::
  -- | 'resizeOption'
  ResizeOption ->
  GridLayoutScreenCanvasSizeOptions
newGridLayoutScreenCanvasSizeOptions pResizeOption_ =
  GridLayoutScreenCanvasSizeOptions'
    { optimizedViewPortWidth =
        Prelude.Nothing,
      resizeOption = pResizeOption_
    }

-- | The width that the view port will be optimized for when the layout
-- renders.
gridLayoutScreenCanvasSizeOptions_optimizedViewPortWidth :: Lens.Lens' GridLayoutScreenCanvasSizeOptions (Prelude.Maybe Prelude.Text)
gridLayoutScreenCanvasSizeOptions_optimizedViewPortWidth = Lens.lens (\GridLayoutScreenCanvasSizeOptions' {optimizedViewPortWidth} -> optimizedViewPortWidth) (\s@GridLayoutScreenCanvasSizeOptions' {} a -> s {optimizedViewPortWidth = a} :: GridLayoutScreenCanvasSizeOptions)

-- | This value determines the layout behavior when the viewport is resized.
--
-- -   @FIXED@: A fixed width will be used when optimizing the layout. In
--     the Amazon QuickSight console, this option is called @Classic@.
--
-- -   @RESPONSIVE@: The width of the canvas will be responsive and
--     optimized to the view port. In the Amazon QuickSight console, this
--     option is called @Tiled@.
gridLayoutScreenCanvasSizeOptions_resizeOption :: Lens.Lens' GridLayoutScreenCanvasSizeOptions ResizeOption
gridLayoutScreenCanvasSizeOptions_resizeOption = Lens.lens (\GridLayoutScreenCanvasSizeOptions' {resizeOption} -> resizeOption) (\s@GridLayoutScreenCanvasSizeOptions' {} a -> s {resizeOption = a} :: GridLayoutScreenCanvasSizeOptions)

instance
  Data.FromJSON
    GridLayoutScreenCanvasSizeOptions
  where
  parseJSON =
    Data.withObject
      "GridLayoutScreenCanvasSizeOptions"
      ( \x ->
          GridLayoutScreenCanvasSizeOptions'
            Prelude.<$> (x Data..:? "OptimizedViewPortWidth")
            Prelude.<*> (x Data..: "ResizeOption")
      )

instance
  Prelude.Hashable
    GridLayoutScreenCanvasSizeOptions
  where
  hashWithSalt
    _salt
    GridLayoutScreenCanvasSizeOptions' {..} =
      _salt
        `Prelude.hashWithSalt` optimizedViewPortWidth
        `Prelude.hashWithSalt` resizeOption

instance
  Prelude.NFData
    GridLayoutScreenCanvasSizeOptions
  where
  rnf GridLayoutScreenCanvasSizeOptions' {..} =
    Prelude.rnf optimizedViewPortWidth `Prelude.seq`
      Prelude.rnf resizeOption

instance
  Data.ToJSON
    GridLayoutScreenCanvasSizeOptions
  where
  toJSON GridLayoutScreenCanvasSizeOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OptimizedViewPortWidth" Data..=)
              Prelude.<$> optimizedViewPortWidth,
            Prelude.Just ("ResizeOption" Data..= resizeOption)
          ]
      )
