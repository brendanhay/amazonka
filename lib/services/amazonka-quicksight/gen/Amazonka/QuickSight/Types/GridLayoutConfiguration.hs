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
-- Module      : Amazonka.QuickSight.Types.GridLayoutConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GridLayoutConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.GridLayoutCanvasSizeOptions
import Amazonka.QuickSight.Types.GridLayoutElement

-- | The configuration for a grid layout. Also called a tiled layout.
--
-- Visuals snap to a grid with standard spacing and alignment. Dashboards
-- are displayed as designed, with options to fit to screen or view at
-- actual size.
--
-- /See:/ 'newGridLayoutConfiguration' smart constructor.
data GridLayoutConfiguration = GridLayoutConfiguration'
  { canvasSizeOptions :: Prelude.Maybe GridLayoutCanvasSizeOptions,
    -- | The elements that are included in a grid layout.
    elements :: [GridLayoutElement]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GridLayoutConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'canvasSizeOptions', 'gridLayoutConfiguration_canvasSizeOptions' - Undocumented member.
--
-- 'elements', 'gridLayoutConfiguration_elements' - The elements that are included in a grid layout.
newGridLayoutConfiguration ::
  GridLayoutConfiguration
newGridLayoutConfiguration =
  GridLayoutConfiguration'
    { canvasSizeOptions =
        Prelude.Nothing,
      elements = Prelude.mempty
    }

-- | Undocumented member.
gridLayoutConfiguration_canvasSizeOptions :: Lens.Lens' GridLayoutConfiguration (Prelude.Maybe GridLayoutCanvasSizeOptions)
gridLayoutConfiguration_canvasSizeOptions = Lens.lens (\GridLayoutConfiguration' {canvasSizeOptions} -> canvasSizeOptions) (\s@GridLayoutConfiguration' {} a -> s {canvasSizeOptions = a} :: GridLayoutConfiguration)

-- | The elements that are included in a grid layout.
gridLayoutConfiguration_elements :: Lens.Lens' GridLayoutConfiguration [GridLayoutElement]
gridLayoutConfiguration_elements = Lens.lens (\GridLayoutConfiguration' {elements} -> elements) (\s@GridLayoutConfiguration' {} a -> s {elements = a} :: GridLayoutConfiguration) Prelude.. Lens.coerced

instance Data.FromJSON GridLayoutConfiguration where
  parseJSON =
    Data.withObject
      "GridLayoutConfiguration"
      ( \x ->
          GridLayoutConfiguration'
            Prelude.<$> (x Data..:? "CanvasSizeOptions")
            Prelude.<*> (x Data..:? "Elements" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable GridLayoutConfiguration where
  hashWithSalt _salt GridLayoutConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` canvasSizeOptions
      `Prelude.hashWithSalt` elements

instance Prelude.NFData GridLayoutConfiguration where
  rnf GridLayoutConfiguration' {..} =
    Prelude.rnf canvasSizeOptions `Prelude.seq`
      Prelude.rnf elements

instance Data.ToJSON GridLayoutConfiguration where
  toJSON GridLayoutConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CanvasSizeOptions" Data..=)
              Prelude.<$> canvasSizeOptions,
            Prelude.Just ("Elements" Data..= elements)
          ]
      )
