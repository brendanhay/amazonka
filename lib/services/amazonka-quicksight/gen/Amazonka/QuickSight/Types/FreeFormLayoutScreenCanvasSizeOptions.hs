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
-- Module      : Amazonka.QuickSight.Types.FreeFormLayoutScreenCanvasSizeOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FreeFormLayoutScreenCanvasSizeOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The options that determine the sizing of the canvas used in a free-form
-- layout.
--
-- /See:/ 'newFreeFormLayoutScreenCanvasSizeOptions' smart constructor.
data FreeFormLayoutScreenCanvasSizeOptions = FreeFormLayoutScreenCanvasSizeOptions'
  { -- | The width that the view port will be optimized for when the layout
    -- renders.
    optimizedViewPortWidth :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FreeFormLayoutScreenCanvasSizeOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optimizedViewPortWidth', 'freeFormLayoutScreenCanvasSizeOptions_optimizedViewPortWidth' - The width that the view port will be optimized for when the layout
-- renders.
newFreeFormLayoutScreenCanvasSizeOptions ::
  -- | 'optimizedViewPortWidth'
  Prelude.Text ->
  FreeFormLayoutScreenCanvasSizeOptions
newFreeFormLayoutScreenCanvasSizeOptions
  pOptimizedViewPortWidth_ =
    FreeFormLayoutScreenCanvasSizeOptions'
      { optimizedViewPortWidth =
          pOptimizedViewPortWidth_
      }

-- | The width that the view port will be optimized for when the layout
-- renders.
freeFormLayoutScreenCanvasSizeOptions_optimizedViewPortWidth :: Lens.Lens' FreeFormLayoutScreenCanvasSizeOptions Prelude.Text
freeFormLayoutScreenCanvasSizeOptions_optimizedViewPortWidth = Lens.lens (\FreeFormLayoutScreenCanvasSizeOptions' {optimizedViewPortWidth} -> optimizedViewPortWidth) (\s@FreeFormLayoutScreenCanvasSizeOptions' {} a -> s {optimizedViewPortWidth = a} :: FreeFormLayoutScreenCanvasSizeOptions)

instance
  Data.FromJSON
    FreeFormLayoutScreenCanvasSizeOptions
  where
  parseJSON =
    Data.withObject
      "FreeFormLayoutScreenCanvasSizeOptions"
      ( \x ->
          FreeFormLayoutScreenCanvasSizeOptions'
            Prelude.<$> (x Data..: "OptimizedViewPortWidth")
      )

instance
  Prelude.Hashable
    FreeFormLayoutScreenCanvasSizeOptions
  where
  hashWithSalt
    _salt
    FreeFormLayoutScreenCanvasSizeOptions' {..} =
      _salt `Prelude.hashWithSalt` optimizedViewPortWidth

instance
  Prelude.NFData
    FreeFormLayoutScreenCanvasSizeOptions
  where
  rnf FreeFormLayoutScreenCanvasSizeOptions' {..} =
    Prelude.rnf optimizedViewPortWidth

instance
  Data.ToJSON
    FreeFormLayoutScreenCanvasSizeOptions
  where
  toJSON FreeFormLayoutScreenCanvasSizeOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "OptimizedViewPortWidth"
                  Data..= optimizedViewPortWidth
              )
          ]
      )
