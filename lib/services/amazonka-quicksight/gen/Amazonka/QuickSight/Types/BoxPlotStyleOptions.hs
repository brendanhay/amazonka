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
-- Module      : Amazonka.QuickSight.Types.BoxPlotStyleOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.BoxPlotStyleOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.BoxPlotFillStyle

-- | The style options of the box plot.
--
-- /See:/ 'newBoxPlotStyleOptions' smart constructor.
data BoxPlotStyleOptions = BoxPlotStyleOptions'
  { -- | The fill styles (solid, transparent) of the box plot.
    fillStyle :: Prelude.Maybe BoxPlotFillStyle
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BoxPlotStyleOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fillStyle', 'boxPlotStyleOptions_fillStyle' - The fill styles (solid, transparent) of the box plot.
newBoxPlotStyleOptions ::
  BoxPlotStyleOptions
newBoxPlotStyleOptions =
  BoxPlotStyleOptions' {fillStyle = Prelude.Nothing}

-- | The fill styles (solid, transparent) of the box plot.
boxPlotStyleOptions_fillStyle :: Lens.Lens' BoxPlotStyleOptions (Prelude.Maybe BoxPlotFillStyle)
boxPlotStyleOptions_fillStyle = Lens.lens (\BoxPlotStyleOptions' {fillStyle} -> fillStyle) (\s@BoxPlotStyleOptions' {} a -> s {fillStyle = a} :: BoxPlotStyleOptions)

instance Data.FromJSON BoxPlotStyleOptions where
  parseJSON =
    Data.withObject
      "BoxPlotStyleOptions"
      ( \x ->
          BoxPlotStyleOptions'
            Prelude.<$> (x Data..:? "FillStyle")
      )

instance Prelude.Hashable BoxPlotStyleOptions where
  hashWithSalt _salt BoxPlotStyleOptions' {..} =
    _salt `Prelude.hashWithSalt` fillStyle

instance Prelude.NFData BoxPlotStyleOptions where
  rnf BoxPlotStyleOptions' {..} = Prelude.rnf fillStyle

instance Data.ToJSON BoxPlotStyleOptions where
  toJSON BoxPlotStyleOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [("FillStyle" Data..=) Prelude.<$> fillStyle]
      )
