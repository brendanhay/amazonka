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
-- Module      : Amazonka.QuickSight.Types.LegendOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LegendOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.LabelOptions
import Amazonka.QuickSight.Types.LegendPosition
import Amazonka.QuickSight.Types.Visibility

-- | The options for the legend setup of a visual.
--
-- /See:/ 'newLegendOptions' smart constructor.
data LegendOptions = LegendOptions'
  { -- | The height of the legend. If this value is omitted, a default height is
    -- used when rendering.
    height :: Prelude.Maybe Prelude.Text,
    -- | The positions for the legend. Choose one of the following options:
    --
    -- -   @AUTO@
    --
    -- -   @RIGHT@
    --
    -- -   @BOTTOM@
    --
    -- -   @LEFT@
    position :: Prelude.Maybe LegendPosition,
    -- | The custom title for the legend.
    title :: Prelude.Maybe LabelOptions,
    -- | Determines whether or not the legend is visible.
    visibility :: Prelude.Maybe Visibility,
    -- | The width of the legend. If this value is omitted, a default width is
    -- used when rendering.
    width :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LegendOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'height', 'legendOptions_height' - The height of the legend. If this value is omitted, a default height is
-- used when rendering.
--
-- 'position', 'legendOptions_position' - The positions for the legend. Choose one of the following options:
--
-- -   @AUTO@
--
-- -   @RIGHT@
--
-- -   @BOTTOM@
--
-- -   @LEFT@
--
-- 'title', 'legendOptions_title' - The custom title for the legend.
--
-- 'visibility', 'legendOptions_visibility' - Determines whether or not the legend is visible.
--
-- 'width', 'legendOptions_width' - The width of the legend. If this value is omitted, a default width is
-- used when rendering.
newLegendOptions ::
  LegendOptions
newLegendOptions =
  LegendOptions'
    { height = Prelude.Nothing,
      position = Prelude.Nothing,
      title = Prelude.Nothing,
      visibility = Prelude.Nothing,
      width = Prelude.Nothing
    }

-- | The height of the legend. If this value is omitted, a default height is
-- used when rendering.
legendOptions_height :: Lens.Lens' LegendOptions (Prelude.Maybe Prelude.Text)
legendOptions_height = Lens.lens (\LegendOptions' {height} -> height) (\s@LegendOptions' {} a -> s {height = a} :: LegendOptions)

-- | The positions for the legend. Choose one of the following options:
--
-- -   @AUTO@
--
-- -   @RIGHT@
--
-- -   @BOTTOM@
--
-- -   @LEFT@
legendOptions_position :: Lens.Lens' LegendOptions (Prelude.Maybe LegendPosition)
legendOptions_position = Lens.lens (\LegendOptions' {position} -> position) (\s@LegendOptions' {} a -> s {position = a} :: LegendOptions)

-- | The custom title for the legend.
legendOptions_title :: Lens.Lens' LegendOptions (Prelude.Maybe LabelOptions)
legendOptions_title = Lens.lens (\LegendOptions' {title} -> title) (\s@LegendOptions' {} a -> s {title = a} :: LegendOptions)

-- | Determines whether or not the legend is visible.
legendOptions_visibility :: Lens.Lens' LegendOptions (Prelude.Maybe Visibility)
legendOptions_visibility = Lens.lens (\LegendOptions' {visibility} -> visibility) (\s@LegendOptions' {} a -> s {visibility = a} :: LegendOptions)

-- | The width of the legend. If this value is omitted, a default width is
-- used when rendering.
legendOptions_width :: Lens.Lens' LegendOptions (Prelude.Maybe Prelude.Text)
legendOptions_width = Lens.lens (\LegendOptions' {width} -> width) (\s@LegendOptions' {} a -> s {width = a} :: LegendOptions)

instance Data.FromJSON LegendOptions where
  parseJSON =
    Data.withObject
      "LegendOptions"
      ( \x ->
          LegendOptions'
            Prelude.<$> (x Data..:? "Height")
            Prelude.<*> (x Data..:? "Position")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..:? "Visibility")
            Prelude.<*> (x Data..:? "Width")
      )

instance Prelude.Hashable LegendOptions where
  hashWithSalt _salt LegendOptions' {..} =
    _salt `Prelude.hashWithSalt` height
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visibility
      `Prelude.hashWithSalt` width

instance Prelude.NFData LegendOptions where
  rnf LegendOptions' {..} =
    Prelude.rnf height
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf visibility
      `Prelude.seq` Prelude.rnf width

instance Data.ToJSON LegendOptions where
  toJSON LegendOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Height" Data..=) Prelude.<$> height,
            ("Position" Data..=) Prelude.<$> position,
            ("Title" Data..=) Prelude.<$> title,
            ("Visibility" Data..=) Prelude.<$> visibility,
            ("Width" Data..=) Prelude.<$> width
          ]
      )
