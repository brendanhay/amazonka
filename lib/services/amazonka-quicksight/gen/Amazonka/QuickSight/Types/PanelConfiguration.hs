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
-- Module      : Amazonka.QuickSight.Types.PanelConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PanelConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.PanelBorderStyle
import Amazonka.QuickSight.Types.PanelTitleOptions
import Amazonka.QuickSight.Types.Visibility

-- | A collection of options that configure how each panel displays in a
-- small multiples chart.
--
-- /See:/ 'newPanelConfiguration' smart constructor.
data PanelConfiguration = PanelConfiguration'
  { -- | Sets the background color for each panel.
    backgroundColor :: Prelude.Maybe Prelude.Text,
    -- | Determines whether or not a background for each small multiples panel is
    -- rendered.
    backgroundVisibility :: Prelude.Maybe Visibility,
    -- | Sets the line color of panel borders.
    borderColor :: Prelude.Maybe Prelude.Text,
    -- | Sets the line style of panel borders.
    borderStyle :: Prelude.Maybe PanelBorderStyle,
    -- | Sets the line thickness of panel borders.
    borderThickness :: Prelude.Maybe Prelude.Text,
    -- | Determines whether or not each panel displays a border.
    borderVisibility :: Prelude.Maybe Visibility,
    -- | Sets the total amount of negative space to display between sibling
    -- panels.
    gutterSpacing :: Prelude.Maybe Prelude.Text,
    -- | Determines whether or not negative space between sibling panels is
    -- rendered.
    gutterVisibility :: Prelude.Maybe Visibility,
    -- | Configures the title display within each small multiples panel.
    title :: Prelude.Maybe PanelTitleOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PanelConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backgroundColor', 'panelConfiguration_backgroundColor' - Sets the background color for each panel.
--
-- 'backgroundVisibility', 'panelConfiguration_backgroundVisibility' - Determines whether or not a background for each small multiples panel is
-- rendered.
--
-- 'borderColor', 'panelConfiguration_borderColor' - Sets the line color of panel borders.
--
-- 'borderStyle', 'panelConfiguration_borderStyle' - Sets the line style of panel borders.
--
-- 'borderThickness', 'panelConfiguration_borderThickness' - Sets the line thickness of panel borders.
--
-- 'borderVisibility', 'panelConfiguration_borderVisibility' - Determines whether or not each panel displays a border.
--
-- 'gutterSpacing', 'panelConfiguration_gutterSpacing' - Sets the total amount of negative space to display between sibling
-- panels.
--
-- 'gutterVisibility', 'panelConfiguration_gutterVisibility' - Determines whether or not negative space between sibling panels is
-- rendered.
--
-- 'title', 'panelConfiguration_title' - Configures the title display within each small multiples panel.
newPanelConfiguration ::
  PanelConfiguration
newPanelConfiguration =
  PanelConfiguration'
    { backgroundColor =
        Prelude.Nothing,
      backgroundVisibility = Prelude.Nothing,
      borderColor = Prelude.Nothing,
      borderStyle = Prelude.Nothing,
      borderThickness = Prelude.Nothing,
      borderVisibility = Prelude.Nothing,
      gutterSpacing = Prelude.Nothing,
      gutterVisibility = Prelude.Nothing,
      title = Prelude.Nothing
    }

-- | Sets the background color for each panel.
panelConfiguration_backgroundColor :: Lens.Lens' PanelConfiguration (Prelude.Maybe Prelude.Text)
panelConfiguration_backgroundColor = Lens.lens (\PanelConfiguration' {backgroundColor} -> backgroundColor) (\s@PanelConfiguration' {} a -> s {backgroundColor = a} :: PanelConfiguration)

-- | Determines whether or not a background for each small multiples panel is
-- rendered.
panelConfiguration_backgroundVisibility :: Lens.Lens' PanelConfiguration (Prelude.Maybe Visibility)
panelConfiguration_backgroundVisibility = Lens.lens (\PanelConfiguration' {backgroundVisibility} -> backgroundVisibility) (\s@PanelConfiguration' {} a -> s {backgroundVisibility = a} :: PanelConfiguration)

-- | Sets the line color of panel borders.
panelConfiguration_borderColor :: Lens.Lens' PanelConfiguration (Prelude.Maybe Prelude.Text)
panelConfiguration_borderColor = Lens.lens (\PanelConfiguration' {borderColor} -> borderColor) (\s@PanelConfiguration' {} a -> s {borderColor = a} :: PanelConfiguration)

-- | Sets the line style of panel borders.
panelConfiguration_borderStyle :: Lens.Lens' PanelConfiguration (Prelude.Maybe PanelBorderStyle)
panelConfiguration_borderStyle = Lens.lens (\PanelConfiguration' {borderStyle} -> borderStyle) (\s@PanelConfiguration' {} a -> s {borderStyle = a} :: PanelConfiguration)

-- | Sets the line thickness of panel borders.
panelConfiguration_borderThickness :: Lens.Lens' PanelConfiguration (Prelude.Maybe Prelude.Text)
panelConfiguration_borderThickness = Lens.lens (\PanelConfiguration' {borderThickness} -> borderThickness) (\s@PanelConfiguration' {} a -> s {borderThickness = a} :: PanelConfiguration)

-- | Determines whether or not each panel displays a border.
panelConfiguration_borderVisibility :: Lens.Lens' PanelConfiguration (Prelude.Maybe Visibility)
panelConfiguration_borderVisibility = Lens.lens (\PanelConfiguration' {borderVisibility} -> borderVisibility) (\s@PanelConfiguration' {} a -> s {borderVisibility = a} :: PanelConfiguration)

-- | Sets the total amount of negative space to display between sibling
-- panels.
panelConfiguration_gutterSpacing :: Lens.Lens' PanelConfiguration (Prelude.Maybe Prelude.Text)
panelConfiguration_gutterSpacing = Lens.lens (\PanelConfiguration' {gutterSpacing} -> gutterSpacing) (\s@PanelConfiguration' {} a -> s {gutterSpacing = a} :: PanelConfiguration)

-- | Determines whether or not negative space between sibling panels is
-- rendered.
panelConfiguration_gutterVisibility :: Lens.Lens' PanelConfiguration (Prelude.Maybe Visibility)
panelConfiguration_gutterVisibility = Lens.lens (\PanelConfiguration' {gutterVisibility} -> gutterVisibility) (\s@PanelConfiguration' {} a -> s {gutterVisibility = a} :: PanelConfiguration)

-- | Configures the title display within each small multiples panel.
panelConfiguration_title :: Lens.Lens' PanelConfiguration (Prelude.Maybe PanelTitleOptions)
panelConfiguration_title = Lens.lens (\PanelConfiguration' {title} -> title) (\s@PanelConfiguration' {} a -> s {title = a} :: PanelConfiguration)

instance Data.FromJSON PanelConfiguration where
  parseJSON =
    Data.withObject
      "PanelConfiguration"
      ( \x ->
          PanelConfiguration'
            Prelude.<$> (x Data..:? "BackgroundColor")
            Prelude.<*> (x Data..:? "BackgroundVisibility")
            Prelude.<*> (x Data..:? "BorderColor")
            Prelude.<*> (x Data..:? "BorderStyle")
            Prelude.<*> (x Data..:? "BorderThickness")
            Prelude.<*> (x Data..:? "BorderVisibility")
            Prelude.<*> (x Data..:? "GutterSpacing")
            Prelude.<*> (x Data..:? "GutterVisibility")
            Prelude.<*> (x Data..:? "Title")
      )

instance Prelude.Hashable PanelConfiguration where
  hashWithSalt _salt PanelConfiguration' {..} =
    _salt `Prelude.hashWithSalt` backgroundColor
      `Prelude.hashWithSalt` backgroundVisibility
      `Prelude.hashWithSalt` borderColor
      `Prelude.hashWithSalt` borderStyle
      `Prelude.hashWithSalt` borderThickness
      `Prelude.hashWithSalt` borderVisibility
      `Prelude.hashWithSalt` gutterSpacing
      `Prelude.hashWithSalt` gutterVisibility
      `Prelude.hashWithSalt` title

instance Prelude.NFData PanelConfiguration where
  rnf PanelConfiguration' {..} =
    Prelude.rnf backgroundColor
      `Prelude.seq` Prelude.rnf backgroundVisibility
      `Prelude.seq` Prelude.rnf borderColor
      `Prelude.seq` Prelude.rnf borderStyle
      `Prelude.seq` Prelude.rnf borderThickness
      `Prelude.seq` Prelude.rnf borderVisibility
      `Prelude.seq` Prelude.rnf gutterSpacing
      `Prelude.seq` Prelude.rnf gutterVisibility
      `Prelude.seq` Prelude.rnf title

instance Data.ToJSON PanelConfiguration where
  toJSON PanelConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BackgroundColor" Data..=)
              Prelude.<$> backgroundColor,
            ("BackgroundVisibility" Data..=)
              Prelude.<$> backgroundVisibility,
            ("BorderColor" Data..=) Prelude.<$> borderColor,
            ("BorderStyle" Data..=) Prelude.<$> borderStyle,
            ("BorderThickness" Data..=)
              Prelude.<$> borderThickness,
            ("BorderVisibility" Data..=)
              Prelude.<$> borderVisibility,
            ("GutterSpacing" Data..=) Prelude.<$> gutterSpacing,
            ("GutterVisibility" Data..=)
              Prelude.<$> gutterVisibility,
            ("Title" Data..=) Prelude.<$> title
          ]
      )
