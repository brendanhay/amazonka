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
-- Module      : Amazonka.QuickSight.Types.UIColorPalette
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.UIColorPalette where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The theme colors that apply to UI and to charts, excluding data colors.
-- The colors description is a hexadecimal color code that consists of six
-- alphanumerical characters, prefixed with @#@, for example #37BFF5. For
-- more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/themes-in-quicksight.html Using Themes in Amazon QuickSight>
-- in the /Amazon QuickSight User Guide./
--
-- /See:/ 'newUIColorPalette' smart constructor.
data UIColorPalette = UIColorPalette'
  { -- | This color is that applies to selected states and buttons.
    accent :: Prelude.Maybe Prelude.Text,
    -- | The foreground color that applies to any text or other elements that
    -- appear over the accent color.
    accentForeground :: Prelude.Maybe Prelude.Text,
    -- | The color that applies to error messages.
    danger :: Prelude.Maybe Prelude.Text,
    -- | The foreground color that applies to any text or other elements that
    -- appear over the error color.
    dangerForeground :: Prelude.Maybe Prelude.Text,
    -- | The color that applies to the names of fields that are identified as
    -- dimensions.
    dimension :: Prelude.Maybe Prelude.Text,
    -- | The foreground color that applies to any text or other elements that
    -- appear over the dimension color.
    dimensionForeground :: Prelude.Maybe Prelude.Text,
    -- | The color that applies to the names of fields that are identified as
    -- measures.
    measure :: Prelude.Maybe Prelude.Text,
    -- | The foreground color that applies to any text or other elements that
    -- appear over the measure color.
    measureForeground :: Prelude.Maybe Prelude.Text,
    -- | The background color that applies to visuals and other high emphasis UI.
    primaryBackground :: Prelude.Maybe Prelude.Text,
    -- | The color of text and other foreground elements that appear over the
    -- primary background regions, such as grid lines, borders, table banding,
    -- icons, and so on.
    primaryForeground :: Prelude.Maybe Prelude.Text,
    -- | The background color that applies to the sheet background and sheet
    -- controls.
    secondaryBackground :: Prelude.Maybe Prelude.Text,
    -- | The foreground color that applies to any sheet title, sheet control
    -- text, or UI that appears over the secondary background.
    secondaryForeground :: Prelude.Maybe Prelude.Text,
    -- | The color that applies to success messages, for example the check mark
    -- for a successful download.
    success :: Prelude.Maybe Prelude.Text,
    -- | The foreground color that applies to any text or other elements that
    -- appear over the success color.
    successForeground :: Prelude.Maybe Prelude.Text,
    -- | This color that applies to warning and informational messages.
    warning :: Prelude.Maybe Prelude.Text,
    -- | The foreground color that applies to any text or other elements that
    -- appear over the warning color.
    warningForeground :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UIColorPalette' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accent', 'uIColorPalette_accent' - This color is that applies to selected states and buttons.
--
-- 'accentForeground', 'uIColorPalette_accentForeground' - The foreground color that applies to any text or other elements that
-- appear over the accent color.
--
-- 'danger', 'uIColorPalette_danger' - The color that applies to error messages.
--
-- 'dangerForeground', 'uIColorPalette_dangerForeground' - The foreground color that applies to any text or other elements that
-- appear over the error color.
--
-- 'dimension', 'uIColorPalette_dimension' - The color that applies to the names of fields that are identified as
-- dimensions.
--
-- 'dimensionForeground', 'uIColorPalette_dimensionForeground' - The foreground color that applies to any text or other elements that
-- appear over the dimension color.
--
-- 'measure', 'uIColorPalette_measure' - The color that applies to the names of fields that are identified as
-- measures.
--
-- 'measureForeground', 'uIColorPalette_measureForeground' - The foreground color that applies to any text or other elements that
-- appear over the measure color.
--
-- 'primaryBackground', 'uIColorPalette_primaryBackground' - The background color that applies to visuals and other high emphasis UI.
--
-- 'primaryForeground', 'uIColorPalette_primaryForeground' - The color of text and other foreground elements that appear over the
-- primary background regions, such as grid lines, borders, table banding,
-- icons, and so on.
--
-- 'secondaryBackground', 'uIColorPalette_secondaryBackground' - The background color that applies to the sheet background and sheet
-- controls.
--
-- 'secondaryForeground', 'uIColorPalette_secondaryForeground' - The foreground color that applies to any sheet title, sheet control
-- text, or UI that appears over the secondary background.
--
-- 'success', 'uIColorPalette_success' - The color that applies to success messages, for example the check mark
-- for a successful download.
--
-- 'successForeground', 'uIColorPalette_successForeground' - The foreground color that applies to any text or other elements that
-- appear over the success color.
--
-- 'warning', 'uIColorPalette_warning' - This color that applies to warning and informational messages.
--
-- 'warningForeground', 'uIColorPalette_warningForeground' - The foreground color that applies to any text or other elements that
-- appear over the warning color.
newUIColorPalette ::
  UIColorPalette
newUIColorPalette =
  UIColorPalette'
    { accent = Prelude.Nothing,
      accentForeground = Prelude.Nothing,
      danger = Prelude.Nothing,
      dangerForeground = Prelude.Nothing,
      dimension = Prelude.Nothing,
      dimensionForeground = Prelude.Nothing,
      measure = Prelude.Nothing,
      measureForeground = Prelude.Nothing,
      primaryBackground = Prelude.Nothing,
      primaryForeground = Prelude.Nothing,
      secondaryBackground = Prelude.Nothing,
      secondaryForeground = Prelude.Nothing,
      success = Prelude.Nothing,
      successForeground = Prelude.Nothing,
      warning = Prelude.Nothing,
      warningForeground = Prelude.Nothing
    }

-- | This color is that applies to selected states and buttons.
uIColorPalette_accent :: Lens.Lens' UIColorPalette (Prelude.Maybe Prelude.Text)
uIColorPalette_accent = Lens.lens (\UIColorPalette' {accent} -> accent) (\s@UIColorPalette' {} a -> s {accent = a} :: UIColorPalette)

-- | The foreground color that applies to any text or other elements that
-- appear over the accent color.
uIColorPalette_accentForeground :: Lens.Lens' UIColorPalette (Prelude.Maybe Prelude.Text)
uIColorPalette_accentForeground = Lens.lens (\UIColorPalette' {accentForeground} -> accentForeground) (\s@UIColorPalette' {} a -> s {accentForeground = a} :: UIColorPalette)

-- | The color that applies to error messages.
uIColorPalette_danger :: Lens.Lens' UIColorPalette (Prelude.Maybe Prelude.Text)
uIColorPalette_danger = Lens.lens (\UIColorPalette' {danger} -> danger) (\s@UIColorPalette' {} a -> s {danger = a} :: UIColorPalette)

-- | The foreground color that applies to any text or other elements that
-- appear over the error color.
uIColorPalette_dangerForeground :: Lens.Lens' UIColorPalette (Prelude.Maybe Prelude.Text)
uIColorPalette_dangerForeground = Lens.lens (\UIColorPalette' {dangerForeground} -> dangerForeground) (\s@UIColorPalette' {} a -> s {dangerForeground = a} :: UIColorPalette)

-- | The color that applies to the names of fields that are identified as
-- dimensions.
uIColorPalette_dimension :: Lens.Lens' UIColorPalette (Prelude.Maybe Prelude.Text)
uIColorPalette_dimension = Lens.lens (\UIColorPalette' {dimension} -> dimension) (\s@UIColorPalette' {} a -> s {dimension = a} :: UIColorPalette)

-- | The foreground color that applies to any text or other elements that
-- appear over the dimension color.
uIColorPalette_dimensionForeground :: Lens.Lens' UIColorPalette (Prelude.Maybe Prelude.Text)
uIColorPalette_dimensionForeground = Lens.lens (\UIColorPalette' {dimensionForeground} -> dimensionForeground) (\s@UIColorPalette' {} a -> s {dimensionForeground = a} :: UIColorPalette)

-- | The color that applies to the names of fields that are identified as
-- measures.
uIColorPalette_measure :: Lens.Lens' UIColorPalette (Prelude.Maybe Prelude.Text)
uIColorPalette_measure = Lens.lens (\UIColorPalette' {measure} -> measure) (\s@UIColorPalette' {} a -> s {measure = a} :: UIColorPalette)

-- | The foreground color that applies to any text or other elements that
-- appear over the measure color.
uIColorPalette_measureForeground :: Lens.Lens' UIColorPalette (Prelude.Maybe Prelude.Text)
uIColorPalette_measureForeground = Lens.lens (\UIColorPalette' {measureForeground} -> measureForeground) (\s@UIColorPalette' {} a -> s {measureForeground = a} :: UIColorPalette)

-- | The background color that applies to visuals and other high emphasis UI.
uIColorPalette_primaryBackground :: Lens.Lens' UIColorPalette (Prelude.Maybe Prelude.Text)
uIColorPalette_primaryBackground = Lens.lens (\UIColorPalette' {primaryBackground} -> primaryBackground) (\s@UIColorPalette' {} a -> s {primaryBackground = a} :: UIColorPalette)

-- | The color of text and other foreground elements that appear over the
-- primary background regions, such as grid lines, borders, table banding,
-- icons, and so on.
uIColorPalette_primaryForeground :: Lens.Lens' UIColorPalette (Prelude.Maybe Prelude.Text)
uIColorPalette_primaryForeground = Lens.lens (\UIColorPalette' {primaryForeground} -> primaryForeground) (\s@UIColorPalette' {} a -> s {primaryForeground = a} :: UIColorPalette)

-- | The background color that applies to the sheet background and sheet
-- controls.
uIColorPalette_secondaryBackground :: Lens.Lens' UIColorPalette (Prelude.Maybe Prelude.Text)
uIColorPalette_secondaryBackground = Lens.lens (\UIColorPalette' {secondaryBackground} -> secondaryBackground) (\s@UIColorPalette' {} a -> s {secondaryBackground = a} :: UIColorPalette)

-- | The foreground color that applies to any sheet title, sheet control
-- text, or UI that appears over the secondary background.
uIColorPalette_secondaryForeground :: Lens.Lens' UIColorPalette (Prelude.Maybe Prelude.Text)
uIColorPalette_secondaryForeground = Lens.lens (\UIColorPalette' {secondaryForeground} -> secondaryForeground) (\s@UIColorPalette' {} a -> s {secondaryForeground = a} :: UIColorPalette)

-- | The color that applies to success messages, for example the check mark
-- for a successful download.
uIColorPalette_success :: Lens.Lens' UIColorPalette (Prelude.Maybe Prelude.Text)
uIColorPalette_success = Lens.lens (\UIColorPalette' {success} -> success) (\s@UIColorPalette' {} a -> s {success = a} :: UIColorPalette)

-- | The foreground color that applies to any text or other elements that
-- appear over the success color.
uIColorPalette_successForeground :: Lens.Lens' UIColorPalette (Prelude.Maybe Prelude.Text)
uIColorPalette_successForeground = Lens.lens (\UIColorPalette' {successForeground} -> successForeground) (\s@UIColorPalette' {} a -> s {successForeground = a} :: UIColorPalette)

-- | This color that applies to warning and informational messages.
uIColorPalette_warning :: Lens.Lens' UIColorPalette (Prelude.Maybe Prelude.Text)
uIColorPalette_warning = Lens.lens (\UIColorPalette' {warning} -> warning) (\s@UIColorPalette' {} a -> s {warning = a} :: UIColorPalette)

-- | The foreground color that applies to any text or other elements that
-- appear over the warning color.
uIColorPalette_warningForeground :: Lens.Lens' UIColorPalette (Prelude.Maybe Prelude.Text)
uIColorPalette_warningForeground = Lens.lens (\UIColorPalette' {warningForeground} -> warningForeground) (\s@UIColorPalette' {} a -> s {warningForeground = a} :: UIColorPalette)

instance Data.FromJSON UIColorPalette where
  parseJSON =
    Data.withObject
      "UIColorPalette"
      ( \x ->
          UIColorPalette'
            Prelude.<$> (x Data..:? "Accent")
            Prelude.<*> (x Data..:? "AccentForeground")
            Prelude.<*> (x Data..:? "Danger")
            Prelude.<*> (x Data..:? "DangerForeground")
            Prelude.<*> (x Data..:? "Dimension")
            Prelude.<*> (x Data..:? "DimensionForeground")
            Prelude.<*> (x Data..:? "Measure")
            Prelude.<*> (x Data..:? "MeasureForeground")
            Prelude.<*> (x Data..:? "PrimaryBackground")
            Prelude.<*> (x Data..:? "PrimaryForeground")
            Prelude.<*> (x Data..:? "SecondaryBackground")
            Prelude.<*> (x Data..:? "SecondaryForeground")
            Prelude.<*> (x Data..:? "Success")
            Prelude.<*> (x Data..:? "SuccessForeground")
            Prelude.<*> (x Data..:? "Warning")
            Prelude.<*> (x Data..:? "WarningForeground")
      )

instance Prelude.Hashable UIColorPalette where
  hashWithSalt _salt UIColorPalette' {..} =
    _salt
      `Prelude.hashWithSalt` accent
      `Prelude.hashWithSalt` accentForeground
      `Prelude.hashWithSalt` danger
      `Prelude.hashWithSalt` dangerForeground
      `Prelude.hashWithSalt` dimension
      `Prelude.hashWithSalt` dimensionForeground
      `Prelude.hashWithSalt` measure
      `Prelude.hashWithSalt` measureForeground
      `Prelude.hashWithSalt` primaryBackground
      `Prelude.hashWithSalt` primaryForeground
      `Prelude.hashWithSalt` secondaryBackground
      `Prelude.hashWithSalt` secondaryForeground
      `Prelude.hashWithSalt` success
      `Prelude.hashWithSalt` successForeground
      `Prelude.hashWithSalt` warning
      `Prelude.hashWithSalt` warningForeground

instance Prelude.NFData UIColorPalette where
  rnf UIColorPalette' {..} =
    Prelude.rnf accent
      `Prelude.seq` Prelude.rnf accentForeground
      `Prelude.seq` Prelude.rnf danger
      `Prelude.seq` Prelude.rnf dangerForeground
      `Prelude.seq` Prelude.rnf dimension
      `Prelude.seq` Prelude.rnf dimensionForeground
      `Prelude.seq` Prelude.rnf measure
      `Prelude.seq` Prelude.rnf measureForeground
      `Prelude.seq` Prelude.rnf primaryBackground
      `Prelude.seq` Prelude.rnf primaryForeground
      `Prelude.seq` Prelude.rnf secondaryBackground
      `Prelude.seq` Prelude.rnf secondaryForeground
      `Prelude.seq` Prelude.rnf success
      `Prelude.seq` Prelude.rnf successForeground
      `Prelude.seq` Prelude.rnf warning
      `Prelude.seq` Prelude.rnf warningForeground

instance Data.ToJSON UIColorPalette where
  toJSON UIColorPalette' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Accent" Data..=) Prelude.<$> accent,
            ("AccentForeground" Data..=)
              Prelude.<$> accentForeground,
            ("Danger" Data..=) Prelude.<$> danger,
            ("DangerForeground" Data..=)
              Prelude.<$> dangerForeground,
            ("Dimension" Data..=) Prelude.<$> dimension,
            ("DimensionForeground" Data..=)
              Prelude.<$> dimensionForeground,
            ("Measure" Data..=) Prelude.<$> measure,
            ("MeasureForeground" Data..=)
              Prelude.<$> measureForeground,
            ("PrimaryBackground" Data..=)
              Prelude.<$> primaryBackground,
            ("PrimaryForeground" Data..=)
              Prelude.<$> primaryForeground,
            ("SecondaryBackground" Data..=)
              Prelude.<$> secondaryBackground,
            ("SecondaryForeground" Data..=)
              Prelude.<$> secondaryForeground,
            ("Success" Data..=) Prelude.<$> success,
            ("SuccessForeground" Data..=)
              Prelude.<$> successForeground,
            ("Warning" Data..=) Prelude.<$> warning,
            ("WarningForeground" Data..=)
              Prelude.<$> warningForeground
          ]
      )
