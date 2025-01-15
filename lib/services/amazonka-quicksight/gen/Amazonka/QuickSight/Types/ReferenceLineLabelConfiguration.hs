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
-- Module      : Amazonka.QuickSight.Types.ReferenceLineLabelConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ReferenceLineLabelConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FontConfiguration
import Amazonka.QuickSight.Types.ReferenceLineCustomLabelConfiguration
import Amazonka.QuickSight.Types.ReferenceLineLabelHorizontalPosition
import Amazonka.QuickSight.Types.ReferenceLineLabelVerticalPosition
import Amazonka.QuickSight.Types.ReferenceLineValueLabelConfiguration

-- | The label configuration of a reference line.
--
-- /See:/ 'newReferenceLineLabelConfiguration' smart constructor.
data ReferenceLineLabelConfiguration = ReferenceLineLabelConfiguration'
  { -- | The custom label configuration of the label in a reference line.
    customLabelConfiguration :: Prelude.Maybe ReferenceLineCustomLabelConfiguration,
    -- | The font color configuration of the label in a reference line.
    fontColor :: Prelude.Maybe Prelude.Text,
    -- | The font configuration of the label in a reference line.
    fontConfiguration :: Prelude.Maybe FontConfiguration,
    -- | The horizontal position configuration of the label in a reference line.
    -- Choose one of the following options:
    --
    -- -   @LEFT@
    --
    -- -   @CENTER@
    --
    -- -   @RIGHT@
    horizontalPosition :: Prelude.Maybe ReferenceLineLabelHorizontalPosition,
    -- | The value label configuration of the label in a reference line.
    valueLabelConfiguration :: Prelude.Maybe ReferenceLineValueLabelConfiguration,
    -- | The vertical position configuration of the label in a reference line.
    -- Choose one of the following options:
    --
    -- -   @ABOVE@
    --
    -- -   @BELOW@
    verticalPosition :: Prelude.Maybe ReferenceLineLabelVerticalPosition
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReferenceLineLabelConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customLabelConfiguration', 'referenceLineLabelConfiguration_customLabelConfiguration' - The custom label configuration of the label in a reference line.
--
-- 'fontColor', 'referenceLineLabelConfiguration_fontColor' - The font color configuration of the label in a reference line.
--
-- 'fontConfiguration', 'referenceLineLabelConfiguration_fontConfiguration' - The font configuration of the label in a reference line.
--
-- 'horizontalPosition', 'referenceLineLabelConfiguration_horizontalPosition' - The horizontal position configuration of the label in a reference line.
-- Choose one of the following options:
--
-- -   @LEFT@
--
-- -   @CENTER@
--
-- -   @RIGHT@
--
-- 'valueLabelConfiguration', 'referenceLineLabelConfiguration_valueLabelConfiguration' - The value label configuration of the label in a reference line.
--
-- 'verticalPosition', 'referenceLineLabelConfiguration_verticalPosition' - The vertical position configuration of the label in a reference line.
-- Choose one of the following options:
--
-- -   @ABOVE@
--
-- -   @BELOW@
newReferenceLineLabelConfiguration ::
  ReferenceLineLabelConfiguration
newReferenceLineLabelConfiguration =
  ReferenceLineLabelConfiguration'
    { customLabelConfiguration =
        Prelude.Nothing,
      fontColor = Prelude.Nothing,
      fontConfiguration = Prelude.Nothing,
      horizontalPosition = Prelude.Nothing,
      valueLabelConfiguration = Prelude.Nothing,
      verticalPosition = Prelude.Nothing
    }

-- | The custom label configuration of the label in a reference line.
referenceLineLabelConfiguration_customLabelConfiguration :: Lens.Lens' ReferenceLineLabelConfiguration (Prelude.Maybe ReferenceLineCustomLabelConfiguration)
referenceLineLabelConfiguration_customLabelConfiguration = Lens.lens (\ReferenceLineLabelConfiguration' {customLabelConfiguration} -> customLabelConfiguration) (\s@ReferenceLineLabelConfiguration' {} a -> s {customLabelConfiguration = a} :: ReferenceLineLabelConfiguration)

-- | The font color configuration of the label in a reference line.
referenceLineLabelConfiguration_fontColor :: Lens.Lens' ReferenceLineLabelConfiguration (Prelude.Maybe Prelude.Text)
referenceLineLabelConfiguration_fontColor = Lens.lens (\ReferenceLineLabelConfiguration' {fontColor} -> fontColor) (\s@ReferenceLineLabelConfiguration' {} a -> s {fontColor = a} :: ReferenceLineLabelConfiguration)

-- | The font configuration of the label in a reference line.
referenceLineLabelConfiguration_fontConfiguration :: Lens.Lens' ReferenceLineLabelConfiguration (Prelude.Maybe FontConfiguration)
referenceLineLabelConfiguration_fontConfiguration = Lens.lens (\ReferenceLineLabelConfiguration' {fontConfiguration} -> fontConfiguration) (\s@ReferenceLineLabelConfiguration' {} a -> s {fontConfiguration = a} :: ReferenceLineLabelConfiguration)

-- | The horizontal position configuration of the label in a reference line.
-- Choose one of the following options:
--
-- -   @LEFT@
--
-- -   @CENTER@
--
-- -   @RIGHT@
referenceLineLabelConfiguration_horizontalPosition :: Lens.Lens' ReferenceLineLabelConfiguration (Prelude.Maybe ReferenceLineLabelHorizontalPosition)
referenceLineLabelConfiguration_horizontalPosition = Lens.lens (\ReferenceLineLabelConfiguration' {horizontalPosition} -> horizontalPosition) (\s@ReferenceLineLabelConfiguration' {} a -> s {horizontalPosition = a} :: ReferenceLineLabelConfiguration)

-- | The value label configuration of the label in a reference line.
referenceLineLabelConfiguration_valueLabelConfiguration :: Lens.Lens' ReferenceLineLabelConfiguration (Prelude.Maybe ReferenceLineValueLabelConfiguration)
referenceLineLabelConfiguration_valueLabelConfiguration = Lens.lens (\ReferenceLineLabelConfiguration' {valueLabelConfiguration} -> valueLabelConfiguration) (\s@ReferenceLineLabelConfiguration' {} a -> s {valueLabelConfiguration = a} :: ReferenceLineLabelConfiguration)

-- | The vertical position configuration of the label in a reference line.
-- Choose one of the following options:
--
-- -   @ABOVE@
--
-- -   @BELOW@
referenceLineLabelConfiguration_verticalPosition :: Lens.Lens' ReferenceLineLabelConfiguration (Prelude.Maybe ReferenceLineLabelVerticalPosition)
referenceLineLabelConfiguration_verticalPosition = Lens.lens (\ReferenceLineLabelConfiguration' {verticalPosition} -> verticalPosition) (\s@ReferenceLineLabelConfiguration' {} a -> s {verticalPosition = a} :: ReferenceLineLabelConfiguration)

instance
  Data.FromJSON
    ReferenceLineLabelConfiguration
  where
  parseJSON =
    Data.withObject
      "ReferenceLineLabelConfiguration"
      ( \x ->
          ReferenceLineLabelConfiguration'
            Prelude.<$> (x Data..:? "CustomLabelConfiguration")
            Prelude.<*> (x Data..:? "FontColor")
            Prelude.<*> (x Data..:? "FontConfiguration")
            Prelude.<*> (x Data..:? "HorizontalPosition")
            Prelude.<*> (x Data..:? "ValueLabelConfiguration")
            Prelude.<*> (x Data..:? "VerticalPosition")
      )

instance
  Prelude.Hashable
    ReferenceLineLabelConfiguration
  where
  hashWithSalt
    _salt
    ReferenceLineLabelConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` customLabelConfiguration
        `Prelude.hashWithSalt` fontColor
        `Prelude.hashWithSalt` fontConfiguration
        `Prelude.hashWithSalt` horizontalPosition
        `Prelude.hashWithSalt` valueLabelConfiguration
        `Prelude.hashWithSalt` verticalPosition

instance
  Prelude.NFData
    ReferenceLineLabelConfiguration
  where
  rnf ReferenceLineLabelConfiguration' {..} =
    Prelude.rnf customLabelConfiguration `Prelude.seq`
      Prelude.rnf fontColor `Prelude.seq`
        Prelude.rnf fontConfiguration `Prelude.seq`
          Prelude.rnf horizontalPosition `Prelude.seq`
            Prelude.rnf valueLabelConfiguration `Prelude.seq`
              Prelude.rnf verticalPosition

instance Data.ToJSON ReferenceLineLabelConfiguration where
  toJSON ReferenceLineLabelConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomLabelConfiguration" Data..=)
              Prelude.<$> customLabelConfiguration,
            ("FontColor" Data..=) Prelude.<$> fontColor,
            ("FontConfiguration" Data..=)
              Prelude.<$> fontConfiguration,
            ("HorizontalPosition" Data..=)
              Prelude.<$> horizontalPosition,
            ("ValueLabelConfiguration" Data..=)
              Prelude.<$> valueLabelConfiguration,
            ("VerticalPosition" Data..=)
              Prelude.<$> verticalPosition
          ]
      )
