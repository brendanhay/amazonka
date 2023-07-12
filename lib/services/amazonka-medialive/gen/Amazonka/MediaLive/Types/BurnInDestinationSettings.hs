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
-- Module      : Amazonka.MediaLive.Types.BurnInDestinationSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.BurnInDestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.BurnInAlignment
import Amazonka.MediaLive.Types.BurnInBackgroundColor
import Amazonka.MediaLive.Types.BurnInFontColor
import Amazonka.MediaLive.Types.BurnInOutlineColor
import Amazonka.MediaLive.Types.BurnInShadowColor
import Amazonka.MediaLive.Types.BurnInTeletextGridControl
import Amazonka.MediaLive.Types.InputLocation
import qualified Amazonka.Prelude as Prelude

-- | Burn In Destination Settings
--
-- /See:/ 'newBurnInDestinationSettings' smart constructor.
data BurnInDestinationSettings = BurnInDestinationSettings'
  { -- | If no explicit xPosition or yPosition is provided, setting alignment to
    -- centered will place the captions at the bottom center of the output.
    -- Similarly, setting a left alignment will align captions to the bottom
    -- left of the output. If x and y positions are given in conjunction with
    -- the alignment parameter, the font will be justified (either left or
    -- centered) relative to those coordinates. Selecting \"smart\"
    -- justification will left-justify live subtitles and center-justify
    -- pre-recorded subtitles. All burn-in and DVB-Sub font settings must
    -- match.
    alignment :: Prelude.Maybe BurnInAlignment,
    -- | Specifies the color of the rectangle behind the captions. All burn-in
    -- and DVB-Sub font settings must match.
    backgroundColor :: Prelude.Maybe BurnInBackgroundColor,
    -- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is
    -- transparent. Leaving this parameter out is equivalent to setting it to 0
    -- (transparent). All burn-in and DVB-Sub font settings must match.
    backgroundOpacity :: Prelude.Maybe Prelude.Natural,
    -- | External font file used for caption burn-in. File extension must be
    -- \'ttf\' or \'tte\'. Although the user can select output fonts for many
    -- different types of input captions, embedded, STL and teletext sources
    -- use a strict grid system. Using external fonts with these caption
    -- sources could cause unexpected display of proportional fonts. All
    -- burn-in and DVB-Sub font settings must match.
    font :: Prelude.Maybe InputLocation,
    -- | Specifies the color of the burned-in captions. This option is not valid
    -- for source captions that are STL, 608\/embedded or teletext. These
    -- source settings are already pre-defined by the caption stream. All
    -- burn-in and DVB-Sub font settings must match.
    fontColor :: Prelude.Maybe BurnInFontColor,
    -- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is
    -- transparent. All burn-in and DVB-Sub font settings must match.
    fontOpacity :: Prelude.Maybe Prelude.Natural,
    -- | Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in
    -- and DVB-Sub font settings must match.
    fontResolution :: Prelude.Maybe Prelude.Natural,
    -- | When set to \'auto\' fontSize will scale depending on the size of the
    -- output. Giving a positive integer will specify the exact font size in
    -- points. All burn-in and DVB-Sub font settings must match.
    fontSize :: Prelude.Maybe Prelude.Text,
    -- | Specifies font outline color. This option is not valid for source
    -- captions that are either 608\/embedded or teletext. These source
    -- settings are already pre-defined by the caption stream. All burn-in and
    -- DVB-Sub font settings must match.
    outlineColor :: Prelude.Maybe BurnInOutlineColor,
    -- | Specifies font outline size in pixels. This option is not valid for
    -- source captions that are either 608\/embedded or teletext. These source
    -- settings are already pre-defined by the caption stream. All burn-in and
    -- DVB-Sub font settings must match.
    outlineSize :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the color of the shadow cast by the captions. All burn-in and
    -- DVB-Sub font settings must match.
    shadowColor :: Prelude.Maybe BurnInShadowColor,
    -- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent.
    -- Leaving this parameter out is equivalent to setting it to 0
    -- (transparent). All burn-in and DVB-Sub font settings must match.
    shadowOpacity :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the horizontal offset of the shadow relative to the captions
    -- in pixels. A value of -2 would result in a shadow offset 2 pixels to the
    -- left. All burn-in and DVB-Sub font settings must match.
    shadowXOffset :: Prelude.Maybe Prelude.Int,
    -- | Specifies the vertical offset of the shadow relative to the captions in
    -- pixels. A value of -2 would result in a shadow offset 2 pixels above the
    -- text. All burn-in and DVB-Sub font settings must match.
    shadowYOffset :: Prelude.Maybe Prelude.Int,
    -- | Controls whether a fixed grid size will be used to generate the output
    -- subtitles bitmap. Only applicable for Teletext inputs and
    -- DVB-Sub\/Burn-in outputs.
    teletextGridControl :: Prelude.Maybe BurnInTeletextGridControl,
    -- | Specifies the horizontal position of the caption relative to the left
    -- side of the output in pixels. A value of 10 would result in the captions
    -- starting 10 pixels from the left of the output. If no explicit xPosition
    -- is provided, the horizontal caption position will be determined by the
    -- alignment parameter. All burn-in and DVB-Sub font settings must match.
    xPosition :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the vertical position of the caption relative to the top of
    -- the output in pixels. A value of 10 would result in the captions
    -- starting 10 pixels from the top of the output. If no explicit yPosition
    -- is provided, the caption will be positioned towards the bottom of the
    -- output. All burn-in and DVB-Sub font settings must match.
    yPosition :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BurnInDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alignment', 'burnInDestinationSettings_alignment' - If no explicit xPosition or yPosition is provided, setting alignment to
-- centered will place the captions at the bottom center of the output.
-- Similarly, setting a left alignment will align captions to the bottom
-- left of the output. If x and y positions are given in conjunction with
-- the alignment parameter, the font will be justified (either left or
-- centered) relative to those coordinates. Selecting \"smart\"
-- justification will left-justify live subtitles and center-justify
-- pre-recorded subtitles. All burn-in and DVB-Sub font settings must
-- match.
--
-- 'backgroundColor', 'burnInDestinationSettings_backgroundColor' - Specifies the color of the rectangle behind the captions. All burn-in
-- and DVB-Sub font settings must match.
--
-- 'backgroundOpacity', 'burnInDestinationSettings_backgroundOpacity' - Specifies the opacity of the background rectangle. 255 is opaque; 0 is
-- transparent. Leaving this parameter out is equivalent to setting it to 0
-- (transparent). All burn-in and DVB-Sub font settings must match.
--
-- 'font', 'burnInDestinationSettings_font' - External font file used for caption burn-in. File extension must be
-- \'ttf\' or \'tte\'. Although the user can select output fonts for many
-- different types of input captions, embedded, STL and teletext sources
-- use a strict grid system. Using external fonts with these caption
-- sources could cause unexpected display of proportional fonts. All
-- burn-in and DVB-Sub font settings must match.
--
-- 'fontColor', 'burnInDestinationSettings_fontColor' - Specifies the color of the burned-in captions. This option is not valid
-- for source captions that are STL, 608\/embedded or teletext. These
-- source settings are already pre-defined by the caption stream. All
-- burn-in and DVB-Sub font settings must match.
--
-- 'fontOpacity', 'burnInDestinationSettings_fontOpacity' - Specifies the opacity of the burned-in captions. 255 is opaque; 0 is
-- transparent. All burn-in and DVB-Sub font settings must match.
--
-- 'fontResolution', 'burnInDestinationSettings_fontResolution' - Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in
-- and DVB-Sub font settings must match.
--
-- 'fontSize', 'burnInDestinationSettings_fontSize' - When set to \'auto\' fontSize will scale depending on the size of the
-- output. Giving a positive integer will specify the exact font size in
-- points. All burn-in and DVB-Sub font settings must match.
--
-- 'outlineColor', 'burnInDestinationSettings_outlineColor' - Specifies font outline color. This option is not valid for source
-- captions that are either 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
--
-- 'outlineSize', 'burnInDestinationSettings_outlineSize' - Specifies font outline size in pixels. This option is not valid for
-- source captions that are either 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
--
-- 'shadowColor', 'burnInDestinationSettings_shadowColor' - Specifies the color of the shadow cast by the captions. All burn-in and
-- DVB-Sub font settings must match.
--
-- 'shadowOpacity', 'burnInDestinationSettings_shadowOpacity' - Specifies the opacity of the shadow. 255 is opaque; 0 is transparent.
-- Leaving this parameter out is equivalent to setting it to 0
-- (transparent). All burn-in and DVB-Sub font settings must match.
--
-- 'shadowXOffset', 'burnInDestinationSettings_shadowXOffset' - Specifies the horizontal offset of the shadow relative to the captions
-- in pixels. A value of -2 would result in a shadow offset 2 pixels to the
-- left. All burn-in and DVB-Sub font settings must match.
--
-- 'shadowYOffset', 'burnInDestinationSettings_shadowYOffset' - Specifies the vertical offset of the shadow relative to the captions in
-- pixels. A value of -2 would result in a shadow offset 2 pixels above the
-- text. All burn-in and DVB-Sub font settings must match.
--
-- 'teletextGridControl', 'burnInDestinationSettings_teletextGridControl' - Controls whether a fixed grid size will be used to generate the output
-- subtitles bitmap. Only applicable for Teletext inputs and
-- DVB-Sub\/Burn-in outputs.
--
-- 'xPosition', 'burnInDestinationSettings_xPosition' - Specifies the horizontal position of the caption relative to the left
-- side of the output in pixels. A value of 10 would result in the captions
-- starting 10 pixels from the left of the output. If no explicit xPosition
-- is provided, the horizontal caption position will be determined by the
-- alignment parameter. All burn-in and DVB-Sub font settings must match.
--
-- 'yPosition', 'burnInDestinationSettings_yPosition' - Specifies the vertical position of the caption relative to the top of
-- the output in pixels. A value of 10 would result in the captions
-- starting 10 pixels from the top of the output. If no explicit yPosition
-- is provided, the caption will be positioned towards the bottom of the
-- output. All burn-in and DVB-Sub font settings must match.
newBurnInDestinationSettings ::
  BurnInDestinationSettings
newBurnInDestinationSettings =
  BurnInDestinationSettings'
    { alignment =
        Prelude.Nothing,
      backgroundColor = Prelude.Nothing,
      backgroundOpacity = Prelude.Nothing,
      font = Prelude.Nothing,
      fontColor = Prelude.Nothing,
      fontOpacity = Prelude.Nothing,
      fontResolution = Prelude.Nothing,
      fontSize = Prelude.Nothing,
      outlineColor = Prelude.Nothing,
      outlineSize = Prelude.Nothing,
      shadowColor = Prelude.Nothing,
      shadowOpacity = Prelude.Nothing,
      shadowXOffset = Prelude.Nothing,
      shadowYOffset = Prelude.Nothing,
      teletextGridControl = Prelude.Nothing,
      xPosition = Prelude.Nothing,
      yPosition = Prelude.Nothing
    }

-- | If no explicit xPosition or yPosition is provided, setting alignment to
-- centered will place the captions at the bottom center of the output.
-- Similarly, setting a left alignment will align captions to the bottom
-- left of the output. If x and y positions are given in conjunction with
-- the alignment parameter, the font will be justified (either left or
-- centered) relative to those coordinates. Selecting \"smart\"
-- justification will left-justify live subtitles and center-justify
-- pre-recorded subtitles. All burn-in and DVB-Sub font settings must
-- match.
burnInDestinationSettings_alignment :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe BurnInAlignment)
burnInDestinationSettings_alignment = Lens.lens (\BurnInDestinationSettings' {alignment} -> alignment) (\s@BurnInDestinationSettings' {} a -> s {alignment = a} :: BurnInDestinationSettings)

-- | Specifies the color of the rectangle behind the captions. All burn-in
-- and DVB-Sub font settings must match.
burnInDestinationSettings_backgroundColor :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe BurnInBackgroundColor)
burnInDestinationSettings_backgroundColor = Lens.lens (\BurnInDestinationSettings' {backgroundColor} -> backgroundColor) (\s@BurnInDestinationSettings' {} a -> s {backgroundColor = a} :: BurnInDestinationSettings)

-- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is
-- transparent. Leaving this parameter out is equivalent to setting it to 0
-- (transparent). All burn-in and DVB-Sub font settings must match.
burnInDestinationSettings_backgroundOpacity :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe Prelude.Natural)
burnInDestinationSettings_backgroundOpacity = Lens.lens (\BurnInDestinationSettings' {backgroundOpacity} -> backgroundOpacity) (\s@BurnInDestinationSettings' {} a -> s {backgroundOpacity = a} :: BurnInDestinationSettings)

-- | External font file used for caption burn-in. File extension must be
-- \'ttf\' or \'tte\'. Although the user can select output fonts for many
-- different types of input captions, embedded, STL and teletext sources
-- use a strict grid system. Using external fonts with these caption
-- sources could cause unexpected display of proportional fonts. All
-- burn-in and DVB-Sub font settings must match.
burnInDestinationSettings_font :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe InputLocation)
burnInDestinationSettings_font = Lens.lens (\BurnInDestinationSettings' {font} -> font) (\s@BurnInDestinationSettings' {} a -> s {font = a} :: BurnInDestinationSettings)

-- | Specifies the color of the burned-in captions. This option is not valid
-- for source captions that are STL, 608\/embedded or teletext. These
-- source settings are already pre-defined by the caption stream. All
-- burn-in and DVB-Sub font settings must match.
burnInDestinationSettings_fontColor :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe BurnInFontColor)
burnInDestinationSettings_fontColor = Lens.lens (\BurnInDestinationSettings' {fontColor} -> fontColor) (\s@BurnInDestinationSettings' {} a -> s {fontColor = a} :: BurnInDestinationSettings)

-- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is
-- transparent. All burn-in and DVB-Sub font settings must match.
burnInDestinationSettings_fontOpacity :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe Prelude.Natural)
burnInDestinationSettings_fontOpacity = Lens.lens (\BurnInDestinationSettings' {fontOpacity} -> fontOpacity) (\s@BurnInDestinationSettings' {} a -> s {fontOpacity = a} :: BurnInDestinationSettings)

-- | Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in
-- and DVB-Sub font settings must match.
burnInDestinationSettings_fontResolution :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe Prelude.Natural)
burnInDestinationSettings_fontResolution = Lens.lens (\BurnInDestinationSettings' {fontResolution} -> fontResolution) (\s@BurnInDestinationSettings' {} a -> s {fontResolution = a} :: BurnInDestinationSettings)

-- | When set to \'auto\' fontSize will scale depending on the size of the
-- output. Giving a positive integer will specify the exact font size in
-- points. All burn-in and DVB-Sub font settings must match.
burnInDestinationSettings_fontSize :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe Prelude.Text)
burnInDestinationSettings_fontSize = Lens.lens (\BurnInDestinationSettings' {fontSize} -> fontSize) (\s@BurnInDestinationSettings' {} a -> s {fontSize = a} :: BurnInDestinationSettings)

-- | Specifies font outline color. This option is not valid for source
-- captions that are either 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
burnInDestinationSettings_outlineColor :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe BurnInOutlineColor)
burnInDestinationSettings_outlineColor = Lens.lens (\BurnInDestinationSettings' {outlineColor} -> outlineColor) (\s@BurnInDestinationSettings' {} a -> s {outlineColor = a} :: BurnInDestinationSettings)

-- | Specifies font outline size in pixels. This option is not valid for
-- source captions that are either 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
burnInDestinationSettings_outlineSize :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe Prelude.Natural)
burnInDestinationSettings_outlineSize = Lens.lens (\BurnInDestinationSettings' {outlineSize} -> outlineSize) (\s@BurnInDestinationSettings' {} a -> s {outlineSize = a} :: BurnInDestinationSettings)

-- | Specifies the color of the shadow cast by the captions. All burn-in and
-- DVB-Sub font settings must match.
burnInDestinationSettings_shadowColor :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe BurnInShadowColor)
burnInDestinationSettings_shadowColor = Lens.lens (\BurnInDestinationSettings' {shadowColor} -> shadowColor) (\s@BurnInDestinationSettings' {} a -> s {shadowColor = a} :: BurnInDestinationSettings)

-- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent.
-- Leaving this parameter out is equivalent to setting it to 0
-- (transparent). All burn-in and DVB-Sub font settings must match.
burnInDestinationSettings_shadowOpacity :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe Prelude.Natural)
burnInDestinationSettings_shadowOpacity = Lens.lens (\BurnInDestinationSettings' {shadowOpacity} -> shadowOpacity) (\s@BurnInDestinationSettings' {} a -> s {shadowOpacity = a} :: BurnInDestinationSettings)

-- | Specifies the horizontal offset of the shadow relative to the captions
-- in pixels. A value of -2 would result in a shadow offset 2 pixels to the
-- left. All burn-in and DVB-Sub font settings must match.
burnInDestinationSettings_shadowXOffset :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe Prelude.Int)
burnInDestinationSettings_shadowXOffset = Lens.lens (\BurnInDestinationSettings' {shadowXOffset} -> shadowXOffset) (\s@BurnInDestinationSettings' {} a -> s {shadowXOffset = a} :: BurnInDestinationSettings)

-- | Specifies the vertical offset of the shadow relative to the captions in
-- pixels. A value of -2 would result in a shadow offset 2 pixels above the
-- text. All burn-in and DVB-Sub font settings must match.
burnInDestinationSettings_shadowYOffset :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe Prelude.Int)
burnInDestinationSettings_shadowYOffset = Lens.lens (\BurnInDestinationSettings' {shadowYOffset} -> shadowYOffset) (\s@BurnInDestinationSettings' {} a -> s {shadowYOffset = a} :: BurnInDestinationSettings)

-- | Controls whether a fixed grid size will be used to generate the output
-- subtitles bitmap. Only applicable for Teletext inputs and
-- DVB-Sub\/Burn-in outputs.
burnInDestinationSettings_teletextGridControl :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe BurnInTeletextGridControl)
burnInDestinationSettings_teletextGridControl = Lens.lens (\BurnInDestinationSettings' {teletextGridControl} -> teletextGridControl) (\s@BurnInDestinationSettings' {} a -> s {teletextGridControl = a} :: BurnInDestinationSettings)

-- | Specifies the horizontal position of the caption relative to the left
-- side of the output in pixels. A value of 10 would result in the captions
-- starting 10 pixels from the left of the output. If no explicit xPosition
-- is provided, the horizontal caption position will be determined by the
-- alignment parameter. All burn-in and DVB-Sub font settings must match.
burnInDestinationSettings_xPosition :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe Prelude.Natural)
burnInDestinationSettings_xPosition = Lens.lens (\BurnInDestinationSettings' {xPosition} -> xPosition) (\s@BurnInDestinationSettings' {} a -> s {xPosition = a} :: BurnInDestinationSettings)

-- | Specifies the vertical position of the caption relative to the top of
-- the output in pixels. A value of 10 would result in the captions
-- starting 10 pixels from the top of the output. If no explicit yPosition
-- is provided, the caption will be positioned towards the bottom of the
-- output. All burn-in and DVB-Sub font settings must match.
burnInDestinationSettings_yPosition :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe Prelude.Natural)
burnInDestinationSettings_yPosition = Lens.lens (\BurnInDestinationSettings' {yPosition} -> yPosition) (\s@BurnInDestinationSettings' {} a -> s {yPosition = a} :: BurnInDestinationSettings)

instance Data.FromJSON BurnInDestinationSettings where
  parseJSON =
    Data.withObject
      "BurnInDestinationSettings"
      ( \x ->
          BurnInDestinationSettings'
            Prelude.<$> (x Data..:? "alignment")
            Prelude.<*> (x Data..:? "backgroundColor")
            Prelude.<*> (x Data..:? "backgroundOpacity")
            Prelude.<*> (x Data..:? "font")
            Prelude.<*> (x Data..:? "fontColor")
            Prelude.<*> (x Data..:? "fontOpacity")
            Prelude.<*> (x Data..:? "fontResolution")
            Prelude.<*> (x Data..:? "fontSize")
            Prelude.<*> (x Data..:? "outlineColor")
            Prelude.<*> (x Data..:? "outlineSize")
            Prelude.<*> (x Data..:? "shadowColor")
            Prelude.<*> (x Data..:? "shadowOpacity")
            Prelude.<*> (x Data..:? "shadowXOffset")
            Prelude.<*> (x Data..:? "shadowYOffset")
            Prelude.<*> (x Data..:? "teletextGridControl")
            Prelude.<*> (x Data..:? "xPosition")
            Prelude.<*> (x Data..:? "yPosition")
      )

instance Prelude.Hashable BurnInDestinationSettings where
  hashWithSalt _salt BurnInDestinationSettings' {..} =
    _salt
      `Prelude.hashWithSalt` alignment
      `Prelude.hashWithSalt` backgroundColor
      `Prelude.hashWithSalt` backgroundOpacity
      `Prelude.hashWithSalt` font
      `Prelude.hashWithSalt` fontColor
      `Prelude.hashWithSalt` fontOpacity
      `Prelude.hashWithSalt` fontResolution
      `Prelude.hashWithSalt` fontSize
      `Prelude.hashWithSalt` outlineColor
      `Prelude.hashWithSalt` outlineSize
      `Prelude.hashWithSalt` shadowColor
      `Prelude.hashWithSalt` shadowOpacity
      `Prelude.hashWithSalt` shadowXOffset
      `Prelude.hashWithSalt` shadowYOffset
      `Prelude.hashWithSalt` teletextGridControl
      `Prelude.hashWithSalt` xPosition
      `Prelude.hashWithSalt` yPosition

instance Prelude.NFData BurnInDestinationSettings where
  rnf BurnInDestinationSettings' {..} =
    Prelude.rnf alignment
      `Prelude.seq` Prelude.rnf backgroundColor
      `Prelude.seq` Prelude.rnf backgroundOpacity
      `Prelude.seq` Prelude.rnf font
      `Prelude.seq` Prelude.rnf fontColor
      `Prelude.seq` Prelude.rnf fontOpacity
      `Prelude.seq` Prelude.rnf fontResolution
      `Prelude.seq` Prelude.rnf fontSize
      `Prelude.seq` Prelude.rnf outlineColor
      `Prelude.seq` Prelude.rnf outlineSize
      `Prelude.seq` Prelude.rnf shadowColor
      `Prelude.seq` Prelude.rnf shadowOpacity
      `Prelude.seq` Prelude.rnf shadowXOffset
      `Prelude.seq` Prelude.rnf shadowYOffset
      `Prelude.seq` Prelude.rnf teletextGridControl
      `Prelude.seq` Prelude.rnf xPosition
      `Prelude.seq` Prelude.rnf yPosition

instance Data.ToJSON BurnInDestinationSettings where
  toJSON BurnInDestinationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("alignment" Data..=) Prelude.<$> alignment,
            ("backgroundColor" Data..=)
              Prelude.<$> backgroundColor,
            ("backgroundOpacity" Data..=)
              Prelude.<$> backgroundOpacity,
            ("font" Data..=) Prelude.<$> font,
            ("fontColor" Data..=) Prelude.<$> fontColor,
            ("fontOpacity" Data..=) Prelude.<$> fontOpacity,
            ("fontResolution" Data..=)
              Prelude.<$> fontResolution,
            ("fontSize" Data..=) Prelude.<$> fontSize,
            ("outlineColor" Data..=) Prelude.<$> outlineColor,
            ("outlineSize" Data..=) Prelude.<$> outlineSize,
            ("shadowColor" Data..=) Prelude.<$> shadowColor,
            ("shadowOpacity" Data..=) Prelude.<$> shadowOpacity,
            ("shadowXOffset" Data..=) Prelude.<$> shadowXOffset,
            ("shadowYOffset" Data..=) Prelude.<$> shadowYOffset,
            ("teletextGridControl" Data..=)
              Prelude.<$> teletextGridControl,
            ("xPosition" Data..=) Prelude.<$> xPosition,
            ("yPosition" Data..=) Prelude.<$> yPosition
          ]
      )
