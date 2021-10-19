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
-- Module      : Network.AWS.MediaLive.Types.BurnInDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BurnInDestinationSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.BurnInAlignment
import Network.AWS.MediaLive.Types.BurnInBackgroundColor
import Network.AWS.MediaLive.Types.BurnInFontColor
import Network.AWS.MediaLive.Types.BurnInOutlineColor
import Network.AWS.MediaLive.Types.BurnInShadowColor
import Network.AWS.MediaLive.Types.BurnInTeletextGridControl
import Network.AWS.MediaLive.Types.InputLocation
import qualified Network.AWS.Prelude as Prelude

-- | Burn In Destination Settings
--
-- /See:/ 'newBurnInDestinationSettings' smart constructor.
data BurnInDestinationSettings = BurnInDestinationSettings'
  { -- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is
    -- transparent. Leaving this parameter out is equivalent to setting it to 0
    -- (transparent). All burn-in and DVB-Sub font settings must match.
    backgroundOpacity :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is
    -- transparent. All burn-in and DVB-Sub font settings must match.
    fontOpacity :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the vertical offset of the shadow relative to the captions in
    -- pixels. A value of -2 would result in a shadow offset 2 pixels above the
    -- text. All burn-in and DVB-Sub font settings must match.
    shadowYOffset :: Prelude.Maybe Prelude.Int,
    -- | Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in
    -- and DVB-Sub font settings must match.
    fontResolution :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the vertical position of the caption relative to the top of
    -- the output in pixels. A value of 10 would result in the captions
    -- starting 10 pixels from the top of the output. If no explicit yPosition
    -- is provided, the caption will be positioned towards the bottom of the
    -- output. All burn-in and DVB-Sub font settings must match.
    yPosition :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the color of the rectangle behind the captions. All burn-in
    -- and DVB-Sub font settings must match.
    backgroundColor :: Prelude.Maybe BurnInBackgroundColor,
    -- | Specifies the horizontal offset of the shadow relative to the captions
    -- in pixels. A value of -2 would result in a shadow offset 2 pixels to the
    -- left. All burn-in and DVB-Sub font settings must match.
    shadowXOffset :: Prelude.Maybe Prelude.Int,
    -- | When set to \'auto\' fontSize will scale depending on the size of the
    -- output. Giving a positive integer will specify the exact font size in
    -- points. All burn-in and DVB-Sub font settings must match.
    fontSize :: Prelude.Maybe Prelude.Text,
    -- | Specifies the horizontal position of the caption relative to the left
    -- side of the output in pixels. A value of 10 would result in the captions
    -- starting 10 pixels from the left of the output. If no explicit xPosition
    -- is provided, the horizontal caption position will be determined by the
    -- alignment parameter. All burn-in and DVB-Sub font settings must match.
    xPosition :: Prelude.Maybe Prelude.Natural,
    -- | If no explicit xPosition or yPosition is provided, setting alignment to
    -- centered will place the captions at the bottom center of the output.
    -- Similarly, setting a left alignment will align captions to the bottom
    -- left of the output. If x and y positions are given in conjunction with
    -- the alignment parameter, the font will be justified (either left or
    -- centered) relative to those coordinates. Selecting \"smart\"
    -- justification will left-justify live subtitles and center-justify
    -- pre-recorded subtitles. All burn-in and DVB-Sub font settings must
    -- match.
    alignment :: Prelude.Maybe BurnInAlignment,
    -- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent.
    -- Leaving this parameter out is equivalent to setting it to 0
    -- (transparent). All burn-in and DVB-Sub font settings must match.
    shadowOpacity :: Prelude.Maybe Prelude.Natural,
    -- | Controls whether a fixed grid size will be used to generate the output
    -- subtitles bitmap. Only applicable for Teletext inputs and
    -- DVB-Sub\/Burn-in outputs.
    teletextGridControl :: Prelude.Maybe BurnInTeletextGridControl,
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
    -- | External font file used for caption burn-in. File extension must be
    -- \'ttf\' or \'tte\'. Although the user can select output fonts for many
    -- different types of input captions, embedded, STL and teletext sources
    -- use a strict grid system. Using external fonts with these caption
    -- sources could cause unexpected display of proportional fonts. All
    -- burn-in and DVB-Sub font settings must match.
    font :: Prelude.Maybe InputLocation,
    -- | Specifies the color of the shadow cast by the captions. All burn-in and
    -- DVB-Sub font settings must match.
    shadowColor :: Prelude.Maybe BurnInShadowColor,
    -- | Specifies the color of the burned-in captions. This option is not valid
    -- for source captions that are STL, 608\/embedded or teletext. These
    -- source settings are already pre-defined by the caption stream. All
    -- burn-in and DVB-Sub font settings must match.
    fontColor :: Prelude.Maybe BurnInFontColor
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
-- 'backgroundOpacity', 'burnInDestinationSettings_backgroundOpacity' - Specifies the opacity of the background rectangle. 255 is opaque; 0 is
-- transparent. Leaving this parameter out is equivalent to setting it to 0
-- (transparent). All burn-in and DVB-Sub font settings must match.
--
-- 'fontOpacity', 'burnInDestinationSettings_fontOpacity' - Specifies the opacity of the burned-in captions. 255 is opaque; 0 is
-- transparent. All burn-in and DVB-Sub font settings must match.
--
-- 'shadowYOffset', 'burnInDestinationSettings_shadowYOffset' - Specifies the vertical offset of the shadow relative to the captions in
-- pixels. A value of -2 would result in a shadow offset 2 pixels above the
-- text. All burn-in and DVB-Sub font settings must match.
--
-- 'fontResolution', 'burnInDestinationSettings_fontResolution' - Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in
-- and DVB-Sub font settings must match.
--
-- 'yPosition', 'burnInDestinationSettings_yPosition' - Specifies the vertical position of the caption relative to the top of
-- the output in pixels. A value of 10 would result in the captions
-- starting 10 pixels from the top of the output. If no explicit yPosition
-- is provided, the caption will be positioned towards the bottom of the
-- output. All burn-in and DVB-Sub font settings must match.
--
-- 'backgroundColor', 'burnInDestinationSettings_backgroundColor' - Specifies the color of the rectangle behind the captions. All burn-in
-- and DVB-Sub font settings must match.
--
-- 'shadowXOffset', 'burnInDestinationSettings_shadowXOffset' - Specifies the horizontal offset of the shadow relative to the captions
-- in pixels. A value of -2 would result in a shadow offset 2 pixels to the
-- left. All burn-in and DVB-Sub font settings must match.
--
-- 'fontSize', 'burnInDestinationSettings_fontSize' - When set to \'auto\' fontSize will scale depending on the size of the
-- output. Giving a positive integer will specify the exact font size in
-- points. All burn-in and DVB-Sub font settings must match.
--
-- 'xPosition', 'burnInDestinationSettings_xPosition' - Specifies the horizontal position of the caption relative to the left
-- side of the output in pixels. A value of 10 would result in the captions
-- starting 10 pixels from the left of the output. If no explicit xPosition
-- is provided, the horizontal caption position will be determined by the
-- alignment parameter. All burn-in and DVB-Sub font settings must match.
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
-- 'shadowOpacity', 'burnInDestinationSettings_shadowOpacity' - Specifies the opacity of the shadow. 255 is opaque; 0 is transparent.
-- Leaving this parameter out is equivalent to setting it to 0
-- (transparent). All burn-in and DVB-Sub font settings must match.
--
-- 'teletextGridControl', 'burnInDestinationSettings_teletextGridControl' - Controls whether a fixed grid size will be used to generate the output
-- subtitles bitmap. Only applicable for Teletext inputs and
-- DVB-Sub\/Burn-in outputs.
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
-- 'font', 'burnInDestinationSettings_font' - External font file used for caption burn-in. File extension must be
-- \'ttf\' or \'tte\'. Although the user can select output fonts for many
-- different types of input captions, embedded, STL and teletext sources
-- use a strict grid system. Using external fonts with these caption
-- sources could cause unexpected display of proportional fonts. All
-- burn-in and DVB-Sub font settings must match.
--
-- 'shadowColor', 'burnInDestinationSettings_shadowColor' - Specifies the color of the shadow cast by the captions. All burn-in and
-- DVB-Sub font settings must match.
--
-- 'fontColor', 'burnInDestinationSettings_fontColor' - Specifies the color of the burned-in captions. This option is not valid
-- for source captions that are STL, 608\/embedded or teletext. These
-- source settings are already pre-defined by the caption stream. All
-- burn-in and DVB-Sub font settings must match.
newBurnInDestinationSettings ::
  BurnInDestinationSettings
newBurnInDestinationSettings =
  BurnInDestinationSettings'
    { backgroundOpacity =
        Prelude.Nothing,
      fontOpacity = Prelude.Nothing,
      shadowYOffset = Prelude.Nothing,
      fontResolution = Prelude.Nothing,
      yPosition = Prelude.Nothing,
      backgroundColor = Prelude.Nothing,
      shadowXOffset = Prelude.Nothing,
      fontSize = Prelude.Nothing,
      xPosition = Prelude.Nothing,
      alignment = Prelude.Nothing,
      shadowOpacity = Prelude.Nothing,
      teletextGridControl = Prelude.Nothing,
      outlineColor = Prelude.Nothing,
      outlineSize = Prelude.Nothing,
      font = Prelude.Nothing,
      shadowColor = Prelude.Nothing,
      fontColor = Prelude.Nothing
    }

-- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is
-- transparent. Leaving this parameter out is equivalent to setting it to 0
-- (transparent). All burn-in and DVB-Sub font settings must match.
burnInDestinationSettings_backgroundOpacity :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe Prelude.Natural)
burnInDestinationSettings_backgroundOpacity = Lens.lens (\BurnInDestinationSettings' {backgroundOpacity} -> backgroundOpacity) (\s@BurnInDestinationSettings' {} a -> s {backgroundOpacity = a} :: BurnInDestinationSettings)

-- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is
-- transparent. All burn-in and DVB-Sub font settings must match.
burnInDestinationSettings_fontOpacity :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe Prelude.Natural)
burnInDestinationSettings_fontOpacity = Lens.lens (\BurnInDestinationSettings' {fontOpacity} -> fontOpacity) (\s@BurnInDestinationSettings' {} a -> s {fontOpacity = a} :: BurnInDestinationSettings)

-- | Specifies the vertical offset of the shadow relative to the captions in
-- pixels. A value of -2 would result in a shadow offset 2 pixels above the
-- text. All burn-in and DVB-Sub font settings must match.
burnInDestinationSettings_shadowYOffset :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe Prelude.Int)
burnInDestinationSettings_shadowYOffset = Lens.lens (\BurnInDestinationSettings' {shadowYOffset} -> shadowYOffset) (\s@BurnInDestinationSettings' {} a -> s {shadowYOffset = a} :: BurnInDestinationSettings)

-- | Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in
-- and DVB-Sub font settings must match.
burnInDestinationSettings_fontResolution :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe Prelude.Natural)
burnInDestinationSettings_fontResolution = Lens.lens (\BurnInDestinationSettings' {fontResolution} -> fontResolution) (\s@BurnInDestinationSettings' {} a -> s {fontResolution = a} :: BurnInDestinationSettings)

-- | Specifies the vertical position of the caption relative to the top of
-- the output in pixels. A value of 10 would result in the captions
-- starting 10 pixels from the top of the output. If no explicit yPosition
-- is provided, the caption will be positioned towards the bottom of the
-- output. All burn-in and DVB-Sub font settings must match.
burnInDestinationSettings_yPosition :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe Prelude.Natural)
burnInDestinationSettings_yPosition = Lens.lens (\BurnInDestinationSettings' {yPosition} -> yPosition) (\s@BurnInDestinationSettings' {} a -> s {yPosition = a} :: BurnInDestinationSettings)

-- | Specifies the color of the rectangle behind the captions. All burn-in
-- and DVB-Sub font settings must match.
burnInDestinationSettings_backgroundColor :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe BurnInBackgroundColor)
burnInDestinationSettings_backgroundColor = Lens.lens (\BurnInDestinationSettings' {backgroundColor} -> backgroundColor) (\s@BurnInDestinationSettings' {} a -> s {backgroundColor = a} :: BurnInDestinationSettings)

-- | Specifies the horizontal offset of the shadow relative to the captions
-- in pixels. A value of -2 would result in a shadow offset 2 pixels to the
-- left. All burn-in and DVB-Sub font settings must match.
burnInDestinationSettings_shadowXOffset :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe Prelude.Int)
burnInDestinationSettings_shadowXOffset = Lens.lens (\BurnInDestinationSettings' {shadowXOffset} -> shadowXOffset) (\s@BurnInDestinationSettings' {} a -> s {shadowXOffset = a} :: BurnInDestinationSettings)

-- | When set to \'auto\' fontSize will scale depending on the size of the
-- output. Giving a positive integer will specify the exact font size in
-- points. All burn-in and DVB-Sub font settings must match.
burnInDestinationSettings_fontSize :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe Prelude.Text)
burnInDestinationSettings_fontSize = Lens.lens (\BurnInDestinationSettings' {fontSize} -> fontSize) (\s@BurnInDestinationSettings' {} a -> s {fontSize = a} :: BurnInDestinationSettings)

-- | Specifies the horizontal position of the caption relative to the left
-- side of the output in pixels. A value of 10 would result in the captions
-- starting 10 pixels from the left of the output. If no explicit xPosition
-- is provided, the horizontal caption position will be determined by the
-- alignment parameter. All burn-in and DVB-Sub font settings must match.
burnInDestinationSettings_xPosition :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe Prelude.Natural)
burnInDestinationSettings_xPosition = Lens.lens (\BurnInDestinationSettings' {xPosition} -> xPosition) (\s@BurnInDestinationSettings' {} a -> s {xPosition = a} :: BurnInDestinationSettings)

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

-- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent.
-- Leaving this parameter out is equivalent to setting it to 0
-- (transparent). All burn-in and DVB-Sub font settings must match.
burnInDestinationSettings_shadowOpacity :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe Prelude.Natural)
burnInDestinationSettings_shadowOpacity = Lens.lens (\BurnInDestinationSettings' {shadowOpacity} -> shadowOpacity) (\s@BurnInDestinationSettings' {} a -> s {shadowOpacity = a} :: BurnInDestinationSettings)

-- | Controls whether a fixed grid size will be used to generate the output
-- subtitles bitmap. Only applicable for Teletext inputs and
-- DVB-Sub\/Burn-in outputs.
burnInDestinationSettings_teletextGridControl :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe BurnInTeletextGridControl)
burnInDestinationSettings_teletextGridControl = Lens.lens (\BurnInDestinationSettings' {teletextGridControl} -> teletextGridControl) (\s@BurnInDestinationSettings' {} a -> s {teletextGridControl = a} :: BurnInDestinationSettings)

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

-- | External font file used for caption burn-in. File extension must be
-- \'ttf\' or \'tte\'. Although the user can select output fonts for many
-- different types of input captions, embedded, STL and teletext sources
-- use a strict grid system. Using external fonts with these caption
-- sources could cause unexpected display of proportional fonts. All
-- burn-in and DVB-Sub font settings must match.
burnInDestinationSettings_font :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe InputLocation)
burnInDestinationSettings_font = Lens.lens (\BurnInDestinationSettings' {font} -> font) (\s@BurnInDestinationSettings' {} a -> s {font = a} :: BurnInDestinationSettings)

-- | Specifies the color of the shadow cast by the captions. All burn-in and
-- DVB-Sub font settings must match.
burnInDestinationSettings_shadowColor :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe BurnInShadowColor)
burnInDestinationSettings_shadowColor = Lens.lens (\BurnInDestinationSettings' {shadowColor} -> shadowColor) (\s@BurnInDestinationSettings' {} a -> s {shadowColor = a} :: BurnInDestinationSettings)

-- | Specifies the color of the burned-in captions. This option is not valid
-- for source captions that are STL, 608\/embedded or teletext. These
-- source settings are already pre-defined by the caption stream. All
-- burn-in and DVB-Sub font settings must match.
burnInDestinationSettings_fontColor :: Lens.Lens' BurnInDestinationSettings (Prelude.Maybe BurnInFontColor)
burnInDestinationSettings_fontColor = Lens.lens (\BurnInDestinationSettings' {fontColor} -> fontColor) (\s@BurnInDestinationSettings' {} a -> s {fontColor = a} :: BurnInDestinationSettings)

instance Core.FromJSON BurnInDestinationSettings where
  parseJSON =
    Core.withObject
      "BurnInDestinationSettings"
      ( \x ->
          BurnInDestinationSettings'
            Prelude.<$> (x Core..:? "backgroundOpacity")
            Prelude.<*> (x Core..:? "fontOpacity")
            Prelude.<*> (x Core..:? "shadowYOffset")
            Prelude.<*> (x Core..:? "fontResolution")
            Prelude.<*> (x Core..:? "yPosition")
            Prelude.<*> (x Core..:? "backgroundColor")
            Prelude.<*> (x Core..:? "shadowXOffset")
            Prelude.<*> (x Core..:? "fontSize")
            Prelude.<*> (x Core..:? "xPosition")
            Prelude.<*> (x Core..:? "alignment")
            Prelude.<*> (x Core..:? "shadowOpacity")
            Prelude.<*> (x Core..:? "teletextGridControl")
            Prelude.<*> (x Core..:? "outlineColor")
            Prelude.<*> (x Core..:? "outlineSize")
            Prelude.<*> (x Core..:? "font")
            Prelude.<*> (x Core..:? "shadowColor")
            Prelude.<*> (x Core..:? "fontColor")
      )

instance Prelude.Hashable BurnInDestinationSettings

instance Prelude.NFData BurnInDestinationSettings

instance Core.ToJSON BurnInDestinationSettings where
  toJSON BurnInDestinationSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("backgroundOpacity" Core..=)
              Prelude.<$> backgroundOpacity,
            ("fontOpacity" Core..=) Prelude.<$> fontOpacity,
            ("shadowYOffset" Core..=) Prelude.<$> shadowYOffset,
            ("fontResolution" Core..=)
              Prelude.<$> fontResolution,
            ("yPosition" Core..=) Prelude.<$> yPosition,
            ("backgroundColor" Core..=)
              Prelude.<$> backgroundColor,
            ("shadowXOffset" Core..=) Prelude.<$> shadowXOffset,
            ("fontSize" Core..=) Prelude.<$> fontSize,
            ("xPosition" Core..=) Prelude.<$> xPosition,
            ("alignment" Core..=) Prelude.<$> alignment,
            ("shadowOpacity" Core..=) Prelude.<$> shadowOpacity,
            ("teletextGridControl" Core..=)
              Prelude.<$> teletextGridControl,
            ("outlineColor" Core..=) Prelude.<$> outlineColor,
            ("outlineSize" Core..=) Prelude.<$> outlineSize,
            ("font" Core..=) Prelude.<$> font,
            ("shadowColor" Core..=) Prelude.<$> shadowColor,
            ("fontColor" Core..=) Prelude.<$> fontColor
          ]
      )
