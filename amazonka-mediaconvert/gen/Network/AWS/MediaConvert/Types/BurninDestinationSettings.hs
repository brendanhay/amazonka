{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaConvert.Types.BurninDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.BurninDestinationSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.BurninSubtitleAlignment
import Network.AWS.MediaConvert.Types.BurninSubtitleBackgroundColor
import Network.AWS.MediaConvert.Types.BurninSubtitleFontColor
import Network.AWS.MediaConvert.Types.BurninSubtitleOutlineColor
import Network.AWS.MediaConvert.Types.BurninSubtitleShadowColor
import Network.AWS.MediaConvert.Types.BurninSubtitleTeletextSpacing
import Network.AWS.MediaConvert.Types.FontScript
import qualified Network.AWS.Prelude as Prelude

-- | Burn-In Destination Settings.
--
-- /See:/ 'newBurninDestinationSettings' smart constructor.
data BurninDestinationSettings = BurninDestinationSettings'
  { -- | If no explicit x_position or y_position is provided, setting alignment
    -- to centered will place the captions at the bottom center of the output.
    -- Similarly, setting a left alignment will align captions to the bottom
    -- left of the output. If x and y positions are given in conjunction with
    -- the alignment parameter, the font will be justified (either left or
    -- centered) relative to those coordinates. This option is not valid for
    -- source captions that are STL, 608\/embedded or teletext. These source
    -- settings are already pre-defined by the caption stream. All burn-in and
    -- DVB-Sub font settings must match.
    alignment :: Prelude.Maybe BurninSubtitleAlignment,
    -- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent.
    -- Leaving this parameter blank is equivalent to setting it to 0
    -- (transparent). All burn-in and DVB-Sub font settings must match.
    shadowOpacity :: Prelude.Maybe Prelude.Natural,
    -- | Only applies to jobs with input captions in Teletext or STL formats.
    -- Specify whether the spacing between letters in your captions is set by
    -- the captions grid or varies depending on letter width. Choose fixed grid
    -- to conform to the spacing specified in the captions file more
    -- accurately. Choose proportional to make the text easier to read if the
    -- captions are closed caption.
    teletextSpacing :: Prelude.Maybe BurninSubtitleTeletextSpacing,
    -- | Specifies the color of the shadow cast by the captions. All burn-in and
    -- DVB-Sub font settings must match.
    shadowColor :: Prelude.Maybe BurninSubtitleShadowColor,
    -- | Specifies font outline color. This option is not valid for source
    -- captions that are either 608\/embedded or teletext. These source
    -- settings are already pre-defined by the caption stream. All burn-in and
    -- DVB-Sub font settings must match.
    outlineColor :: Prelude.Maybe BurninSubtitleOutlineColor,
    -- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is
    -- transparent. Leaving this parameter blank is equivalent to setting it to
    -- 0 (transparent). All burn-in and DVB-Sub font settings must match.
    backgroundOpacity :: Prelude.Maybe Prelude.Natural,
    -- | Provide the font script, using an ISO 15924 script code, if the
    -- LanguageCode is not sufficient for determining the script type. Where
    -- LanguageCode or CustomLanguageCode is sufficient, use \"AUTOMATIC\" or
    -- leave unset. This is used to help determine the appropriate font for
    -- rendering burn-in captions.
    fontScript :: Prelude.Maybe FontScript,
    -- | Specifies the horizontal position of the caption relative to the left
    -- side of the output in pixels. A value of 10 would result in the captions
    -- starting 10 pixels from the left of the output. If no explicit
    -- x_position is provided, the horizontal caption position will be
    -- determined by the alignment parameter. This option is not valid for
    -- source captions that are STL, 608\/embedded or teletext. These source
    -- settings are already pre-defined by the caption stream. All burn-in and
    -- DVB-Sub font settings must match.
    xPosition :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the color of the burned-in captions. This option is not valid
    -- for source captions that are STL, 608\/embedded or teletext. These
    -- source settings are already pre-defined by the caption stream. All
    -- burn-in and DVB-Sub font settings must match.
    fontColor :: Prelude.Maybe BurninSubtitleFontColor,
    -- | A positive integer indicates the exact font size in points. Set to 0 for
    -- automatic font size selection. All burn-in and DVB-Sub font settings
    -- must match.
    fontSize :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the color of the rectangle behind the captions. All burn-in
    -- and DVB-Sub font settings must match.
    backgroundColor :: Prelude.Maybe BurninSubtitleBackgroundColor,
    -- | Specifies the horizontal offset of the shadow relative to the captions
    -- in pixels. A value of -2 would result in a shadow offset 2 pixels to the
    -- left. All burn-in and DVB-Sub font settings must match.
    shadowXOffset :: Prelude.Maybe Prelude.Int,
    -- | Specifies the vertical position of the caption relative to the top of
    -- the output in pixels. A value of 10 would result in the captions
    -- starting 10 pixels from the top of the output. If no explicit y_position
    -- is provided, the caption will be positioned towards the bottom of the
    -- output. This option is not valid for source captions that are STL,
    -- 608\/embedded or teletext. These source settings are already pre-defined
    -- by the caption stream. All burn-in and DVB-Sub font settings must match.
    yPosition :: Prelude.Maybe Prelude.Natural,
    -- | Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in
    -- and DVB-Sub font settings must match.
    fontResolution :: Prelude.Maybe Prelude.Natural,
    -- | Specifies font outline size in pixels. This option is not valid for
    -- source captions that are either 608\/embedded or teletext. These source
    -- settings are already pre-defined by the caption stream. All burn-in and
    -- DVB-Sub font settings must match.
    outlineSize :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is
    -- transparent. All burn-in and DVB-Sub font settings must match.
    fontOpacity :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the vertical offset of the shadow relative to the captions in
    -- pixels. A value of -2 would result in a shadow offset 2 pixels above the
    -- text. All burn-in and DVB-Sub font settings must match.
    shadowYOffset :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BurninDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alignment', 'burninDestinationSettings_alignment' - If no explicit x_position or y_position is provided, setting alignment
-- to centered will place the captions at the bottom center of the output.
-- Similarly, setting a left alignment will align captions to the bottom
-- left of the output. If x and y positions are given in conjunction with
-- the alignment parameter, the font will be justified (either left or
-- centered) relative to those coordinates. This option is not valid for
-- source captions that are STL, 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
--
-- 'shadowOpacity', 'burninDestinationSettings_shadowOpacity' - Specifies the opacity of the shadow. 255 is opaque; 0 is transparent.
-- Leaving this parameter blank is equivalent to setting it to 0
-- (transparent). All burn-in and DVB-Sub font settings must match.
--
-- 'teletextSpacing', 'burninDestinationSettings_teletextSpacing' - Only applies to jobs with input captions in Teletext or STL formats.
-- Specify whether the spacing between letters in your captions is set by
-- the captions grid or varies depending on letter width. Choose fixed grid
-- to conform to the spacing specified in the captions file more
-- accurately. Choose proportional to make the text easier to read if the
-- captions are closed caption.
--
-- 'shadowColor', 'burninDestinationSettings_shadowColor' - Specifies the color of the shadow cast by the captions. All burn-in and
-- DVB-Sub font settings must match.
--
-- 'outlineColor', 'burninDestinationSettings_outlineColor' - Specifies font outline color. This option is not valid for source
-- captions that are either 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
--
-- 'backgroundOpacity', 'burninDestinationSettings_backgroundOpacity' - Specifies the opacity of the background rectangle. 255 is opaque; 0 is
-- transparent. Leaving this parameter blank is equivalent to setting it to
-- 0 (transparent). All burn-in and DVB-Sub font settings must match.
--
-- 'fontScript', 'burninDestinationSettings_fontScript' - Provide the font script, using an ISO 15924 script code, if the
-- LanguageCode is not sufficient for determining the script type. Where
-- LanguageCode or CustomLanguageCode is sufficient, use \"AUTOMATIC\" or
-- leave unset. This is used to help determine the appropriate font for
-- rendering burn-in captions.
--
-- 'xPosition', 'burninDestinationSettings_xPosition' - Specifies the horizontal position of the caption relative to the left
-- side of the output in pixels. A value of 10 would result in the captions
-- starting 10 pixels from the left of the output. If no explicit
-- x_position is provided, the horizontal caption position will be
-- determined by the alignment parameter. This option is not valid for
-- source captions that are STL, 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
--
-- 'fontColor', 'burninDestinationSettings_fontColor' - Specifies the color of the burned-in captions. This option is not valid
-- for source captions that are STL, 608\/embedded or teletext. These
-- source settings are already pre-defined by the caption stream. All
-- burn-in and DVB-Sub font settings must match.
--
-- 'fontSize', 'burninDestinationSettings_fontSize' - A positive integer indicates the exact font size in points. Set to 0 for
-- automatic font size selection. All burn-in and DVB-Sub font settings
-- must match.
--
-- 'backgroundColor', 'burninDestinationSettings_backgroundColor' - Specifies the color of the rectangle behind the captions. All burn-in
-- and DVB-Sub font settings must match.
--
-- 'shadowXOffset', 'burninDestinationSettings_shadowXOffset' - Specifies the horizontal offset of the shadow relative to the captions
-- in pixels. A value of -2 would result in a shadow offset 2 pixels to the
-- left. All burn-in and DVB-Sub font settings must match.
--
-- 'yPosition', 'burninDestinationSettings_yPosition' - Specifies the vertical position of the caption relative to the top of
-- the output in pixels. A value of 10 would result in the captions
-- starting 10 pixels from the top of the output. If no explicit y_position
-- is provided, the caption will be positioned towards the bottom of the
-- output. This option is not valid for source captions that are STL,
-- 608\/embedded or teletext. These source settings are already pre-defined
-- by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- 'fontResolution', 'burninDestinationSettings_fontResolution' - Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in
-- and DVB-Sub font settings must match.
--
-- 'outlineSize', 'burninDestinationSettings_outlineSize' - Specifies font outline size in pixels. This option is not valid for
-- source captions that are either 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
--
-- 'fontOpacity', 'burninDestinationSettings_fontOpacity' - Specifies the opacity of the burned-in captions. 255 is opaque; 0 is
-- transparent. All burn-in and DVB-Sub font settings must match.
--
-- 'shadowYOffset', 'burninDestinationSettings_shadowYOffset' - Specifies the vertical offset of the shadow relative to the captions in
-- pixels. A value of -2 would result in a shadow offset 2 pixels above the
-- text. All burn-in and DVB-Sub font settings must match.
newBurninDestinationSettings ::
  BurninDestinationSettings
newBurninDestinationSettings =
  BurninDestinationSettings'
    { alignment =
        Prelude.Nothing,
      shadowOpacity = Prelude.Nothing,
      teletextSpacing = Prelude.Nothing,
      shadowColor = Prelude.Nothing,
      outlineColor = Prelude.Nothing,
      backgroundOpacity = Prelude.Nothing,
      fontScript = Prelude.Nothing,
      xPosition = Prelude.Nothing,
      fontColor = Prelude.Nothing,
      fontSize = Prelude.Nothing,
      backgroundColor = Prelude.Nothing,
      shadowXOffset = Prelude.Nothing,
      yPosition = Prelude.Nothing,
      fontResolution = Prelude.Nothing,
      outlineSize = Prelude.Nothing,
      fontOpacity = Prelude.Nothing,
      shadowYOffset = Prelude.Nothing
    }

-- | If no explicit x_position or y_position is provided, setting alignment
-- to centered will place the captions at the bottom center of the output.
-- Similarly, setting a left alignment will align captions to the bottom
-- left of the output. If x and y positions are given in conjunction with
-- the alignment parameter, the font will be justified (either left or
-- centered) relative to those coordinates. This option is not valid for
-- source captions that are STL, 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
burninDestinationSettings_alignment :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe BurninSubtitleAlignment)
burninDestinationSettings_alignment = Lens.lens (\BurninDestinationSettings' {alignment} -> alignment) (\s@BurninDestinationSettings' {} a -> s {alignment = a} :: BurninDestinationSettings)

-- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent.
-- Leaving this parameter blank is equivalent to setting it to 0
-- (transparent). All burn-in and DVB-Sub font settings must match.
burninDestinationSettings_shadowOpacity :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe Prelude.Natural)
burninDestinationSettings_shadowOpacity = Lens.lens (\BurninDestinationSettings' {shadowOpacity} -> shadowOpacity) (\s@BurninDestinationSettings' {} a -> s {shadowOpacity = a} :: BurninDestinationSettings)

-- | Only applies to jobs with input captions in Teletext or STL formats.
-- Specify whether the spacing between letters in your captions is set by
-- the captions grid or varies depending on letter width. Choose fixed grid
-- to conform to the spacing specified in the captions file more
-- accurately. Choose proportional to make the text easier to read if the
-- captions are closed caption.
burninDestinationSettings_teletextSpacing :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe BurninSubtitleTeletextSpacing)
burninDestinationSettings_teletextSpacing = Lens.lens (\BurninDestinationSettings' {teletextSpacing} -> teletextSpacing) (\s@BurninDestinationSettings' {} a -> s {teletextSpacing = a} :: BurninDestinationSettings)

-- | Specifies the color of the shadow cast by the captions. All burn-in and
-- DVB-Sub font settings must match.
burninDestinationSettings_shadowColor :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe BurninSubtitleShadowColor)
burninDestinationSettings_shadowColor = Lens.lens (\BurninDestinationSettings' {shadowColor} -> shadowColor) (\s@BurninDestinationSettings' {} a -> s {shadowColor = a} :: BurninDestinationSettings)

-- | Specifies font outline color. This option is not valid for source
-- captions that are either 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
burninDestinationSettings_outlineColor :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe BurninSubtitleOutlineColor)
burninDestinationSettings_outlineColor = Lens.lens (\BurninDestinationSettings' {outlineColor} -> outlineColor) (\s@BurninDestinationSettings' {} a -> s {outlineColor = a} :: BurninDestinationSettings)

-- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is
-- transparent. Leaving this parameter blank is equivalent to setting it to
-- 0 (transparent). All burn-in and DVB-Sub font settings must match.
burninDestinationSettings_backgroundOpacity :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe Prelude.Natural)
burninDestinationSettings_backgroundOpacity = Lens.lens (\BurninDestinationSettings' {backgroundOpacity} -> backgroundOpacity) (\s@BurninDestinationSettings' {} a -> s {backgroundOpacity = a} :: BurninDestinationSettings)

-- | Provide the font script, using an ISO 15924 script code, if the
-- LanguageCode is not sufficient for determining the script type. Where
-- LanguageCode or CustomLanguageCode is sufficient, use \"AUTOMATIC\" or
-- leave unset. This is used to help determine the appropriate font for
-- rendering burn-in captions.
burninDestinationSettings_fontScript :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe FontScript)
burninDestinationSettings_fontScript = Lens.lens (\BurninDestinationSettings' {fontScript} -> fontScript) (\s@BurninDestinationSettings' {} a -> s {fontScript = a} :: BurninDestinationSettings)

-- | Specifies the horizontal position of the caption relative to the left
-- side of the output in pixels. A value of 10 would result in the captions
-- starting 10 pixels from the left of the output. If no explicit
-- x_position is provided, the horizontal caption position will be
-- determined by the alignment parameter. This option is not valid for
-- source captions that are STL, 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
burninDestinationSettings_xPosition :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe Prelude.Natural)
burninDestinationSettings_xPosition = Lens.lens (\BurninDestinationSettings' {xPosition} -> xPosition) (\s@BurninDestinationSettings' {} a -> s {xPosition = a} :: BurninDestinationSettings)

-- | Specifies the color of the burned-in captions. This option is not valid
-- for source captions that are STL, 608\/embedded or teletext. These
-- source settings are already pre-defined by the caption stream. All
-- burn-in and DVB-Sub font settings must match.
burninDestinationSettings_fontColor :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe BurninSubtitleFontColor)
burninDestinationSettings_fontColor = Lens.lens (\BurninDestinationSettings' {fontColor} -> fontColor) (\s@BurninDestinationSettings' {} a -> s {fontColor = a} :: BurninDestinationSettings)

-- | A positive integer indicates the exact font size in points. Set to 0 for
-- automatic font size selection. All burn-in and DVB-Sub font settings
-- must match.
burninDestinationSettings_fontSize :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe Prelude.Natural)
burninDestinationSettings_fontSize = Lens.lens (\BurninDestinationSettings' {fontSize} -> fontSize) (\s@BurninDestinationSettings' {} a -> s {fontSize = a} :: BurninDestinationSettings)

-- | Specifies the color of the rectangle behind the captions. All burn-in
-- and DVB-Sub font settings must match.
burninDestinationSettings_backgroundColor :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe BurninSubtitleBackgroundColor)
burninDestinationSettings_backgroundColor = Lens.lens (\BurninDestinationSettings' {backgroundColor} -> backgroundColor) (\s@BurninDestinationSettings' {} a -> s {backgroundColor = a} :: BurninDestinationSettings)

-- | Specifies the horizontal offset of the shadow relative to the captions
-- in pixels. A value of -2 would result in a shadow offset 2 pixels to the
-- left. All burn-in and DVB-Sub font settings must match.
burninDestinationSettings_shadowXOffset :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe Prelude.Int)
burninDestinationSettings_shadowXOffset = Lens.lens (\BurninDestinationSettings' {shadowXOffset} -> shadowXOffset) (\s@BurninDestinationSettings' {} a -> s {shadowXOffset = a} :: BurninDestinationSettings)

-- | Specifies the vertical position of the caption relative to the top of
-- the output in pixels. A value of 10 would result in the captions
-- starting 10 pixels from the top of the output. If no explicit y_position
-- is provided, the caption will be positioned towards the bottom of the
-- output. This option is not valid for source captions that are STL,
-- 608\/embedded or teletext. These source settings are already pre-defined
-- by the caption stream. All burn-in and DVB-Sub font settings must match.
burninDestinationSettings_yPosition :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe Prelude.Natural)
burninDestinationSettings_yPosition = Lens.lens (\BurninDestinationSettings' {yPosition} -> yPosition) (\s@BurninDestinationSettings' {} a -> s {yPosition = a} :: BurninDestinationSettings)

-- | Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in
-- and DVB-Sub font settings must match.
burninDestinationSettings_fontResolution :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe Prelude.Natural)
burninDestinationSettings_fontResolution = Lens.lens (\BurninDestinationSettings' {fontResolution} -> fontResolution) (\s@BurninDestinationSettings' {} a -> s {fontResolution = a} :: BurninDestinationSettings)

-- | Specifies font outline size in pixels. This option is not valid for
-- source captions that are either 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
burninDestinationSettings_outlineSize :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe Prelude.Natural)
burninDestinationSettings_outlineSize = Lens.lens (\BurninDestinationSettings' {outlineSize} -> outlineSize) (\s@BurninDestinationSettings' {} a -> s {outlineSize = a} :: BurninDestinationSettings)

-- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is
-- transparent. All burn-in and DVB-Sub font settings must match.
burninDestinationSettings_fontOpacity :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe Prelude.Natural)
burninDestinationSettings_fontOpacity = Lens.lens (\BurninDestinationSettings' {fontOpacity} -> fontOpacity) (\s@BurninDestinationSettings' {} a -> s {fontOpacity = a} :: BurninDestinationSettings)

-- | Specifies the vertical offset of the shadow relative to the captions in
-- pixels. A value of -2 would result in a shadow offset 2 pixels above the
-- text. All burn-in and DVB-Sub font settings must match.
burninDestinationSettings_shadowYOffset :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe Prelude.Int)
burninDestinationSettings_shadowYOffset = Lens.lens (\BurninDestinationSettings' {shadowYOffset} -> shadowYOffset) (\s@BurninDestinationSettings' {} a -> s {shadowYOffset = a} :: BurninDestinationSettings)

instance Prelude.FromJSON BurninDestinationSettings where
  parseJSON =
    Prelude.withObject
      "BurninDestinationSettings"
      ( \x ->
          BurninDestinationSettings'
            Prelude.<$> (x Prelude..:? "alignment")
            Prelude.<*> (x Prelude..:? "shadowOpacity")
            Prelude.<*> (x Prelude..:? "teletextSpacing")
            Prelude.<*> (x Prelude..:? "shadowColor")
            Prelude.<*> (x Prelude..:? "outlineColor")
            Prelude.<*> (x Prelude..:? "backgroundOpacity")
            Prelude.<*> (x Prelude..:? "fontScript")
            Prelude.<*> (x Prelude..:? "xPosition")
            Prelude.<*> (x Prelude..:? "fontColor")
            Prelude.<*> (x Prelude..:? "fontSize")
            Prelude.<*> (x Prelude..:? "backgroundColor")
            Prelude.<*> (x Prelude..:? "shadowXOffset")
            Prelude.<*> (x Prelude..:? "yPosition")
            Prelude.<*> (x Prelude..:? "fontResolution")
            Prelude.<*> (x Prelude..:? "outlineSize")
            Prelude.<*> (x Prelude..:? "fontOpacity")
            Prelude.<*> (x Prelude..:? "shadowYOffset")
      )

instance Prelude.Hashable BurninDestinationSettings

instance Prelude.NFData BurninDestinationSettings

instance Prelude.ToJSON BurninDestinationSettings where
  toJSON BurninDestinationSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("alignment" Prelude..=) Prelude.<$> alignment,
            ("shadowOpacity" Prelude..=)
              Prelude.<$> shadowOpacity,
            ("teletextSpacing" Prelude..=)
              Prelude.<$> teletextSpacing,
            ("shadowColor" Prelude..=) Prelude.<$> shadowColor,
            ("outlineColor" Prelude..=) Prelude.<$> outlineColor,
            ("backgroundOpacity" Prelude..=)
              Prelude.<$> backgroundOpacity,
            ("fontScript" Prelude..=) Prelude.<$> fontScript,
            ("xPosition" Prelude..=) Prelude.<$> xPosition,
            ("fontColor" Prelude..=) Prelude.<$> fontColor,
            ("fontSize" Prelude..=) Prelude.<$> fontSize,
            ("backgroundColor" Prelude..=)
              Prelude.<$> backgroundColor,
            ("shadowXOffset" Prelude..=)
              Prelude.<$> shadowXOffset,
            ("yPosition" Prelude..=) Prelude.<$> yPosition,
            ("fontResolution" Prelude..=)
              Prelude.<$> fontResolution,
            ("outlineSize" Prelude..=) Prelude.<$> outlineSize,
            ("fontOpacity" Prelude..=) Prelude.<$> fontOpacity,
            ("shadowYOffset" Prelude..=)
              Prelude.<$> shadowYOffset
          ]
      )
