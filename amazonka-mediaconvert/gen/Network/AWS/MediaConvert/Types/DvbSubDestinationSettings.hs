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
-- Module      : Network.AWS.MediaConvert.Types.DvbSubDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubDestinationSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.DvbSubtitleAlignment
import Network.AWS.MediaConvert.Types.DvbSubtitleBackgroundColor
import Network.AWS.MediaConvert.Types.DvbSubtitleFontColor
import Network.AWS.MediaConvert.Types.DvbSubtitleOutlineColor
import Network.AWS.MediaConvert.Types.DvbSubtitleShadowColor
import Network.AWS.MediaConvert.Types.DvbSubtitleTeletextSpacing
import Network.AWS.MediaConvert.Types.DvbSubtitlingType
import Network.AWS.MediaConvert.Types.FontScript
import qualified Network.AWS.Prelude as Prelude

-- | DVB-Sub Destination Settings
--
-- /See:/ 'newDvbSubDestinationSettings' smart constructor.
data DvbSubDestinationSettings = DvbSubDestinationSettings'
  { -- | If no explicit x_position or y_position is provided, setting alignment
    -- to centered will place the captions at the bottom center of the output.
    -- Similarly, setting a left alignment will align captions to the bottom
    -- left of the output. If x and y positions are given in conjunction with
    -- the alignment parameter, the font will be justified (either left or
    -- centered) relative to those coordinates. This option is not valid for
    -- source captions that are STL, 608\/embedded or teletext. These source
    -- settings are already pre-defined by the caption stream. All burn-in and
    -- DVB-Sub font settings must match.
    alignment :: Prelude.Maybe DvbSubtitleAlignment,
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
    teletextSpacing :: Prelude.Maybe DvbSubtitleTeletextSpacing,
    -- | Specifies the color of the shadow cast by the captions. All burn-in and
    -- DVB-Sub font settings must match.
    shadowColor :: Prelude.Maybe DvbSubtitleShadowColor,
    -- | Specifies font outline color. This option is not valid for source
    -- captions that are either 608\/embedded or teletext. These source
    -- settings are already pre-defined by the caption stream. All burn-in and
    -- DVB-Sub font settings must match.
    outlineColor :: Prelude.Maybe DvbSubtitleOutlineColor,
    -- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is
    -- transparent. Leaving this parameter blank is equivalent to setting it to
    -- 0 (transparent). All burn-in and DVB-Sub font settings must match.
    backgroundOpacity :: Prelude.Maybe Prelude.Natural,
    -- | Provide the font script, using an ISO 15924 script code, if the
    -- LanguageCode is not sufficient for determining the script type. Where
    -- LanguageCode or CustomLanguageCode is sufficient, use \"AUTOMATIC\" or
    -- leave unset. This is used to help determine the appropriate font for
    -- rendering DVB-Sub captions.
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
    -- | Specify whether your DVB subtitles are standard or for hearing impaired.
    -- Choose hearing impaired if your subtitles include audio descriptions and
    -- dialogue. Choose standard if your subtitles include only dialogue.
    subtitlingType :: Prelude.Maybe DvbSubtitlingType,
    -- | Specifies the color of the burned-in captions. This option is not valid
    -- for source captions that are STL, 608\/embedded or teletext. These
    -- source settings are already pre-defined by the caption stream. All
    -- burn-in and DVB-Sub font settings must match.
    fontColor :: Prelude.Maybe DvbSubtitleFontColor,
    -- | A positive integer indicates the exact font size in points. Set to 0 for
    -- automatic font size selection. All burn-in and DVB-Sub font settings
    -- must match.
    fontSize :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the color of the rectangle behind the captions. All burn-in
    -- and DVB-Sub font settings must match.
    backgroundColor :: Prelude.Maybe DvbSubtitleBackgroundColor,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DvbSubDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alignment', 'dvbSubDestinationSettings_alignment' - If no explicit x_position or y_position is provided, setting alignment
-- to centered will place the captions at the bottom center of the output.
-- Similarly, setting a left alignment will align captions to the bottom
-- left of the output. If x and y positions are given in conjunction with
-- the alignment parameter, the font will be justified (either left or
-- centered) relative to those coordinates. This option is not valid for
-- source captions that are STL, 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
--
-- 'shadowOpacity', 'dvbSubDestinationSettings_shadowOpacity' - Specifies the opacity of the shadow. 255 is opaque; 0 is transparent.
-- Leaving this parameter blank is equivalent to setting it to 0
-- (transparent). All burn-in and DVB-Sub font settings must match.
--
-- 'teletextSpacing', 'dvbSubDestinationSettings_teletextSpacing' - Only applies to jobs with input captions in Teletext or STL formats.
-- Specify whether the spacing between letters in your captions is set by
-- the captions grid or varies depending on letter width. Choose fixed grid
-- to conform to the spacing specified in the captions file more
-- accurately. Choose proportional to make the text easier to read if the
-- captions are closed caption.
--
-- 'shadowColor', 'dvbSubDestinationSettings_shadowColor' - Specifies the color of the shadow cast by the captions. All burn-in and
-- DVB-Sub font settings must match.
--
-- 'outlineColor', 'dvbSubDestinationSettings_outlineColor' - Specifies font outline color. This option is not valid for source
-- captions that are either 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
--
-- 'backgroundOpacity', 'dvbSubDestinationSettings_backgroundOpacity' - Specifies the opacity of the background rectangle. 255 is opaque; 0 is
-- transparent. Leaving this parameter blank is equivalent to setting it to
-- 0 (transparent). All burn-in and DVB-Sub font settings must match.
--
-- 'fontScript', 'dvbSubDestinationSettings_fontScript' - Provide the font script, using an ISO 15924 script code, if the
-- LanguageCode is not sufficient for determining the script type. Where
-- LanguageCode or CustomLanguageCode is sufficient, use \"AUTOMATIC\" or
-- leave unset. This is used to help determine the appropriate font for
-- rendering DVB-Sub captions.
--
-- 'xPosition', 'dvbSubDestinationSettings_xPosition' - Specifies the horizontal position of the caption relative to the left
-- side of the output in pixels. A value of 10 would result in the captions
-- starting 10 pixels from the left of the output. If no explicit
-- x_position is provided, the horizontal caption position will be
-- determined by the alignment parameter. This option is not valid for
-- source captions that are STL, 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
--
-- 'subtitlingType', 'dvbSubDestinationSettings_subtitlingType' - Specify whether your DVB subtitles are standard or for hearing impaired.
-- Choose hearing impaired if your subtitles include audio descriptions and
-- dialogue. Choose standard if your subtitles include only dialogue.
--
-- 'fontColor', 'dvbSubDestinationSettings_fontColor' - Specifies the color of the burned-in captions. This option is not valid
-- for source captions that are STL, 608\/embedded or teletext. These
-- source settings are already pre-defined by the caption stream. All
-- burn-in and DVB-Sub font settings must match.
--
-- 'fontSize', 'dvbSubDestinationSettings_fontSize' - A positive integer indicates the exact font size in points. Set to 0 for
-- automatic font size selection. All burn-in and DVB-Sub font settings
-- must match.
--
-- 'backgroundColor', 'dvbSubDestinationSettings_backgroundColor' - Specifies the color of the rectangle behind the captions. All burn-in
-- and DVB-Sub font settings must match.
--
-- 'shadowXOffset', 'dvbSubDestinationSettings_shadowXOffset' - Specifies the horizontal offset of the shadow relative to the captions
-- in pixels. A value of -2 would result in a shadow offset 2 pixels to the
-- left. All burn-in and DVB-Sub font settings must match.
--
-- 'yPosition', 'dvbSubDestinationSettings_yPosition' - Specifies the vertical position of the caption relative to the top of
-- the output in pixels. A value of 10 would result in the captions
-- starting 10 pixels from the top of the output. If no explicit y_position
-- is provided, the caption will be positioned towards the bottom of the
-- output. This option is not valid for source captions that are STL,
-- 608\/embedded or teletext. These source settings are already pre-defined
-- by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- 'fontResolution', 'dvbSubDestinationSettings_fontResolution' - Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in
-- and DVB-Sub font settings must match.
--
-- 'outlineSize', 'dvbSubDestinationSettings_outlineSize' - Specifies font outline size in pixels. This option is not valid for
-- source captions that are either 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
--
-- 'fontOpacity', 'dvbSubDestinationSettings_fontOpacity' - Specifies the opacity of the burned-in captions. 255 is opaque; 0 is
-- transparent. All burn-in and DVB-Sub font settings must match.
--
-- 'shadowYOffset', 'dvbSubDestinationSettings_shadowYOffset' - Specifies the vertical offset of the shadow relative to the captions in
-- pixels. A value of -2 would result in a shadow offset 2 pixels above the
-- text. All burn-in and DVB-Sub font settings must match.
newDvbSubDestinationSettings ::
  DvbSubDestinationSettings
newDvbSubDestinationSettings =
  DvbSubDestinationSettings'
    { alignment =
        Prelude.Nothing,
      shadowOpacity = Prelude.Nothing,
      teletextSpacing = Prelude.Nothing,
      shadowColor = Prelude.Nothing,
      outlineColor = Prelude.Nothing,
      backgroundOpacity = Prelude.Nothing,
      fontScript = Prelude.Nothing,
      xPosition = Prelude.Nothing,
      subtitlingType = Prelude.Nothing,
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
dvbSubDestinationSettings_alignment :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubtitleAlignment)
dvbSubDestinationSettings_alignment = Lens.lens (\DvbSubDestinationSettings' {alignment} -> alignment) (\s@DvbSubDestinationSettings' {} a -> s {alignment = a} :: DvbSubDestinationSettings)

-- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent.
-- Leaving this parameter blank is equivalent to setting it to 0
-- (transparent). All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_shadowOpacity :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_shadowOpacity = Lens.lens (\DvbSubDestinationSettings' {shadowOpacity} -> shadowOpacity) (\s@DvbSubDestinationSettings' {} a -> s {shadowOpacity = a} :: DvbSubDestinationSettings)

-- | Only applies to jobs with input captions in Teletext or STL formats.
-- Specify whether the spacing between letters in your captions is set by
-- the captions grid or varies depending on letter width. Choose fixed grid
-- to conform to the spacing specified in the captions file more
-- accurately. Choose proportional to make the text easier to read if the
-- captions are closed caption.
dvbSubDestinationSettings_teletextSpacing :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubtitleTeletextSpacing)
dvbSubDestinationSettings_teletextSpacing = Lens.lens (\DvbSubDestinationSettings' {teletextSpacing} -> teletextSpacing) (\s@DvbSubDestinationSettings' {} a -> s {teletextSpacing = a} :: DvbSubDestinationSettings)

-- | Specifies the color of the shadow cast by the captions. All burn-in and
-- DVB-Sub font settings must match.
dvbSubDestinationSettings_shadowColor :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubtitleShadowColor)
dvbSubDestinationSettings_shadowColor = Lens.lens (\DvbSubDestinationSettings' {shadowColor} -> shadowColor) (\s@DvbSubDestinationSettings' {} a -> s {shadowColor = a} :: DvbSubDestinationSettings)

-- | Specifies font outline color. This option is not valid for source
-- captions that are either 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
dvbSubDestinationSettings_outlineColor :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubtitleOutlineColor)
dvbSubDestinationSettings_outlineColor = Lens.lens (\DvbSubDestinationSettings' {outlineColor} -> outlineColor) (\s@DvbSubDestinationSettings' {} a -> s {outlineColor = a} :: DvbSubDestinationSettings)

-- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is
-- transparent. Leaving this parameter blank is equivalent to setting it to
-- 0 (transparent). All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_backgroundOpacity :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_backgroundOpacity = Lens.lens (\DvbSubDestinationSettings' {backgroundOpacity} -> backgroundOpacity) (\s@DvbSubDestinationSettings' {} a -> s {backgroundOpacity = a} :: DvbSubDestinationSettings)

-- | Provide the font script, using an ISO 15924 script code, if the
-- LanguageCode is not sufficient for determining the script type. Where
-- LanguageCode or CustomLanguageCode is sufficient, use \"AUTOMATIC\" or
-- leave unset. This is used to help determine the appropriate font for
-- rendering DVB-Sub captions.
dvbSubDestinationSettings_fontScript :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe FontScript)
dvbSubDestinationSettings_fontScript = Lens.lens (\DvbSubDestinationSettings' {fontScript} -> fontScript) (\s@DvbSubDestinationSettings' {} a -> s {fontScript = a} :: DvbSubDestinationSettings)

-- | Specifies the horizontal position of the caption relative to the left
-- side of the output in pixels. A value of 10 would result in the captions
-- starting 10 pixels from the left of the output. If no explicit
-- x_position is provided, the horizontal caption position will be
-- determined by the alignment parameter. This option is not valid for
-- source captions that are STL, 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
dvbSubDestinationSettings_xPosition :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_xPosition = Lens.lens (\DvbSubDestinationSettings' {xPosition} -> xPosition) (\s@DvbSubDestinationSettings' {} a -> s {xPosition = a} :: DvbSubDestinationSettings)

-- | Specify whether your DVB subtitles are standard or for hearing impaired.
-- Choose hearing impaired if your subtitles include audio descriptions and
-- dialogue. Choose standard if your subtitles include only dialogue.
dvbSubDestinationSettings_subtitlingType :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubtitlingType)
dvbSubDestinationSettings_subtitlingType = Lens.lens (\DvbSubDestinationSettings' {subtitlingType} -> subtitlingType) (\s@DvbSubDestinationSettings' {} a -> s {subtitlingType = a} :: DvbSubDestinationSettings)

-- | Specifies the color of the burned-in captions. This option is not valid
-- for source captions that are STL, 608\/embedded or teletext. These
-- source settings are already pre-defined by the caption stream. All
-- burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_fontColor :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubtitleFontColor)
dvbSubDestinationSettings_fontColor = Lens.lens (\DvbSubDestinationSettings' {fontColor} -> fontColor) (\s@DvbSubDestinationSettings' {} a -> s {fontColor = a} :: DvbSubDestinationSettings)

-- | A positive integer indicates the exact font size in points. Set to 0 for
-- automatic font size selection. All burn-in and DVB-Sub font settings
-- must match.
dvbSubDestinationSettings_fontSize :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_fontSize = Lens.lens (\DvbSubDestinationSettings' {fontSize} -> fontSize) (\s@DvbSubDestinationSettings' {} a -> s {fontSize = a} :: DvbSubDestinationSettings)

-- | Specifies the color of the rectangle behind the captions. All burn-in
-- and DVB-Sub font settings must match.
dvbSubDestinationSettings_backgroundColor :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubtitleBackgroundColor)
dvbSubDestinationSettings_backgroundColor = Lens.lens (\DvbSubDestinationSettings' {backgroundColor} -> backgroundColor) (\s@DvbSubDestinationSettings' {} a -> s {backgroundColor = a} :: DvbSubDestinationSettings)

-- | Specifies the horizontal offset of the shadow relative to the captions
-- in pixels. A value of -2 would result in a shadow offset 2 pixels to the
-- left. All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_shadowXOffset :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Int)
dvbSubDestinationSettings_shadowXOffset = Lens.lens (\DvbSubDestinationSettings' {shadowXOffset} -> shadowXOffset) (\s@DvbSubDestinationSettings' {} a -> s {shadowXOffset = a} :: DvbSubDestinationSettings)

-- | Specifies the vertical position of the caption relative to the top of
-- the output in pixels. A value of 10 would result in the captions
-- starting 10 pixels from the top of the output. If no explicit y_position
-- is provided, the caption will be positioned towards the bottom of the
-- output. This option is not valid for source captions that are STL,
-- 608\/embedded or teletext. These source settings are already pre-defined
-- by the caption stream. All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_yPosition :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_yPosition = Lens.lens (\DvbSubDestinationSettings' {yPosition} -> yPosition) (\s@DvbSubDestinationSettings' {} a -> s {yPosition = a} :: DvbSubDestinationSettings)

-- | Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in
-- and DVB-Sub font settings must match.
dvbSubDestinationSettings_fontResolution :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_fontResolution = Lens.lens (\DvbSubDestinationSettings' {fontResolution} -> fontResolution) (\s@DvbSubDestinationSettings' {} a -> s {fontResolution = a} :: DvbSubDestinationSettings)

-- | Specifies font outline size in pixels. This option is not valid for
-- source captions that are either 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
dvbSubDestinationSettings_outlineSize :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_outlineSize = Lens.lens (\DvbSubDestinationSettings' {outlineSize} -> outlineSize) (\s@DvbSubDestinationSettings' {} a -> s {outlineSize = a} :: DvbSubDestinationSettings)

-- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is
-- transparent. All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_fontOpacity :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_fontOpacity = Lens.lens (\DvbSubDestinationSettings' {fontOpacity} -> fontOpacity) (\s@DvbSubDestinationSettings' {} a -> s {fontOpacity = a} :: DvbSubDestinationSettings)

-- | Specifies the vertical offset of the shadow relative to the captions in
-- pixels. A value of -2 would result in a shadow offset 2 pixels above the
-- text. All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_shadowYOffset :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Int)
dvbSubDestinationSettings_shadowYOffset = Lens.lens (\DvbSubDestinationSettings' {shadowYOffset} -> shadowYOffset) (\s@DvbSubDestinationSettings' {} a -> s {shadowYOffset = a} :: DvbSubDestinationSettings)

instance Core.FromJSON DvbSubDestinationSettings where
  parseJSON =
    Core.withObject
      "DvbSubDestinationSettings"
      ( \x ->
          DvbSubDestinationSettings'
            Prelude.<$> (x Core..:? "alignment")
            Prelude.<*> (x Core..:? "shadowOpacity")
            Prelude.<*> (x Core..:? "teletextSpacing")
            Prelude.<*> (x Core..:? "shadowColor")
            Prelude.<*> (x Core..:? "outlineColor")
            Prelude.<*> (x Core..:? "backgroundOpacity")
            Prelude.<*> (x Core..:? "fontScript")
            Prelude.<*> (x Core..:? "xPosition")
            Prelude.<*> (x Core..:? "subtitlingType")
            Prelude.<*> (x Core..:? "fontColor")
            Prelude.<*> (x Core..:? "fontSize")
            Prelude.<*> (x Core..:? "backgroundColor")
            Prelude.<*> (x Core..:? "shadowXOffset")
            Prelude.<*> (x Core..:? "yPosition")
            Prelude.<*> (x Core..:? "fontResolution")
            Prelude.<*> (x Core..:? "outlineSize")
            Prelude.<*> (x Core..:? "fontOpacity")
            Prelude.<*> (x Core..:? "shadowYOffset")
      )

instance Prelude.Hashable DvbSubDestinationSettings

instance Prelude.NFData DvbSubDestinationSettings

instance Core.ToJSON DvbSubDestinationSettings where
  toJSON DvbSubDestinationSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("alignment" Core..=) Prelude.<$> alignment,
            ("shadowOpacity" Core..=) Prelude.<$> shadowOpacity,
            ("teletextSpacing" Core..=)
              Prelude.<$> teletextSpacing,
            ("shadowColor" Core..=) Prelude.<$> shadowColor,
            ("outlineColor" Core..=) Prelude.<$> outlineColor,
            ("backgroundOpacity" Core..=)
              Prelude.<$> backgroundOpacity,
            ("fontScript" Core..=) Prelude.<$> fontScript,
            ("xPosition" Core..=) Prelude.<$> xPosition,
            ("subtitlingType" Core..=)
              Prelude.<$> subtitlingType,
            ("fontColor" Core..=) Prelude.<$> fontColor,
            ("fontSize" Core..=) Prelude.<$> fontSize,
            ("backgroundColor" Core..=)
              Prelude.<$> backgroundColor,
            ("shadowXOffset" Core..=) Prelude.<$> shadowXOffset,
            ("yPosition" Core..=) Prelude.<$> yPosition,
            ("fontResolution" Core..=)
              Prelude.<$> fontResolution,
            ("outlineSize" Core..=) Prelude.<$> outlineSize,
            ("fontOpacity" Core..=) Prelude.<$> fontOpacity,
            ("shadowYOffset" Core..=) Prelude.<$> shadowYOffset
          ]
      )
