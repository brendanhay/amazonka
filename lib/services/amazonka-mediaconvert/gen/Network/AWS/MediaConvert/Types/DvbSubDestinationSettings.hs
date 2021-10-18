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
import Network.AWS.MediaConvert.Types.DvbSubSubtitleFallbackFont
import Network.AWS.MediaConvert.Types.DvbSubtitleAlignment
import Network.AWS.MediaConvert.Types.DvbSubtitleApplyFontColor
import Network.AWS.MediaConvert.Types.DvbSubtitleBackgroundColor
import Network.AWS.MediaConvert.Types.DvbSubtitleFontColor
import Network.AWS.MediaConvert.Types.DvbSubtitleOutlineColor
import Network.AWS.MediaConvert.Types.DvbSubtitleShadowColor
import Network.AWS.MediaConvert.Types.DvbSubtitleStylePassthrough
import Network.AWS.MediaConvert.Types.DvbSubtitleTeletextSpacing
import Network.AWS.MediaConvert.Types.DvbSubtitlingType
import Network.AWS.MediaConvert.Types.DvbddsHandling
import Network.AWS.MediaConvert.Types.FontScript
import qualified Network.AWS.Prelude as Prelude

-- | Settings related to DVB-Sub captions. Set up DVB-Sub captions in the
-- same output as your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/dvb-sub-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to
-- DVB_SUB.
--
-- /See:/ 'newDvbSubDestinationSettings' smart constructor.
data DvbSubDestinationSettings = DvbSubDestinationSettings'
  { -- | Specify the height, in pixels, of this set of DVB-Sub captions. The
    -- default value is 576 pixels. Related setting: When you use this setting,
    -- you must set DDS handling (ddsHandling) to a value other than None
    -- (NONE). All burn-in and DVB-Sub font settings must match.
    height :: Prelude.Maybe Prelude.Natural,
    -- | If no explicit x_position or y_position is provided, setting alignment
    -- to centered will place the captions at the bottom center of the output.
    -- Similarly, setting a left alignment will align captions to the bottom
    -- left of the output. If x and y positions are given in conjunction with
    -- the alignment parameter, the font will be justified (either left or
    -- centered) relative to those coordinates. This option is not valid for
    -- source captions that are STL, 608\/embedded or teletext. These source
    -- settings are already pre-defined by the caption stream. All burn-in and
    -- DVB-Sub font settings must match.
    alignment :: Prelude.Maybe DvbSubtitleAlignment,
    -- | Specify the font that you want the service to use for your burn in
    -- captions when your input captions specify a font that MediaConvert
    -- doesn\'t support. When you keep the default value, Best match
    -- (BEST_MATCH), MediaConvert uses a supported font that most closely
    -- matches the font that your input captions specify. When there are
    -- multiple unsupported fonts in your input captions, MediaConvert matches
    -- each font with the supported font that matches best. When you explicitly
    -- choose a replacement font, MediaConvert uses that font to replace all
    -- unsupported fonts from your input.
    fallbackFont :: Prelude.Maybe DvbSubSubtitleFallbackFont,
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
    -- | Use this setting, along with DDS y-coordinate (ddsYCoordinate), to
    -- specify the upper left corner of the display definition segment (DDS)
    -- display window. With this setting, specify the distance, in pixels,
    -- between the left side of the frame and the left side of the DDS display
    -- window. Keep the default value, 0, to have MediaConvert automatically
    -- choose this offset. Related setting: When you use this setting, you must
    -- set DDS handling (ddsHandling) to a value other than None (NONE).
    -- MediaConvert uses these values to determine whether to write page
    -- position data to the DDS or to the page composition segment (PCS). All
    -- burn-in and DVB-Sub font settings must match.
    ddsXCoordinate :: Prelude.Maybe Prelude.Natural,
    -- | Specify the width, in pixels, of this set of DVB-Sub captions. The
    -- default value is 720 pixels. Related setting: When you use this setting,
    -- you must set DDS handling (ddsHandling) to a value other than None
    -- (NONE). All burn-in and DVB-Sub font settings must match.
    width :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the color of the shadow cast by the captions. All burn-in and
    -- DVB-Sub font settings must match.
    shadowColor :: Prelude.Maybe DvbSubtitleShadowColor,
    -- | Use this setting, along with DDS x-coordinate (ddsXCoordinate), to
    -- specify the upper left corner of the display definition segment (DDS)
    -- display window. With this setting, specify the distance, in pixels,
    -- between the top of the frame and the top of the DDS display window. Keep
    -- the default value, 0, to have MediaConvert automatically choose this
    -- offset. Related setting: When you use this setting, you must set DDS
    -- handling (ddsHandling) to a value other than None (NONE). MediaConvert
    -- uses these values to determine whether to write page position data to
    -- the DDS or to the page composition segment (PCS). All burn-in and
    -- DVB-Sub font settings must match.
    ddsYCoordinate :: Prelude.Maybe Prelude.Natural,
    -- | Specifies font outline color. This option is not valid for source
    -- captions that are either 608\/embedded or teletext. These source
    -- settings are already pre-defined by the caption stream. All burn-in and
    -- DVB-Sub font settings must match.
    outlineColor :: Prelude.Maybe DvbSubtitleOutlineColor,
    -- | Choose which set of style and position values the service applies to
    -- your output captions. When you choose ENABLED, the service uses the
    -- input style and position information from your input. When you choose
    -- DISABLED, the service uses any style values that you specify in your
    -- output settings. If you don\'t specify values, the service uses default
    -- style and position values. When you choose DISABLED, the service ignores
    -- all style and position values from your input.
    stylePassthrough :: Prelude.Maybe DvbSubtitleStylePassthrough,
    -- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is
    -- transparent. Leaving this parameter blank is equivalent to setting it to
    -- 0 (transparent). All burn-in and DVB-Sub font settings must match.
    backgroundOpacity :: Prelude.Maybe Prelude.Natural,
    -- | Specify how MediaConvert handles the display definition segment (DDS).
    -- Keep the default, None (NONE), to exclude the DDS from this set of
    -- captions. Choose No display window (NO_DISPLAY_WINDOW) to have
    -- MediaConvert include the DDS but not include display window data. In
    -- this case, MediaConvert writes that information to the page composition
    -- segment (PCS) instead. Choose Specify (SPECIFIED) to have MediaConvert
    -- set up the display window based on the values that you specify in
    -- related job settings. For video resolutions that are 576 pixels or
    -- smaller in height, MediaConvert doesn\'t include the DDS, regardless of
    -- the value you choose for DDS handling (ddsHandling). In this case, it
    -- doesn\'t write the display window data to the PCS either. Related
    -- settings: Use the settings DDS x-coordinate (ddsXCoordinate) and DDS
    -- y-coordinate (ddsYCoordinate) to specify the offset between the top left
    -- corner of the display window and the top left corner of the video frame.
    -- All burn-in and DVB-Sub font settings must match.
    ddsHandling :: Prelude.Maybe DvbddsHandling,
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
    -- | Specifies the color of the DVB-SUB captions. This option is not valid
    -- for source captions that are STL, 608\/embedded or teletext. These
    -- source settings are already pre-defined by the caption stream. All
    -- burn-in and DVB-Sub font settings must match.
    fontColor :: Prelude.Maybe DvbSubtitleFontColor,
    -- | A positive integer indicates the exact font size in points. Set to 0 for
    -- automatic font size selection. All burn-in and DVB-Sub font settings
    -- must match.
    fontSize :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the horizontal offset of the shadow relative to the captions
    -- in pixels. A value of -2 would result in a shadow offset 2 pixels to the
    -- left. All burn-in and DVB-Sub font settings must match.
    shadowXOffset :: Prelude.Maybe Prelude.Int,
    -- | Ignore this setting unless your DvbSubtitleFontColor setting is HEX.
    -- Format is six or eight hexidecimal digits, representing the red, green,
    -- and blue components, with the two extra digits used for an optional
    -- alpha value. For example a value of 1122AABB is a red value of 0x11, a
    -- green value of 0x22, a blue value of 0xAA, and an alpha value of 0xBB.
    hexFontColor :: Prelude.Maybe Prelude.Text,
    -- | Specifies the color of the rectangle behind the captions. All burn-in
    -- and DVB-Sub font settings must match.
    backgroundColor :: Prelude.Maybe DvbSubtitleBackgroundColor,
    -- | Specifies the vertical position of the caption relative to the top of
    -- the output in pixels. A value of 10 would result in the captions
    -- starting 10 pixels from the top of the output. If no explicit y_position
    -- is provided, the caption will be positioned towards the bottom of the
    -- output. This option is not valid for source captions that are STL,
    -- 608\/embedded or teletext. These source settings are already pre-defined
    -- by the caption stream. All burn-in and DVB-Sub font settings must match.
    yPosition :: Prelude.Maybe Prelude.Natural,
    -- | Specifies font outline size in pixels. This option is not valid for
    -- source captions that are either 608\/embedded or teletext. These source
    -- settings are already pre-defined by the caption stream. All burn-in and
    -- DVB-Sub font settings must match.
    outlineSize :: Prelude.Maybe Prelude.Natural,
    -- | Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in
    -- and DVB-Sub font settings must match.
    fontResolution :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the vertical offset of the shadow relative to the captions in
    -- pixels. A value of -2 would result in a shadow offset 2 pixels above the
    -- text. All burn-in and DVB-Sub font settings must match.
    shadowYOffset :: Prelude.Maybe Prelude.Int,
    -- | Ignore this setting unless your input captions are STL, any type of 608,
    -- teletext, or TTML, and your output captions are DVB-SUB. Specify how the
    -- service applies the color specified in the setting Font color
    -- (DvbSubtitleFontColor). By default, this color is white. When you choose
    -- WHITE_TEXT_ONLY, the service uses the specified font color only for text
    -- that is white in the input. When you choose ALL_TEXT, the service uses
    -- the specified font color for all output captions text. If you leave both
    -- settings at their default value, your output font color is the same as
    -- your input font color.
    applyFontColor :: Prelude.Maybe DvbSubtitleApplyFontColor,
    -- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is
    -- transparent. All burn-in and DVB-Sub font settings must match.
    fontOpacity :: Prelude.Maybe Prelude.Natural
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
-- 'height', 'dvbSubDestinationSettings_height' - Specify the height, in pixels, of this set of DVB-Sub captions. The
-- default value is 576 pixels. Related setting: When you use this setting,
-- you must set DDS handling (ddsHandling) to a value other than None
-- (NONE). All burn-in and DVB-Sub font settings must match.
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
-- 'fallbackFont', 'dvbSubDestinationSettings_fallbackFont' - Specify the font that you want the service to use for your burn in
-- captions when your input captions specify a font that MediaConvert
-- doesn\'t support. When you keep the default value, Best match
-- (BEST_MATCH), MediaConvert uses a supported font that most closely
-- matches the font that your input captions specify. When there are
-- multiple unsupported fonts in your input captions, MediaConvert matches
-- each font with the supported font that matches best. When you explicitly
-- choose a replacement font, MediaConvert uses that font to replace all
-- unsupported fonts from your input.
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
-- 'ddsXCoordinate', 'dvbSubDestinationSettings_ddsXCoordinate' - Use this setting, along with DDS y-coordinate (ddsYCoordinate), to
-- specify the upper left corner of the display definition segment (DDS)
-- display window. With this setting, specify the distance, in pixels,
-- between the left side of the frame and the left side of the DDS display
-- window. Keep the default value, 0, to have MediaConvert automatically
-- choose this offset. Related setting: When you use this setting, you must
-- set DDS handling (ddsHandling) to a value other than None (NONE).
-- MediaConvert uses these values to determine whether to write page
-- position data to the DDS or to the page composition segment (PCS). All
-- burn-in and DVB-Sub font settings must match.
--
-- 'width', 'dvbSubDestinationSettings_width' - Specify the width, in pixels, of this set of DVB-Sub captions. The
-- default value is 720 pixels. Related setting: When you use this setting,
-- you must set DDS handling (ddsHandling) to a value other than None
-- (NONE). All burn-in and DVB-Sub font settings must match.
--
-- 'shadowColor', 'dvbSubDestinationSettings_shadowColor' - Specifies the color of the shadow cast by the captions. All burn-in and
-- DVB-Sub font settings must match.
--
-- 'ddsYCoordinate', 'dvbSubDestinationSettings_ddsYCoordinate' - Use this setting, along with DDS x-coordinate (ddsXCoordinate), to
-- specify the upper left corner of the display definition segment (DDS)
-- display window. With this setting, specify the distance, in pixels,
-- between the top of the frame and the top of the DDS display window. Keep
-- the default value, 0, to have MediaConvert automatically choose this
-- offset. Related setting: When you use this setting, you must set DDS
-- handling (ddsHandling) to a value other than None (NONE). MediaConvert
-- uses these values to determine whether to write page position data to
-- the DDS or to the page composition segment (PCS). All burn-in and
-- DVB-Sub font settings must match.
--
-- 'outlineColor', 'dvbSubDestinationSettings_outlineColor' - Specifies font outline color. This option is not valid for source
-- captions that are either 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
--
-- 'stylePassthrough', 'dvbSubDestinationSettings_stylePassthrough' - Choose which set of style and position values the service applies to
-- your output captions. When you choose ENABLED, the service uses the
-- input style and position information from your input. When you choose
-- DISABLED, the service uses any style values that you specify in your
-- output settings. If you don\'t specify values, the service uses default
-- style and position values. When you choose DISABLED, the service ignores
-- all style and position values from your input.
--
-- 'backgroundOpacity', 'dvbSubDestinationSettings_backgroundOpacity' - Specifies the opacity of the background rectangle. 255 is opaque; 0 is
-- transparent. Leaving this parameter blank is equivalent to setting it to
-- 0 (transparent). All burn-in and DVB-Sub font settings must match.
--
-- 'ddsHandling', 'dvbSubDestinationSettings_ddsHandling' - Specify how MediaConvert handles the display definition segment (DDS).
-- Keep the default, None (NONE), to exclude the DDS from this set of
-- captions. Choose No display window (NO_DISPLAY_WINDOW) to have
-- MediaConvert include the DDS but not include display window data. In
-- this case, MediaConvert writes that information to the page composition
-- segment (PCS) instead. Choose Specify (SPECIFIED) to have MediaConvert
-- set up the display window based on the values that you specify in
-- related job settings. For video resolutions that are 576 pixels or
-- smaller in height, MediaConvert doesn\'t include the DDS, regardless of
-- the value you choose for DDS handling (ddsHandling). In this case, it
-- doesn\'t write the display window data to the PCS either. Related
-- settings: Use the settings DDS x-coordinate (ddsXCoordinate) and DDS
-- y-coordinate (ddsYCoordinate) to specify the offset between the top left
-- corner of the display window and the top left corner of the video frame.
-- All burn-in and DVB-Sub font settings must match.
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
-- 'fontColor', 'dvbSubDestinationSettings_fontColor' - Specifies the color of the DVB-SUB captions. This option is not valid
-- for source captions that are STL, 608\/embedded or teletext. These
-- source settings are already pre-defined by the caption stream. All
-- burn-in and DVB-Sub font settings must match.
--
-- 'fontSize', 'dvbSubDestinationSettings_fontSize' - A positive integer indicates the exact font size in points. Set to 0 for
-- automatic font size selection. All burn-in and DVB-Sub font settings
-- must match.
--
-- 'shadowXOffset', 'dvbSubDestinationSettings_shadowXOffset' - Specifies the horizontal offset of the shadow relative to the captions
-- in pixels. A value of -2 would result in a shadow offset 2 pixels to the
-- left. All burn-in and DVB-Sub font settings must match.
--
-- 'hexFontColor', 'dvbSubDestinationSettings_hexFontColor' - Ignore this setting unless your DvbSubtitleFontColor setting is HEX.
-- Format is six or eight hexidecimal digits, representing the red, green,
-- and blue components, with the two extra digits used for an optional
-- alpha value. For example a value of 1122AABB is a red value of 0x11, a
-- green value of 0x22, a blue value of 0xAA, and an alpha value of 0xBB.
--
-- 'backgroundColor', 'dvbSubDestinationSettings_backgroundColor' - Specifies the color of the rectangle behind the captions. All burn-in
-- and DVB-Sub font settings must match.
--
-- 'yPosition', 'dvbSubDestinationSettings_yPosition' - Specifies the vertical position of the caption relative to the top of
-- the output in pixels. A value of 10 would result in the captions
-- starting 10 pixels from the top of the output. If no explicit y_position
-- is provided, the caption will be positioned towards the bottom of the
-- output. This option is not valid for source captions that are STL,
-- 608\/embedded or teletext. These source settings are already pre-defined
-- by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- 'outlineSize', 'dvbSubDestinationSettings_outlineSize' - Specifies font outline size in pixels. This option is not valid for
-- source captions that are either 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
--
-- 'fontResolution', 'dvbSubDestinationSettings_fontResolution' - Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in
-- and DVB-Sub font settings must match.
--
-- 'shadowYOffset', 'dvbSubDestinationSettings_shadowYOffset' - Specifies the vertical offset of the shadow relative to the captions in
-- pixels. A value of -2 would result in a shadow offset 2 pixels above the
-- text. All burn-in and DVB-Sub font settings must match.
--
-- 'applyFontColor', 'dvbSubDestinationSettings_applyFontColor' - Ignore this setting unless your input captions are STL, any type of 608,
-- teletext, or TTML, and your output captions are DVB-SUB. Specify how the
-- service applies the color specified in the setting Font color
-- (DvbSubtitleFontColor). By default, this color is white. When you choose
-- WHITE_TEXT_ONLY, the service uses the specified font color only for text
-- that is white in the input. When you choose ALL_TEXT, the service uses
-- the specified font color for all output captions text. If you leave both
-- settings at their default value, your output font color is the same as
-- your input font color.
--
-- 'fontOpacity', 'dvbSubDestinationSettings_fontOpacity' - Specifies the opacity of the burned-in captions. 255 is opaque; 0 is
-- transparent. All burn-in and DVB-Sub font settings must match.
newDvbSubDestinationSettings ::
  DvbSubDestinationSettings
newDvbSubDestinationSettings =
  DvbSubDestinationSettings'
    { height =
        Prelude.Nothing,
      alignment = Prelude.Nothing,
      fallbackFont = Prelude.Nothing,
      shadowOpacity = Prelude.Nothing,
      teletextSpacing = Prelude.Nothing,
      ddsXCoordinate = Prelude.Nothing,
      width = Prelude.Nothing,
      shadowColor = Prelude.Nothing,
      ddsYCoordinate = Prelude.Nothing,
      outlineColor = Prelude.Nothing,
      stylePassthrough = Prelude.Nothing,
      backgroundOpacity = Prelude.Nothing,
      ddsHandling = Prelude.Nothing,
      fontScript = Prelude.Nothing,
      xPosition = Prelude.Nothing,
      subtitlingType = Prelude.Nothing,
      fontColor = Prelude.Nothing,
      fontSize = Prelude.Nothing,
      shadowXOffset = Prelude.Nothing,
      hexFontColor = Prelude.Nothing,
      backgroundColor = Prelude.Nothing,
      yPosition = Prelude.Nothing,
      outlineSize = Prelude.Nothing,
      fontResolution = Prelude.Nothing,
      shadowYOffset = Prelude.Nothing,
      applyFontColor = Prelude.Nothing,
      fontOpacity = Prelude.Nothing
    }

-- | Specify the height, in pixels, of this set of DVB-Sub captions. The
-- default value is 576 pixels. Related setting: When you use this setting,
-- you must set DDS handling (ddsHandling) to a value other than None
-- (NONE). All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_height :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_height = Lens.lens (\DvbSubDestinationSettings' {height} -> height) (\s@DvbSubDestinationSettings' {} a -> s {height = a} :: DvbSubDestinationSettings)

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

-- | Specify the font that you want the service to use for your burn in
-- captions when your input captions specify a font that MediaConvert
-- doesn\'t support. When you keep the default value, Best match
-- (BEST_MATCH), MediaConvert uses a supported font that most closely
-- matches the font that your input captions specify. When there are
-- multiple unsupported fonts in your input captions, MediaConvert matches
-- each font with the supported font that matches best. When you explicitly
-- choose a replacement font, MediaConvert uses that font to replace all
-- unsupported fonts from your input.
dvbSubDestinationSettings_fallbackFont :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubSubtitleFallbackFont)
dvbSubDestinationSettings_fallbackFont = Lens.lens (\DvbSubDestinationSettings' {fallbackFont} -> fallbackFont) (\s@DvbSubDestinationSettings' {} a -> s {fallbackFont = a} :: DvbSubDestinationSettings)

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

-- | Use this setting, along with DDS y-coordinate (ddsYCoordinate), to
-- specify the upper left corner of the display definition segment (DDS)
-- display window. With this setting, specify the distance, in pixels,
-- between the left side of the frame and the left side of the DDS display
-- window. Keep the default value, 0, to have MediaConvert automatically
-- choose this offset. Related setting: When you use this setting, you must
-- set DDS handling (ddsHandling) to a value other than None (NONE).
-- MediaConvert uses these values to determine whether to write page
-- position data to the DDS or to the page composition segment (PCS). All
-- burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_ddsXCoordinate :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_ddsXCoordinate = Lens.lens (\DvbSubDestinationSettings' {ddsXCoordinate} -> ddsXCoordinate) (\s@DvbSubDestinationSettings' {} a -> s {ddsXCoordinate = a} :: DvbSubDestinationSettings)

-- | Specify the width, in pixels, of this set of DVB-Sub captions. The
-- default value is 720 pixels. Related setting: When you use this setting,
-- you must set DDS handling (ddsHandling) to a value other than None
-- (NONE). All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_width :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_width = Lens.lens (\DvbSubDestinationSettings' {width} -> width) (\s@DvbSubDestinationSettings' {} a -> s {width = a} :: DvbSubDestinationSettings)

-- | Specifies the color of the shadow cast by the captions. All burn-in and
-- DVB-Sub font settings must match.
dvbSubDestinationSettings_shadowColor :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubtitleShadowColor)
dvbSubDestinationSettings_shadowColor = Lens.lens (\DvbSubDestinationSettings' {shadowColor} -> shadowColor) (\s@DvbSubDestinationSettings' {} a -> s {shadowColor = a} :: DvbSubDestinationSettings)

-- | Use this setting, along with DDS x-coordinate (ddsXCoordinate), to
-- specify the upper left corner of the display definition segment (DDS)
-- display window. With this setting, specify the distance, in pixels,
-- between the top of the frame and the top of the DDS display window. Keep
-- the default value, 0, to have MediaConvert automatically choose this
-- offset. Related setting: When you use this setting, you must set DDS
-- handling (ddsHandling) to a value other than None (NONE). MediaConvert
-- uses these values to determine whether to write page position data to
-- the DDS or to the page composition segment (PCS). All burn-in and
-- DVB-Sub font settings must match.
dvbSubDestinationSettings_ddsYCoordinate :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_ddsYCoordinate = Lens.lens (\DvbSubDestinationSettings' {ddsYCoordinate} -> ddsYCoordinate) (\s@DvbSubDestinationSettings' {} a -> s {ddsYCoordinate = a} :: DvbSubDestinationSettings)

-- | Specifies font outline color. This option is not valid for source
-- captions that are either 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
dvbSubDestinationSettings_outlineColor :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubtitleOutlineColor)
dvbSubDestinationSettings_outlineColor = Lens.lens (\DvbSubDestinationSettings' {outlineColor} -> outlineColor) (\s@DvbSubDestinationSettings' {} a -> s {outlineColor = a} :: DvbSubDestinationSettings)

-- | Choose which set of style and position values the service applies to
-- your output captions. When you choose ENABLED, the service uses the
-- input style and position information from your input. When you choose
-- DISABLED, the service uses any style values that you specify in your
-- output settings. If you don\'t specify values, the service uses default
-- style and position values. When you choose DISABLED, the service ignores
-- all style and position values from your input.
dvbSubDestinationSettings_stylePassthrough :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubtitleStylePassthrough)
dvbSubDestinationSettings_stylePassthrough = Lens.lens (\DvbSubDestinationSettings' {stylePassthrough} -> stylePassthrough) (\s@DvbSubDestinationSettings' {} a -> s {stylePassthrough = a} :: DvbSubDestinationSettings)

-- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is
-- transparent. Leaving this parameter blank is equivalent to setting it to
-- 0 (transparent). All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_backgroundOpacity :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_backgroundOpacity = Lens.lens (\DvbSubDestinationSettings' {backgroundOpacity} -> backgroundOpacity) (\s@DvbSubDestinationSettings' {} a -> s {backgroundOpacity = a} :: DvbSubDestinationSettings)

-- | Specify how MediaConvert handles the display definition segment (DDS).
-- Keep the default, None (NONE), to exclude the DDS from this set of
-- captions. Choose No display window (NO_DISPLAY_WINDOW) to have
-- MediaConvert include the DDS but not include display window data. In
-- this case, MediaConvert writes that information to the page composition
-- segment (PCS) instead. Choose Specify (SPECIFIED) to have MediaConvert
-- set up the display window based on the values that you specify in
-- related job settings. For video resolutions that are 576 pixels or
-- smaller in height, MediaConvert doesn\'t include the DDS, regardless of
-- the value you choose for DDS handling (ddsHandling). In this case, it
-- doesn\'t write the display window data to the PCS either. Related
-- settings: Use the settings DDS x-coordinate (ddsXCoordinate) and DDS
-- y-coordinate (ddsYCoordinate) to specify the offset between the top left
-- corner of the display window and the top left corner of the video frame.
-- All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_ddsHandling :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbddsHandling)
dvbSubDestinationSettings_ddsHandling = Lens.lens (\DvbSubDestinationSettings' {ddsHandling} -> ddsHandling) (\s@DvbSubDestinationSettings' {} a -> s {ddsHandling = a} :: DvbSubDestinationSettings)

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

-- | Specifies the color of the DVB-SUB captions. This option is not valid
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

-- | Specifies the horizontal offset of the shadow relative to the captions
-- in pixels. A value of -2 would result in a shadow offset 2 pixels to the
-- left. All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_shadowXOffset :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Int)
dvbSubDestinationSettings_shadowXOffset = Lens.lens (\DvbSubDestinationSettings' {shadowXOffset} -> shadowXOffset) (\s@DvbSubDestinationSettings' {} a -> s {shadowXOffset = a} :: DvbSubDestinationSettings)

-- | Ignore this setting unless your DvbSubtitleFontColor setting is HEX.
-- Format is six or eight hexidecimal digits, representing the red, green,
-- and blue components, with the two extra digits used for an optional
-- alpha value. For example a value of 1122AABB is a red value of 0x11, a
-- green value of 0x22, a blue value of 0xAA, and an alpha value of 0xBB.
dvbSubDestinationSettings_hexFontColor :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Text)
dvbSubDestinationSettings_hexFontColor = Lens.lens (\DvbSubDestinationSettings' {hexFontColor} -> hexFontColor) (\s@DvbSubDestinationSettings' {} a -> s {hexFontColor = a} :: DvbSubDestinationSettings)

-- | Specifies the color of the rectangle behind the captions. All burn-in
-- and DVB-Sub font settings must match.
dvbSubDestinationSettings_backgroundColor :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubtitleBackgroundColor)
dvbSubDestinationSettings_backgroundColor = Lens.lens (\DvbSubDestinationSettings' {backgroundColor} -> backgroundColor) (\s@DvbSubDestinationSettings' {} a -> s {backgroundColor = a} :: DvbSubDestinationSettings)

-- | Specifies the vertical position of the caption relative to the top of
-- the output in pixels. A value of 10 would result in the captions
-- starting 10 pixels from the top of the output. If no explicit y_position
-- is provided, the caption will be positioned towards the bottom of the
-- output. This option is not valid for source captions that are STL,
-- 608\/embedded or teletext. These source settings are already pre-defined
-- by the caption stream. All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_yPosition :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_yPosition = Lens.lens (\DvbSubDestinationSettings' {yPosition} -> yPosition) (\s@DvbSubDestinationSettings' {} a -> s {yPosition = a} :: DvbSubDestinationSettings)

-- | Specifies font outline size in pixels. This option is not valid for
-- source captions that are either 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
dvbSubDestinationSettings_outlineSize :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_outlineSize = Lens.lens (\DvbSubDestinationSettings' {outlineSize} -> outlineSize) (\s@DvbSubDestinationSettings' {} a -> s {outlineSize = a} :: DvbSubDestinationSettings)

-- | Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in
-- and DVB-Sub font settings must match.
dvbSubDestinationSettings_fontResolution :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_fontResolution = Lens.lens (\DvbSubDestinationSettings' {fontResolution} -> fontResolution) (\s@DvbSubDestinationSettings' {} a -> s {fontResolution = a} :: DvbSubDestinationSettings)

-- | Specifies the vertical offset of the shadow relative to the captions in
-- pixels. A value of -2 would result in a shadow offset 2 pixels above the
-- text. All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_shadowYOffset :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Int)
dvbSubDestinationSettings_shadowYOffset = Lens.lens (\DvbSubDestinationSettings' {shadowYOffset} -> shadowYOffset) (\s@DvbSubDestinationSettings' {} a -> s {shadowYOffset = a} :: DvbSubDestinationSettings)

-- | Ignore this setting unless your input captions are STL, any type of 608,
-- teletext, or TTML, and your output captions are DVB-SUB. Specify how the
-- service applies the color specified in the setting Font color
-- (DvbSubtitleFontColor). By default, this color is white. When you choose
-- WHITE_TEXT_ONLY, the service uses the specified font color only for text
-- that is white in the input. When you choose ALL_TEXT, the service uses
-- the specified font color for all output captions text. If you leave both
-- settings at their default value, your output font color is the same as
-- your input font color.
dvbSubDestinationSettings_applyFontColor :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubtitleApplyFontColor)
dvbSubDestinationSettings_applyFontColor = Lens.lens (\DvbSubDestinationSettings' {applyFontColor} -> applyFontColor) (\s@DvbSubDestinationSettings' {} a -> s {applyFontColor = a} :: DvbSubDestinationSettings)

-- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is
-- transparent. All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_fontOpacity :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_fontOpacity = Lens.lens (\DvbSubDestinationSettings' {fontOpacity} -> fontOpacity) (\s@DvbSubDestinationSettings' {} a -> s {fontOpacity = a} :: DvbSubDestinationSettings)

instance Core.FromJSON DvbSubDestinationSettings where
  parseJSON =
    Core.withObject
      "DvbSubDestinationSettings"
      ( \x ->
          DvbSubDestinationSettings'
            Prelude.<$> (x Core..:? "height")
            Prelude.<*> (x Core..:? "alignment")
            Prelude.<*> (x Core..:? "fallbackFont")
            Prelude.<*> (x Core..:? "shadowOpacity")
            Prelude.<*> (x Core..:? "teletextSpacing")
            Prelude.<*> (x Core..:? "ddsXCoordinate")
            Prelude.<*> (x Core..:? "width")
            Prelude.<*> (x Core..:? "shadowColor")
            Prelude.<*> (x Core..:? "ddsYCoordinate")
            Prelude.<*> (x Core..:? "outlineColor")
            Prelude.<*> (x Core..:? "stylePassthrough")
            Prelude.<*> (x Core..:? "backgroundOpacity")
            Prelude.<*> (x Core..:? "ddsHandling")
            Prelude.<*> (x Core..:? "fontScript")
            Prelude.<*> (x Core..:? "xPosition")
            Prelude.<*> (x Core..:? "subtitlingType")
            Prelude.<*> (x Core..:? "fontColor")
            Prelude.<*> (x Core..:? "fontSize")
            Prelude.<*> (x Core..:? "shadowXOffset")
            Prelude.<*> (x Core..:? "hexFontColor")
            Prelude.<*> (x Core..:? "backgroundColor")
            Prelude.<*> (x Core..:? "yPosition")
            Prelude.<*> (x Core..:? "outlineSize")
            Prelude.<*> (x Core..:? "fontResolution")
            Prelude.<*> (x Core..:? "shadowYOffset")
            Prelude.<*> (x Core..:? "applyFontColor")
            Prelude.<*> (x Core..:? "fontOpacity")
      )

instance Prelude.Hashable DvbSubDestinationSettings

instance Prelude.NFData DvbSubDestinationSettings

instance Core.ToJSON DvbSubDestinationSettings where
  toJSON DvbSubDestinationSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("height" Core..=) Prelude.<$> height,
            ("alignment" Core..=) Prelude.<$> alignment,
            ("fallbackFont" Core..=) Prelude.<$> fallbackFont,
            ("shadowOpacity" Core..=) Prelude.<$> shadowOpacity,
            ("teletextSpacing" Core..=)
              Prelude.<$> teletextSpacing,
            ("ddsXCoordinate" Core..=)
              Prelude.<$> ddsXCoordinate,
            ("width" Core..=) Prelude.<$> width,
            ("shadowColor" Core..=) Prelude.<$> shadowColor,
            ("ddsYCoordinate" Core..=)
              Prelude.<$> ddsYCoordinate,
            ("outlineColor" Core..=) Prelude.<$> outlineColor,
            ("stylePassthrough" Core..=)
              Prelude.<$> stylePassthrough,
            ("backgroundOpacity" Core..=)
              Prelude.<$> backgroundOpacity,
            ("ddsHandling" Core..=) Prelude.<$> ddsHandling,
            ("fontScript" Core..=) Prelude.<$> fontScript,
            ("xPosition" Core..=) Prelude.<$> xPosition,
            ("subtitlingType" Core..=)
              Prelude.<$> subtitlingType,
            ("fontColor" Core..=) Prelude.<$> fontColor,
            ("fontSize" Core..=) Prelude.<$> fontSize,
            ("shadowXOffset" Core..=) Prelude.<$> shadowXOffset,
            ("hexFontColor" Core..=) Prelude.<$> hexFontColor,
            ("backgroundColor" Core..=)
              Prelude.<$> backgroundColor,
            ("yPosition" Core..=) Prelude.<$> yPosition,
            ("outlineSize" Core..=) Prelude.<$> outlineSize,
            ("fontResolution" Core..=)
              Prelude.<$> fontResolution,
            ("shadowYOffset" Core..=) Prelude.<$> shadowYOffset,
            ("applyFontColor" Core..=)
              Prelude.<$> applyFontColor,
            ("fontOpacity" Core..=) Prelude.<$> fontOpacity
          ]
      )
