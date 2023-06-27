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
-- Module      : Amazonka.MediaConvert.Types.DvbSubDestinationSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.DvbSubDestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.DvbSubSubtitleFallbackFont
import Amazonka.MediaConvert.Types.DvbSubtitleAlignment
import Amazonka.MediaConvert.Types.DvbSubtitleApplyFontColor
import Amazonka.MediaConvert.Types.DvbSubtitleBackgroundColor
import Amazonka.MediaConvert.Types.DvbSubtitleFontColor
import Amazonka.MediaConvert.Types.DvbSubtitleOutlineColor
import Amazonka.MediaConvert.Types.DvbSubtitleShadowColor
import Amazonka.MediaConvert.Types.DvbSubtitleStylePassthrough
import Amazonka.MediaConvert.Types.DvbSubtitleTeletextSpacing
import Amazonka.MediaConvert.Types.DvbSubtitlingType
import Amazonka.MediaConvert.Types.DvbddsHandling
import Amazonka.MediaConvert.Types.FontScript
import qualified Amazonka.Prelude as Prelude

-- | Settings related to DVB-Sub captions. Set up DVB-Sub captions in the
-- same output as your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/dvb-sub-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to
-- DVB_SUB.
--
-- /See:/ 'newDvbSubDestinationSettings' smart constructor.
data DvbSubDestinationSettings = DvbSubDestinationSettings'
  { -- | Specify the alignment of your captions. If no explicit x_position is
    -- provided, setting alignment to centered will placethe captions at the
    -- bottom center of the output. Similarly, setting a left alignment
    -- willalign captions to the bottom left of the output. If x and y
    -- positions are given in conjunction with the alignment parameter, the
    -- font will be justified (either left or centered) relative to those
    -- coordinates. Within your job settings, all of your DVB-Sub settings must
    -- be identical.
    alignment :: Prelude.Maybe DvbSubtitleAlignment,
    -- | Ignore this setting unless Style Passthrough (StylePassthrough) is set
    -- to Enabled and Font color (FontColor) set to Black, Yellow, Red, Green,
    -- Blue, or Hex. Use Apply font color (ApplyFontColor) for additional font
    -- color controls. When you choose White text only (WHITE_TEXT_ONLY), or
    -- leave blank, your font color setting only applies to white text in your
    -- input captions. For example, if your font color setting is Yellow, and
    -- your input captions have red and white text, your output captions will
    -- have red and yellow text. When you choose ALL_TEXT, your font color
    -- setting applies to all of your output captions text.
    applyFontColor :: Prelude.Maybe DvbSubtitleApplyFontColor,
    -- | Specify the color of the rectangle behind the captions. Leave background
    -- color (BackgroundColor) blank and set Style passthrough
    -- (StylePassthrough) to enabled to use the background color data from your
    -- input captions, if present.
    backgroundColor :: Prelude.Maybe DvbSubtitleBackgroundColor,
    -- | Specify the opacity of the background rectangle. Enter a value from 0 to
    -- 255, where 0 is transparent and 255 is opaque. If Style passthrough
    -- (StylePassthrough) is set to enabled, leave blank to pass through the
    -- background style information in your input captions to your output
    -- captions. If Style passthrough is set to disabled, leave blank to use a
    -- value of 0 and remove all backgrounds from your output captions. Within
    -- your job settings, all of your DVB-Sub settings must be identical.
    backgroundOpacity :: Prelude.Maybe Prelude.Natural,
    -- | Specify how MediaConvert handles the display definition segment (DDS).
    -- To exclude the DDS from this set of captions: Keep the default, None. To
    -- include the DDS: Choose Specified. When you do, also specify the offset
    -- coordinates of the display window with DDS x-coordinate and DDS
    -- y-coordinate. To include the DDS, but not include display window data:
    -- Choose No display window. When you do, you can write position metadata
    -- to the page composition segment (PCS) with DDS x-coordinate and DDS
    -- y-coordinate. For video resolutions with a height of 576 pixels or less,
    -- MediaConvert doesn\'t include the DDS, regardless of the value you
    -- choose for DDS handling. All burn-in and DVB-Sub font settings must
    -- match.
    ddsHandling :: Prelude.Maybe DvbddsHandling,
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
    -- | Specify the font that you want the service to use for your burn in
    -- captions when your input captions specify a font that MediaConvert
    -- doesn\'t support. When you set Fallback font (FallbackFont) to best
    -- match (BEST_MATCH), or leave blank, MediaConvert uses a supported font
    -- that most closely matches the font that your input captions specify.
    -- When there are multiple unsupported fonts in your input captions,
    -- MediaConvert matches each font with the supported font that matches
    -- best. When you explicitly choose a replacement font, MediaConvert uses
    -- that font to replace all unsupported fonts from your input.
    fallbackFont :: Prelude.Maybe DvbSubSubtitleFallbackFont,
    -- | Specify the color of the captions text. Leave Font color (FontColor)
    -- blank and set Style passthrough (StylePassthrough) to enabled to use the
    -- font color data from your input captions, if present. Within your job
    -- settings, all of your DVB-Sub settings must be identical.
    fontColor :: Prelude.Maybe DvbSubtitleFontColor,
    -- | Specify the opacity of the burned-in captions. 255 is opaque; 0 is
    -- transparent. Within your job settings, all of your DVB-Sub settings must
    -- be identical.
    fontOpacity :: Prelude.Maybe Prelude.Natural,
    -- | Specify the Font resolution (FontResolution) in DPI (dots per inch).
    -- Within your job settings, all of your DVB-Sub settings must be
    -- identical.
    fontResolution :: Prelude.Maybe Prelude.Natural,
    -- | Set Font script (FontScript) to Automatically determined (AUTOMATIC), or
    -- leave blank, to automatically determine the font script in your input
    -- captions. Otherwise, set to Simplified Chinese (HANS) or Traditional
    -- Chinese (HANT) if your input font script uses Simplified or Traditional
    -- Chinese. Within your job settings, all of your DVB-Sub settings must be
    -- identical.
    fontScript :: Prelude.Maybe FontScript,
    -- | Specify the Font size (FontSize) in pixels. Must be a positive integer.
    -- Set to 0, or leave blank, for automatic font size. Within your job
    -- settings, all of your DVB-Sub settings must be identical.
    fontSize :: Prelude.Maybe Prelude.Natural,
    -- | Specify the height, in pixels, of this set of DVB-Sub captions. The
    -- default value is 576 pixels. Related setting: When you use this setting,
    -- you must set DDS handling (ddsHandling) to a value other than None
    -- (NONE). All burn-in and DVB-Sub font settings must match.
    height :: Prelude.Maybe Prelude.Natural,
    -- | Ignore this setting unless your Font color is set to Hex. Enter either
    -- six or eight hexidecimal digits, representing red, green, and blue, with
    -- two optional extra digits for alpha. For example a value of 1122AABB is
    -- a red value of 0x11, a green value of 0x22, a blue value of 0xAA, and an
    -- alpha value of 0xBB.
    hexFontColor :: Prelude.Maybe Prelude.Text,
    -- | Specify font outline color. Leave Outline color (OutlineColor) blank and
    -- set Style passthrough (StylePassthrough) to enabled to use the font
    -- outline color data from your input captions, if present. Within your job
    -- settings, all of your DVB-Sub settings must be identical.
    outlineColor :: Prelude.Maybe DvbSubtitleOutlineColor,
    -- | Specify the Outline size (OutlineSize) of the caption text, in pixels.
    -- Leave Outline size blank and set Style passthrough (StylePassthrough) to
    -- enabled to use the outline size data from your input captions, if
    -- present. Within your job settings, all of your DVB-Sub settings must be
    -- identical.
    outlineSize :: Prelude.Maybe Prelude.Natural,
    -- | Specify the color of the shadow cast by the captions. Leave Shadow color
    -- (ShadowColor) blank and set Style passthrough (StylePassthrough) to
    -- enabled to use the shadow color data from your input captions, if
    -- present. Within your job settings, all of your DVB-Sub settings must be
    -- identical.
    shadowColor :: Prelude.Maybe DvbSubtitleShadowColor,
    -- | Specify the opacity of the shadow. Enter a value from 0 to 255, where 0
    -- is transparent and 255 is opaque. If Style passthrough
    -- (StylePassthrough) is set to Enabled, leave Shadow opacity
    -- (ShadowOpacity) blank to pass through the shadow style information in
    -- your input captions to your output captions. If Style passthrough is set
    -- to disabled, leave blank to use a value of 0 and remove all shadows from
    -- your output captions. Within your job settings, all of your DVB-Sub
    -- settings must be identical.
    shadowOpacity :: Prelude.Maybe Prelude.Natural,
    -- | Specify the horizontal offset of the shadow, relative to the captions in
    -- pixels. A value of -2 would result in a shadow offset 2 pixels to the
    -- left. Within your job settings, all of your DVB-Sub settings must be
    -- identical.
    shadowXOffset :: Prelude.Maybe Prelude.Int,
    -- | Specify the vertical offset of the shadow relative to the captions in
    -- pixels. A value of -2 would result in a shadow offset 2 pixels above the
    -- text. Leave Shadow y-offset (ShadowYOffset) blank and set Style
    -- passthrough (StylePassthrough) to enabled to use the shadow y-offset
    -- data from your input captions, if present. Within your job settings, all
    -- of your DVB-Sub settings must be identical.
    shadowYOffset :: Prelude.Maybe Prelude.Int,
    -- | Set Style passthrough (StylePassthrough) to ENABLED to use the available
    -- style, color, and position information from your input captions.
    -- MediaConvert uses default settings for any missing style and position
    -- information in your input captions. Set Style passthrough to DISABLED,
    -- or leave blank, to ignore the style and position information from your
    -- input captions and use default settings: white text with black
    -- outlining, bottom-center positioning, and automatic sizing. Whether you
    -- set Style passthrough to enabled or not, you can also choose to manually
    -- override any of the individual style and position settings.
    stylePassthrough :: Prelude.Maybe DvbSubtitleStylePassthrough,
    -- | Specify whether your DVB subtitles are standard or for hearing impaired.
    -- Choose hearing impaired if your subtitles include audio descriptions and
    -- dialogue. Choose standard if your subtitles include only dialogue.
    subtitlingType :: Prelude.Maybe DvbSubtitlingType,
    -- | Specify whether the Text spacing (TeletextSpacing) in your captions is
    -- set by the captions grid, or varies depending on letter width. Choose
    -- fixed grid (FIXED_GRID) to conform to the spacing specified in the
    -- captions file more accurately. Choose proportional (PROPORTIONAL) to
    -- make the text easier to read for closed captions. Within your job
    -- settings, all of your DVB-Sub settings must be identical.
    teletextSpacing :: Prelude.Maybe DvbSubtitleTeletextSpacing,
    -- | Specify the width, in pixels, of this set of DVB-Sub captions. The
    -- default value is 720 pixels. Related setting: When you use this setting,
    -- you must set DDS handling (ddsHandling) to a value other than None
    -- (NONE). All burn-in and DVB-Sub font settings must match.
    width :: Prelude.Maybe Prelude.Natural,
    -- | Specify the horizontal position (XPosition) of the captions, relative to
    -- the left side of the outputin pixels. A value of 10 would result in the
    -- captions starting 10 pixels from the left ofthe output. If no explicit
    -- x_position is provided, the horizontal caption position will
    -- bedetermined by the alignment parameter. Within your job settings, all
    -- of your DVB-Sub settings must be identical.
    xPosition :: Prelude.Maybe Prelude.Natural,
    -- | Specify the vertical position (YPosition) of the captions, relative to
    -- the top of the output in pixels. A value of 10 would result in the
    -- captions starting 10 pixels from the top of the output. If no explicit
    -- y_position is provided, the caption will be positioned towards the
    -- bottom of the output. Within your job settings, all of your DVB-Sub
    -- settings must be identical.
    yPosition :: Prelude.Maybe Prelude.Natural
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
-- 'alignment', 'dvbSubDestinationSettings_alignment' - Specify the alignment of your captions. If no explicit x_position is
-- provided, setting alignment to centered will placethe captions at the
-- bottom center of the output. Similarly, setting a left alignment
-- willalign captions to the bottom left of the output. If x and y
-- positions are given in conjunction with the alignment parameter, the
-- font will be justified (either left or centered) relative to those
-- coordinates. Within your job settings, all of your DVB-Sub settings must
-- be identical.
--
-- 'applyFontColor', 'dvbSubDestinationSettings_applyFontColor' - Ignore this setting unless Style Passthrough (StylePassthrough) is set
-- to Enabled and Font color (FontColor) set to Black, Yellow, Red, Green,
-- Blue, or Hex. Use Apply font color (ApplyFontColor) for additional font
-- color controls. When you choose White text only (WHITE_TEXT_ONLY), or
-- leave blank, your font color setting only applies to white text in your
-- input captions. For example, if your font color setting is Yellow, and
-- your input captions have red and white text, your output captions will
-- have red and yellow text. When you choose ALL_TEXT, your font color
-- setting applies to all of your output captions text.
--
-- 'backgroundColor', 'dvbSubDestinationSettings_backgroundColor' - Specify the color of the rectangle behind the captions. Leave background
-- color (BackgroundColor) blank and set Style passthrough
-- (StylePassthrough) to enabled to use the background color data from your
-- input captions, if present.
--
-- 'backgroundOpacity', 'dvbSubDestinationSettings_backgroundOpacity' - Specify the opacity of the background rectangle. Enter a value from 0 to
-- 255, where 0 is transparent and 255 is opaque. If Style passthrough
-- (StylePassthrough) is set to enabled, leave blank to pass through the
-- background style information in your input captions to your output
-- captions. If Style passthrough is set to disabled, leave blank to use a
-- value of 0 and remove all backgrounds from your output captions. Within
-- your job settings, all of your DVB-Sub settings must be identical.
--
-- 'ddsHandling', 'dvbSubDestinationSettings_ddsHandling' - Specify how MediaConvert handles the display definition segment (DDS).
-- To exclude the DDS from this set of captions: Keep the default, None. To
-- include the DDS: Choose Specified. When you do, also specify the offset
-- coordinates of the display window with DDS x-coordinate and DDS
-- y-coordinate. To include the DDS, but not include display window data:
-- Choose No display window. When you do, you can write position metadata
-- to the page composition segment (PCS) with DDS x-coordinate and DDS
-- y-coordinate. For video resolutions with a height of 576 pixels or less,
-- MediaConvert doesn\'t include the DDS, regardless of the value you
-- choose for DDS handling. All burn-in and DVB-Sub font settings must
-- match.
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
-- 'fallbackFont', 'dvbSubDestinationSettings_fallbackFont' - Specify the font that you want the service to use for your burn in
-- captions when your input captions specify a font that MediaConvert
-- doesn\'t support. When you set Fallback font (FallbackFont) to best
-- match (BEST_MATCH), or leave blank, MediaConvert uses a supported font
-- that most closely matches the font that your input captions specify.
-- When there are multiple unsupported fonts in your input captions,
-- MediaConvert matches each font with the supported font that matches
-- best. When you explicitly choose a replacement font, MediaConvert uses
-- that font to replace all unsupported fonts from your input.
--
-- 'fontColor', 'dvbSubDestinationSettings_fontColor' - Specify the color of the captions text. Leave Font color (FontColor)
-- blank and set Style passthrough (StylePassthrough) to enabled to use the
-- font color data from your input captions, if present. Within your job
-- settings, all of your DVB-Sub settings must be identical.
--
-- 'fontOpacity', 'dvbSubDestinationSettings_fontOpacity' - Specify the opacity of the burned-in captions. 255 is opaque; 0 is
-- transparent. Within your job settings, all of your DVB-Sub settings must
-- be identical.
--
-- 'fontResolution', 'dvbSubDestinationSettings_fontResolution' - Specify the Font resolution (FontResolution) in DPI (dots per inch).
-- Within your job settings, all of your DVB-Sub settings must be
-- identical.
--
-- 'fontScript', 'dvbSubDestinationSettings_fontScript' - Set Font script (FontScript) to Automatically determined (AUTOMATIC), or
-- leave blank, to automatically determine the font script in your input
-- captions. Otherwise, set to Simplified Chinese (HANS) or Traditional
-- Chinese (HANT) if your input font script uses Simplified or Traditional
-- Chinese. Within your job settings, all of your DVB-Sub settings must be
-- identical.
--
-- 'fontSize', 'dvbSubDestinationSettings_fontSize' - Specify the Font size (FontSize) in pixels. Must be a positive integer.
-- Set to 0, or leave blank, for automatic font size. Within your job
-- settings, all of your DVB-Sub settings must be identical.
--
-- 'height', 'dvbSubDestinationSettings_height' - Specify the height, in pixels, of this set of DVB-Sub captions. The
-- default value is 576 pixels. Related setting: When you use this setting,
-- you must set DDS handling (ddsHandling) to a value other than None
-- (NONE). All burn-in and DVB-Sub font settings must match.
--
-- 'hexFontColor', 'dvbSubDestinationSettings_hexFontColor' - Ignore this setting unless your Font color is set to Hex. Enter either
-- six or eight hexidecimal digits, representing red, green, and blue, with
-- two optional extra digits for alpha. For example a value of 1122AABB is
-- a red value of 0x11, a green value of 0x22, a blue value of 0xAA, and an
-- alpha value of 0xBB.
--
-- 'outlineColor', 'dvbSubDestinationSettings_outlineColor' - Specify font outline color. Leave Outline color (OutlineColor) blank and
-- set Style passthrough (StylePassthrough) to enabled to use the font
-- outline color data from your input captions, if present. Within your job
-- settings, all of your DVB-Sub settings must be identical.
--
-- 'outlineSize', 'dvbSubDestinationSettings_outlineSize' - Specify the Outline size (OutlineSize) of the caption text, in pixels.
-- Leave Outline size blank and set Style passthrough (StylePassthrough) to
-- enabled to use the outline size data from your input captions, if
-- present. Within your job settings, all of your DVB-Sub settings must be
-- identical.
--
-- 'shadowColor', 'dvbSubDestinationSettings_shadowColor' - Specify the color of the shadow cast by the captions. Leave Shadow color
-- (ShadowColor) blank and set Style passthrough (StylePassthrough) to
-- enabled to use the shadow color data from your input captions, if
-- present. Within your job settings, all of your DVB-Sub settings must be
-- identical.
--
-- 'shadowOpacity', 'dvbSubDestinationSettings_shadowOpacity' - Specify the opacity of the shadow. Enter a value from 0 to 255, where 0
-- is transparent and 255 is opaque. If Style passthrough
-- (StylePassthrough) is set to Enabled, leave Shadow opacity
-- (ShadowOpacity) blank to pass through the shadow style information in
-- your input captions to your output captions. If Style passthrough is set
-- to disabled, leave blank to use a value of 0 and remove all shadows from
-- your output captions. Within your job settings, all of your DVB-Sub
-- settings must be identical.
--
-- 'shadowXOffset', 'dvbSubDestinationSettings_shadowXOffset' - Specify the horizontal offset of the shadow, relative to the captions in
-- pixels. A value of -2 would result in a shadow offset 2 pixels to the
-- left. Within your job settings, all of your DVB-Sub settings must be
-- identical.
--
-- 'shadowYOffset', 'dvbSubDestinationSettings_shadowYOffset' - Specify the vertical offset of the shadow relative to the captions in
-- pixels. A value of -2 would result in a shadow offset 2 pixels above the
-- text. Leave Shadow y-offset (ShadowYOffset) blank and set Style
-- passthrough (StylePassthrough) to enabled to use the shadow y-offset
-- data from your input captions, if present. Within your job settings, all
-- of your DVB-Sub settings must be identical.
--
-- 'stylePassthrough', 'dvbSubDestinationSettings_stylePassthrough' - Set Style passthrough (StylePassthrough) to ENABLED to use the available
-- style, color, and position information from your input captions.
-- MediaConvert uses default settings for any missing style and position
-- information in your input captions. Set Style passthrough to DISABLED,
-- or leave blank, to ignore the style and position information from your
-- input captions and use default settings: white text with black
-- outlining, bottom-center positioning, and automatic sizing. Whether you
-- set Style passthrough to enabled or not, you can also choose to manually
-- override any of the individual style and position settings.
--
-- 'subtitlingType', 'dvbSubDestinationSettings_subtitlingType' - Specify whether your DVB subtitles are standard or for hearing impaired.
-- Choose hearing impaired if your subtitles include audio descriptions and
-- dialogue. Choose standard if your subtitles include only dialogue.
--
-- 'teletextSpacing', 'dvbSubDestinationSettings_teletextSpacing' - Specify whether the Text spacing (TeletextSpacing) in your captions is
-- set by the captions grid, or varies depending on letter width. Choose
-- fixed grid (FIXED_GRID) to conform to the spacing specified in the
-- captions file more accurately. Choose proportional (PROPORTIONAL) to
-- make the text easier to read for closed captions. Within your job
-- settings, all of your DVB-Sub settings must be identical.
--
-- 'width', 'dvbSubDestinationSettings_width' - Specify the width, in pixels, of this set of DVB-Sub captions. The
-- default value is 720 pixels. Related setting: When you use this setting,
-- you must set DDS handling (ddsHandling) to a value other than None
-- (NONE). All burn-in and DVB-Sub font settings must match.
--
-- 'xPosition', 'dvbSubDestinationSettings_xPosition' - Specify the horizontal position (XPosition) of the captions, relative to
-- the left side of the outputin pixels. A value of 10 would result in the
-- captions starting 10 pixels from the left ofthe output. If no explicit
-- x_position is provided, the horizontal caption position will
-- bedetermined by the alignment parameter. Within your job settings, all
-- of your DVB-Sub settings must be identical.
--
-- 'yPosition', 'dvbSubDestinationSettings_yPosition' - Specify the vertical position (YPosition) of the captions, relative to
-- the top of the output in pixels. A value of 10 would result in the
-- captions starting 10 pixels from the top of the output. If no explicit
-- y_position is provided, the caption will be positioned towards the
-- bottom of the output. Within your job settings, all of your DVB-Sub
-- settings must be identical.
newDvbSubDestinationSettings ::
  DvbSubDestinationSettings
newDvbSubDestinationSettings =
  DvbSubDestinationSettings'
    { alignment =
        Prelude.Nothing,
      applyFontColor = Prelude.Nothing,
      backgroundColor = Prelude.Nothing,
      backgroundOpacity = Prelude.Nothing,
      ddsHandling = Prelude.Nothing,
      ddsXCoordinate = Prelude.Nothing,
      ddsYCoordinate = Prelude.Nothing,
      fallbackFont = Prelude.Nothing,
      fontColor = Prelude.Nothing,
      fontOpacity = Prelude.Nothing,
      fontResolution = Prelude.Nothing,
      fontScript = Prelude.Nothing,
      fontSize = Prelude.Nothing,
      height = Prelude.Nothing,
      hexFontColor = Prelude.Nothing,
      outlineColor = Prelude.Nothing,
      outlineSize = Prelude.Nothing,
      shadowColor = Prelude.Nothing,
      shadowOpacity = Prelude.Nothing,
      shadowXOffset = Prelude.Nothing,
      shadowYOffset = Prelude.Nothing,
      stylePassthrough = Prelude.Nothing,
      subtitlingType = Prelude.Nothing,
      teletextSpacing = Prelude.Nothing,
      width = Prelude.Nothing,
      xPosition = Prelude.Nothing,
      yPosition = Prelude.Nothing
    }

-- | Specify the alignment of your captions. If no explicit x_position is
-- provided, setting alignment to centered will placethe captions at the
-- bottom center of the output. Similarly, setting a left alignment
-- willalign captions to the bottom left of the output. If x and y
-- positions are given in conjunction with the alignment parameter, the
-- font will be justified (either left or centered) relative to those
-- coordinates. Within your job settings, all of your DVB-Sub settings must
-- be identical.
dvbSubDestinationSettings_alignment :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubtitleAlignment)
dvbSubDestinationSettings_alignment = Lens.lens (\DvbSubDestinationSettings' {alignment} -> alignment) (\s@DvbSubDestinationSettings' {} a -> s {alignment = a} :: DvbSubDestinationSettings)

-- | Ignore this setting unless Style Passthrough (StylePassthrough) is set
-- to Enabled and Font color (FontColor) set to Black, Yellow, Red, Green,
-- Blue, or Hex. Use Apply font color (ApplyFontColor) for additional font
-- color controls. When you choose White text only (WHITE_TEXT_ONLY), or
-- leave blank, your font color setting only applies to white text in your
-- input captions. For example, if your font color setting is Yellow, and
-- your input captions have red and white text, your output captions will
-- have red and yellow text. When you choose ALL_TEXT, your font color
-- setting applies to all of your output captions text.
dvbSubDestinationSettings_applyFontColor :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubtitleApplyFontColor)
dvbSubDestinationSettings_applyFontColor = Lens.lens (\DvbSubDestinationSettings' {applyFontColor} -> applyFontColor) (\s@DvbSubDestinationSettings' {} a -> s {applyFontColor = a} :: DvbSubDestinationSettings)

-- | Specify the color of the rectangle behind the captions. Leave background
-- color (BackgroundColor) blank and set Style passthrough
-- (StylePassthrough) to enabled to use the background color data from your
-- input captions, if present.
dvbSubDestinationSettings_backgroundColor :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubtitleBackgroundColor)
dvbSubDestinationSettings_backgroundColor = Lens.lens (\DvbSubDestinationSettings' {backgroundColor} -> backgroundColor) (\s@DvbSubDestinationSettings' {} a -> s {backgroundColor = a} :: DvbSubDestinationSettings)

-- | Specify the opacity of the background rectangle. Enter a value from 0 to
-- 255, where 0 is transparent and 255 is opaque. If Style passthrough
-- (StylePassthrough) is set to enabled, leave blank to pass through the
-- background style information in your input captions to your output
-- captions. If Style passthrough is set to disabled, leave blank to use a
-- value of 0 and remove all backgrounds from your output captions. Within
-- your job settings, all of your DVB-Sub settings must be identical.
dvbSubDestinationSettings_backgroundOpacity :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_backgroundOpacity = Lens.lens (\DvbSubDestinationSettings' {backgroundOpacity} -> backgroundOpacity) (\s@DvbSubDestinationSettings' {} a -> s {backgroundOpacity = a} :: DvbSubDestinationSettings)

-- | Specify how MediaConvert handles the display definition segment (DDS).
-- To exclude the DDS from this set of captions: Keep the default, None. To
-- include the DDS: Choose Specified. When you do, also specify the offset
-- coordinates of the display window with DDS x-coordinate and DDS
-- y-coordinate. To include the DDS, but not include display window data:
-- Choose No display window. When you do, you can write position metadata
-- to the page composition segment (PCS) with DDS x-coordinate and DDS
-- y-coordinate. For video resolutions with a height of 576 pixels or less,
-- MediaConvert doesn\'t include the DDS, regardless of the value you
-- choose for DDS handling. All burn-in and DVB-Sub font settings must
-- match.
dvbSubDestinationSettings_ddsHandling :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbddsHandling)
dvbSubDestinationSettings_ddsHandling = Lens.lens (\DvbSubDestinationSettings' {ddsHandling} -> ddsHandling) (\s@DvbSubDestinationSettings' {} a -> s {ddsHandling = a} :: DvbSubDestinationSettings)

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

-- | Specify the font that you want the service to use for your burn in
-- captions when your input captions specify a font that MediaConvert
-- doesn\'t support. When you set Fallback font (FallbackFont) to best
-- match (BEST_MATCH), or leave blank, MediaConvert uses a supported font
-- that most closely matches the font that your input captions specify.
-- When there are multiple unsupported fonts in your input captions,
-- MediaConvert matches each font with the supported font that matches
-- best. When you explicitly choose a replacement font, MediaConvert uses
-- that font to replace all unsupported fonts from your input.
dvbSubDestinationSettings_fallbackFont :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubSubtitleFallbackFont)
dvbSubDestinationSettings_fallbackFont = Lens.lens (\DvbSubDestinationSettings' {fallbackFont} -> fallbackFont) (\s@DvbSubDestinationSettings' {} a -> s {fallbackFont = a} :: DvbSubDestinationSettings)

-- | Specify the color of the captions text. Leave Font color (FontColor)
-- blank and set Style passthrough (StylePassthrough) to enabled to use the
-- font color data from your input captions, if present. Within your job
-- settings, all of your DVB-Sub settings must be identical.
dvbSubDestinationSettings_fontColor :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubtitleFontColor)
dvbSubDestinationSettings_fontColor = Lens.lens (\DvbSubDestinationSettings' {fontColor} -> fontColor) (\s@DvbSubDestinationSettings' {} a -> s {fontColor = a} :: DvbSubDestinationSettings)

-- | Specify the opacity of the burned-in captions. 255 is opaque; 0 is
-- transparent. Within your job settings, all of your DVB-Sub settings must
-- be identical.
dvbSubDestinationSettings_fontOpacity :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_fontOpacity = Lens.lens (\DvbSubDestinationSettings' {fontOpacity} -> fontOpacity) (\s@DvbSubDestinationSettings' {} a -> s {fontOpacity = a} :: DvbSubDestinationSettings)

-- | Specify the Font resolution (FontResolution) in DPI (dots per inch).
-- Within your job settings, all of your DVB-Sub settings must be
-- identical.
dvbSubDestinationSettings_fontResolution :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_fontResolution = Lens.lens (\DvbSubDestinationSettings' {fontResolution} -> fontResolution) (\s@DvbSubDestinationSettings' {} a -> s {fontResolution = a} :: DvbSubDestinationSettings)

-- | Set Font script (FontScript) to Automatically determined (AUTOMATIC), or
-- leave blank, to automatically determine the font script in your input
-- captions. Otherwise, set to Simplified Chinese (HANS) or Traditional
-- Chinese (HANT) if your input font script uses Simplified or Traditional
-- Chinese. Within your job settings, all of your DVB-Sub settings must be
-- identical.
dvbSubDestinationSettings_fontScript :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe FontScript)
dvbSubDestinationSettings_fontScript = Lens.lens (\DvbSubDestinationSettings' {fontScript} -> fontScript) (\s@DvbSubDestinationSettings' {} a -> s {fontScript = a} :: DvbSubDestinationSettings)

-- | Specify the Font size (FontSize) in pixels. Must be a positive integer.
-- Set to 0, or leave blank, for automatic font size. Within your job
-- settings, all of your DVB-Sub settings must be identical.
dvbSubDestinationSettings_fontSize :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_fontSize = Lens.lens (\DvbSubDestinationSettings' {fontSize} -> fontSize) (\s@DvbSubDestinationSettings' {} a -> s {fontSize = a} :: DvbSubDestinationSettings)

-- | Specify the height, in pixels, of this set of DVB-Sub captions. The
-- default value is 576 pixels. Related setting: When you use this setting,
-- you must set DDS handling (ddsHandling) to a value other than None
-- (NONE). All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_height :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_height = Lens.lens (\DvbSubDestinationSettings' {height} -> height) (\s@DvbSubDestinationSettings' {} a -> s {height = a} :: DvbSubDestinationSettings)

-- | Ignore this setting unless your Font color is set to Hex. Enter either
-- six or eight hexidecimal digits, representing red, green, and blue, with
-- two optional extra digits for alpha. For example a value of 1122AABB is
-- a red value of 0x11, a green value of 0x22, a blue value of 0xAA, and an
-- alpha value of 0xBB.
dvbSubDestinationSettings_hexFontColor :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Text)
dvbSubDestinationSettings_hexFontColor = Lens.lens (\DvbSubDestinationSettings' {hexFontColor} -> hexFontColor) (\s@DvbSubDestinationSettings' {} a -> s {hexFontColor = a} :: DvbSubDestinationSettings)

-- | Specify font outline color. Leave Outline color (OutlineColor) blank and
-- set Style passthrough (StylePassthrough) to enabled to use the font
-- outline color data from your input captions, if present. Within your job
-- settings, all of your DVB-Sub settings must be identical.
dvbSubDestinationSettings_outlineColor :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubtitleOutlineColor)
dvbSubDestinationSettings_outlineColor = Lens.lens (\DvbSubDestinationSettings' {outlineColor} -> outlineColor) (\s@DvbSubDestinationSettings' {} a -> s {outlineColor = a} :: DvbSubDestinationSettings)

-- | Specify the Outline size (OutlineSize) of the caption text, in pixels.
-- Leave Outline size blank and set Style passthrough (StylePassthrough) to
-- enabled to use the outline size data from your input captions, if
-- present. Within your job settings, all of your DVB-Sub settings must be
-- identical.
dvbSubDestinationSettings_outlineSize :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_outlineSize = Lens.lens (\DvbSubDestinationSettings' {outlineSize} -> outlineSize) (\s@DvbSubDestinationSettings' {} a -> s {outlineSize = a} :: DvbSubDestinationSettings)

-- | Specify the color of the shadow cast by the captions. Leave Shadow color
-- (ShadowColor) blank and set Style passthrough (StylePassthrough) to
-- enabled to use the shadow color data from your input captions, if
-- present. Within your job settings, all of your DVB-Sub settings must be
-- identical.
dvbSubDestinationSettings_shadowColor :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubtitleShadowColor)
dvbSubDestinationSettings_shadowColor = Lens.lens (\DvbSubDestinationSettings' {shadowColor} -> shadowColor) (\s@DvbSubDestinationSettings' {} a -> s {shadowColor = a} :: DvbSubDestinationSettings)

-- | Specify the opacity of the shadow. Enter a value from 0 to 255, where 0
-- is transparent and 255 is opaque. If Style passthrough
-- (StylePassthrough) is set to Enabled, leave Shadow opacity
-- (ShadowOpacity) blank to pass through the shadow style information in
-- your input captions to your output captions. If Style passthrough is set
-- to disabled, leave blank to use a value of 0 and remove all shadows from
-- your output captions. Within your job settings, all of your DVB-Sub
-- settings must be identical.
dvbSubDestinationSettings_shadowOpacity :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_shadowOpacity = Lens.lens (\DvbSubDestinationSettings' {shadowOpacity} -> shadowOpacity) (\s@DvbSubDestinationSettings' {} a -> s {shadowOpacity = a} :: DvbSubDestinationSettings)

-- | Specify the horizontal offset of the shadow, relative to the captions in
-- pixels. A value of -2 would result in a shadow offset 2 pixels to the
-- left. Within your job settings, all of your DVB-Sub settings must be
-- identical.
dvbSubDestinationSettings_shadowXOffset :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Int)
dvbSubDestinationSettings_shadowXOffset = Lens.lens (\DvbSubDestinationSettings' {shadowXOffset} -> shadowXOffset) (\s@DvbSubDestinationSettings' {} a -> s {shadowXOffset = a} :: DvbSubDestinationSettings)

-- | Specify the vertical offset of the shadow relative to the captions in
-- pixels. A value of -2 would result in a shadow offset 2 pixels above the
-- text. Leave Shadow y-offset (ShadowYOffset) blank and set Style
-- passthrough (StylePassthrough) to enabled to use the shadow y-offset
-- data from your input captions, if present. Within your job settings, all
-- of your DVB-Sub settings must be identical.
dvbSubDestinationSettings_shadowYOffset :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Int)
dvbSubDestinationSettings_shadowYOffset = Lens.lens (\DvbSubDestinationSettings' {shadowYOffset} -> shadowYOffset) (\s@DvbSubDestinationSettings' {} a -> s {shadowYOffset = a} :: DvbSubDestinationSettings)

-- | Set Style passthrough (StylePassthrough) to ENABLED to use the available
-- style, color, and position information from your input captions.
-- MediaConvert uses default settings for any missing style and position
-- information in your input captions. Set Style passthrough to DISABLED,
-- or leave blank, to ignore the style and position information from your
-- input captions and use default settings: white text with black
-- outlining, bottom-center positioning, and automatic sizing. Whether you
-- set Style passthrough to enabled or not, you can also choose to manually
-- override any of the individual style and position settings.
dvbSubDestinationSettings_stylePassthrough :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubtitleStylePassthrough)
dvbSubDestinationSettings_stylePassthrough = Lens.lens (\DvbSubDestinationSettings' {stylePassthrough} -> stylePassthrough) (\s@DvbSubDestinationSettings' {} a -> s {stylePassthrough = a} :: DvbSubDestinationSettings)

-- | Specify whether your DVB subtitles are standard or for hearing impaired.
-- Choose hearing impaired if your subtitles include audio descriptions and
-- dialogue. Choose standard if your subtitles include only dialogue.
dvbSubDestinationSettings_subtitlingType :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubtitlingType)
dvbSubDestinationSettings_subtitlingType = Lens.lens (\DvbSubDestinationSettings' {subtitlingType} -> subtitlingType) (\s@DvbSubDestinationSettings' {} a -> s {subtitlingType = a} :: DvbSubDestinationSettings)

-- | Specify whether the Text spacing (TeletextSpacing) in your captions is
-- set by the captions grid, or varies depending on letter width. Choose
-- fixed grid (FIXED_GRID) to conform to the spacing specified in the
-- captions file more accurately. Choose proportional (PROPORTIONAL) to
-- make the text easier to read for closed captions. Within your job
-- settings, all of your DVB-Sub settings must be identical.
dvbSubDestinationSettings_teletextSpacing :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubtitleTeletextSpacing)
dvbSubDestinationSettings_teletextSpacing = Lens.lens (\DvbSubDestinationSettings' {teletextSpacing} -> teletextSpacing) (\s@DvbSubDestinationSettings' {} a -> s {teletextSpacing = a} :: DvbSubDestinationSettings)

-- | Specify the width, in pixels, of this set of DVB-Sub captions. The
-- default value is 720 pixels. Related setting: When you use this setting,
-- you must set DDS handling (ddsHandling) to a value other than None
-- (NONE). All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_width :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_width = Lens.lens (\DvbSubDestinationSettings' {width} -> width) (\s@DvbSubDestinationSettings' {} a -> s {width = a} :: DvbSubDestinationSettings)

-- | Specify the horizontal position (XPosition) of the captions, relative to
-- the left side of the outputin pixels. A value of 10 would result in the
-- captions starting 10 pixels from the left ofthe output. If no explicit
-- x_position is provided, the horizontal caption position will
-- bedetermined by the alignment parameter. Within your job settings, all
-- of your DVB-Sub settings must be identical.
dvbSubDestinationSettings_xPosition :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_xPosition = Lens.lens (\DvbSubDestinationSettings' {xPosition} -> xPosition) (\s@DvbSubDestinationSettings' {} a -> s {xPosition = a} :: DvbSubDestinationSettings)

-- | Specify the vertical position (YPosition) of the captions, relative to
-- the top of the output in pixels. A value of 10 would result in the
-- captions starting 10 pixels from the top of the output. If no explicit
-- y_position is provided, the caption will be positioned towards the
-- bottom of the output. Within your job settings, all of your DVB-Sub
-- settings must be identical.
dvbSubDestinationSettings_yPosition :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_yPosition = Lens.lens (\DvbSubDestinationSettings' {yPosition} -> yPosition) (\s@DvbSubDestinationSettings' {} a -> s {yPosition = a} :: DvbSubDestinationSettings)

instance Data.FromJSON DvbSubDestinationSettings where
  parseJSON =
    Data.withObject
      "DvbSubDestinationSettings"
      ( \x ->
          DvbSubDestinationSettings'
            Prelude.<$> (x Data..:? "alignment")
            Prelude.<*> (x Data..:? "applyFontColor")
            Prelude.<*> (x Data..:? "backgroundColor")
            Prelude.<*> (x Data..:? "backgroundOpacity")
            Prelude.<*> (x Data..:? "ddsHandling")
            Prelude.<*> (x Data..:? "ddsXCoordinate")
            Prelude.<*> (x Data..:? "ddsYCoordinate")
            Prelude.<*> (x Data..:? "fallbackFont")
            Prelude.<*> (x Data..:? "fontColor")
            Prelude.<*> (x Data..:? "fontOpacity")
            Prelude.<*> (x Data..:? "fontResolution")
            Prelude.<*> (x Data..:? "fontScript")
            Prelude.<*> (x Data..:? "fontSize")
            Prelude.<*> (x Data..:? "height")
            Prelude.<*> (x Data..:? "hexFontColor")
            Prelude.<*> (x Data..:? "outlineColor")
            Prelude.<*> (x Data..:? "outlineSize")
            Prelude.<*> (x Data..:? "shadowColor")
            Prelude.<*> (x Data..:? "shadowOpacity")
            Prelude.<*> (x Data..:? "shadowXOffset")
            Prelude.<*> (x Data..:? "shadowYOffset")
            Prelude.<*> (x Data..:? "stylePassthrough")
            Prelude.<*> (x Data..:? "subtitlingType")
            Prelude.<*> (x Data..:? "teletextSpacing")
            Prelude.<*> (x Data..:? "width")
            Prelude.<*> (x Data..:? "xPosition")
            Prelude.<*> (x Data..:? "yPosition")
      )

instance Prelude.Hashable DvbSubDestinationSettings where
  hashWithSalt _salt DvbSubDestinationSettings' {..} =
    _salt
      `Prelude.hashWithSalt` alignment
      `Prelude.hashWithSalt` applyFontColor
      `Prelude.hashWithSalt` backgroundColor
      `Prelude.hashWithSalt` backgroundOpacity
      `Prelude.hashWithSalt` ddsHandling
      `Prelude.hashWithSalt` ddsXCoordinate
      `Prelude.hashWithSalt` ddsYCoordinate
      `Prelude.hashWithSalt` fallbackFont
      `Prelude.hashWithSalt` fontColor
      `Prelude.hashWithSalt` fontOpacity
      `Prelude.hashWithSalt` fontResolution
      `Prelude.hashWithSalt` fontScript
      `Prelude.hashWithSalt` fontSize
      `Prelude.hashWithSalt` height
      `Prelude.hashWithSalt` hexFontColor
      `Prelude.hashWithSalt` outlineColor
      `Prelude.hashWithSalt` outlineSize
      `Prelude.hashWithSalt` shadowColor
      `Prelude.hashWithSalt` shadowOpacity
      `Prelude.hashWithSalt` shadowXOffset
      `Prelude.hashWithSalt` shadowYOffset
      `Prelude.hashWithSalt` stylePassthrough
      `Prelude.hashWithSalt` subtitlingType
      `Prelude.hashWithSalt` teletextSpacing
      `Prelude.hashWithSalt` width
      `Prelude.hashWithSalt` xPosition
      `Prelude.hashWithSalt` yPosition

instance Prelude.NFData DvbSubDestinationSettings where
  rnf DvbSubDestinationSettings' {..} =
    Prelude.rnf alignment
      `Prelude.seq` Prelude.rnf applyFontColor
      `Prelude.seq` Prelude.rnf backgroundColor
      `Prelude.seq` Prelude.rnf backgroundOpacity
      `Prelude.seq` Prelude.rnf ddsHandling
      `Prelude.seq` Prelude.rnf ddsXCoordinate
      `Prelude.seq` Prelude.rnf ddsYCoordinate
      `Prelude.seq` Prelude.rnf fallbackFont
      `Prelude.seq` Prelude.rnf fontColor
      `Prelude.seq` Prelude.rnf fontOpacity
      `Prelude.seq` Prelude.rnf fontResolution
      `Prelude.seq` Prelude.rnf fontScript
      `Prelude.seq` Prelude.rnf fontSize
      `Prelude.seq` Prelude.rnf height
      `Prelude.seq` Prelude.rnf hexFontColor
      `Prelude.seq` Prelude.rnf outlineColor
      `Prelude.seq` Prelude.rnf outlineSize
      `Prelude.seq` Prelude.rnf shadowColor
      `Prelude.seq` Prelude.rnf shadowOpacity
      `Prelude.seq` Prelude.rnf shadowXOffset
      `Prelude.seq` Prelude.rnf shadowYOffset
      `Prelude.seq` Prelude.rnf
        stylePassthrough
      `Prelude.seq` Prelude.rnf
        subtitlingType
      `Prelude.seq` Prelude.rnf
        teletextSpacing
      `Prelude.seq` Prelude.rnf width
      `Prelude.seq` Prelude.rnf
        xPosition
      `Prelude.seq` Prelude.rnf
        yPosition

instance Data.ToJSON DvbSubDestinationSettings where
  toJSON DvbSubDestinationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("alignment" Data..=) Prelude.<$> alignment,
            ("applyFontColor" Data..=)
              Prelude.<$> applyFontColor,
            ("backgroundColor" Data..=)
              Prelude.<$> backgroundColor,
            ("backgroundOpacity" Data..=)
              Prelude.<$> backgroundOpacity,
            ("ddsHandling" Data..=) Prelude.<$> ddsHandling,
            ("ddsXCoordinate" Data..=)
              Prelude.<$> ddsXCoordinate,
            ("ddsYCoordinate" Data..=)
              Prelude.<$> ddsYCoordinate,
            ("fallbackFont" Data..=) Prelude.<$> fallbackFont,
            ("fontColor" Data..=) Prelude.<$> fontColor,
            ("fontOpacity" Data..=) Prelude.<$> fontOpacity,
            ("fontResolution" Data..=)
              Prelude.<$> fontResolution,
            ("fontScript" Data..=) Prelude.<$> fontScript,
            ("fontSize" Data..=) Prelude.<$> fontSize,
            ("height" Data..=) Prelude.<$> height,
            ("hexFontColor" Data..=) Prelude.<$> hexFontColor,
            ("outlineColor" Data..=) Prelude.<$> outlineColor,
            ("outlineSize" Data..=) Prelude.<$> outlineSize,
            ("shadowColor" Data..=) Prelude.<$> shadowColor,
            ("shadowOpacity" Data..=) Prelude.<$> shadowOpacity,
            ("shadowXOffset" Data..=) Prelude.<$> shadowXOffset,
            ("shadowYOffset" Data..=) Prelude.<$> shadowYOffset,
            ("stylePassthrough" Data..=)
              Prelude.<$> stylePassthrough,
            ("subtitlingType" Data..=)
              Prelude.<$> subtitlingType,
            ("teletextSpacing" Data..=)
              Prelude.<$> teletextSpacing,
            ("width" Data..=) Prelude.<$> width,
            ("xPosition" Data..=) Prelude.<$> xPosition,
            ("yPosition" Data..=) Prelude.<$> yPosition
          ]
      )
