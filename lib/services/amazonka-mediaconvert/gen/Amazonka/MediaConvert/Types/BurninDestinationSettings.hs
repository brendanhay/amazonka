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
-- Module      : Amazonka.MediaConvert.Types.BurninDestinationSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.BurninDestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.BurnInSubtitleStylePassthrough
import Amazonka.MediaConvert.Types.BurninSubtitleAlignment
import Amazonka.MediaConvert.Types.BurninSubtitleApplyFontColor
import Amazonka.MediaConvert.Types.BurninSubtitleBackgroundColor
import Amazonka.MediaConvert.Types.BurninSubtitleFallbackFont
import Amazonka.MediaConvert.Types.BurninSubtitleFontColor
import Amazonka.MediaConvert.Types.BurninSubtitleOutlineColor
import Amazonka.MediaConvert.Types.BurninSubtitleShadowColor
import Amazonka.MediaConvert.Types.BurninSubtitleTeletextSpacing
import Amazonka.MediaConvert.Types.FontScript
import qualified Amazonka.Prelude as Prelude

-- | Burn-in is a captions delivery method, rather than a captions format.
-- Burn-in writes the captions directly on your video frames, replacing
-- pixels of video content with the captions. Set up burn-in captions in
-- the same output as your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/burn-in-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to
-- BURN_IN.
--
-- /See:/ 'newBurninDestinationSettings' smart constructor.
data BurninDestinationSettings = BurninDestinationSettings'
  { -- | Specify the alignment of your captions. If no explicit x_position is
    -- provided, setting alignment to centered will placethe captions at the
    -- bottom center of the output. Similarly, setting a left alignment
    -- willalign captions to the bottom left of the output. If x and y
    -- positions are given in conjunction with the alignment parameter, the
    -- font will be justified (either left or centered) relative to those
    -- coordinates.
    alignment :: Prelude.Maybe BurninSubtitleAlignment,
    -- | Ignore this setting unless Style passthrough (StylePassthrough) is set
    -- to Enabled and Font color (FontColor) set to Black, Yellow, Red, Green,
    -- Blue, or Hex. Use Apply font color (ApplyFontColor) for additional font
    -- color controls. When you choose White text only (WHITE_TEXT_ONLY), or
    -- leave blank, your font color setting only applies to white text in your
    -- input captions. For example, if your font color setting is Yellow, and
    -- your input captions have red and white text, your output captions will
    -- have red and yellow text. When you choose ALL_TEXT, your font color
    -- setting applies to all of your output captions text.
    applyFontColor :: Prelude.Maybe BurninSubtitleApplyFontColor,
    -- | Specify the color of the rectangle behind the captions. Leave background
    -- color (BackgroundColor) blank and set Style passthrough
    -- (StylePassthrough) to enabled to use the background color data from your
    -- input captions, if present.
    backgroundColor :: Prelude.Maybe BurninSubtitleBackgroundColor,
    -- | Specify the opacity of the background rectangle. Enter a value from 0 to
    -- 255, where 0 is transparent and 255 is opaque. If Style passthrough
    -- (StylePassthrough) is set to enabled, leave blank to pass through the
    -- background style information in your input captions to your output
    -- captions. If Style passthrough is set to disabled, leave blank to use a
    -- value of 0 and remove all backgrounds from your output captions.
    backgroundOpacity :: Prelude.Maybe Prelude.Natural,
    -- | Specify the font that you want the service to use for your burn in
    -- captions when your input captions specify a font that MediaConvert
    -- doesn\'t support. When you set Fallback font (FallbackFont) to best
    -- match (BEST_MATCH), or leave blank, MediaConvert uses a supported font
    -- that most closely matches the font that your input captions specify.
    -- When there are multiple unsupported fonts in your input captions,
    -- MediaConvert matches each font with the supported font that matches
    -- best. When you explicitly choose a replacement font, MediaConvert uses
    -- that font to replace all unsupported fonts from your input.
    fallbackFont :: Prelude.Maybe BurninSubtitleFallbackFont,
    -- | Specify the color of the burned-in captions text. Leave Font color
    -- (FontColor) blank and set Style passthrough (StylePassthrough) to
    -- enabled to use the font color data from your input captions, if present.
    fontColor :: Prelude.Maybe BurninSubtitleFontColor,
    -- | Specify the opacity of the burned-in captions. 255 is opaque; 0 is
    -- transparent.
    fontOpacity :: Prelude.Maybe Prelude.Natural,
    -- | Specify the Font resolution (FontResolution) in DPI (dots per inch).
    fontResolution :: Prelude.Maybe Prelude.Natural,
    -- | Set Font script (FontScript) to Automatically determined (AUTOMATIC), or
    -- leave blank, to automatically determine the font script in your input
    -- captions. Otherwise, set to Simplified Chinese (HANS) or Traditional
    -- Chinese (HANT) if your input font script uses Simplified or Traditional
    -- Chinese.
    fontScript :: Prelude.Maybe FontScript,
    -- | Specify the Font size (FontSize) in pixels. Must be a positive integer.
    -- Set to 0, or leave blank, for automatic font size.
    fontSize :: Prelude.Maybe Prelude.Natural,
    -- | Ignore this setting unless your Font color is set to Hex. Enter either
    -- six or eight hexidecimal digits, representing red, green, and blue, with
    -- two optional extra digits for alpha. For example a value of 1122AABB is
    -- a red value of 0x11, a green value of 0x22, a blue value of 0xAA, and an
    -- alpha value of 0xBB.
    hexFontColor :: Prelude.Maybe Prelude.Text,
    -- | Specify font outline color. Leave Outline color (OutlineColor) blank and
    -- set Style passthrough (StylePassthrough) to enabled to use the font
    -- outline color data from your input captions, if present.
    outlineColor :: Prelude.Maybe BurninSubtitleOutlineColor,
    -- | Specify the Outline size (OutlineSize) of the caption text, in pixels.
    -- Leave Outline size blank and set Style passthrough (StylePassthrough) to
    -- enabled to use the outline size data from your input captions, if
    -- present.
    outlineSize :: Prelude.Maybe Prelude.Natural,
    -- | Specify the color of the shadow cast by the captions. Leave Shadow color
    -- (ShadowColor) blank and set Style passthrough (StylePassthrough) to
    -- enabled to use the shadow color data from your input captions, if
    -- present.
    shadowColor :: Prelude.Maybe BurninSubtitleShadowColor,
    -- | Specify the opacity of the shadow. Enter a value from 0 to 255, where 0
    -- is transparent and 255 is opaque. If Style passthrough
    -- (StylePassthrough) is set to Enabled, leave Shadow opacity
    -- (ShadowOpacity) blank to pass through the shadow style information in
    -- your input captions to your output captions. If Style passthrough is set
    -- to disabled, leave blank to use a value of 0 and remove all shadows from
    -- your output captions.
    shadowOpacity :: Prelude.Maybe Prelude.Natural,
    -- | Specify the horizontal offset of the shadow, relative to the captions in
    -- pixels. A value of -2 would result in a shadow offset 2 pixels to the
    -- left.
    shadowXOffset :: Prelude.Maybe Prelude.Int,
    -- | Specify the vertical offset of the shadow relative to the captions in
    -- pixels. A value of -2 would result in a shadow offset 2 pixels above the
    -- text. Leave Shadow y-offset (ShadowYOffset) blank and set Style
    -- passthrough (StylePassthrough) to enabled to use the shadow y-offset
    -- data from your input captions, if present.
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
    stylePassthrough :: Prelude.Maybe BurnInSubtitleStylePassthrough,
    -- | Specify whether the text spacing (TeletextSpacing) in your captions is
    -- set by the captions grid, or varies depending on letter width. Choose
    -- fixed grid (FIXED_GRID) to conform to the spacing specified in the
    -- captions file more accurately. Choose proportional (PROPORTIONAL) to
    -- make the text easier to read for closed captions.
    teletextSpacing :: Prelude.Maybe BurninSubtitleTeletextSpacing,
    -- | Specify the horizontal position (XPosition) of the captions, relative to
    -- the left side of the output in pixels. A value of 10 would result in the
    -- captions starting 10 pixels from the left of the output. If no explicit
    -- x_position is provided, the horizontal caption position will be
    -- determined by the alignment parameter.
    xPosition :: Prelude.Maybe Prelude.Natural,
    -- | Specify the vertical position (YPosition) of the captions, relative to
    -- the top of the output in pixels. A value of 10 would result in the
    -- captions starting 10 pixels from the top of the output. If no explicit
    -- y_position is provided, the caption will be positioned towards the
    -- bottom of the output.
    yPosition :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BurninDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alignment', 'burninDestinationSettings_alignment' - Specify the alignment of your captions. If no explicit x_position is
-- provided, setting alignment to centered will placethe captions at the
-- bottom center of the output. Similarly, setting a left alignment
-- willalign captions to the bottom left of the output. If x and y
-- positions are given in conjunction with the alignment parameter, the
-- font will be justified (either left or centered) relative to those
-- coordinates.
--
-- 'applyFontColor', 'burninDestinationSettings_applyFontColor' - Ignore this setting unless Style passthrough (StylePassthrough) is set
-- to Enabled and Font color (FontColor) set to Black, Yellow, Red, Green,
-- Blue, or Hex. Use Apply font color (ApplyFontColor) for additional font
-- color controls. When you choose White text only (WHITE_TEXT_ONLY), or
-- leave blank, your font color setting only applies to white text in your
-- input captions. For example, if your font color setting is Yellow, and
-- your input captions have red and white text, your output captions will
-- have red and yellow text. When you choose ALL_TEXT, your font color
-- setting applies to all of your output captions text.
--
-- 'backgroundColor', 'burninDestinationSettings_backgroundColor' - Specify the color of the rectangle behind the captions. Leave background
-- color (BackgroundColor) blank and set Style passthrough
-- (StylePassthrough) to enabled to use the background color data from your
-- input captions, if present.
--
-- 'backgroundOpacity', 'burninDestinationSettings_backgroundOpacity' - Specify the opacity of the background rectangle. Enter a value from 0 to
-- 255, where 0 is transparent and 255 is opaque. If Style passthrough
-- (StylePassthrough) is set to enabled, leave blank to pass through the
-- background style information in your input captions to your output
-- captions. If Style passthrough is set to disabled, leave blank to use a
-- value of 0 and remove all backgrounds from your output captions.
--
-- 'fallbackFont', 'burninDestinationSettings_fallbackFont' - Specify the font that you want the service to use for your burn in
-- captions when your input captions specify a font that MediaConvert
-- doesn\'t support. When you set Fallback font (FallbackFont) to best
-- match (BEST_MATCH), or leave blank, MediaConvert uses a supported font
-- that most closely matches the font that your input captions specify.
-- When there are multiple unsupported fonts in your input captions,
-- MediaConvert matches each font with the supported font that matches
-- best. When you explicitly choose a replacement font, MediaConvert uses
-- that font to replace all unsupported fonts from your input.
--
-- 'fontColor', 'burninDestinationSettings_fontColor' - Specify the color of the burned-in captions text. Leave Font color
-- (FontColor) blank and set Style passthrough (StylePassthrough) to
-- enabled to use the font color data from your input captions, if present.
--
-- 'fontOpacity', 'burninDestinationSettings_fontOpacity' - Specify the opacity of the burned-in captions. 255 is opaque; 0 is
-- transparent.
--
-- 'fontResolution', 'burninDestinationSettings_fontResolution' - Specify the Font resolution (FontResolution) in DPI (dots per inch).
--
-- 'fontScript', 'burninDestinationSettings_fontScript' - Set Font script (FontScript) to Automatically determined (AUTOMATIC), or
-- leave blank, to automatically determine the font script in your input
-- captions. Otherwise, set to Simplified Chinese (HANS) or Traditional
-- Chinese (HANT) if your input font script uses Simplified or Traditional
-- Chinese.
--
-- 'fontSize', 'burninDestinationSettings_fontSize' - Specify the Font size (FontSize) in pixels. Must be a positive integer.
-- Set to 0, or leave blank, for automatic font size.
--
-- 'hexFontColor', 'burninDestinationSettings_hexFontColor' - Ignore this setting unless your Font color is set to Hex. Enter either
-- six or eight hexidecimal digits, representing red, green, and blue, with
-- two optional extra digits for alpha. For example a value of 1122AABB is
-- a red value of 0x11, a green value of 0x22, a blue value of 0xAA, and an
-- alpha value of 0xBB.
--
-- 'outlineColor', 'burninDestinationSettings_outlineColor' - Specify font outline color. Leave Outline color (OutlineColor) blank and
-- set Style passthrough (StylePassthrough) to enabled to use the font
-- outline color data from your input captions, if present.
--
-- 'outlineSize', 'burninDestinationSettings_outlineSize' - Specify the Outline size (OutlineSize) of the caption text, in pixels.
-- Leave Outline size blank and set Style passthrough (StylePassthrough) to
-- enabled to use the outline size data from your input captions, if
-- present.
--
-- 'shadowColor', 'burninDestinationSettings_shadowColor' - Specify the color of the shadow cast by the captions. Leave Shadow color
-- (ShadowColor) blank and set Style passthrough (StylePassthrough) to
-- enabled to use the shadow color data from your input captions, if
-- present.
--
-- 'shadowOpacity', 'burninDestinationSettings_shadowOpacity' - Specify the opacity of the shadow. Enter a value from 0 to 255, where 0
-- is transparent and 255 is opaque. If Style passthrough
-- (StylePassthrough) is set to Enabled, leave Shadow opacity
-- (ShadowOpacity) blank to pass through the shadow style information in
-- your input captions to your output captions. If Style passthrough is set
-- to disabled, leave blank to use a value of 0 and remove all shadows from
-- your output captions.
--
-- 'shadowXOffset', 'burninDestinationSettings_shadowXOffset' - Specify the horizontal offset of the shadow, relative to the captions in
-- pixels. A value of -2 would result in a shadow offset 2 pixels to the
-- left.
--
-- 'shadowYOffset', 'burninDestinationSettings_shadowYOffset' - Specify the vertical offset of the shadow relative to the captions in
-- pixels. A value of -2 would result in a shadow offset 2 pixels above the
-- text. Leave Shadow y-offset (ShadowYOffset) blank and set Style
-- passthrough (StylePassthrough) to enabled to use the shadow y-offset
-- data from your input captions, if present.
--
-- 'stylePassthrough', 'burninDestinationSettings_stylePassthrough' - Set Style passthrough (StylePassthrough) to ENABLED to use the available
-- style, color, and position information from your input captions.
-- MediaConvert uses default settings for any missing style and position
-- information in your input captions. Set Style passthrough to DISABLED,
-- or leave blank, to ignore the style and position information from your
-- input captions and use default settings: white text with black
-- outlining, bottom-center positioning, and automatic sizing. Whether you
-- set Style passthrough to enabled or not, you can also choose to manually
-- override any of the individual style and position settings.
--
-- 'teletextSpacing', 'burninDestinationSettings_teletextSpacing' - Specify whether the text spacing (TeletextSpacing) in your captions is
-- set by the captions grid, or varies depending on letter width. Choose
-- fixed grid (FIXED_GRID) to conform to the spacing specified in the
-- captions file more accurately. Choose proportional (PROPORTIONAL) to
-- make the text easier to read for closed captions.
--
-- 'xPosition', 'burninDestinationSettings_xPosition' - Specify the horizontal position (XPosition) of the captions, relative to
-- the left side of the output in pixels. A value of 10 would result in the
-- captions starting 10 pixels from the left of the output. If no explicit
-- x_position is provided, the horizontal caption position will be
-- determined by the alignment parameter.
--
-- 'yPosition', 'burninDestinationSettings_yPosition' - Specify the vertical position (YPosition) of the captions, relative to
-- the top of the output in pixels. A value of 10 would result in the
-- captions starting 10 pixels from the top of the output. If no explicit
-- y_position is provided, the caption will be positioned towards the
-- bottom of the output.
newBurninDestinationSettings ::
  BurninDestinationSettings
newBurninDestinationSettings =
  BurninDestinationSettings'
    { alignment =
        Prelude.Nothing,
      applyFontColor = Prelude.Nothing,
      backgroundColor = Prelude.Nothing,
      backgroundOpacity = Prelude.Nothing,
      fallbackFont = Prelude.Nothing,
      fontColor = Prelude.Nothing,
      fontOpacity = Prelude.Nothing,
      fontResolution = Prelude.Nothing,
      fontScript = Prelude.Nothing,
      fontSize = Prelude.Nothing,
      hexFontColor = Prelude.Nothing,
      outlineColor = Prelude.Nothing,
      outlineSize = Prelude.Nothing,
      shadowColor = Prelude.Nothing,
      shadowOpacity = Prelude.Nothing,
      shadowXOffset = Prelude.Nothing,
      shadowYOffset = Prelude.Nothing,
      stylePassthrough = Prelude.Nothing,
      teletextSpacing = Prelude.Nothing,
      xPosition = Prelude.Nothing,
      yPosition = Prelude.Nothing
    }

-- | Specify the alignment of your captions. If no explicit x_position is
-- provided, setting alignment to centered will placethe captions at the
-- bottom center of the output. Similarly, setting a left alignment
-- willalign captions to the bottom left of the output. If x and y
-- positions are given in conjunction with the alignment parameter, the
-- font will be justified (either left or centered) relative to those
-- coordinates.
burninDestinationSettings_alignment :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe BurninSubtitleAlignment)
burninDestinationSettings_alignment = Lens.lens (\BurninDestinationSettings' {alignment} -> alignment) (\s@BurninDestinationSettings' {} a -> s {alignment = a} :: BurninDestinationSettings)

-- | Ignore this setting unless Style passthrough (StylePassthrough) is set
-- to Enabled and Font color (FontColor) set to Black, Yellow, Red, Green,
-- Blue, or Hex. Use Apply font color (ApplyFontColor) for additional font
-- color controls. When you choose White text only (WHITE_TEXT_ONLY), or
-- leave blank, your font color setting only applies to white text in your
-- input captions. For example, if your font color setting is Yellow, and
-- your input captions have red and white text, your output captions will
-- have red and yellow text. When you choose ALL_TEXT, your font color
-- setting applies to all of your output captions text.
burninDestinationSettings_applyFontColor :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe BurninSubtitleApplyFontColor)
burninDestinationSettings_applyFontColor = Lens.lens (\BurninDestinationSettings' {applyFontColor} -> applyFontColor) (\s@BurninDestinationSettings' {} a -> s {applyFontColor = a} :: BurninDestinationSettings)

-- | Specify the color of the rectangle behind the captions. Leave background
-- color (BackgroundColor) blank and set Style passthrough
-- (StylePassthrough) to enabled to use the background color data from your
-- input captions, if present.
burninDestinationSettings_backgroundColor :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe BurninSubtitleBackgroundColor)
burninDestinationSettings_backgroundColor = Lens.lens (\BurninDestinationSettings' {backgroundColor} -> backgroundColor) (\s@BurninDestinationSettings' {} a -> s {backgroundColor = a} :: BurninDestinationSettings)

-- | Specify the opacity of the background rectangle. Enter a value from 0 to
-- 255, where 0 is transparent and 255 is opaque. If Style passthrough
-- (StylePassthrough) is set to enabled, leave blank to pass through the
-- background style information in your input captions to your output
-- captions. If Style passthrough is set to disabled, leave blank to use a
-- value of 0 and remove all backgrounds from your output captions.
burninDestinationSettings_backgroundOpacity :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe Prelude.Natural)
burninDestinationSettings_backgroundOpacity = Lens.lens (\BurninDestinationSettings' {backgroundOpacity} -> backgroundOpacity) (\s@BurninDestinationSettings' {} a -> s {backgroundOpacity = a} :: BurninDestinationSettings)

-- | Specify the font that you want the service to use for your burn in
-- captions when your input captions specify a font that MediaConvert
-- doesn\'t support. When you set Fallback font (FallbackFont) to best
-- match (BEST_MATCH), or leave blank, MediaConvert uses a supported font
-- that most closely matches the font that your input captions specify.
-- When there are multiple unsupported fonts in your input captions,
-- MediaConvert matches each font with the supported font that matches
-- best. When you explicitly choose a replacement font, MediaConvert uses
-- that font to replace all unsupported fonts from your input.
burninDestinationSettings_fallbackFont :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe BurninSubtitleFallbackFont)
burninDestinationSettings_fallbackFont = Lens.lens (\BurninDestinationSettings' {fallbackFont} -> fallbackFont) (\s@BurninDestinationSettings' {} a -> s {fallbackFont = a} :: BurninDestinationSettings)

-- | Specify the color of the burned-in captions text. Leave Font color
-- (FontColor) blank and set Style passthrough (StylePassthrough) to
-- enabled to use the font color data from your input captions, if present.
burninDestinationSettings_fontColor :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe BurninSubtitleFontColor)
burninDestinationSettings_fontColor = Lens.lens (\BurninDestinationSettings' {fontColor} -> fontColor) (\s@BurninDestinationSettings' {} a -> s {fontColor = a} :: BurninDestinationSettings)

-- | Specify the opacity of the burned-in captions. 255 is opaque; 0 is
-- transparent.
burninDestinationSettings_fontOpacity :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe Prelude.Natural)
burninDestinationSettings_fontOpacity = Lens.lens (\BurninDestinationSettings' {fontOpacity} -> fontOpacity) (\s@BurninDestinationSettings' {} a -> s {fontOpacity = a} :: BurninDestinationSettings)

-- | Specify the Font resolution (FontResolution) in DPI (dots per inch).
burninDestinationSettings_fontResolution :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe Prelude.Natural)
burninDestinationSettings_fontResolution = Lens.lens (\BurninDestinationSettings' {fontResolution} -> fontResolution) (\s@BurninDestinationSettings' {} a -> s {fontResolution = a} :: BurninDestinationSettings)

-- | Set Font script (FontScript) to Automatically determined (AUTOMATIC), or
-- leave blank, to automatically determine the font script in your input
-- captions. Otherwise, set to Simplified Chinese (HANS) or Traditional
-- Chinese (HANT) if your input font script uses Simplified or Traditional
-- Chinese.
burninDestinationSettings_fontScript :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe FontScript)
burninDestinationSettings_fontScript = Lens.lens (\BurninDestinationSettings' {fontScript} -> fontScript) (\s@BurninDestinationSettings' {} a -> s {fontScript = a} :: BurninDestinationSettings)

-- | Specify the Font size (FontSize) in pixels. Must be a positive integer.
-- Set to 0, or leave blank, for automatic font size.
burninDestinationSettings_fontSize :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe Prelude.Natural)
burninDestinationSettings_fontSize = Lens.lens (\BurninDestinationSettings' {fontSize} -> fontSize) (\s@BurninDestinationSettings' {} a -> s {fontSize = a} :: BurninDestinationSettings)

-- | Ignore this setting unless your Font color is set to Hex. Enter either
-- six or eight hexidecimal digits, representing red, green, and blue, with
-- two optional extra digits for alpha. For example a value of 1122AABB is
-- a red value of 0x11, a green value of 0x22, a blue value of 0xAA, and an
-- alpha value of 0xBB.
burninDestinationSettings_hexFontColor :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe Prelude.Text)
burninDestinationSettings_hexFontColor = Lens.lens (\BurninDestinationSettings' {hexFontColor} -> hexFontColor) (\s@BurninDestinationSettings' {} a -> s {hexFontColor = a} :: BurninDestinationSettings)

-- | Specify font outline color. Leave Outline color (OutlineColor) blank and
-- set Style passthrough (StylePassthrough) to enabled to use the font
-- outline color data from your input captions, if present.
burninDestinationSettings_outlineColor :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe BurninSubtitleOutlineColor)
burninDestinationSettings_outlineColor = Lens.lens (\BurninDestinationSettings' {outlineColor} -> outlineColor) (\s@BurninDestinationSettings' {} a -> s {outlineColor = a} :: BurninDestinationSettings)

-- | Specify the Outline size (OutlineSize) of the caption text, in pixels.
-- Leave Outline size blank and set Style passthrough (StylePassthrough) to
-- enabled to use the outline size data from your input captions, if
-- present.
burninDestinationSettings_outlineSize :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe Prelude.Natural)
burninDestinationSettings_outlineSize = Lens.lens (\BurninDestinationSettings' {outlineSize} -> outlineSize) (\s@BurninDestinationSettings' {} a -> s {outlineSize = a} :: BurninDestinationSettings)

-- | Specify the color of the shadow cast by the captions. Leave Shadow color
-- (ShadowColor) blank and set Style passthrough (StylePassthrough) to
-- enabled to use the shadow color data from your input captions, if
-- present.
burninDestinationSettings_shadowColor :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe BurninSubtitleShadowColor)
burninDestinationSettings_shadowColor = Lens.lens (\BurninDestinationSettings' {shadowColor} -> shadowColor) (\s@BurninDestinationSettings' {} a -> s {shadowColor = a} :: BurninDestinationSettings)

-- | Specify the opacity of the shadow. Enter a value from 0 to 255, where 0
-- is transparent and 255 is opaque. If Style passthrough
-- (StylePassthrough) is set to Enabled, leave Shadow opacity
-- (ShadowOpacity) blank to pass through the shadow style information in
-- your input captions to your output captions. If Style passthrough is set
-- to disabled, leave blank to use a value of 0 and remove all shadows from
-- your output captions.
burninDestinationSettings_shadowOpacity :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe Prelude.Natural)
burninDestinationSettings_shadowOpacity = Lens.lens (\BurninDestinationSettings' {shadowOpacity} -> shadowOpacity) (\s@BurninDestinationSettings' {} a -> s {shadowOpacity = a} :: BurninDestinationSettings)

-- | Specify the horizontal offset of the shadow, relative to the captions in
-- pixels. A value of -2 would result in a shadow offset 2 pixels to the
-- left.
burninDestinationSettings_shadowXOffset :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe Prelude.Int)
burninDestinationSettings_shadowXOffset = Lens.lens (\BurninDestinationSettings' {shadowXOffset} -> shadowXOffset) (\s@BurninDestinationSettings' {} a -> s {shadowXOffset = a} :: BurninDestinationSettings)

-- | Specify the vertical offset of the shadow relative to the captions in
-- pixels. A value of -2 would result in a shadow offset 2 pixels above the
-- text. Leave Shadow y-offset (ShadowYOffset) blank and set Style
-- passthrough (StylePassthrough) to enabled to use the shadow y-offset
-- data from your input captions, if present.
burninDestinationSettings_shadowYOffset :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe Prelude.Int)
burninDestinationSettings_shadowYOffset = Lens.lens (\BurninDestinationSettings' {shadowYOffset} -> shadowYOffset) (\s@BurninDestinationSettings' {} a -> s {shadowYOffset = a} :: BurninDestinationSettings)

-- | Set Style passthrough (StylePassthrough) to ENABLED to use the available
-- style, color, and position information from your input captions.
-- MediaConvert uses default settings for any missing style and position
-- information in your input captions. Set Style passthrough to DISABLED,
-- or leave blank, to ignore the style and position information from your
-- input captions and use default settings: white text with black
-- outlining, bottom-center positioning, and automatic sizing. Whether you
-- set Style passthrough to enabled or not, you can also choose to manually
-- override any of the individual style and position settings.
burninDestinationSettings_stylePassthrough :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe BurnInSubtitleStylePassthrough)
burninDestinationSettings_stylePassthrough = Lens.lens (\BurninDestinationSettings' {stylePassthrough} -> stylePassthrough) (\s@BurninDestinationSettings' {} a -> s {stylePassthrough = a} :: BurninDestinationSettings)

-- | Specify whether the text spacing (TeletextSpacing) in your captions is
-- set by the captions grid, or varies depending on letter width. Choose
-- fixed grid (FIXED_GRID) to conform to the spacing specified in the
-- captions file more accurately. Choose proportional (PROPORTIONAL) to
-- make the text easier to read for closed captions.
burninDestinationSettings_teletextSpacing :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe BurninSubtitleTeletextSpacing)
burninDestinationSettings_teletextSpacing = Lens.lens (\BurninDestinationSettings' {teletextSpacing} -> teletextSpacing) (\s@BurninDestinationSettings' {} a -> s {teletextSpacing = a} :: BurninDestinationSettings)

-- | Specify the horizontal position (XPosition) of the captions, relative to
-- the left side of the output in pixels. A value of 10 would result in the
-- captions starting 10 pixels from the left of the output. If no explicit
-- x_position is provided, the horizontal caption position will be
-- determined by the alignment parameter.
burninDestinationSettings_xPosition :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe Prelude.Natural)
burninDestinationSettings_xPosition = Lens.lens (\BurninDestinationSettings' {xPosition} -> xPosition) (\s@BurninDestinationSettings' {} a -> s {xPosition = a} :: BurninDestinationSettings)

-- | Specify the vertical position (YPosition) of the captions, relative to
-- the top of the output in pixels. A value of 10 would result in the
-- captions starting 10 pixels from the top of the output. If no explicit
-- y_position is provided, the caption will be positioned towards the
-- bottom of the output.
burninDestinationSettings_yPosition :: Lens.Lens' BurninDestinationSettings (Prelude.Maybe Prelude.Natural)
burninDestinationSettings_yPosition = Lens.lens (\BurninDestinationSettings' {yPosition} -> yPosition) (\s@BurninDestinationSettings' {} a -> s {yPosition = a} :: BurninDestinationSettings)

instance Data.FromJSON BurninDestinationSettings where
  parseJSON =
    Data.withObject
      "BurninDestinationSettings"
      ( \x ->
          BurninDestinationSettings'
            Prelude.<$> (x Data..:? "alignment")
            Prelude.<*> (x Data..:? "applyFontColor")
            Prelude.<*> (x Data..:? "backgroundColor")
            Prelude.<*> (x Data..:? "backgroundOpacity")
            Prelude.<*> (x Data..:? "fallbackFont")
            Prelude.<*> (x Data..:? "fontColor")
            Prelude.<*> (x Data..:? "fontOpacity")
            Prelude.<*> (x Data..:? "fontResolution")
            Prelude.<*> (x Data..:? "fontScript")
            Prelude.<*> (x Data..:? "fontSize")
            Prelude.<*> (x Data..:? "hexFontColor")
            Prelude.<*> (x Data..:? "outlineColor")
            Prelude.<*> (x Data..:? "outlineSize")
            Prelude.<*> (x Data..:? "shadowColor")
            Prelude.<*> (x Data..:? "shadowOpacity")
            Prelude.<*> (x Data..:? "shadowXOffset")
            Prelude.<*> (x Data..:? "shadowYOffset")
            Prelude.<*> (x Data..:? "stylePassthrough")
            Prelude.<*> (x Data..:? "teletextSpacing")
            Prelude.<*> (x Data..:? "xPosition")
            Prelude.<*> (x Data..:? "yPosition")
      )

instance Prelude.Hashable BurninDestinationSettings where
  hashWithSalt _salt BurninDestinationSettings' {..} =
    _salt
      `Prelude.hashWithSalt` alignment
      `Prelude.hashWithSalt` applyFontColor
      `Prelude.hashWithSalt` backgroundColor
      `Prelude.hashWithSalt` backgroundOpacity
      `Prelude.hashWithSalt` fallbackFont
      `Prelude.hashWithSalt` fontColor
      `Prelude.hashWithSalt` fontOpacity
      `Prelude.hashWithSalt` fontResolution
      `Prelude.hashWithSalt` fontScript
      `Prelude.hashWithSalt` fontSize
      `Prelude.hashWithSalt` hexFontColor
      `Prelude.hashWithSalt` outlineColor
      `Prelude.hashWithSalt` outlineSize
      `Prelude.hashWithSalt` shadowColor
      `Prelude.hashWithSalt` shadowOpacity
      `Prelude.hashWithSalt` shadowXOffset
      `Prelude.hashWithSalt` shadowYOffset
      `Prelude.hashWithSalt` stylePassthrough
      `Prelude.hashWithSalt` teletextSpacing
      `Prelude.hashWithSalt` xPosition
      `Prelude.hashWithSalt` yPosition

instance Prelude.NFData BurninDestinationSettings where
  rnf BurninDestinationSettings' {..} =
    Prelude.rnf alignment
      `Prelude.seq` Prelude.rnf applyFontColor
      `Prelude.seq` Prelude.rnf backgroundColor
      `Prelude.seq` Prelude.rnf backgroundOpacity
      `Prelude.seq` Prelude.rnf fallbackFont
      `Prelude.seq` Prelude.rnf fontColor
      `Prelude.seq` Prelude.rnf fontOpacity
      `Prelude.seq` Prelude.rnf fontResolution
      `Prelude.seq` Prelude.rnf fontScript
      `Prelude.seq` Prelude.rnf fontSize
      `Prelude.seq` Prelude.rnf hexFontColor
      `Prelude.seq` Prelude.rnf outlineColor
      `Prelude.seq` Prelude.rnf outlineSize
      `Prelude.seq` Prelude.rnf shadowColor
      `Prelude.seq` Prelude.rnf shadowOpacity
      `Prelude.seq` Prelude.rnf shadowXOffset
      `Prelude.seq` Prelude.rnf shadowYOffset
      `Prelude.seq` Prelude.rnf stylePassthrough
      `Prelude.seq` Prelude.rnf teletextSpacing
      `Prelude.seq` Prelude.rnf xPosition
      `Prelude.seq` Prelude.rnf yPosition

instance Data.ToJSON BurninDestinationSettings where
  toJSON BurninDestinationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("alignment" Data..=) Prelude.<$> alignment,
            ("applyFontColor" Data..=)
              Prelude.<$> applyFontColor,
            ("backgroundColor" Data..=)
              Prelude.<$> backgroundColor,
            ("backgroundOpacity" Data..=)
              Prelude.<$> backgroundOpacity,
            ("fallbackFont" Data..=) Prelude.<$> fallbackFont,
            ("fontColor" Data..=) Prelude.<$> fontColor,
            ("fontOpacity" Data..=) Prelude.<$> fontOpacity,
            ("fontResolution" Data..=)
              Prelude.<$> fontResolution,
            ("fontScript" Data..=) Prelude.<$> fontScript,
            ("fontSize" Data..=) Prelude.<$> fontSize,
            ("hexFontColor" Data..=) Prelude.<$> hexFontColor,
            ("outlineColor" Data..=) Prelude.<$> outlineColor,
            ("outlineSize" Data..=) Prelude.<$> outlineSize,
            ("shadowColor" Data..=) Prelude.<$> shadowColor,
            ("shadowOpacity" Data..=) Prelude.<$> shadowOpacity,
            ("shadowXOffset" Data..=) Prelude.<$> shadowXOffset,
            ("shadowYOffset" Data..=) Prelude.<$> shadowYOffset,
            ("stylePassthrough" Data..=)
              Prelude.<$> stylePassthrough,
            ("teletextSpacing" Data..=)
              Prelude.<$> teletextSpacing,
            ("xPosition" Data..=) Prelude.<$> xPosition,
            ("yPosition" Data..=) Prelude.<$> yPosition
          ]
      )
