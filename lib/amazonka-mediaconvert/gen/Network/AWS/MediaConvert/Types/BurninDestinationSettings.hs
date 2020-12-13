{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.BurninDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.BurninDestinationSettings
  ( BurninDestinationSettings (..),

    -- * Smart constructor
    mkBurninDestinationSettings,

    -- * Lenses
    bdsBackgroundOpacity,
    bdsFontOpacity,
    bdsShadowYOffset,
    bdsFontResolution,
    bdsYPosition,
    bdsBackgroundColor,
    bdsShadowXOffset,
    bdsFontSize,
    bdsXPosition,
    bdsTeletextSpacing,
    bdsFontScript,
    bdsAlignment,
    bdsShadowOpacity,
    bdsOutlineColor,
    bdsOutlineSize,
    bdsShadowColor,
    bdsFontColor,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.BurninSubtitleAlignment
import Network.AWS.MediaConvert.Types.BurninSubtitleBackgroundColor
import Network.AWS.MediaConvert.Types.BurninSubtitleFontColor
import Network.AWS.MediaConvert.Types.BurninSubtitleOutlineColor
import Network.AWS.MediaConvert.Types.BurninSubtitleShadowColor
import Network.AWS.MediaConvert.Types.BurninSubtitleTeletextSpacing
import Network.AWS.MediaConvert.Types.FontScript
import qualified Network.AWS.Prelude as Lude

-- | Burn-In Destination Settings.
--
-- /See:/ 'mkBurninDestinationSettings' smart constructor.
data BurninDestinationSettings = BurninDestinationSettings'
  { -- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
    backgroundOpacity :: Lude.Maybe Lude.Natural,
    -- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent.
    --
    -- All burn-in and DVB-Sub font settings must match.
    fontOpacity :: Lude.Maybe Lude.Natural,
    -- | Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text. All burn-in and DVB-Sub font settings must match.
    shadowYOffset :: Lude.Maybe Lude.Int,
    -- | Font resolution in DPI (dots per inch); default is 96 dpi.
    --
    -- All burn-in and DVB-Sub font settings must match.
    fontResolution :: Lude.Maybe Lude.Natural,
    -- | Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit y_position is provided, the caption will be positioned towards the bottom of the output. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
    yPosition :: Lude.Maybe Lude.Natural,
    -- | Specifies the color of the rectangle behind the captions.
    --
    -- All burn-in and DVB-Sub font settings must match.
    backgroundColor :: Lude.Maybe BurninSubtitleBackgroundColor,
    -- | Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left. All burn-in and DVB-Sub font settings must match.
    shadowXOffset :: Lude.Maybe Lude.Int,
    -- | A positive integer indicates the exact font size in points. Set to 0 for automatic font size selection. All burn-in and DVB-Sub font settings must match.
    fontSize :: Lude.Maybe Lude.Natural,
    -- | Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit x_position is provided, the horizontal caption position will be determined by the alignment parameter. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
    xPosition :: Lude.Maybe Lude.Natural,
    -- | Only applies to jobs with input captions in Teletext or STL formats. Specify whether the spacing between letters in your captions is set by the captions grid or varies depending on letter width. Choose fixed grid to conform to the spacing specified in the captions file more accurately. Choose proportional to make the text easier to read if the captions are closed caption.
    teletextSpacing :: Lude.Maybe BurninSubtitleTeletextSpacing,
    -- | Provide the font script, using an ISO 15924 script code, if the LanguageCode is not sufficient for determining the script type. Where LanguageCode or CustomLanguageCode is sufficient, use "AUTOMATIC" or leave unset. This is used to help determine the appropriate font for rendering burn-in captions.
    fontScript :: Lude.Maybe FontScript,
    -- | If no explicit x_position or y_position is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
    alignment :: Lude.Maybe BurninSubtitleAlignment,
    -- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
    shadowOpacity :: Lude.Maybe Lude.Natural,
    -- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
    outlineColor :: Lude.Maybe BurninSubtitleOutlineColor,
    -- | Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
    outlineSize :: Lude.Maybe Lude.Natural,
    -- | Specifies the color of the shadow cast by the captions.
    --
    -- All burn-in and DVB-Sub font settings must match.
    shadowColor :: Lude.Maybe BurninSubtitleShadowColor,
    -- | Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
    fontColor :: Lude.Maybe BurninSubtitleFontColor
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BurninDestinationSettings' with the minimum fields required to make a request.
--
-- * 'backgroundOpacity' - Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
-- * 'fontOpacity' - Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent.
--
-- All burn-in and DVB-Sub font settings must match.
-- * 'shadowYOffset' - Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text. All burn-in and DVB-Sub font settings must match.
-- * 'fontResolution' - Font resolution in DPI (dots per inch); default is 96 dpi.
--
-- All burn-in and DVB-Sub font settings must match.
-- * 'yPosition' - Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit y_position is provided, the caption will be positioned towards the bottom of the output. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
-- * 'backgroundColor' - Specifies the color of the rectangle behind the captions.
--
-- All burn-in and DVB-Sub font settings must match.
-- * 'shadowXOffset' - Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left. All burn-in and DVB-Sub font settings must match.
-- * 'fontSize' - A positive integer indicates the exact font size in points. Set to 0 for automatic font size selection. All burn-in and DVB-Sub font settings must match.
-- * 'xPosition' - Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit x_position is provided, the horizontal caption position will be determined by the alignment parameter. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
-- * 'teletextSpacing' - Only applies to jobs with input captions in Teletext or STL formats. Specify whether the spacing between letters in your captions is set by the captions grid or varies depending on letter width. Choose fixed grid to conform to the spacing specified in the captions file more accurately. Choose proportional to make the text easier to read if the captions are closed caption.
-- * 'fontScript' - Provide the font script, using an ISO 15924 script code, if the LanguageCode is not sufficient for determining the script type. Where LanguageCode or CustomLanguageCode is sufficient, use "AUTOMATIC" or leave unset. This is used to help determine the appropriate font for rendering burn-in captions.
-- * 'alignment' - If no explicit x_position or y_position is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
-- * 'shadowOpacity' - Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
-- * 'outlineColor' - Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
-- * 'outlineSize' - Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
-- * 'shadowColor' - Specifies the color of the shadow cast by the captions.
--
-- All burn-in and DVB-Sub font settings must match.
-- * 'fontColor' - Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
mkBurninDestinationSettings ::
  BurninDestinationSettings
mkBurninDestinationSettings =
  BurninDestinationSettings'
    { backgroundOpacity = Lude.Nothing,
      fontOpacity = Lude.Nothing,
      shadowYOffset = Lude.Nothing,
      fontResolution = Lude.Nothing,
      yPosition = Lude.Nothing,
      backgroundColor = Lude.Nothing,
      shadowXOffset = Lude.Nothing,
      fontSize = Lude.Nothing,
      xPosition = Lude.Nothing,
      teletextSpacing = Lude.Nothing,
      fontScript = Lude.Nothing,
      alignment = Lude.Nothing,
      shadowOpacity = Lude.Nothing,
      outlineColor = Lude.Nothing,
      outlineSize = Lude.Nothing,
      shadowColor = Lude.Nothing,
      fontColor = Lude.Nothing
    }

-- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'backgroundOpacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsBackgroundOpacity :: Lens.Lens' BurninDestinationSettings (Lude.Maybe Lude.Natural)
bdsBackgroundOpacity = Lens.lens (backgroundOpacity :: BurninDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {backgroundOpacity = a} :: BurninDestinationSettings)
{-# DEPRECATED bdsBackgroundOpacity "Use generic-lens or generic-optics with 'backgroundOpacity' instead." #-}

-- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent.
--
-- All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontOpacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsFontOpacity :: Lens.Lens' BurninDestinationSettings (Lude.Maybe Lude.Natural)
bdsFontOpacity = Lens.lens (fontOpacity :: BurninDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {fontOpacity = a} :: BurninDestinationSettings)
{-# DEPRECATED bdsFontOpacity "Use generic-lens or generic-optics with 'fontOpacity' instead." #-}

-- | Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowYOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsShadowYOffset :: Lens.Lens' BurninDestinationSettings (Lude.Maybe Lude.Int)
bdsShadowYOffset = Lens.lens (shadowYOffset :: BurninDestinationSettings -> Lude.Maybe Lude.Int) (\s a -> s {shadowYOffset = a} :: BurninDestinationSettings)
{-# DEPRECATED bdsShadowYOffset "Use generic-lens or generic-optics with 'shadowYOffset' instead." #-}

-- | Font resolution in DPI (dots per inch); default is 96 dpi.
--
-- All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsFontResolution :: Lens.Lens' BurninDestinationSettings (Lude.Maybe Lude.Natural)
bdsFontResolution = Lens.lens (fontResolution :: BurninDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {fontResolution = a} :: BurninDestinationSettings)
{-# DEPRECATED bdsFontResolution "Use generic-lens or generic-optics with 'fontResolution' instead." #-}

-- | Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit y_position is provided, the caption will be positioned towards the bottom of the output. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'yPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsYPosition :: Lens.Lens' BurninDestinationSettings (Lude.Maybe Lude.Natural)
bdsYPosition = Lens.lens (yPosition :: BurninDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {yPosition = a} :: BurninDestinationSettings)
{-# DEPRECATED bdsYPosition "Use generic-lens or generic-optics with 'yPosition' instead." #-}

-- | Specifies the color of the rectangle behind the captions.
--
-- All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'backgroundColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsBackgroundColor :: Lens.Lens' BurninDestinationSettings (Lude.Maybe BurninSubtitleBackgroundColor)
bdsBackgroundColor = Lens.lens (backgroundColor :: BurninDestinationSettings -> Lude.Maybe BurninSubtitleBackgroundColor) (\s a -> s {backgroundColor = a} :: BurninDestinationSettings)
{-# DEPRECATED bdsBackgroundColor "Use generic-lens or generic-optics with 'backgroundColor' instead." #-}

-- | Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowXOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsShadowXOffset :: Lens.Lens' BurninDestinationSettings (Lude.Maybe Lude.Int)
bdsShadowXOffset = Lens.lens (shadowXOffset :: BurninDestinationSettings -> Lude.Maybe Lude.Int) (\s a -> s {shadowXOffset = a} :: BurninDestinationSettings)
{-# DEPRECATED bdsShadowXOffset "Use generic-lens or generic-optics with 'shadowXOffset' instead." #-}

-- | A positive integer indicates the exact font size in points. Set to 0 for automatic font size selection. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsFontSize :: Lens.Lens' BurninDestinationSettings (Lude.Maybe Lude.Natural)
bdsFontSize = Lens.lens (fontSize :: BurninDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {fontSize = a} :: BurninDestinationSettings)
{-# DEPRECATED bdsFontSize "Use generic-lens or generic-optics with 'fontSize' instead." #-}

-- | Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit x_position is provided, the horizontal caption position will be determined by the alignment parameter. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'xPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsXPosition :: Lens.Lens' BurninDestinationSettings (Lude.Maybe Lude.Natural)
bdsXPosition = Lens.lens (xPosition :: BurninDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {xPosition = a} :: BurninDestinationSettings)
{-# DEPRECATED bdsXPosition "Use generic-lens or generic-optics with 'xPosition' instead." #-}

-- | Only applies to jobs with input captions in Teletext or STL formats. Specify whether the spacing between letters in your captions is set by the captions grid or varies depending on letter width. Choose fixed grid to conform to the spacing specified in the captions file more accurately. Choose proportional to make the text easier to read if the captions are closed caption.
--
-- /Note:/ Consider using 'teletextSpacing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsTeletextSpacing :: Lens.Lens' BurninDestinationSettings (Lude.Maybe BurninSubtitleTeletextSpacing)
bdsTeletextSpacing = Lens.lens (teletextSpacing :: BurninDestinationSettings -> Lude.Maybe BurninSubtitleTeletextSpacing) (\s a -> s {teletextSpacing = a} :: BurninDestinationSettings)
{-# DEPRECATED bdsTeletextSpacing "Use generic-lens or generic-optics with 'teletextSpacing' instead." #-}

-- | Provide the font script, using an ISO 15924 script code, if the LanguageCode is not sufficient for determining the script type. Where LanguageCode or CustomLanguageCode is sufficient, use "AUTOMATIC" or leave unset. This is used to help determine the appropriate font for rendering burn-in captions.
--
-- /Note:/ Consider using 'fontScript' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsFontScript :: Lens.Lens' BurninDestinationSettings (Lude.Maybe FontScript)
bdsFontScript = Lens.lens (fontScript :: BurninDestinationSettings -> Lude.Maybe FontScript) (\s a -> s {fontScript = a} :: BurninDestinationSettings)
{-# DEPRECATED bdsFontScript "Use generic-lens or generic-optics with 'fontScript' instead." #-}

-- | If no explicit x_position or y_position is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'alignment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsAlignment :: Lens.Lens' BurninDestinationSettings (Lude.Maybe BurninSubtitleAlignment)
bdsAlignment = Lens.lens (alignment :: BurninDestinationSettings -> Lude.Maybe BurninSubtitleAlignment) (\s a -> s {alignment = a} :: BurninDestinationSettings)
{-# DEPRECATED bdsAlignment "Use generic-lens or generic-optics with 'alignment' instead." #-}

-- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowOpacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsShadowOpacity :: Lens.Lens' BurninDestinationSettings (Lude.Maybe Lude.Natural)
bdsShadowOpacity = Lens.lens (shadowOpacity :: BurninDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {shadowOpacity = a} :: BurninDestinationSettings)
{-# DEPRECATED bdsShadowOpacity "Use generic-lens or generic-optics with 'shadowOpacity' instead." #-}

-- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'outlineColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsOutlineColor :: Lens.Lens' BurninDestinationSettings (Lude.Maybe BurninSubtitleOutlineColor)
bdsOutlineColor = Lens.lens (outlineColor :: BurninDestinationSettings -> Lude.Maybe BurninSubtitleOutlineColor) (\s a -> s {outlineColor = a} :: BurninDestinationSettings)
{-# DEPRECATED bdsOutlineColor "Use generic-lens or generic-optics with 'outlineColor' instead." #-}

-- | Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'outlineSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsOutlineSize :: Lens.Lens' BurninDestinationSettings (Lude.Maybe Lude.Natural)
bdsOutlineSize = Lens.lens (outlineSize :: BurninDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {outlineSize = a} :: BurninDestinationSettings)
{-# DEPRECATED bdsOutlineSize "Use generic-lens or generic-optics with 'outlineSize' instead." #-}

-- | Specifies the color of the shadow cast by the captions.
--
-- All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsShadowColor :: Lens.Lens' BurninDestinationSettings (Lude.Maybe BurninSubtitleShadowColor)
bdsShadowColor = Lens.lens (shadowColor :: BurninDestinationSettings -> Lude.Maybe BurninSubtitleShadowColor) (\s a -> s {shadowColor = a} :: BurninDestinationSettings)
{-# DEPRECATED bdsShadowColor "Use generic-lens or generic-optics with 'shadowColor' instead." #-}

-- | Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsFontColor :: Lens.Lens' BurninDestinationSettings (Lude.Maybe BurninSubtitleFontColor)
bdsFontColor = Lens.lens (fontColor :: BurninDestinationSettings -> Lude.Maybe BurninSubtitleFontColor) (\s a -> s {fontColor = a} :: BurninDestinationSettings)
{-# DEPRECATED bdsFontColor "Use generic-lens or generic-optics with 'fontColor' instead." #-}

instance Lude.FromJSON BurninDestinationSettings where
  parseJSON =
    Lude.withObject
      "BurninDestinationSettings"
      ( \x ->
          BurninDestinationSettings'
            Lude.<$> (x Lude..:? "backgroundOpacity")
            Lude.<*> (x Lude..:? "fontOpacity")
            Lude.<*> (x Lude..:? "shadowYOffset")
            Lude.<*> (x Lude..:? "fontResolution")
            Lude.<*> (x Lude..:? "yPosition")
            Lude.<*> (x Lude..:? "backgroundColor")
            Lude.<*> (x Lude..:? "shadowXOffset")
            Lude.<*> (x Lude..:? "fontSize")
            Lude.<*> (x Lude..:? "xPosition")
            Lude.<*> (x Lude..:? "teletextSpacing")
            Lude.<*> (x Lude..:? "fontScript")
            Lude.<*> (x Lude..:? "alignment")
            Lude.<*> (x Lude..:? "shadowOpacity")
            Lude.<*> (x Lude..:? "outlineColor")
            Lude.<*> (x Lude..:? "outlineSize")
            Lude.<*> (x Lude..:? "shadowColor")
            Lude.<*> (x Lude..:? "fontColor")
      )

instance Lude.ToJSON BurninDestinationSettings where
  toJSON BurninDestinationSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("backgroundOpacity" Lude..=) Lude.<$> backgroundOpacity,
            ("fontOpacity" Lude..=) Lude.<$> fontOpacity,
            ("shadowYOffset" Lude..=) Lude.<$> shadowYOffset,
            ("fontResolution" Lude..=) Lude.<$> fontResolution,
            ("yPosition" Lude..=) Lude.<$> yPosition,
            ("backgroundColor" Lude..=) Lude.<$> backgroundColor,
            ("shadowXOffset" Lude..=) Lude.<$> shadowXOffset,
            ("fontSize" Lude..=) Lude.<$> fontSize,
            ("xPosition" Lude..=) Lude.<$> xPosition,
            ("teletextSpacing" Lude..=) Lude.<$> teletextSpacing,
            ("fontScript" Lude..=) Lude.<$> fontScript,
            ("alignment" Lude..=) Lude.<$> alignment,
            ("shadowOpacity" Lude..=) Lude.<$> shadowOpacity,
            ("outlineColor" Lude..=) Lude.<$> outlineColor,
            ("outlineSize" Lude..=) Lude.<$> outlineSize,
            ("shadowColor" Lude..=) Lude.<$> shadowColor,
            ("fontColor" Lude..=) Lude.<$> fontColor
          ]
      )
