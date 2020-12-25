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
    bdsAlignment,
    bdsBackgroundColor,
    bdsBackgroundOpacity,
    bdsFontColor,
    bdsFontOpacity,
    bdsFontResolution,
    bdsFontScript,
    bdsFontSize,
    bdsOutlineColor,
    bdsOutlineSize,
    bdsShadowColor,
    bdsShadowOpacity,
    bdsShadowXOffset,
    bdsShadowYOffset,
    bdsTeletextSpacing,
    bdsXPosition,
    bdsYPosition,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.BurninSubtitleAlignment as Types
import qualified Network.AWS.MediaConvert.Types.BurninSubtitleBackgroundColor as Types
import qualified Network.AWS.MediaConvert.Types.BurninSubtitleFontColor as Types
import qualified Network.AWS.MediaConvert.Types.BurninSubtitleOutlineColor as Types
import qualified Network.AWS.MediaConvert.Types.BurninSubtitleShadowColor as Types
import qualified Network.AWS.MediaConvert.Types.BurninSubtitleTeletextSpacing as Types
import qualified Network.AWS.MediaConvert.Types.FontScript as Types
import qualified Network.AWS.Prelude as Core

-- | Burn-In Destination Settings.
--
-- /See:/ 'mkBurninDestinationSettings' smart constructor.
data BurninDestinationSettings = BurninDestinationSettings'
  { -- | If no explicit x_position or y_position is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
    alignment :: Core.Maybe Types.BurninSubtitleAlignment,
    -- | Specifies the color of the rectangle behind the captions.
    --
    -- All burn-in and DVB-Sub font settings must match.
    backgroundColor :: Core.Maybe Types.BurninSubtitleBackgroundColor,
    -- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
    backgroundOpacity :: Core.Maybe Core.Natural,
    -- | Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
    fontColor :: Core.Maybe Types.BurninSubtitleFontColor,
    -- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent.
    --
    -- All burn-in and DVB-Sub font settings must match.
    fontOpacity :: Core.Maybe Core.Natural,
    -- | Font resolution in DPI (dots per inch); default is 96 dpi.
    --
    -- All burn-in and DVB-Sub font settings must match.
    fontResolution :: Core.Maybe Core.Natural,
    -- | Provide the font script, using an ISO 15924 script code, if the LanguageCode is not sufficient for determining the script type. Where LanguageCode or CustomLanguageCode is sufficient, use "AUTOMATIC" or leave unset. This is used to help determine the appropriate font for rendering burn-in captions.
    fontScript :: Core.Maybe Types.FontScript,
    -- | A positive integer indicates the exact font size in points. Set to 0 for automatic font size selection. All burn-in and DVB-Sub font settings must match.
    fontSize :: Core.Maybe Core.Natural,
    -- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
    outlineColor :: Core.Maybe Types.BurninSubtitleOutlineColor,
    -- | Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
    outlineSize :: Core.Maybe Core.Natural,
    -- | Specifies the color of the shadow cast by the captions.
    --
    -- All burn-in and DVB-Sub font settings must match.
    shadowColor :: Core.Maybe Types.BurninSubtitleShadowColor,
    -- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
    shadowOpacity :: Core.Maybe Core.Natural,
    -- | Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left. All burn-in and DVB-Sub font settings must match.
    shadowXOffset :: Core.Maybe Core.Int,
    -- | Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text. All burn-in and DVB-Sub font settings must match.
    shadowYOffset :: Core.Maybe Core.Int,
    -- | Only applies to jobs with input captions in Teletext or STL formats. Specify whether the spacing between letters in your captions is set by the captions grid or varies depending on letter width. Choose fixed grid to conform to the spacing specified in the captions file more accurately. Choose proportional to make the text easier to read if the captions are closed caption.
    teletextSpacing :: Core.Maybe Types.BurninSubtitleTeletextSpacing,
    -- | Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit x_position is provided, the horizontal caption position will be determined by the alignment parameter. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
    xPosition :: Core.Maybe Core.Natural,
    -- | Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit y_position is provided, the caption will be positioned towards the bottom of the output. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
    yPosition :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BurninDestinationSettings' value with any optional fields omitted.
mkBurninDestinationSettings ::
  BurninDestinationSettings
mkBurninDestinationSettings =
  BurninDestinationSettings'
    { alignment = Core.Nothing,
      backgroundColor = Core.Nothing,
      backgroundOpacity = Core.Nothing,
      fontColor = Core.Nothing,
      fontOpacity = Core.Nothing,
      fontResolution = Core.Nothing,
      fontScript = Core.Nothing,
      fontSize = Core.Nothing,
      outlineColor = Core.Nothing,
      outlineSize = Core.Nothing,
      shadowColor = Core.Nothing,
      shadowOpacity = Core.Nothing,
      shadowXOffset = Core.Nothing,
      shadowYOffset = Core.Nothing,
      teletextSpacing = Core.Nothing,
      xPosition = Core.Nothing,
      yPosition = Core.Nothing
    }

-- | If no explicit x_position or y_position is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'alignment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsAlignment :: Lens.Lens' BurninDestinationSettings (Core.Maybe Types.BurninSubtitleAlignment)
bdsAlignment = Lens.field @"alignment"
{-# DEPRECATED bdsAlignment "Use generic-lens or generic-optics with 'alignment' instead." #-}

-- | Specifies the color of the rectangle behind the captions.
--
-- All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'backgroundColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsBackgroundColor :: Lens.Lens' BurninDestinationSettings (Core.Maybe Types.BurninSubtitleBackgroundColor)
bdsBackgroundColor = Lens.field @"backgroundColor"
{-# DEPRECATED bdsBackgroundColor "Use generic-lens or generic-optics with 'backgroundColor' instead." #-}

-- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'backgroundOpacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsBackgroundOpacity :: Lens.Lens' BurninDestinationSettings (Core.Maybe Core.Natural)
bdsBackgroundOpacity = Lens.field @"backgroundOpacity"
{-# DEPRECATED bdsBackgroundOpacity "Use generic-lens or generic-optics with 'backgroundOpacity' instead." #-}

-- | Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsFontColor :: Lens.Lens' BurninDestinationSettings (Core.Maybe Types.BurninSubtitleFontColor)
bdsFontColor = Lens.field @"fontColor"
{-# DEPRECATED bdsFontColor "Use generic-lens or generic-optics with 'fontColor' instead." #-}

-- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent.
--
-- All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontOpacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsFontOpacity :: Lens.Lens' BurninDestinationSettings (Core.Maybe Core.Natural)
bdsFontOpacity = Lens.field @"fontOpacity"
{-# DEPRECATED bdsFontOpacity "Use generic-lens or generic-optics with 'fontOpacity' instead." #-}

-- | Font resolution in DPI (dots per inch); default is 96 dpi.
--
-- All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsFontResolution :: Lens.Lens' BurninDestinationSettings (Core.Maybe Core.Natural)
bdsFontResolution = Lens.field @"fontResolution"
{-# DEPRECATED bdsFontResolution "Use generic-lens or generic-optics with 'fontResolution' instead." #-}

-- | Provide the font script, using an ISO 15924 script code, if the LanguageCode is not sufficient for determining the script type. Where LanguageCode or CustomLanguageCode is sufficient, use "AUTOMATIC" or leave unset. This is used to help determine the appropriate font for rendering burn-in captions.
--
-- /Note:/ Consider using 'fontScript' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsFontScript :: Lens.Lens' BurninDestinationSettings (Core.Maybe Types.FontScript)
bdsFontScript = Lens.field @"fontScript"
{-# DEPRECATED bdsFontScript "Use generic-lens or generic-optics with 'fontScript' instead." #-}

-- | A positive integer indicates the exact font size in points. Set to 0 for automatic font size selection. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsFontSize :: Lens.Lens' BurninDestinationSettings (Core.Maybe Core.Natural)
bdsFontSize = Lens.field @"fontSize"
{-# DEPRECATED bdsFontSize "Use generic-lens or generic-optics with 'fontSize' instead." #-}

-- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'outlineColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsOutlineColor :: Lens.Lens' BurninDestinationSettings (Core.Maybe Types.BurninSubtitleOutlineColor)
bdsOutlineColor = Lens.field @"outlineColor"
{-# DEPRECATED bdsOutlineColor "Use generic-lens or generic-optics with 'outlineColor' instead." #-}

-- | Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'outlineSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsOutlineSize :: Lens.Lens' BurninDestinationSettings (Core.Maybe Core.Natural)
bdsOutlineSize = Lens.field @"outlineSize"
{-# DEPRECATED bdsOutlineSize "Use generic-lens or generic-optics with 'outlineSize' instead." #-}

-- | Specifies the color of the shadow cast by the captions.
--
-- All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsShadowColor :: Lens.Lens' BurninDestinationSettings (Core.Maybe Types.BurninSubtitleShadowColor)
bdsShadowColor = Lens.field @"shadowColor"
{-# DEPRECATED bdsShadowColor "Use generic-lens or generic-optics with 'shadowColor' instead." #-}

-- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowOpacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsShadowOpacity :: Lens.Lens' BurninDestinationSettings (Core.Maybe Core.Natural)
bdsShadowOpacity = Lens.field @"shadowOpacity"
{-# DEPRECATED bdsShadowOpacity "Use generic-lens or generic-optics with 'shadowOpacity' instead." #-}

-- | Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowXOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsShadowXOffset :: Lens.Lens' BurninDestinationSettings (Core.Maybe Core.Int)
bdsShadowXOffset = Lens.field @"shadowXOffset"
{-# DEPRECATED bdsShadowXOffset "Use generic-lens or generic-optics with 'shadowXOffset' instead." #-}

-- | Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowYOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsShadowYOffset :: Lens.Lens' BurninDestinationSettings (Core.Maybe Core.Int)
bdsShadowYOffset = Lens.field @"shadowYOffset"
{-# DEPRECATED bdsShadowYOffset "Use generic-lens or generic-optics with 'shadowYOffset' instead." #-}

-- | Only applies to jobs with input captions in Teletext or STL formats. Specify whether the spacing between letters in your captions is set by the captions grid or varies depending on letter width. Choose fixed grid to conform to the spacing specified in the captions file more accurately. Choose proportional to make the text easier to read if the captions are closed caption.
--
-- /Note:/ Consider using 'teletextSpacing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsTeletextSpacing :: Lens.Lens' BurninDestinationSettings (Core.Maybe Types.BurninSubtitleTeletextSpacing)
bdsTeletextSpacing = Lens.field @"teletextSpacing"
{-# DEPRECATED bdsTeletextSpacing "Use generic-lens or generic-optics with 'teletextSpacing' instead." #-}

-- | Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit x_position is provided, the horizontal caption position will be determined by the alignment parameter. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'xPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsXPosition :: Lens.Lens' BurninDestinationSettings (Core.Maybe Core.Natural)
bdsXPosition = Lens.field @"xPosition"
{-# DEPRECATED bdsXPosition "Use generic-lens or generic-optics with 'xPosition' instead." #-}

-- | Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit y_position is provided, the caption will be positioned towards the bottom of the output. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'yPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsYPosition :: Lens.Lens' BurninDestinationSettings (Core.Maybe Core.Natural)
bdsYPosition = Lens.field @"yPosition"
{-# DEPRECATED bdsYPosition "Use generic-lens or generic-optics with 'yPosition' instead." #-}

instance Core.FromJSON BurninDestinationSettings where
  toJSON BurninDestinationSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("alignment" Core..=) Core.<$> alignment,
            ("backgroundColor" Core..=) Core.<$> backgroundColor,
            ("backgroundOpacity" Core..=) Core.<$> backgroundOpacity,
            ("fontColor" Core..=) Core.<$> fontColor,
            ("fontOpacity" Core..=) Core.<$> fontOpacity,
            ("fontResolution" Core..=) Core.<$> fontResolution,
            ("fontScript" Core..=) Core.<$> fontScript,
            ("fontSize" Core..=) Core.<$> fontSize,
            ("outlineColor" Core..=) Core.<$> outlineColor,
            ("outlineSize" Core..=) Core.<$> outlineSize,
            ("shadowColor" Core..=) Core.<$> shadowColor,
            ("shadowOpacity" Core..=) Core.<$> shadowOpacity,
            ("shadowXOffset" Core..=) Core.<$> shadowXOffset,
            ("shadowYOffset" Core..=) Core.<$> shadowYOffset,
            ("teletextSpacing" Core..=) Core.<$> teletextSpacing,
            ("xPosition" Core..=) Core.<$> xPosition,
            ("yPosition" Core..=) Core.<$> yPosition
          ]
      )

instance Core.FromJSON BurninDestinationSettings where
  parseJSON =
    Core.withObject "BurninDestinationSettings" Core.$
      \x ->
        BurninDestinationSettings'
          Core.<$> (x Core..:? "alignment")
          Core.<*> (x Core..:? "backgroundColor")
          Core.<*> (x Core..:? "backgroundOpacity")
          Core.<*> (x Core..:? "fontColor")
          Core.<*> (x Core..:? "fontOpacity")
          Core.<*> (x Core..:? "fontResolution")
          Core.<*> (x Core..:? "fontScript")
          Core.<*> (x Core..:? "fontSize")
          Core.<*> (x Core..:? "outlineColor")
          Core.<*> (x Core..:? "outlineSize")
          Core.<*> (x Core..:? "shadowColor")
          Core.<*> (x Core..:? "shadowOpacity")
          Core.<*> (x Core..:? "shadowXOffset")
          Core.<*> (x Core..:? "shadowYOffset")
          Core.<*> (x Core..:? "teletextSpacing")
          Core.<*> (x Core..:? "xPosition")
          Core.<*> (x Core..:? "yPosition")
