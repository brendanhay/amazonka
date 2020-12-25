{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubDestinationSettings
  ( DvbSubDestinationSettings (..),

    -- * Smart constructor
    mkDvbSubDestinationSettings,

    -- * Lenses
    dsdsAlignment,
    dsdsBackgroundColor,
    dsdsBackgroundOpacity,
    dsdsFontColor,
    dsdsFontOpacity,
    dsdsFontResolution,
    dsdsFontScript,
    dsdsFontSize,
    dsdsOutlineColor,
    dsdsOutlineSize,
    dsdsShadowColor,
    dsdsShadowOpacity,
    dsdsShadowXOffset,
    dsdsShadowYOffset,
    dsdsSubtitlingType,
    dsdsTeletextSpacing,
    dsdsXPosition,
    dsdsYPosition,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.DvbSubtitleAlignment as Types
import qualified Network.AWS.MediaConvert.Types.DvbSubtitleBackgroundColor as Types
import qualified Network.AWS.MediaConvert.Types.DvbSubtitleFontColor as Types
import qualified Network.AWS.MediaConvert.Types.DvbSubtitleOutlineColor as Types
import qualified Network.AWS.MediaConvert.Types.DvbSubtitleShadowColor as Types
import qualified Network.AWS.MediaConvert.Types.DvbSubtitleTeletextSpacing as Types
import qualified Network.AWS.MediaConvert.Types.DvbSubtitlingType as Types
import qualified Network.AWS.MediaConvert.Types.FontScript as Types
import qualified Network.AWS.Prelude as Core

-- | DVB-Sub Destination Settings
--
-- /See:/ 'mkDvbSubDestinationSettings' smart constructor.
data DvbSubDestinationSettings = DvbSubDestinationSettings'
  { -- | If no explicit x_position or y_position is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
    alignment :: Core.Maybe Types.DvbSubtitleAlignment,
    -- | Specifies the color of the rectangle behind the captions.
    --
    -- All burn-in and DVB-Sub font settings must match.
    backgroundColor :: Core.Maybe Types.DvbSubtitleBackgroundColor,
    -- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
    backgroundOpacity :: Core.Maybe Core.Natural,
    -- | Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
    fontColor :: Core.Maybe Types.DvbSubtitleFontColor,
    -- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent.
    --
    -- All burn-in and DVB-Sub font settings must match.
    fontOpacity :: Core.Maybe Core.Natural,
    -- | Font resolution in DPI (dots per inch); default is 96 dpi.
    --
    -- All burn-in and DVB-Sub font settings must match.
    fontResolution :: Core.Maybe Core.Natural,
    -- | Provide the font script, using an ISO 15924 script code, if the LanguageCode is not sufficient for determining the script type. Where LanguageCode or CustomLanguageCode is sufficient, use "AUTOMATIC" or leave unset. This is used to help determine the appropriate font for rendering DVB-Sub captions.
    fontScript :: Core.Maybe Types.FontScript,
    -- | A positive integer indicates the exact font size in points. Set to 0 for automatic font size selection. All burn-in and DVB-Sub font settings must match.
    fontSize :: Core.Maybe Core.Natural,
    -- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
    outlineColor :: Core.Maybe Types.DvbSubtitleOutlineColor,
    -- | Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
    outlineSize :: Core.Maybe Core.Natural,
    -- | Specifies the color of the shadow cast by the captions.
    --
    -- All burn-in and DVB-Sub font settings must match.
    shadowColor :: Core.Maybe Types.DvbSubtitleShadowColor,
    -- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
    shadowOpacity :: Core.Maybe Core.Natural,
    -- | Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left. All burn-in and DVB-Sub font settings must match.
    shadowXOffset :: Core.Maybe Core.Int,
    -- | Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text. All burn-in and DVB-Sub font settings must match.
    shadowYOffset :: Core.Maybe Core.Int,
    -- | Specify whether your DVB subtitles are standard or for hearing impaired. Choose hearing impaired if your subtitles include audio descriptions and dialogue. Choose standard if your subtitles include only dialogue.
    subtitlingType :: Core.Maybe Types.DvbSubtitlingType,
    -- | Only applies to jobs with input captions in Teletext or STL formats. Specify whether the spacing between letters in your captions is set by the captions grid or varies depending on letter width. Choose fixed grid to conform to the spacing specified in the captions file more accurately. Choose proportional to make the text easier to read if the captions are closed caption.
    teletextSpacing :: Core.Maybe Types.DvbSubtitleTeletextSpacing,
    -- | Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit x_position is provided, the horizontal caption position will be determined by the alignment parameter. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
    xPosition :: Core.Maybe Core.Natural,
    -- | Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit y_position is provided, the caption will be positioned towards the bottom of the output. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
    yPosition :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DvbSubDestinationSettings' value with any optional fields omitted.
mkDvbSubDestinationSettings ::
  DvbSubDestinationSettings
mkDvbSubDestinationSettings =
  DvbSubDestinationSettings'
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
      subtitlingType = Core.Nothing,
      teletextSpacing = Core.Nothing,
      xPosition = Core.Nothing,
      yPosition = Core.Nothing
    }

-- | If no explicit x_position or y_position is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'alignment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsAlignment :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Types.DvbSubtitleAlignment)
dsdsAlignment = Lens.field @"alignment"
{-# DEPRECATED dsdsAlignment "Use generic-lens or generic-optics with 'alignment' instead." #-}

-- | Specifies the color of the rectangle behind the captions.
--
-- All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'backgroundColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsBackgroundColor :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Types.DvbSubtitleBackgroundColor)
dsdsBackgroundColor = Lens.field @"backgroundColor"
{-# DEPRECATED dsdsBackgroundColor "Use generic-lens or generic-optics with 'backgroundColor' instead." #-}

-- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'backgroundOpacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsBackgroundOpacity :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Core.Natural)
dsdsBackgroundOpacity = Lens.field @"backgroundOpacity"
{-# DEPRECATED dsdsBackgroundOpacity "Use generic-lens or generic-optics with 'backgroundOpacity' instead." #-}

-- | Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsFontColor :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Types.DvbSubtitleFontColor)
dsdsFontColor = Lens.field @"fontColor"
{-# DEPRECATED dsdsFontColor "Use generic-lens or generic-optics with 'fontColor' instead." #-}

-- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent.
--
-- All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontOpacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsFontOpacity :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Core.Natural)
dsdsFontOpacity = Lens.field @"fontOpacity"
{-# DEPRECATED dsdsFontOpacity "Use generic-lens or generic-optics with 'fontOpacity' instead." #-}

-- | Font resolution in DPI (dots per inch); default is 96 dpi.
--
-- All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsFontResolution :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Core.Natural)
dsdsFontResolution = Lens.field @"fontResolution"
{-# DEPRECATED dsdsFontResolution "Use generic-lens or generic-optics with 'fontResolution' instead." #-}

-- | Provide the font script, using an ISO 15924 script code, if the LanguageCode is not sufficient for determining the script type. Where LanguageCode or CustomLanguageCode is sufficient, use "AUTOMATIC" or leave unset. This is used to help determine the appropriate font for rendering DVB-Sub captions.
--
-- /Note:/ Consider using 'fontScript' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsFontScript :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Types.FontScript)
dsdsFontScript = Lens.field @"fontScript"
{-# DEPRECATED dsdsFontScript "Use generic-lens or generic-optics with 'fontScript' instead." #-}

-- | A positive integer indicates the exact font size in points. Set to 0 for automatic font size selection. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsFontSize :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Core.Natural)
dsdsFontSize = Lens.field @"fontSize"
{-# DEPRECATED dsdsFontSize "Use generic-lens or generic-optics with 'fontSize' instead." #-}

-- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'outlineColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsOutlineColor :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Types.DvbSubtitleOutlineColor)
dsdsOutlineColor = Lens.field @"outlineColor"
{-# DEPRECATED dsdsOutlineColor "Use generic-lens or generic-optics with 'outlineColor' instead." #-}

-- | Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'outlineSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsOutlineSize :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Core.Natural)
dsdsOutlineSize = Lens.field @"outlineSize"
{-# DEPRECATED dsdsOutlineSize "Use generic-lens or generic-optics with 'outlineSize' instead." #-}

-- | Specifies the color of the shadow cast by the captions.
--
-- All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsShadowColor :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Types.DvbSubtitleShadowColor)
dsdsShadowColor = Lens.field @"shadowColor"
{-# DEPRECATED dsdsShadowColor "Use generic-lens or generic-optics with 'shadowColor' instead." #-}

-- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowOpacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsShadowOpacity :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Core.Natural)
dsdsShadowOpacity = Lens.field @"shadowOpacity"
{-# DEPRECATED dsdsShadowOpacity "Use generic-lens or generic-optics with 'shadowOpacity' instead." #-}

-- | Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowXOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsShadowXOffset :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Core.Int)
dsdsShadowXOffset = Lens.field @"shadowXOffset"
{-# DEPRECATED dsdsShadowXOffset "Use generic-lens or generic-optics with 'shadowXOffset' instead." #-}

-- | Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowYOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsShadowYOffset :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Core.Int)
dsdsShadowYOffset = Lens.field @"shadowYOffset"
{-# DEPRECATED dsdsShadowYOffset "Use generic-lens or generic-optics with 'shadowYOffset' instead." #-}

-- | Specify whether your DVB subtitles are standard or for hearing impaired. Choose hearing impaired if your subtitles include audio descriptions and dialogue. Choose standard if your subtitles include only dialogue.
--
-- /Note:/ Consider using 'subtitlingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsSubtitlingType :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Types.DvbSubtitlingType)
dsdsSubtitlingType = Lens.field @"subtitlingType"
{-# DEPRECATED dsdsSubtitlingType "Use generic-lens or generic-optics with 'subtitlingType' instead." #-}

-- | Only applies to jobs with input captions in Teletext or STL formats. Specify whether the spacing between letters in your captions is set by the captions grid or varies depending on letter width. Choose fixed grid to conform to the spacing specified in the captions file more accurately. Choose proportional to make the text easier to read if the captions are closed caption.
--
-- /Note:/ Consider using 'teletextSpacing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsTeletextSpacing :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Types.DvbSubtitleTeletextSpacing)
dsdsTeletextSpacing = Lens.field @"teletextSpacing"
{-# DEPRECATED dsdsTeletextSpacing "Use generic-lens or generic-optics with 'teletextSpacing' instead." #-}

-- | Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit x_position is provided, the horizontal caption position will be determined by the alignment parameter. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'xPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsXPosition :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Core.Natural)
dsdsXPosition = Lens.field @"xPosition"
{-# DEPRECATED dsdsXPosition "Use generic-lens or generic-optics with 'xPosition' instead." #-}

-- | Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit y_position is provided, the caption will be positioned towards the bottom of the output. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'yPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsYPosition :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Core.Natural)
dsdsYPosition = Lens.field @"yPosition"
{-# DEPRECATED dsdsYPosition "Use generic-lens or generic-optics with 'yPosition' instead." #-}

instance Core.FromJSON DvbSubDestinationSettings where
  toJSON DvbSubDestinationSettings {..} =
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
            ("subtitlingType" Core..=) Core.<$> subtitlingType,
            ("teletextSpacing" Core..=) Core.<$> teletextSpacing,
            ("xPosition" Core..=) Core.<$> xPosition,
            ("yPosition" Core..=) Core.<$> yPosition
          ]
      )

instance Core.FromJSON DvbSubDestinationSettings where
  parseJSON =
    Core.withObject "DvbSubDestinationSettings" Core.$
      \x ->
        DvbSubDestinationSettings'
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
          Core.<*> (x Core..:? "subtitlingType")
          Core.<*> (x Core..:? "teletextSpacing")
          Core.<*> (x Core..:? "xPosition")
          Core.<*> (x Core..:? "yPosition")
