{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbSubDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.DvbSubDestinationSettings
  ( DvbSubDestinationSettings (..)
  -- * Smart constructor
  , mkDvbSubDestinationSettings
  -- * Lenses
  , dsdsAlignment
  , dsdsBackgroundColor
  , dsdsBackgroundOpacity
  , dsdsFont
  , dsdsFontColor
  , dsdsFontOpacity
  , dsdsFontResolution
  , dsdsFontSize
  , dsdsOutlineColor
  , dsdsOutlineSize
  , dsdsShadowColor
  , dsdsShadowOpacity
  , dsdsShadowXOffset
  , dsdsShadowYOffset
  , dsdsTeletextGridControl
  , dsdsXPosition
  , dsdsYPosition
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.DvbSubDestinationAlignment as Types
import qualified Network.AWS.MediaLive.Types.DvbSubDestinationBackgroundColor as Types
import qualified Network.AWS.MediaLive.Types.DvbSubDestinationFontColor as Types
import qualified Network.AWS.MediaLive.Types.DvbSubDestinationOutlineColor as Types
import qualified Network.AWS.MediaLive.Types.DvbSubDestinationShadowColor as Types
import qualified Network.AWS.MediaLive.Types.DvbSubDestinationTeletextGridControl as Types
import qualified Network.AWS.MediaLive.Types.InputLocation as Types
import qualified Network.AWS.Prelude as Core

-- | Dvb Sub Destination Settings
--
-- /See:/ 'mkDvbSubDestinationSettings' smart constructor.
data DvbSubDestinationSettings = DvbSubDestinationSettings'
  { alignment :: Core.Maybe Types.DvbSubDestinationAlignment
    -- ^ If no explicit xPosition or yPosition is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. Selecting "smart" justification will left-justify live subtitles and center-justify pre-recorded subtitles.  This option is not valid for source captions that are STL or 608/embedded.  These source settings are already pre-defined by the caption stream.  All burn-in and DVB-Sub font settings must match.
  , backgroundColor :: Core.Maybe Types.DvbSubDestinationBackgroundColor
    -- ^ Specifies the color of the rectangle behind the captions.  All burn-in and DVB-Sub font settings must match.
  , backgroundOpacity :: Core.Maybe Core.Natural
    -- ^ Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent).  All burn-in and DVB-Sub font settings must match.
  , font :: Core.Maybe Types.InputLocation
    -- ^ External font file used for caption burn-in. File extension must be 'ttf' or 'tte'.  Although the user can select output fonts for many different types of input captions, embedded, STL and teletext sources use a strict grid system. Using external fonts with these caption sources could cause unexpected display of proportional fonts.  All burn-in and DVB-Sub font settings must match.
  , fontColor :: Core.Maybe Types.DvbSubDestinationFontColor
    -- ^ Specifies the color of the burned-in captions.  This option is not valid for source captions that are STL, 608/embedded or teletext.  These source settings are already pre-defined by the caption stream.  All burn-in and DVB-Sub font settings must match.
  , fontOpacity :: Core.Maybe Core.Natural
    -- ^ Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent.  All burn-in and DVB-Sub font settings must match.
  , fontResolution :: Core.Maybe Core.Natural
    -- ^ Font resolution in DPI (dots per inch); default is 96 dpi.  All burn-in and DVB-Sub font settings must match.
  , fontSize :: Core.Maybe Core.Text
    -- ^ When set to auto fontSize will scale depending on the size of the output.  Giving a positive integer will specify the exact font size in points.  All burn-in and DVB-Sub font settings must match.
  , outlineColor :: Core.Maybe Types.DvbSubDestinationOutlineColor
    -- ^ Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
  , outlineSize :: Core.Maybe Core.Natural
    -- ^ Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
  , shadowColor :: Core.Maybe Types.DvbSubDestinationShadowColor
    -- ^ Specifies the color of the shadow cast by the captions.  All burn-in and DVB-Sub font settings must match.
  , shadowOpacity :: Core.Maybe Core.Natural
    -- ^ Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent).  All burn-in and DVB-Sub font settings must match.
  , shadowXOffset :: Core.Maybe Core.Int
    -- ^ Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left.  All burn-in and DVB-Sub font settings must match.
  , shadowYOffset :: Core.Maybe Core.Int
    -- ^ Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text.  All burn-in and DVB-Sub font settings must match.
  , teletextGridControl :: Core.Maybe Types.DvbSubDestinationTeletextGridControl
    -- ^ Controls whether a fixed grid size will be used to generate the output subtitles bitmap. Only applicable for Teletext inputs and DVB-Sub/Burn-in outputs.
  , xPosition :: Core.Maybe Core.Natural
    -- ^ Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit xPosition is provided, the horizontal caption position will be determined by the alignment parameter.  This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream.  All burn-in and DVB-Sub font settings must match.
  , yPosition :: Core.Maybe Core.Natural
    -- ^ Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit yPosition is provided, the caption will be positioned towards the bottom of the output.  This option is not valid for source captions that are STL, 608/embedded or teletext.  These source settings are already pre-defined by the caption stream.  All burn-in and DVB-Sub font settings must match.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DvbSubDestinationSettings' value with any optional fields omitted.
mkDvbSubDestinationSettings
    :: DvbSubDestinationSettings
mkDvbSubDestinationSettings
  = DvbSubDestinationSettings'{alignment = Core.Nothing,
                               backgroundColor = Core.Nothing, backgroundOpacity = Core.Nothing,
                               font = Core.Nothing, fontColor = Core.Nothing,
                               fontOpacity = Core.Nothing, fontResolution = Core.Nothing,
                               fontSize = Core.Nothing, outlineColor = Core.Nothing,
                               outlineSize = Core.Nothing, shadowColor = Core.Nothing,
                               shadowOpacity = Core.Nothing, shadowXOffset = Core.Nothing,
                               shadowYOffset = Core.Nothing, teletextGridControl = Core.Nothing,
                               xPosition = Core.Nothing, yPosition = Core.Nothing}

-- | If no explicit xPosition or yPosition is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. Selecting "smart" justification will left-justify live subtitles and center-justify pre-recorded subtitles.  This option is not valid for source captions that are STL or 608/embedded.  These source settings are already pre-defined by the caption stream.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'alignment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsAlignment :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Types.DvbSubDestinationAlignment)
dsdsAlignment = Lens.field @"alignment"
{-# INLINEABLE dsdsAlignment #-}
{-# DEPRECATED alignment "Use generic-lens or generic-optics with 'alignment' instead"  #-}

-- | Specifies the color of the rectangle behind the captions.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'backgroundColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsBackgroundColor :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Types.DvbSubDestinationBackgroundColor)
dsdsBackgroundColor = Lens.field @"backgroundColor"
{-# INLINEABLE dsdsBackgroundColor #-}
{-# DEPRECATED backgroundColor "Use generic-lens or generic-optics with 'backgroundColor' instead"  #-}

-- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent).  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'backgroundOpacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsBackgroundOpacity :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Core.Natural)
dsdsBackgroundOpacity = Lens.field @"backgroundOpacity"
{-# INLINEABLE dsdsBackgroundOpacity #-}
{-# DEPRECATED backgroundOpacity "Use generic-lens or generic-optics with 'backgroundOpacity' instead"  #-}

-- | External font file used for caption burn-in. File extension must be 'ttf' or 'tte'.  Although the user can select output fonts for many different types of input captions, embedded, STL and teletext sources use a strict grid system. Using external fonts with these caption sources could cause unexpected display of proportional fonts.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'font' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsFont :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Types.InputLocation)
dsdsFont = Lens.field @"font"
{-# INLINEABLE dsdsFont #-}
{-# DEPRECATED font "Use generic-lens or generic-optics with 'font' instead"  #-}

-- | Specifies the color of the burned-in captions.  This option is not valid for source captions that are STL, 608/embedded or teletext.  These source settings are already pre-defined by the caption stream.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsFontColor :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Types.DvbSubDestinationFontColor)
dsdsFontColor = Lens.field @"fontColor"
{-# INLINEABLE dsdsFontColor #-}
{-# DEPRECATED fontColor "Use generic-lens or generic-optics with 'fontColor' instead"  #-}

-- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontOpacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsFontOpacity :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Core.Natural)
dsdsFontOpacity = Lens.field @"fontOpacity"
{-# INLINEABLE dsdsFontOpacity #-}
{-# DEPRECATED fontOpacity "Use generic-lens or generic-optics with 'fontOpacity' instead"  #-}

-- | Font resolution in DPI (dots per inch); default is 96 dpi.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsFontResolution :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Core.Natural)
dsdsFontResolution = Lens.field @"fontResolution"
{-# INLINEABLE dsdsFontResolution #-}
{-# DEPRECATED fontResolution "Use generic-lens or generic-optics with 'fontResolution' instead"  #-}

-- | When set to auto fontSize will scale depending on the size of the output.  Giving a positive integer will specify the exact font size in points.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsFontSize :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Core.Text)
dsdsFontSize = Lens.field @"fontSize"
{-# INLINEABLE dsdsFontSize #-}
{-# DEPRECATED fontSize "Use generic-lens or generic-optics with 'fontSize' instead"  #-}

-- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'outlineColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsOutlineColor :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Types.DvbSubDestinationOutlineColor)
dsdsOutlineColor = Lens.field @"outlineColor"
{-# INLINEABLE dsdsOutlineColor #-}
{-# DEPRECATED outlineColor "Use generic-lens or generic-optics with 'outlineColor' instead"  #-}

-- | Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'outlineSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsOutlineSize :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Core.Natural)
dsdsOutlineSize = Lens.field @"outlineSize"
{-# INLINEABLE dsdsOutlineSize #-}
{-# DEPRECATED outlineSize "Use generic-lens or generic-optics with 'outlineSize' instead"  #-}

-- | Specifies the color of the shadow cast by the captions.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsShadowColor :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Types.DvbSubDestinationShadowColor)
dsdsShadowColor = Lens.field @"shadowColor"
{-# INLINEABLE dsdsShadowColor #-}
{-# DEPRECATED shadowColor "Use generic-lens or generic-optics with 'shadowColor' instead"  #-}

-- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent).  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowOpacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsShadowOpacity :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Core.Natural)
dsdsShadowOpacity = Lens.field @"shadowOpacity"
{-# INLINEABLE dsdsShadowOpacity #-}
{-# DEPRECATED shadowOpacity "Use generic-lens or generic-optics with 'shadowOpacity' instead"  #-}

-- | Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowXOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsShadowXOffset :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Core.Int)
dsdsShadowXOffset = Lens.field @"shadowXOffset"
{-# INLINEABLE dsdsShadowXOffset #-}
{-# DEPRECATED shadowXOffset "Use generic-lens or generic-optics with 'shadowXOffset' instead"  #-}

-- | Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowYOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsShadowYOffset :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Core.Int)
dsdsShadowYOffset = Lens.field @"shadowYOffset"
{-# INLINEABLE dsdsShadowYOffset #-}
{-# DEPRECATED shadowYOffset "Use generic-lens or generic-optics with 'shadowYOffset' instead"  #-}

-- | Controls whether a fixed grid size will be used to generate the output subtitles bitmap. Only applicable for Teletext inputs and DVB-Sub/Burn-in outputs.
--
-- /Note:/ Consider using 'teletextGridControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsTeletextGridControl :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Types.DvbSubDestinationTeletextGridControl)
dsdsTeletextGridControl = Lens.field @"teletextGridControl"
{-# INLINEABLE dsdsTeletextGridControl #-}
{-# DEPRECATED teletextGridControl "Use generic-lens or generic-optics with 'teletextGridControl' instead"  #-}

-- | Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit xPosition is provided, the horizontal caption position will be determined by the alignment parameter.  This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'xPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsXPosition :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Core.Natural)
dsdsXPosition = Lens.field @"xPosition"
{-# INLINEABLE dsdsXPosition #-}
{-# DEPRECATED xPosition "Use generic-lens or generic-optics with 'xPosition' instead"  #-}

-- | Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit yPosition is provided, the caption will be positioned towards the bottom of the output.  This option is not valid for source captions that are STL, 608/embedded or teletext.  These source settings are already pre-defined by the caption stream.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'yPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsYPosition :: Lens.Lens' DvbSubDestinationSettings (Core.Maybe Core.Natural)
dsdsYPosition = Lens.field @"yPosition"
{-# INLINEABLE dsdsYPosition #-}
{-# DEPRECATED yPosition "Use generic-lens or generic-optics with 'yPosition' instead"  #-}

instance Core.FromJSON DvbSubDestinationSettings where
        toJSON DvbSubDestinationSettings{..}
          = Core.object
              (Core.catMaybes
                 [("alignment" Core..=) Core.<$> alignment,
                  ("backgroundColor" Core..=) Core.<$> backgroundColor,
                  ("backgroundOpacity" Core..=) Core.<$> backgroundOpacity,
                  ("font" Core..=) Core.<$> font,
                  ("fontColor" Core..=) Core.<$> fontColor,
                  ("fontOpacity" Core..=) Core.<$> fontOpacity,
                  ("fontResolution" Core..=) Core.<$> fontResolution,
                  ("fontSize" Core..=) Core.<$> fontSize,
                  ("outlineColor" Core..=) Core.<$> outlineColor,
                  ("outlineSize" Core..=) Core.<$> outlineSize,
                  ("shadowColor" Core..=) Core.<$> shadowColor,
                  ("shadowOpacity" Core..=) Core.<$> shadowOpacity,
                  ("shadowXOffset" Core..=) Core.<$> shadowXOffset,
                  ("shadowYOffset" Core..=) Core.<$> shadowYOffset,
                  ("teletextGridControl" Core..=) Core.<$> teletextGridControl,
                  ("xPosition" Core..=) Core.<$> xPosition,
                  ("yPosition" Core..=) Core.<$> yPosition])

instance Core.FromJSON DvbSubDestinationSettings where
        parseJSON
          = Core.withObject "DvbSubDestinationSettings" Core.$
              \ x ->
                DvbSubDestinationSettings' Core.<$>
                  (x Core..:? "alignment") Core.<*> x Core..:? "backgroundColor"
                    Core.<*> x Core..:? "backgroundOpacity"
                    Core.<*> x Core..:? "font"
                    Core.<*> x Core..:? "fontColor"
                    Core.<*> x Core..:? "fontOpacity"
                    Core.<*> x Core..:? "fontResolution"
                    Core.<*> x Core..:? "fontSize"
                    Core.<*> x Core..:? "outlineColor"
                    Core.<*> x Core..:? "outlineSize"
                    Core.<*> x Core..:? "shadowColor"
                    Core.<*> x Core..:? "shadowOpacity"
                    Core.<*> x Core..:? "shadowXOffset"
                    Core.<*> x Core..:? "shadowYOffset"
                    Core.<*> x Core..:? "teletextGridControl"
                    Core.<*> x Core..:? "xPosition"
                    Core.<*> x Core..:? "yPosition"
