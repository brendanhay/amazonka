{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BurnInDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BurnInDestinationSettings
  ( BurnInDestinationSettings (..),

    -- * Smart constructor
    mkBurnInDestinationSettings,

    -- * Lenses
    bidsBackgroundOpacity,
    bidsFontOpacity,
    bidsShadowYOffset,
    bidsFontResolution,
    bidsYPosition,
    bidsBackgroundColor,
    bidsShadowXOffset,
    bidsFontSize,
    bidsXPosition,
    bidsAlignment,
    bidsShadowOpacity,
    bidsTeletextGridControl,
    bidsOutlineColor,
    bidsOutlineSize,
    bidsFont,
    bidsShadowColor,
    bidsFontColor,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.BurnInAlignment
import Network.AWS.MediaLive.Types.BurnInBackgroundColor
import Network.AWS.MediaLive.Types.BurnInFontColor
import Network.AWS.MediaLive.Types.BurnInOutlineColor
import Network.AWS.MediaLive.Types.BurnInShadowColor
import Network.AWS.MediaLive.Types.BurnInTeletextGridControl
import Network.AWS.MediaLive.Types.InputLocation
import qualified Network.AWS.Prelude as Lude

-- | Burn In Destination Settings
--
-- /See:/ 'mkBurnInDestinationSettings' smart constructor.
data BurnInDestinationSettings = BurnInDestinationSettings'
  { backgroundOpacity ::
      Lude.Maybe Lude.Natural,
    fontOpacity :: Lude.Maybe Lude.Natural,
    shadowYOffset :: Lude.Maybe Lude.Int,
    fontResolution ::
      Lude.Maybe Lude.Natural,
    yPosition :: Lude.Maybe Lude.Natural,
    backgroundColor ::
      Lude.Maybe BurnInBackgroundColor,
    shadowXOffset :: Lude.Maybe Lude.Int,
    fontSize :: Lude.Maybe Lude.Text,
    xPosition :: Lude.Maybe Lude.Natural,
    alignment :: Lude.Maybe BurnInAlignment,
    shadowOpacity ::
      Lude.Maybe Lude.Natural,
    teletextGridControl ::
      Lude.Maybe BurnInTeletextGridControl,
    outlineColor ::
      Lude.Maybe BurnInOutlineColor,
    outlineSize :: Lude.Maybe Lude.Natural,
    font :: Lude.Maybe InputLocation,
    shadowColor ::
      Lude.Maybe BurnInShadowColor,
    fontColor :: Lude.Maybe BurnInFontColor
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BurnInDestinationSettings' with the minimum fields required to make a request.
--
-- * 'alignment' - If no explicit xPosition or yPosition is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. Selecting "smart" justification will left-justify live subtitles and center-justify pre-recorded subtitles.  All burn-in and DVB-Sub font settings must match.
-- * 'backgroundColor' - Specifies the color of the rectangle behind the captions.  All burn-in and DVB-Sub font settings must match.
-- * 'backgroundOpacity' - Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter out is equivalent to setting it to 0 (transparent).  All burn-in and DVB-Sub font settings must match.
-- * 'font' - External font file used for caption burn-in. File extension must be 'ttf' or 'tte'.  Although the user can select output fonts for many different types of input captions,  embedded, STL and teletext sources use a strict grid system. Using external fonts with these caption sources could cause unexpected display of proportional fonts.  All burn-in and DVB-Sub font settings must match.
-- * 'fontColor' - Specifies the color of the burned-in captions.  This option is not valid for source captions that are STL, 608/embedded or teletext.  These source settings are already pre-defined by the caption stream.  All burn-in and DVB-Sub font settings must match.
-- * 'fontOpacity' - Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent.  All burn-in and DVB-Sub font settings must match.
-- * 'fontResolution' - Font resolution in DPI (dots per inch); default is 96 dpi.  All burn-in and DVB-Sub font settings must match.
-- * 'fontSize' - When set to 'auto' fontSize will scale depending on the size of the output.  Giving a positive integer will specify the exact font size in points.  All burn-in and DVB-Sub font settings must match.
-- * 'outlineColor' - Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
-- * 'outlineSize' - Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
-- * 'shadowColor' - Specifies the color of the shadow cast by the captions.  All burn-in and DVB-Sub font settings must match.
-- * 'shadowOpacity' - Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter out is equivalent to setting it to 0 (transparent).  All burn-in and DVB-Sub font settings must match.
-- * 'shadowXOffset' - Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left.  All burn-in and DVB-Sub font settings must match.
-- * 'shadowYOffset' - Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text.  All burn-in and DVB-Sub font settings must match.
-- * 'teletextGridControl' - Controls whether a fixed grid size will be used to generate the output subtitles bitmap. Only applicable for Teletext inputs and DVB-Sub/Burn-in outputs.
-- * 'xPosition' - Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit xPosition is provided, the horizontal caption position will be determined by the alignment parameter.  All burn-in and DVB-Sub font settings must match.
-- * 'yPosition' - Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit yPosition is provided, the caption will be positioned towards the bottom of the output.  All burn-in and DVB-Sub font settings must match.
mkBurnInDestinationSettings ::
  BurnInDestinationSettings
mkBurnInDestinationSettings =
  BurnInDestinationSettings'
    { backgroundOpacity = Lude.Nothing,
      fontOpacity = Lude.Nothing,
      shadowYOffset = Lude.Nothing,
      fontResolution = Lude.Nothing,
      yPosition = Lude.Nothing,
      backgroundColor = Lude.Nothing,
      shadowXOffset = Lude.Nothing,
      fontSize = Lude.Nothing,
      xPosition = Lude.Nothing,
      alignment = Lude.Nothing,
      shadowOpacity = Lude.Nothing,
      teletextGridControl = Lude.Nothing,
      outlineColor = Lude.Nothing,
      outlineSize = Lude.Nothing,
      font = Lude.Nothing,
      shadowColor = Lude.Nothing,
      fontColor = Lude.Nothing
    }

-- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter out is equivalent to setting it to 0 (transparent).  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'backgroundOpacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bidsBackgroundOpacity :: Lens.Lens' BurnInDestinationSettings (Lude.Maybe Lude.Natural)
bidsBackgroundOpacity = Lens.lens (backgroundOpacity :: BurnInDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {backgroundOpacity = a} :: BurnInDestinationSettings)
{-# DEPRECATED bidsBackgroundOpacity "Use generic-lens or generic-optics with 'backgroundOpacity' instead." #-}

-- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontOpacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bidsFontOpacity :: Lens.Lens' BurnInDestinationSettings (Lude.Maybe Lude.Natural)
bidsFontOpacity = Lens.lens (fontOpacity :: BurnInDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {fontOpacity = a} :: BurnInDestinationSettings)
{-# DEPRECATED bidsFontOpacity "Use generic-lens or generic-optics with 'fontOpacity' instead." #-}

-- | Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowYOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bidsShadowYOffset :: Lens.Lens' BurnInDestinationSettings (Lude.Maybe Lude.Int)
bidsShadowYOffset = Lens.lens (shadowYOffset :: BurnInDestinationSettings -> Lude.Maybe Lude.Int) (\s a -> s {shadowYOffset = a} :: BurnInDestinationSettings)
{-# DEPRECATED bidsShadowYOffset "Use generic-lens or generic-optics with 'shadowYOffset' instead." #-}

-- | Font resolution in DPI (dots per inch); default is 96 dpi.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bidsFontResolution :: Lens.Lens' BurnInDestinationSettings (Lude.Maybe Lude.Natural)
bidsFontResolution = Lens.lens (fontResolution :: BurnInDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {fontResolution = a} :: BurnInDestinationSettings)
{-# DEPRECATED bidsFontResolution "Use generic-lens or generic-optics with 'fontResolution' instead." #-}

-- | Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit yPosition is provided, the caption will be positioned towards the bottom of the output.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'yPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bidsYPosition :: Lens.Lens' BurnInDestinationSettings (Lude.Maybe Lude.Natural)
bidsYPosition = Lens.lens (yPosition :: BurnInDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {yPosition = a} :: BurnInDestinationSettings)
{-# DEPRECATED bidsYPosition "Use generic-lens or generic-optics with 'yPosition' instead." #-}

-- | Specifies the color of the rectangle behind the captions.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'backgroundColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bidsBackgroundColor :: Lens.Lens' BurnInDestinationSettings (Lude.Maybe BurnInBackgroundColor)
bidsBackgroundColor = Lens.lens (backgroundColor :: BurnInDestinationSettings -> Lude.Maybe BurnInBackgroundColor) (\s a -> s {backgroundColor = a} :: BurnInDestinationSettings)
{-# DEPRECATED bidsBackgroundColor "Use generic-lens or generic-optics with 'backgroundColor' instead." #-}

-- | Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowXOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bidsShadowXOffset :: Lens.Lens' BurnInDestinationSettings (Lude.Maybe Lude.Int)
bidsShadowXOffset = Lens.lens (shadowXOffset :: BurnInDestinationSettings -> Lude.Maybe Lude.Int) (\s a -> s {shadowXOffset = a} :: BurnInDestinationSettings)
{-# DEPRECATED bidsShadowXOffset "Use generic-lens or generic-optics with 'shadowXOffset' instead." #-}

-- | When set to 'auto' fontSize will scale depending on the size of the output.  Giving a positive integer will specify the exact font size in points.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bidsFontSize :: Lens.Lens' BurnInDestinationSettings (Lude.Maybe Lude.Text)
bidsFontSize = Lens.lens (fontSize :: BurnInDestinationSettings -> Lude.Maybe Lude.Text) (\s a -> s {fontSize = a} :: BurnInDestinationSettings)
{-# DEPRECATED bidsFontSize "Use generic-lens or generic-optics with 'fontSize' instead." #-}

-- | Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit xPosition is provided, the horizontal caption position will be determined by the alignment parameter.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'xPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bidsXPosition :: Lens.Lens' BurnInDestinationSettings (Lude.Maybe Lude.Natural)
bidsXPosition = Lens.lens (xPosition :: BurnInDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {xPosition = a} :: BurnInDestinationSettings)
{-# DEPRECATED bidsXPosition "Use generic-lens or generic-optics with 'xPosition' instead." #-}

-- | If no explicit xPosition or yPosition is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. Selecting "smart" justification will left-justify live subtitles and center-justify pre-recorded subtitles.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'alignment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bidsAlignment :: Lens.Lens' BurnInDestinationSettings (Lude.Maybe BurnInAlignment)
bidsAlignment = Lens.lens (alignment :: BurnInDestinationSettings -> Lude.Maybe BurnInAlignment) (\s a -> s {alignment = a} :: BurnInDestinationSettings)
{-# DEPRECATED bidsAlignment "Use generic-lens or generic-optics with 'alignment' instead." #-}

-- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter out is equivalent to setting it to 0 (transparent).  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowOpacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bidsShadowOpacity :: Lens.Lens' BurnInDestinationSettings (Lude.Maybe Lude.Natural)
bidsShadowOpacity = Lens.lens (shadowOpacity :: BurnInDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {shadowOpacity = a} :: BurnInDestinationSettings)
{-# DEPRECATED bidsShadowOpacity "Use generic-lens or generic-optics with 'shadowOpacity' instead." #-}

-- | Controls whether a fixed grid size will be used to generate the output subtitles bitmap. Only applicable for Teletext inputs and DVB-Sub/Burn-in outputs.
--
-- /Note:/ Consider using 'teletextGridControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bidsTeletextGridControl :: Lens.Lens' BurnInDestinationSettings (Lude.Maybe BurnInTeletextGridControl)
bidsTeletextGridControl = Lens.lens (teletextGridControl :: BurnInDestinationSettings -> Lude.Maybe BurnInTeletextGridControl) (\s a -> s {teletextGridControl = a} :: BurnInDestinationSettings)
{-# DEPRECATED bidsTeletextGridControl "Use generic-lens or generic-optics with 'teletextGridControl' instead." #-}

-- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'outlineColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bidsOutlineColor :: Lens.Lens' BurnInDestinationSettings (Lude.Maybe BurnInOutlineColor)
bidsOutlineColor = Lens.lens (outlineColor :: BurnInDestinationSettings -> Lude.Maybe BurnInOutlineColor) (\s a -> s {outlineColor = a} :: BurnInDestinationSettings)
{-# DEPRECATED bidsOutlineColor "Use generic-lens or generic-optics with 'outlineColor' instead." #-}

-- | Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'outlineSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bidsOutlineSize :: Lens.Lens' BurnInDestinationSettings (Lude.Maybe Lude.Natural)
bidsOutlineSize = Lens.lens (outlineSize :: BurnInDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {outlineSize = a} :: BurnInDestinationSettings)
{-# DEPRECATED bidsOutlineSize "Use generic-lens or generic-optics with 'outlineSize' instead." #-}

-- | External font file used for caption burn-in. File extension must be 'ttf' or 'tte'.  Although the user can select output fonts for many different types of input captions,  embedded, STL and teletext sources use a strict grid system. Using external fonts with these caption sources could cause unexpected display of proportional fonts.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'font' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bidsFont :: Lens.Lens' BurnInDestinationSettings (Lude.Maybe InputLocation)
bidsFont = Lens.lens (font :: BurnInDestinationSettings -> Lude.Maybe InputLocation) (\s a -> s {font = a} :: BurnInDestinationSettings)
{-# DEPRECATED bidsFont "Use generic-lens or generic-optics with 'font' instead." #-}

-- | Specifies the color of the shadow cast by the captions.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bidsShadowColor :: Lens.Lens' BurnInDestinationSettings (Lude.Maybe BurnInShadowColor)
bidsShadowColor = Lens.lens (shadowColor :: BurnInDestinationSettings -> Lude.Maybe BurnInShadowColor) (\s a -> s {shadowColor = a} :: BurnInDestinationSettings)
{-# DEPRECATED bidsShadowColor "Use generic-lens or generic-optics with 'shadowColor' instead." #-}

-- | Specifies the color of the burned-in captions.  This option is not valid for source captions that are STL, 608/embedded or teletext.  These source settings are already pre-defined by the caption stream.  All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bidsFontColor :: Lens.Lens' BurnInDestinationSettings (Lude.Maybe BurnInFontColor)
bidsFontColor = Lens.lens (fontColor :: BurnInDestinationSettings -> Lude.Maybe BurnInFontColor) (\s a -> s {fontColor = a} :: BurnInDestinationSettings)
{-# DEPRECATED bidsFontColor "Use generic-lens or generic-optics with 'fontColor' instead." #-}

instance Lude.FromJSON BurnInDestinationSettings where
  parseJSON =
    Lude.withObject
      "BurnInDestinationSettings"
      ( \x ->
          BurnInDestinationSettings'
            Lude.<$> (x Lude..:? "backgroundOpacity")
            Lude.<*> (x Lude..:? "fontOpacity")
            Lude.<*> (x Lude..:? "shadowYOffset")
            Lude.<*> (x Lude..:? "fontResolution")
            Lude.<*> (x Lude..:? "yPosition")
            Lude.<*> (x Lude..:? "backgroundColor")
            Lude.<*> (x Lude..:? "shadowXOffset")
            Lude.<*> (x Lude..:? "fontSize")
            Lude.<*> (x Lude..:? "xPosition")
            Lude.<*> (x Lude..:? "alignment")
            Lude.<*> (x Lude..:? "shadowOpacity")
            Lude.<*> (x Lude..:? "teletextGridControl")
            Lude.<*> (x Lude..:? "outlineColor")
            Lude.<*> (x Lude..:? "outlineSize")
            Lude.<*> (x Lude..:? "font")
            Lude.<*> (x Lude..:? "shadowColor")
            Lude.<*> (x Lude..:? "fontColor")
      )

instance Lude.ToJSON BurnInDestinationSettings where
  toJSON BurnInDestinationSettings' {..} =
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
            ("alignment" Lude..=) Lude.<$> alignment,
            ("shadowOpacity" Lude..=) Lude.<$> shadowOpacity,
            ("teletextGridControl" Lude..=) Lude.<$> teletextGridControl,
            ("outlineColor" Lude..=) Lude.<$> outlineColor,
            ("outlineSize" Lude..=) Lude.<$> outlineSize,
            ("font" Lude..=) Lude.<$> font,
            ("shadowColor" Lude..=) Lude.<$> shadowColor,
            ("fontColor" Lude..=) Lude.<$> fontColor
          ]
      )
