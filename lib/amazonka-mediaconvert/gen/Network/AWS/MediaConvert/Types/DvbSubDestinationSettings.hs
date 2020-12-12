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
    dsdsBackgroundOpacity,
    dsdsFontOpacity,
    dsdsShadowYOffset,
    dsdsFontResolution,
    dsdsYPosition,
    dsdsBackgroundColor,
    dsdsShadowXOffset,
    dsdsFontSize,
    dsdsXPosition,
    dsdsTeletextSpacing,
    dsdsFontScript,
    dsdsAlignment,
    dsdsShadowOpacity,
    dsdsOutlineColor,
    dsdsOutlineSize,
    dsdsShadowColor,
    dsdsFontColor,
    dsdsSubtitlingType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.DvbSubtitleAlignment
import Network.AWS.MediaConvert.Types.DvbSubtitleBackgroundColor
import Network.AWS.MediaConvert.Types.DvbSubtitleFontColor
import Network.AWS.MediaConvert.Types.DvbSubtitleOutlineColor
import Network.AWS.MediaConvert.Types.DvbSubtitleShadowColor
import Network.AWS.MediaConvert.Types.DvbSubtitleTeletextSpacing
import Network.AWS.MediaConvert.Types.DvbSubtitlingType
import Network.AWS.MediaConvert.Types.FontScript
import qualified Network.AWS.Prelude as Lude

-- | DVB-Sub Destination Settings
--
-- /See:/ 'mkDvbSubDestinationSettings' smart constructor.
data DvbSubDestinationSettings = DvbSubDestinationSettings'
  { backgroundOpacity ::
      Lude.Maybe Lude.Natural,
    fontOpacity :: Lude.Maybe Lude.Natural,
    shadowYOffset :: Lude.Maybe Lude.Int,
    fontResolution ::
      Lude.Maybe Lude.Natural,
    yPosition :: Lude.Maybe Lude.Natural,
    backgroundColor ::
      Lude.Maybe DvbSubtitleBackgroundColor,
    shadowXOffset :: Lude.Maybe Lude.Int,
    fontSize :: Lude.Maybe Lude.Natural,
    xPosition :: Lude.Maybe Lude.Natural,
    teletextSpacing ::
      Lude.Maybe DvbSubtitleTeletextSpacing,
    fontScript :: Lude.Maybe FontScript,
    alignment ::
      Lude.Maybe DvbSubtitleAlignment,
    shadowOpacity ::
      Lude.Maybe Lude.Natural,
    outlineColor ::
      Lude.Maybe DvbSubtitleOutlineColor,
    outlineSize :: Lude.Maybe Lude.Natural,
    shadowColor ::
      Lude.Maybe DvbSubtitleShadowColor,
    fontColor ::
      Lude.Maybe DvbSubtitleFontColor,
    subtitlingType ::
      Lude.Maybe DvbSubtitlingType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DvbSubDestinationSettings' with the minimum fields required to make a request.
--
-- * 'alignment' - If no explicit x_position or y_position is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
-- * 'backgroundColor' - Specifies the color of the rectangle behind the captions.
--
-- All burn-in and DVB-Sub font settings must match.
-- * 'backgroundOpacity' - Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
-- * 'fontColor' - Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
-- * 'fontOpacity' - Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent.
--
-- All burn-in and DVB-Sub font settings must match.
-- * 'fontResolution' - Font resolution in DPI (dots per inch); default is 96 dpi.
--
-- All burn-in and DVB-Sub font settings must match.
-- * 'fontScript' - Provide the font script, using an ISO 15924 script code, if the LanguageCode is not sufficient for determining the script type. Where LanguageCode or CustomLanguageCode is sufficient, use "AUTOMATIC" or leave unset. This is used to help determine the appropriate font for rendering DVB-Sub captions.
-- * 'fontSize' - A positive integer indicates the exact font size in points. Set to 0 for automatic font size selection. All burn-in and DVB-Sub font settings must match.
-- * 'outlineColor' - Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
-- * 'outlineSize' - Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
-- * 'shadowColor' - Specifies the color of the shadow cast by the captions.
--
-- All burn-in and DVB-Sub font settings must match.
-- * 'shadowOpacity' - Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
-- * 'shadowXOffset' - Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left. All burn-in and DVB-Sub font settings must match.
-- * 'shadowYOffset' - Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text. All burn-in and DVB-Sub font settings must match.
-- * 'subtitlingType' - Specify whether your DVB subtitles are standard or for hearing impaired. Choose hearing impaired if your subtitles include audio descriptions and dialogue. Choose standard if your subtitles include only dialogue.
-- * 'teletextSpacing' - Only applies to jobs with input captions in Teletext or STL formats. Specify whether the spacing between letters in your captions is set by the captions grid or varies depending on letter width. Choose fixed grid to conform to the spacing specified in the captions file more accurately. Choose proportional to make the text easier to read if the captions are closed caption.
-- * 'xPosition' - Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit x_position is provided, the horizontal caption position will be determined by the alignment parameter. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
-- * 'yPosition' - Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit y_position is provided, the caption will be positioned towards the bottom of the output. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
mkDvbSubDestinationSettings ::
  DvbSubDestinationSettings
mkDvbSubDestinationSettings =
  DvbSubDestinationSettings'
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
      fontColor = Lude.Nothing,
      subtitlingType = Lude.Nothing
    }

-- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'backgroundOpacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsBackgroundOpacity :: Lens.Lens' DvbSubDestinationSettings (Lude.Maybe Lude.Natural)
dsdsBackgroundOpacity = Lens.lens (backgroundOpacity :: DvbSubDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {backgroundOpacity = a} :: DvbSubDestinationSettings)
{-# DEPRECATED dsdsBackgroundOpacity "Use generic-lens or generic-optics with 'backgroundOpacity' instead." #-}

-- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent.
--
-- All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontOpacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsFontOpacity :: Lens.Lens' DvbSubDestinationSettings (Lude.Maybe Lude.Natural)
dsdsFontOpacity = Lens.lens (fontOpacity :: DvbSubDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {fontOpacity = a} :: DvbSubDestinationSettings)
{-# DEPRECATED dsdsFontOpacity "Use generic-lens or generic-optics with 'fontOpacity' instead." #-}

-- | Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowYOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsShadowYOffset :: Lens.Lens' DvbSubDestinationSettings (Lude.Maybe Lude.Int)
dsdsShadowYOffset = Lens.lens (shadowYOffset :: DvbSubDestinationSettings -> Lude.Maybe Lude.Int) (\s a -> s {shadowYOffset = a} :: DvbSubDestinationSettings)
{-# DEPRECATED dsdsShadowYOffset "Use generic-lens or generic-optics with 'shadowYOffset' instead." #-}

-- | Font resolution in DPI (dots per inch); default is 96 dpi.
--
-- All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsFontResolution :: Lens.Lens' DvbSubDestinationSettings (Lude.Maybe Lude.Natural)
dsdsFontResolution = Lens.lens (fontResolution :: DvbSubDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {fontResolution = a} :: DvbSubDestinationSettings)
{-# DEPRECATED dsdsFontResolution "Use generic-lens or generic-optics with 'fontResolution' instead." #-}

-- | Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit y_position is provided, the caption will be positioned towards the bottom of the output. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'yPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsYPosition :: Lens.Lens' DvbSubDestinationSettings (Lude.Maybe Lude.Natural)
dsdsYPosition = Lens.lens (yPosition :: DvbSubDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {yPosition = a} :: DvbSubDestinationSettings)
{-# DEPRECATED dsdsYPosition "Use generic-lens or generic-optics with 'yPosition' instead." #-}

-- | Specifies the color of the rectangle behind the captions.
--
-- All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'backgroundColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsBackgroundColor :: Lens.Lens' DvbSubDestinationSettings (Lude.Maybe DvbSubtitleBackgroundColor)
dsdsBackgroundColor = Lens.lens (backgroundColor :: DvbSubDestinationSettings -> Lude.Maybe DvbSubtitleBackgroundColor) (\s a -> s {backgroundColor = a} :: DvbSubDestinationSettings)
{-# DEPRECATED dsdsBackgroundColor "Use generic-lens or generic-optics with 'backgroundColor' instead." #-}

-- | Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowXOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsShadowXOffset :: Lens.Lens' DvbSubDestinationSettings (Lude.Maybe Lude.Int)
dsdsShadowXOffset = Lens.lens (shadowXOffset :: DvbSubDestinationSettings -> Lude.Maybe Lude.Int) (\s a -> s {shadowXOffset = a} :: DvbSubDestinationSettings)
{-# DEPRECATED dsdsShadowXOffset "Use generic-lens or generic-optics with 'shadowXOffset' instead." #-}

-- | A positive integer indicates the exact font size in points. Set to 0 for automatic font size selection. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsFontSize :: Lens.Lens' DvbSubDestinationSettings (Lude.Maybe Lude.Natural)
dsdsFontSize = Lens.lens (fontSize :: DvbSubDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {fontSize = a} :: DvbSubDestinationSettings)
{-# DEPRECATED dsdsFontSize "Use generic-lens or generic-optics with 'fontSize' instead." #-}

-- | Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit x_position is provided, the horizontal caption position will be determined by the alignment parameter. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'xPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsXPosition :: Lens.Lens' DvbSubDestinationSettings (Lude.Maybe Lude.Natural)
dsdsXPosition = Lens.lens (xPosition :: DvbSubDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {xPosition = a} :: DvbSubDestinationSettings)
{-# DEPRECATED dsdsXPosition "Use generic-lens or generic-optics with 'xPosition' instead." #-}

-- | Only applies to jobs with input captions in Teletext or STL formats. Specify whether the spacing between letters in your captions is set by the captions grid or varies depending on letter width. Choose fixed grid to conform to the spacing specified in the captions file more accurately. Choose proportional to make the text easier to read if the captions are closed caption.
--
-- /Note:/ Consider using 'teletextSpacing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsTeletextSpacing :: Lens.Lens' DvbSubDestinationSettings (Lude.Maybe DvbSubtitleTeletextSpacing)
dsdsTeletextSpacing = Lens.lens (teletextSpacing :: DvbSubDestinationSettings -> Lude.Maybe DvbSubtitleTeletextSpacing) (\s a -> s {teletextSpacing = a} :: DvbSubDestinationSettings)
{-# DEPRECATED dsdsTeletextSpacing "Use generic-lens or generic-optics with 'teletextSpacing' instead." #-}

-- | Provide the font script, using an ISO 15924 script code, if the LanguageCode is not sufficient for determining the script type. Where LanguageCode or CustomLanguageCode is sufficient, use "AUTOMATIC" or leave unset. This is used to help determine the appropriate font for rendering DVB-Sub captions.
--
-- /Note:/ Consider using 'fontScript' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsFontScript :: Lens.Lens' DvbSubDestinationSettings (Lude.Maybe FontScript)
dsdsFontScript = Lens.lens (fontScript :: DvbSubDestinationSettings -> Lude.Maybe FontScript) (\s a -> s {fontScript = a} :: DvbSubDestinationSettings)
{-# DEPRECATED dsdsFontScript "Use generic-lens or generic-optics with 'fontScript' instead." #-}

-- | If no explicit x_position or y_position is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'alignment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsAlignment :: Lens.Lens' DvbSubDestinationSettings (Lude.Maybe DvbSubtitleAlignment)
dsdsAlignment = Lens.lens (alignment :: DvbSubDestinationSettings -> Lude.Maybe DvbSubtitleAlignment) (\s a -> s {alignment = a} :: DvbSubDestinationSettings)
{-# DEPRECATED dsdsAlignment "Use generic-lens or generic-optics with 'alignment' instead." #-}

-- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowOpacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsShadowOpacity :: Lens.Lens' DvbSubDestinationSettings (Lude.Maybe Lude.Natural)
dsdsShadowOpacity = Lens.lens (shadowOpacity :: DvbSubDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {shadowOpacity = a} :: DvbSubDestinationSettings)
{-# DEPRECATED dsdsShadowOpacity "Use generic-lens or generic-optics with 'shadowOpacity' instead." #-}

-- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'outlineColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsOutlineColor :: Lens.Lens' DvbSubDestinationSettings (Lude.Maybe DvbSubtitleOutlineColor)
dsdsOutlineColor = Lens.lens (outlineColor :: DvbSubDestinationSettings -> Lude.Maybe DvbSubtitleOutlineColor) (\s a -> s {outlineColor = a} :: DvbSubDestinationSettings)
{-# DEPRECATED dsdsOutlineColor "Use generic-lens or generic-optics with 'outlineColor' instead." #-}

-- | Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'outlineSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsOutlineSize :: Lens.Lens' DvbSubDestinationSettings (Lude.Maybe Lude.Natural)
dsdsOutlineSize = Lens.lens (outlineSize :: DvbSubDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {outlineSize = a} :: DvbSubDestinationSettings)
{-# DEPRECATED dsdsOutlineSize "Use generic-lens or generic-optics with 'outlineSize' instead." #-}

-- | Specifies the color of the shadow cast by the captions.
--
-- All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'shadowColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsShadowColor :: Lens.Lens' DvbSubDestinationSettings (Lude.Maybe DvbSubtitleShadowColor)
dsdsShadowColor = Lens.lens (shadowColor :: DvbSubDestinationSettings -> Lude.Maybe DvbSubtitleShadowColor) (\s a -> s {shadowColor = a} :: DvbSubDestinationSettings)
{-# DEPRECATED dsdsShadowColor "Use generic-lens or generic-optics with 'shadowColor' instead." #-}

-- | Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- /Note:/ Consider using 'fontColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsFontColor :: Lens.Lens' DvbSubDestinationSettings (Lude.Maybe DvbSubtitleFontColor)
dsdsFontColor = Lens.lens (fontColor :: DvbSubDestinationSettings -> Lude.Maybe DvbSubtitleFontColor) (\s a -> s {fontColor = a} :: DvbSubDestinationSettings)
{-# DEPRECATED dsdsFontColor "Use generic-lens or generic-optics with 'fontColor' instead." #-}

-- | Specify whether your DVB subtitles are standard or for hearing impaired. Choose hearing impaired if your subtitles include audio descriptions and dialogue. Choose standard if your subtitles include only dialogue.
--
-- /Note:/ Consider using 'subtitlingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsSubtitlingType :: Lens.Lens' DvbSubDestinationSettings (Lude.Maybe DvbSubtitlingType)
dsdsSubtitlingType = Lens.lens (subtitlingType :: DvbSubDestinationSettings -> Lude.Maybe DvbSubtitlingType) (\s a -> s {subtitlingType = a} :: DvbSubDestinationSettings)
{-# DEPRECATED dsdsSubtitlingType "Use generic-lens or generic-optics with 'subtitlingType' instead." #-}

instance Lude.FromJSON DvbSubDestinationSettings where
  parseJSON =
    Lude.withObject
      "DvbSubDestinationSettings"
      ( \x ->
          DvbSubDestinationSettings'
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
            Lude.<*> (x Lude..:? "subtitlingType")
      )

instance Lude.ToJSON DvbSubDestinationSettings where
  toJSON DvbSubDestinationSettings' {..} =
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
            ("fontColor" Lude..=) Lude.<$> fontColor,
            ("subtitlingType" Lude..=) Lude.<$> subtitlingType
          ]
      )
