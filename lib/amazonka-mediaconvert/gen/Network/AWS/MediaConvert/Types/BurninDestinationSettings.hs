{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.BurninDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.BurninDestinationSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.BurninSubtitleAlignment
import Network.AWS.MediaConvert.Types.BurninSubtitleBackgroundColor
import Network.AWS.MediaConvert.Types.BurninSubtitleFontColor
import Network.AWS.MediaConvert.Types.BurninSubtitleOutlineColor
import Network.AWS.MediaConvert.Types.BurninSubtitleShadowColor
import Network.AWS.MediaConvert.Types.BurninSubtitleTeletextSpacing
import Network.AWS.MediaConvert.Types.FontScript
import Network.AWS.Prelude

-- | Burn-In Destination Settings.
--
-- /See:/ 'burninDestinationSettings' smart constructor.
data BurninDestinationSettings = BurninDestinationSettings'
  { _bdsBackgroundOpacity ::
      !(Maybe Nat),
    _bdsFontOpacity :: !(Maybe Nat),
    _bdsShadowYOffset :: !(Maybe Int),
    _bdsFontResolution :: !(Maybe Nat),
    _bdsYPosition :: !(Maybe Nat),
    _bdsBackgroundColor ::
      !(Maybe BurninSubtitleBackgroundColor),
    _bdsShadowXOffset :: !(Maybe Int),
    _bdsFontSize :: !(Maybe Nat),
    _bdsXPosition :: !(Maybe Nat),
    _bdsTeletextSpacing ::
      !(Maybe BurninSubtitleTeletextSpacing),
    _bdsFontScript :: !(Maybe FontScript),
    _bdsAlignment ::
      !(Maybe BurninSubtitleAlignment),
    _bdsShadowOpacity :: !(Maybe Nat),
    _bdsOutlineColor ::
      !(Maybe BurninSubtitleOutlineColor),
    _bdsOutlineSize :: !(Maybe Nat),
    _bdsShadowColor ::
      !(Maybe BurninSubtitleShadowColor),
    _bdsFontColor ::
      !(Maybe BurninSubtitleFontColor)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BurninDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdsBackgroundOpacity' - Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsFontOpacity' - Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent. All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsShadowYOffset' - Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text. All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsFontResolution' - Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsYPosition' - Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit y_position is provided, the caption will be positioned towards the bottom of the output. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsBackgroundColor' - Specifies the color of the rectangle behind the captions. All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsShadowXOffset' - Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left. All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsFontSize' - A positive integer indicates the exact font size in points. Set to 0 for automatic font size selection. All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsXPosition' - Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit x_position is provided, the horizontal caption position will be determined by the alignment parameter. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsTeletextSpacing' - Only applies to jobs with input captions in Teletext or STL formats. Specify whether the spacing between letters in your captions is set by the captions grid or varies depending on letter width. Choose fixed grid to conform to the spacing specified in the captions file more accurately. Choose proportional to make the text easier to read if the captions are closed caption.
--
-- * 'bdsFontScript' - Provide the font script, using an ISO 15924 script code, if the LanguageCode is not sufficient for determining the script type. Where LanguageCode or CustomLanguageCode is sufficient, use "AUTOMATIC" or leave unset. This is used to help determine the appropriate font for rendering burn-in captions.
--
-- * 'bdsAlignment' - If no explicit x_position or y_position is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsShadowOpacity' - Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsOutlineColor' - Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsOutlineSize' - Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsShadowColor' - Specifies the color of the shadow cast by the captions. All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsFontColor' - Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
burninDestinationSettings ::
  BurninDestinationSettings
burninDestinationSettings =
  BurninDestinationSettings'
    { _bdsBackgroundOpacity = Nothing,
      _bdsFontOpacity = Nothing,
      _bdsShadowYOffset = Nothing,
      _bdsFontResolution = Nothing,
      _bdsYPosition = Nothing,
      _bdsBackgroundColor = Nothing,
      _bdsShadowXOffset = Nothing,
      _bdsFontSize = Nothing,
      _bdsXPosition = Nothing,
      _bdsTeletextSpacing = Nothing,
      _bdsFontScript = Nothing,
      _bdsAlignment = Nothing,
      _bdsShadowOpacity = Nothing,
      _bdsOutlineColor = Nothing,
      _bdsOutlineSize = Nothing,
      _bdsShadowColor = Nothing,
      _bdsFontColor = Nothing
    }

-- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
bdsBackgroundOpacity :: Lens' BurninDestinationSettings (Maybe Natural)
bdsBackgroundOpacity = lens _bdsBackgroundOpacity (\s a -> s {_bdsBackgroundOpacity = a}) . mapping _Nat

-- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent. All burn-in and DVB-Sub font settings must match.
bdsFontOpacity :: Lens' BurninDestinationSettings (Maybe Natural)
bdsFontOpacity = lens _bdsFontOpacity (\s a -> s {_bdsFontOpacity = a}) . mapping _Nat

-- | Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text. All burn-in and DVB-Sub font settings must match.
bdsShadowYOffset :: Lens' BurninDestinationSettings (Maybe Int)
bdsShadowYOffset = lens _bdsShadowYOffset (\s a -> s {_bdsShadowYOffset = a})

-- | Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in and DVB-Sub font settings must match.
bdsFontResolution :: Lens' BurninDestinationSettings (Maybe Natural)
bdsFontResolution = lens _bdsFontResolution (\s a -> s {_bdsFontResolution = a}) . mapping _Nat

-- | Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit y_position is provided, the caption will be positioned towards the bottom of the output. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
bdsYPosition :: Lens' BurninDestinationSettings (Maybe Natural)
bdsYPosition = lens _bdsYPosition (\s a -> s {_bdsYPosition = a}) . mapping _Nat

-- | Specifies the color of the rectangle behind the captions. All burn-in and DVB-Sub font settings must match.
bdsBackgroundColor :: Lens' BurninDestinationSettings (Maybe BurninSubtitleBackgroundColor)
bdsBackgroundColor = lens _bdsBackgroundColor (\s a -> s {_bdsBackgroundColor = a})

-- | Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left. All burn-in and DVB-Sub font settings must match.
bdsShadowXOffset :: Lens' BurninDestinationSettings (Maybe Int)
bdsShadowXOffset = lens _bdsShadowXOffset (\s a -> s {_bdsShadowXOffset = a})

-- | A positive integer indicates the exact font size in points. Set to 0 for automatic font size selection. All burn-in and DVB-Sub font settings must match.
bdsFontSize :: Lens' BurninDestinationSettings (Maybe Natural)
bdsFontSize = lens _bdsFontSize (\s a -> s {_bdsFontSize = a}) . mapping _Nat

-- | Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit x_position is provided, the horizontal caption position will be determined by the alignment parameter. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
bdsXPosition :: Lens' BurninDestinationSettings (Maybe Natural)
bdsXPosition = lens _bdsXPosition (\s a -> s {_bdsXPosition = a}) . mapping _Nat

-- | Only applies to jobs with input captions in Teletext or STL formats. Specify whether the spacing between letters in your captions is set by the captions grid or varies depending on letter width. Choose fixed grid to conform to the spacing specified in the captions file more accurately. Choose proportional to make the text easier to read if the captions are closed caption.
bdsTeletextSpacing :: Lens' BurninDestinationSettings (Maybe BurninSubtitleTeletextSpacing)
bdsTeletextSpacing = lens _bdsTeletextSpacing (\s a -> s {_bdsTeletextSpacing = a})

-- | Provide the font script, using an ISO 15924 script code, if the LanguageCode is not sufficient for determining the script type. Where LanguageCode or CustomLanguageCode is sufficient, use "AUTOMATIC" or leave unset. This is used to help determine the appropriate font for rendering burn-in captions.
bdsFontScript :: Lens' BurninDestinationSettings (Maybe FontScript)
bdsFontScript = lens _bdsFontScript (\s a -> s {_bdsFontScript = a})

-- | If no explicit x_position or y_position is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
bdsAlignment :: Lens' BurninDestinationSettings (Maybe BurninSubtitleAlignment)
bdsAlignment = lens _bdsAlignment (\s a -> s {_bdsAlignment = a})

-- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
bdsShadowOpacity :: Lens' BurninDestinationSettings (Maybe Natural)
bdsShadowOpacity = lens _bdsShadowOpacity (\s a -> s {_bdsShadowOpacity = a}) . mapping _Nat

-- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
bdsOutlineColor :: Lens' BurninDestinationSettings (Maybe BurninSubtitleOutlineColor)
bdsOutlineColor = lens _bdsOutlineColor (\s a -> s {_bdsOutlineColor = a})

-- | Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
bdsOutlineSize :: Lens' BurninDestinationSettings (Maybe Natural)
bdsOutlineSize = lens _bdsOutlineSize (\s a -> s {_bdsOutlineSize = a}) . mapping _Nat

-- | Specifies the color of the shadow cast by the captions. All burn-in and DVB-Sub font settings must match.
bdsShadowColor :: Lens' BurninDestinationSettings (Maybe BurninSubtitleShadowColor)
bdsShadowColor = lens _bdsShadowColor (\s a -> s {_bdsShadowColor = a})

-- | Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
bdsFontColor :: Lens' BurninDestinationSettings (Maybe BurninSubtitleFontColor)
bdsFontColor = lens _bdsFontColor (\s a -> s {_bdsFontColor = a})

instance FromJSON BurninDestinationSettings where
  parseJSON =
    withObject
      "BurninDestinationSettings"
      ( \x ->
          BurninDestinationSettings'
            <$> (x .:? "backgroundOpacity")
            <*> (x .:? "fontOpacity")
            <*> (x .:? "shadowYOffset")
            <*> (x .:? "fontResolution")
            <*> (x .:? "yPosition")
            <*> (x .:? "backgroundColor")
            <*> (x .:? "shadowXOffset")
            <*> (x .:? "fontSize")
            <*> (x .:? "xPosition")
            <*> (x .:? "teletextSpacing")
            <*> (x .:? "fontScript")
            <*> (x .:? "alignment")
            <*> (x .:? "shadowOpacity")
            <*> (x .:? "outlineColor")
            <*> (x .:? "outlineSize")
            <*> (x .:? "shadowColor")
            <*> (x .:? "fontColor")
      )

instance Hashable BurninDestinationSettings

instance NFData BurninDestinationSettings

instance ToJSON BurninDestinationSettings where
  toJSON BurninDestinationSettings' {..} =
    object
      ( catMaybes
          [ ("backgroundOpacity" .=) <$> _bdsBackgroundOpacity,
            ("fontOpacity" .=) <$> _bdsFontOpacity,
            ("shadowYOffset" .=) <$> _bdsShadowYOffset,
            ("fontResolution" .=) <$> _bdsFontResolution,
            ("yPosition" .=) <$> _bdsYPosition,
            ("backgroundColor" .=) <$> _bdsBackgroundColor,
            ("shadowXOffset" .=) <$> _bdsShadowXOffset,
            ("fontSize" .=) <$> _bdsFontSize,
            ("xPosition" .=) <$> _bdsXPosition,
            ("teletextSpacing" .=) <$> _bdsTeletextSpacing,
            ("fontScript" .=) <$> _bdsFontScript,
            ("alignment" .=) <$> _bdsAlignment,
            ("shadowOpacity" .=) <$> _bdsShadowOpacity,
            ("outlineColor" .=) <$> _bdsOutlineColor,
            ("outlineSize" .=) <$> _bdsOutlineSize,
            ("shadowColor" .=) <$> _bdsShadowColor,
            ("fontColor" .=) <$> _bdsFontColor
          ]
      )
