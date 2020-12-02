{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubDestinationSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.DvbSubtitleAlignment
import Network.AWS.MediaConvert.Types.DvbSubtitleBackgroundColor
import Network.AWS.MediaConvert.Types.DvbSubtitleFontColor
import Network.AWS.MediaConvert.Types.DvbSubtitleOutlineColor
import Network.AWS.MediaConvert.Types.DvbSubtitleShadowColor
import Network.AWS.MediaConvert.Types.DvbSubtitleTeletextSpacing
import Network.AWS.MediaConvert.Types.DvbSubtitlingType
import Network.AWS.MediaConvert.Types.FontScript
import Network.AWS.Prelude

-- | DVB-Sub Destination Settings
--
-- /See:/ 'dvbSubDestinationSettings' smart constructor.
data DvbSubDestinationSettings = DvbSubDestinationSettings'
  { _dsdsBackgroundOpacity ::
      !(Maybe Nat),
    _dsdsFontOpacity :: !(Maybe Nat),
    _dsdsShadowYOffset :: !(Maybe Int),
    _dsdsFontResolution :: !(Maybe Nat),
    _dsdsYPosition :: !(Maybe Nat),
    _dsdsBackgroundColor ::
      !(Maybe DvbSubtitleBackgroundColor),
    _dsdsShadowXOffset :: !(Maybe Int),
    _dsdsFontSize :: !(Maybe Nat),
    _dsdsXPosition :: !(Maybe Nat),
    _dsdsTeletextSpacing ::
      !(Maybe DvbSubtitleTeletextSpacing),
    _dsdsFontScript :: !(Maybe FontScript),
    _dsdsAlignment ::
      !(Maybe DvbSubtitleAlignment),
    _dsdsShadowOpacity :: !(Maybe Nat),
    _dsdsOutlineColor ::
      !(Maybe DvbSubtitleOutlineColor),
    _dsdsOutlineSize :: !(Maybe Nat),
    _dsdsShadowColor ::
      !(Maybe DvbSubtitleShadowColor),
    _dsdsFontColor ::
      !(Maybe DvbSubtitleFontColor),
    _dsdsSubtitlingType ::
      !(Maybe DvbSubtitlingType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DvbSubDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsdsBackgroundOpacity' - Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsFontOpacity' - Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent. All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsShadowYOffset' - Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text. All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsFontResolution' - Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsYPosition' - Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit y_position is provided, the caption will be positioned towards the bottom of the output. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsBackgroundColor' - Specifies the color of the rectangle behind the captions. All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsShadowXOffset' - Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left. All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsFontSize' - A positive integer indicates the exact font size in points. Set to 0 for automatic font size selection. All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsXPosition' - Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit x_position is provided, the horizontal caption position will be determined by the alignment parameter. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsTeletextSpacing' - Only applies to jobs with input captions in Teletext or STL formats. Specify whether the spacing between letters in your captions is set by the captions grid or varies depending on letter width. Choose fixed grid to conform to the spacing specified in the captions file more accurately. Choose proportional to make the text easier to read if the captions are closed caption.
--
-- * 'dsdsFontScript' - Provide the font script, using an ISO 15924 script code, if the LanguageCode is not sufficient for determining the script type. Where LanguageCode or CustomLanguageCode is sufficient, use "AUTOMATIC" or leave unset. This is used to help determine the appropriate font for rendering DVB-Sub captions.
--
-- * 'dsdsAlignment' - If no explicit x_position or y_position is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsShadowOpacity' - Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsOutlineColor' - Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsOutlineSize' - Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsShadowColor' - Specifies the color of the shadow cast by the captions. All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsFontColor' - Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsSubtitlingType' - Specify whether your DVB subtitles are standard or for hearing impaired. Choose hearing impaired if your subtitles include audio descriptions and dialogue. Choose standard if your subtitles include only dialogue.
dvbSubDestinationSettings ::
  DvbSubDestinationSettings
dvbSubDestinationSettings =
  DvbSubDestinationSettings'
    { _dsdsBackgroundOpacity = Nothing,
      _dsdsFontOpacity = Nothing,
      _dsdsShadowYOffset = Nothing,
      _dsdsFontResolution = Nothing,
      _dsdsYPosition = Nothing,
      _dsdsBackgroundColor = Nothing,
      _dsdsShadowXOffset = Nothing,
      _dsdsFontSize = Nothing,
      _dsdsXPosition = Nothing,
      _dsdsTeletextSpacing = Nothing,
      _dsdsFontScript = Nothing,
      _dsdsAlignment = Nothing,
      _dsdsShadowOpacity = Nothing,
      _dsdsOutlineColor = Nothing,
      _dsdsOutlineSize = Nothing,
      _dsdsShadowColor = Nothing,
      _dsdsFontColor = Nothing,
      _dsdsSubtitlingType = Nothing
    }

-- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
dsdsBackgroundOpacity :: Lens' DvbSubDestinationSettings (Maybe Natural)
dsdsBackgroundOpacity = lens _dsdsBackgroundOpacity (\s a -> s {_dsdsBackgroundOpacity = a}) . mapping _Nat

-- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent. All burn-in and DVB-Sub font settings must match.
dsdsFontOpacity :: Lens' DvbSubDestinationSettings (Maybe Natural)
dsdsFontOpacity = lens _dsdsFontOpacity (\s a -> s {_dsdsFontOpacity = a}) . mapping _Nat

-- | Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text. All burn-in and DVB-Sub font settings must match.
dsdsShadowYOffset :: Lens' DvbSubDestinationSettings (Maybe Int)
dsdsShadowYOffset = lens _dsdsShadowYOffset (\s a -> s {_dsdsShadowYOffset = a})

-- | Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in and DVB-Sub font settings must match.
dsdsFontResolution :: Lens' DvbSubDestinationSettings (Maybe Natural)
dsdsFontResolution = lens _dsdsFontResolution (\s a -> s {_dsdsFontResolution = a}) . mapping _Nat

-- | Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit y_position is provided, the caption will be positioned towards the bottom of the output. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
dsdsYPosition :: Lens' DvbSubDestinationSettings (Maybe Natural)
dsdsYPosition = lens _dsdsYPosition (\s a -> s {_dsdsYPosition = a}) . mapping _Nat

-- | Specifies the color of the rectangle behind the captions. All burn-in and DVB-Sub font settings must match.
dsdsBackgroundColor :: Lens' DvbSubDestinationSettings (Maybe DvbSubtitleBackgroundColor)
dsdsBackgroundColor = lens _dsdsBackgroundColor (\s a -> s {_dsdsBackgroundColor = a})

-- | Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left. All burn-in and DVB-Sub font settings must match.
dsdsShadowXOffset :: Lens' DvbSubDestinationSettings (Maybe Int)
dsdsShadowXOffset = lens _dsdsShadowXOffset (\s a -> s {_dsdsShadowXOffset = a})

-- | A positive integer indicates the exact font size in points. Set to 0 for automatic font size selection. All burn-in and DVB-Sub font settings must match.
dsdsFontSize :: Lens' DvbSubDestinationSettings (Maybe Natural)
dsdsFontSize = lens _dsdsFontSize (\s a -> s {_dsdsFontSize = a}) . mapping _Nat

-- | Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit x_position is provided, the horizontal caption position will be determined by the alignment parameter. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
dsdsXPosition :: Lens' DvbSubDestinationSettings (Maybe Natural)
dsdsXPosition = lens _dsdsXPosition (\s a -> s {_dsdsXPosition = a}) . mapping _Nat

-- | Only applies to jobs with input captions in Teletext or STL formats. Specify whether the spacing between letters in your captions is set by the captions grid or varies depending on letter width. Choose fixed grid to conform to the spacing specified in the captions file more accurately. Choose proportional to make the text easier to read if the captions are closed caption.
dsdsTeletextSpacing :: Lens' DvbSubDestinationSettings (Maybe DvbSubtitleTeletextSpacing)
dsdsTeletextSpacing = lens _dsdsTeletextSpacing (\s a -> s {_dsdsTeletextSpacing = a})

-- | Provide the font script, using an ISO 15924 script code, if the LanguageCode is not sufficient for determining the script type. Where LanguageCode or CustomLanguageCode is sufficient, use "AUTOMATIC" or leave unset. This is used to help determine the appropriate font for rendering DVB-Sub captions.
dsdsFontScript :: Lens' DvbSubDestinationSettings (Maybe FontScript)
dsdsFontScript = lens _dsdsFontScript (\s a -> s {_dsdsFontScript = a})

-- | If no explicit x_position or y_position is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
dsdsAlignment :: Lens' DvbSubDestinationSettings (Maybe DvbSubtitleAlignment)
dsdsAlignment = lens _dsdsAlignment (\s a -> s {_dsdsAlignment = a})

-- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
dsdsShadowOpacity :: Lens' DvbSubDestinationSettings (Maybe Natural)
dsdsShadowOpacity = lens _dsdsShadowOpacity (\s a -> s {_dsdsShadowOpacity = a}) . mapping _Nat

-- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
dsdsOutlineColor :: Lens' DvbSubDestinationSettings (Maybe DvbSubtitleOutlineColor)
dsdsOutlineColor = lens _dsdsOutlineColor (\s a -> s {_dsdsOutlineColor = a})

-- | Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
dsdsOutlineSize :: Lens' DvbSubDestinationSettings (Maybe Natural)
dsdsOutlineSize = lens _dsdsOutlineSize (\s a -> s {_dsdsOutlineSize = a}) . mapping _Nat

-- | Specifies the color of the shadow cast by the captions. All burn-in and DVB-Sub font settings must match.
dsdsShadowColor :: Lens' DvbSubDestinationSettings (Maybe DvbSubtitleShadowColor)
dsdsShadowColor = lens _dsdsShadowColor (\s a -> s {_dsdsShadowColor = a})

-- | Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
dsdsFontColor :: Lens' DvbSubDestinationSettings (Maybe DvbSubtitleFontColor)
dsdsFontColor = lens _dsdsFontColor (\s a -> s {_dsdsFontColor = a})

-- | Specify whether your DVB subtitles are standard or for hearing impaired. Choose hearing impaired if your subtitles include audio descriptions and dialogue. Choose standard if your subtitles include only dialogue.
dsdsSubtitlingType :: Lens' DvbSubDestinationSettings (Maybe DvbSubtitlingType)
dsdsSubtitlingType = lens _dsdsSubtitlingType (\s a -> s {_dsdsSubtitlingType = a})

instance FromJSON DvbSubDestinationSettings where
  parseJSON =
    withObject
      "DvbSubDestinationSettings"
      ( \x ->
          DvbSubDestinationSettings'
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
            <*> (x .:? "subtitlingType")
      )

instance Hashable DvbSubDestinationSettings

instance NFData DvbSubDestinationSettings

instance ToJSON DvbSubDestinationSettings where
  toJSON DvbSubDestinationSettings' {..} =
    object
      ( catMaybes
          [ ("backgroundOpacity" .=) <$> _dsdsBackgroundOpacity,
            ("fontOpacity" .=) <$> _dsdsFontOpacity,
            ("shadowYOffset" .=) <$> _dsdsShadowYOffset,
            ("fontResolution" .=) <$> _dsdsFontResolution,
            ("yPosition" .=) <$> _dsdsYPosition,
            ("backgroundColor" .=) <$> _dsdsBackgroundColor,
            ("shadowXOffset" .=) <$> _dsdsShadowXOffset,
            ("fontSize" .=) <$> _dsdsFontSize,
            ("xPosition" .=) <$> _dsdsXPosition,
            ("teletextSpacing" .=) <$> _dsdsTeletextSpacing,
            ("fontScript" .=) <$> _dsdsFontScript,
            ("alignment" .=) <$> _dsdsAlignment,
            ("shadowOpacity" .=) <$> _dsdsShadowOpacity,
            ("outlineColor" .=) <$> _dsdsOutlineColor,
            ("outlineSize" .=) <$> _dsdsOutlineSize,
            ("shadowColor" .=) <$> _dsdsShadowColor,
            ("fontColor" .=) <$> _dsdsFontColor,
            ("subtitlingType" .=) <$> _dsdsSubtitlingType
          ]
      )
