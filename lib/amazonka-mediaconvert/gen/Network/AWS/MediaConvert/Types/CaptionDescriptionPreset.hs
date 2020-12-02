{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CaptionDescriptionPreset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CaptionDescriptionPreset where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.CaptionDestinationSettings
import Network.AWS.MediaConvert.Types.LanguageCode
import Network.AWS.Prelude

-- | Caption Description for preset
--
-- /See:/ 'captionDescriptionPreset' smart constructor.
data CaptionDescriptionPreset = CaptionDescriptionPreset'
  { _cdpCustomLanguageCode ::
      !(Maybe Text),
    _cdpLanguageCode :: !(Maybe LanguageCode),
    _cdpDestinationSettings ::
      !(Maybe CaptionDestinationSettings),
    _cdpLanguageDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CaptionDescriptionPreset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdpCustomLanguageCode' - Specify the language for this captions output track. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information when automatically selecting the font script for rendering the captions text. For all outputs, you can use an ISO 639-2 or ISO 639-3 code. For streaming outputs, you can also use any other code in the full RFC-5646 specification. Streaming outputs are those that are in one of the following output groups: CMAF, DASH ISO, Apple HLS, or Microsoft Smooth Streaming.
--
-- * 'cdpLanguageCode' - Specify the language of this captions output track. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information to choose the font language for rendering the captions text.
--
-- * 'cdpDestinationSettings' - Specific settings required by destination type. Note that burnin_destination_settings are not available if the source of the caption data is Embedded or Teletext.
--
-- * 'cdpLanguageDescription' - Specify a label for this set of output captions. For example, "English", "Director commentary", or "track_2". For streaming outputs, MediaConvert passes this information into destination manifests for display on the end-viewer's player device. For outputs in other output groups, the service ignores this setting.
captionDescriptionPreset ::
  CaptionDescriptionPreset
captionDescriptionPreset =
  CaptionDescriptionPreset'
    { _cdpCustomLanguageCode = Nothing,
      _cdpLanguageCode = Nothing,
      _cdpDestinationSettings = Nothing,
      _cdpLanguageDescription = Nothing
    }

-- | Specify the language for this captions output track. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information when automatically selecting the font script for rendering the captions text. For all outputs, you can use an ISO 639-2 or ISO 639-3 code. For streaming outputs, you can also use any other code in the full RFC-5646 specification. Streaming outputs are those that are in one of the following output groups: CMAF, DASH ISO, Apple HLS, or Microsoft Smooth Streaming.
cdpCustomLanguageCode :: Lens' CaptionDescriptionPreset (Maybe Text)
cdpCustomLanguageCode = lens _cdpCustomLanguageCode (\s a -> s {_cdpCustomLanguageCode = a})

-- | Specify the language of this captions output track. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information to choose the font language for rendering the captions text.
cdpLanguageCode :: Lens' CaptionDescriptionPreset (Maybe LanguageCode)
cdpLanguageCode = lens _cdpLanguageCode (\s a -> s {_cdpLanguageCode = a})

-- | Specific settings required by destination type. Note that burnin_destination_settings are not available if the source of the caption data is Embedded or Teletext.
cdpDestinationSettings :: Lens' CaptionDescriptionPreset (Maybe CaptionDestinationSettings)
cdpDestinationSettings = lens _cdpDestinationSettings (\s a -> s {_cdpDestinationSettings = a})

-- | Specify a label for this set of output captions. For example, "English", "Director commentary", or "track_2". For streaming outputs, MediaConvert passes this information into destination manifests for display on the end-viewer's player device. For outputs in other output groups, the service ignores this setting.
cdpLanguageDescription :: Lens' CaptionDescriptionPreset (Maybe Text)
cdpLanguageDescription = lens _cdpLanguageDescription (\s a -> s {_cdpLanguageDescription = a})

instance FromJSON CaptionDescriptionPreset where
  parseJSON =
    withObject
      "CaptionDescriptionPreset"
      ( \x ->
          CaptionDescriptionPreset'
            <$> (x .:? "customLanguageCode")
            <*> (x .:? "languageCode")
            <*> (x .:? "destinationSettings")
            <*> (x .:? "languageDescription")
      )

instance Hashable CaptionDescriptionPreset

instance NFData CaptionDescriptionPreset

instance ToJSON CaptionDescriptionPreset where
  toJSON CaptionDescriptionPreset' {..} =
    object
      ( catMaybes
          [ ("customLanguageCode" .=) <$> _cdpCustomLanguageCode,
            ("languageCode" .=) <$> _cdpLanguageCode,
            ("destinationSettings" .=) <$> _cdpDestinationSettings,
            ("languageDescription" .=) <$> _cdpLanguageDescription
          ]
      )
