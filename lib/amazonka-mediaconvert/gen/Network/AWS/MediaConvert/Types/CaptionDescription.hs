{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CaptionDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CaptionDescription where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.CaptionDestinationSettings
import Network.AWS.MediaConvert.Types.LanguageCode
import Network.AWS.Prelude

-- | Description of Caption output
--
-- /See:/ 'captionDescription' smart constructor.
data CaptionDescription = CaptionDescription'
  { _cdCaptionSelectorName ::
      !(Maybe Text),
    _cdCustomLanguageCode :: !(Maybe Text),
    _cdLanguageCode :: !(Maybe LanguageCode),
    _cdDestinationSettings ::
      !(Maybe CaptionDestinationSettings),
    _cdLanguageDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CaptionDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdCaptionSelectorName' - <N>", which denotes that the Nth Caption Selector will be used from each input.
--
-- * 'cdCustomLanguageCode' - Specify the language for this captions output track. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information when automatically selecting the font script for rendering the captions text. For all outputs, you can use an ISO 639-2 or ISO 639-3 code. For streaming outputs, you can also use any other code in the full RFC-5646 specification. Streaming outputs are those that are in one of the following output groups: CMAF, DASH ISO, Apple HLS, or Microsoft Smooth Streaming.
--
-- * 'cdLanguageCode' - Specify the language of this captions output track. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information to choose the font language for rendering the captions text.
--
-- * 'cdDestinationSettings' - Specific settings required by destination type. Note that burnin_destination_settings are not available if the source of the caption data is Embedded or Teletext.
--
-- * 'cdLanguageDescription' - Specify a label for this set of output captions. For example, "English", "Director commentary", or "track_2". For streaming outputs, MediaConvert passes this information into destination manifests for display on the end-viewer's player device. For outputs in other output groups, the service ignores this setting.
captionDescription ::
  CaptionDescription
captionDescription =
  CaptionDescription'
    { _cdCaptionSelectorName = Nothing,
      _cdCustomLanguageCode = Nothing,
      _cdLanguageCode = Nothing,
      _cdDestinationSettings = Nothing,
      _cdLanguageDescription = Nothing
    }

-- | <N>", which denotes that the Nth Caption Selector will be used from each input.
cdCaptionSelectorName :: Lens' CaptionDescription (Maybe Text)
cdCaptionSelectorName = lens _cdCaptionSelectorName (\s a -> s {_cdCaptionSelectorName = a})

-- | Specify the language for this captions output track. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information when automatically selecting the font script for rendering the captions text. For all outputs, you can use an ISO 639-2 or ISO 639-3 code. For streaming outputs, you can also use any other code in the full RFC-5646 specification. Streaming outputs are those that are in one of the following output groups: CMAF, DASH ISO, Apple HLS, or Microsoft Smooth Streaming.
cdCustomLanguageCode :: Lens' CaptionDescription (Maybe Text)
cdCustomLanguageCode = lens _cdCustomLanguageCode (\s a -> s {_cdCustomLanguageCode = a})

-- | Specify the language of this captions output track. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information to choose the font language for rendering the captions text.
cdLanguageCode :: Lens' CaptionDescription (Maybe LanguageCode)
cdLanguageCode = lens _cdLanguageCode (\s a -> s {_cdLanguageCode = a})

-- | Specific settings required by destination type. Note that burnin_destination_settings are not available if the source of the caption data is Embedded or Teletext.
cdDestinationSettings :: Lens' CaptionDescription (Maybe CaptionDestinationSettings)
cdDestinationSettings = lens _cdDestinationSettings (\s a -> s {_cdDestinationSettings = a})

-- | Specify a label for this set of output captions. For example, "English", "Director commentary", or "track_2". For streaming outputs, MediaConvert passes this information into destination manifests for display on the end-viewer's player device. For outputs in other output groups, the service ignores this setting.
cdLanguageDescription :: Lens' CaptionDescription (Maybe Text)
cdLanguageDescription = lens _cdLanguageDescription (\s a -> s {_cdLanguageDescription = a})

instance FromJSON CaptionDescription where
  parseJSON =
    withObject
      "CaptionDescription"
      ( \x ->
          CaptionDescription'
            <$> (x .:? "captionSelectorName")
            <*> (x .:? "customLanguageCode")
            <*> (x .:? "languageCode")
            <*> (x .:? "destinationSettings")
            <*> (x .:? "languageDescription")
      )

instance Hashable CaptionDescription

instance NFData CaptionDescription

instance ToJSON CaptionDescription where
  toJSON CaptionDescription' {..} =
    object
      ( catMaybes
          [ ("captionSelectorName" .=) <$> _cdCaptionSelectorName,
            ("customLanguageCode" .=) <$> _cdCustomLanguageCode,
            ("languageCode" .=) <$> _cdLanguageCode,
            ("destinationSettings" .=) <$> _cdDestinationSettings,
            ("languageDescription" .=) <$> _cdLanguageDescription
          ]
      )
