{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.CaptionDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CaptionDescription where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.CaptionDestinationSettings
import Network.AWS.Prelude

-- | Caption Description
--
-- /See:/ 'captionDescription' smart constructor.
data CaptionDescription = CaptionDescription'
  { _cdLanguageCode ::
      !(Maybe Text),
    _cdDestinationSettings ::
      !(Maybe CaptionDestinationSettings),
    _cdLanguageDescription :: !(Maybe Text),
    _cdCaptionSelectorName :: !Text,
    _cdName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CaptionDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdLanguageCode' - ISO 639-2 three-digit code: http://www.loc.gov/standards/iso639-2/
--
-- * 'cdDestinationSettings' - Additional settings for captions destination that depend on the destination type.
--
-- * 'cdLanguageDescription' - Human readable information to indicate captions available for players (eg. English, or Spanish).
--
-- * 'cdCaptionSelectorName' - Specifies which input caption selector to use as a caption source when generating output captions. This field should match a captionSelector name.
--
-- * 'cdName' - Name of the caption description.  Used to associate a caption description with an output.  Names must be unique within an event.
captionDescription ::
  -- | 'cdCaptionSelectorName'
  Text ->
  -- | 'cdName'
  Text ->
  CaptionDescription
captionDescription pCaptionSelectorName_ pName_ =
  CaptionDescription'
    { _cdLanguageCode = Nothing,
      _cdDestinationSettings = Nothing,
      _cdLanguageDescription = Nothing,
      _cdCaptionSelectorName = pCaptionSelectorName_,
      _cdName = pName_
    }

-- | ISO 639-2 three-digit code: http://www.loc.gov/standards/iso639-2/
cdLanguageCode :: Lens' CaptionDescription (Maybe Text)
cdLanguageCode = lens _cdLanguageCode (\s a -> s {_cdLanguageCode = a})

-- | Additional settings for captions destination that depend on the destination type.
cdDestinationSettings :: Lens' CaptionDescription (Maybe CaptionDestinationSettings)
cdDestinationSettings = lens _cdDestinationSettings (\s a -> s {_cdDestinationSettings = a})

-- | Human readable information to indicate captions available for players (eg. English, or Spanish).
cdLanguageDescription :: Lens' CaptionDescription (Maybe Text)
cdLanguageDescription = lens _cdLanguageDescription (\s a -> s {_cdLanguageDescription = a})

-- | Specifies which input caption selector to use as a caption source when generating output captions. This field should match a captionSelector name.
cdCaptionSelectorName :: Lens' CaptionDescription Text
cdCaptionSelectorName = lens _cdCaptionSelectorName (\s a -> s {_cdCaptionSelectorName = a})

-- | Name of the caption description.  Used to associate a caption description with an output.  Names must be unique within an event.
cdName :: Lens' CaptionDescription Text
cdName = lens _cdName (\s a -> s {_cdName = a})

instance FromJSON CaptionDescription where
  parseJSON =
    withObject
      "CaptionDescription"
      ( \x ->
          CaptionDescription'
            <$> (x .:? "languageCode")
            <*> (x .:? "destinationSettings")
            <*> (x .:? "languageDescription")
            <*> (x .: "captionSelectorName")
            <*> (x .: "name")
      )

instance Hashable CaptionDescription

instance NFData CaptionDescription

instance ToJSON CaptionDescription where
  toJSON CaptionDescription' {..} =
    object
      ( catMaybes
          [ ("languageCode" .=) <$> _cdLanguageCode,
            ("destinationSettings" .=) <$> _cdDestinationSettings,
            ("languageDescription" .=) <$> _cdLanguageDescription,
            Just ("captionSelectorName" .= _cdCaptionSelectorName),
            Just ("name" .= _cdName)
          ]
      )
