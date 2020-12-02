{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsCaptionLanguageMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsCaptionLanguageMapping where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.LanguageCode
import Network.AWS.Prelude

-- | Caption Language Mapping
--
-- /See:/ 'hlsCaptionLanguageMapping' smart constructor.
data HlsCaptionLanguageMapping = HlsCaptionLanguageMapping'
  { _hclmCustomLanguageCode ::
      !(Maybe Text),
    _hclmLanguageCode ::
      !(Maybe LanguageCode),
    _hclmLanguageDescription ::
      !(Maybe Text),
    _hclmCaptionChannel :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsCaptionLanguageMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hclmCustomLanguageCode' - Specify the language for this captions channel, using the ISO 639-2 or ISO 639-3 three-letter language code
--
-- * 'hclmLanguageCode' - Specify the language, using the ISO 639-2 three-letter code listed at https://www.loc.gov/standards/iso639-2/php/code_list.php.
--
-- * 'hclmLanguageDescription' - Caption language description.
--
-- * 'hclmCaptionChannel' - Caption channel.
hlsCaptionLanguageMapping ::
  HlsCaptionLanguageMapping
hlsCaptionLanguageMapping =
  HlsCaptionLanguageMapping'
    { _hclmCustomLanguageCode = Nothing,
      _hclmLanguageCode = Nothing,
      _hclmLanguageDescription = Nothing,
      _hclmCaptionChannel = Nothing
    }

-- | Specify the language for this captions channel, using the ISO 639-2 or ISO 639-3 three-letter language code
hclmCustomLanguageCode :: Lens' HlsCaptionLanguageMapping (Maybe Text)
hclmCustomLanguageCode = lens _hclmCustomLanguageCode (\s a -> s {_hclmCustomLanguageCode = a})

-- | Specify the language, using the ISO 639-2 three-letter code listed at https://www.loc.gov/standards/iso639-2/php/code_list.php.
hclmLanguageCode :: Lens' HlsCaptionLanguageMapping (Maybe LanguageCode)
hclmLanguageCode = lens _hclmLanguageCode (\s a -> s {_hclmLanguageCode = a})

-- | Caption language description.
hclmLanguageDescription :: Lens' HlsCaptionLanguageMapping (Maybe Text)
hclmLanguageDescription = lens _hclmLanguageDescription (\s a -> s {_hclmLanguageDescription = a})

-- | Caption channel.
hclmCaptionChannel :: Lens' HlsCaptionLanguageMapping (Maybe Int)
hclmCaptionChannel = lens _hclmCaptionChannel (\s a -> s {_hclmCaptionChannel = a})

instance FromJSON HlsCaptionLanguageMapping where
  parseJSON =
    withObject
      "HlsCaptionLanguageMapping"
      ( \x ->
          HlsCaptionLanguageMapping'
            <$> (x .:? "customLanguageCode")
            <*> (x .:? "languageCode")
            <*> (x .:? "languageDescription")
            <*> (x .:? "captionChannel")
      )

instance Hashable HlsCaptionLanguageMapping

instance NFData HlsCaptionLanguageMapping

instance ToJSON HlsCaptionLanguageMapping where
  toJSON HlsCaptionLanguageMapping' {..} =
    object
      ( catMaybes
          [ ("customLanguageCode" .=) <$> _hclmCustomLanguageCode,
            ("languageCode" .=) <$> _hclmLanguageCode,
            ("languageDescription" .=) <$> _hclmLanguageDescription,
            ("captionChannel" .=) <$> _hclmCaptionChannel
          ]
      )
