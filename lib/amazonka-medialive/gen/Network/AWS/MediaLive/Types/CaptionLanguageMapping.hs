{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.CaptionLanguageMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CaptionLanguageMapping where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Maps a caption channel to an ISO 693-2 language code (http://www.loc.gov/standards/iso639-2), with an optional description.
--
-- /See:/ 'captionLanguageMapping' smart constructor.
data CaptionLanguageMapping = CaptionLanguageMapping'
  { _clmLanguageCode ::
      !Text,
    _clmLanguageDescription :: !Text,
    _clmCaptionChannel :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CaptionLanguageMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clmLanguageCode' - Three character ISO 639-2 language code (see http://www.loc.gov/standards/iso639-2)
--
-- * 'clmLanguageDescription' - Textual description of language
--
-- * 'clmCaptionChannel' - The closed caption channel being described by this CaptionLanguageMapping.  Each channel mapping must have a unique channel number (maximum of 4)
captionLanguageMapping ::
  -- | 'clmLanguageCode'
  Text ->
  -- | 'clmLanguageDescription'
  Text ->
  -- | 'clmCaptionChannel'
  Natural ->
  CaptionLanguageMapping
captionLanguageMapping
  pLanguageCode_
  pLanguageDescription_
  pCaptionChannel_ =
    CaptionLanguageMapping'
      { _clmLanguageCode = pLanguageCode_,
        _clmLanguageDescription = pLanguageDescription_,
        _clmCaptionChannel = _Nat # pCaptionChannel_
      }

-- | Three character ISO 639-2 language code (see http://www.loc.gov/standards/iso639-2)
clmLanguageCode :: Lens' CaptionLanguageMapping Text
clmLanguageCode = lens _clmLanguageCode (\s a -> s {_clmLanguageCode = a})

-- | Textual description of language
clmLanguageDescription :: Lens' CaptionLanguageMapping Text
clmLanguageDescription = lens _clmLanguageDescription (\s a -> s {_clmLanguageDescription = a})

-- | The closed caption channel being described by this CaptionLanguageMapping.  Each channel mapping must have a unique channel number (maximum of 4)
clmCaptionChannel :: Lens' CaptionLanguageMapping Natural
clmCaptionChannel = lens _clmCaptionChannel (\s a -> s {_clmCaptionChannel = a}) . _Nat

instance FromJSON CaptionLanguageMapping where
  parseJSON =
    withObject
      "CaptionLanguageMapping"
      ( \x ->
          CaptionLanguageMapping'
            <$> (x .: "languageCode")
            <*> (x .: "languageDescription")
            <*> (x .: "captionChannel")
      )

instance Hashable CaptionLanguageMapping

instance NFData CaptionLanguageMapping

instance ToJSON CaptionLanguageMapping where
  toJSON CaptionLanguageMapping' {..} =
    object
      ( catMaybes
          [ Just ("languageCode" .= _clmLanguageCode),
            Just ("languageDescription" .= _clmLanguageDescription),
            Just ("captionChannel" .= _clmCaptionChannel)
          ]
      )
