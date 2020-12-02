{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DominantLanguage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DominantLanguage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns the code for the dominant language in the input text and the level of confidence that Amazon Comprehend has in the accuracy of the detection.
--
--
--
-- /See:/ 'dominantLanguage' smart constructor.
data DominantLanguage = DominantLanguage'
  { _dlLanguageCode ::
      !(Maybe Text),
    _dlScore :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DominantLanguage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlLanguageCode' - The RFC 5646 language code for the dominant language. For more information about RFC 5646, see <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on the /IETF Tools/ web site.
--
-- * 'dlScore' - The level of confidence that Amazon Comprehend has in the accuracy of the detection.
dominantLanguage ::
  DominantLanguage
dominantLanguage =
  DominantLanguage' {_dlLanguageCode = Nothing, _dlScore = Nothing}

-- | The RFC 5646 language code for the dominant language. For more information about RFC 5646, see <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on the /IETF Tools/ web site.
dlLanguageCode :: Lens' DominantLanguage (Maybe Text)
dlLanguageCode = lens _dlLanguageCode (\s a -> s {_dlLanguageCode = a})

-- | The level of confidence that Amazon Comprehend has in the accuracy of the detection.
dlScore :: Lens' DominantLanguage (Maybe Double)
dlScore = lens _dlScore (\s a -> s {_dlScore = a})

instance FromJSON DominantLanguage where
  parseJSON =
    withObject
      "DominantLanguage"
      ( \x ->
          DominantLanguage' <$> (x .:? "LanguageCode") <*> (x .:? "Score")
      )

instance Hashable DominantLanguage

instance NFData DominantLanguage
