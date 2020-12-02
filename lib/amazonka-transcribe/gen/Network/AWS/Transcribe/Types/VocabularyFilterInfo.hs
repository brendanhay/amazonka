{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.VocabularyFilterInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.VocabularyFilterInfo where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Transcribe.Types.LanguageCode

-- | Provides information about a vocabulary filter.
--
--
--
-- /See:/ 'vocabularyFilterInfo' smart constructor.
data VocabularyFilterInfo = VocabularyFilterInfo'
  { _vfiLanguageCode ::
      !(Maybe LanguageCode),
    _vfiLastModifiedTime :: !(Maybe POSIX),
    _vfiVocabularyFilterName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VocabularyFilterInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vfiLanguageCode' - The language code of the words in the vocabulary filter.
--
-- * 'vfiLastModifiedTime' - The date and time that the vocabulary was last updated.
--
-- * 'vfiVocabularyFilterName' - The name of the vocabulary filter. The name must be unique in the account that holds the filter.
vocabularyFilterInfo ::
  VocabularyFilterInfo
vocabularyFilterInfo =
  VocabularyFilterInfo'
    { _vfiLanguageCode = Nothing,
      _vfiLastModifiedTime = Nothing,
      _vfiVocabularyFilterName = Nothing
    }

-- | The language code of the words in the vocabulary filter.
vfiLanguageCode :: Lens' VocabularyFilterInfo (Maybe LanguageCode)
vfiLanguageCode = lens _vfiLanguageCode (\s a -> s {_vfiLanguageCode = a})

-- | The date and time that the vocabulary was last updated.
vfiLastModifiedTime :: Lens' VocabularyFilterInfo (Maybe UTCTime)
vfiLastModifiedTime = lens _vfiLastModifiedTime (\s a -> s {_vfiLastModifiedTime = a}) . mapping _Time

-- | The name of the vocabulary filter. The name must be unique in the account that holds the filter.
vfiVocabularyFilterName :: Lens' VocabularyFilterInfo (Maybe Text)
vfiVocabularyFilterName = lens _vfiVocabularyFilterName (\s a -> s {_vfiVocabularyFilterName = a})

instance FromJSON VocabularyFilterInfo where
  parseJSON =
    withObject
      "VocabularyFilterInfo"
      ( \x ->
          VocabularyFilterInfo'
            <$> (x .:? "LanguageCode")
            <*> (x .:? "LastModifiedTime")
            <*> (x .:? "VocabularyFilterName")
      )

instance Hashable VocabularyFilterInfo

instance NFData VocabularyFilterInfo
