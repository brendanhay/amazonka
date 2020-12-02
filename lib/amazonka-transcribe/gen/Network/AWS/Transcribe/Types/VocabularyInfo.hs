{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.VocabularyInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.VocabularyInfo where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Transcribe.Types.LanguageCode
import Network.AWS.Transcribe.Types.VocabularyState

-- | Provides information about a custom vocabulary.
--
--
--
-- /See:/ 'vocabularyInfo' smart constructor.
data VocabularyInfo = VocabularyInfo'
  { _viLanguageCode ::
      !(Maybe LanguageCode),
    _viVocabularyName :: !(Maybe Text),
    _viLastModifiedTime :: !(Maybe POSIX),
    _viVocabularyState :: !(Maybe VocabularyState)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VocabularyInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'viLanguageCode' - The language code of the vocabulary entries.
--
-- * 'viVocabularyName' - The name of the vocabulary.
--
-- * 'viLastModifiedTime' - The date and time that the vocabulary was last modified.
--
-- * 'viVocabularyState' - The processing state of the vocabulary. If the state is @READY@ you can use the vocabulary in a @StartTranscriptionJob@ request.
vocabularyInfo ::
  VocabularyInfo
vocabularyInfo =
  VocabularyInfo'
    { _viLanguageCode = Nothing,
      _viVocabularyName = Nothing,
      _viLastModifiedTime = Nothing,
      _viVocabularyState = Nothing
    }

-- | The language code of the vocabulary entries.
viLanguageCode :: Lens' VocabularyInfo (Maybe LanguageCode)
viLanguageCode = lens _viLanguageCode (\s a -> s {_viLanguageCode = a})

-- | The name of the vocabulary.
viVocabularyName :: Lens' VocabularyInfo (Maybe Text)
viVocabularyName = lens _viVocabularyName (\s a -> s {_viVocabularyName = a})

-- | The date and time that the vocabulary was last modified.
viLastModifiedTime :: Lens' VocabularyInfo (Maybe UTCTime)
viLastModifiedTime = lens _viLastModifiedTime (\s a -> s {_viLastModifiedTime = a}) . mapping _Time

-- | The processing state of the vocabulary. If the state is @READY@ you can use the vocabulary in a @StartTranscriptionJob@ request.
viVocabularyState :: Lens' VocabularyInfo (Maybe VocabularyState)
viVocabularyState = lens _viVocabularyState (\s a -> s {_viVocabularyState = a})

instance FromJSON VocabularyInfo where
  parseJSON =
    withObject
      "VocabularyInfo"
      ( \x ->
          VocabularyInfo'
            <$> (x .:? "LanguageCode")
            <*> (x .:? "VocabularyName")
            <*> (x .:? "LastModifiedTime")
            <*> (x .:? "VocabularyState")
      )

instance Hashable VocabularyInfo

instance NFData VocabularyInfo
