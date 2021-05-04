{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.VocabularyInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.VocabularyInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Transcribe.Types.LanguageCode
import Network.AWS.Transcribe.Types.VocabularyState

-- | Provides information about a custom vocabulary.
--
-- /See:/ 'newVocabularyInfo' smart constructor.
data VocabularyInfo = VocabularyInfo'
  { -- | The language code of the vocabulary entries.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The date and time that the vocabulary was last modified.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The processing state of the vocabulary. If the state is @READY@ you can
    -- use the vocabulary in a @StartTranscriptionJob@ request.
    vocabularyState :: Prelude.Maybe VocabularyState,
    -- | The name of the vocabulary.
    vocabularyName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'VocabularyInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'vocabularyInfo_languageCode' - The language code of the vocabulary entries.
--
-- 'lastModifiedTime', 'vocabularyInfo_lastModifiedTime' - The date and time that the vocabulary was last modified.
--
-- 'vocabularyState', 'vocabularyInfo_vocabularyState' - The processing state of the vocabulary. If the state is @READY@ you can
-- use the vocabulary in a @StartTranscriptionJob@ request.
--
-- 'vocabularyName', 'vocabularyInfo_vocabularyName' - The name of the vocabulary.
newVocabularyInfo ::
  VocabularyInfo
newVocabularyInfo =
  VocabularyInfo'
    { languageCode = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      vocabularyState = Prelude.Nothing,
      vocabularyName = Prelude.Nothing
    }

-- | The language code of the vocabulary entries.
vocabularyInfo_languageCode :: Lens.Lens' VocabularyInfo (Prelude.Maybe LanguageCode)
vocabularyInfo_languageCode = Lens.lens (\VocabularyInfo' {languageCode} -> languageCode) (\s@VocabularyInfo' {} a -> s {languageCode = a} :: VocabularyInfo)

-- | The date and time that the vocabulary was last modified.
vocabularyInfo_lastModifiedTime :: Lens.Lens' VocabularyInfo (Prelude.Maybe Prelude.UTCTime)
vocabularyInfo_lastModifiedTime = Lens.lens (\VocabularyInfo' {lastModifiedTime} -> lastModifiedTime) (\s@VocabularyInfo' {} a -> s {lastModifiedTime = a} :: VocabularyInfo) Prelude.. Lens.mapping Prelude._Time

-- | The processing state of the vocabulary. If the state is @READY@ you can
-- use the vocabulary in a @StartTranscriptionJob@ request.
vocabularyInfo_vocabularyState :: Lens.Lens' VocabularyInfo (Prelude.Maybe VocabularyState)
vocabularyInfo_vocabularyState = Lens.lens (\VocabularyInfo' {vocabularyState} -> vocabularyState) (\s@VocabularyInfo' {} a -> s {vocabularyState = a} :: VocabularyInfo)

-- | The name of the vocabulary.
vocabularyInfo_vocabularyName :: Lens.Lens' VocabularyInfo (Prelude.Maybe Prelude.Text)
vocabularyInfo_vocabularyName = Lens.lens (\VocabularyInfo' {vocabularyName} -> vocabularyName) (\s@VocabularyInfo' {} a -> s {vocabularyName = a} :: VocabularyInfo)

instance Prelude.FromJSON VocabularyInfo where
  parseJSON =
    Prelude.withObject
      "VocabularyInfo"
      ( \x ->
          VocabularyInfo'
            Prelude.<$> (x Prelude..:? "LanguageCode")
            Prelude.<*> (x Prelude..:? "LastModifiedTime")
            Prelude.<*> (x Prelude..:? "VocabularyState")
            Prelude.<*> (x Prelude..:? "VocabularyName")
      )

instance Prelude.Hashable VocabularyInfo

instance Prelude.NFData VocabularyInfo
