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
-- Module      : Amazonka.Transcribe.Types.VocabularyInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.VocabularyInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.LanguageCode
import Amazonka.Transcribe.Types.VocabularyState

-- | Provides information about a custom vocabulary.
--
-- /See:/ 'newVocabularyInfo' smart constructor.
data VocabularyInfo = VocabularyInfo'
  { -- | The name of the vocabulary.
    vocabularyName :: Prelude.Maybe Prelude.Text,
    -- | The processing state of the vocabulary. If the state is @READY@ you can
    -- use the vocabulary in a @StartTranscriptionJob@ request.
    vocabularyState :: Prelude.Maybe VocabularyState,
    -- | The date and time that the vocabulary was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The language code of the vocabulary entries.
    languageCode :: Prelude.Maybe LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VocabularyInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vocabularyName', 'vocabularyInfo_vocabularyName' - The name of the vocabulary.
--
-- 'vocabularyState', 'vocabularyInfo_vocabularyState' - The processing state of the vocabulary. If the state is @READY@ you can
-- use the vocabulary in a @StartTranscriptionJob@ request.
--
-- 'lastModifiedTime', 'vocabularyInfo_lastModifiedTime' - The date and time that the vocabulary was last modified.
--
-- 'languageCode', 'vocabularyInfo_languageCode' - The language code of the vocabulary entries.
newVocabularyInfo ::
  VocabularyInfo
newVocabularyInfo =
  VocabularyInfo'
    { vocabularyName = Prelude.Nothing,
      vocabularyState = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      languageCode = Prelude.Nothing
    }

-- | The name of the vocabulary.
vocabularyInfo_vocabularyName :: Lens.Lens' VocabularyInfo (Prelude.Maybe Prelude.Text)
vocabularyInfo_vocabularyName = Lens.lens (\VocabularyInfo' {vocabularyName} -> vocabularyName) (\s@VocabularyInfo' {} a -> s {vocabularyName = a} :: VocabularyInfo)

-- | The processing state of the vocabulary. If the state is @READY@ you can
-- use the vocabulary in a @StartTranscriptionJob@ request.
vocabularyInfo_vocabularyState :: Lens.Lens' VocabularyInfo (Prelude.Maybe VocabularyState)
vocabularyInfo_vocabularyState = Lens.lens (\VocabularyInfo' {vocabularyState} -> vocabularyState) (\s@VocabularyInfo' {} a -> s {vocabularyState = a} :: VocabularyInfo)

-- | The date and time that the vocabulary was last modified.
vocabularyInfo_lastModifiedTime :: Lens.Lens' VocabularyInfo (Prelude.Maybe Prelude.UTCTime)
vocabularyInfo_lastModifiedTime = Lens.lens (\VocabularyInfo' {lastModifiedTime} -> lastModifiedTime) (\s@VocabularyInfo' {} a -> s {lastModifiedTime = a} :: VocabularyInfo) Prelude.. Lens.mapping Core._Time

-- | The language code of the vocabulary entries.
vocabularyInfo_languageCode :: Lens.Lens' VocabularyInfo (Prelude.Maybe LanguageCode)
vocabularyInfo_languageCode = Lens.lens (\VocabularyInfo' {languageCode} -> languageCode) (\s@VocabularyInfo' {} a -> s {languageCode = a} :: VocabularyInfo)

instance Core.FromJSON VocabularyInfo where
  parseJSON =
    Core.withObject
      "VocabularyInfo"
      ( \x ->
          VocabularyInfo'
            Prelude.<$> (x Core..:? "VocabularyName")
            Prelude.<*> (x Core..:? "VocabularyState")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "LanguageCode")
      )

instance Prelude.Hashable VocabularyInfo where
  hashWithSalt _salt VocabularyInfo' {..} =
    _salt `Prelude.hashWithSalt` vocabularyName
      `Prelude.hashWithSalt` vocabularyState
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData VocabularyInfo where
  rnf VocabularyInfo' {..} =
    Prelude.rnf vocabularyName
      `Prelude.seq` Prelude.rnf vocabularyState
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf languageCode
