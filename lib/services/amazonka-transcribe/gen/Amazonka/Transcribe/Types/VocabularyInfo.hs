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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.VocabularyInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.LanguageCode
import Amazonka.Transcribe.Types.VocabularyState

-- | Provides information about a custom vocabulary, including the language
-- of the vocabulary, when it was last modified, its name, and the
-- processing state.
--
-- /See:/ 'newVocabularyInfo' smart constructor.
data VocabularyInfo = VocabularyInfo'
  { -- | A unique name, chosen by you, for your custom vocabulary. This name is
    -- case sensitive, cannot contain spaces, and must be unique within an
    -- Amazon Web Services account.
    vocabularyName :: Prelude.Maybe Prelude.Text,
    -- | The processing state of your custom vocabulary. If the state is @READY@,
    -- you can use the vocabulary in a @StartTranscriptionJob@ request.
    vocabularyState :: Prelude.Maybe VocabularyState,
    -- | The date and time the specified vocabulary was last modified.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
    -- May 4, 2022.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The language code used to create your custom vocabulary. Each vocabulary
    -- must contain terms in only one language.
    --
    -- A custom vocabulary can only be used to transcribe files in the same
    -- language as the vocabulary. For example, if you create a vocabulary
    -- using US English (@en-US@), you can only apply this vocabulary to files
    -- that contain English audio.
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
-- 'vocabularyName', 'vocabularyInfo_vocabularyName' - A unique name, chosen by you, for your custom vocabulary. This name is
-- case sensitive, cannot contain spaces, and must be unique within an
-- Amazon Web Services account.
--
-- 'vocabularyState', 'vocabularyInfo_vocabularyState' - The processing state of your custom vocabulary. If the state is @READY@,
-- you can use the vocabulary in a @StartTranscriptionJob@ request.
--
-- 'lastModifiedTime', 'vocabularyInfo_lastModifiedTime' - The date and time the specified vocabulary was last modified.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
--
-- 'languageCode', 'vocabularyInfo_languageCode' - The language code used to create your custom vocabulary. Each vocabulary
-- must contain terms in only one language.
--
-- A custom vocabulary can only be used to transcribe files in the same
-- language as the vocabulary. For example, if you create a vocabulary
-- using US English (@en-US@), you can only apply this vocabulary to files
-- that contain English audio.
newVocabularyInfo ::
  VocabularyInfo
newVocabularyInfo =
  VocabularyInfo'
    { vocabularyName = Prelude.Nothing,
      vocabularyState = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      languageCode = Prelude.Nothing
    }

-- | A unique name, chosen by you, for your custom vocabulary. This name is
-- case sensitive, cannot contain spaces, and must be unique within an
-- Amazon Web Services account.
vocabularyInfo_vocabularyName :: Lens.Lens' VocabularyInfo (Prelude.Maybe Prelude.Text)
vocabularyInfo_vocabularyName = Lens.lens (\VocabularyInfo' {vocabularyName} -> vocabularyName) (\s@VocabularyInfo' {} a -> s {vocabularyName = a} :: VocabularyInfo)

-- | The processing state of your custom vocabulary. If the state is @READY@,
-- you can use the vocabulary in a @StartTranscriptionJob@ request.
vocabularyInfo_vocabularyState :: Lens.Lens' VocabularyInfo (Prelude.Maybe VocabularyState)
vocabularyInfo_vocabularyState = Lens.lens (\VocabularyInfo' {vocabularyState} -> vocabularyState) (\s@VocabularyInfo' {} a -> s {vocabularyState = a} :: VocabularyInfo)

-- | The date and time the specified vocabulary was last modified.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
vocabularyInfo_lastModifiedTime :: Lens.Lens' VocabularyInfo (Prelude.Maybe Prelude.UTCTime)
vocabularyInfo_lastModifiedTime = Lens.lens (\VocabularyInfo' {lastModifiedTime} -> lastModifiedTime) (\s@VocabularyInfo' {} a -> s {lastModifiedTime = a} :: VocabularyInfo) Prelude.. Lens.mapping Data._Time

-- | The language code used to create your custom vocabulary. Each vocabulary
-- must contain terms in only one language.
--
-- A custom vocabulary can only be used to transcribe files in the same
-- language as the vocabulary. For example, if you create a vocabulary
-- using US English (@en-US@), you can only apply this vocabulary to files
-- that contain English audio.
vocabularyInfo_languageCode :: Lens.Lens' VocabularyInfo (Prelude.Maybe LanguageCode)
vocabularyInfo_languageCode = Lens.lens (\VocabularyInfo' {languageCode} -> languageCode) (\s@VocabularyInfo' {} a -> s {languageCode = a} :: VocabularyInfo)

instance Data.FromJSON VocabularyInfo where
  parseJSON =
    Data.withObject
      "VocabularyInfo"
      ( \x ->
          VocabularyInfo'
            Prelude.<$> (x Data..:? "VocabularyName")
            Prelude.<*> (x Data..:? "VocabularyState")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "LanguageCode")
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
