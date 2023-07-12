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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- of the custom vocabulary, when it was last modified, its name, and the
-- processing state.
--
-- /See:/ 'newVocabularyInfo' smart constructor.
data VocabularyInfo = VocabularyInfo'
  { -- | The language code used to create your custom vocabulary. Each custom
    -- vocabulary must contain terms in only one language.
    --
    -- A custom vocabulary can only be used to transcribe files in the same
    -- language as the custom vocabulary. For example, if you create a custom
    -- vocabulary using US English (@en-US@), you can only apply this custom
    -- vocabulary to files that contain English audio.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The date and time the specified custom vocabulary was last modified.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
    -- May 4, 2022.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | A unique name, chosen by you, for your custom vocabulary. This name is
    -- case sensitive, cannot contain spaces, and must be unique within an
    -- Amazon Web Services account.
    vocabularyName :: Prelude.Maybe Prelude.Text,
    -- | The processing state of your custom vocabulary. If the state is @READY@,
    -- you can use the custom vocabulary in a @StartTranscriptionJob@ request.
    vocabularyState :: Prelude.Maybe VocabularyState
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
-- 'languageCode', 'vocabularyInfo_languageCode' - The language code used to create your custom vocabulary. Each custom
-- vocabulary must contain terms in only one language.
--
-- A custom vocabulary can only be used to transcribe files in the same
-- language as the custom vocabulary. For example, if you create a custom
-- vocabulary using US English (@en-US@), you can only apply this custom
-- vocabulary to files that contain English audio.
--
-- 'lastModifiedTime', 'vocabularyInfo_lastModifiedTime' - The date and time the specified custom vocabulary was last modified.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
--
-- 'vocabularyName', 'vocabularyInfo_vocabularyName' - A unique name, chosen by you, for your custom vocabulary. This name is
-- case sensitive, cannot contain spaces, and must be unique within an
-- Amazon Web Services account.
--
-- 'vocabularyState', 'vocabularyInfo_vocabularyState' - The processing state of your custom vocabulary. If the state is @READY@,
-- you can use the custom vocabulary in a @StartTranscriptionJob@ request.
newVocabularyInfo ::
  VocabularyInfo
newVocabularyInfo =
  VocabularyInfo'
    { languageCode = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      vocabularyName = Prelude.Nothing,
      vocabularyState = Prelude.Nothing
    }

-- | The language code used to create your custom vocabulary. Each custom
-- vocabulary must contain terms in only one language.
--
-- A custom vocabulary can only be used to transcribe files in the same
-- language as the custom vocabulary. For example, if you create a custom
-- vocabulary using US English (@en-US@), you can only apply this custom
-- vocabulary to files that contain English audio.
vocabularyInfo_languageCode :: Lens.Lens' VocabularyInfo (Prelude.Maybe LanguageCode)
vocabularyInfo_languageCode = Lens.lens (\VocabularyInfo' {languageCode} -> languageCode) (\s@VocabularyInfo' {} a -> s {languageCode = a} :: VocabularyInfo)

-- | The date and time the specified custom vocabulary was last modified.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
vocabularyInfo_lastModifiedTime :: Lens.Lens' VocabularyInfo (Prelude.Maybe Prelude.UTCTime)
vocabularyInfo_lastModifiedTime = Lens.lens (\VocabularyInfo' {lastModifiedTime} -> lastModifiedTime) (\s@VocabularyInfo' {} a -> s {lastModifiedTime = a} :: VocabularyInfo) Prelude.. Lens.mapping Data._Time

-- | A unique name, chosen by you, for your custom vocabulary. This name is
-- case sensitive, cannot contain spaces, and must be unique within an
-- Amazon Web Services account.
vocabularyInfo_vocabularyName :: Lens.Lens' VocabularyInfo (Prelude.Maybe Prelude.Text)
vocabularyInfo_vocabularyName = Lens.lens (\VocabularyInfo' {vocabularyName} -> vocabularyName) (\s@VocabularyInfo' {} a -> s {vocabularyName = a} :: VocabularyInfo)

-- | The processing state of your custom vocabulary. If the state is @READY@,
-- you can use the custom vocabulary in a @StartTranscriptionJob@ request.
vocabularyInfo_vocabularyState :: Lens.Lens' VocabularyInfo (Prelude.Maybe VocabularyState)
vocabularyInfo_vocabularyState = Lens.lens (\VocabularyInfo' {vocabularyState} -> vocabularyState) (\s@VocabularyInfo' {} a -> s {vocabularyState = a} :: VocabularyInfo)

instance Data.FromJSON VocabularyInfo where
  parseJSON =
    Data.withObject
      "VocabularyInfo"
      ( \x ->
          VocabularyInfo'
            Prelude.<$> (x Data..:? "LanguageCode")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "VocabularyName")
            Prelude.<*> (x Data..:? "VocabularyState")
      )

instance Prelude.Hashable VocabularyInfo where
  hashWithSalt _salt VocabularyInfo' {..} =
    _salt
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` vocabularyName
      `Prelude.hashWithSalt` vocabularyState

instance Prelude.NFData VocabularyInfo where
  rnf VocabularyInfo' {..} =
    Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf vocabularyName
      `Prelude.seq` Prelude.rnf vocabularyState
