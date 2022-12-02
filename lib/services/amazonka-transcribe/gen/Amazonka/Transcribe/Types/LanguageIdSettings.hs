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
-- Module      : Amazonka.Transcribe.Types.LanguageIdSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.LanguageIdSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | If using automatic language identification (@IdentifyLanguage@) in your
-- request and you want to apply a custom language model, a custom
-- vocabulary, or a custom vocabulary filter, include @LanguageIdSettings@
-- with the relevant sub-parameters (@VocabularyName@, @LanguageModelName@,
-- and @VocabularyFilterName@).
--
-- You can specify two or more language codes that represent the languages
-- you think may be present in your media; including more than five is not
-- recommended. Each language code you include can have an associated
-- custom language model, custom vocabulary, and custom vocabulary filter.
-- The languages you specify must match the languages of the specified
-- custom language models, custom vocabularies, and custom vocabulary
-- filters.
--
-- To include language options using @IdentifyLanguage@ __without__
-- including a custom language model, a custom vocabulary, or a custom
-- vocabulary filter, use @LanguageOptions@ instead of
-- @LanguageIdSettings@. Including language options can improve the
-- accuracy of automatic language identification.
--
-- If you want to include a custom language model with your request but
-- __do not__ want to use automatic language identification, use instead
-- the @@ parameter with the @LanguageModelName@ sub-parameter.
--
-- If you want to include a custom vocabulary or a custom vocabulary filter
-- (or both) with your request but __do not__ want to use automatic
-- language identification, use instead the @@ parameter with the
-- @VocabularyName@ or @VocabularyFilterName@ (or both) sub-parameter.
--
-- /See:/ 'newLanguageIdSettings' smart constructor.
data LanguageIdSettings = LanguageIdSettings'
  { -- | The name of the custom vocabulary you want to use when processing your
    -- transcription job. Vocabulary names are case sensitive.
    --
    -- The language of the specified vocabulary must match the language code
    -- you specify in your transcription request. If the languages don\'t
    -- match, the vocabulary isn\'t applied. There are no errors or warnings
    -- associated with a language mismatch.
    vocabularyName :: Prelude.Maybe Prelude.Text,
    -- | The name of the custom language model you want to use when processing
    -- your transcription job. Note that language model names are case
    -- sensitive.
    --
    -- The language of the specified language model must match the language
    -- code you specify in your transcription request. If the languages don\'t
    -- match, the language model isn\'t applied. There are no errors or
    -- warnings associated with a language mismatch.
    languageModelName :: Prelude.Maybe Prelude.Text,
    -- | The name of the custom vocabulary filter you want to use when processing
    -- your transcription job. Vocabulary filter names are case sensitive.
    --
    -- The language of the specified vocabulary filter must match the language
    -- code you specify in your transcription request. If the languages don\'t
    -- match, the vocabulary filter isn\'t applied. There are no errors or
    -- warnings associated with a language mismatch.
    --
    -- Note that if you include @VocabularyFilterName@ in your request, you
    -- must also include @VocabularyFilterMethod@.
    vocabularyFilterName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LanguageIdSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vocabularyName', 'languageIdSettings_vocabularyName' - The name of the custom vocabulary you want to use when processing your
-- transcription job. Vocabulary names are case sensitive.
--
-- The language of the specified vocabulary must match the language code
-- you specify in your transcription request. If the languages don\'t
-- match, the vocabulary isn\'t applied. There are no errors or warnings
-- associated with a language mismatch.
--
-- 'languageModelName', 'languageIdSettings_languageModelName' - The name of the custom language model you want to use when processing
-- your transcription job. Note that language model names are case
-- sensitive.
--
-- The language of the specified language model must match the language
-- code you specify in your transcription request. If the languages don\'t
-- match, the language model isn\'t applied. There are no errors or
-- warnings associated with a language mismatch.
--
-- 'vocabularyFilterName', 'languageIdSettings_vocabularyFilterName' - The name of the custom vocabulary filter you want to use when processing
-- your transcription job. Vocabulary filter names are case sensitive.
--
-- The language of the specified vocabulary filter must match the language
-- code you specify in your transcription request. If the languages don\'t
-- match, the vocabulary filter isn\'t applied. There are no errors or
-- warnings associated with a language mismatch.
--
-- Note that if you include @VocabularyFilterName@ in your request, you
-- must also include @VocabularyFilterMethod@.
newLanguageIdSettings ::
  LanguageIdSettings
newLanguageIdSettings =
  LanguageIdSettings'
    { vocabularyName =
        Prelude.Nothing,
      languageModelName = Prelude.Nothing,
      vocabularyFilterName = Prelude.Nothing
    }

-- | The name of the custom vocabulary you want to use when processing your
-- transcription job. Vocabulary names are case sensitive.
--
-- The language of the specified vocabulary must match the language code
-- you specify in your transcription request. If the languages don\'t
-- match, the vocabulary isn\'t applied. There are no errors or warnings
-- associated with a language mismatch.
languageIdSettings_vocabularyName :: Lens.Lens' LanguageIdSettings (Prelude.Maybe Prelude.Text)
languageIdSettings_vocabularyName = Lens.lens (\LanguageIdSettings' {vocabularyName} -> vocabularyName) (\s@LanguageIdSettings' {} a -> s {vocabularyName = a} :: LanguageIdSettings)

-- | The name of the custom language model you want to use when processing
-- your transcription job. Note that language model names are case
-- sensitive.
--
-- The language of the specified language model must match the language
-- code you specify in your transcription request. If the languages don\'t
-- match, the language model isn\'t applied. There are no errors or
-- warnings associated with a language mismatch.
languageIdSettings_languageModelName :: Lens.Lens' LanguageIdSettings (Prelude.Maybe Prelude.Text)
languageIdSettings_languageModelName = Lens.lens (\LanguageIdSettings' {languageModelName} -> languageModelName) (\s@LanguageIdSettings' {} a -> s {languageModelName = a} :: LanguageIdSettings)

-- | The name of the custom vocabulary filter you want to use when processing
-- your transcription job. Vocabulary filter names are case sensitive.
--
-- The language of the specified vocabulary filter must match the language
-- code you specify in your transcription request. If the languages don\'t
-- match, the vocabulary filter isn\'t applied. There are no errors or
-- warnings associated with a language mismatch.
--
-- Note that if you include @VocabularyFilterName@ in your request, you
-- must also include @VocabularyFilterMethod@.
languageIdSettings_vocabularyFilterName :: Lens.Lens' LanguageIdSettings (Prelude.Maybe Prelude.Text)
languageIdSettings_vocabularyFilterName = Lens.lens (\LanguageIdSettings' {vocabularyFilterName} -> vocabularyFilterName) (\s@LanguageIdSettings' {} a -> s {vocabularyFilterName = a} :: LanguageIdSettings)

instance Data.FromJSON LanguageIdSettings where
  parseJSON =
    Data.withObject
      "LanguageIdSettings"
      ( \x ->
          LanguageIdSettings'
            Prelude.<$> (x Data..:? "VocabularyName")
            Prelude.<*> (x Data..:? "LanguageModelName")
            Prelude.<*> (x Data..:? "VocabularyFilterName")
      )

instance Prelude.Hashable LanguageIdSettings where
  hashWithSalt _salt LanguageIdSettings' {..} =
    _salt `Prelude.hashWithSalt` vocabularyName
      `Prelude.hashWithSalt` languageModelName
      `Prelude.hashWithSalt` vocabularyFilterName

instance Prelude.NFData LanguageIdSettings where
  rnf LanguageIdSettings' {..} =
    Prelude.rnf vocabularyName
      `Prelude.seq` Prelude.rnf languageModelName
      `Prelude.seq` Prelude.rnf vocabularyFilterName

instance Data.ToJSON LanguageIdSettings where
  toJSON LanguageIdSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("VocabularyName" Data..=)
              Prelude.<$> vocabularyName,
            ("LanguageModelName" Data..=)
              Prelude.<$> languageModelName,
            ("VocabularyFilterName" Data..=)
              Prelude.<$> vocabularyFilterName
          ]
      )
