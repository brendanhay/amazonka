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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.LanguageIdSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | If using automatic language identification in your request and you want
-- to apply a custom language model, a custom vocabulary, or a custom
-- vocabulary filter, include @LanguageIdSettings@ with the relevant
-- sub-parameters (@VocabularyName@, @LanguageModelName@, and
-- @VocabularyFilterName@). Note that multi-language identification
-- (@IdentifyMultipleLanguages@) doesn\'t support custom language models.
--
-- @LanguageIdSettings@ supports two to five language codes. Each language
-- code you include can have an associated custom language model, custom
-- vocabulary, and custom vocabulary filter. The language codes that you
-- specify must match the languages of the associated custom language
-- models, custom vocabularies, and custom vocabulary filters.
--
-- It\'s recommended that you include @LanguageOptions@ when using
-- @LanguageIdSettings@ to ensure that the correct language dialect is
-- identified. For example, if you specify a custom vocabulary that is in
-- @en-US@ but Amazon Transcribe determines that the language spoken in
-- your media is @en-AU@, your custom vocabulary /is not/ applied to your
-- transcription. If you include @LanguageOptions@ and include @en-US@ as
-- the only English language dialect, your custom vocabulary /is/ applied
-- to your transcription.
--
-- If you want to include a custom language model with your request but
-- __do not__ want to use automatic language identification, use instead
-- the parameter with the @LanguageModelName@ sub-parameter. If you want to
-- include a custom vocabulary or a custom vocabulary filter (or both) with
-- your request but __do not__ want to use automatic language
-- identification, use instead the parameter with the @VocabularyName@ or
-- @VocabularyFilterName@ (or both) sub-parameter.
--
-- /See:/ 'newLanguageIdSettings' smart constructor.
data LanguageIdSettings = LanguageIdSettings'
  { -- | The name of the custom language model you want to use when processing
    -- your transcription job. Note that custom language model names are case
    -- sensitive.
    --
    -- The language of the specified custom language model must match the
    -- language code that you specify in your transcription request. If the
    -- languages don\'t match, the custom language model isn\'t applied. There
    -- are no errors or warnings associated with a language mismatch.
    languageModelName :: Prelude.Maybe Prelude.Text,
    -- | The name of the custom vocabulary filter you want to use when processing
    -- your transcription job. Custom vocabulary filter names are case
    -- sensitive.
    --
    -- The language of the specified custom vocabulary filter must match the
    -- language code that you specify in your transcription request. If the
    -- languages don\'t match, the custom vocabulary filter isn\'t applied.
    -- There are no errors or warnings associated with a language mismatch.
    --
    -- Note that if you include @VocabularyFilterName@ in your request, you
    -- must also include @VocabularyFilterMethod@.
    vocabularyFilterName :: Prelude.Maybe Prelude.Text,
    -- | The name of the custom vocabulary you want to use when processing your
    -- transcription job. Custom vocabulary names are case sensitive.
    --
    -- The language of the specified custom vocabulary must match the language
    -- code that you specify in your transcription request. If the languages
    -- don\'t match, the custom vocabulary isn\'t applied. There are no errors
    -- or warnings associated with a language mismatch.
    vocabularyName :: Prelude.Maybe Prelude.Text
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
-- 'languageModelName', 'languageIdSettings_languageModelName' - The name of the custom language model you want to use when processing
-- your transcription job. Note that custom language model names are case
-- sensitive.
--
-- The language of the specified custom language model must match the
-- language code that you specify in your transcription request. If the
-- languages don\'t match, the custom language model isn\'t applied. There
-- are no errors or warnings associated with a language mismatch.
--
-- 'vocabularyFilterName', 'languageIdSettings_vocabularyFilterName' - The name of the custom vocabulary filter you want to use when processing
-- your transcription job. Custom vocabulary filter names are case
-- sensitive.
--
-- The language of the specified custom vocabulary filter must match the
-- language code that you specify in your transcription request. If the
-- languages don\'t match, the custom vocabulary filter isn\'t applied.
-- There are no errors or warnings associated with a language mismatch.
--
-- Note that if you include @VocabularyFilterName@ in your request, you
-- must also include @VocabularyFilterMethod@.
--
-- 'vocabularyName', 'languageIdSettings_vocabularyName' - The name of the custom vocabulary you want to use when processing your
-- transcription job. Custom vocabulary names are case sensitive.
--
-- The language of the specified custom vocabulary must match the language
-- code that you specify in your transcription request. If the languages
-- don\'t match, the custom vocabulary isn\'t applied. There are no errors
-- or warnings associated with a language mismatch.
newLanguageIdSettings ::
  LanguageIdSettings
newLanguageIdSettings =
  LanguageIdSettings'
    { languageModelName =
        Prelude.Nothing,
      vocabularyFilterName = Prelude.Nothing,
      vocabularyName = Prelude.Nothing
    }

-- | The name of the custom language model you want to use when processing
-- your transcription job. Note that custom language model names are case
-- sensitive.
--
-- The language of the specified custom language model must match the
-- language code that you specify in your transcription request. If the
-- languages don\'t match, the custom language model isn\'t applied. There
-- are no errors or warnings associated with a language mismatch.
languageIdSettings_languageModelName :: Lens.Lens' LanguageIdSettings (Prelude.Maybe Prelude.Text)
languageIdSettings_languageModelName = Lens.lens (\LanguageIdSettings' {languageModelName} -> languageModelName) (\s@LanguageIdSettings' {} a -> s {languageModelName = a} :: LanguageIdSettings)

-- | The name of the custom vocabulary filter you want to use when processing
-- your transcription job. Custom vocabulary filter names are case
-- sensitive.
--
-- The language of the specified custom vocabulary filter must match the
-- language code that you specify in your transcription request. If the
-- languages don\'t match, the custom vocabulary filter isn\'t applied.
-- There are no errors or warnings associated with a language mismatch.
--
-- Note that if you include @VocabularyFilterName@ in your request, you
-- must also include @VocabularyFilterMethod@.
languageIdSettings_vocabularyFilterName :: Lens.Lens' LanguageIdSettings (Prelude.Maybe Prelude.Text)
languageIdSettings_vocabularyFilterName = Lens.lens (\LanguageIdSettings' {vocabularyFilterName} -> vocabularyFilterName) (\s@LanguageIdSettings' {} a -> s {vocabularyFilterName = a} :: LanguageIdSettings)

-- | The name of the custom vocabulary you want to use when processing your
-- transcription job. Custom vocabulary names are case sensitive.
--
-- The language of the specified custom vocabulary must match the language
-- code that you specify in your transcription request. If the languages
-- don\'t match, the custom vocabulary isn\'t applied. There are no errors
-- or warnings associated with a language mismatch.
languageIdSettings_vocabularyName :: Lens.Lens' LanguageIdSettings (Prelude.Maybe Prelude.Text)
languageIdSettings_vocabularyName = Lens.lens (\LanguageIdSettings' {vocabularyName} -> vocabularyName) (\s@LanguageIdSettings' {} a -> s {vocabularyName = a} :: LanguageIdSettings)

instance Data.FromJSON LanguageIdSettings where
  parseJSON =
    Data.withObject
      "LanguageIdSettings"
      ( \x ->
          LanguageIdSettings'
            Prelude.<$> (x Data..:? "LanguageModelName")
            Prelude.<*> (x Data..:? "VocabularyFilterName")
            Prelude.<*> (x Data..:? "VocabularyName")
      )

instance Prelude.Hashable LanguageIdSettings where
  hashWithSalt _salt LanguageIdSettings' {..} =
    _salt
      `Prelude.hashWithSalt` languageModelName
      `Prelude.hashWithSalt` vocabularyFilterName
      `Prelude.hashWithSalt` vocabularyName

instance Prelude.NFData LanguageIdSettings where
  rnf LanguageIdSettings' {..} =
    Prelude.rnf languageModelName
      `Prelude.seq` Prelude.rnf vocabularyFilterName
      `Prelude.seq` Prelude.rnf vocabularyName

instance Data.ToJSON LanguageIdSettings where
  toJSON LanguageIdSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LanguageModelName" Data..=)
              Prelude.<$> languageModelName,
            ("VocabularyFilterName" Data..=)
              Prelude.<$> vocabularyFilterName,
            ("VocabularyName" Data..=)
              Prelude.<$> vocabularyName
          ]
      )
