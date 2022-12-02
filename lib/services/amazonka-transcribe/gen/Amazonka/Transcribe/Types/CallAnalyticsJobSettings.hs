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
-- Module      : Amazonka.Transcribe.Types.CallAnalyticsJobSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.CallAnalyticsJobSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.ContentRedaction
import Amazonka.Transcribe.Types.LanguageCode
import Amazonka.Transcribe.Types.LanguageIdSettings
import Amazonka.Transcribe.Types.VocabularyFilterMethod

-- | Provides additional optional settings for your request, including
-- content redaction, automatic language identification; allows you to
-- apply custom language models, vocabulary filters, and custom
-- vocabularies.
--
-- /See:/ 'newCallAnalyticsJobSettings' smart constructor.
data CallAnalyticsJobSettings = CallAnalyticsJobSettings'
  { -- | Specify how you want your vocabulary filter applied to your transcript.
    --
    -- To replace words with @***@, choose @mask@.
    --
    -- To delete words, choose @remove@.
    --
    -- To flag words without changing them, choose @tag@.
    vocabularyFilterMethod :: Prelude.Maybe VocabularyFilterMethod,
    -- | The name of the custom vocabulary you want to include in your Call
    -- Analytics transcription request. Vocabulary names are case sensitive.
    vocabularyName :: Prelude.Maybe Prelude.Text,
    -- | The name of the custom language model you want to use when processing
    -- your Call Analytics job. Note that language model names are case
    -- sensitive.
    --
    -- The language of the specified language model must match the language
    -- code you specify in your transcription request. If the languages don\'t
    -- match, the language model isn\'t applied. There are no errors or
    -- warnings associated with a language mismatch.
    languageModelName :: Prelude.Maybe Prelude.Text,
    contentRedaction :: Prelude.Maybe ContentRedaction,
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
    languageIdSettings :: Prelude.Maybe (Prelude.HashMap LanguageCode LanguageIdSettings),
    -- | The name of the custom vocabulary filter you want to include in your
    -- Call Analytics transcription request. Vocabulary filter names are case
    -- sensitive.
    --
    -- Note that if you include @VocabularyFilterName@ in your request, you
    -- must also include @VocabularyFilterMethod@.
    vocabularyFilterName :: Prelude.Maybe Prelude.Text,
    -- | You can specify two or more language codes that represent the languages
    -- you think may be present in your media; including more than five is not
    -- recommended. If you\'re unsure what languages are present, do not
    -- include this parameter.
    --
    -- Including language options can improve the accuracy of language
    -- identification.
    --
    -- For a list of languages supported with Call Analytics, refer to the
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
    -- table.
    languageOptions :: Prelude.Maybe (Prelude.NonEmpty LanguageCode)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CallAnalyticsJobSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vocabularyFilterMethod', 'callAnalyticsJobSettings_vocabularyFilterMethod' - Specify how you want your vocabulary filter applied to your transcript.
--
-- To replace words with @***@, choose @mask@.
--
-- To delete words, choose @remove@.
--
-- To flag words without changing them, choose @tag@.
--
-- 'vocabularyName', 'callAnalyticsJobSettings_vocabularyName' - The name of the custom vocabulary you want to include in your Call
-- Analytics transcription request. Vocabulary names are case sensitive.
--
-- 'languageModelName', 'callAnalyticsJobSettings_languageModelName' - The name of the custom language model you want to use when processing
-- your Call Analytics job. Note that language model names are case
-- sensitive.
--
-- The language of the specified language model must match the language
-- code you specify in your transcription request. If the languages don\'t
-- match, the language model isn\'t applied. There are no errors or
-- warnings associated with a language mismatch.
--
-- 'contentRedaction', 'callAnalyticsJobSettings_contentRedaction' - Undocumented member.
--
-- 'languageIdSettings', 'callAnalyticsJobSettings_languageIdSettings' - If using automatic language identification (@IdentifyLanguage@) in your
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
-- 'vocabularyFilterName', 'callAnalyticsJobSettings_vocabularyFilterName' - The name of the custom vocabulary filter you want to include in your
-- Call Analytics transcription request. Vocabulary filter names are case
-- sensitive.
--
-- Note that if you include @VocabularyFilterName@ in your request, you
-- must also include @VocabularyFilterMethod@.
--
-- 'languageOptions', 'callAnalyticsJobSettings_languageOptions' - You can specify two or more language codes that represent the languages
-- you think may be present in your media; including more than five is not
-- recommended. If you\'re unsure what languages are present, do not
-- include this parameter.
--
-- Including language options can improve the accuracy of language
-- identification.
--
-- For a list of languages supported with Call Analytics, refer to the
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
-- table.
newCallAnalyticsJobSettings ::
  CallAnalyticsJobSettings
newCallAnalyticsJobSettings =
  CallAnalyticsJobSettings'
    { vocabularyFilterMethod =
        Prelude.Nothing,
      vocabularyName = Prelude.Nothing,
      languageModelName = Prelude.Nothing,
      contentRedaction = Prelude.Nothing,
      languageIdSettings = Prelude.Nothing,
      vocabularyFilterName = Prelude.Nothing,
      languageOptions = Prelude.Nothing
    }

-- | Specify how you want your vocabulary filter applied to your transcript.
--
-- To replace words with @***@, choose @mask@.
--
-- To delete words, choose @remove@.
--
-- To flag words without changing them, choose @tag@.
callAnalyticsJobSettings_vocabularyFilterMethod :: Lens.Lens' CallAnalyticsJobSettings (Prelude.Maybe VocabularyFilterMethod)
callAnalyticsJobSettings_vocabularyFilterMethod = Lens.lens (\CallAnalyticsJobSettings' {vocabularyFilterMethod} -> vocabularyFilterMethod) (\s@CallAnalyticsJobSettings' {} a -> s {vocabularyFilterMethod = a} :: CallAnalyticsJobSettings)

-- | The name of the custom vocabulary you want to include in your Call
-- Analytics transcription request. Vocabulary names are case sensitive.
callAnalyticsJobSettings_vocabularyName :: Lens.Lens' CallAnalyticsJobSettings (Prelude.Maybe Prelude.Text)
callAnalyticsJobSettings_vocabularyName = Lens.lens (\CallAnalyticsJobSettings' {vocabularyName} -> vocabularyName) (\s@CallAnalyticsJobSettings' {} a -> s {vocabularyName = a} :: CallAnalyticsJobSettings)

-- | The name of the custom language model you want to use when processing
-- your Call Analytics job. Note that language model names are case
-- sensitive.
--
-- The language of the specified language model must match the language
-- code you specify in your transcription request. If the languages don\'t
-- match, the language model isn\'t applied. There are no errors or
-- warnings associated with a language mismatch.
callAnalyticsJobSettings_languageModelName :: Lens.Lens' CallAnalyticsJobSettings (Prelude.Maybe Prelude.Text)
callAnalyticsJobSettings_languageModelName = Lens.lens (\CallAnalyticsJobSettings' {languageModelName} -> languageModelName) (\s@CallAnalyticsJobSettings' {} a -> s {languageModelName = a} :: CallAnalyticsJobSettings)

-- | Undocumented member.
callAnalyticsJobSettings_contentRedaction :: Lens.Lens' CallAnalyticsJobSettings (Prelude.Maybe ContentRedaction)
callAnalyticsJobSettings_contentRedaction = Lens.lens (\CallAnalyticsJobSettings' {contentRedaction} -> contentRedaction) (\s@CallAnalyticsJobSettings' {} a -> s {contentRedaction = a} :: CallAnalyticsJobSettings)

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
callAnalyticsJobSettings_languageIdSettings :: Lens.Lens' CallAnalyticsJobSettings (Prelude.Maybe (Prelude.HashMap LanguageCode LanguageIdSettings))
callAnalyticsJobSettings_languageIdSettings = Lens.lens (\CallAnalyticsJobSettings' {languageIdSettings} -> languageIdSettings) (\s@CallAnalyticsJobSettings' {} a -> s {languageIdSettings = a} :: CallAnalyticsJobSettings) Prelude.. Lens.mapping Lens.coerced

-- | The name of the custom vocabulary filter you want to include in your
-- Call Analytics transcription request. Vocabulary filter names are case
-- sensitive.
--
-- Note that if you include @VocabularyFilterName@ in your request, you
-- must also include @VocabularyFilterMethod@.
callAnalyticsJobSettings_vocabularyFilterName :: Lens.Lens' CallAnalyticsJobSettings (Prelude.Maybe Prelude.Text)
callAnalyticsJobSettings_vocabularyFilterName = Lens.lens (\CallAnalyticsJobSettings' {vocabularyFilterName} -> vocabularyFilterName) (\s@CallAnalyticsJobSettings' {} a -> s {vocabularyFilterName = a} :: CallAnalyticsJobSettings)

-- | You can specify two or more language codes that represent the languages
-- you think may be present in your media; including more than five is not
-- recommended. If you\'re unsure what languages are present, do not
-- include this parameter.
--
-- Including language options can improve the accuracy of language
-- identification.
--
-- For a list of languages supported with Call Analytics, refer to the
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
-- table.
callAnalyticsJobSettings_languageOptions :: Lens.Lens' CallAnalyticsJobSettings (Prelude.Maybe (Prelude.NonEmpty LanguageCode))
callAnalyticsJobSettings_languageOptions = Lens.lens (\CallAnalyticsJobSettings' {languageOptions} -> languageOptions) (\s@CallAnalyticsJobSettings' {} a -> s {languageOptions = a} :: CallAnalyticsJobSettings) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CallAnalyticsJobSettings where
  parseJSON =
    Data.withObject
      "CallAnalyticsJobSettings"
      ( \x ->
          CallAnalyticsJobSettings'
            Prelude.<$> (x Data..:? "VocabularyFilterMethod")
            Prelude.<*> (x Data..:? "VocabularyName")
            Prelude.<*> (x Data..:? "LanguageModelName")
            Prelude.<*> (x Data..:? "ContentRedaction")
            Prelude.<*> ( x Data..:? "LanguageIdSettings"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "VocabularyFilterName")
            Prelude.<*> (x Data..:? "LanguageOptions")
      )

instance Prelude.Hashable CallAnalyticsJobSettings where
  hashWithSalt _salt CallAnalyticsJobSettings' {..} =
    _salt `Prelude.hashWithSalt` vocabularyFilterMethod
      `Prelude.hashWithSalt` vocabularyName
      `Prelude.hashWithSalt` languageModelName
      `Prelude.hashWithSalt` contentRedaction
      `Prelude.hashWithSalt` languageIdSettings
      `Prelude.hashWithSalt` vocabularyFilterName
      `Prelude.hashWithSalt` languageOptions

instance Prelude.NFData CallAnalyticsJobSettings where
  rnf CallAnalyticsJobSettings' {..} =
    Prelude.rnf vocabularyFilterMethod
      `Prelude.seq` Prelude.rnf vocabularyName
      `Prelude.seq` Prelude.rnf languageModelName
      `Prelude.seq` Prelude.rnf contentRedaction
      `Prelude.seq` Prelude.rnf languageIdSettings
      `Prelude.seq` Prelude.rnf vocabularyFilterName
      `Prelude.seq` Prelude.rnf languageOptions

instance Data.ToJSON CallAnalyticsJobSettings where
  toJSON CallAnalyticsJobSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("VocabularyFilterMethod" Data..=)
              Prelude.<$> vocabularyFilterMethod,
            ("VocabularyName" Data..=)
              Prelude.<$> vocabularyName,
            ("LanguageModelName" Data..=)
              Prelude.<$> languageModelName,
            ("ContentRedaction" Data..=)
              Prelude.<$> contentRedaction,
            ("LanguageIdSettings" Data..=)
              Prelude.<$> languageIdSettings,
            ("VocabularyFilterName" Data..=)
              Prelude.<$> vocabularyFilterName,
            ("LanguageOptions" Data..=)
              Prelude.<$> languageOptions
          ]
      )
