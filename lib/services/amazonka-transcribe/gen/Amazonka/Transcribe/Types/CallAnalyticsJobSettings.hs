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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- apply custom language models, custom vocabulary filters, and custom
-- vocabularies.
--
-- /See:/ 'newCallAnalyticsJobSettings' smart constructor.
data CallAnalyticsJobSettings = CallAnalyticsJobSettings'
  { contentRedaction :: Prelude.Maybe ContentRedaction,
    -- | If using automatic language identification in your request and you want
    -- to apply a custom language model, a custom vocabulary, or a custom
    -- vocabulary filter, include @LanguageIdSettings@ with the relevant
    -- sub-parameters (@VocabularyName@, @LanguageModelName@, and
    -- @VocabularyFilterName@).
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
    -- If you want to include a custom language model, custom vocabulary, or
    -- custom vocabulary filter with your request but __do not__ want to use
    -- automatic language identification, use instead the @@ parameter with the
    -- @LanguageModelName@, @VocabularyName@, or @VocabularyFilterName@
    -- sub-parameters.
    --
    -- For a list of languages supported with Call Analytics, refer to
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages and language-specific features>.
    languageIdSettings :: Prelude.Maybe (Prelude.HashMap LanguageCode LanguageIdSettings),
    -- | The name of the custom language model you want to use when processing
    -- your Call Analytics job. Note that custom language model names are case
    -- sensitive.
    --
    -- The language of the specified custom language model must match the
    -- language code that you specify in your transcription request. If the
    -- languages don\'t match, the custom language model isn\'t applied. There
    -- are no errors or warnings associated with a language mismatch.
    languageModelName :: Prelude.Maybe Prelude.Text,
    -- | You can specify two or more language codes that represent the languages
    -- you think may be present in your media. Including more than five is not
    -- recommended. If you\'re unsure what languages are present, do not
    -- include this parameter.
    --
    -- Including language options can improve the accuracy of language
    -- identification.
    --
    -- For a list of languages supported with Call Analytics, refer to the
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
    -- table.
    --
    -- To transcribe speech in Modern Standard Arabic (@ar-SA@), your media
    -- file must be encoded at a sample rate of 16,000 Hz or higher.
    languageOptions :: Prelude.Maybe (Prelude.NonEmpty LanguageCode),
    -- | Specify how you want your custom vocabulary filter applied to your
    -- transcript.
    --
    -- To replace words with @***@, choose @mask@.
    --
    -- To delete words, choose @remove@.
    --
    -- To flag words without changing them, choose @tag@.
    vocabularyFilterMethod :: Prelude.Maybe VocabularyFilterMethod,
    -- | The name of the custom vocabulary filter you want to include in your
    -- Call Analytics transcription request. Custom vocabulary filter names are
    -- case sensitive.
    --
    -- Note that if you include @VocabularyFilterName@ in your request, you
    -- must also include @VocabularyFilterMethod@.
    vocabularyFilterName :: Prelude.Maybe Prelude.Text,
    -- | The name of the custom vocabulary you want to include in your Call
    -- Analytics transcription request. Custom vocabulary names are case
    -- sensitive.
    vocabularyName :: Prelude.Maybe Prelude.Text
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
-- 'contentRedaction', 'callAnalyticsJobSettings_contentRedaction' - Undocumented member.
--
-- 'languageIdSettings', 'callAnalyticsJobSettings_languageIdSettings' - If using automatic language identification in your request and you want
-- to apply a custom language model, a custom vocabulary, or a custom
-- vocabulary filter, include @LanguageIdSettings@ with the relevant
-- sub-parameters (@VocabularyName@, @LanguageModelName@, and
-- @VocabularyFilterName@).
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
-- If you want to include a custom language model, custom vocabulary, or
-- custom vocabulary filter with your request but __do not__ want to use
-- automatic language identification, use instead the @@ parameter with the
-- @LanguageModelName@, @VocabularyName@, or @VocabularyFilterName@
-- sub-parameters.
--
-- For a list of languages supported with Call Analytics, refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages and language-specific features>.
--
-- 'languageModelName', 'callAnalyticsJobSettings_languageModelName' - The name of the custom language model you want to use when processing
-- your Call Analytics job. Note that custom language model names are case
-- sensitive.
--
-- The language of the specified custom language model must match the
-- language code that you specify in your transcription request. If the
-- languages don\'t match, the custom language model isn\'t applied. There
-- are no errors or warnings associated with a language mismatch.
--
-- 'languageOptions', 'callAnalyticsJobSettings_languageOptions' - You can specify two or more language codes that represent the languages
-- you think may be present in your media. Including more than five is not
-- recommended. If you\'re unsure what languages are present, do not
-- include this parameter.
--
-- Including language options can improve the accuracy of language
-- identification.
--
-- For a list of languages supported with Call Analytics, refer to the
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
-- table.
--
-- To transcribe speech in Modern Standard Arabic (@ar-SA@), your media
-- file must be encoded at a sample rate of 16,000 Hz or higher.
--
-- 'vocabularyFilterMethod', 'callAnalyticsJobSettings_vocabularyFilterMethod' - Specify how you want your custom vocabulary filter applied to your
-- transcript.
--
-- To replace words with @***@, choose @mask@.
--
-- To delete words, choose @remove@.
--
-- To flag words without changing them, choose @tag@.
--
-- 'vocabularyFilterName', 'callAnalyticsJobSettings_vocabularyFilterName' - The name of the custom vocabulary filter you want to include in your
-- Call Analytics transcription request. Custom vocabulary filter names are
-- case sensitive.
--
-- Note that if you include @VocabularyFilterName@ in your request, you
-- must also include @VocabularyFilterMethod@.
--
-- 'vocabularyName', 'callAnalyticsJobSettings_vocabularyName' - The name of the custom vocabulary you want to include in your Call
-- Analytics transcription request. Custom vocabulary names are case
-- sensitive.
newCallAnalyticsJobSettings ::
  CallAnalyticsJobSettings
newCallAnalyticsJobSettings =
  CallAnalyticsJobSettings'
    { contentRedaction =
        Prelude.Nothing,
      languageIdSettings = Prelude.Nothing,
      languageModelName = Prelude.Nothing,
      languageOptions = Prelude.Nothing,
      vocabularyFilterMethod = Prelude.Nothing,
      vocabularyFilterName = Prelude.Nothing,
      vocabularyName = Prelude.Nothing
    }

-- | Undocumented member.
callAnalyticsJobSettings_contentRedaction :: Lens.Lens' CallAnalyticsJobSettings (Prelude.Maybe ContentRedaction)
callAnalyticsJobSettings_contentRedaction = Lens.lens (\CallAnalyticsJobSettings' {contentRedaction} -> contentRedaction) (\s@CallAnalyticsJobSettings' {} a -> s {contentRedaction = a} :: CallAnalyticsJobSettings)

-- | If using automatic language identification in your request and you want
-- to apply a custom language model, a custom vocabulary, or a custom
-- vocabulary filter, include @LanguageIdSettings@ with the relevant
-- sub-parameters (@VocabularyName@, @LanguageModelName@, and
-- @VocabularyFilterName@).
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
-- If you want to include a custom language model, custom vocabulary, or
-- custom vocabulary filter with your request but __do not__ want to use
-- automatic language identification, use instead the @@ parameter with the
-- @LanguageModelName@, @VocabularyName@, or @VocabularyFilterName@
-- sub-parameters.
--
-- For a list of languages supported with Call Analytics, refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages and language-specific features>.
callAnalyticsJobSettings_languageIdSettings :: Lens.Lens' CallAnalyticsJobSettings (Prelude.Maybe (Prelude.HashMap LanguageCode LanguageIdSettings))
callAnalyticsJobSettings_languageIdSettings = Lens.lens (\CallAnalyticsJobSettings' {languageIdSettings} -> languageIdSettings) (\s@CallAnalyticsJobSettings' {} a -> s {languageIdSettings = a} :: CallAnalyticsJobSettings) Prelude.. Lens.mapping Lens.coerced

-- | The name of the custom language model you want to use when processing
-- your Call Analytics job. Note that custom language model names are case
-- sensitive.
--
-- The language of the specified custom language model must match the
-- language code that you specify in your transcription request. If the
-- languages don\'t match, the custom language model isn\'t applied. There
-- are no errors or warnings associated with a language mismatch.
callAnalyticsJobSettings_languageModelName :: Lens.Lens' CallAnalyticsJobSettings (Prelude.Maybe Prelude.Text)
callAnalyticsJobSettings_languageModelName = Lens.lens (\CallAnalyticsJobSettings' {languageModelName} -> languageModelName) (\s@CallAnalyticsJobSettings' {} a -> s {languageModelName = a} :: CallAnalyticsJobSettings)

-- | You can specify two or more language codes that represent the languages
-- you think may be present in your media. Including more than five is not
-- recommended. If you\'re unsure what languages are present, do not
-- include this parameter.
--
-- Including language options can improve the accuracy of language
-- identification.
--
-- For a list of languages supported with Call Analytics, refer to the
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
-- table.
--
-- To transcribe speech in Modern Standard Arabic (@ar-SA@), your media
-- file must be encoded at a sample rate of 16,000 Hz or higher.
callAnalyticsJobSettings_languageOptions :: Lens.Lens' CallAnalyticsJobSettings (Prelude.Maybe (Prelude.NonEmpty LanguageCode))
callAnalyticsJobSettings_languageOptions = Lens.lens (\CallAnalyticsJobSettings' {languageOptions} -> languageOptions) (\s@CallAnalyticsJobSettings' {} a -> s {languageOptions = a} :: CallAnalyticsJobSettings) Prelude.. Lens.mapping Lens.coerced

-- | Specify how you want your custom vocabulary filter applied to your
-- transcript.
--
-- To replace words with @***@, choose @mask@.
--
-- To delete words, choose @remove@.
--
-- To flag words without changing them, choose @tag@.
callAnalyticsJobSettings_vocabularyFilterMethod :: Lens.Lens' CallAnalyticsJobSettings (Prelude.Maybe VocabularyFilterMethod)
callAnalyticsJobSettings_vocabularyFilterMethod = Lens.lens (\CallAnalyticsJobSettings' {vocabularyFilterMethod} -> vocabularyFilterMethod) (\s@CallAnalyticsJobSettings' {} a -> s {vocabularyFilterMethod = a} :: CallAnalyticsJobSettings)

-- | The name of the custom vocabulary filter you want to include in your
-- Call Analytics transcription request. Custom vocabulary filter names are
-- case sensitive.
--
-- Note that if you include @VocabularyFilterName@ in your request, you
-- must also include @VocabularyFilterMethod@.
callAnalyticsJobSettings_vocabularyFilterName :: Lens.Lens' CallAnalyticsJobSettings (Prelude.Maybe Prelude.Text)
callAnalyticsJobSettings_vocabularyFilterName = Lens.lens (\CallAnalyticsJobSettings' {vocabularyFilterName} -> vocabularyFilterName) (\s@CallAnalyticsJobSettings' {} a -> s {vocabularyFilterName = a} :: CallAnalyticsJobSettings)

-- | The name of the custom vocabulary you want to include in your Call
-- Analytics transcription request. Custom vocabulary names are case
-- sensitive.
callAnalyticsJobSettings_vocabularyName :: Lens.Lens' CallAnalyticsJobSettings (Prelude.Maybe Prelude.Text)
callAnalyticsJobSettings_vocabularyName = Lens.lens (\CallAnalyticsJobSettings' {vocabularyName} -> vocabularyName) (\s@CallAnalyticsJobSettings' {} a -> s {vocabularyName = a} :: CallAnalyticsJobSettings)

instance Data.FromJSON CallAnalyticsJobSettings where
  parseJSON =
    Data.withObject
      "CallAnalyticsJobSettings"
      ( \x ->
          CallAnalyticsJobSettings'
            Prelude.<$> (x Data..:? "ContentRedaction")
            Prelude.<*> ( x Data..:? "LanguageIdSettings"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "LanguageModelName")
            Prelude.<*> (x Data..:? "LanguageOptions")
            Prelude.<*> (x Data..:? "VocabularyFilterMethod")
            Prelude.<*> (x Data..:? "VocabularyFilterName")
            Prelude.<*> (x Data..:? "VocabularyName")
      )

instance Prelude.Hashable CallAnalyticsJobSettings where
  hashWithSalt _salt CallAnalyticsJobSettings' {..} =
    _salt `Prelude.hashWithSalt` contentRedaction
      `Prelude.hashWithSalt` languageIdSettings
      `Prelude.hashWithSalt` languageModelName
      `Prelude.hashWithSalt` languageOptions
      `Prelude.hashWithSalt` vocabularyFilterMethod
      `Prelude.hashWithSalt` vocabularyFilterName
      `Prelude.hashWithSalt` vocabularyName

instance Prelude.NFData CallAnalyticsJobSettings where
  rnf CallAnalyticsJobSettings' {..} =
    Prelude.rnf contentRedaction
      `Prelude.seq` Prelude.rnf languageIdSettings
      `Prelude.seq` Prelude.rnf languageModelName
      `Prelude.seq` Prelude.rnf languageOptions
      `Prelude.seq` Prelude.rnf vocabularyFilterMethod
      `Prelude.seq` Prelude.rnf vocabularyFilterName
      `Prelude.seq` Prelude.rnf vocabularyName

instance Data.ToJSON CallAnalyticsJobSettings where
  toJSON CallAnalyticsJobSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContentRedaction" Data..=)
              Prelude.<$> contentRedaction,
            ("LanguageIdSettings" Data..=)
              Prelude.<$> languageIdSettings,
            ("LanguageModelName" Data..=)
              Prelude.<$> languageModelName,
            ("LanguageOptions" Data..=)
              Prelude.<$> languageOptions,
            ("VocabularyFilterMethod" Data..=)
              Prelude.<$> vocabularyFilterMethod,
            ("VocabularyFilterName" Data..=)
              Prelude.<$> vocabularyFilterName,
            ("VocabularyName" Data..=)
              Prelude.<$> vocabularyName
          ]
      )
