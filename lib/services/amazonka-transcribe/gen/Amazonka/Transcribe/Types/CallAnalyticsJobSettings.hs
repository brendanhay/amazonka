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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.CallAnalyticsJobSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.ContentRedaction
import Amazonka.Transcribe.Types.LanguageCode
import Amazonka.Transcribe.Types.VocabularyFilterMethod

-- | Provides optional settings for the @CallAnalyticsJob@ operation.
--
-- /See:/ 'newCallAnalyticsJobSettings' smart constructor.
data CallAnalyticsJobSettings = CallAnalyticsJobSettings'
  { contentRedaction :: Prelude.Maybe ContentRedaction,
    -- | When you run a call analytics job, you can specify the language spoken
    -- in the audio, or you can have Amazon Transcribe identify the language
    -- for you.
    --
    -- To specify a language, specify an array with one language code. If you
    -- don\'t know the language, you can leave this field blank and Amazon
    -- Transcribe will use machine learning to identify the language for you.
    -- To improve the ability of Amazon Transcribe to correctly identify the
    -- language, you can provide an array of the languages that can be present
    -- in the audio. Refer to
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html Supported languages and language-specific features>
    -- for additional information.
    languageOptions :: Prelude.Maybe (Prelude.NonEmpty LanguageCode),
    -- | The name of a vocabulary to use when processing the call analytics job.
    vocabularyName :: Prelude.Maybe Prelude.Text,
    -- | The structure used to describe a custom language model.
    languageModelName :: Prelude.Maybe Prelude.Text,
    -- | The name of the vocabulary filter to use when running a call analytics
    -- job. The filter that you specify must have the same language code as the
    -- analytics job.
    vocabularyFilterName :: Prelude.Maybe Prelude.Text,
    -- | Set to mask to remove filtered text from the transcript and replace it
    -- with three asterisks (\"***\") as placeholder text. Set to @remove@ to
    -- remove filtered text from the transcript without using placeholder text.
    -- Set to @tag@ to mark the word in the transcription output that matches
    -- the vocabulary filter. When you set the filter method to @tag@, the
    -- words matching your vocabulary filter are not masked or removed.
    vocabularyFilterMethod :: Prelude.Maybe VocabularyFilterMethod
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
-- 'languageOptions', 'callAnalyticsJobSettings_languageOptions' - When you run a call analytics job, you can specify the language spoken
-- in the audio, or you can have Amazon Transcribe identify the language
-- for you.
--
-- To specify a language, specify an array with one language code. If you
-- don\'t know the language, you can leave this field blank and Amazon
-- Transcribe will use machine learning to identify the language for you.
-- To improve the ability of Amazon Transcribe to correctly identify the
-- language, you can provide an array of the languages that can be present
-- in the audio. Refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html Supported languages and language-specific features>
-- for additional information.
--
-- 'vocabularyName', 'callAnalyticsJobSettings_vocabularyName' - The name of a vocabulary to use when processing the call analytics job.
--
-- 'languageModelName', 'callAnalyticsJobSettings_languageModelName' - The structure used to describe a custom language model.
--
-- 'vocabularyFilterName', 'callAnalyticsJobSettings_vocabularyFilterName' - The name of the vocabulary filter to use when running a call analytics
-- job. The filter that you specify must have the same language code as the
-- analytics job.
--
-- 'vocabularyFilterMethod', 'callAnalyticsJobSettings_vocabularyFilterMethod' - Set to mask to remove filtered text from the transcript and replace it
-- with three asterisks (\"***\") as placeholder text. Set to @remove@ to
-- remove filtered text from the transcript without using placeholder text.
-- Set to @tag@ to mark the word in the transcription output that matches
-- the vocabulary filter. When you set the filter method to @tag@, the
-- words matching your vocabulary filter are not masked or removed.
newCallAnalyticsJobSettings ::
  CallAnalyticsJobSettings
newCallAnalyticsJobSettings =
  CallAnalyticsJobSettings'
    { contentRedaction =
        Prelude.Nothing,
      languageOptions = Prelude.Nothing,
      vocabularyName = Prelude.Nothing,
      languageModelName = Prelude.Nothing,
      vocabularyFilterName = Prelude.Nothing,
      vocabularyFilterMethod = Prelude.Nothing
    }

-- | Undocumented member.
callAnalyticsJobSettings_contentRedaction :: Lens.Lens' CallAnalyticsJobSettings (Prelude.Maybe ContentRedaction)
callAnalyticsJobSettings_contentRedaction = Lens.lens (\CallAnalyticsJobSettings' {contentRedaction} -> contentRedaction) (\s@CallAnalyticsJobSettings' {} a -> s {contentRedaction = a} :: CallAnalyticsJobSettings)

-- | When you run a call analytics job, you can specify the language spoken
-- in the audio, or you can have Amazon Transcribe identify the language
-- for you.
--
-- To specify a language, specify an array with one language code. If you
-- don\'t know the language, you can leave this field blank and Amazon
-- Transcribe will use machine learning to identify the language for you.
-- To improve the ability of Amazon Transcribe to correctly identify the
-- language, you can provide an array of the languages that can be present
-- in the audio. Refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html Supported languages and language-specific features>
-- for additional information.
callAnalyticsJobSettings_languageOptions :: Lens.Lens' CallAnalyticsJobSettings (Prelude.Maybe (Prelude.NonEmpty LanguageCode))
callAnalyticsJobSettings_languageOptions = Lens.lens (\CallAnalyticsJobSettings' {languageOptions} -> languageOptions) (\s@CallAnalyticsJobSettings' {} a -> s {languageOptions = a} :: CallAnalyticsJobSettings) Prelude.. Lens.mapping Lens.coerced

-- | The name of a vocabulary to use when processing the call analytics job.
callAnalyticsJobSettings_vocabularyName :: Lens.Lens' CallAnalyticsJobSettings (Prelude.Maybe Prelude.Text)
callAnalyticsJobSettings_vocabularyName = Lens.lens (\CallAnalyticsJobSettings' {vocabularyName} -> vocabularyName) (\s@CallAnalyticsJobSettings' {} a -> s {vocabularyName = a} :: CallAnalyticsJobSettings)

-- | The structure used to describe a custom language model.
callAnalyticsJobSettings_languageModelName :: Lens.Lens' CallAnalyticsJobSettings (Prelude.Maybe Prelude.Text)
callAnalyticsJobSettings_languageModelName = Lens.lens (\CallAnalyticsJobSettings' {languageModelName} -> languageModelName) (\s@CallAnalyticsJobSettings' {} a -> s {languageModelName = a} :: CallAnalyticsJobSettings)

-- | The name of the vocabulary filter to use when running a call analytics
-- job. The filter that you specify must have the same language code as the
-- analytics job.
callAnalyticsJobSettings_vocabularyFilterName :: Lens.Lens' CallAnalyticsJobSettings (Prelude.Maybe Prelude.Text)
callAnalyticsJobSettings_vocabularyFilterName = Lens.lens (\CallAnalyticsJobSettings' {vocabularyFilterName} -> vocabularyFilterName) (\s@CallAnalyticsJobSettings' {} a -> s {vocabularyFilterName = a} :: CallAnalyticsJobSettings)

-- | Set to mask to remove filtered text from the transcript and replace it
-- with three asterisks (\"***\") as placeholder text. Set to @remove@ to
-- remove filtered text from the transcript without using placeholder text.
-- Set to @tag@ to mark the word in the transcription output that matches
-- the vocabulary filter. When you set the filter method to @tag@, the
-- words matching your vocabulary filter are not masked or removed.
callAnalyticsJobSettings_vocabularyFilterMethod :: Lens.Lens' CallAnalyticsJobSettings (Prelude.Maybe VocabularyFilterMethod)
callAnalyticsJobSettings_vocabularyFilterMethod = Lens.lens (\CallAnalyticsJobSettings' {vocabularyFilterMethod} -> vocabularyFilterMethod) (\s@CallAnalyticsJobSettings' {} a -> s {vocabularyFilterMethod = a} :: CallAnalyticsJobSettings)

instance Core.FromJSON CallAnalyticsJobSettings where
  parseJSON =
    Core.withObject
      "CallAnalyticsJobSettings"
      ( \x ->
          CallAnalyticsJobSettings'
            Prelude.<$> (x Core..:? "ContentRedaction")
            Prelude.<*> (x Core..:? "LanguageOptions")
            Prelude.<*> (x Core..:? "VocabularyName")
            Prelude.<*> (x Core..:? "LanguageModelName")
            Prelude.<*> (x Core..:? "VocabularyFilterName")
            Prelude.<*> (x Core..:? "VocabularyFilterMethod")
      )

instance Prelude.Hashable CallAnalyticsJobSettings where
  hashWithSalt salt' CallAnalyticsJobSettings' {..} =
    salt' `Prelude.hashWithSalt` vocabularyFilterMethod
      `Prelude.hashWithSalt` vocabularyFilterName
      `Prelude.hashWithSalt` languageModelName
      `Prelude.hashWithSalt` vocabularyName
      `Prelude.hashWithSalt` languageOptions
      `Prelude.hashWithSalt` contentRedaction

instance Prelude.NFData CallAnalyticsJobSettings where
  rnf CallAnalyticsJobSettings' {..} =
    Prelude.rnf contentRedaction
      `Prelude.seq` Prelude.rnf vocabularyFilterMethod
      `Prelude.seq` Prelude.rnf vocabularyFilterName
      `Prelude.seq` Prelude.rnf languageModelName
      `Prelude.seq` Prelude.rnf vocabularyName
      `Prelude.seq` Prelude.rnf languageOptions

instance Core.ToJSON CallAnalyticsJobSettings where
  toJSON CallAnalyticsJobSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ContentRedaction" Core..=)
              Prelude.<$> contentRedaction,
            ("LanguageOptions" Core..=)
              Prelude.<$> languageOptions,
            ("VocabularyName" Core..=)
              Prelude.<$> vocabularyName,
            ("LanguageModelName" Core..=)
              Prelude.<$> languageModelName,
            ("VocabularyFilterName" Core..=)
              Prelude.<$> vocabularyFilterName,
            ("VocabularyFilterMethod" Core..=)
              Prelude.<$> vocabularyFilterMethod
          ]
      )
