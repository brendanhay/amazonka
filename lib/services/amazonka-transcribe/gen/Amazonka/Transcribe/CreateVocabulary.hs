{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Transcribe.CreateVocabulary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom vocabulary.
--
-- When creating a new custom vocabulary, you can either upload a text file
-- that contains your new entries, phrases, and terms into an Amazon S3
-- bucket and include the URI in your request. Or you can include a list of
-- terms directly in your request using the @Phrases@ flag.
--
-- Each language has a character set that contains all allowed characters
-- for that specific language. If you use unsupported characters, your
-- custom vocabulary request fails. Refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/charsets.html Character Sets for Custom Vocabularies>
-- to get the character set for your language.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/custom-vocabulary.html Custom vocabularies>.
module Amazonka.Transcribe.CreateVocabulary
  ( -- * Creating a Request
    CreateVocabulary (..),
    newCreateVocabulary,

    -- * Request Lenses
    createVocabulary_phrases,
    createVocabulary_tags,
    createVocabulary_vocabularyFileUri,
    createVocabulary_vocabularyName,
    createVocabulary_languageCode,

    -- * Destructuring the Response
    CreateVocabularyResponse (..),
    newCreateVocabularyResponse,

    -- * Response Lenses
    createVocabularyResponse_failureReason,
    createVocabularyResponse_languageCode,
    createVocabularyResponse_lastModifiedTime,
    createVocabularyResponse_vocabularyName,
    createVocabularyResponse_vocabularyState,
    createVocabularyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newCreateVocabulary' smart constructor.
data CreateVocabulary = CreateVocabulary'
  { -- | Use this parameter if you want to create your custom vocabulary by
    -- including all desired terms, as comma-separated values, within your
    -- request. The other option for creating your custom vocabulary is to save
    -- your entries in a text file and upload them to an Amazon S3 bucket, then
    -- specify the location of your file using the @VocabularyFileUri@
    -- parameter.
    --
    -- Note that if you include @Phrases@ in your request, you cannot use
    -- @VocabularyFileUri@; you must choose one or the other.
    --
    -- Each language has a character set that contains all allowed characters
    -- for that specific language. If you use unsupported characters, your
    -- custom vocabulary filter request fails. Refer to
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/charsets.html Character Sets for Custom Vocabularies>
    -- to get the character set for your language.
    phrases :: Prelude.Maybe [Prelude.Text],
    -- | Adds one or more custom tags, each in the form of a key:value pair, to a
    -- new custom vocabulary at the time you create this new custom vocabulary.
    --
    -- To learn more about using tags with Amazon Transcribe, refer to
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/tagging.html Tagging resources>.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The Amazon S3 location of the text file that contains your custom
    -- vocabulary. The URI must be located in the same Amazon Web Services
    -- Region as the resource you\'re calling.
    --
    -- Here\'s an example URI path:
    -- @s3:\/\/DOC-EXAMPLE-BUCKET\/my-vocab-file.txt@
    --
    -- Note that if you include @VocabularyFileUri@ in your request, you cannot
    -- use the @Phrases@ flag; you must choose one or the other.
    vocabularyFileUri :: Prelude.Maybe Prelude.Text,
    -- | A unique name, chosen by you, for your new custom vocabulary.
    --
    -- This name is case sensitive, cannot contain spaces, and must be unique
    -- within an Amazon Web Services account. If you try to create a new custom
    -- vocabulary with the same name as an existing custom vocabulary, you get
    -- a @ConflictException@ error.
    vocabularyName :: Prelude.Text,
    -- | The language code that represents the language of the entries in your
    -- custom vocabulary. Each custom vocabulary must contain terms in only one
    -- language.
    --
    -- A custom vocabulary can only be used to transcribe files in the same
    -- language as the custom vocabulary. For example, if you create a custom
    -- vocabulary using US English (@en-US@), you can only apply this custom
    -- vocabulary to files that contain English audio.
    --
    -- For a list of supported languages and their associated language codes,
    -- refer to the
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
    -- table.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVocabulary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phrases', 'createVocabulary_phrases' - Use this parameter if you want to create your custom vocabulary by
-- including all desired terms, as comma-separated values, within your
-- request. The other option for creating your custom vocabulary is to save
-- your entries in a text file and upload them to an Amazon S3 bucket, then
-- specify the location of your file using the @VocabularyFileUri@
-- parameter.
--
-- Note that if you include @Phrases@ in your request, you cannot use
-- @VocabularyFileUri@; you must choose one or the other.
--
-- Each language has a character set that contains all allowed characters
-- for that specific language. If you use unsupported characters, your
-- custom vocabulary filter request fails. Refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/charsets.html Character Sets for Custom Vocabularies>
-- to get the character set for your language.
--
-- 'tags', 'createVocabulary_tags' - Adds one or more custom tags, each in the form of a key:value pair, to a
-- new custom vocabulary at the time you create this new custom vocabulary.
--
-- To learn more about using tags with Amazon Transcribe, refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/tagging.html Tagging resources>.
--
-- 'vocabularyFileUri', 'createVocabulary_vocabularyFileUri' - The Amazon S3 location of the text file that contains your custom
-- vocabulary. The URI must be located in the same Amazon Web Services
-- Region as the resource you\'re calling.
--
-- Here\'s an example URI path:
-- @s3:\/\/DOC-EXAMPLE-BUCKET\/my-vocab-file.txt@
--
-- Note that if you include @VocabularyFileUri@ in your request, you cannot
-- use the @Phrases@ flag; you must choose one or the other.
--
-- 'vocabularyName', 'createVocabulary_vocabularyName' - A unique name, chosen by you, for your new custom vocabulary.
--
-- This name is case sensitive, cannot contain spaces, and must be unique
-- within an Amazon Web Services account. If you try to create a new custom
-- vocabulary with the same name as an existing custom vocabulary, you get
-- a @ConflictException@ error.
--
-- 'languageCode', 'createVocabulary_languageCode' - The language code that represents the language of the entries in your
-- custom vocabulary. Each custom vocabulary must contain terms in only one
-- language.
--
-- A custom vocabulary can only be used to transcribe files in the same
-- language as the custom vocabulary. For example, if you create a custom
-- vocabulary using US English (@en-US@), you can only apply this custom
-- vocabulary to files that contain English audio.
--
-- For a list of supported languages and their associated language codes,
-- refer to the
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
-- table.
newCreateVocabulary ::
  -- | 'vocabularyName'
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  CreateVocabulary
newCreateVocabulary pVocabularyName_ pLanguageCode_ =
  CreateVocabulary'
    { phrases = Prelude.Nothing,
      tags = Prelude.Nothing,
      vocabularyFileUri = Prelude.Nothing,
      vocabularyName = pVocabularyName_,
      languageCode = pLanguageCode_
    }

-- | Use this parameter if you want to create your custom vocabulary by
-- including all desired terms, as comma-separated values, within your
-- request. The other option for creating your custom vocabulary is to save
-- your entries in a text file and upload them to an Amazon S3 bucket, then
-- specify the location of your file using the @VocabularyFileUri@
-- parameter.
--
-- Note that if you include @Phrases@ in your request, you cannot use
-- @VocabularyFileUri@; you must choose one or the other.
--
-- Each language has a character set that contains all allowed characters
-- for that specific language. If you use unsupported characters, your
-- custom vocabulary filter request fails. Refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/charsets.html Character Sets for Custom Vocabularies>
-- to get the character set for your language.
createVocabulary_phrases :: Lens.Lens' CreateVocabulary (Prelude.Maybe [Prelude.Text])
createVocabulary_phrases = Lens.lens (\CreateVocabulary' {phrases} -> phrases) (\s@CreateVocabulary' {} a -> s {phrases = a} :: CreateVocabulary) Prelude.. Lens.mapping Lens.coerced

-- | Adds one or more custom tags, each in the form of a key:value pair, to a
-- new custom vocabulary at the time you create this new custom vocabulary.
--
-- To learn more about using tags with Amazon Transcribe, refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/tagging.html Tagging resources>.
createVocabulary_tags :: Lens.Lens' CreateVocabulary (Prelude.Maybe (Prelude.NonEmpty Tag))
createVocabulary_tags = Lens.lens (\CreateVocabulary' {tags} -> tags) (\s@CreateVocabulary' {} a -> s {tags = a} :: CreateVocabulary) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon S3 location of the text file that contains your custom
-- vocabulary. The URI must be located in the same Amazon Web Services
-- Region as the resource you\'re calling.
--
-- Here\'s an example URI path:
-- @s3:\/\/DOC-EXAMPLE-BUCKET\/my-vocab-file.txt@
--
-- Note that if you include @VocabularyFileUri@ in your request, you cannot
-- use the @Phrases@ flag; you must choose one or the other.
createVocabulary_vocabularyFileUri :: Lens.Lens' CreateVocabulary (Prelude.Maybe Prelude.Text)
createVocabulary_vocabularyFileUri = Lens.lens (\CreateVocabulary' {vocabularyFileUri} -> vocabularyFileUri) (\s@CreateVocabulary' {} a -> s {vocabularyFileUri = a} :: CreateVocabulary)

-- | A unique name, chosen by you, for your new custom vocabulary.
--
-- This name is case sensitive, cannot contain spaces, and must be unique
-- within an Amazon Web Services account. If you try to create a new custom
-- vocabulary with the same name as an existing custom vocabulary, you get
-- a @ConflictException@ error.
createVocabulary_vocabularyName :: Lens.Lens' CreateVocabulary Prelude.Text
createVocabulary_vocabularyName = Lens.lens (\CreateVocabulary' {vocabularyName} -> vocabularyName) (\s@CreateVocabulary' {} a -> s {vocabularyName = a} :: CreateVocabulary)

-- | The language code that represents the language of the entries in your
-- custom vocabulary. Each custom vocabulary must contain terms in only one
-- language.
--
-- A custom vocabulary can only be used to transcribe files in the same
-- language as the custom vocabulary. For example, if you create a custom
-- vocabulary using US English (@en-US@), you can only apply this custom
-- vocabulary to files that contain English audio.
--
-- For a list of supported languages and their associated language codes,
-- refer to the
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
-- table.
createVocabulary_languageCode :: Lens.Lens' CreateVocabulary LanguageCode
createVocabulary_languageCode = Lens.lens (\CreateVocabulary' {languageCode} -> languageCode) (\s@CreateVocabulary' {} a -> s {languageCode = a} :: CreateVocabulary)

instance Core.AWSRequest CreateVocabulary where
  type
    AWSResponse CreateVocabulary =
      CreateVocabularyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVocabularyResponse'
            Prelude.<$> (x Data..?> "FailureReason")
            Prelude.<*> (x Data..?> "LanguageCode")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "VocabularyName")
            Prelude.<*> (x Data..?> "VocabularyState")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVocabulary where
  hashWithSalt _salt CreateVocabulary' {..} =
    _salt `Prelude.hashWithSalt` phrases
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vocabularyFileUri
      `Prelude.hashWithSalt` vocabularyName
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData CreateVocabulary where
  rnf CreateVocabulary' {..} =
    Prelude.rnf phrases
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vocabularyFileUri
      `Prelude.seq` Prelude.rnf vocabularyName
      `Prelude.seq` Prelude.rnf languageCode

instance Data.ToHeaders CreateVocabulary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Transcribe.CreateVocabulary" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateVocabulary where
  toJSON CreateVocabulary' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Phrases" Data..=) Prelude.<$> phrases,
            ("Tags" Data..=) Prelude.<$> tags,
            ("VocabularyFileUri" Data..=)
              Prelude.<$> vocabularyFileUri,
            Prelude.Just
              ("VocabularyName" Data..= vocabularyName),
            Prelude.Just ("LanguageCode" Data..= languageCode)
          ]
      )

instance Data.ToPath CreateVocabulary where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateVocabulary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVocabularyResponse' smart constructor.
data CreateVocabularyResponse = CreateVocabularyResponse'
  { -- | If @VocabularyState@ is @FAILED@, @FailureReason@ contains information
    -- about why the custom vocabulary request failed. See also:
    -- <https://docs.aws.amazon.com/transcribe/latest/APIReference/CommonErrors.html Common Errors>.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The language code you selected for your custom vocabulary.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The date and time you created your custom vocabulary.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
    -- May 4, 2022.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name you chose for your custom vocabulary.
    vocabularyName :: Prelude.Maybe Prelude.Text,
    -- | The processing state of your custom vocabulary. If the state is @READY@,
    -- you can use the custom vocabulary in a @StartTranscriptionJob@ request.
    vocabularyState :: Prelude.Maybe VocabularyState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVocabularyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'createVocabularyResponse_failureReason' - If @VocabularyState@ is @FAILED@, @FailureReason@ contains information
-- about why the custom vocabulary request failed. See also:
-- <https://docs.aws.amazon.com/transcribe/latest/APIReference/CommonErrors.html Common Errors>.
--
-- 'languageCode', 'createVocabularyResponse_languageCode' - The language code you selected for your custom vocabulary.
--
-- 'lastModifiedTime', 'createVocabularyResponse_lastModifiedTime' - The date and time you created your custom vocabulary.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
--
-- 'vocabularyName', 'createVocabularyResponse_vocabularyName' - The name you chose for your custom vocabulary.
--
-- 'vocabularyState', 'createVocabularyResponse_vocabularyState' - The processing state of your custom vocabulary. If the state is @READY@,
-- you can use the custom vocabulary in a @StartTranscriptionJob@ request.
--
-- 'httpStatus', 'createVocabularyResponse_httpStatus' - The response's http status code.
newCreateVocabularyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVocabularyResponse
newCreateVocabularyResponse pHttpStatus_ =
  CreateVocabularyResponse'
    { failureReason =
        Prelude.Nothing,
      languageCode = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      vocabularyName = Prelude.Nothing,
      vocabularyState = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If @VocabularyState@ is @FAILED@, @FailureReason@ contains information
-- about why the custom vocabulary request failed. See also:
-- <https://docs.aws.amazon.com/transcribe/latest/APIReference/CommonErrors.html Common Errors>.
createVocabularyResponse_failureReason :: Lens.Lens' CreateVocabularyResponse (Prelude.Maybe Prelude.Text)
createVocabularyResponse_failureReason = Lens.lens (\CreateVocabularyResponse' {failureReason} -> failureReason) (\s@CreateVocabularyResponse' {} a -> s {failureReason = a} :: CreateVocabularyResponse)

-- | The language code you selected for your custom vocabulary.
createVocabularyResponse_languageCode :: Lens.Lens' CreateVocabularyResponse (Prelude.Maybe LanguageCode)
createVocabularyResponse_languageCode = Lens.lens (\CreateVocabularyResponse' {languageCode} -> languageCode) (\s@CreateVocabularyResponse' {} a -> s {languageCode = a} :: CreateVocabularyResponse)

-- | The date and time you created your custom vocabulary.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
createVocabularyResponse_lastModifiedTime :: Lens.Lens' CreateVocabularyResponse (Prelude.Maybe Prelude.UTCTime)
createVocabularyResponse_lastModifiedTime = Lens.lens (\CreateVocabularyResponse' {lastModifiedTime} -> lastModifiedTime) (\s@CreateVocabularyResponse' {} a -> s {lastModifiedTime = a} :: CreateVocabularyResponse) Prelude.. Lens.mapping Data._Time

-- | The name you chose for your custom vocabulary.
createVocabularyResponse_vocabularyName :: Lens.Lens' CreateVocabularyResponse (Prelude.Maybe Prelude.Text)
createVocabularyResponse_vocabularyName = Lens.lens (\CreateVocabularyResponse' {vocabularyName} -> vocabularyName) (\s@CreateVocabularyResponse' {} a -> s {vocabularyName = a} :: CreateVocabularyResponse)

-- | The processing state of your custom vocabulary. If the state is @READY@,
-- you can use the custom vocabulary in a @StartTranscriptionJob@ request.
createVocabularyResponse_vocabularyState :: Lens.Lens' CreateVocabularyResponse (Prelude.Maybe VocabularyState)
createVocabularyResponse_vocabularyState = Lens.lens (\CreateVocabularyResponse' {vocabularyState} -> vocabularyState) (\s@CreateVocabularyResponse' {} a -> s {vocabularyState = a} :: CreateVocabularyResponse)

-- | The response's http status code.
createVocabularyResponse_httpStatus :: Lens.Lens' CreateVocabularyResponse Prelude.Int
createVocabularyResponse_httpStatus = Lens.lens (\CreateVocabularyResponse' {httpStatus} -> httpStatus) (\s@CreateVocabularyResponse' {} a -> s {httpStatus = a} :: CreateVocabularyResponse)

instance Prelude.NFData CreateVocabularyResponse where
  rnf CreateVocabularyResponse' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf vocabularyName
      `Prelude.seq` Prelude.rnf vocabularyState
      `Prelude.seq` Prelude.rnf httpStatus
