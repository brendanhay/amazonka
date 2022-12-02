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
-- Module      : Amazonka.Transcribe.CreateVocabularyFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom vocabulary filter.
--
-- You can use vocabulary filters to mask, delete, or flag specific words
-- from your transcript. Vocabulary filters are commonly used to mask
-- profanity in transcripts.
--
-- Each language has a character set that contains all allowed characters
-- for that specific language. If you use unsupported characters, your
-- vocabulary filter request fails. Refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/charsets.html Character Sets for Custom Vocabularies>
-- to get the character set for your language.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/vocabulary-filtering.html Using vocabulary filtering with unwanted words>.
module Amazonka.Transcribe.CreateVocabularyFilter
  ( -- * Creating a Request
    CreateVocabularyFilter (..),
    newCreateVocabularyFilter,

    -- * Request Lenses
    createVocabularyFilter_tags,
    createVocabularyFilter_words,
    createVocabularyFilter_vocabularyFilterFileUri,
    createVocabularyFilter_vocabularyFilterName,
    createVocabularyFilter_languageCode,

    -- * Destructuring the Response
    CreateVocabularyFilterResponse (..),
    newCreateVocabularyFilterResponse,

    -- * Response Lenses
    createVocabularyFilterResponse_lastModifiedTime,
    createVocabularyFilterResponse_languageCode,
    createVocabularyFilterResponse_vocabularyFilterName,
    createVocabularyFilterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newCreateVocabularyFilter' smart constructor.
data CreateVocabularyFilter = CreateVocabularyFilter'
  { -- | Adds one or more custom tags, each in the form of a key:value pair, to a
    -- new custom vocabulary filter at the time you create this new filter.
    --
    -- To learn more about using tags with Amazon Transcribe, refer to
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/tagging.html Tagging resources>.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | Use this parameter if you want to create your vocabulary filter by
    -- including all desired terms, as comma-separated values, within your
    -- request. The other option for creating your vocabulary filter is to save
    -- your entries in a text file and upload them to an Amazon S3 bucket, then
    -- specify the location of your file using the @VocabularyFilterFileUri@
    -- parameter.
    --
    -- Note that if you include @Words@ in your request, you cannot use
    -- @VocabularyFilterFileUri@; you must choose one or the other.
    --
    -- Each language has a character set that contains all allowed characters
    -- for that specific language. If you use unsupported characters, your
    -- vocabulary filter request fails. Refer to
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/charsets.html Character Sets for Custom Vocabularies>
    -- to get the character set for your language.
    words :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The Amazon S3 location of the text file that contains your custom
    -- vocabulary filter terms. The URI must be located in the same Amazon Web
    -- Services Region as the resource you\'re calling.
    --
    -- Here\'s an example URI path:
    -- @s3:\/\/DOC-EXAMPLE-BUCKET\/my-vocab-filter-file.txt@
    --
    -- Note that if you include @VocabularyFilterFileUri@ in your request, you
    -- cannot use @Words@; you must choose one or the other.
    vocabularyFilterFileUri :: Prelude.Maybe Prelude.Text,
    -- | A unique name, chosen by you, for your new custom vocabulary filter.
    --
    -- This name is case sensitive, cannot contain spaces, and must be unique
    -- within an Amazon Web Services account. If you try to create a new
    -- vocabulary filter with the same name as an existing vocabulary filter,
    -- you get a @ConflictException@ error.
    vocabularyFilterName :: Prelude.Text,
    -- | The language code that represents the language of the entries in your
    -- vocabulary filter. Each vocabulary filter must contain terms in only one
    -- language.
    --
    -- A vocabulary filter can only be used to transcribe files in the same
    -- language as the filter. For example, if you create a vocabulary filter
    -- using US English (@en-US@), you can only apply this filter to files that
    -- contain English audio.
    --
    -- For a list of supported languages and their associated language codes,
    -- refer to the
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
    -- table.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVocabularyFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createVocabularyFilter_tags' - Adds one or more custom tags, each in the form of a key:value pair, to a
-- new custom vocabulary filter at the time you create this new filter.
--
-- To learn more about using tags with Amazon Transcribe, refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/tagging.html Tagging resources>.
--
-- 'words', 'createVocabularyFilter_words' - Use this parameter if you want to create your vocabulary filter by
-- including all desired terms, as comma-separated values, within your
-- request. The other option for creating your vocabulary filter is to save
-- your entries in a text file and upload them to an Amazon S3 bucket, then
-- specify the location of your file using the @VocabularyFilterFileUri@
-- parameter.
--
-- Note that if you include @Words@ in your request, you cannot use
-- @VocabularyFilterFileUri@; you must choose one or the other.
--
-- Each language has a character set that contains all allowed characters
-- for that specific language. If you use unsupported characters, your
-- vocabulary filter request fails. Refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/charsets.html Character Sets for Custom Vocabularies>
-- to get the character set for your language.
--
-- 'vocabularyFilterFileUri', 'createVocabularyFilter_vocabularyFilterFileUri' - The Amazon S3 location of the text file that contains your custom
-- vocabulary filter terms. The URI must be located in the same Amazon Web
-- Services Region as the resource you\'re calling.
--
-- Here\'s an example URI path:
-- @s3:\/\/DOC-EXAMPLE-BUCKET\/my-vocab-filter-file.txt@
--
-- Note that if you include @VocabularyFilterFileUri@ in your request, you
-- cannot use @Words@; you must choose one or the other.
--
-- 'vocabularyFilterName', 'createVocabularyFilter_vocabularyFilterName' - A unique name, chosen by you, for your new custom vocabulary filter.
--
-- This name is case sensitive, cannot contain spaces, and must be unique
-- within an Amazon Web Services account. If you try to create a new
-- vocabulary filter with the same name as an existing vocabulary filter,
-- you get a @ConflictException@ error.
--
-- 'languageCode', 'createVocabularyFilter_languageCode' - The language code that represents the language of the entries in your
-- vocabulary filter. Each vocabulary filter must contain terms in only one
-- language.
--
-- A vocabulary filter can only be used to transcribe files in the same
-- language as the filter. For example, if you create a vocabulary filter
-- using US English (@en-US@), you can only apply this filter to files that
-- contain English audio.
--
-- For a list of supported languages and their associated language codes,
-- refer to the
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
-- table.
newCreateVocabularyFilter ::
  -- | 'vocabularyFilterName'
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  CreateVocabularyFilter
newCreateVocabularyFilter
  pVocabularyFilterName_
  pLanguageCode_ =
    CreateVocabularyFilter'
      { tags = Prelude.Nothing,
        words = Prelude.Nothing,
        vocabularyFilterFileUri = Prelude.Nothing,
        vocabularyFilterName = pVocabularyFilterName_,
        languageCode = pLanguageCode_
      }

-- | Adds one or more custom tags, each in the form of a key:value pair, to a
-- new custom vocabulary filter at the time you create this new filter.
--
-- To learn more about using tags with Amazon Transcribe, refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/tagging.html Tagging resources>.
createVocabularyFilter_tags :: Lens.Lens' CreateVocabularyFilter (Prelude.Maybe (Prelude.NonEmpty Tag))
createVocabularyFilter_tags = Lens.lens (\CreateVocabularyFilter' {tags} -> tags) (\s@CreateVocabularyFilter' {} a -> s {tags = a} :: CreateVocabularyFilter) Prelude.. Lens.mapping Lens.coerced

-- | Use this parameter if you want to create your vocabulary filter by
-- including all desired terms, as comma-separated values, within your
-- request. The other option for creating your vocabulary filter is to save
-- your entries in a text file and upload them to an Amazon S3 bucket, then
-- specify the location of your file using the @VocabularyFilterFileUri@
-- parameter.
--
-- Note that if you include @Words@ in your request, you cannot use
-- @VocabularyFilterFileUri@; you must choose one or the other.
--
-- Each language has a character set that contains all allowed characters
-- for that specific language. If you use unsupported characters, your
-- vocabulary filter request fails. Refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/charsets.html Character Sets for Custom Vocabularies>
-- to get the character set for your language.
createVocabularyFilter_words :: Lens.Lens' CreateVocabularyFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createVocabularyFilter_words = Lens.lens (\CreateVocabularyFilter' {words} -> words) (\s@CreateVocabularyFilter' {} a -> s {words = a} :: CreateVocabularyFilter) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon S3 location of the text file that contains your custom
-- vocabulary filter terms. The URI must be located in the same Amazon Web
-- Services Region as the resource you\'re calling.
--
-- Here\'s an example URI path:
-- @s3:\/\/DOC-EXAMPLE-BUCKET\/my-vocab-filter-file.txt@
--
-- Note that if you include @VocabularyFilterFileUri@ in your request, you
-- cannot use @Words@; you must choose one or the other.
createVocabularyFilter_vocabularyFilterFileUri :: Lens.Lens' CreateVocabularyFilter (Prelude.Maybe Prelude.Text)
createVocabularyFilter_vocabularyFilterFileUri = Lens.lens (\CreateVocabularyFilter' {vocabularyFilterFileUri} -> vocabularyFilterFileUri) (\s@CreateVocabularyFilter' {} a -> s {vocabularyFilterFileUri = a} :: CreateVocabularyFilter)

-- | A unique name, chosen by you, for your new custom vocabulary filter.
--
-- This name is case sensitive, cannot contain spaces, and must be unique
-- within an Amazon Web Services account. If you try to create a new
-- vocabulary filter with the same name as an existing vocabulary filter,
-- you get a @ConflictException@ error.
createVocabularyFilter_vocabularyFilterName :: Lens.Lens' CreateVocabularyFilter Prelude.Text
createVocabularyFilter_vocabularyFilterName = Lens.lens (\CreateVocabularyFilter' {vocabularyFilterName} -> vocabularyFilterName) (\s@CreateVocabularyFilter' {} a -> s {vocabularyFilterName = a} :: CreateVocabularyFilter)

-- | The language code that represents the language of the entries in your
-- vocabulary filter. Each vocabulary filter must contain terms in only one
-- language.
--
-- A vocabulary filter can only be used to transcribe files in the same
-- language as the filter. For example, if you create a vocabulary filter
-- using US English (@en-US@), you can only apply this filter to files that
-- contain English audio.
--
-- For a list of supported languages and their associated language codes,
-- refer to the
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
-- table.
createVocabularyFilter_languageCode :: Lens.Lens' CreateVocabularyFilter LanguageCode
createVocabularyFilter_languageCode = Lens.lens (\CreateVocabularyFilter' {languageCode} -> languageCode) (\s@CreateVocabularyFilter' {} a -> s {languageCode = a} :: CreateVocabularyFilter)

instance Core.AWSRequest CreateVocabularyFilter where
  type
    AWSResponse CreateVocabularyFilter =
      CreateVocabularyFilterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVocabularyFilterResponse'
            Prelude.<$> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "LanguageCode")
            Prelude.<*> (x Data..?> "VocabularyFilterName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVocabularyFilter where
  hashWithSalt _salt CreateVocabularyFilter' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` words
      `Prelude.hashWithSalt` vocabularyFilterFileUri
      `Prelude.hashWithSalt` vocabularyFilterName
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData CreateVocabularyFilter where
  rnf CreateVocabularyFilter' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf words
      `Prelude.seq` Prelude.rnf vocabularyFilterFileUri
      `Prelude.seq` Prelude.rnf vocabularyFilterName
      `Prelude.seq` Prelude.rnf languageCode

instance Data.ToHeaders CreateVocabularyFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Transcribe.CreateVocabularyFilter" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateVocabularyFilter where
  toJSON CreateVocabularyFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("Words" Data..=) Prelude.<$> words,
            ("VocabularyFilterFileUri" Data..=)
              Prelude.<$> vocabularyFilterFileUri,
            Prelude.Just
              ( "VocabularyFilterName"
                  Data..= vocabularyFilterName
              ),
            Prelude.Just ("LanguageCode" Data..= languageCode)
          ]
      )

instance Data.ToPath CreateVocabularyFilter where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateVocabularyFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVocabularyFilterResponse' smart constructor.
data CreateVocabularyFilterResponse = CreateVocabularyFilterResponse'
  { -- | The date and time you created your vocabulary filter.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
    -- May 4, 2022.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The language code you selected for your vocabulary filter.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The name you chose for your custom vocabulary filter.
    vocabularyFilterName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVocabularyFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTime', 'createVocabularyFilterResponse_lastModifiedTime' - The date and time you created your vocabulary filter.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
--
-- 'languageCode', 'createVocabularyFilterResponse_languageCode' - The language code you selected for your vocabulary filter.
--
-- 'vocabularyFilterName', 'createVocabularyFilterResponse_vocabularyFilterName' - The name you chose for your custom vocabulary filter.
--
-- 'httpStatus', 'createVocabularyFilterResponse_httpStatus' - The response's http status code.
newCreateVocabularyFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVocabularyFilterResponse
newCreateVocabularyFilterResponse pHttpStatus_ =
  CreateVocabularyFilterResponse'
    { lastModifiedTime =
        Prelude.Nothing,
      languageCode = Prelude.Nothing,
      vocabularyFilterName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time you created your vocabulary filter.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
createVocabularyFilterResponse_lastModifiedTime :: Lens.Lens' CreateVocabularyFilterResponse (Prelude.Maybe Prelude.UTCTime)
createVocabularyFilterResponse_lastModifiedTime = Lens.lens (\CreateVocabularyFilterResponse' {lastModifiedTime} -> lastModifiedTime) (\s@CreateVocabularyFilterResponse' {} a -> s {lastModifiedTime = a} :: CreateVocabularyFilterResponse) Prelude.. Lens.mapping Data._Time

-- | The language code you selected for your vocabulary filter.
createVocabularyFilterResponse_languageCode :: Lens.Lens' CreateVocabularyFilterResponse (Prelude.Maybe LanguageCode)
createVocabularyFilterResponse_languageCode = Lens.lens (\CreateVocabularyFilterResponse' {languageCode} -> languageCode) (\s@CreateVocabularyFilterResponse' {} a -> s {languageCode = a} :: CreateVocabularyFilterResponse)

-- | The name you chose for your custom vocabulary filter.
createVocabularyFilterResponse_vocabularyFilterName :: Lens.Lens' CreateVocabularyFilterResponse (Prelude.Maybe Prelude.Text)
createVocabularyFilterResponse_vocabularyFilterName = Lens.lens (\CreateVocabularyFilterResponse' {vocabularyFilterName} -> vocabularyFilterName) (\s@CreateVocabularyFilterResponse' {} a -> s {vocabularyFilterName = a} :: CreateVocabularyFilterResponse)

-- | The response's http status code.
createVocabularyFilterResponse_httpStatus :: Lens.Lens' CreateVocabularyFilterResponse Prelude.Int
createVocabularyFilterResponse_httpStatus = Lens.lens (\CreateVocabularyFilterResponse' {httpStatus} -> httpStatus) (\s@CreateVocabularyFilterResponse' {} a -> s {httpStatus = a} :: CreateVocabularyFilterResponse)

instance
  Prelude.NFData
    CreateVocabularyFilterResponse
  where
  rnf CreateVocabularyFilterResponse' {..} =
    Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf vocabularyFilterName
      `Prelude.seq` Prelude.rnf httpStatus
