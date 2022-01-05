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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new vocabulary filter that you can use to filter words, such
-- as profane words, from the output of a transcription job.
module Amazonka.Transcribe.CreateVocabularyFilter
  ( -- * Creating a Request
    CreateVocabularyFilter (..),
    newCreateVocabularyFilter,

    -- * Request Lenses
    createVocabularyFilter_vocabularyFilterFileUri,
    createVocabularyFilter_words,
    createVocabularyFilter_tags,
    createVocabularyFilter_vocabularyFilterName,
    createVocabularyFilter_languageCode,

    -- * Destructuring the Response
    CreateVocabularyFilterResponse (..),
    newCreateVocabularyFilterResponse,

    -- * Response Lenses
    createVocabularyFilterResponse_languageCode,
    createVocabularyFilterResponse_lastModifiedTime,
    createVocabularyFilterResponse_vocabularyFilterName,
    createVocabularyFilterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newCreateVocabularyFilter' smart constructor.
data CreateVocabularyFilter = CreateVocabularyFilter'
  { -- | The Amazon S3 location of a text file used as input to create the
    -- vocabulary filter. Only use characters from the character set defined
    -- for custom vocabularies. For a list of character sets, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies>.
    --
    -- The specified file must be less than 50 KB of UTF-8 characters.
    --
    -- If you provide the location of a list of words in the
    -- @VocabularyFilterFileUri@ parameter, you can\'t use the @Words@
    -- parameter.
    vocabularyFilterFileUri :: Prelude.Maybe Prelude.Text,
    -- | The words to use in the vocabulary filter. Only use characters from the
    -- character set defined for custom vocabularies. For a list of character
    -- sets, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies>.
    --
    -- If you provide a list of words in the @Words@ parameter, you can\'t use
    -- the @VocabularyFilterFileUri@ parameter.
    words :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Adds one or more tags, each in the form of a key:value pair, to a new
    -- Amazon Transcribe vocabulary filter at the time you create this new
    -- vocabulary filter.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The vocabulary filter name. The name must be unique within the account
    -- that contains it. If you try to create a vocabulary filter with the same
    -- name as another vocabulary filter, you get a @ConflictException@ error.
    vocabularyFilterName :: Prelude.Text,
    -- | The language code of the words in the vocabulary filter. All words in
    -- the filter must be in the same language. The vocabulary filter can only
    -- be used with transcription jobs in the specified language.
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
-- 'vocabularyFilterFileUri', 'createVocabularyFilter_vocabularyFilterFileUri' - The Amazon S3 location of a text file used as input to create the
-- vocabulary filter. Only use characters from the character set defined
-- for custom vocabularies. For a list of character sets, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies>.
--
-- The specified file must be less than 50 KB of UTF-8 characters.
--
-- If you provide the location of a list of words in the
-- @VocabularyFilterFileUri@ parameter, you can\'t use the @Words@
-- parameter.
--
-- 'words', 'createVocabularyFilter_words' - The words to use in the vocabulary filter. Only use characters from the
-- character set defined for custom vocabularies. For a list of character
-- sets, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies>.
--
-- If you provide a list of words in the @Words@ parameter, you can\'t use
-- the @VocabularyFilterFileUri@ parameter.
--
-- 'tags', 'createVocabularyFilter_tags' - Adds one or more tags, each in the form of a key:value pair, to a new
-- Amazon Transcribe vocabulary filter at the time you create this new
-- vocabulary filter.
--
-- 'vocabularyFilterName', 'createVocabularyFilter_vocabularyFilterName' - The vocabulary filter name. The name must be unique within the account
-- that contains it. If you try to create a vocabulary filter with the same
-- name as another vocabulary filter, you get a @ConflictException@ error.
--
-- 'languageCode', 'createVocabularyFilter_languageCode' - The language code of the words in the vocabulary filter. All words in
-- the filter must be in the same language. The vocabulary filter can only
-- be used with transcription jobs in the specified language.
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
      { vocabularyFilterFileUri =
          Prelude.Nothing,
        words = Prelude.Nothing,
        tags = Prelude.Nothing,
        vocabularyFilterName = pVocabularyFilterName_,
        languageCode = pLanguageCode_
      }

-- | The Amazon S3 location of a text file used as input to create the
-- vocabulary filter. Only use characters from the character set defined
-- for custom vocabularies. For a list of character sets, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies>.
--
-- The specified file must be less than 50 KB of UTF-8 characters.
--
-- If you provide the location of a list of words in the
-- @VocabularyFilterFileUri@ parameter, you can\'t use the @Words@
-- parameter.
createVocabularyFilter_vocabularyFilterFileUri :: Lens.Lens' CreateVocabularyFilter (Prelude.Maybe Prelude.Text)
createVocabularyFilter_vocabularyFilterFileUri = Lens.lens (\CreateVocabularyFilter' {vocabularyFilterFileUri} -> vocabularyFilterFileUri) (\s@CreateVocabularyFilter' {} a -> s {vocabularyFilterFileUri = a} :: CreateVocabularyFilter)

-- | The words to use in the vocabulary filter. Only use characters from the
-- character set defined for custom vocabularies. For a list of character
-- sets, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies>.
--
-- If you provide a list of words in the @Words@ parameter, you can\'t use
-- the @VocabularyFilterFileUri@ parameter.
createVocabularyFilter_words :: Lens.Lens' CreateVocabularyFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createVocabularyFilter_words = Lens.lens (\CreateVocabularyFilter' {words} -> words) (\s@CreateVocabularyFilter' {} a -> s {words = a} :: CreateVocabularyFilter) Prelude.. Lens.mapping Lens.coerced

-- | Adds one or more tags, each in the form of a key:value pair, to a new
-- Amazon Transcribe vocabulary filter at the time you create this new
-- vocabulary filter.
createVocabularyFilter_tags :: Lens.Lens' CreateVocabularyFilter (Prelude.Maybe (Prelude.NonEmpty Tag))
createVocabularyFilter_tags = Lens.lens (\CreateVocabularyFilter' {tags} -> tags) (\s@CreateVocabularyFilter' {} a -> s {tags = a} :: CreateVocabularyFilter) Prelude.. Lens.mapping Lens.coerced

-- | The vocabulary filter name. The name must be unique within the account
-- that contains it. If you try to create a vocabulary filter with the same
-- name as another vocabulary filter, you get a @ConflictException@ error.
createVocabularyFilter_vocabularyFilterName :: Lens.Lens' CreateVocabularyFilter Prelude.Text
createVocabularyFilter_vocabularyFilterName = Lens.lens (\CreateVocabularyFilter' {vocabularyFilterName} -> vocabularyFilterName) (\s@CreateVocabularyFilter' {} a -> s {vocabularyFilterName = a} :: CreateVocabularyFilter)

-- | The language code of the words in the vocabulary filter. All words in
-- the filter must be in the same language. The vocabulary filter can only
-- be used with transcription jobs in the specified language.
createVocabularyFilter_languageCode :: Lens.Lens' CreateVocabularyFilter LanguageCode
createVocabularyFilter_languageCode = Lens.lens (\CreateVocabularyFilter' {languageCode} -> languageCode) (\s@CreateVocabularyFilter' {} a -> s {languageCode = a} :: CreateVocabularyFilter)

instance Core.AWSRequest CreateVocabularyFilter where
  type
    AWSResponse CreateVocabularyFilter =
      CreateVocabularyFilterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVocabularyFilterResponse'
            Prelude.<$> (x Core..?> "LanguageCode")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "VocabularyFilterName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVocabularyFilter where
  hashWithSalt _salt CreateVocabularyFilter' {..} =
    _salt
      `Prelude.hashWithSalt` vocabularyFilterFileUri
      `Prelude.hashWithSalt` words
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vocabularyFilterName
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData CreateVocabularyFilter where
  rnf CreateVocabularyFilter' {..} =
    Prelude.rnf vocabularyFilterFileUri
      `Prelude.seq` Prelude.rnf words
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vocabularyFilterName
      `Prelude.seq` Prelude.rnf languageCode

instance Core.ToHeaders CreateVocabularyFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.CreateVocabularyFilter" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateVocabularyFilter where
  toJSON CreateVocabularyFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VocabularyFilterFileUri" Core..=)
              Prelude.<$> vocabularyFilterFileUri,
            ("Words" Core..=) Prelude.<$> words,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ( "VocabularyFilterName"
                  Core..= vocabularyFilterName
              ),
            Prelude.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.ToPath CreateVocabularyFilter where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateVocabularyFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVocabularyFilterResponse' smart constructor.
data CreateVocabularyFilterResponse = CreateVocabularyFilterResponse'
  { -- | The language code of the words in the collection.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The date and time that the vocabulary filter was modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the vocabulary filter.
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
-- 'languageCode', 'createVocabularyFilterResponse_languageCode' - The language code of the words in the collection.
--
-- 'lastModifiedTime', 'createVocabularyFilterResponse_lastModifiedTime' - The date and time that the vocabulary filter was modified.
--
-- 'vocabularyFilterName', 'createVocabularyFilterResponse_vocabularyFilterName' - The name of the vocabulary filter.
--
-- 'httpStatus', 'createVocabularyFilterResponse_httpStatus' - The response's http status code.
newCreateVocabularyFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVocabularyFilterResponse
newCreateVocabularyFilterResponse pHttpStatus_ =
  CreateVocabularyFilterResponse'
    { languageCode =
        Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      vocabularyFilterName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The language code of the words in the collection.
createVocabularyFilterResponse_languageCode :: Lens.Lens' CreateVocabularyFilterResponse (Prelude.Maybe LanguageCode)
createVocabularyFilterResponse_languageCode = Lens.lens (\CreateVocabularyFilterResponse' {languageCode} -> languageCode) (\s@CreateVocabularyFilterResponse' {} a -> s {languageCode = a} :: CreateVocabularyFilterResponse)

-- | The date and time that the vocabulary filter was modified.
createVocabularyFilterResponse_lastModifiedTime :: Lens.Lens' CreateVocabularyFilterResponse (Prelude.Maybe Prelude.UTCTime)
createVocabularyFilterResponse_lastModifiedTime = Lens.lens (\CreateVocabularyFilterResponse' {lastModifiedTime} -> lastModifiedTime) (\s@CreateVocabularyFilterResponse' {} a -> s {lastModifiedTime = a} :: CreateVocabularyFilterResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the vocabulary filter.
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
    Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf vocabularyFilterName
      `Prelude.seq` Prelude.rnf httpStatus
