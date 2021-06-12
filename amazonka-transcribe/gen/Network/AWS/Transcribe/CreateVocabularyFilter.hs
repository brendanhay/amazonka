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
-- Module      : Network.AWS.Transcribe.CreateVocabularyFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new vocabulary filter that you can use to filter words, such
-- as profane words, from the output of a transcription job.
module Network.AWS.Transcribe.CreateVocabularyFilter
  ( -- * Creating a Request
    CreateVocabularyFilter (..),
    newCreateVocabularyFilter,

    -- * Request Lenses
    createVocabularyFilter_vocabularyFilterFileUri,
    createVocabularyFilter_words,
    createVocabularyFilter_vocabularyFilterName,
    createVocabularyFilter_languageCode,

    -- * Destructuring the Response
    CreateVocabularyFilterResponse (..),
    newCreateVocabularyFilterResponse,

    -- * Response Lenses
    createVocabularyFilterResponse_languageCode,
    createVocabularyFilterResponse_vocabularyFilterName,
    createVocabularyFilterResponse_lastModifiedTime,
    createVocabularyFilterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

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
    vocabularyFilterFileUri :: Core.Maybe Core.Text,
    -- | The words to use in the vocabulary filter. Only use characters from the
    -- character set defined for custom vocabularies. For a list of character
    -- sets, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies>.
    --
    -- If you provide a list of words in the @Words@ parameter, you can\'t use
    -- the @VocabularyFilterFileUri@ parameter.
    words :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The vocabulary filter name. The name must be unique within the account
    -- that contains it. If you try to create a vocabulary filter with the same
    -- name as another vocabulary filter, you get a @ConflictException@ error.
    vocabularyFilterName :: Core.Text,
    -- | The language code of the words in the vocabulary filter. All words in
    -- the filter must be in the same language. The vocabulary filter can only
    -- be used with transcription jobs in the specified language.
    languageCode :: LanguageCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'vocabularyFilterName', 'createVocabularyFilter_vocabularyFilterName' - The vocabulary filter name. The name must be unique within the account
-- that contains it. If you try to create a vocabulary filter with the same
-- name as another vocabulary filter, you get a @ConflictException@ error.
--
-- 'languageCode', 'createVocabularyFilter_languageCode' - The language code of the words in the vocabulary filter. All words in
-- the filter must be in the same language. The vocabulary filter can only
-- be used with transcription jobs in the specified language.
newCreateVocabularyFilter ::
  -- | 'vocabularyFilterName'
  Core.Text ->
  -- | 'languageCode'
  LanguageCode ->
  CreateVocabularyFilter
newCreateVocabularyFilter
  pVocabularyFilterName_
  pLanguageCode_ =
    CreateVocabularyFilter'
      { vocabularyFilterFileUri =
          Core.Nothing,
        words = Core.Nothing,
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
createVocabularyFilter_vocabularyFilterFileUri :: Lens.Lens' CreateVocabularyFilter (Core.Maybe Core.Text)
createVocabularyFilter_vocabularyFilterFileUri = Lens.lens (\CreateVocabularyFilter' {vocabularyFilterFileUri} -> vocabularyFilterFileUri) (\s@CreateVocabularyFilter' {} a -> s {vocabularyFilterFileUri = a} :: CreateVocabularyFilter)

-- | The words to use in the vocabulary filter. Only use characters from the
-- character set defined for custom vocabularies. For a list of character
-- sets, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies>.
--
-- If you provide a list of words in the @Words@ parameter, you can\'t use
-- the @VocabularyFilterFileUri@ parameter.
createVocabularyFilter_words :: Lens.Lens' CreateVocabularyFilter (Core.Maybe (Core.NonEmpty Core.Text))
createVocabularyFilter_words = Lens.lens (\CreateVocabularyFilter' {words} -> words) (\s@CreateVocabularyFilter' {} a -> s {words = a} :: CreateVocabularyFilter) Core.. Lens.mapping Lens._Coerce

-- | The vocabulary filter name. The name must be unique within the account
-- that contains it. If you try to create a vocabulary filter with the same
-- name as another vocabulary filter, you get a @ConflictException@ error.
createVocabularyFilter_vocabularyFilterName :: Lens.Lens' CreateVocabularyFilter Core.Text
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
            Core.<$> (x Core..?> "LanguageCode")
            Core.<*> (x Core..?> "VocabularyFilterName")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateVocabularyFilter

instance Core.NFData CreateVocabularyFilter

instance Core.ToHeaders CreateVocabularyFilter where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.CreateVocabularyFilter" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateVocabularyFilter where
  toJSON CreateVocabularyFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("VocabularyFilterFileUri" Core..=)
              Core.<$> vocabularyFilterFileUri,
            ("Words" Core..=) Core.<$> words,
            Core.Just
              ( "VocabularyFilterName"
                  Core..= vocabularyFilterName
              ),
            Core.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.ToPath CreateVocabularyFilter where
  toPath = Core.const "/"

instance Core.ToQuery CreateVocabularyFilter where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateVocabularyFilterResponse' smart constructor.
data CreateVocabularyFilterResponse = CreateVocabularyFilterResponse'
  { -- | The language code of the words in the collection.
    languageCode :: Core.Maybe LanguageCode,
    -- | The name of the vocabulary filter.
    vocabularyFilterName :: Core.Maybe Core.Text,
    -- | The date and time that the vocabulary filter was modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'vocabularyFilterName', 'createVocabularyFilterResponse_vocabularyFilterName' - The name of the vocabulary filter.
--
-- 'lastModifiedTime', 'createVocabularyFilterResponse_lastModifiedTime' - The date and time that the vocabulary filter was modified.
--
-- 'httpStatus', 'createVocabularyFilterResponse_httpStatus' - The response's http status code.
newCreateVocabularyFilterResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateVocabularyFilterResponse
newCreateVocabularyFilterResponse pHttpStatus_ =
  CreateVocabularyFilterResponse'
    { languageCode =
        Core.Nothing,
      vocabularyFilterName = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The language code of the words in the collection.
createVocabularyFilterResponse_languageCode :: Lens.Lens' CreateVocabularyFilterResponse (Core.Maybe LanguageCode)
createVocabularyFilterResponse_languageCode = Lens.lens (\CreateVocabularyFilterResponse' {languageCode} -> languageCode) (\s@CreateVocabularyFilterResponse' {} a -> s {languageCode = a} :: CreateVocabularyFilterResponse)

-- | The name of the vocabulary filter.
createVocabularyFilterResponse_vocabularyFilterName :: Lens.Lens' CreateVocabularyFilterResponse (Core.Maybe Core.Text)
createVocabularyFilterResponse_vocabularyFilterName = Lens.lens (\CreateVocabularyFilterResponse' {vocabularyFilterName} -> vocabularyFilterName) (\s@CreateVocabularyFilterResponse' {} a -> s {vocabularyFilterName = a} :: CreateVocabularyFilterResponse)

-- | The date and time that the vocabulary filter was modified.
createVocabularyFilterResponse_lastModifiedTime :: Lens.Lens' CreateVocabularyFilterResponse (Core.Maybe Core.UTCTime)
createVocabularyFilterResponse_lastModifiedTime = Lens.lens (\CreateVocabularyFilterResponse' {lastModifiedTime} -> lastModifiedTime) (\s@CreateVocabularyFilterResponse' {} a -> s {lastModifiedTime = a} :: CreateVocabularyFilterResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
createVocabularyFilterResponse_httpStatus :: Lens.Lens' CreateVocabularyFilterResponse Core.Int
createVocabularyFilterResponse_httpStatus = Lens.lens (\CreateVocabularyFilterResponse' {httpStatus} -> httpStatus) (\s@CreateVocabularyFilterResponse' {} a -> s {httpStatus = a} :: CreateVocabularyFilterResponse)

instance Core.NFData CreateVocabularyFilterResponse
