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
-- Module      : Network.AWS.Transcribe.UpdateVocabularyFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a vocabulary filter with a new list of filtered words.
module Network.AWS.Transcribe.UpdateVocabularyFilter
  ( -- * Creating a Request
    UpdateVocabularyFilter (..),
    newUpdateVocabularyFilter,

    -- * Request Lenses
    updateVocabularyFilter_vocabularyFilterFileUri,
    updateVocabularyFilter_words,
    updateVocabularyFilter_vocabularyFilterName,

    -- * Destructuring the Response
    UpdateVocabularyFilterResponse (..),
    newUpdateVocabularyFilterResponse,

    -- * Response Lenses
    updateVocabularyFilterResponse_languageCode,
    updateVocabularyFilterResponse_vocabularyFilterName,
    updateVocabularyFilterResponse_lastModifiedTime,
    updateVocabularyFilterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newUpdateVocabularyFilter' smart constructor.
data UpdateVocabularyFilter = UpdateVocabularyFilter'
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
    -- | The name of the vocabulary filter to update. If you try to update a
    -- vocabulary filter with the same name as another vocabulary filter, you
    -- get a @ConflictException@ error.
    vocabularyFilterName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateVocabularyFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vocabularyFilterFileUri', 'updateVocabularyFilter_vocabularyFilterFileUri' - The Amazon S3 location of a text file used as input to create the
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
-- 'words', 'updateVocabularyFilter_words' - The words to use in the vocabulary filter. Only use characters from the
-- character set defined for custom vocabularies. For a list of character
-- sets, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies>.
--
-- If you provide a list of words in the @Words@ parameter, you can\'t use
-- the @VocabularyFilterFileUri@ parameter.
--
-- 'vocabularyFilterName', 'updateVocabularyFilter_vocabularyFilterName' - The name of the vocabulary filter to update. If you try to update a
-- vocabulary filter with the same name as another vocabulary filter, you
-- get a @ConflictException@ error.
newUpdateVocabularyFilter ::
  -- | 'vocabularyFilterName'
  Core.Text ->
  UpdateVocabularyFilter
newUpdateVocabularyFilter pVocabularyFilterName_ =
  UpdateVocabularyFilter'
    { vocabularyFilterFileUri =
        Core.Nothing,
      words = Core.Nothing,
      vocabularyFilterName = pVocabularyFilterName_
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
updateVocabularyFilter_vocabularyFilterFileUri :: Lens.Lens' UpdateVocabularyFilter (Core.Maybe Core.Text)
updateVocabularyFilter_vocabularyFilterFileUri = Lens.lens (\UpdateVocabularyFilter' {vocabularyFilterFileUri} -> vocabularyFilterFileUri) (\s@UpdateVocabularyFilter' {} a -> s {vocabularyFilterFileUri = a} :: UpdateVocabularyFilter)

-- | The words to use in the vocabulary filter. Only use characters from the
-- character set defined for custom vocabularies. For a list of character
-- sets, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies>.
--
-- If you provide a list of words in the @Words@ parameter, you can\'t use
-- the @VocabularyFilterFileUri@ parameter.
updateVocabularyFilter_words :: Lens.Lens' UpdateVocabularyFilter (Core.Maybe (Core.NonEmpty Core.Text))
updateVocabularyFilter_words = Lens.lens (\UpdateVocabularyFilter' {words} -> words) (\s@UpdateVocabularyFilter' {} a -> s {words = a} :: UpdateVocabularyFilter) Core.. Lens.mapping Lens._Coerce

-- | The name of the vocabulary filter to update. If you try to update a
-- vocabulary filter with the same name as another vocabulary filter, you
-- get a @ConflictException@ error.
updateVocabularyFilter_vocabularyFilterName :: Lens.Lens' UpdateVocabularyFilter Core.Text
updateVocabularyFilter_vocabularyFilterName = Lens.lens (\UpdateVocabularyFilter' {vocabularyFilterName} -> vocabularyFilterName) (\s@UpdateVocabularyFilter' {} a -> s {vocabularyFilterName = a} :: UpdateVocabularyFilter)

instance Core.AWSRequest UpdateVocabularyFilter where
  type
    AWSResponse UpdateVocabularyFilter =
      UpdateVocabularyFilterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVocabularyFilterResponse'
            Core.<$> (x Core..?> "LanguageCode")
            Core.<*> (x Core..?> "VocabularyFilterName")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateVocabularyFilter

instance Core.NFData UpdateVocabularyFilter

instance Core.ToHeaders UpdateVocabularyFilter where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.UpdateVocabularyFilter" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateVocabularyFilter where
  toJSON UpdateVocabularyFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("VocabularyFilterFileUri" Core..=)
              Core.<$> vocabularyFilterFileUri,
            ("Words" Core..=) Core.<$> words,
            Core.Just
              ( "VocabularyFilterName"
                  Core..= vocabularyFilterName
              )
          ]
      )

instance Core.ToPath UpdateVocabularyFilter where
  toPath = Core.const "/"

instance Core.ToQuery UpdateVocabularyFilter where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateVocabularyFilterResponse' smart constructor.
data UpdateVocabularyFilterResponse = UpdateVocabularyFilterResponse'
  { -- | The language code of the words in the vocabulary filter.
    languageCode :: Core.Maybe LanguageCode,
    -- | The name of the updated vocabulary filter.
    vocabularyFilterName :: Core.Maybe Core.Text,
    -- | The date and time that the vocabulary filter was updated.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateVocabularyFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'updateVocabularyFilterResponse_languageCode' - The language code of the words in the vocabulary filter.
--
-- 'vocabularyFilterName', 'updateVocabularyFilterResponse_vocabularyFilterName' - The name of the updated vocabulary filter.
--
-- 'lastModifiedTime', 'updateVocabularyFilterResponse_lastModifiedTime' - The date and time that the vocabulary filter was updated.
--
-- 'httpStatus', 'updateVocabularyFilterResponse_httpStatus' - The response's http status code.
newUpdateVocabularyFilterResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateVocabularyFilterResponse
newUpdateVocabularyFilterResponse pHttpStatus_ =
  UpdateVocabularyFilterResponse'
    { languageCode =
        Core.Nothing,
      vocabularyFilterName = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The language code of the words in the vocabulary filter.
updateVocabularyFilterResponse_languageCode :: Lens.Lens' UpdateVocabularyFilterResponse (Core.Maybe LanguageCode)
updateVocabularyFilterResponse_languageCode = Lens.lens (\UpdateVocabularyFilterResponse' {languageCode} -> languageCode) (\s@UpdateVocabularyFilterResponse' {} a -> s {languageCode = a} :: UpdateVocabularyFilterResponse)

-- | The name of the updated vocabulary filter.
updateVocabularyFilterResponse_vocabularyFilterName :: Lens.Lens' UpdateVocabularyFilterResponse (Core.Maybe Core.Text)
updateVocabularyFilterResponse_vocabularyFilterName = Lens.lens (\UpdateVocabularyFilterResponse' {vocabularyFilterName} -> vocabularyFilterName) (\s@UpdateVocabularyFilterResponse' {} a -> s {vocabularyFilterName = a} :: UpdateVocabularyFilterResponse)

-- | The date and time that the vocabulary filter was updated.
updateVocabularyFilterResponse_lastModifiedTime :: Lens.Lens' UpdateVocabularyFilterResponse (Core.Maybe Core.UTCTime)
updateVocabularyFilterResponse_lastModifiedTime = Lens.lens (\UpdateVocabularyFilterResponse' {lastModifiedTime} -> lastModifiedTime) (\s@UpdateVocabularyFilterResponse' {} a -> s {lastModifiedTime = a} :: UpdateVocabularyFilterResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
updateVocabularyFilterResponse_httpStatus :: Lens.Lens' UpdateVocabularyFilterResponse Core.Int
updateVocabularyFilterResponse_httpStatus = Lens.lens (\UpdateVocabularyFilterResponse' {httpStatus} -> httpStatus) (\s@UpdateVocabularyFilterResponse' {} a -> s {httpStatus = a} :: UpdateVocabularyFilterResponse)

instance Core.NFData UpdateVocabularyFilterResponse
