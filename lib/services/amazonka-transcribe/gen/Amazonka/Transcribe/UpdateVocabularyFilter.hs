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
-- Module      : Amazonka.Transcribe.UpdateVocabularyFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing custom vocabulary filter with a new list of words.
-- The new list you provide overwrites all previous entries; you cannot
-- append new terms onto an existing vocabulary filter.
module Amazonka.Transcribe.UpdateVocabularyFilter
  ( -- * Creating a Request
    UpdateVocabularyFilter (..),
    newUpdateVocabularyFilter,

    -- * Request Lenses
    updateVocabularyFilter_words,
    updateVocabularyFilter_vocabularyFilterFileUri,
    updateVocabularyFilter_vocabularyFilterName,

    -- * Destructuring the Response
    UpdateVocabularyFilterResponse (..),
    newUpdateVocabularyFilterResponse,

    -- * Response Lenses
    updateVocabularyFilterResponse_lastModifiedTime,
    updateVocabularyFilterResponse_languageCode,
    updateVocabularyFilterResponse_vocabularyFilterName,
    updateVocabularyFilterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newUpdateVocabularyFilter' smart constructor.
data UpdateVocabularyFilter = UpdateVocabularyFilter'
  { -- | Use this parameter if you want to update your vocabulary filter by
    -- including all desired terms, as comma-separated values, within your
    -- request. The other option for updating your vocabulary filter is to save
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
    -- | The name of the custom vocabulary filter you want to update. Vocabulary
    -- filter names are case sensitive.
    vocabularyFilterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVocabularyFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'words', 'updateVocabularyFilter_words' - Use this parameter if you want to update your vocabulary filter by
-- including all desired terms, as comma-separated values, within your
-- request. The other option for updating your vocabulary filter is to save
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
-- 'vocabularyFilterFileUri', 'updateVocabularyFilter_vocabularyFilterFileUri' - The Amazon S3 location of the text file that contains your custom
-- vocabulary filter terms. The URI must be located in the same Amazon Web
-- Services Region as the resource you\'re calling.
--
-- Here\'s an example URI path:
-- @s3:\/\/DOC-EXAMPLE-BUCKET\/my-vocab-filter-file.txt@
--
-- Note that if you include @VocabularyFilterFileUri@ in your request, you
-- cannot use @Words@; you must choose one or the other.
--
-- 'vocabularyFilterName', 'updateVocabularyFilter_vocabularyFilterName' - The name of the custom vocabulary filter you want to update. Vocabulary
-- filter names are case sensitive.
newUpdateVocabularyFilter ::
  -- | 'vocabularyFilterName'
  Prelude.Text ->
  UpdateVocabularyFilter
newUpdateVocabularyFilter pVocabularyFilterName_ =
  UpdateVocabularyFilter'
    { words = Prelude.Nothing,
      vocabularyFilterFileUri = Prelude.Nothing,
      vocabularyFilterName = pVocabularyFilterName_
    }

-- | Use this parameter if you want to update your vocabulary filter by
-- including all desired terms, as comma-separated values, within your
-- request. The other option for updating your vocabulary filter is to save
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
updateVocabularyFilter_words :: Lens.Lens' UpdateVocabularyFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateVocabularyFilter_words = Lens.lens (\UpdateVocabularyFilter' {words} -> words) (\s@UpdateVocabularyFilter' {} a -> s {words = a} :: UpdateVocabularyFilter) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon S3 location of the text file that contains your custom
-- vocabulary filter terms. The URI must be located in the same Amazon Web
-- Services Region as the resource you\'re calling.
--
-- Here\'s an example URI path:
-- @s3:\/\/DOC-EXAMPLE-BUCKET\/my-vocab-filter-file.txt@
--
-- Note that if you include @VocabularyFilterFileUri@ in your request, you
-- cannot use @Words@; you must choose one or the other.
updateVocabularyFilter_vocabularyFilterFileUri :: Lens.Lens' UpdateVocabularyFilter (Prelude.Maybe Prelude.Text)
updateVocabularyFilter_vocabularyFilterFileUri = Lens.lens (\UpdateVocabularyFilter' {vocabularyFilterFileUri} -> vocabularyFilterFileUri) (\s@UpdateVocabularyFilter' {} a -> s {vocabularyFilterFileUri = a} :: UpdateVocabularyFilter)

-- | The name of the custom vocabulary filter you want to update. Vocabulary
-- filter names are case sensitive.
updateVocabularyFilter_vocabularyFilterName :: Lens.Lens' UpdateVocabularyFilter Prelude.Text
updateVocabularyFilter_vocabularyFilterName = Lens.lens (\UpdateVocabularyFilter' {vocabularyFilterName} -> vocabularyFilterName) (\s@UpdateVocabularyFilter' {} a -> s {vocabularyFilterName = a} :: UpdateVocabularyFilter)

instance Core.AWSRequest UpdateVocabularyFilter where
  type
    AWSResponse UpdateVocabularyFilter =
      UpdateVocabularyFilterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVocabularyFilterResponse'
            Prelude.<$> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "LanguageCode")
            Prelude.<*> (x Core..?> "VocabularyFilterName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateVocabularyFilter where
  hashWithSalt _salt UpdateVocabularyFilter' {..} =
    _salt `Prelude.hashWithSalt` words
      `Prelude.hashWithSalt` vocabularyFilterFileUri
      `Prelude.hashWithSalt` vocabularyFilterName

instance Prelude.NFData UpdateVocabularyFilter where
  rnf UpdateVocabularyFilter' {..} =
    Prelude.rnf words
      `Prelude.seq` Prelude.rnf vocabularyFilterFileUri
      `Prelude.seq` Prelude.rnf vocabularyFilterName

instance Core.ToHeaders UpdateVocabularyFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.UpdateVocabularyFilter" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateVocabularyFilter where
  toJSON UpdateVocabularyFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Words" Core..=) Prelude.<$> words,
            ("VocabularyFilterFileUri" Core..=)
              Prelude.<$> vocabularyFilterFileUri,
            Prelude.Just
              ( "VocabularyFilterName"
                  Core..= vocabularyFilterName
              )
          ]
      )

instance Core.ToPath UpdateVocabularyFilter where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateVocabularyFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateVocabularyFilterResponse' smart constructor.
data UpdateVocabularyFilterResponse = UpdateVocabularyFilterResponse'
  { -- | The date and time the specified vocabulary filter was last updated.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
    -- May 4, 2022.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The language code you selected for your vocabulary filter.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The name of the updated custom vocabulary filter.
    vocabularyFilterName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVocabularyFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTime', 'updateVocabularyFilterResponse_lastModifiedTime' - The date and time the specified vocabulary filter was last updated.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
--
-- 'languageCode', 'updateVocabularyFilterResponse_languageCode' - The language code you selected for your vocabulary filter.
--
-- 'vocabularyFilterName', 'updateVocabularyFilterResponse_vocabularyFilterName' - The name of the updated custom vocabulary filter.
--
-- 'httpStatus', 'updateVocabularyFilterResponse_httpStatus' - The response's http status code.
newUpdateVocabularyFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateVocabularyFilterResponse
newUpdateVocabularyFilterResponse pHttpStatus_ =
  UpdateVocabularyFilterResponse'
    { lastModifiedTime =
        Prelude.Nothing,
      languageCode = Prelude.Nothing,
      vocabularyFilterName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time the specified vocabulary filter was last updated.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
updateVocabularyFilterResponse_lastModifiedTime :: Lens.Lens' UpdateVocabularyFilterResponse (Prelude.Maybe Prelude.UTCTime)
updateVocabularyFilterResponse_lastModifiedTime = Lens.lens (\UpdateVocabularyFilterResponse' {lastModifiedTime} -> lastModifiedTime) (\s@UpdateVocabularyFilterResponse' {} a -> s {lastModifiedTime = a} :: UpdateVocabularyFilterResponse) Prelude.. Lens.mapping Core._Time

-- | The language code you selected for your vocabulary filter.
updateVocabularyFilterResponse_languageCode :: Lens.Lens' UpdateVocabularyFilterResponse (Prelude.Maybe LanguageCode)
updateVocabularyFilterResponse_languageCode = Lens.lens (\UpdateVocabularyFilterResponse' {languageCode} -> languageCode) (\s@UpdateVocabularyFilterResponse' {} a -> s {languageCode = a} :: UpdateVocabularyFilterResponse)

-- | The name of the updated custom vocabulary filter.
updateVocabularyFilterResponse_vocabularyFilterName :: Lens.Lens' UpdateVocabularyFilterResponse (Prelude.Maybe Prelude.Text)
updateVocabularyFilterResponse_vocabularyFilterName = Lens.lens (\UpdateVocabularyFilterResponse' {vocabularyFilterName} -> vocabularyFilterName) (\s@UpdateVocabularyFilterResponse' {} a -> s {vocabularyFilterName = a} :: UpdateVocabularyFilterResponse)

-- | The response's http status code.
updateVocabularyFilterResponse_httpStatus :: Lens.Lens' UpdateVocabularyFilterResponse Prelude.Int
updateVocabularyFilterResponse_httpStatus = Lens.lens (\UpdateVocabularyFilterResponse' {httpStatus} -> httpStatus) (\s@UpdateVocabularyFilterResponse' {} a -> s {httpStatus = a} :: UpdateVocabularyFilterResponse)

instance
  Prelude.NFData
    UpdateVocabularyFilterResponse
  where
  rnf UpdateVocabularyFilterResponse' {..} =
    Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf vocabularyFilterName
      `Prelude.seq` Prelude.rnf httpStatus
