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
-- Module      : Amazonka.Transcribe.UpdateVocabulary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing vocabulary with new values. The @UpdateVocabulary@
-- operation overwrites all of the existing information with the values
-- that you provide in the request.
module Amazonka.Transcribe.UpdateVocabulary
  ( -- * Creating a Request
    UpdateVocabulary (..),
    newUpdateVocabulary,

    -- * Request Lenses
    updateVocabulary_vocabularyFileUri,
    updateVocabulary_phrases,
    updateVocabulary_vocabularyName,
    updateVocabulary_languageCode,

    -- * Destructuring the Response
    UpdateVocabularyResponse (..),
    newUpdateVocabularyResponse,

    -- * Response Lenses
    updateVocabularyResponse_languageCode,
    updateVocabularyResponse_vocabularyName,
    updateVocabularyResponse_lastModifiedTime,
    updateVocabularyResponse_vocabularyState,
    updateVocabularyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newUpdateVocabulary' smart constructor.
data UpdateVocabulary = UpdateVocabulary'
  { -- | The S3 location of the text file that contains the definition of the
    -- custom vocabulary. The URI must be in the same region as the API
    -- endpoint that you are calling. The general form is
    --
    -- For example:
    --
    -- For more information about S3 object names, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
    -- in the /Amazon S3 Developer Guide/.
    --
    -- For more information about custom vocabularies, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Custom Vocabularies>.
    vocabularyFileUri :: Prelude.Maybe Prelude.Text,
    -- | An array of strings containing the vocabulary entries.
    phrases :: Prelude.Maybe [Prelude.Text],
    -- | The name of the vocabulary to update. The name is case sensitive. If you
    -- try to update a vocabulary with the same name as a previous vocabulary
    -- you will receive a @ConflictException@ error.
    vocabularyName :: Prelude.Text,
    -- | The language code of the vocabulary entries. For a list of languages and
    -- their corresponding language codes, see transcribe-whatis.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVocabulary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vocabularyFileUri', 'updateVocabulary_vocabularyFileUri' - The S3 location of the text file that contains the definition of the
-- custom vocabulary. The URI must be in the same region as the API
-- endpoint that you are calling. The general form is
--
-- For example:
--
-- For more information about S3 object names, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
-- in the /Amazon S3 Developer Guide/.
--
-- For more information about custom vocabularies, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Custom Vocabularies>.
--
-- 'phrases', 'updateVocabulary_phrases' - An array of strings containing the vocabulary entries.
--
-- 'vocabularyName', 'updateVocabulary_vocabularyName' - The name of the vocabulary to update. The name is case sensitive. If you
-- try to update a vocabulary with the same name as a previous vocabulary
-- you will receive a @ConflictException@ error.
--
-- 'languageCode', 'updateVocabulary_languageCode' - The language code of the vocabulary entries. For a list of languages and
-- their corresponding language codes, see transcribe-whatis.
newUpdateVocabulary ::
  -- | 'vocabularyName'
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  UpdateVocabulary
newUpdateVocabulary pVocabularyName_ pLanguageCode_ =
  UpdateVocabulary'
    { vocabularyFileUri =
        Prelude.Nothing,
      phrases = Prelude.Nothing,
      vocabularyName = pVocabularyName_,
      languageCode = pLanguageCode_
    }

-- | The S3 location of the text file that contains the definition of the
-- custom vocabulary. The URI must be in the same region as the API
-- endpoint that you are calling. The general form is
--
-- For example:
--
-- For more information about S3 object names, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
-- in the /Amazon S3 Developer Guide/.
--
-- For more information about custom vocabularies, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Custom Vocabularies>.
updateVocabulary_vocabularyFileUri :: Lens.Lens' UpdateVocabulary (Prelude.Maybe Prelude.Text)
updateVocabulary_vocabularyFileUri = Lens.lens (\UpdateVocabulary' {vocabularyFileUri} -> vocabularyFileUri) (\s@UpdateVocabulary' {} a -> s {vocabularyFileUri = a} :: UpdateVocabulary)

-- | An array of strings containing the vocabulary entries.
updateVocabulary_phrases :: Lens.Lens' UpdateVocabulary (Prelude.Maybe [Prelude.Text])
updateVocabulary_phrases = Lens.lens (\UpdateVocabulary' {phrases} -> phrases) (\s@UpdateVocabulary' {} a -> s {phrases = a} :: UpdateVocabulary) Prelude.. Lens.mapping Lens.coerced

-- | The name of the vocabulary to update. The name is case sensitive. If you
-- try to update a vocabulary with the same name as a previous vocabulary
-- you will receive a @ConflictException@ error.
updateVocabulary_vocabularyName :: Lens.Lens' UpdateVocabulary Prelude.Text
updateVocabulary_vocabularyName = Lens.lens (\UpdateVocabulary' {vocabularyName} -> vocabularyName) (\s@UpdateVocabulary' {} a -> s {vocabularyName = a} :: UpdateVocabulary)

-- | The language code of the vocabulary entries. For a list of languages and
-- their corresponding language codes, see transcribe-whatis.
updateVocabulary_languageCode :: Lens.Lens' UpdateVocabulary LanguageCode
updateVocabulary_languageCode = Lens.lens (\UpdateVocabulary' {languageCode} -> languageCode) (\s@UpdateVocabulary' {} a -> s {languageCode = a} :: UpdateVocabulary)

instance Core.AWSRequest UpdateVocabulary where
  type
    AWSResponse UpdateVocabulary =
      UpdateVocabularyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVocabularyResponse'
            Prelude.<$> (x Core..?> "LanguageCode")
            Prelude.<*> (x Core..?> "VocabularyName")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "VocabularyState")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateVocabulary

instance Prelude.NFData UpdateVocabulary

instance Core.ToHeaders UpdateVocabulary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.UpdateVocabulary" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateVocabulary where
  toJSON UpdateVocabulary' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VocabularyFileUri" Core..=)
              Prelude.<$> vocabularyFileUri,
            ("Phrases" Core..=) Prelude.<$> phrases,
            Prelude.Just
              ("VocabularyName" Core..= vocabularyName),
            Prelude.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.ToPath UpdateVocabulary where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateVocabulary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateVocabularyResponse' smart constructor.
data UpdateVocabularyResponse = UpdateVocabularyResponse'
  { -- | The language code of the vocabulary entries.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The name of the vocabulary that was updated.
    vocabularyName :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the vocabulary was updated.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The processing state of the vocabulary. When the @VocabularyState@ field
    -- contains @READY@ the vocabulary is ready to be used in a
    -- @StartTranscriptionJob@ request.
    vocabularyState :: Prelude.Maybe VocabularyState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVocabularyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'updateVocabularyResponse_languageCode' - The language code of the vocabulary entries.
--
-- 'vocabularyName', 'updateVocabularyResponse_vocabularyName' - The name of the vocabulary that was updated.
--
-- 'lastModifiedTime', 'updateVocabularyResponse_lastModifiedTime' - The date and time that the vocabulary was updated.
--
-- 'vocabularyState', 'updateVocabularyResponse_vocabularyState' - The processing state of the vocabulary. When the @VocabularyState@ field
-- contains @READY@ the vocabulary is ready to be used in a
-- @StartTranscriptionJob@ request.
--
-- 'httpStatus', 'updateVocabularyResponse_httpStatus' - The response's http status code.
newUpdateVocabularyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateVocabularyResponse
newUpdateVocabularyResponse pHttpStatus_ =
  UpdateVocabularyResponse'
    { languageCode =
        Prelude.Nothing,
      vocabularyName = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      vocabularyState = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The language code of the vocabulary entries.
updateVocabularyResponse_languageCode :: Lens.Lens' UpdateVocabularyResponse (Prelude.Maybe LanguageCode)
updateVocabularyResponse_languageCode = Lens.lens (\UpdateVocabularyResponse' {languageCode} -> languageCode) (\s@UpdateVocabularyResponse' {} a -> s {languageCode = a} :: UpdateVocabularyResponse)

-- | The name of the vocabulary that was updated.
updateVocabularyResponse_vocabularyName :: Lens.Lens' UpdateVocabularyResponse (Prelude.Maybe Prelude.Text)
updateVocabularyResponse_vocabularyName = Lens.lens (\UpdateVocabularyResponse' {vocabularyName} -> vocabularyName) (\s@UpdateVocabularyResponse' {} a -> s {vocabularyName = a} :: UpdateVocabularyResponse)

-- | The date and time that the vocabulary was updated.
updateVocabularyResponse_lastModifiedTime :: Lens.Lens' UpdateVocabularyResponse (Prelude.Maybe Prelude.UTCTime)
updateVocabularyResponse_lastModifiedTime = Lens.lens (\UpdateVocabularyResponse' {lastModifiedTime} -> lastModifiedTime) (\s@UpdateVocabularyResponse' {} a -> s {lastModifiedTime = a} :: UpdateVocabularyResponse) Prelude.. Lens.mapping Core._Time

-- | The processing state of the vocabulary. When the @VocabularyState@ field
-- contains @READY@ the vocabulary is ready to be used in a
-- @StartTranscriptionJob@ request.
updateVocabularyResponse_vocabularyState :: Lens.Lens' UpdateVocabularyResponse (Prelude.Maybe VocabularyState)
updateVocabularyResponse_vocabularyState = Lens.lens (\UpdateVocabularyResponse' {vocabularyState} -> vocabularyState) (\s@UpdateVocabularyResponse' {} a -> s {vocabularyState = a} :: UpdateVocabularyResponse)

-- | The response's http status code.
updateVocabularyResponse_httpStatus :: Lens.Lens' UpdateVocabularyResponse Prelude.Int
updateVocabularyResponse_httpStatus = Lens.lens (\UpdateVocabularyResponse' {httpStatus} -> httpStatus) (\s@UpdateVocabularyResponse' {} a -> s {httpStatus = a} :: UpdateVocabularyResponse)

instance Prelude.NFData UpdateVocabularyResponse
