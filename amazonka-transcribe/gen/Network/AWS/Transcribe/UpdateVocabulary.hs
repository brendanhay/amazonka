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
-- Module      : Network.AWS.Transcribe.UpdateVocabulary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing vocabulary with new values. The @UpdateVocabulary@
-- operation overwrites all of the existing information with the values
-- that you provide in the request.
module Network.AWS.Transcribe.UpdateVocabulary
  ( -- * Creating a Request
    UpdateVocabulary (..),
    newUpdateVocabulary,

    -- * Request Lenses
    updateVocabulary_phrases,
    updateVocabulary_vocabularyFileUri,
    updateVocabulary_vocabularyName,
    updateVocabulary_languageCode,

    -- * Destructuring the Response
    UpdateVocabularyResponse (..),
    newUpdateVocabularyResponse,

    -- * Response Lenses
    updateVocabularyResponse_languageCode,
    updateVocabularyResponse_lastModifiedTime,
    updateVocabularyResponse_vocabularyState,
    updateVocabularyResponse_vocabularyName,
    updateVocabularyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newUpdateVocabulary' smart constructor.
data UpdateVocabulary = UpdateVocabulary'
  { -- | An array of strings containing the vocabulary entries.
    phrases :: Core.Maybe [Core.Text],
    -- | The S3 location of the text file that contains the definition of the
    -- custom vocabulary. The URI must be in the same region as the API
    -- endpoint that you are calling. The general form is
    --
    -- For example:
    --
    -- For more information about S3 object names, see
    -- <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
    -- in the /Amazon S3 Developer Guide/.
    --
    -- For more information about custom vocabularies, see
    -- <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Custom Vocabularies>.
    vocabularyFileUri :: Core.Maybe Core.Text,
    -- | The name of the vocabulary to update. The name is case sensitive. If you
    -- try to update a vocabulary with the same name as a previous vocabulary
    -- you will receive a @ConflictException@ error.
    vocabularyName :: Core.Text,
    -- | The language code of the vocabulary entries.
    languageCode :: LanguageCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateVocabulary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phrases', 'updateVocabulary_phrases' - An array of strings containing the vocabulary entries.
--
-- 'vocabularyFileUri', 'updateVocabulary_vocabularyFileUri' - The S3 location of the text file that contains the definition of the
-- custom vocabulary. The URI must be in the same region as the API
-- endpoint that you are calling. The general form is
--
-- For example:
--
-- For more information about S3 object names, see
-- <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
-- in the /Amazon S3 Developer Guide/.
--
-- For more information about custom vocabularies, see
-- <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Custom Vocabularies>.
--
-- 'vocabularyName', 'updateVocabulary_vocabularyName' - The name of the vocabulary to update. The name is case sensitive. If you
-- try to update a vocabulary with the same name as a previous vocabulary
-- you will receive a @ConflictException@ error.
--
-- 'languageCode', 'updateVocabulary_languageCode' - The language code of the vocabulary entries.
newUpdateVocabulary ::
  -- | 'vocabularyName'
  Core.Text ->
  -- | 'languageCode'
  LanguageCode ->
  UpdateVocabulary
newUpdateVocabulary pVocabularyName_ pLanguageCode_ =
  UpdateVocabulary'
    { phrases = Core.Nothing,
      vocabularyFileUri = Core.Nothing,
      vocabularyName = pVocabularyName_,
      languageCode = pLanguageCode_
    }

-- | An array of strings containing the vocabulary entries.
updateVocabulary_phrases :: Lens.Lens' UpdateVocabulary (Core.Maybe [Core.Text])
updateVocabulary_phrases = Lens.lens (\UpdateVocabulary' {phrases} -> phrases) (\s@UpdateVocabulary' {} a -> s {phrases = a} :: UpdateVocabulary) Core.. Lens.mapping Lens._Coerce

-- | The S3 location of the text file that contains the definition of the
-- custom vocabulary. The URI must be in the same region as the API
-- endpoint that you are calling. The general form is
--
-- For example:
--
-- For more information about S3 object names, see
-- <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
-- in the /Amazon S3 Developer Guide/.
--
-- For more information about custom vocabularies, see
-- <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Custom Vocabularies>.
updateVocabulary_vocabularyFileUri :: Lens.Lens' UpdateVocabulary (Core.Maybe Core.Text)
updateVocabulary_vocabularyFileUri = Lens.lens (\UpdateVocabulary' {vocabularyFileUri} -> vocabularyFileUri) (\s@UpdateVocabulary' {} a -> s {vocabularyFileUri = a} :: UpdateVocabulary)

-- | The name of the vocabulary to update. The name is case sensitive. If you
-- try to update a vocabulary with the same name as a previous vocabulary
-- you will receive a @ConflictException@ error.
updateVocabulary_vocabularyName :: Lens.Lens' UpdateVocabulary Core.Text
updateVocabulary_vocabularyName = Lens.lens (\UpdateVocabulary' {vocabularyName} -> vocabularyName) (\s@UpdateVocabulary' {} a -> s {vocabularyName = a} :: UpdateVocabulary)

-- | The language code of the vocabulary entries.
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
            Core.<$> (x Core..?> "LanguageCode")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "VocabularyState")
            Core.<*> (x Core..?> "VocabularyName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateVocabulary

instance Core.NFData UpdateVocabulary

instance Core.ToHeaders UpdateVocabulary where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Transcribe.UpdateVocabulary" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateVocabulary where
  toJSON UpdateVocabulary' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Phrases" Core..=) Core.<$> phrases,
            ("VocabularyFileUri" Core..=)
              Core.<$> vocabularyFileUri,
            Core.Just ("VocabularyName" Core..= vocabularyName),
            Core.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.ToPath UpdateVocabulary where
  toPath = Core.const "/"

instance Core.ToQuery UpdateVocabulary where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateVocabularyResponse' smart constructor.
data UpdateVocabularyResponse = UpdateVocabularyResponse'
  { -- | The language code of the vocabulary entries.
    languageCode :: Core.Maybe LanguageCode,
    -- | The date and time that the vocabulary was updated.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The processing state of the vocabulary. When the @VocabularyState@ field
    -- contains @READY@ the vocabulary is ready to be used in a
    -- @StartTranscriptionJob@ request.
    vocabularyState :: Core.Maybe VocabularyState,
    -- | The name of the vocabulary that was updated.
    vocabularyName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'lastModifiedTime', 'updateVocabularyResponse_lastModifiedTime' - The date and time that the vocabulary was updated.
--
-- 'vocabularyState', 'updateVocabularyResponse_vocabularyState' - The processing state of the vocabulary. When the @VocabularyState@ field
-- contains @READY@ the vocabulary is ready to be used in a
-- @StartTranscriptionJob@ request.
--
-- 'vocabularyName', 'updateVocabularyResponse_vocabularyName' - The name of the vocabulary that was updated.
--
-- 'httpStatus', 'updateVocabularyResponse_httpStatus' - The response's http status code.
newUpdateVocabularyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateVocabularyResponse
newUpdateVocabularyResponse pHttpStatus_ =
  UpdateVocabularyResponse'
    { languageCode =
        Core.Nothing,
      lastModifiedTime = Core.Nothing,
      vocabularyState = Core.Nothing,
      vocabularyName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The language code of the vocabulary entries.
updateVocabularyResponse_languageCode :: Lens.Lens' UpdateVocabularyResponse (Core.Maybe LanguageCode)
updateVocabularyResponse_languageCode = Lens.lens (\UpdateVocabularyResponse' {languageCode} -> languageCode) (\s@UpdateVocabularyResponse' {} a -> s {languageCode = a} :: UpdateVocabularyResponse)

-- | The date and time that the vocabulary was updated.
updateVocabularyResponse_lastModifiedTime :: Lens.Lens' UpdateVocabularyResponse (Core.Maybe Core.UTCTime)
updateVocabularyResponse_lastModifiedTime = Lens.lens (\UpdateVocabularyResponse' {lastModifiedTime} -> lastModifiedTime) (\s@UpdateVocabularyResponse' {} a -> s {lastModifiedTime = a} :: UpdateVocabularyResponse) Core.. Lens.mapping Core._Time

-- | The processing state of the vocabulary. When the @VocabularyState@ field
-- contains @READY@ the vocabulary is ready to be used in a
-- @StartTranscriptionJob@ request.
updateVocabularyResponse_vocabularyState :: Lens.Lens' UpdateVocabularyResponse (Core.Maybe VocabularyState)
updateVocabularyResponse_vocabularyState = Lens.lens (\UpdateVocabularyResponse' {vocabularyState} -> vocabularyState) (\s@UpdateVocabularyResponse' {} a -> s {vocabularyState = a} :: UpdateVocabularyResponse)

-- | The name of the vocabulary that was updated.
updateVocabularyResponse_vocabularyName :: Lens.Lens' UpdateVocabularyResponse (Core.Maybe Core.Text)
updateVocabularyResponse_vocabularyName = Lens.lens (\UpdateVocabularyResponse' {vocabularyName} -> vocabularyName) (\s@UpdateVocabularyResponse' {} a -> s {vocabularyName = a} :: UpdateVocabularyResponse)

-- | The response's http status code.
updateVocabularyResponse_httpStatus :: Lens.Lens' UpdateVocabularyResponse Core.Int
updateVocabularyResponse_httpStatus = Lens.lens (\UpdateVocabularyResponse' {httpStatus} -> httpStatus) (\s@UpdateVocabularyResponse' {} a -> s {httpStatus = a} :: UpdateVocabularyResponse)

instance Core.NFData UpdateVocabularyResponse
