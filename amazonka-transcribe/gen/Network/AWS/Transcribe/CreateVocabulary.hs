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
-- Module      : Network.AWS.Transcribe.CreateVocabulary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom vocabulary that you can use to change the way
-- Amazon Transcribe handles transcription of an audio file.
module Network.AWS.Transcribe.CreateVocabulary
  ( -- * Creating a Request
    CreateVocabulary (..),
    newCreateVocabulary,

    -- * Request Lenses
    createVocabulary_phrases,
    createVocabulary_vocabularyFileUri,
    createVocabulary_vocabularyName,
    createVocabulary_languageCode,

    -- * Destructuring the Response
    CreateVocabularyResponse (..),
    newCreateVocabularyResponse,

    -- * Response Lenses
    createVocabularyResponse_languageCode,
    createVocabularyResponse_failureReason,
    createVocabularyResponse_lastModifiedTime,
    createVocabularyResponse_vocabularyState,
    createVocabularyResponse_vocabularyName,
    createVocabularyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newCreateVocabulary' smart constructor.
data CreateVocabulary = CreateVocabulary'
  { -- | An array of strings that contains the vocabulary entries.
    phrases :: Core.Maybe [Core.Text],
    -- | The S3 location of the text file that contains the definition of the
    -- custom vocabulary. The URI must be in the same region as the API
    -- endpoint that you are calling. The general form is
    --
    -- For more information about S3 object names, see
    -- <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
    -- in the /Amazon S3 Developer Guide/.
    --
    -- For more information about custom vocabularies, see
    -- <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Custom Vocabularies>.
    vocabularyFileUri :: Core.Maybe Core.Text,
    -- | The name of the vocabulary. The name must be unique within an AWS
    -- account. The name is case sensitive. If you try to create a vocabulary
    -- with the same name as a previous vocabulary you will receive a
    -- @ConflictException@ error.
    vocabularyName :: Core.Text,
    -- | The language code of the vocabulary entries.
    languageCode :: LanguageCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateVocabulary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phrases', 'createVocabulary_phrases' - An array of strings that contains the vocabulary entries.
--
-- 'vocabularyFileUri', 'createVocabulary_vocabularyFileUri' - The S3 location of the text file that contains the definition of the
-- custom vocabulary. The URI must be in the same region as the API
-- endpoint that you are calling. The general form is
--
-- For more information about S3 object names, see
-- <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
-- in the /Amazon S3 Developer Guide/.
--
-- For more information about custom vocabularies, see
-- <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Custom Vocabularies>.
--
-- 'vocabularyName', 'createVocabulary_vocabularyName' - The name of the vocabulary. The name must be unique within an AWS
-- account. The name is case sensitive. If you try to create a vocabulary
-- with the same name as a previous vocabulary you will receive a
-- @ConflictException@ error.
--
-- 'languageCode', 'createVocabulary_languageCode' - The language code of the vocabulary entries.
newCreateVocabulary ::
  -- | 'vocabularyName'
  Core.Text ->
  -- | 'languageCode'
  LanguageCode ->
  CreateVocabulary
newCreateVocabulary pVocabularyName_ pLanguageCode_ =
  CreateVocabulary'
    { phrases = Core.Nothing,
      vocabularyFileUri = Core.Nothing,
      vocabularyName = pVocabularyName_,
      languageCode = pLanguageCode_
    }

-- | An array of strings that contains the vocabulary entries.
createVocabulary_phrases :: Lens.Lens' CreateVocabulary (Core.Maybe [Core.Text])
createVocabulary_phrases = Lens.lens (\CreateVocabulary' {phrases} -> phrases) (\s@CreateVocabulary' {} a -> s {phrases = a} :: CreateVocabulary) Core.. Lens.mapping Lens._Coerce

-- | The S3 location of the text file that contains the definition of the
-- custom vocabulary. The URI must be in the same region as the API
-- endpoint that you are calling. The general form is
--
-- For more information about S3 object names, see
-- <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
-- in the /Amazon S3 Developer Guide/.
--
-- For more information about custom vocabularies, see
-- <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Custom Vocabularies>.
createVocabulary_vocabularyFileUri :: Lens.Lens' CreateVocabulary (Core.Maybe Core.Text)
createVocabulary_vocabularyFileUri = Lens.lens (\CreateVocabulary' {vocabularyFileUri} -> vocabularyFileUri) (\s@CreateVocabulary' {} a -> s {vocabularyFileUri = a} :: CreateVocabulary)

-- | The name of the vocabulary. The name must be unique within an AWS
-- account. The name is case sensitive. If you try to create a vocabulary
-- with the same name as a previous vocabulary you will receive a
-- @ConflictException@ error.
createVocabulary_vocabularyName :: Lens.Lens' CreateVocabulary Core.Text
createVocabulary_vocabularyName = Lens.lens (\CreateVocabulary' {vocabularyName} -> vocabularyName) (\s@CreateVocabulary' {} a -> s {vocabularyName = a} :: CreateVocabulary)

-- | The language code of the vocabulary entries.
createVocabulary_languageCode :: Lens.Lens' CreateVocabulary LanguageCode
createVocabulary_languageCode = Lens.lens (\CreateVocabulary' {languageCode} -> languageCode) (\s@CreateVocabulary' {} a -> s {languageCode = a} :: CreateVocabulary)

instance Core.AWSRequest CreateVocabulary where
  type
    AWSResponse CreateVocabulary =
      CreateVocabularyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVocabularyResponse'
            Core.<$> (x Core..?> "LanguageCode")
            Core.<*> (x Core..?> "FailureReason")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "VocabularyState")
            Core.<*> (x Core..?> "VocabularyName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateVocabulary

instance Core.NFData CreateVocabulary

instance Core.ToHeaders CreateVocabulary where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Transcribe.CreateVocabulary" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateVocabulary where
  toJSON CreateVocabulary' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Phrases" Core..=) Core.<$> phrases,
            ("VocabularyFileUri" Core..=)
              Core.<$> vocabularyFileUri,
            Core.Just ("VocabularyName" Core..= vocabularyName),
            Core.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.ToPath CreateVocabulary where
  toPath = Core.const "/"

instance Core.ToQuery CreateVocabulary where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateVocabularyResponse' smart constructor.
data CreateVocabularyResponse = CreateVocabularyResponse'
  { -- | The language code of the vocabulary entries.
    languageCode :: Core.Maybe LanguageCode,
    -- | If the @VocabularyState@ field is @FAILED@, this field contains
    -- information about why the job failed.
    failureReason :: Core.Maybe Core.Text,
    -- | The date and time that the vocabulary was created.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The processing state of the vocabulary. When the @VocabularyState@ field
    -- contains @READY@ the vocabulary is ready to be used in a
    -- @StartTranscriptionJob@ request.
    vocabularyState :: Core.Maybe VocabularyState,
    -- | The name of the vocabulary.
    vocabularyName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateVocabularyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'createVocabularyResponse_languageCode' - The language code of the vocabulary entries.
--
-- 'failureReason', 'createVocabularyResponse_failureReason' - If the @VocabularyState@ field is @FAILED@, this field contains
-- information about why the job failed.
--
-- 'lastModifiedTime', 'createVocabularyResponse_lastModifiedTime' - The date and time that the vocabulary was created.
--
-- 'vocabularyState', 'createVocabularyResponse_vocabularyState' - The processing state of the vocabulary. When the @VocabularyState@ field
-- contains @READY@ the vocabulary is ready to be used in a
-- @StartTranscriptionJob@ request.
--
-- 'vocabularyName', 'createVocabularyResponse_vocabularyName' - The name of the vocabulary.
--
-- 'httpStatus', 'createVocabularyResponse_httpStatus' - The response's http status code.
newCreateVocabularyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateVocabularyResponse
newCreateVocabularyResponse pHttpStatus_ =
  CreateVocabularyResponse'
    { languageCode =
        Core.Nothing,
      failureReason = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      vocabularyState = Core.Nothing,
      vocabularyName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The language code of the vocabulary entries.
createVocabularyResponse_languageCode :: Lens.Lens' CreateVocabularyResponse (Core.Maybe LanguageCode)
createVocabularyResponse_languageCode = Lens.lens (\CreateVocabularyResponse' {languageCode} -> languageCode) (\s@CreateVocabularyResponse' {} a -> s {languageCode = a} :: CreateVocabularyResponse)

-- | If the @VocabularyState@ field is @FAILED@, this field contains
-- information about why the job failed.
createVocabularyResponse_failureReason :: Lens.Lens' CreateVocabularyResponse (Core.Maybe Core.Text)
createVocabularyResponse_failureReason = Lens.lens (\CreateVocabularyResponse' {failureReason} -> failureReason) (\s@CreateVocabularyResponse' {} a -> s {failureReason = a} :: CreateVocabularyResponse)

-- | The date and time that the vocabulary was created.
createVocabularyResponse_lastModifiedTime :: Lens.Lens' CreateVocabularyResponse (Core.Maybe Core.UTCTime)
createVocabularyResponse_lastModifiedTime = Lens.lens (\CreateVocabularyResponse' {lastModifiedTime} -> lastModifiedTime) (\s@CreateVocabularyResponse' {} a -> s {lastModifiedTime = a} :: CreateVocabularyResponse) Core.. Lens.mapping Core._Time

-- | The processing state of the vocabulary. When the @VocabularyState@ field
-- contains @READY@ the vocabulary is ready to be used in a
-- @StartTranscriptionJob@ request.
createVocabularyResponse_vocabularyState :: Lens.Lens' CreateVocabularyResponse (Core.Maybe VocabularyState)
createVocabularyResponse_vocabularyState = Lens.lens (\CreateVocabularyResponse' {vocabularyState} -> vocabularyState) (\s@CreateVocabularyResponse' {} a -> s {vocabularyState = a} :: CreateVocabularyResponse)

-- | The name of the vocabulary.
createVocabularyResponse_vocabularyName :: Lens.Lens' CreateVocabularyResponse (Core.Maybe Core.Text)
createVocabularyResponse_vocabularyName = Lens.lens (\CreateVocabularyResponse' {vocabularyName} -> vocabularyName) (\s@CreateVocabularyResponse' {} a -> s {vocabularyName = a} :: CreateVocabularyResponse)

-- | The response's http status code.
createVocabularyResponse_httpStatus :: Lens.Lens' CreateVocabularyResponse Core.Int
createVocabularyResponse_httpStatus = Lens.lens (\CreateVocabularyResponse' {httpStatus} -> httpStatus) (\s@CreateVocabularyResponse' {} a -> s {httpStatus = a} :: CreateVocabularyResponse)

instance Core.NFData CreateVocabularyResponse
