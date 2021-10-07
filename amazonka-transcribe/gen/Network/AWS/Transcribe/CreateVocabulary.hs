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
    createVocabulary_tags,
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
    createVocabularyResponse_vocabularyName,
    createVocabularyResponse_vocabularyState,
    createVocabularyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newCreateVocabulary' smart constructor.
data CreateVocabulary = CreateVocabulary'
  { -- | An array of strings that contains the vocabulary entries.
    phrases :: Prelude.Maybe [Prelude.Text],
    -- | Adds one or more tags, each in the form of a key:value pair, to a new
    -- Amazon Transcribe vocabulary at the time you create this new vocabulary.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The S3 location of the text file that contains the definition of the
    -- custom vocabulary. The URI must be in the same region as the API
    -- endpoint that you are calling. The general form is:
    --
    -- For more information about S3 object names, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
    -- in the /Amazon S3 Developer Guide/.
    --
    -- For more information about custom vocabularies, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary Custom vocabularies>.
    vocabularyFileUri :: Prelude.Maybe Prelude.Text,
    -- | The name of the vocabulary. The name must be unique within an Amazon Web
    -- Services account. The name is case sensitive. If you try to create a
    -- vocabulary with the same name as a previous vocabulary you will receive
    -- a @ConflictException@ error.
    vocabularyName :: Prelude.Text,
    -- | The language code of the vocabulary entries. For a list of languages and
    -- their corresponding language codes, see transcribe-whatis.
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
-- 'phrases', 'createVocabulary_phrases' - An array of strings that contains the vocabulary entries.
--
-- 'tags', 'createVocabulary_tags' - Adds one or more tags, each in the form of a key:value pair, to a new
-- Amazon Transcribe vocabulary at the time you create this new vocabulary.
--
-- 'vocabularyFileUri', 'createVocabulary_vocabularyFileUri' - The S3 location of the text file that contains the definition of the
-- custom vocabulary. The URI must be in the same region as the API
-- endpoint that you are calling. The general form is:
--
-- For more information about S3 object names, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
-- in the /Amazon S3 Developer Guide/.
--
-- For more information about custom vocabularies, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary Custom vocabularies>.
--
-- 'vocabularyName', 'createVocabulary_vocabularyName' - The name of the vocabulary. The name must be unique within an Amazon Web
-- Services account. The name is case sensitive. If you try to create a
-- vocabulary with the same name as a previous vocabulary you will receive
-- a @ConflictException@ error.
--
-- 'languageCode', 'createVocabulary_languageCode' - The language code of the vocabulary entries. For a list of languages and
-- their corresponding language codes, see transcribe-whatis.
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

-- | An array of strings that contains the vocabulary entries.
createVocabulary_phrases :: Lens.Lens' CreateVocabulary (Prelude.Maybe [Prelude.Text])
createVocabulary_phrases = Lens.lens (\CreateVocabulary' {phrases} -> phrases) (\s@CreateVocabulary' {} a -> s {phrases = a} :: CreateVocabulary) Prelude.. Lens.mapping Lens._Coerce

-- | Adds one or more tags, each in the form of a key:value pair, to a new
-- Amazon Transcribe vocabulary at the time you create this new vocabulary.
createVocabulary_tags :: Lens.Lens' CreateVocabulary (Prelude.Maybe (Prelude.NonEmpty Tag))
createVocabulary_tags = Lens.lens (\CreateVocabulary' {tags} -> tags) (\s@CreateVocabulary' {} a -> s {tags = a} :: CreateVocabulary) Prelude.. Lens.mapping Lens._Coerce

-- | The S3 location of the text file that contains the definition of the
-- custom vocabulary. The URI must be in the same region as the API
-- endpoint that you are calling. The general form is:
--
-- For more information about S3 object names, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
-- in the /Amazon S3 Developer Guide/.
--
-- For more information about custom vocabularies, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary Custom vocabularies>.
createVocabulary_vocabularyFileUri :: Lens.Lens' CreateVocabulary (Prelude.Maybe Prelude.Text)
createVocabulary_vocabularyFileUri = Lens.lens (\CreateVocabulary' {vocabularyFileUri} -> vocabularyFileUri) (\s@CreateVocabulary' {} a -> s {vocabularyFileUri = a} :: CreateVocabulary)

-- | The name of the vocabulary. The name must be unique within an Amazon Web
-- Services account. The name is case sensitive. If you try to create a
-- vocabulary with the same name as a previous vocabulary you will receive
-- a @ConflictException@ error.
createVocabulary_vocabularyName :: Lens.Lens' CreateVocabulary Prelude.Text
createVocabulary_vocabularyName = Lens.lens (\CreateVocabulary' {vocabularyName} -> vocabularyName) (\s@CreateVocabulary' {} a -> s {vocabularyName = a} :: CreateVocabulary)

-- | The language code of the vocabulary entries. For a list of languages and
-- their corresponding language codes, see transcribe-whatis.
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
            Prelude.<$> (x Core..?> "LanguageCode")
            Prelude.<*> (x Core..?> "FailureReason")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "VocabularyName")
            Prelude.<*> (x Core..?> "VocabularyState")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVocabulary

instance Prelude.NFData CreateVocabulary

instance Core.ToHeaders CreateVocabulary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.CreateVocabulary" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateVocabulary where
  toJSON CreateVocabulary' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Phrases" Core..=) Prelude.<$> phrases,
            ("Tags" Core..=) Prelude.<$> tags,
            ("VocabularyFileUri" Core..=)
              Prelude.<$> vocabularyFileUri,
            Prelude.Just
              ("VocabularyName" Core..= vocabularyName),
            Prelude.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.ToPath CreateVocabulary where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateVocabulary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVocabularyResponse' smart constructor.
data CreateVocabularyResponse = CreateVocabularyResponse'
  { -- | The language code of the vocabulary entries.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | If the @VocabularyState@ field is @FAILED@, this field contains
    -- information about why the job failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the vocabulary was created.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the vocabulary.
    vocabularyName :: Prelude.Maybe Prelude.Text,
    -- | The processing state of the vocabulary. When the @VocabularyState@ field
    -- contains @READY@ the vocabulary is ready to be used in a
    -- @StartTranscriptionJob@ request.
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
-- 'languageCode', 'createVocabularyResponse_languageCode' - The language code of the vocabulary entries.
--
-- 'failureReason', 'createVocabularyResponse_failureReason' - If the @VocabularyState@ field is @FAILED@, this field contains
-- information about why the job failed.
--
-- 'lastModifiedTime', 'createVocabularyResponse_lastModifiedTime' - The date and time that the vocabulary was created.
--
-- 'vocabularyName', 'createVocabularyResponse_vocabularyName' - The name of the vocabulary.
--
-- 'vocabularyState', 'createVocabularyResponse_vocabularyState' - The processing state of the vocabulary. When the @VocabularyState@ field
-- contains @READY@ the vocabulary is ready to be used in a
-- @StartTranscriptionJob@ request.
--
-- 'httpStatus', 'createVocabularyResponse_httpStatus' - The response's http status code.
newCreateVocabularyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVocabularyResponse
newCreateVocabularyResponse pHttpStatus_ =
  CreateVocabularyResponse'
    { languageCode =
        Prelude.Nothing,
      failureReason = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      vocabularyName = Prelude.Nothing,
      vocabularyState = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The language code of the vocabulary entries.
createVocabularyResponse_languageCode :: Lens.Lens' CreateVocabularyResponse (Prelude.Maybe LanguageCode)
createVocabularyResponse_languageCode = Lens.lens (\CreateVocabularyResponse' {languageCode} -> languageCode) (\s@CreateVocabularyResponse' {} a -> s {languageCode = a} :: CreateVocabularyResponse)

-- | If the @VocabularyState@ field is @FAILED@, this field contains
-- information about why the job failed.
createVocabularyResponse_failureReason :: Lens.Lens' CreateVocabularyResponse (Prelude.Maybe Prelude.Text)
createVocabularyResponse_failureReason = Lens.lens (\CreateVocabularyResponse' {failureReason} -> failureReason) (\s@CreateVocabularyResponse' {} a -> s {failureReason = a} :: CreateVocabularyResponse)

-- | The date and time that the vocabulary was created.
createVocabularyResponse_lastModifiedTime :: Lens.Lens' CreateVocabularyResponse (Prelude.Maybe Prelude.UTCTime)
createVocabularyResponse_lastModifiedTime = Lens.lens (\CreateVocabularyResponse' {lastModifiedTime} -> lastModifiedTime) (\s@CreateVocabularyResponse' {} a -> s {lastModifiedTime = a} :: CreateVocabularyResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the vocabulary.
createVocabularyResponse_vocabularyName :: Lens.Lens' CreateVocabularyResponse (Prelude.Maybe Prelude.Text)
createVocabularyResponse_vocabularyName = Lens.lens (\CreateVocabularyResponse' {vocabularyName} -> vocabularyName) (\s@CreateVocabularyResponse' {} a -> s {vocabularyName = a} :: CreateVocabularyResponse)

-- | The processing state of the vocabulary. When the @VocabularyState@ field
-- contains @READY@ the vocabulary is ready to be used in a
-- @StartTranscriptionJob@ request.
createVocabularyResponse_vocabularyState :: Lens.Lens' CreateVocabularyResponse (Prelude.Maybe VocabularyState)
createVocabularyResponse_vocabularyState = Lens.lens (\CreateVocabularyResponse' {vocabularyState} -> vocabularyState) (\s@CreateVocabularyResponse' {} a -> s {vocabularyState = a} :: CreateVocabularyResponse)

-- | The response's http status code.
createVocabularyResponse_httpStatus :: Lens.Lens' CreateVocabularyResponse Prelude.Int
createVocabularyResponse_httpStatus = Lens.lens (\CreateVocabularyResponse' {httpStatus} -> httpStatus) (\s@CreateVocabularyResponse' {} a -> s {httpStatus = a} :: CreateVocabularyResponse)

instance Prelude.NFData CreateVocabularyResponse
