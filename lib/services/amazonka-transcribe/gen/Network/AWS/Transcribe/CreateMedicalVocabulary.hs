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
-- Module      : Network.AWS.Transcribe.CreateMedicalVocabulary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom vocabulary that you can use to modify how Amazon
-- Transcribe Medical transcribes your audio file.
module Network.AWS.Transcribe.CreateMedicalVocabulary
  ( -- * Creating a Request
    CreateMedicalVocabulary (..),
    newCreateMedicalVocabulary,

    -- * Request Lenses
    createMedicalVocabulary_tags,
    createMedicalVocabulary_vocabularyName,
    createMedicalVocabulary_languageCode,
    createMedicalVocabulary_vocabularyFileUri,

    -- * Destructuring the Response
    CreateMedicalVocabularyResponse (..),
    newCreateMedicalVocabularyResponse,

    -- * Response Lenses
    createMedicalVocabularyResponse_failureReason,
    createMedicalVocabularyResponse_languageCode,
    createMedicalVocabularyResponse_vocabularyName,
    createMedicalVocabularyResponse_lastModifiedTime,
    createMedicalVocabularyResponse_vocabularyState,
    createMedicalVocabularyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newCreateMedicalVocabulary' smart constructor.
data CreateMedicalVocabulary = CreateMedicalVocabulary'
  { -- | Adds one or more tags, each in the form of a key:value pair, to a new
    -- medical vocabulary at the time you create this new vocabulary.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The name of the custom vocabulary. This case-sensitive name must be
    -- unique within an Amazon Web Services account. If you try to create a
    -- vocabulary with the same name as a previous vocabulary, you get a
    -- @ConflictException@ error.
    vocabularyName :: Prelude.Text,
    -- | The language code for the language used for the entries in your custom
    -- vocabulary. The language code of your custom vocabulary must match the
    -- language code of your transcription job. US English (en-US) is the only
    -- language code available for Amazon Transcribe Medical.
    languageCode :: LanguageCode,
    -- | The location in Amazon S3 of the text file you use to define your custom
    -- vocabulary. The URI must be in the same Amazon Web Services Region as
    -- the resource that you\'re calling. Enter information about your
    -- @VocabularyFileUri@ in the following format:
    --
    -- @ https:\/\/s3.\<aws-region>.amazonaws.com\/\<bucket-name>\/\<keyprefix>\/\<objectkey> @
    --
    -- The following is an example URI for a vocabulary file that is stored in
    -- Amazon S3:
    --
    -- @https:\/\/s3.us-east-1.amazonaws.com\/AWSDOC-EXAMPLE-BUCKET\/vocab.txt@
    --
    -- For more information about Amazon S3 object names, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
    -- in the /Amazon S3 Developer Guide/.
    --
    -- For more information about custom vocabularies, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary-med Medical Custom Vocabularies>.
    vocabularyFileUri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMedicalVocabulary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createMedicalVocabulary_tags' - Adds one or more tags, each in the form of a key:value pair, to a new
-- medical vocabulary at the time you create this new vocabulary.
--
-- 'vocabularyName', 'createMedicalVocabulary_vocabularyName' - The name of the custom vocabulary. This case-sensitive name must be
-- unique within an Amazon Web Services account. If you try to create a
-- vocabulary with the same name as a previous vocabulary, you get a
-- @ConflictException@ error.
--
-- 'languageCode', 'createMedicalVocabulary_languageCode' - The language code for the language used for the entries in your custom
-- vocabulary. The language code of your custom vocabulary must match the
-- language code of your transcription job. US English (en-US) is the only
-- language code available for Amazon Transcribe Medical.
--
-- 'vocabularyFileUri', 'createMedicalVocabulary_vocabularyFileUri' - The location in Amazon S3 of the text file you use to define your custom
-- vocabulary. The URI must be in the same Amazon Web Services Region as
-- the resource that you\'re calling. Enter information about your
-- @VocabularyFileUri@ in the following format:
--
-- @ https:\/\/s3.\<aws-region>.amazonaws.com\/\<bucket-name>\/\<keyprefix>\/\<objectkey> @
--
-- The following is an example URI for a vocabulary file that is stored in
-- Amazon S3:
--
-- @https:\/\/s3.us-east-1.amazonaws.com\/AWSDOC-EXAMPLE-BUCKET\/vocab.txt@
--
-- For more information about Amazon S3 object names, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
-- in the /Amazon S3 Developer Guide/.
--
-- For more information about custom vocabularies, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary-med Medical Custom Vocabularies>.
newCreateMedicalVocabulary ::
  -- | 'vocabularyName'
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  -- | 'vocabularyFileUri'
  Prelude.Text ->
  CreateMedicalVocabulary
newCreateMedicalVocabulary
  pVocabularyName_
  pLanguageCode_
  pVocabularyFileUri_ =
    CreateMedicalVocabulary'
      { tags = Prelude.Nothing,
        vocabularyName = pVocabularyName_,
        languageCode = pLanguageCode_,
        vocabularyFileUri = pVocabularyFileUri_
      }

-- | Adds one or more tags, each in the form of a key:value pair, to a new
-- medical vocabulary at the time you create this new vocabulary.
createMedicalVocabulary_tags :: Lens.Lens' CreateMedicalVocabulary (Prelude.Maybe (Prelude.NonEmpty Tag))
createMedicalVocabulary_tags = Lens.lens (\CreateMedicalVocabulary' {tags} -> tags) (\s@CreateMedicalVocabulary' {} a -> s {tags = a} :: CreateMedicalVocabulary) Prelude.. Lens.mapping Lens.coerced

-- | The name of the custom vocabulary. This case-sensitive name must be
-- unique within an Amazon Web Services account. If you try to create a
-- vocabulary with the same name as a previous vocabulary, you get a
-- @ConflictException@ error.
createMedicalVocabulary_vocabularyName :: Lens.Lens' CreateMedicalVocabulary Prelude.Text
createMedicalVocabulary_vocabularyName = Lens.lens (\CreateMedicalVocabulary' {vocabularyName} -> vocabularyName) (\s@CreateMedicalVocabulary' {} a -> s {vocabularyName = a} :: CreateMedicalVocabulary)

-- | The language code for the language used for the entries in your custom
-- vocabulary. The language code of your custom vocabulary must match the
-- language code of your transcription job. US English (en-US) is the only
-- language code available for Amazon Transcribe Medical.
createMedicalVocabulary_languageCode :: Lens.Lens' CreateMedicalVocabulary LanguageCode
createMedicalVocabulary_languageCode = Lens.lens (\CreateMedicalVocabulary' {languageCode} -> languageCode) (\s@CreateMedicalVocabulary' {} a -> s {languageCode = a} :: CreateMedicalVocabulary)

-- | The location in Amazon S3 of the text file you use to define your custom
-- vocabulary. The URI must be in the same Amazon Web Services Region as
-- the resource that you\'re calling. Enter information about your
-- @VocabularyFileUri@ in the following format:
--
-- @ https:\/\/s3.\<aws-region>.amazonaws.com\/\<bucket-name>\/\<keyprefix>\/\<objectkey> @
--
-- The following is an example URI for a vocabulary file that is stored in
-- Amazon S3:
--
-- @https:\/\/s3.us-east-1.amazonaws.com\/AWSDOC-EXAMPLE-BUCKET\/vocab.txt@
--
-- For more information about Amazon S3 object names, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
-- in the /Amazon S3 Developer Guide/.
--
-- For more information about custom vocabularies, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary-med Medical Custom Vocabularies>.
createMedicalVocabulary_vocabularyFileUri :: Lens.Lens' CreateMedicalVocabulary Prelude.Text
createMedicalVocabulary_vocabularyFileUri = Lens.lens (\CreateMedicalVocabulary' {vocabularyFileUri} -> vocabularyFileUri) (\s@CreateMedicalVocabulary' {} a -> s {vocabularyFileUri = a} :: CreateMedicalVocabulary)

instance Core.AWSRequest CreateMedicalVocabulary where
  type
    AWSResponse CreateMedicalVocabulary =
      CreateMedicalVocabularyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMedicalVocabularyResponse'
            Prelude.<$> (x Core..?> "FailureReason")
            Prelude.<*> (x Core..?> "LanguageCode")
            Prelude.<*> (x Core..?> "VocabularyName")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "VocabularyState")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateMedicalVocabulary

instance Prelude.NFData CreateMedicalVocabulary

instance Core.ToHeaders CreateMedicalVocabulary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.CreateMedicalVocabulary" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateMedicalVocabulary where
  toJSON CreateMedicalVocabulary' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ("VocabularyName" Core..= vocabularyName),
            Prelude.Just ("LanguageCode" Core..= languageCode),
            Prelude.Just
              ("VocabularyFileUri" Core..= vocabularyFileUri)
          ]
      )

instance Core.ToPath CreateMedicalVocabulary where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateMedicalVocabulary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMedicalVocabularyResponse' smart constructor.
data CreateMedicalVocabularyResponse = CreateMedicalVocabularyResponse'
  { -- | If the @VocabularyState@ field is @FAILED@, this field contains
    -- information about why the job failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The language code for the entries in your custom vocabulary. US English
    -- (en-US) is the only valid language code for Amazon Transcribe Medical.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The name of the vocabulary. The name must be unique within an Amazon Web
    -- Services account and is case sensitive.
    vocabularyName :: Prelude.Maybe Prelude.Text,
    -- | The date and time that you created the vocabulary.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The processing state of your custom vocabulary in Amazon Transcribe
    -- Medical. If the state is @READY@, you can use the vocabulary in a
    -- @StartMedicalTranscriptionJob@ request.
    vocabularyState :: Prelude.Maybe VocabularyState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMedicalVocabularyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'createMedicalVocabularyResponse_failureReason' - If the @VocabularyState@ field is @FAILED@, this field contains
-- information about why the job failed.
--
-- 'languageCode', 'createMedicalVocabularyResponse_languageCode' - The language code for the entries in your custom vocabulary. US English
-- (en-US) is the only valid language code for Amazon Transcribe Medical.
--
-- 'vocabularyName', 'createMedicalVocabularyResponse_vocabularyName' - The name of the vocabulary. The name must be unique within an Amazon Web
-- Services account and is case sensitive.
--
-- 'lastModifiedTime', 'createMedicalVocabularyResponse_lastModifiedTime' - The date and time that you created the vocabulary.
--
-- 'vocabularyState', 'createMedicalVocabularyResponse_vocabularyState' - The processing state of your custom vocabulary in Amazon Transcribe
-- Medical. If the state is @READY@, you can use the vocabulary in a
-- @StartMedicalTranscriptionJob@ request.
--
-- 'httpStatus', 'createMedicalVocabularyResponse_httpStatus' - The response's http status code.
newCreateMedicalVocabularyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateMedicalVocabularyResponse
newCreateMedicalVocabularyResponse pHttpStatus_ =
  CreateMedicalVocabularyResponse'
    { failureReason =
        Prelude.Nothing,
      languageCode = Prelude.Nothing,
      vocabularyName = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      vocabularyState = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the @VocabularyState@ field is @FAILED@, this field contains
-- information about why the job failed.
createMedicalVocabularyResponse_failureReason :: Lens.Lens' CreateMedicalVocabularyResponse (Prelude.Maybe Prelude.Text)
createMedicalVocabularyResponse_failureReason = Lens.lens (\CreateMedicalVocabularyResponse' {failureReason} -> failureReason) (\s@CreateMedicalVocabularyResponse' {} a -> s {failureReason = a} :: CreateMedicalVocabularyResponse)

-- | The language code for the entries in your custom vocabulary. US English
-- (en-US) is the only valid language code for Amazon Transcribe Medical.
createMedicalVocabularyResponse_languageCode :: Lens.Lens' CreateMedicalVocabularyResponse (Prelude.Maybe LanguageCode)
createMedicalVocabularyResponse_languageCode = Lens.lens (\CreateMedicalVocabularyResponse' {languageCode} -> languageCode) (\s@CreateMedicalVocabularyResponse' {} a -> s {languageCode = a} :: CreateMedicalVocabularyResponse)

-- | The name of the vocabulary. The name must be unique within an Amazon Web
-- Services account and is case sensitive.
createMedicalVocabularyResponse_vocabularyName :: Lens.Lens' CreateMedicalVocabularyResponse (Prelude.Maybe Prelude.Text)
createMedicalVocabularyResponse_vocabularyName = Lens.lens (\CreateMedicalVocabularyResponse' {vocabularyName} -> vocabularyName) (\s@CreateMedicalVocabularyResponse' {} a -> s {vocabularyName = a} :: CreateMedicalVocabularyResponse)

-- | The date and time that you created the vocabulary.
createMedicalVocabularyResponse_lastModifiedTime :: Lens.Lens' CreateMedicalVocabularyResponse (Prelude.Maybe Prelude.UTCTime)
createMedicalVocabularyResponse_lastModifiedTime = Lens.lens (\CreateMedicalVocabularyResponse' {lastModifiedTime} -> lastModifiedTime) (\s@CreateMedicalVocabularyResponse' {} a -> s {lastModifiedTime = a} :: CreateMedicalVocabularyResponse) Prelude.. Lens.mapping Core._Time

-- | The processing state of your custom vocabulary in Amazon Transcribe
-- Medical. If the state is @READY@, you can use the vocabulary in a
-- @StartMedicalTranscriptionJob@ request.
createMedicalVocabularyResponse_vocabularyState :: Lens.Lens' CreateMedicalVocabularyResponse (Prelude.Maybe VocabularyState)
createMedicalVocabularyResponse_vocabularyState = Lens.lens (\CreateMedicalVocabularyResponse' {vocabularyState} -> vocabularyState) (\s@CreateMedicalVocabularyResponse' {} a -> s {vocabularyState = a} :: CreateMedicalVocabularyResponse)

-- | The response's http status code.
createMedicalVocabularyResponse_httpStatus :: Lens.Lens' CreateMedicalVocabularyResponse Prelude.Int
createMedicalVocabularyResponse_httpStatus = Lens.lens (\CreateMedicalVocabularyResponse' {httpStatus} -> httpStatus) (\s@CreateMedicalVocabularyResponse' {} a -> s {httpStatus = a} :: CreateMedicalVocabularyResponse)

instance
  Prelude.NFData
    CreateMedicalVocabularyResponse
