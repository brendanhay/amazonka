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
-- Module      : Amazonka.Transcribe.UpdateMedicalVocabulary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a vocabulary with new values that you provide in a different
-- text file from the one you used to create the vocabulary. The
-- @UpdateMedicalVocabulary@ operation overwrites all of the existing
-- information with the values that you provide in the request.
module Amazonka.Transcribe.UpdateMedicalVocabulary
  ( -- * Creating a Request
    UpdateMedicalVocabulary (..),
    newUpdateMedicalVocabulary,

    -- * Request Lenses
    updateMedicalVocabulary_vocabularyFileUri,
    updateMedicalVocabulary_vocabularyName,
    updateMedicalVocabulary_languageCode,

    -- * Destructuring the Response
    UpdateMedicalVocabularyResponse (..),
    newUpdateMedicalVocabularyResponse,

    -- * Response Lenses
    updateMedicalVocabularyResponse_languageCode,
    updateMedicalVocabularyResponse_vocabularyName,
    updateMedicalVocabularyResponse_lastModifiedTime,
    updateMedicalVocabularyResponse_vocabularyState,
    updateMedicalVocabularyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newUpdateMedicalVocabulary' smart constructor.
data UpdateMedicalVocabulary = UpdateMedicalVocabulary'
  { -- | The location in Amazon S3 of the text file that contains your custom
    -- vocabulary. The URI must be in the same Amazon Web Services Region as
    -- the resource that you are calling. The following is the format for a
    -- URI:
    --
    -- @ https:\/\/s3.\<aws-region>.amazonaws.com\/\<bucket-name>\/\<keyprefix>\/\<objectkey> @
    --
    -- For example:
    --
    -- @https:\/\/s3.us-east-1.amazonaws.com\/AWSDOC-EXAMPLE-BUCKET\/vocab.txt@
    --
    -- For more information about Amazon S3 object names, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
    -- in the /Amazon S3 Developer Guide/.
    --
    -- For more information about custom vocabularies in Amazon Transcribe
    -- Medical, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Medical Custom Vocabularies>.
    vocabularyFileUri :: Prelude.Maybe Prelude.Text,
    -- | The name of the vocabulary to update. The name is case sensitive. If you
    -- try to update a vocabulary with the same name as a vocabulary you\'ve
    -- already made, you get a @ConflictException@ error.
    vocabularyName :: Prelude.Text,
    -- | The language code of the language used for the entries in the updated
    -- vocabulary. US English (en-US) is the only valid language code in Amazon
    -- Transcribe Medical.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMedicalVocabulary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vocabularyFileUri', 'updateMedicalVocabulary_vocabularyFileUri' - The location in Amazon S3 of the text file that contains your custom
-- vocabulary. The URI must be in the same Amazon Web Services Region as
-- the resource that you are calling. The following is the format for a
-- URI:
--
-- @ https:\/\/s3.\<aws-region>.amazonaws.com\/\<bucket-name>\/\<keyprefix>\/\<objectkey> @
--
-- For example:
--
-- @https:\/\/s3.us-east-1.amazonaws.com\/AWSDOC-EXAMPLE-BUCKET\/vocab.txt@
--
-- For more information about Amazon S3 object names, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
-- in the /Amazon S3 Developer Guide/.
--
-- For more information about custom vocabularies in Amazon Transcribe
-- Medical, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Medical Custom Vocabularies>.
--
-- 'vocabularyName', 'updateMedicalVocabulary_vocabularyName' - The name of the vocabulary to update. The name is case sensitive. If you
-- try to update a vocabulary with the same name as a vocabulary you\'ve
-- already made, you get a @ConflictException@ error.
--
-- 'languageCode', 'updateMedicalVocabulary_languageCode' - The language code of the language used for the entries in the updated
-- vocabulary. US English (en-US) is the only valid language code in Amazon
-- Transcribe Medical.
newUpdateMedicalVocabulary ::
  -- | 'vocabularyName'
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  UpdateMedicalVocabulary
newUpdateMedicalVocabulary
  pVocabularyName_
  pLanguageCode_ =
    UpdateMedicalVocabulary'
      { vocabularyFileUri =
          Prelude.Nothing,
        vocabularyName = pVocabularyName_,
        languageCode = pLanguageCode_
      }

-- | The location in Amazon S3 of the text file that contains your custom
-- vocabulary. The URI must be in the same Amazon Web Services Region as
-- the resource that you are calling. The following is the format for a
-- URI:
--
-- @ https:\/\/s3.\<aws-region>.amazonaws.com\/\<bucket-name>\/\<keyprefix>\/\<objectkey> @
--
-- For example:
--
-- @https:\/\/s3.us-east-1.amazonaws.com\/AWSDOC-EXAMPLE-BUCKET\/vocab.txt@
--
-- For more information about Amazon S3 object names, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
-- in the /Amazon S3 Developer Guide/.
--
-- For more information about custom vocabularies in Amazon Transcribe
-- Medical, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Medical Custom Vocabularies>.
updateMedicalVocabulary_vocabularyFileUri :: Lens.Lens' UpdateMedicalVocabulary (Prelude.Maybe Prelude.Text)
updateMedicalVocabulary_vocabularyFileUri = Lens.lens (\UpdateMedicalVocabulary' {vocabularyFileUri} -> vocabularyFileUri) (\s@UpdateMedicalVocabulary' {} a -> s {vocabularyFileUri = a} :: UpdateMedicalVocabulary)

-- | The name of the vocabulary to update. The name is case sensitive. If you
-- try to update a vocabulary with the same name as a vocabulary you\'ve
-- already made, you get a @ConflictException@ error.
updateMedicalVocabulary_vocabularyName :: Lens.Lens' UpdateMedicalVocabulary Prelude.Text
updateMedicalVocabulary_vocabularyName = Lens.lens (\UpdateMedicalVocabulary' {vocabularyName} -> vocabularyName) (\s@UpdateMedicalVocabulary' {} a -> s {vocabularyName = a} :: UpdateMedicalVocabulary)

-- | The language code of the language used for the entries in the updated
-- vocabulary. US English (en-US) is the only valid language code in Amazon
-- Transcribe Medical.
updateMedicalVocabulary_languageCode :: Lens.Lens' UpdateMedicalVocabulary LanguageCode
updateMedicalVocabulary_languageCode = Lens.lens (\UpdateMedicalVocabulary' {languageCode} -> languageCode) (\s@UpdateMedicalVocabulary' {} a -> s {languageCode = a} :: UpdateMedicalVocabulary)

instance Core.AWSRequest UpdateMedicalVocabulary where
  type
    AWSResponse UpdateMedicalVocabulary =
      UpdateMedicalVocabularyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMedicalVocabularyResponse'
            Prelude.<$> (x Core..?> "LanguageCode")
            Prelude.<*> (x Core..?> "VocabularyName")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "VocabularyState")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateMedicalVocabulary where
  hashWithSalt _salt UpdateMedicalVocabulary' {..} =
    _salt `Prelude.hashWithSalt` vocabularyFileUri
      `Prelude.hashWithSalt` vocabularyName
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData UpdateMedicalVocabulary where
  rnf UpdateMedicalVocabulary' {..} =
    Prelude.rnf vocabularyFileUri
      `Prelude.seq` Prelude.rnf vocabularyName
      `Prelude.seq` Prelude.rnf languageCode

instance Core.ToHeaders UpdateMedicalVocabulary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.UpdateMedicalVocabulary" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateMedicalVocabulary where
  toJSON UpdateMedicalVocabulary' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VocabularyFileUri" Core..=)
              Prelude.<$> vocabularyFileUri,
            Prelude.Just
              ("VocabularyName" Core..= vocabularyName),
            Prelude.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.ToPath UpdateMedicalVocabulary where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateMedicalVocabulary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMedicalVocabularyResponse' smart constructor.
data UpdateMedicalVocabularyResponse = UpdateMedicalVocabularyResponse'
  { -- | The language code for the language of the text file used to update the
    -- custom vocabulary. US English (en-US) is the only language supported in
    -- Amazon Transcribe Medical.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The name of the updated vocabulary.
    vocabularyName :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the vocabulary was updated.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The processing state of the update to the vocabulary. When the
    -- @VocabularyState@ field is @READY@, the vocabulary is ready to be used
    -- in a @StartMedicalTranscriptionJob@ request.
    vocabularyState :: Prelude.Maybe VocabularyState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMedicalVocabularyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'updateMedicalVocabularyResponse_languageCode' - The language code for the language of the text file used to update the
-- custom vocabulary. US English (en-US) is the only language supported in
-- Amazon Transcribe Medical.
--
-- 'vocabularyName', 'updateMedicalVocabularyResponse_vocabularyName' - The name of the updated vocabulary.
--
-- 'lastModifiedTime', 'updateMedicalVocabularyResponse_lastModifiedTime' - The date and time that the vocabulary was updated.
--
-- 'vocabularyState', 'updateMedicalVocabularyResponse_vocabularyState' - The processing state of the update to the vocabulary. When the
-- @VocabularyState@ field is @READY@, the vocabulary is ready to be used
-- in a @StartMedicalTranscriptionJob@ request.
--
-- 'httpStatus', 'updateMedicalVocabularyResponse_httpStatus' - The response's http status code.
newUpdateMedicalVocabularyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateMedicalVocabularyResponse
newUpdateMedicalVocabularyResponse pHttpStatus_ =
  UpdateMedicalVocabularyResponse'
    { languageCode =
        Prelude.Nothing,
      vocabularyName = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      vocabularyState = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The language code for the language of the text file used to update the
-- custom vocabulary. US English (en-US) is the only language supported in
-- Amazon Transcribe Medical.
updateMedicalVocabularyResponse_languageCode :: Lens.Lens' UpdateMedicalVocabularyResponse (Prelude.Maybe LanguageCode)
updateMedicalVocabularyResponse_languageCode = Lens.lens (\UpdateMedicalVocabularyResponse' {languageCode} -> languageCode) (\s@UpdateMedicalVocabularyResponse' {} a -> s {languageCode = a} :: UpdateMedicalVocabularyResponse)

-- | The name of the updated vocabulary.
updateMedicalVocabularyResponse_vocabularyName :: Lens.Lens' UpdateMedicalVocabularyResponse (Prelude.Maybe Prelude.Text)
updateMedicalVocabularyResponse_vocabularyName = Lens.lens (\UpdateMedicalVocabularyResponse' {vocabularyName} -> vocabularyName) (\s@UpdateMedicalVocabularyResponse' {} a -> s {vocabularyName = a} :: UpdateMedicalVocabularyResponse)

-- | The date and time that the vocabulary was updated.
updateMedicalVocabularyResponse_lastModifiedTime :: Lens.Lens' UpdateMedicalVocabularyResponse (Prelude.Maybe Prelude.UTCTime)
updateMedicalVocabularyResponse_lastModifiedTime = Lens.lens (\UpdateMedicalVocabularyResponse' {lastModifiedTime} -> lastModifiedTime) (\s@UpdateMedicalVocabularyResponse' {} a -> s {lastModifiedTime = a} :: UpdateMedicalVocabularyResponse) Prelude.. Lens.mapping Core._Time

-- | The processing state of the update to the vocabulary. When the
-- @VocabularyState@ field is @READY@, the vocabulary is ready to be used
-- in a @StartMedicalTranscriptionJob@ request.
updateMedicalVocabularyResponse_vocabularyState :: Lens.Lens' UpdateMedicalVocabularyResponse (Prelude.Maybe VocabularyState)
updateMedicalVocabularyResponse_vocabularyState = Lens.lens (\UpdateMedicalVocabularyResponse' {vocabularyState} -> vocabularyState) (\s@UpdateMedicalVocabularyResponse' {} a -> s {vocabularyState = a} :: UpdateMedicalVocabularyResponse)

-- | The response's http status code.
updateMedicalVocabularyResponse_httpStatus :: Lens.Lens' UpdateMedicalVocabularyResponse Prelude.Int
updateMedicalVocabularyResponse_httpStatus = Lens.lens (\UpdateMedicalVocabularyResponse' {httpStatus} -> httpStatus) (\s@UpdateMedicalVocabularyResponse' {} a -> s {httpStatus = a} :: UpdateMedicalVocabularyResponse)

instance
  Prelude.NFData
    UpdateMedicalVocabularyResponse
  where
  rnf UpdateMedicalVocabularyResponse' {..} =
    Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf vocabularyName
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf vocabularyState
      `Prelude.seq` Prelude.rnf httpStatus
