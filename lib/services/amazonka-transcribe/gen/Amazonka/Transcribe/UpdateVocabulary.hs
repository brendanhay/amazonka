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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing custom vocabulary with new values. This operation
-- overwrites all existing information with your new values; you cannot
-- append new terms onto an existing custom vocabulary.
module Amazonka.Transcribe.UpdateVocabulary
  ( -- * Creating a Request
    UpdateVocabulary (..),
    newUpdateVocabulary,

    -- * Request Lenses
    updateVocabulary_dataAccessRoleArn,
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
    updateVocabularyResponse_vocabularyName,
    updateVocabularyResponse_vocabularyState,
    updateVocabularyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newUpdateVocabulary' smart constructor.
data UpdateVocabulary = UpdateVocabulary'
  { -- | The Amazon Resource Name (ARN) of an IAM role that has permissions to
    -- access the Amazon S3 bucket that contains your input files (in this
    -- case, your custom vocabulary). If the role that you specify doesn’t have
    -- the appropriate permissions to access the specified Amazon S3 location,
    -- your request fails.
    --
    -- IAM role ARNs have the format
    -- @arn:partition:iam::account:role\/role-name-with-path@. For example:
    -- @arn:aws:iam::111122223333:role\/Admin@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-arns IAM ARNs>.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Use this parameter if you want to update your custom vocabulary by
    -- including all desired terms, as comma-separated values, within your
    -- request. The other option for updating your custom vocabulary is to save
    -- your entries in a text file and upload them to an Amazon S3 bucket, then
    -- specify the location of your file using the @VocabularyFileUri@
    -- parameter.
    --
    -- Note that if you include @Phrases@ in your request, you cannot use
    -- @VocabularyFileUri@; you must choose one or the other.
    --
    -- Each language has a character set that contains all allowed characters
    -- for that specific language. If you use unsupported characters, your
    -- custom vocabulary filter request fails. Refer to
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/charsets.html Character Sets for Custom Vocabularies>
    -- to get the character set for your language.
    phrases :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon S3 location of the text file that contains your custom
    -- vocabulary. The URI must be located in the same Amazon Web Services
    -- Region as the resource you\'re calling.
    --
    -- Here\'s an example URI path:
    -- @s3:\/\/DOC-EXAMPLE-BUCKET\/my-vocab-file.txt@
    --
    -- Note that if you include @VocabularyFileUri@ in your request, you cannot
    -- use the @Phrases@ flag; you must choose one or the other.
    vocabularyFileUri :: Prelude.Maybe Prelude.Text,
    -- | The name of the custom vocabulary you want to update. Custom vocabulary
    -- names are case sensitive.
    vocabularyName :: Prelude.Text,
    -- | The language code that represents the language of the entries in the
    -- custom vocabulary you want to update. Each custom vocabulary must
    -- contain terms in only one language.
    --
    -- A custom vocabulary can only be used to transcribe files in the same
    -- language as the custom vocabulary. For example, if you create a custom
    -- vocabulary using US English (@en-US@), you can only apply this custom
    -- vocabulary to files that contain English audio.
    --
    -- For a list of supported languages and their associated language codes,
    -- refer to the
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
    -- table.
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
-- 'dataAccessRoleArn', 'updateVocabulary_dataAccessRoleArn' - The Amazon Resource Name (ARN) of an IAM role that has permissions to
-- access the Amazon S3 bucket that contains your input files (in this
-- case, your custom vocabulary). If the role that you specify doesn’t have
-- the appropriate permissions to access the specified Amazon S3 location,
-- your request fails.
--
-- IAM role ARNs have the format
-- @arn:partition:iam::account:role\/role-name-with-path@. For example:
-- @arn:aws:iam::111122223333:role\/Admin@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-arns IAM ARNs>.
--
-- 'phrases', 'updateVocabulary_phrases' - Use this parameter if you want to update your custom vocabulary by
-- including all desired terms, as comma-separated values, within your
-- request. The other option for updating your custom vocabulary is to save
-- your entries in a text file and upload them to an Amazon S3 bucket, then
-- specify the location of your file using the @VocabularyFileUri@
-- parameter.
--
-- Note that if you include @Phrases@ in your request, you cannot use
-- @VocabularyFileUri@; you must choose one or the other.
--
-- Each language has a character set that contains all allowed characters
-- for that specific language. If you use unsupported characters, your
-- custom vocabulary filter request fails. Refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/charsets.html Character Sets for Custom Vocabularies>
-- to get the character set for your language.
--
-- 'vocabularyFileUri', 'updateVocabulary_vocabularyFileUri' - The Amazon S3 location of the text file that contains your custom
-- vocabulary. The URI must be located in the same Amazon Web Services
-- Region as the resource you\'re calling.
--
-- Here\'s an example URI path:
-- @s3:\/\/DOC-EXAMPLE-BUCKET\/my-vocab-file.txt@
--
-- Note that if you include @VocabularyFileUri@ in your request, you cannot
-- use the @Phrases@ flag; you must choose one or the other.
--
-- 'vocabularyName', 'updateVocabulary_vocabularyName' - The name of the custom vocabulary you want to update. Custom vocabulary
-- names are case sensitive.
--
-- 'languageCode', 'updateVocabulary_languageCode' - The language code that represents the language of the entries in the
-- custom vocabulary you want to update. Each custom vocabulary must
-- contain terms in only one language.
--
-- A custom vocabulary can only be used to transcribe files in the same
-- language as the custom vocabulary. For example, if you create a custom
-- vocabulary using US English (@en-US@), you can only apply this custom
-- vocabulary to files that contain English audio.
--
-- For a list of supported languages and their associated language codes,
-- refer to the
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
-- table.
newUpdateVocabulary ::
  -- | 'vocabularyName'
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  UpdateVocabulary
newUpdateVocabulary pVocabularyName_ pLanguageCode_ =
  UpdateVocabulary'
    { dataAccessRoleArn =
        Prelude.Nothing,
      phrases = Prelude.Nothing,
      vocabularyFileUri = Prelude.Nothing,
      vocabularyName = pVocabularyName_,
      languageCode = pLanguageCode_
    }

-- | The Amazon Resource Name (ARN) of an IAM role that has permissions to
-- access the Amazon S3 bucket that contains your input files (in this
-- case, your custom vocabulary). If the role that you specify doesn’t have
-- the appropriate permissions to access the specified Amazon S3 location,
-- your request fails.
--
-- IAM role ARNs have the format
-- @arn:partition:iam::account:role\/role-name-with-path@. For example:
-- @arn:aws:iam::111122223333:role\/Admin@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-arns IAM ARNs>.
updateVocabulary_dataAccessRoleArn :: Lens.Lens' UpdateVocabulary (Prelude.Maybe Prelude.Text)
updateVocabulary_dataAccessRoleArn = Lens.lens (\UpdateVocabulary' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@UpdateVocabulary' {} a -> s {dataAccessRoleArn = a} :: UpdateVocabulary)

-- | Use this parameter if you want to update your custom vocabulary by
-- including all desired terms, as comma-separated values, within your
-- request. The other option for updating your custom vocabulary is to save
-- your entries in a text file and upload them to an Amazon S3 bucket, then
-- specify the location of your file using the @VocabularyFileUri@
-- parameter.
--
-- Note that if you include @Phrases@ in your request, you cannot use
-- @VocabularyFileUri@; you must choose one or the other.
--
-- Each language has a character set that contains all allowed characters
-- for that specific language. If you use unsupported characters, your
-- custom vocabulary filter request fails. Refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/charsets.html Character Sets for Custom Vocabularies>
-- to get the character set for your language.
updateVocabulary_phrases :: Lens.Lens' UpdateVocabulary (Prelude.Maybe [Prelude.Text])
updateVocabulary_phrases = Lens.lens (\UpdateVocabulary' {phrases} -> phrases) (\s@UpdateVocabulary' {} a -> s {phrases = a} :: UpdateVocabulary) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon S3 location of the text file that contains your custom
-- vocabulary. The URI must be located in the same Amazon Web Services
-- Region as the resource you\'re calling.
--
-- Here\'s an example URI path:
-- @s3:\/\/DOC-EXAMPLE-BUCKET\/my-vocab-file.txt@
--
-- Note that if you include @VocabularyFileUri@ in your request, you cannot
-- use the @Phrases@ flag; you must choose one or the other.
updateVocabulary_vocabularyFileUri :: Lens.Lens' UpdateVocabulary (Prelude.Maybe Prelude.Text)
updateVocabulary_vocabularyFileUri = Lens.lens (\UpdateVocabulary' {vocabularyFileUri} -> vocabularyFileUri) (\s@UpdateVocabulary' {} a -> s {vocabularyFileUri = a} :: UpdateVocabulary)

-- | The name of the custom vocabulary you want to update. Custom vocabulary
-- names are case sensitive.
updateVocabulary_vocabularyName :: Lens.Lens' UpdateVocabulary Prelude.Text
updateVocabulary_vocabularyName = Lens.lens (\UpdateVocabulary' {vocabularyName} -> vocabularyName) (\s@UpdateVocabulary' {} a -> s {vocabularyName = a} :: UpdateVocabulary)

-- | The language code that represents the language of the entries in the
-- custom vocabulary you want to update. Each custom vocabulary must
-- contain terms in only one language.
--
-- A custom vocabulary can only be used to transcribe files in the same
-- language as the custom vocabulary. For example, if you create a custom
-- vocabulary using US English (@en-US@), you can only apply this custom
-- vocabulary to files that contain English audio.
--
-- For a list of supported languages and their associated language codes,
-- refer to the
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
-- table.
updateVocabulary_languageCode :: Lens.Lens' UpdateVocabulary LanguageCode
updateVocabulary_languageCode = Lens.lens (\UpdateVocabulary' {languageCode} -> languageCode) (\s@UpdateVocabulary' {} a -> s {languageCode = a} :: UpdateVocabulary)

instance Core.AWSRequest UpdateVocabulary where
  type
    AWSResponse UpdateVocabulary =
      UpdateVocabularyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVocabularyResponse'
            Prelude.<$> (x Data..?> "LanguageCode")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "VocabularyName")
            Prelude.<*> (x Data..?> "VocabularyState")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateVocabulary where
  hashWithSalt _salt UpdateVocabulary' {..} =
    _salt
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` phrases
      `Prelude.hashWithSalt` vocabularyFileUri
      `Prelude.hashWithSalt` vocabularyName
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData UpdateVocabulary where
  rnf UpdateVocabulary' {..} =
    Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf phrases
      `Prelude.seq` Prelude.rnf vocabularyFileUri
      `Prelude.seq` Prelude.rnf vocabularyName
      `Prelude.seq` Prelude.rnf languageCode

instance Data.ToHeaders UpdateVocabulary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Transcribe.UpdateVocabulary" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateVocabulary where
  toJSON UpdateVocabulary' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataAccessRoleArn" Data..=)
              Prelude.<$> dataAccessRoleArn,
            ("Phrases" Data..=) Prelude.<$> phrases,
            ("VocabularyFileUri" Data..=)
              Prelude.<$> vocabularyFileUri,
            Prelude.Just
              ("VocabularyName" Data..= vocabularyName),
            Prelude.Just ("LanguageCode" Data..= languageCode)
          ]
      )

instance Data.ToPath UpdateVocabulary where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateVocabulary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateVocabularyResponse' smart constructor.
data UpdateVocabularyResponse = UpdateVocabularyResponse'
  { -- | The language code you selected for your custom vocabulary.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The date and time the specified custom vocabulary was last updated.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
    -- May 4, 2022.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the updated custom vocabulary.
    vocabularyName :: Prelude.Maybe Prelude.Text,
    -- | The processing state of your custom vocabulary. If the state is @READY@,
    -- you can use the custom vocabulary in a @StartTranscriptionJob@ request.
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
-- 'languageCode', 'updateVocabularyResponse_languageCode' - The language code you selected for your custom vocabulary.
--
-- 'lastModifiedTime', 'updateVocabularyResponse_lastModifiedTime' - The date and time the specified custom vocabulary was last updated.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
--
-- 'vocabularyName', 'updateVocabularyResponse_vocabularyName' - The name of the updated custom vocabulary.
--
-- 'vocabularyState', 'updateVocabularyResponse_vocabularyState' - The processing state of your custom vocabulary. If the state is @READY@,
-- you can use the custom vocabulary in a @StartTranscriptionJob@ request.
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
      lastModifiedTime = Prelude.Nothing,
      vocabularyName = Prelude.Nothing,
      vocabularyState = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The language code you selected for your custom vocabulary.
updateVocabularyResponse_languageCode :: Lens.Lens' UpdateVocabularyResponse (Prelude.Maybe LanguageCode)
updateVocabularyResponse_languageCode = Lens.lens (\UpdateVocabularyResponse' {languageCode} -> languageCode) (\s@UpdateVocabularyResponse' {} a -> s {languageCode = a} :: UpdateVocabularyResponse)

-- | The date and time the specified custom vocabulary was last updated.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
updateVocabularyResponse_lastModifiedTime :: Lens.Lens' UpdateVocabularyResponse (Prelude.Maybe Prelude.UTCTime)
updateVocabularyResponse_lastModifiedTime = Lens.lens (\UpdateVocabularyResponse' {lastModifiedTime} -> lastModifiedTime) (\s@UpdateVocabularyResponse' {} a -> s {lastModifiedTime = a} :: UpdateVocabularyResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the updated custom vocabulary.
updateVocabularyResponse_vocabularyName :: Lens.Lens' UpdateVocabularyResponse (Prelude.Maybe Prelude.Text)
updateVocabularyResponse_vocabularyName = Lens.lens (\UpdateVocabularyResponse' {vocabularyName} -> vocabularyName) (\s@UpdateVocabularyResponse' {} a -> s {vocabularyName = a} :: UpdateVocabularyResponse)

-- | The processing state of your custom vocabulary. If the state is @READY@,
-- you can use the custom vocabulary in a @StartTranscriptionJob@ request.
updateVocabularyResponse_vocabularyState :: Lens.Lens' UpdateVocabularyResponse (Prelude.Maybe VocabularyState)
updateVocabularyResponse_vocabularyState = Lens.lens (\UpdateVocabularyResponse' {vocabularyState} -> vocabularyState) (\s@UpdateVocabularyResponse' {} a -> s {vocabularyState = a} :: UpdateVocabularyResponse)

-- | The response's http status code.
updateVocabularyResponse_httpStatus :: Lens.Lens' UpdateVocabularyResponse Prelude.Int
updateVocabularyResponse_httpStatus = Lens.lens (\UpdateVocabularyResponse' {httpStatus} -> httpStatus) (\s@UpdateVocabularyResponse' {} a -> s {httpStatus = a} :: UpdateVocabularyResponse)

instance Prelude.NFData UpdateVocabularyResponse where
  rnf UpdateVocabularyResponse' {..} =
    Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf vocabularyName
      `Prelude.seq` Prelude.rnf vocabularyState
      `Prelude.seq` Prelude.rnf httpStatus
