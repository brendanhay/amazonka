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
-- Module      : Amazonka.Transcribe.GetMedicalVocabulary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the specified custom medical vocabulary.
--
-- To view the status of the specified medical vocabulary, check the
-- @VocabularyState@ field. If the status is @READY@, your vocabulary is
-- available to use. If the status is @FAILED@, @FailureReason@ provides
-- details on why your vocabulary failed.
--
-- To get a list of your custom medical vocabularies, use the operation.
module Amazonka.Transcribe.GetMedicalVocabulary
  ( -- * Creating a Request
    GetMedicalVocabulary (..),
    newGetMedicalVocabulary,

    -- * Request Lenses
    getMedicalVocabulary_vocabularyName,

    -- * Destructuring the Response
    GetMedicalVocabularyResponse (..),
    newGetMedicalVocabularyResponse,

    -- * Response Lenses
    getMedicalVocabularyResponse_downloadUri,
    getMedicalVocabularyResponse_vocabularyName,
    getMedicalVocabularyResponse_vocabularyState,
    getMedicalVocabularyResponse_lastModifiedTime,
    getMedicalVocabularyResponse_languageCode,
    getMedicalVocabularyResponse_failureReason,
    getMedicalVocabularyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newGetMedicalVocabulary' smart constructor.
data GetMedicalVocabulary = GetMedicalVocabulary'
  { -- | The name of the custom medical vocabulary you want information about.
    -- Vocabulary names are case sensitive.
    vocabularyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMedicalVocabulary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vocabularyName', 'getMedicalVocabulary_vocabularyName' - The name of the custom medical vocabulary you want information about.
-- Vocabulary names are case sensitive.
newGetMedicalVocabulary ::
  -- | 'vocabularyName'
  Prelude.Text ->
  GetMedicalVocabulary
newGetMedicalVocabulary pVocabularyName_ =
  GetMedicalVocabulary'
    { vocabularyName =
        pVocabularyName_
    }

-- | The name of the custom medical vocabulary you want information about.
-- Vocabulary names are case sensitive.
getMedicalVocabulary_vocabularyName :: Lens.Lens' GetMedicalVocabulary Prelude.Text
getMedicalVocabulary_vocabularyName = Lens.lens (\GetMedicalVocabulary' {vocabularyName} -> vocabularyName) (\s@GetMedicalVocabulary' {} a -> s {vocabularyName = a} :: GetMedicalVocabulary)

instance Core.AWSRequest GetMedicalVocabulary where
  type
    AWSResponse GetMedicalVocabulary =
      GetMedicalVocabularyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMedicalVocabularyResponse'
            Prelude.<$> (x Data..?> "DownloadUri")
            Prelude.<*> (x Data..?> "VocabularyName")
            Prelude.<*> (x Data..?> "VocabularyState")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "LanguageCode")
            Prelude.<*> (x Data..?> "FailureReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMedicalVocabulary where
  hashWithSalt _salt GetMedicalVocabulary' {..} =
    _salt `Prelude.hashWithSalt` vocabularyName

instance Prelude.NFData GetMedicalVocabulary where
  rnf GetMedicalVocabulary' {..} =
    Prelude.rnf vocabularyName

instance Data.ToHeaders GetMedicalVocabulary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Transcribe.GetMedicalVocabulary" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetMedicalVocabulary where
  toJSON GetMedicalVocabulary' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("VocabularyName" Data..= vocabularyName)
          ]
      )

instance Data.ToPath GetMedicalVocabulary where
  toPath = Prelude.const "/"

instance Data.ToQuery GetMedicalVocabulary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMedicalVocabularyResponse' smart constructor.
data GetMedicalVocabularyResponse = GetMedicalVocabularyResponse'
  { -- | The S3 location where the specified medical vocabulary is stored; use
    -- this URI to view or download the vocabulary.
    downloadUri :: Prelude.Maybe Prelude.Text,
    -- | The name of the custom medical vocabulary you requested information
    -- about.
    vocabularyName :: Prelude.Maybe Prelude.Text,
    -- | The processing state of your custom medical vocabulary. If the state is
    -- @READY@, you can use the vocabulary in a @StartMedicalTranscriptionJob@
    -- request.
    vocabularyState :: Prelude.Maybe VocabularyState,
    -- | The date and time the specified custom medical vocabulary was last
    -- modified.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
    -- May 4, 2022.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The language code you selected for your medical vocabulary. US English
    -- (@en-US@) is the only language supported with Amazon Transcribe Medical.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | If @VocabularyState@ is @FAILED@, @FailureReason@ contains information
    -- about why the medical vocabulary request failed. See also:
    -- <https://docs.aws.amazon.com/transcribe/latest/APIReference/CommonErrors.html Common Errors>.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMedicalVocabularyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'downloadUri', 'getMedicalVocabularyResponse_downloadUri' - The S3 location where the specified medical vocabulary is stored; use
-- this URI to view or download the vocabulary.
--
-- 'vocabularyName', 'getMedicalVocabularyResponse_vocabularyName' - The name of the custom medical vocabulary you requested information
-- about.
--
-- 'vocabularyState', 'getMedicalVocabularyResponse_vocabularyState' - The processing state of your custom medical vocabulary. If the state is
-- @READY@, you can use the vocabulary in a @StartMedicalTranscriptionJob@
-- request.
--
-- 'lastModifiedTime', 'getMedicalVocabularyResponse_lastModifiedTime' - The date and time the specified custom medical vocabulary was last
-- modified.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
--
-- 'languageCode', 'getMedicalVocabularyResponse_languageCode' - The language code you selected for your medical vocabulary. US English
-- (@en-US@) is the only language supported with Amazon Transcribe Medical.
--
-- 'failureReason', 'getMedicalVocabularyResponse_failureReason' - If @VocabularyState@ is @FAILED@, @FailureReason@ contains information
-- about why the medical vocabulary request failed. See also:
-- <https://docs.aws.amazon.com/transcribe/latest/APIReference/CommonErrors.html Common Errors>.
--
-- 'httpStatus', 'getMedicalVocabularyResponse_httpStatus' - The response's http status code.
newGetMedicalVocabularyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMedicalVocabularyResponse
newGetMedicalVocabularyResponse pHttpStatus_ =
  GetMedicalVocabularyResponse'
    { downloadUri =
        Prelude.Nothing,
      vocabularyName = Prelude.Nothing,
      vocabularyState = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The S3 location where the specified medical vocabulary is stored; use
-- this URI to view or download the vocabulary.
getMedicalVocabularyResponse_downloadUri :: Lens.Lens' GetMedicalVocabularyResponse (Prelude.Maybe Prelude.Text)
getMedicalVocabularyResponse_downloadUri = Lens.lens (\GetMedicalVocabularyResponse' {downloadUri} -> downloadUri) (\s@GetMedicalVocabularyResponse' {} a -> s {downloadUri = a} :: GetMedicalVocabularyResponse)

-- | The name of the custom medical vocabulary you requested information
-- about.
getMedicalVocabularyResponse_vocabularyName :: Lens.Lens' GetMedicalVocabularyResponse (Prelude.Maybe Prelude.Text)
getMedicalVocabularyResponse_vocabularyName = Lens.lens (\GetMedicalVocabularyResponse' {vocabularyName} -> vocabularyName) (\s@GetMedicalVocabularyResponse' {} a -> s {vocabularyName = a} :: GetMedicalVocabularyResponse)

-- | The processing state of your custom medical vocabulary. If the state is
-- @READY@, you can use the vocabulary in a @StartMedicalTranscriptionJob@
-- request.
getMedicalVocabularyResponse_vocabularyState :: Lens.Lens' GetMedicalVocabularyResponse (Prelude.Maybe VocabularyState)
getMedicalVocabularyResponse_vocabularyState = Lens.lens (\GetMedicalVocabularyResponse' {vocabularyState} -> vocabularyState) (\s@GetMedicalVocabularyResponse' {} a -> s {vocabularyState = a} :: GetMedicalVocabularyResponse)

-- | The date and time the specified custom medical vocabulary was last
-- modified.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
getMedicalVocabularyResponse_lastModifiedTime :: Lens.Lens' GetMedicalVocabularyResponse (Prelude.Maybe Prelude.UTCTime)
getMedicalVocabularyResponse_lastModifiedTime = Lens.lens (\GetMedicalVocabularyResponse' {lastModifiedTime} -> lastModifiedTime) (\s@GetMedicalVocabularyResponse' {} a -> s {lastModifiedTime = a} :: GetMedicalVocabularyResponse) Prelude.. Lens.mapping Data._Time

-- | The language code you selected for your medical vocabulary. US English
-- (@en-US@) is the only language supported with Amazon Transcribe Medical.
getMedicalVocabularyResponse_languageCode :: Lens.Lens' GetMedicalVocabularyResponse (Prelude.Maybe LanguageCode)
getMedicalVocabularyResponse_languageCode = Lens.lens (\GetMedicalVocabularyResponse' {languageCode} -> languageCode) (\s@GetMedicalVocabularyResponse' {} a -> s {languageCode = a} :: GetMedicalVocabularyResponse)

-- | If @VocabularyState@ is @FAILED@, @FailureReason@ contains information
-- about why the medical vocabulary request failed. See also:
-- <https://docs.aws.amazon.com/transcribe/latest/APIReference/CommonErrors.html Common Errors>.
getMedicalVocabularyResponse_failureReason :: Lens.Lens' GetMedicalVocabularyResponse (Prelude.Maybe Prelude.Text)
getMedicalVocabularyResponse_failureReason = Lens.lens (\GetMedicalVocabularyResponse' {failureReason} -> failureReason) (\s@GetMedicalVocabularyResponse' {} a -> s {failureReason = a} :: GetMedicalVocabularyResponse)

-- | The response's http status code.
getMedicalVocabularyResponse_httpStatus :: Lens.Lens' GetMedicalVocabularyResponse Prelude.Int
getMedicalVocabularyResponse_httpStatus = Lens.lens (\GetMedicalVocabularyResponse' {httpStatus} -> httpStatus) (\s@GetMedicalVocabularyResponse' {} a -> s {httpStatus = a} :: GetMedicalVocabularyResponse)

instance Prelude.NFData GetMedicalVocabularyResponse where
  rnf GetMedicalVocabularyResponse' {..} =
    Prelude.rnf downloadUri
      `Prelude.seq` Prelude.rnf vocabularyName
      `Prelude.seq` Prelude.rnf vocabularyState
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf httpStatus
