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
-- Module      : Network.AWS.Transcribe.GetMedicalVocabulary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a medical vocabulary.
module Network.AWS.Transcribe.GetMedicalVocabulary
  ( -- * Creating a Request
    GetMedicalVocabulary (..),
    newGetMedicalVocabulary,

    -- * Request Lenses
    getMedicalVocabulary_vocabularyName,

    -- * Destructuring the Response
    GetMedicalVocabularyResponse (..),
    newGetMedicalVocabularyResponse,

    -- * Response Lenses
    getMedicalVocabularyResponse_languageCode,
    getMedicalVocabularyResponse_failureReason,
    getMedicalVocabularyResponse_lastModifiedTime,
    getMedicalVocabularyResponse_vocabularyState,
    getMedicalVocabularyResponse_vocabularyName,
    getMedicalVocabularyResponse_downloadUri,
    getMedicalVocabularyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newGetMedicalVocabulary' smart constructor.
data GetMedicalVocabulary = GetMedicalVocabulary'
  { -- | The name of the vocabulary that you want information about. The value is
    -- case sensitive.
    vocabularyName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetMedicalVocabulary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vocabularyName', 'getMedicalVocabulary_vocabularyName' - The name of the vocabulary that you want information about. The value is
-- case sensitive.
newGetMedicalVocabulary ::
  -- | 'vocabularyName'
  Core.Text ->
  GetMedicalVocabulary
newGetMedicalVocabulary pVocabularyName_ =
  GetMedicalVocabulary'
    { vocabularyName =
        pVocabularyName_
    }

-- | The name of the vocabulary that you want information about. The value is
-- case sensitive.
getMedicalVocabulary_vocabularyName :: Lens.Lens' GetMedicalVocabulary Core.Text
getMedicalVocabulary_vocabularyName = Lens.lens (\GetMedicalVocabulary' {vocabularyName} -> vocabularyName) (\s@GetMedicalVocabulary' {} a -> s {vocabularyName = a} :: GetMedicalVocabulary)

instance Core.AWSRequest GetMedicalVocabulary where
  type
    AWSResponse GetMedicalVocabulary =
      GetMedicalVocabularyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMedicalVocabularyResponse'
            Core.<$> (x Core..?> "LanguageCode")
            Core.<*> (x Core..?> "FailureReason")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "VocabularyState")
            Core.<*> (x Core..?> "VocabularyName")
            Core.<*> (x Core..?> "DownloadUri")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetMedicalVocabulary

instance Core.NFData GetMedicalVocabulary

instance Core.ToHeaders GetMedicalVocabulary where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.GetMedicalVocabulary" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetMedicalVocabulary where
  toJSON GetMedicalVocabulary' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("VocabularyName" Core..= vocabularyName)
          ]
      )

instance Core.ToPath GetMedicalVocabulary where
  toPath = Core.const "/"

instance Core.ToQuery GetMedicalVocabulary where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetMedicalVocabularyResponse' smart constructor.
data GetMedicalVocabularyResponse = GetMedicalVocabularyResponse'
  { -- | The valid language code for your vocabulary entries.
    languageCode :: Core.Maybe LanguageCode,
    -- | If the @VocabularyState@ is @FAILED@, this field contains information
    -- about why the job failed.
    failureReason :: Core.Maybe Core.Text,
    -- | The date and time that the vocabulary was last modified with a text file
    -- different from the one that was previously used.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The processing state of the vocabulary. If the @VocabularyState@ is
    -- @READY@ then you can use it in the @StartMedicalTranscriptionJob@
    -- operation.
    vocabularyState :: Core.Maybe VocabularyState,
    -- | The name of the vocabulary returned by Amazon Transcribe Medical.
    vocabularyName :: Core.Maybe Core.Text,
    -- | The location in Amazon S3 where the vocabulary is stored. Use this URI
    -- to get the contents of the vocabulary. You can download your vocabulary
    -- from the URI for a limited time.
    downloadUri :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetMedicalVocabularyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'getMedicalVocabularyResponse_languageCode' - The valid language code for your vocabulary entries.
--
-- 'failureReason', 'getMedicalVocabularyResponse_failureReason' - If the @VocabularyState@ is @FAILED@, this field contains information
-- about why the job failed.
--
-- 'lastModifiedTime', 'getMedicalVocabularyResponse_lastModifiedTime' - The date and time that the vocabulary was last modified with a text file
-- different from the one that was previously used.
--
-- 'vocabularyState', 'getMedicalVocabularyResponse_vocabularyState' - The processing state of the vocabulary. If the @VocabularyState@ is
-- @READY@ then you can use it in the @StartMedicalTranscriptionJob@
-- operation.
--
-- 'vocabularyName', 'getMedicalVocabularyResponse_vocabularyName' - The name of the vocabulary returned by Amazon Transcribe Medical.
--
-- 'downloadUri', 'getMedicalVocabularyResponse_downloadUri' - The location in Amazon S3 where the vocabulary is stored. Use this URI
-- to get the contents of the vocabulary. You can download your vocabulary
-- from the URI for a limited time.
--
-- 'httpStatus', 'getMedicalVocabularyResponse_httpStatus' - The response's http status code.
newGetMedicalVocabularyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetMedicalVocabularyResponse
newGetMedicalVocabularyResponse pHttpStatus_ =
  GetMedicalVocabularyResponse'
    { languageCode =
        Core.Nothing,
      failureReason = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      vocabularyState = Core.Nothing,
      vocabularyName = Core.Nothing,
      downloadUri = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The valid language code for your vocabulary entries.
getMedicalVocabularyResponse_languageCode :: Lens.Lens' GetMedicalVocabularyResponse (Core.Maybe LanguageCode)
getMedicalVocabularyResponse_languageCode = Lens.lens (\GetMedicalVocabularyResponse' {languageCode} -> languageCode) (\s@GetMedicalVocabularyResponse' {} a -> s {languageCode = a} :: GetMedicalVocabularyResponse)

-- | If the @VocabularyState@ is @FAILED@, this field contains information
-- about why the job failed.
getMedicalVocabularyResponse_failureReason :: Lens.Lens' GetMedicalVocabularyResponse (Core.Maybe Core.Text)
getMedicalVocabularyResponse_failureReason = Lens.lens (\GetMedicalVocabularyResponse' {failureReason} -> failureReason) (\s@GetMedicalVocabularyResponse' {} a -> s {failureReason = a} :: GetMedicalVocabularyResponse)

-- | The date and time that the vocabulary was last modified with a text file
-- different from the one that was previously used.
getMedicalVocabularyResponse_lastModifiedTime :: Lens.Lens' GetMedicalVocabularyResponse (Core.Maybe Core.UTCTime)
getMedicalVocabularyResponse_lastModifiedTime = Lens.lens (\GetMedicalVocabularyResponse' {lastModifiedTime} -> lastModifiedTime) (\s@GetMedicalVocabularyResponse' {} a -> s {lastModifiedTime = a} :: GetMedicalVocabularyResponse) Core.. Lens.mapping Core._Time

-- | The processing state of the vocabulary. If the @VocabularyState@ is
-- @READY@ then you can use it in the @StartMedicalTranscriptionJob@
-- operation.
getMedicalVocabularyResponse_vocabularyState :: Lens.Lens' GetMedicalVocabularyResponse (Core.Maybe VocabularyState)
getMedicalVocabularyResponse_vocabularyState = Lens.lens (\GetMedicalVocabularyResponse' {vocabularyState} -> vocabularyState) (\s@GetMedicalVocabularyResponse' {} a -> s {vocabularyState = a} :: GetMedicalVocabularyResponse)

-- | The name of the vocabulary returned by Amazon Transcribe Medical.
getMedicalVocabularyResponse_vocabularyName :: Lens.Lens' GetMedicalVocabularyResponse (Core.Maybe Core.Text)
getMedicalVocabularyResponse_vocabularyName = Lens.lens (\GetMedicalVocabularyResponse' {vocabularyName} -> vocabularyName) (\s@GetMedicalVocabularyResponse' {} a -> s {vocabularyName = a} :: GetMedicalVocabularyResponse)

-- | The location in Amazon S3 where the vocabulary is stored. Use this URI
-- to get the contents of the vocabulary. You can download your vocabulary
-- from the URI for a limited time.
getMedicalVocabularyResponse_downloadUri :: Lens.Lens' GetMedicalVocabularyResponse (Core.Maybe Core.Text)
getMedicalVocabularyResponse_downloadUri = Lens.lens (\GetMedicalVocabularyResponse' {downloadUri} -> downloadUri) (\s@GetMedicalVocabularyResponse' {} a -> s {downloadUri = a} :: GetMedicalVocabularyResponse)

-- | The response's http status code.
getMedicalVocabularyResponse_httpStatus :: Lens.Lens' GetMedicalVocabularyResponse Core.Int
getMedicalVocabularyResponse_httpStatus = Lens.lens (\GetMedicalVocabularyResponse' {httpStatus} -> httpStatus) (\s@GetMedicalVocabularyResponse' {} a -> s {httpStatus = a} :: GetMedicalVocabularyResponse)

instance Core.NFData GetMedicalVocabularyResponse
