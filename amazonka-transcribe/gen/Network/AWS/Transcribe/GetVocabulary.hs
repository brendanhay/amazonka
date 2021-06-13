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
-- Module      : Network.AWS.Transcribe.GetVocabulary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a vocabulary.
module Network.AWS.Transcribe.GetVocabulary
  ( -- * Creating a Request
    GetVocabulary (..),
    newGetVocabulary,

    -- * Request Lenses
    getVocabulary_vocabularyName,

    -- * Destructuring the Response
    GetVocabularyResponse (..),
    newGetVocabularyResponse,

    -- * Response Lenses
    getVocabularyResponse_languageCode,
    getVocabularyResponse_failureReason,
    getVocabularyResponse_lastModifiedTime,
    getVocabularyResponse_vocabularyState,
    getVocabularyResponse_vocabularyName,
    getVocabularyResponse_downloadUri,
    getVocabularyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newGetVocabulary' smart constructor.
data GetVocabulary = GetVocabulary'
  { -- | The name of the vocabulary to return information about. The name is case
    -- sensitive.
    vocabularyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVocabulary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vocabularyName', 'getVocabulary_vocabularyName' - The name of the vocabulary to return information about. The name is case
-- sensitive.
newGetVocabulary ::
  -- | 'vocabularyName'
  Prelude.Text ->
  GetVocabulary
newGetVocabulary pVocabularyName_ =
  GetVocabulary' {vocabularyName = pVocabularyName_}

-- | The name of the vocabulary to return information about. The name is case
-- sensitive.
getVocabulary_vocabularyName :: Lens.Lens' GetVocabulary Prelude.Text
getVocabulary_vocabularyName = Lens.lens (\GetVocabulary' {vocabularyName} -> vocabularyName) (\s@GetVocabulary' {} a -> s {vocabularyName = a} :: GetVocabulary)

instance Core.AWSRequest GetVocabulary where
  type
    AWSResponse GetVocabulary =
      GetVocabularyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVocabularyResponse'
            Prelude.<$> (x Core..?> "LanguageCode")
            Prelude.<*> (x Core..?> "FailureReason")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "VocabularyState")
            Prelude.<*> (x Core..?> "VocabularyName")
            Prelude.<*> (x Core..?> "DownloadUri")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetVocabulary

instance Prelude.NFData GetVocabulary

instance Core.ToHeaders GetVocabulary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("Transcribe.GetVocabulary" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetVocabulary where
  toJSON GetVocabulary' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("VocabularyName" Core..= vocabularyName)
          ]
      )

instance Core.ToPath GetVocabulary where
  toPath = Prelude.const "/"

instance Core.ToQuery GetVocabulary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetVocabularyResponse' smart constructor.
data GetVocabularyResponse = GetVocabularyResponse'
  { -- | The language code of the vocabulary entries.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | If the @VocabularyState@ field is @FAILED@, this field contains
    -- information about why the job failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the vocabulary was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The processing state of the vocabulary.
    vocabularyState :: Prelude.Maybe VocabularyState,
    -- | The name of the vocabulary to return.
    vocabularyName :: Prelude.Maybe Prelude.Text,
    -- | The S3 location where the vocabulary is stored. Use this URI to get the
    -- contents of the vocabulary. The URI is available for a limited time.
    downloadUri :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVocabularyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'getVocabularyResponse_languageCode' - The language code of the vocabulary entries.
--
-- 'failureReason', 'getVocabularyResponse_failureReason' - If the @VocabularyState@ field is @FAILED@, this field contains
-- information about why the job failed.
--
-- 'lastModifiedTime', 'getVocabularyResponse_lastModifiedTime' - The date and time that the vocabulary was last modified.
--
-- 'vocabularyState', 'getVocabularyResponse_vocabularyState' - The processing state of the vocabulary.
--
-- 'vocabularyName', 'getVocabularyResponse_vocabularyName' - The name of the vocabulary to return.
--
-- 'downloadUri', 'getVocabularyResponse_downloadUri' - The S3 location where the vocabulary is stored. Use this URI to get the
-- contents of the vocabulary. The URI is available for a limited time.
--
-- 'httpStatus', 'getVocabularyResponse_httpStatus' - The response's http status code.
newGetVocabularyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVocabularyResponse
newGetVocabularyResponse pHttpStatus_ =
  GetVocabularyResponse'
    { languageCode =
        Prelude.Nothing,
      failureReason = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      vocabularyState = Prelude.Nothing,
      vocabularyName = Prelude.Nothing,
      downloadUri = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The language code of the vocabulary entries.
getVocabularyResponse_languageCode :: Lens.Lens' GetVocabularyResponse (Prelude.Maybe LanguageCode)
getVocabularyResponse_languageCode = Lens.lens (\GetVocabularyResponse' {languageCode} -> languageCode) (\s@GetVocabularyResponse' {} a -> s {languageCode = a} :: GetVocabularyResponse)

-- | If the @VocabularyState@ field is @FAILED@, this field contains
-- information about why the job failed.
getVocabularyResponse_failureReason :: Lens.Lens' GetVocabularyResponse (Prelude.Maybe Prelude.Text)
getVocabularyResponse_failureReason = Lens.lens (\GetVocabularyResponse' {failureReason} -> failureReason) (\s@GetVocabularyResponse' {} a -> s {failureReason = a} :: GetVocabularyResponse)

-- | The date and time that the vocabulary was last modified.
getVocabularyResponse_lastModifiedTime :: Lens.Lens' GetVocabularyResponse (Prelude.Maybe Prelude.UTCTime)
getVocabularyResponse_lastModifiedTime = Lens.lens (\GetVocabularyResponse' {lastModifiedTime} -> lastModifiedTime) (\s@GetVocabularyResponse' {} a -> s {lastModifiedTime = a} :: GetVocabularyResponse) Prelude.. Lens.mapping Core._Time

-- | The processing state of the vocabulary.
getVocabularyResponse_vocabularyState :: Lens.Lens' GetVocabularyResponse (Prelude.Maybe VocabularyState)
getVocabularyResponse_vocabularyState = Lens.lens (\GetVocabularyResponse' {vocabularyState} -> vocabularyState) (\s@GetVocabularyResponse' {} a -> s {vocabularyState = a} :: GetVocabularyResponse)

-- | The name of the vocabulary to return.
getVocabularyResponse_vocabularyName :: Lens.Lens' GetVocabularyResponse (Prelude.Maybe Prelude.Text)
getVocabularyResponse_vocabularyName = Lens.lens (\GetVocabularyResponse' {vocabularyName} -> vocabularyName) (\s@GetVocabularyResponse' {} a -> s {vocabularyName = a} :: GetVocabularyResponse)

-- | The S3 location where the vocabulary is stored. Use this URI to get the
-- contents of the vocabulary. The URI is available for a limited time.
getVocabularyResponse_downloadUri :: Lens.Lens' GetVocabularyResponse (Prelude.Maybe Prelude.Text)
getVocabularyResponse_downloadUri = Lens.lens (\GetVocabularyResponse' {downloadUri} -> downloadUri) (\s@GetVocabularyResponse' {} a -> s {downloadUri = a} :: GetVocabularyResponse)

-- | The response's http status code.
getVocabularyResponse_httpStatus :: Lens.Lens' GetVocabularyResponse Prelude.Int
getVocabularyResponse_httpStatus = Lens.lens (\GetVocabularyResponse' {httpStatus} -> httpStatus) (\s@GetVocabularyResponse' {} a -> s {httpStatus = a} :: GetVocabularyResponse)

instance Prelude.NFData GetVocabularyResponse
