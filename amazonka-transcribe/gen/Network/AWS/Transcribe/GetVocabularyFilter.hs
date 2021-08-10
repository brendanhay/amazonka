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
-- Module      : Network.AWS.Transcribe.GetVocabularyFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a vocabulary filter.
module Network.AWS.Transcribe.GetVocabularyFilter
  ( -- * Creating a Request
    GetVocabularyFilter (..),
    newGetVocabularyFilter,

    -- * Request Lenses
    getVocabularyFilter_vocabularyFilterName,

    -- * Destructuring the Response
    GetVocabularyFilterResponse (..),
    newGetVocabularyFilterResponse,

    -- * Response Lenses
    getVocabularyFilterResponse_languageCode,
    getVocabularyFilterResponse_vocabularyFilterName,
    getVocabularyFilterResponse_lastModifiedTime,
    getVocabularyFilterResponse_downloadUri,
    getVocabularyFilterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newGetVocabularyFilter' smart constructor.
data GetVocabularyFilter = GetVocabularyFilter'
  { -- | The name of the vocabulary filter for which to return information.
    vocabularyFilterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVocabularyFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vocabularyFilterName', 'getVocabularyFilter_vocabularyFilterName' - The name of the vocabulary filter for which to return information.
newGetVocabularyFilter ::
  -- | 'vocabularyFilterName'
  Prelude.Text ->
  GetVocabularyFilter
newGetVocabularyFilter pVocabularyFilterName_ =
  GetVocabularyFilter'
    { vocabularyFilterName =
        pVocabularyFilterName_
    }

-- | The name of the vocabulary filter for which to return information.
getVocabularyFilter_vocabularyFilterName :: Lens.Lens' GetVocabularyFilter Prelude.Text
getVocabularyFilter_vocabularyFilterName = Lens.lens (\GetVocabularyFilter' {vocabularyFilterName} -> vocabularyFilterName) (\s@GetVocabularyFilter' {} a -> s {vocabularyFilterName = a} :: GetVocabularyFilter)

instance Core.AWSRequest GetVocabularyFilter where
  type
    AWSResponse GetVocabularyFilter =
      GetVocabularyFilterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVocabularyFilterResponse'
            Prelude.<$> (x Core..?> "LanguageCode")
            Prelude.<*> (x Core..?> "VocabularyFilterName")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "DownloadUri")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetVocabularyFilter

instance Prelude.NFData GetVocabularyFilter

instance Core.ToHeaders GetVocabularyFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.GetVocabularyFilter" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetVocabularyFilter where
  toJSON GetVocabularyFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "VocabularyFilterName"
                  Core..= vocabularyFilterName
              )
          ]
      )

instance Core.ToPath GetVocabularyFilter where
  toPath = Prelude.const "/"

instance Core.ToQuery GetVocabularyFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetVocabularyFilterResponse' smart constructor.
data GetVocabularyFilterResponse = GetVocabularyFilterResponse'
  { -- | The language code of the words in the vocabulary filter.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The name of the vocabulary filter.
    vocabularyFilterName :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the contents of the vocabulary filter were
    -- updated.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The URI of the list of words in the vocabulary filter. You can use this
    -- URI to get the list of words.
    downloadUri :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVocabularyFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'getVocabularyFilterResponse_languageCode' - The language code of the words in the vocabulary filter.
--
-- 'vocabularyFilterName', 'getVocabularyFilterResponse_vocabularyFilterName' - The name of the vocabulary filter.
--
-- 'lastModifiedTime', 'getVocabularyFilterResponse_lastModifiedTime' - The date and time that the contents of the vocabulary filter were
-- updated.
--
-- 'downloadUri', 'getVocabularyFilterResponse_downloadUri' - The URI of the list of words in the vocabulary filter. You can use this
-- URI to get the list of words.
--
-- 'httpStatus', 'getVocabularyFilterResponse_httpStatus' - The response's http status code.
newGetVocabularyFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVocabularyFilterResponse
newGetVocabularyFilterResponse pHttpStatus_ =
  GetVocabularyFilterResponse'
    { languageCode =
        Prelude.Nothing,
      vocabularyFilterName = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      downloadUri = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The language code of the words in the vocabulary filter.
getVocabularyFilterResponse_languageCode :: Lens.Lens' GetVocabularyFilterResponse (Prelude.Maybe LanguageCode)
getVocabularyFilterResponse_languageCode = Lens.lens (\GetVocabularyFilterResponse' {languageCode} -> languageCode) (\s@GetVocabularyFilterResponse' {} a -> s {languageCode = a} :: GetVocabularyFilterResponse)

-- | The name of the vocabulary filter.
getVocabularyFilterResponse_vocabularyFilterName :: Lens.Lens' GetVocabularyFilterResponse (Prelude.Maybe Prelude.Text)
getVocabularyFilterResponse_vocabularyFilterName = Lens.lens (\GetVocabularyFilterResponse' {vocabularyFilterName} -> vocabularyFilterName) (\s@GetVocabularyFilterResponse' {} a -> s {vocabularyFilterName = a} :: GetVocabularyFilterResponse)

-- | The date and time that the contents of the vocabulary filter were
-- updated.
getVocabularyFilterResponse_lastModifiedTime :: Lens.Lens' GetVocabularyFilterResponse (Prelude.Maybe Prelude.UTCTime)
getVocabularyFilterResponse_lastModifiedTime = Lens.lens (\GetVocabularyFilterResponse' {lastModifiedTime} -> lastModifiedTime) (\s@GetVocabularyFilterResponse' {} a -> s {lastModifiedTime = a} :: GetVocabularyFilterResponse) Prelude.. Lens.mapping Core._Time

-- | The URI of the list of words in the vocabulary filter. You can use this
-- URI to get the list of words.
getVocabularyFilterResponse_downloadUri :: Lens.Lens' GetVocabularyFilterResponse (Prelude.Maybe Prelude.Text)
getVocabularyFilterResponse_downloadUri = Lens.lens (\GetVocabularyFilterResponse' {downloadUri} -> downloadUri) (\s@GetVocabularyFilterResponse' {} a -> s {downloadUri = a} :: GetVocabularyFilterResponse)

-- | The response's http status code.
getVocabularyFilterResponse_httpStatus :: Lens.Lens' GetVocabularyFilterResponse Prelude.Int
getVocabularyFilterResponse_httpStatus = Lens.lens (\GetVocabularyFilterResponse' {httpStatus} -> httpStatus) (\s@GetVocabularyFilterResponse' {} a -> s {httpStatus = a} :: GetVocabularyFilterResponse)

instance Prelude.NFData GetVocabularyFilterResponse
