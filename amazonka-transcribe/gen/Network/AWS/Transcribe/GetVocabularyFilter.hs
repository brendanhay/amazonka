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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newGetVocabularyFilter' smart constructor.
data GetVocabularyFilter = GetVocabularyFilter'
  { -- | The name of the vocabulary filter for which to return information.
    vocabularyFilterName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetVocabularyFilter
newGetVocabularyFilter pVocabularyFilterName_ =
  GetVocabularyFilter'
    { vocabularyFilterName =
        pVocabularyFilterName_
    }

-- | The name of the vocabulary filter for which to return information.
getVocabularyFilter_vocabularyFilterName :: Lens.Lens' GetVocabularyFilter Core.Text
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
            Core.<$> (x Core..?> "LanguageCode")
            Core.<*> (x Core..?> "VocabularyFilterName")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "DownloadUri")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetVocabularyFilter

instance Core.NFData GetVocabularyFilter

instance Core.ToHeaders GetVocabularyFilter where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.GetVocabularyFilter" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetVocabularyFilter where
  toJSON GetVocabularyFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "VocabularyFilterName"
                  Core..= vocabularyFilterName
              )
          ]
      )

instance Core.ToPath GetVocabularyFilter where
  toPath = Core.const "/"

instance Core.ToQuery GetVocabularyFilter where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetVocabularyFilterResponse' smart constructor.
data GetVocabularyFilterResponse = GetVocabularyFilterResponse'
  { -- | The language code of the words in the vocabulary filter.
    languageCode :: Core.Maybe LanguageCode,
    -- | The name of the vocabulary filter.
    vocabularyFilterName :: Core.Maybe Core.Text,
    -- | The date and time that the contents of the vocabulary filter were
    -- updated.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The URI of the list of words in the vocabulary filter. You can use this
    -- URI to get the list of words.
    downloadUri :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetVocabularyFilterResponse
newGetVocabularyFilterResponse pHttpStatus_ =
  GetVocabularyFilterResponse'
    { languageCode =
        Core.Nothing,
      vocabularyFilterName = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      downloadUri = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The language code of the words in the vocabulary filter.
getVocabularyFilterResponse_languageCode :: Lens.Lens' GetVocabularyFilterResponse (Core.Maybe LanguageCode)
getVocabularyFilterResponse_languageCode = Lens.lens (\GetVocabularyFilterResponse' {languageCode} -> languageCode) (\s@GetVocabularyFilterResponse' {} a -> s {languageCode = a} :: GetVocabularyFilterResponse)

-- | The name of the vocabulary filter.
getVocabularyFilterResponse_vocabularyFilterName :: Lens.Lens' GetVocabularyFilterResponse (Core.Maybe Core.Text)
getVocabularyFilterResponse_vocabularyFilterName = Lens.lens (\GetVocabularyFilterResponse' {vocabularyFilterName} -> vocabularyFilterName) (\s@GetVocabularyFilterResponse' {} a -> s {vocabularyFilterName = a} :: GetVocabularyFilterResponse)

-- | The date and time that the contents of the vocabulary filter were
-- updated.
getVocabularyFilterResponse_lastModifiedTime :: Lens.Lens' GetVocabularyFilterResponse (Core.Maybe Core.UTCTime)
getVocabularyFilterResponse_lastModifiedTime = Lens.lens (\GetVocabularyFilterResponse' {lastModifiedTime} -> lastModifiedTime) (\s@GetVocabularyFilterResponse' {} a -> s {lastModifiedTime = a} :: GetVocabularyFilterResponse) Core.. Lens.mapping Core._Time

-- | The URI of the list of words in the vocabulary filter. You can use this
-- URI to get the list of words.
getVocabularyFilterResponse_downloadUri :: Lens.Lens' GetVocabularyFilterResponse (Core.Maybe Core.Text)
getVocabularyFilterResponse_downloadUri = Lens.lens (\GetVocabularyFilterResponse' {downloadUri} -> downloadUri) (\s@GetVocabularyFilterResponse' {} a -> s {downloadUri = a} :: GetVocabularyFilterResponse)

-- | The response's http status code.
getVocabularyFilterResponse_httpStatus :: Lens.Lens' GetVocabularyFilterResponse Core.Int
getVocabularyFilterResponse_httpStatus = Lens.lens (\GetVocabularyFilterResponse' {httpStatus} -> httpStatus) (\s@GetVocabularyFilterResponse' {} a -> s {httpStatus = a} :: GetVocabularyFilterResponse)

instance Core.NFData GetVocabularyFilterResponse
