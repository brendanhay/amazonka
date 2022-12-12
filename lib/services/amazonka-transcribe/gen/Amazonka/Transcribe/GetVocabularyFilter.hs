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
-- Module      : Amazonka.Transcribe.GetVocabularyFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the specified custom vocabulary filter.
--
-- To get a list of your custom vocabulary filters, use the operation.
module Amazonka.Transcribe.GetVocabularyFilter
  ( -- * Creating a Request
    GetVocabularyFilter (..),
    newGetVocabularyFilter,

    -- * Request Lenses
    getVocabularyFilter_vocabularyFilterName,

    -- * Destructuring the Response
    GetVocabularyFilterResponse (..),
    newGetVocabularyFilterResponse,

    -- * Response Lenses
    getVocabularyFilterResponse_downloadUri,
    getVocabularyFilterResponse_languageCode,
    getVocabularyFilterResponse_lastModifiedTime,
    getVocabularyFilterResponse_vocabularyFilterName,
    getVocabularyFilterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newGetVocabularyFilter' smart constructor.
data GetVocabularyFilter = GetVocabularyFilter'
  { -- | The name of the custom vocabulary filter you want information about.
    -- Custom vocabulary filter names are case sensitive.
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
-- 'vocabularyFilterName', 'getVocabularyFilter_vocabularyFilterName' - The name of the custom vocabulary filter you want information about.
-- Custom vocabulary filter names are case sensitive.
newGetVocabularyFilter ::
  -- | 'vocabularyFilterName'
  Prelude.Text ->
  GetVocabularyFilter
newGetVocabularyFilter pVocabularyFilterName_ =
  GetVocabularyFilter'
    { vocabularyFilterName =
        pVocabularyFilterName_
    }

-- | The name of the custom vocabulary filter you want information about.
-- Custom vocabulary filter names are case sensitive.
getVocabularyFilter_vocabularyFilterName :: Lens.Lens' GetVocabularyFilter Prelude.Text
getVocabularyFilter_vocabularyFilterName = Lens.lens (\GetVocabularyFilter' {vocabularyFilterName} -> vocabularyFilterName) (\s@GetVocabularyFilter' {} a -> s {vocabularyFilterName = a} :: GetVocabularyFilter)

instance Core.AWSRequest GetVocabularyFilter where
  type
    AWSResponse GetVocabularyFilter =
      GetVocabularyFilterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVocabularyFilterResponse'
            Prelude.<$> (x Data..?> "DownloadUri")
            Prelude.<*> (x Data..?> "LanguageCode")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "VocabularyFilterName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetVocabularyFilter where
  hashWithSalt _salt GetVocabularyFilter' {..} =
    _salt `Prelude.hashWithSalt` vocabularyFilterName

instance Prelude.NFData GetVocabularyFilter where
  rnf GetVocabularyFilter' {..} =
    Prelude.rnf vocabularyFilterName

instance Data.ToHeaders GetVocabularyFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Transcribe.GetVocabularyFilter" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetVocabularyFilter where
  toJSON GetVocabularyFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "VocabularyFilterName"
                  Data..= vocabularyFilterName
              )
          ]
      )

instance Data.ToPath GetVocabularyFilter where
  toPath = Prelude.const "/"

instance Data.ToQuery GetVocabularyFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetVocabularyFilterResponse' smart constructor.
data GetVocabularyFilterResponse = GetVocabularyFilterResponse'
  { -- | The Amazon S3 location where the custom vocabulary filter is stored; use
    -- this URI to view or download the custom vocabulary filter.
    downloadUri :: Prelude.Maybe Prelude.Text,
    -- | The language code you selected for your custom vocabulary filter.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The date and time the specified custom vocabulary filter was last
    -- modified.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
    -- May 4, 2022.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the custom vocabulary filter you requested information
    -- about.
    vocabularyFilterName :: Prelude.Maybe Prelude.Text,
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
-- 'downloadUri', 'getVocabularyFilterResponse_downloadUri' - The Amazon S3 location where the custom vocabulary filter is stored; use
-- this URI to view or download the custom vocabulary filter.
--
-- 'languageCode', 'getVocabularyFilterResponse_languageCode' - The language code you selected for your custom vocabulary filter.
--
-- 'lastModifiedTime', 'getVocabularyFilterResponse_lastModifiedTime' - The date and time the specified custom vocabulary filter was last
-- modified.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
--
-- 'vocabularyFilterName', 'getVocabularyFilterResponse_vocabularyFilterName' - The name of the custom vocabulary filter you requested information
-- about.
--
-- 'httpStatus', 'getVocabularyFilterResponse_httpStatus' - The response's http status code.
newGetVocabularyFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVocabularyFilterResponse
newGetVocabularyFilterResponse pHttpStatus_ =
  GetVocabularyFilterResponse'
    { downloadUri =
        Prelude.Nothing,
      languageCode = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      vocabularyFilterName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon S3 location where the custom vocabulary filter is stored; use
-- this URI to view or download the custom vocabulary filter.
getVocabularyFilterResponse_downloadUri :: Lens.Lens' GetVocabularyFilterResponse (Prelude.Maybe Prelude.Text)
getVocabularyFilterResponse_downloadUri = Lens.lens (\GetVocabularyFilterResponse' {downloadUri} -> downloadUri) (\s@GetVocabularyFilterResponse' {} a -> s {downloadUri = a} :: GetVocabularyFilterResponse)

-- | The language code you selected for your custom vocabulary filter.
getVocabularyFilterResponse_languageCode :: Lens.Lens' GetVocabularyFilterResponse (Prelude.Maybe LanguageCode)
getVocabularyFilterResponse_languageCode = Lens.lens (\GetVocabularyFilterResponse' {languageCode} -> languageCode) (\s@GetVocabularyFilterResponse' {} a -> s {languageCode = a} :: GetVocabularyFilterResponse)

-- | The date and time the specified custom vocabulary filter was last
-- modified.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
getVocabularyFilterResponse_lastModifiedTime :: Lens.Lens' GetVocabularyFilterResponse (Prelude.Maybe Prelude.UTCTime)
getVocabularyFilterResponse_lastModifiedTime = Lens.lens (\GetVocabularyFilterResponse' {lastModifiedTime} -> lastModifiedTime) (\s@GetVocabularyFilterResponse' {} a -> s {lastModifiedTime = a} :: GetVocabularyFilterResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the custom vocabulary filter you requested information
-- about.
getVocabularyFilterResponse_vocabularyFilterName :: Lens.Lens' GetVocabularyFilterResponse (Prelude.Maybe Prelude.Text)
getVocabularyFilterResponse_vocabularyFilterName = Lens.lens (\GetVocabularyFilterResponse' {vocabularyFilterName} -> vocabularyFilterName) (\s@GetVocabularyFilterResponse' {} a -> s {vocabularyFilterName = a} :: GetVocabularyFilterResponse)

-- | The response's http status code.
getVocabularyFilterResponse_httpStatus :: Lens.Lens' GetVocabularyFilterResponse Prelude.Int
getVocabularyFilterResponse_httpStatus = Lens.lens (\GetVocabularyFilterResponse' {httpStatus} -> httpStatus) (\s@GetVocabularyFilterResponse' {} a -> s {httpStatus = a} :: GetVocabularyFilterResponse)

instance Prelude.NFData GetVocabularyFilterResponse where
  rnf GetVocabularyFilterResponse' {..} =
    Prelude.rnf downloadUri
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf vocabularyFilterName
      `Prelude.seq` Prelude.rnf httpStatus
