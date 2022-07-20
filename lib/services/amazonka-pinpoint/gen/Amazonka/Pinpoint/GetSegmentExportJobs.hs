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
-- Module      : Amazonka.Pinpoint.GetSegmentExportJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the export jobs
-- for a segment.
module Amazonka.Pinpoint.GetSegmentExportJobs
  ( -- * Creating a Request
    GetSegmentExportJobs (..),
    newGetSegmentExportJobs,

    -- * Request Lenses
    getSegmentExportJobs_pageSize,
    getSegmentExportJobs_token,
    getSegmentExportJobs_segmentId,
    getSegmentExportJobs_applicationId,

    -- * Destructuring the Response
    GetSegmentExportJobsResponse (..),
    newGetSegmentExportJobsResponse,

    -- * Response Lenses
    getSegmentExportJobsResponse_httpStatus,
    getSegmentExportJobsResponse_exportJobsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSegmentExportJobs' smart constructor.
data GetSegmentExportJobs = GetSegmentExportJobs'
  { -- | The maximum number of items to include in each page of a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    pageSize :: Prelude.Maybe Prelude.Text,
    -- | The NextToken string that specifies which page of results to return in a
    -- paginated response.
    token :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the segment.
    segmentId :: Prelude.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSegmentExportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'getSegmentExportJobs_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'token', 'getSegmentExportJobs_token' - The NextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'segmentId', 'getSegmentExportJobs_segmentId' - The unique identifier for the segment.
--
-- 'applicationId', 'getSegmentExportJobs_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetSegmentExportJobs ::
  -- | 'segmentId'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  GetSegmentExportJobs
newGetSegmentExportJobs pSegmentId_ pApplicationId_ =
  GetSegmentExportJobs'
    { pageSize = Prelude.Nothing,
      token = Prelude.Nothing,
      segmentId = pSegmentId_,
      applicationId = pApplicationId_
    }

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getSegmentExportJobs_pageSize :: Lens.Lens' GetSegmentExportJobs (Prelude.Maybe Prelude.Text)
getSegmentExportJobs_pageSize = Lens.lens (\GetSegmentExportJobs' {pageSize} -> pageSize) (\s@GetSegmentExportJobs' {} a -> s {pageSize = a} :: GetSegmentExportJobs)

-- | The NextToken string that specifies which page of results to return in a
-- paginated response.
getSegmentExportJobs_token :: Lens.Lens' GetSegmentExportJobs (Prelude.Maybe Prelude.Text)
getSegmentExportJobs_token = Lens.lens (\GetSegmentExportJobs' {token} -> token) (\s@GetSegmentExportJobs' {} a -> s {token = a} :: GetSegmentExportJobs)

-- | The unique identifier for the segment.
getSegmentExportJobs_segmentId :: Lens.Lens' GetSegmentExportJobs Prelude.Text
getSegmentExportJobs_segmentId = Lens.lens (\GetSegmentExportJobs' {segmentId} -> segmentId) (\s@GetSegmentExportJobs' {} a -> s {segmentId = a} :: GetSegmentExportJobs)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getSegmentExportJobs_applicationId :: Lens.Lens' GetSegmentExportJobs Prelude.Text
getSegmentExportJobs_applicationId = Lens.lens (\GetSegmentExportJobs' {applicationId} -> applicationId) (\s@GetSegmentExportJobs' {} a -> s {applicationId = a} :: GetSegmentExportJobs)

instance Core.AWSRequest GetSegmentExportJobs where
  type
    AWSResponse GetSegmentExportJobs =
      GetSegmentExportJobsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSegmentExportJobsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable GetSegmentExportJobs where
  hashWithSalt _salt GetSegmentExportJobs' {..} =
    _salt `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` token
      `Prelude.hashWithSalt` segmentId
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData GetSegmentExportJobs where
  rnf GetSegmentExportJobs' {..} =
    Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf token
      `Prelude.seq` Prelude.rnf segmentId
      `Prelude.seq` Prelude.rnf applicationId

instance Core.ToHeaders GetSegmentExportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetSegmentExportJobs where
  toPath GetSegmentExportJobs' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/segments/",
        Core.toBS segmentId,
        "/jobs/export"
      ]

instance Core.ToQuery GetSegmentExportJobs where
  toQuery GetSegmentExportJobs' {..} =
    Prelude.mconcat
      ["page-size" Core.=: pageSize, "token" Core.=: token]

-- | /See:/ 'newGetSegmentExportJobsResponse' smart constructor.
data GetSegmentExportJobsResponse = GetSegmentExportJobsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    exportJobsResponse :: ExportJobsResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSegmentExportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getSegmentExportJobsResponse_httpStatus' - The response's http status code.
--
-- 'exportJobsResponse', 'getSegmentExportJobsResponse_exportJobsResponse' - Undocumented member.
newGetSegmentExportJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'exportJobsResponse'
  ExportJobsResponse ->
  GetSegmentExportJobsResponse
newGetSegmentExportJobsResponse
  pHttpStatus_
  pExportJobsResponse_ =
    GetSegmentExportJobsResponse'
      { httpStatus =
          pHttpStatus_,
        exportJobsResponse = pExportJobsResponse_
      }

-- | The response's http status code.
getSegmentExportJobsResponse_httpStatus :: Lens.Lens' GetSegmentExportJobsResponse Prelude.Int
getSegmentExportJobsResponse_httpStatus = Lens.lens (\GetSegmentExportJobsResponse' {httpStatus} -> httpStatus) (\s@GetSegmentExportJobsResponse' {} a -> s {httpStatus = a} :: GetSegmentExportJobsResponse)

-- | Undocumented member.
getSegmentExportJobsResponse_exportJobsResponse :: Lens.Lens' GetSegmentExportJobsResponse ExportJobsResponse
getSegmentExportJobsResponse_exportJobsResponse = Lens.lens (\GetSegmentExportJobsResponse' {exportJobsResponse} -> exportJobsResponse) (\s@GetSegmentExportJobsResponse' {} a -> s {exportJobsResponse = a} :: GetSegmentExportJobsResponse)

instance Prelude.NFData GetSegmentExportJobsResponse where
  rnf GetSegmentExportJobsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf exportJobsResponse
