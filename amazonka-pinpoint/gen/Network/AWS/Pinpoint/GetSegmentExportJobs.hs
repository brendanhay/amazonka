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
-- Module      : Network.AWS.Pinpoint.GetSegmentExportJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the export jobs
-- for a segment.
module Network.AWS.Pinpoint.GetSegmentExportJobs
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSegmentExportJobs' smart constructor.
data GetSegmentExportJobs = GetSegmentExportJobs'
  { -- | The maximum number of items to include in each page of a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    pageSize :: Core.Maybe Core.Text,
    -- | The NextToken string that specifies which page of results to return in a
    -- paginated response.
    token :: Core.Maybe Core.Text,
    -- | The unique identifier for the segment.
    segmentId :: Core.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  GetSegmentExportJobs
newGetSegmentExportJobs pSegmentId_ pApplicationId_ =
  GetSegmentExportJobs'
    { pageSize = Core.Nothing,
      token = Core.Nothing,
      segmentId = pSegmentId_,
      applicationId = pApplicationId_
    }

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getSegmentExportJobs_pageSize :: Lens.Lens' GetSegmentExportJobs (Core.Maybe Core.Text)
getSegmentExportJobs_pageSize = Lens.lens (\GetSegmentExportJobs' {pageSize} -> pageSize) (\s@GetSegmentExportJobs' {} a -> s {pageSize = a} :: GetSegmentExportJobs)

-- | The NextToken string that specifies which page of results to return in a
-- paginated response.
getSegmentExportJobs_token :: Lens.Lens' GetSegmentExportJobs (Core.Maybe Core.Text)
getSegmentExportJobs_token = Lens.lens (\GetSegmentExportJobs' {token} -> token) (\s@GetSegmentExportJobs' {} a -> s {token = a} :: GetSegmentExportJobs)

-- | The unique identifier for the segment.
getSegmentExportJobs_segmentId :: Lens.Lens' GetSegmentExportJobs Core.Text
getSegmentExportJobs_segmentId = Lens.lens (\GetSegmentExportJobs' {segmentId} -> segmentId) (\s@GetSegmentExportJobs' {} a -> s {segmentId = a} :: GetSegmentExportJobs)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getSegmentExportJobs_applicationId :: Lens.Lens' GetSegmentExportJobs Core.Text
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
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetSegmentExportJobs

instance Core.NFData GetSegmentExportJobs

instance Core.ToHeaders GetSegmentExportJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetSegmentExportJobs where
  toPath GetSegmentExportJobs' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/segments/",
        Core.toBS segmentId,
        "/jobs/export"
      ]

instance Core.ToQuery GetSegmentExportJobs where
  toQuery GetSegmentExportJobs' {..} =
    Core.mconcat
      ["page-size" Core.=: pageSize, "token" Core.=: token]

-- | /See:/ 'newGetSegmentExportJobsResponse' smart constructor.
data GetSegmentExportJobsResponse = GetSegmentExportJobsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    exportJobsResponse :: ExportJobsResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
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
getSegmentExportJobsResponse_httpStatus :: Lens.Lens' GetSegmentExportJobsResponse Core.Int
getSegmentExportJobsResponse_httpStatus = Lens.lens (\GetSegmentExportJobsResponse' {httpStatus} -> httpStatus) (\s@GetSegmentExportJobsResponse' {} a -> s {httpStatus = a} :: GetSegmentExportJobsResponse)

-- | Undocumented member.
getSegmentExportJobsResponse_exportJobsResponse :: Lens.Lens' GetSegmentExportJobsResponse ExportJobsResponse
getSegmentExportJobsResponse_exportJobsResponse = Lens.lens (\GetSegmentExportJobsResponse' {exportJobsResponse} -> exportJobsResponse) (\s@GetSegmentExportJobsResponse' {} a -> s {exportJobsResponse = a} :: GetSegmentExportJobsResponse)

instance Core.NFData GetSegmentExportJobsResponse
