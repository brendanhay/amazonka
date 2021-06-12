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
-- Module      : Network.AWS.Pinpoint.GetExportJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of all the export
-- jobs for an application.
module Network.AWS.Pinpoint.GetExportJobs
  ( -- * Creating a Request
    GetExportJobs (..),
    newGetExportJobs,

    -- * Request Lenses
    getExportJobs_pageSize,
    getExportJobs_token,
    getExportJobs_applicationId,

    -- * Destructuring the Response
    GetExportJobsResponse (..),
    newGetExportJobsResponse,

    -- * Response Lenses
    getExportJobsResponse_httpStatus,
    getExportJobsResponse_exportJobsResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetExportJobs' smart constructor.
data GetExportJobs = GetExportJobs'
  { -- | The maximum number of items to include in each page of a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    pageSize :: Core.Maybe Core.Text,
    -- | The NextToken string that specifies which page of results to return in a
    -- paginated response.
    token :: Core.Maybe Core.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetExportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'getExportJobs_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'token', 'getExportJobs_token' - The NextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'applicationId', 'getExportJobs_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetExportJobs ::
  -- | 'applicationId'
  Core.Text ->
  GetExportJobs
newGetExportJobs pApplicationId_ =
  GetExportJobs'
    { pageSize = Core.Nothing,
      token = Core.Nothing,
      applicationId = pApplicationId_
    }

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getExportJobs_pageSize :: Lens.Lens' GetExportJobs (Core.Maybe Core.Text)
getExportJobs_pageSize = Lens.lens (\GetExportJobs' {pageSize} -> pageSize) (\s@GetExportJobs' {} a -> s {pageSize = a} :: GetExportJobs)

-- | The NextToken string that specifies which page of results to return in a
-- paginated response.
getExportJobs_token :: Lens.Lens' GetExportJobs (Core.Maybe Core.Text)
getExportJobs_token = Lens.lens (\GetExportJobs' {token} -> token) (\s@GetExportJobs' {} a -> s {token = a} :: GetExportJobs)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getExportJobs_applicationId :: Lens.Lens' GetExportJobs Core.Text
getExportJobs_applicationId = Lens.lens (\GetExportJobs' {applicationId} -> applicationId) (\s@GetExportJobs' {} a -> s {applicationId = a} :: GetExportJobs)

instance Core.AWSRequest GetExportJobs where
  type
    AWSResponse GetExportJobs =
      GetExportJobsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetExportJobsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetExportJobs

instance Core.NFData GetExportJobs

instance Core.ToHeaders GetExportJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetExportJobs where
  toPath GetExportJobs' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/jobs/export"
      ]

instance Core.ToQuery GetExportJobs where
  toQuery GetExportJobs' {..} =
    Core.mconcat
      ["page-size" Core.=: pageSize, "token" Core.=: token]

-- | /See:/ 'newGetExportJobsResponse' smart constructor.
data GetExportJobsResponse = GetExportJobsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    exportJobsResponse :: ExportJobsResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetExportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getExportJobsResponse_httpStatus' - The response's http status code.
--
-- 'exportJobsResponse', 'getExportJobsResponse_exportJobsResponse' - Undocumented member.
newGetExportJobsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'exportJobsResponse'
  ExportJobsResponse ->
  GetExportJobsResponse
newGetExportJobsResponse
  pHttpStatus_
  pExportJobsResponse_ =
    GetExportJobsResponse'
      { httpStatus = pHttpStatus_,
        exportJobsResponse = pExportJobsResponse_
      }

-- | The response's http status code.
getExportJobsResponse_httpStatus :: Lens.Lens' GetExportJobsResponse Core.Int
getExportJobsResponse_httpStatus = Lens.lens (\GetExportJobsResponse' {httpStatus} -> httpStatus) (\s@GetExportJobsResponse' {} a -> s {httpStatus = a} :: GetExportJobsResponse)

-- | Undocumented member.
getExportJobsResponse_exportJobsResponse :: Lens.Lens' GetExportJobsResponse ExportJobsResponse
getExportJobsResponse_exportJobsResponse = Lens.lens (\GetExportJobsResponse' {exportJobsResponse} -> exportJobsResponse) (\s@GetExportJobsResponse' {} a -> s {exportJobsResponse = a} :: GetExportJobsResponse)

instance Core.NFData GetExportJobsResponse
