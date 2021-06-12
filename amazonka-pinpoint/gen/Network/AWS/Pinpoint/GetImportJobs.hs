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
-- Module      : Network.AWS.Pinpoint.GetImportJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of all the import
-- jobs for an application.
module Network.AWS.Pinpoint.GetImportJobs
  ( -- * Creating a Request
    GetImportJobs (..),
    newGetImportJobs,

    -- * Request Lenses
    getImportJobs_pageSize,
    getImportJobs_token,
    getImportJobs_applicationId,

    -- * Destructuring the Response
    GetImportJobsResponse (..),
    newGetImportJobsResponse,

    -- * Response Lenses
    getImportJobsResponse_httpStatus,
    getImportJobsResponse_importJobsResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetImportJobs' smart constructor.
data GetImportJobs = GetImportJobs'
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
-- Create a value of 'GetImportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'getImportJobs_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'token', 'getImportJobs_token' - The NextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'applicationId', 'getImportJobs_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetImportJobs ::
  -- | 'applicationId'
  Core.Text ->
  GetImportJobs
newGetImportJobs pApplicationId_ =
  GetImportJobs'
    { pageSize = Core.Nothing,
      token = Core.Nothing,
      applicationId = pApplicationId_
    }

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getImportJobs_pageSize :: Lens.Lens' GetImportJobs (Core.Maybe Core.Text)
getImportJobs_pageSize = Lens.lens (\GetImportJobs' {pageSize} -> pageSize) (\s@GetImportJobs' {} a -> s {pageSize = a} :: GetImportJobs)

-- | The NextToken string that specifies which page of results to return in a
-- paginated response.
getImportJobs_token :: Lens.Lens' GetImportJobs (Core.Maybe Core.Text)
getImportJobs_token = Lens.lens (\GetImportJobs' {token} -> token) (\s@GetImportJobs' {} a -> s {token = a} :: GetImportJobs)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getImportJobs_applicationId :: Lens.Lens' GetImportJobs Core.Text
getImportJobs_applicationId = Lens.lens (\GetImportJobs' {applicationId} -> applicationId) (\s@GetImportJobs' {} a -> s {applicationId = a} :: GetImportJobs)

instance Core.AWSRequest GetImportJobs where
  type
    AWSResponse GetImportJobs =
      GetImportJobsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetImportJobsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetImportJobs

instance Core.NFData GetImportJobs

instance Core.ToHeaders GetImportJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetImportJobs where
  toPath GetImportJobs' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/jobs/import"
      ]

instance Core.ToQuery GetImportJobs where
  toQuery GetImportJobs' {..} =
    Core.mconcat
      ["page-size" Core.=: pageSize, "token" Core.=: token]

-- | /See:/ 'newGetImportJobsResponse' smart constructor.
data GetImportJobsResponse = GetImportJobsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    importJobsResponse :: ImportJobsResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetImportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getImportJobsResponse_httpStatus' - The response's http status code.
--
-- 'importJobsResponse', 'getImportJobsResponse_importJobsResponse' - Undocumented member.
newGetImportJobsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'importJobsResponse'
  ImportJobsResponse ->
  GetImportJobsResponse
newGetImportJobsResponse
  pHttpStatus_
  pImportJobsResponse_ =
    GetImportJobsResponse'
      { httpStatus = pHttpStatus_,
        importJobsResponse = pImportJobsResponse_
      }

-- | The response's http status code.
getImportJobsResponse_httpStatus :: Lens.Lens' GetImportJobsResponse Core.Int
getImportJobsResponse_httpStatus = Lens.lens (\GetImportJobsResponse' {httpStatus} -> httpStatus) (\s@GetImportJobsResponse' {} a -> s {httpStatus = a} :: GetImportJobsResponse)

-- | Undocumented member.
getImportJobsResponse_importJobsResponse :: Lens.Lens' GetImportJobsResponse ImportJobsResponse
getImportJobsResponse_importJobsResponse = Lens.lens (\GetImportJobsResponse' {importJobsResponse} -> importJobsResponse) (\s@GetImportJobsResponse' {} a -> s {importJobsResponse = a} :: GetImportJobsResponse)

instance Core.NFData GetImportJobsResponse
