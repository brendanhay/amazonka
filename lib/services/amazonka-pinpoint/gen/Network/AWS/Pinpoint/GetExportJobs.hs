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
    getExportJobs_token,
    getExportJobs_pageSize,
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetExportJobs' smart constructor.
data GetExportJobs = GetExportJobs'
  { -- | The NextToken string that specifies which page of results to return in a
    -- paginated response.
    token :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to include in each page of a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    pageSize :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'token', 'getExportJobs_token' - The NextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'pageSize', 'getExportJobs_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'applicationId', 'getExportJobs_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetExportJobs ::
  -- | 'applicationId'
  Prelude.Text ->
  GetExportJobs
newGetExportJobs pApplicationId_ =
  GetExportJobs'
    { token = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- | The NextToken string that specifies which page of results to return in a
-- paginated response.
getExportJobs_token :: Lens.Lens' GetExportJobs (Prelude.Maybe Prelude.Text)
getExportJobs_token = Lens.lens (\GetExportJobs' {token} -> token) (\s@GetExportJobs' {} a -> s {token = a} :: GetExportJobs)

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getExportJobs_pageSize :: Lens.Lens' GetExportJobs (Prelude.Maybe Prelude.Text)
getExportJobs_pageSize = Lens.lens (\GetExportJobs' {pageSize} -> pageSize) (\s@GetExportJobs' {} a -> s {pageSize = a} :: GetExportJobs)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getExportJobs_applicationId :: Lens.Lens' GetExportJobs Prelude.Text
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
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable GetExportJobs

instance Prelude.NFData GetExportJobs

instance Core.ToHeaders GetExportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetExportJobs where
  toPath GetExportJobs' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/jobs/export"
      ]

instance Core.ToQuery GetExportJobs where
  toQuery GetExportJobs' {..} =
    Prelude.mconcat
      ["token" Core.=: token, "page-size" Core.=: pageSize]

-- | /See:/ 'newGetExportJobsResponse' smart constructor.
data GetExportJobsResponse = GetExportJobsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    exportJobsResponse :: ExportJobsResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
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
getExportJobsResponse_httpStatus :: Lens.Lens' GetExportJobsResponse Prelude.Int
getExportJobsResponse_httpStatus = Lens.lens (\GetExportJobsResponse' {httpStatus} -> httpStatus) (\s@GetExportJobsResponse' {} a -> s {httpStatus = a} :: GetExportJobsResponse)

-- | Undocumented member.
getExportJobsResponse_exportJobsResponse :: Lens.Lens' GetExportJobsResponse ExportJobsResponse
getExportJobsResponse_exportJobsResponse = Lens.lens (\GetExportJobsResponse' {exportJobsResponse} -> exportJobsResponse) (\s@GetExportJobsResponse' {} a -> s {exportJobsResponse = a} :: GetExportJobsResponse)

instance Prelude.NFData GetExportJobsResponse
