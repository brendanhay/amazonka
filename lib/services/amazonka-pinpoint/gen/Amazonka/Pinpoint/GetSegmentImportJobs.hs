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
-- Module      : Amazonka.Pinpoint.GetSegmentImportJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the import jobs
-- for a segment.
module Amazonka.Pinpoint.GetSegmentImportJobs
  ( -- * Creating a Request
    GetSegmentImportJobs (..),
    newGetSegmentImportJobs,

    -- * Request Lenses
    getSegmentImportJobs_pageSize,
    getSegmentImportJobs_token,
    getSegmentImportJobs_segmentId,
    getSegmentImportJobs_applicationId,

    -- * Destructuring the Response
    GetSegmentImportJobsResponse (..),
    newGetSegmentImportJobsResponse,

    -- * Response Lenses
    getSegmentImportJobsResponse_httpStatus,
    getSegmentImportJobsResponse_importJobsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSegmentImportJobs' smart constructor.
data GetSegmentImportJobs = GetSegmentImportJobs'
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
-- Create a value of 'GetSegmentImportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'getSegmentImportJobs_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'token', 'getSegmentImportJobs_token' - The NextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'segmentId', 'getSegmentImportJobs_segmentId' - The unique identifier for the segment.
--
-- 'applicationId', 'getSegmentImportJobs_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetSegmentImportJobs ::
  -- | 'segmentId'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  GetSegmentImportJobs
newGetSegmentImportJobs pSegmentId_ pApplicationId_ =
  GetSegmentImportJobs'
    { pageSize = Prelude.Nothing,
      token = Prelude.Nothing,
      segmentId = pSegmentId_,
      applicationId = pApplicationId_
    }

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getSegmentImportJobs_pageSize :: Lens.Lens' GetSegmentImportJobs (Prelude.Maybe Prelude.Text)
getSegmentImportJobs_pageSize = Lens.lens (\GetSegmentImportJobs' {pageSize} -> pageSize) (\s@GetSegmentImportJobs' {} a -> s {pageSize = a} :: GetSegmentImportJobs)

-- | The NextToken string that specifies which page of results to return in a
-- paginated response.
getSegmentImportJobs_token :: Lens.Lens' GetSegmentImportJobs (Prelude.Maybe Prelude.Text)
getSegmentImportJobs_token = Lens.lens (\GetSegmentImportJobs' {token} -> token) (\s@GetSegmentImportJobs' {} a -> s {token = a} :: GetSegmentImportJobs)

-- | The unique identifier for the segment.
getSegmentImportJobs_segmentId :: Lens.Lens' GetSegmentImportJobs Prelude.Text
getSegmentImportJobs_segmentId = Lens.lens (\GetSegmentImportJobs' {segmentId} -> segmentId) (\s@GetSegmentImportJobs' {} a -> s {segmentId = a} :: GetSegmentImportJobs)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getSegmentImportJobs_applicationId :: Lens.Lens' GetSegmentImportJobs Prelude.Text
getSegmentImportJobs_applicationId = Lens.lens (\GetSegmentImportJobs' {applicationId} -> applicationId) (\s@GetSegmentImportJobs' {} a -> s {applicationId = a} :: GetSegmentImportJobs)

instance Core.AWSRequest GetSegmentImportJobs where
  type
    AWSResponse GetSegmentImportJobs =
      GetSegmentImportJobsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSegmentImportJobsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable GetSegmentImportJobs where
  hashWithSalt _salt GetSegmentImportJobs' {..} =
    _salt `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` token
      `Prelude.hashWithSalt` segmentId
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData GetSegmentImportJobs where
  rnf GetSegmentImportJobs' {..} =
    Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf token
      `Prelude.seq` Prelude.rnf segmentId
      `Prelude.seq` Prelude.rnf applicationId

instance Core.ToHeaders GetSegmentImportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetSegmentImportJobs where
  toPath GetSegmentImportJobs' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/segments/",
        Core.toBS segmentId,
        "/jobs/import"
      ]

instance Core.ToQuery GetSegmentImportJobs where
  toQuery GetSegmentImportJobs' {..} =
    Prelude.mconcat
      ["page-size" Core.=: pageSize, "token" Core.=: token]

-- | /See:/ 'newGetSegmentImportJobsResponse' smart constructor.
data GetSegmentImportJobsResponse = GetSegmentImportJobsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    importJobsResponse :: ImportJobsResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSegmentImportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getSegmentImportJobsResponse_httpStatus' - The response's http status code.
--
-- 'importJobsResponse', 'getSegmentImportJobsResponse_importJobsResponse' - Undocumented member.
newGetSegmentImportJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'importJobsResponse'
  ImportJobsResponse ->
  GetSegmentImportJobsResponse
newGetSegmentImportJobsResponse
  pHttpStatus_
  pImportJobsResponse_ =
    GetSegmentImportJobsResponse'
      { httpStatus =
          pHttpStatus_,
        importJobsResponse = pImportJobsResponse_
      }

-- | The response's http status code.
getSegmentImportJobsResponse_httpStatus :: Lens.Lens' GetSegmentImportJobsResponse Prelude.Int
getSegmentImportJobsResponse_httpStatus = Lens.lens (\GetSegmentImportJobsResponse' {httpStatus} -> httpStatus) (\s@GetSegmentImportJobsResponse' {} a -> s {httpStatus = a} :: GetSegmentImportJobsResponse)

-- | Undocumented member.
getSegmentImportJobsResponse_importJobsResponse :: Lens.Lens' GetSegmentImportJobsResponse ImportJobsResponse
getSegmentImportJobsResponse_importJobsResponse = Lens.lens (\GetSegmentImportJobsResponse' {importJobsResponse} -> importJobsResponse) (\s@GetSegmentImportJobsResponse' {} a -> s {importJobsResponse = a} :: GetSegmentImportJobsResponse)

instance Prelude.NFData GetSegmentImportJobsResponse where
  rnf GetSegmentImportJobsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf importJobsResponse
