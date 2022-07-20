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
-- Module      : Amazonka.Pinpoint.GetImportJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of all the import
-- jobs for an application.
module Amazonka.Pinpoint.GetImportJobs
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetImportJobs' smart constructor.
data GetImportJobs = GetImportJobs'
  { -- | The maximum number of items to include in each page of a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    pageSize :: Prelude.Maybe Prelude.Text,
    -- | The NextToken string that specifies which page of results to return in a
    -- paginated response.
    token :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetImportJobs
newGetImportJobs pApplicationId_ =
  GetImportJobs'
    { pageSize = Prelude.Nothing,
      token = Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getImportJobs_pageSize :: Lens.Lens' GetImportJobs (Prelude.Maybe Prelude.Text)
getImportJobs_pageSize = Lens.lens (\GetImportJobs' {pageSize} -> pageSize) (\s@GetImportJobs' {} a -> s {pageSize = a} :: GetImportJobs)

-- | The NextToken string that specifies which page of results to return in a
-- paginated response.
getImportJobs_token :: Lens.Lens' GetImportJobs (Prelude.Maybe Prelude.Text)
getImportJobs_token = Lens.lens (\GetImportJobs' {token} -> token) (\s@GetImportJobs' {} a -> s {token = a} :: GetImportJobs)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getImportJobs_applicationId :: Lens.Lens' GetImportJobs Prelude.Text
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
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable GetImportJobs where
  hashWithSalt _salt GetImportJobs' {..} =
    _salt `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` token
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData GetImportJobs where
  rnf GetImportJobs' {..} =
    Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf token
      `Prelude.seq` Prelude.rnf applicationId

instance Core.ToHeaders GetImportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetImportJobs where
  toPath GetImportJobs' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/jobs/import"
      ]

instance Core.ToQuery GetImportJobs where
  toQuery GetImportJobs' {..} =
    Prelude.mconcat
      ["page-size" Core.=: pageSize, "token" Core.=: token]

-- | /See:/ 'newGetImportJobsResponse' smart constructor.
data GetImportJobsResponse = GetImportJobsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    importJobsResponse :: ImportJobsResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
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
getImportJobsResponse_httpStatus :: Lens.Lens' GetImportJobsResponse Prelude.Int
getImportJobsResponse_httpStatus = Lens.lens (\GetImportJobsResponse' {httpStatus} -> httpStatus) (\s@GetImportJobsResponse' {} a -> s {httpStatus = a} :: GetImportJobsResponse)

-- | Undocumented member.
getImportJobsResponse_importJobsResponse :: Lens.Lens' GetImportJobsResponse ImportJobsResponse
getImportJobsResponse_importJobsResponse = Lens.lens (\GetImportJobsResponse' {importJobsResponse} -> importJobsResponse) (\s@GetImportJobsResponse' {} a -> s {importJobsResponse = a} :: GetImportJobsResponse)

instance Prelude.NFData GetImportJobsResponse where
  rnf GetImportJobsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf importJobsResponse
