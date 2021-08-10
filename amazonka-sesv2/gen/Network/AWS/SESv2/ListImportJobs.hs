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
-- Module      : Network.AWS.SESv2.ListImportJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the import jobs.
module Network.AWS.SESv2.ListImportJobs
  ( -- * Creating a Request
    ListImportJobs (..),
    newListImportJobs,

    -- * Request Lenses
    listImportJobs_nextToken,
    listImportJobs_importDestinationType,
    listImportJobs_pageSize,

    -- * Destructuring the Response
    ListImportJobsResponse (..),
    newListImportJobsResponse,

    -- * Response Lenses
    listImportJobsResponse_nextToken,
    listImportJobsResponse_importJobs,
    listImportJobsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | Represents a request to list all of the import jobs for a data
-- destination within the specified maximum number of import jobs.
--
-- /See:/ 'newListImportJobs' smart constructor.
data ListImportJobs = ListImportJobs'
  { -- | A string token indicating that there might be additional import jobs
    -- available to be listed. Copy this token to a subsequent call to
    -- @ListImportJobs@ with the same parameters to retrieve the next page of
    -- import jobs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The destination of the import job, which can be used to list import jobs
    -- that have a certain @ImportDestinationType@.
    importDestinationType :: Prelude.Maybe ImportDestinationType,
    -- | Maximum number of import jobs to return at once. Use this parameter to
    -- paginate results. If additional import jobs exist beyond the specified
    -- limit, the @NextToken@ element is sent in the response. Use the
    -- @NextToken@ value in subsequent requests to retrieve additional
    -- addresses.
    pageSize :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listImportJobs_nextToken' - A string token indicating that there might be additional import jobs
-- available to be listed. Copy this token to a subsequent call to
-- @ListImportJobs@ with the same parameters to retrieve the next page of
-- import jobs.
--
-- 'importDestinationType', 'listImportJobs_importDestinationType' - The destination of the import job, which can be used to list import jobs
-- that have a certain @ImportDestinationType@.
--
-- 'pageSize', 'listImportJobs_pageSize' - Maximum number of import jobs to return at once. Use this parameter to
-- paginate results. If additional import jobs exist beyond the specified
-- limit, the @NextToken@ element is sent in the response. Use the
-- @NextToken@ value in subsequent requests to retrieve additional
-- addresses.
newListImportJobs ::
  ListImportJobs
newListImportJobs =
  ListImportJobs'
    { nextToken = Prelude.Nothing,
      importDestinationType = Prelude.Nothing,
      pageSize = Prelude.Nothing
    }

-- | A string token indicating that there might be additional import jobs
-- available to be listed. Copy this token to a subsequent call to
-- @ListImportJobs@ with the same parameters to retrieve the next page of
-- import jobs.
listImportJobs_nextToken :: Lens.Lens' ListImportJobs (Prelude.Maybe Prelude.Text)
listImportJobs_nextToken = Lens.lens (\ListImportJobs' {nextToken} -> nextToken) (\s@ListImportJobs' {} a -> s {nextToken = a} :: ListImportJobs)

-- | The destination of the import job, which can be used to list import jobs
-- that have a certain @ImportDestinationType@.
listImportJobs_importDestinationType :: Lens.Lens' ListImportJobs (Prelude.Maybe ImportDestinationType)
listImportJobs_importDestinationType = Lens.lens (\ListImportJobs' {importDestinationType} -> importDestinationType) (\s@ListImportJobs' {} a -> s {importDestinationType = a} :: ListImportJobs)

-- | Maximum number of import jobs to return at once. Use this parameter to
-- paginate results. If additional import jobs exist beyond the specified
-- limit, the @NextToken@ element is sent in the response. Use the
-- @NextToken@ value in subsequent requests to retrieve additional
-- addresses.
listImportJobs_pageSize :: Lens.Lens' ListImportJobs (Prelude.Maybe Prelude.Int)
listImportJobs_pageSize = Lens.lens (\ListImportJobs' {pageSize} -> pageSize) (\s@ListImportJobs' {} a -> s {pageSize = a} :: ListImportJobs)

instance Core.AWSRequest ListImportJobs where
  type
    AWSResponse ListImportJobs =
      ListImportJobsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListImportJobsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "ImportJobs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListImportJobs

instance Prelude.NFData ListImportJobs

instance Core.ToHeaders ListImportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListImportJobs where
  toPath = Prelude.const "/v2/email/import-jobs"

instance Core.ToQuery ListImportJobs where
  toQuery ListImportJobs' {..} =
    Prelude.mconcat
      [ "NextToken" Core.=: nextToken,
        "PageSize" Core.=: pageSize
      ]

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newListImportJobsResponse' smart constructor.
data ListImportJobsResponse = ListImportJobsResponse'
  { -- | A string token indicating that there might be additional import jobs
    -- available to be listed. Copy this token to a subsequent call to
    -- @ListImportJobs@ with the same parameters to retrieve the next page of
    -- import jobs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the import job summaries.
    importJobs :: Prelude.Maybe [ImportJobSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listImportJobsResponse_nextToken' - A string token indicating that there might be additional import jobs
-- available to be listed. Copy this token to a subsequent call to
-- @ListImportJobs@ with the same parameters to retrieve the next page of
-- import jobs.
--
-- 'importJobs', 'listImportJobsResponse_importJobs' - A list of the import job summaries.
--
-- 'httpStatus', 'listImportJobsResponse_httpStatus' - The response's http status code.
newListImportJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListImportJobsResponse
newListImportJobsResponse pHttpStatus_ =
  ListImportJobsResponse'
    { nextToken =
        Prelude.Nothing,
      importJobs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string token indicating that there might be additional import jobs
-- available to be listed. Copy this token to a subsequent call to
-- @ListImportJobs@ with the same parameters to retrieve the next page of
-- import jobs.
listImportJobsResponse_nextToken :: Lens.Lens' ListImportJobsResponse (Prelude.Maybe Prelude.Text)
listImportJobsResponse_nextToken = Lens.lens (\ListImportJobsResponse' {nextToken} -> nextToken) (\s@ListImportJobsResponse' {} a -> s {nextToken = a} :: ListImportJobsResponse)

-- | A list of the import job summaries.
listImportJobsResponse_importJobs :: Lens.Lens' ListImportJobsResponse (Prelude.Maybe [ImportJobSummary])
listImportJobsResponse_importJobs = Lens.lens (\ListImportJobsResponse' {importJobs} -> importJobs) (\s@ListImportJobsResponse' {} a -> s {importJobs = a} :: ListImportJobsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listImportJobsResponse_httpStatus :: Lens.Lens' ListImportJobsResponse Prelude.Int
listImportJobsResponse_httpStatus = Lens.lens (\ListImportJobsResponse' {httpStatus} -> httpStatus) (\s@ListImportJobsResponse' {} a -> s {httpStatus = a} :: ListImportJobsResponse)

instance Prelude.NFData ListImportJobsResponse
