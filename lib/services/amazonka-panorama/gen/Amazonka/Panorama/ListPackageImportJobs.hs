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
-- Module      : Amazonka.Panorama.ListPackageImportJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of package import jobs.
module Amazonka.Panorama.ListPackageImportJobs
  ( -- * Creating a Request
    ListPackageImportJobs (..),
    newListPackageImportJobs,

    -- * Request Lenses
    listPackageImportJobs_nextToken,
    listPackageImportJobs_maxResults,

    -- * Destructuring the Response
    ListPackageImportJobsResponse (..),
    newListPackageImportJobsResponse,

    -- * Response Lenses
    listPackageImportJobsResponse_nextToken,
    listPackageImportJobsResponse_httpStatus,
    listPackageImportJobsResponse_packageImportJobs,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPackageImportJobs' smart constructor.
data ListPackageImportJobs = ListPackageImportJobs'
  { -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of package import jobs to return in one page of
    -- results.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPackageImportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPackageImportJobs_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'maxResults', 'listPackageImportJobs_maxResults' - The maximum number of package import jobs to return in one page of
-- results.
newListPackageImportJobs ::
  ListPackageImportJobs
newListPackageImportJobs =
  ListPackageImportJobs'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listPackageImportJobs_nextToken :: Lens.Lens' ListPackageImportJobs (Prelude.Maybe Prelude.Text)
listPackageImportJobs_nextToken = Lens.lens (\ListPackageImportJobs' {nextToken} -> nextToken) (\s@ListPackageImportJobs' {} a -> s {nextToken = a} :: ListPackageImportJobs)

-- | The maximum number of package import jobs to return in one page of
-- results.
listPackageImportJobs_maxResults :: Lens.Lens' ListPackageImportJobs (Prelude.Maybe Prelude.Natural)
listPackageImportJobs_maxResults = Lens.lens (\ListPackageImportJobs' {maxResults} -> maxResults) (\s@ListPackageImportJobs' {} a -> s {maxResults = a} :: ListPackageImportJobs)

instance Core.AWSRequest ListPackageImportJobs where
  type
    AWSResponse ListPackageImportJobs =
      ListPackageImportJobsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPackageImportJobsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "PackageImportJobs"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListPackageImportJobs where
  hashWithSalt _salt ListPackageImportJobs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListPackageImportJobs where
  rnf ListPackageImportJobs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListPackageImportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListPackageImportJobs where
  toPath = Prelude.const "/packages/import-jobs"

instance Core.ToQuery ListPackageImportJobs where
  toQuery ListPackageImportJobs' {..} =
    Prelude.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListPackageImportJobsResponse' smart constructor.
data ListPackageImportJobsResponse = ListPackageImportJobsResponse'
  { -- | A pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of package import jobs.
    packageImportJobs :: [PackageImportJob]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPackageImportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPackageImportJobsResponse_nextToken' - A pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listPackageImportJobsResponse_httpStatus' - The response's http status code.
--
-- 'packageImportJobs', 'listPackageImportJobsResponse_packageImportJobs' - A list of package import jobs.
newListPackageImportJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPackageImportJobsResponse
newListPackageImportJobsResponse pHttpStatus_ =
  ListPackageImportJobsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      packageImportJobs = Prelude.mempty
    }

-- | A pagination token that\'s included if more results are available.
listPackageImportJobsResponse_nextToken :: Lens.Lens' ListPackageImportJobsResponse (Prelude.Maybe Prelude.Text)
listPackageImportJobsResponse_nextToken = Lens.lens (\ListPackageImportJobsResponse' {nextToken} -> nextToken) (\s@ListPackageImportJobsResponse' {} a -> s {nextToken = a} :: ListPackageImportJobsResponse)

-- | The response's http status code.
listPackageImportJobsResponse_httpStatus :: Lens.Lens' ListPackageImportJobsResponse Prelude.Int
listPackageImportJobsResponse_httpStatus = Lens.lens (\ListPackageImportJobsResponse' {httpStatus} -> httpStatus) (\s@ListPackageImportJobsResponse' {} a -> s {httpStatus = a} :: ListPackageImportJobsResponse)

-- | A list of package import jobs.
listPackageImportJobsResponse_packageImportJobs :: Lens.Lens' ListPackageImportJobsResponse [PackageImportJob]
listPackageImportJobsResponse_packageImportJobs = Lens.lens (\ListPackageImportJobsResponse' {packageImportJobs} -> packageImportJobs) (\s@ListPackageImportJobsResponse' {} a -> s {packageImportJobs = a} :: ListPackageImportJobsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListPackageImportJobsResponse where
  rnf ListPackageImportJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf packageImportJobs
