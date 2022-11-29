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
-- Module      : Amazonka.Panorama.ListNodeFromTemplateJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of camera stream node jobs.
module Amazonka.Panorama.ListNodeFromTemplateJobs
  ( -- * Creating a Request
    ListNodeFromTemplateJobs (..),
    newListNodeFromTemplateJobs,

    -- * Request Lenses
    listNodeFromTemplateJobs_nextToken,
    listNodeFromTemplateJobs_maxResults,

    -- * Destructuring the Response
    ListNodeFromTemplateJobsResponse (..),
    newListNodeFromTemplateJobsResponse,

    -- * Response Lenses
    listNodeFromTemplateJobsResponse_nextToken,
    listNodeFromTemplateJobsResponse_httpStatus,
    listNodeFromTemplateJobsResponse_nodeFromTemplateJobs,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListNodeFromTemplateJobs' smart constructor.
data ListNodeFromTemplateJobs = ListNodeFromTemplateJobs'
  { -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of node from template jobs to return in one page of
    -- results.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNodeFromTemplateJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNodeFromTemplateJobs_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'maxResults', 'listNodeFromTemplateJobs_maxResults' - The maximum number of node from template jobs to return in one page of
-- results.
newListNodeFromTemplateJobs ::
  ListNodeFromTemplateJobs
newListNodeFromTemplateJobs =
  ListNodeFromTemplateJobs'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listNodeFromTemplateJobs_nextToken :: Lens.Lens' ListNodeFromTemplateJobs (Prelude.Maybe Prelude.Text)
listNodeFromTemplateJobs_nextToken = Lens.lens (\ListNodeFromTemplateJobs' {nextToken} -> nextToken) (\s@ListNodeFromTemplateJobs' {} a -> s {nextToken = a} :: ListNodeFromTemplateJobs)

-- | The maximum number of node from template jobs to return in one page of
-- results.
listNodeFromTemplateJobs_maxResults :: Lens.Lens' ListNodeFromTemplateJobs (Prelude.Maybe Prelude.Natural)
listNodeFromTemplateJobs_maxResults = Lens.lens (\ListNodeFromTemplateJobs' {maxResults} -> maxResults) (\s@ListNodeFromTemplateJobs' {} a -> s {maxResults = a} :: ListNodeFromTemplateJobs)

instance Core.AWSRequest ListNodeFromTemplateJobs where
  type
    AWSResponse ListNodeFromTemplateJobs =
      ListNodeFromTemplateJobsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNodeFromTemplateJobsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "NodeFromTemplateJobs"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListNodeFromTemplateJobs where
  hashWithSalt _salt ListNodeFromTemplateJobs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListNodeFromTemplateJobs where
  rnf ListNodeFromTemplateJobs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListNodeFromTemplateJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListNodeFromTemplateJobs where
  toPath = Prelude.const "/packages/template-job"

instance Core.ToQuery ListNodeFromTemplateJobs where
  toQuery ListNodeFromTemplateJobs' {..} =
    Prelude.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListNodeFromTemplateJobsResponse' smart constructor.
data ListNodeFromTemplateJobsResponse = ListNodeFromTemplateJobsResponse'
  { -- | A pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of jobs.
    nodeFromTemplateJobs :: [NodeFromTemplateJob]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNodeFromTemplateJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNodeFromTemplateJobsResponse_nextToken' - A pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listNodeFromTemplateJobsResponse_httpStatus' - The response's http status code.
--
-- 'nodeFromTemplateJobs', 'listNodeFromTemplateJobsResponse_nodeFromTemplateJobs' - A list of jobs.
newListNodeFromTemplateJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListNodeFromTemplateJobsResponse
newListNodeFromTemplateJobsResponse pHttpStatus_ =
  ListNodeFromTemplateJobsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      nodeFromTemplateJobs = Prelude.mempty
    }

-- | A pagination token that\'s included if more results are available.
listNodeFromTemplateJobsResponse_nextToken :: Lens.Lens' ListNodeFromTemplateJobsResponse (Prelude.Maybe Prelude.Text)
listNodeFromTemplateJobsResponse_nextToken = Lens.lens (\ListNodeFromTemplateJobsResponse' {nextToken} -> nextToken) (\s@ListNodeFromTemplateJobsResponse' {} a -> s {nextToken = a} :: ListNodeFromTemplateJobsResponse)

-- | The response's http status code.
listNodeFromTemplateJobsResponse_httpStatus :: Lens.Lens' ListNodeFromTemplateJobsResponse Prelude.Int
listNodeFromTemplateJobsResponse_httpStatus = Lens.lens (\ListNodeFromTemplateJobsResponse' {httpStatus} -> httpStatus) (\s@ListNodeFromTemplateJobsResponse' {} a -> s {httpStatus = a} :: ListNodeFromTemplateJobsResponse)

-- | A list of jobs.
listNodeFromTemplateJobsResponse_nodeFromTemplateJobs :: Lens.Lens' ListNodeFromTemplateJobsResponse [NodeFromTemplateJob]
listNodeFromTemplateJobsResponse_nodeFromTemplateJobs = Lens.lens (\ListNodeFromTemplateJobsResponse' {nodeFromTemplateJobs} -> nodeFromTemplateJobs) (\s@ListNodeFromTemplateJobsResponse' {} a -> s {nodeFromTemplateJobs = a} :: ListNodeFromTemplateJobsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListNodeFromTemplateJobsResponse
  where
  rnf ListNodeFromTemplateJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nodeFromTemplateJobs
