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
-- Module      : Amazonka.ComprehendMedical.ListRxNormInferenceJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of InferRxNorm jobs that you have submitted.
module Amazonka.ComprehendMedical.ListRxNormInferenceJobs
  ( -- * Creating a Request
    ListRxNormInferenceJobs (..),
    newListRxNormInferenceJobs,

    -- * Request Lenses
    listRxNormInferenceJobs_nextToken,
    listRxNormInferenceJobs_filter,
    listRxNormInferenceJobs_maxResults,

    -- * Destructuring the Response
    ListRxNormInferenceJobsResponse (..),
    newListRxNormInferenceJobsResponse,

    -- * Response Lenses
    listRxNormInferenceJobsResponse_nextToken,
    listRxNormInferenceJobsResponse_comprehendMedicalAsyncJobPropertiesList,
    listRxNormInferenceJobsResponse_httpStatus,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRxNormInferenceJobs' smart constructor.
data ListRxNormInferenceJobs = ListRxNormInferenceJobs'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters the jobs that are returned. You can filter jobs based on their
    -- names, status, or the date and time that they were submitted. You can
    -- only set one filter at a time.
    filter' :: Prelude.Maybe ComprehendMedicalAsyncJobFilter,
    -- | Identifies the next page of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRxNormInferenceJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRxNormInferenceJobs_nextToken' - Identifies the next page of results to return.
--
-- 'filter'', 'listRxNormInferenceJobs_filter' - Filters the jobs that are returned. You can filter jobs based on their
-- names, status, or the date and time that they were submitted. You can
-- only set one filter at a time.
--
-- 'maxResults', 'listRxNormInferenceJobs_maxResults' - Identifies the next page of results to return.
newListRxNormInferenceJobs ::
  ListRxNormInferenceJobs
newListRxNormInferenceJobs =
  ListRxNormInferenceJobs'
    { nextToken =
        Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Identifies the next page of results to return.
listRxNormInferenceJobs_nextToken :: Lens.Lens' ListRxNormInferenceJobs (Prelude.Maybe Prelude.Text)
listRxNormInferenceJobs_nextToken = Lens.lens (\ListRxNormInferenceJobs' {nextToken} -> nextToken) (\s@ListRxNormInferenceJobs' {} a -> s {nextToken = a} :: ListRxNormInferenceJobs)

-- | Filters the jobs that are returned. You can filter jobs based on their
-- names, status, or the date and time that they were submitted. You can
-- only set one filter at a time.
listRxNormInferenceJobs_filter :: Lens.Lens' ListRxNormInferenceJobs (Prelude.Maybe ComprehendMedicalAsyncJobFilter)
listRxNormInferenceJobs_filter = Lens.lens (\ListRxNormInferenceJobs' {filter'} -> filter') (\s@ListRxNormInferenceJobs' {} a -> s {filter' = a} :: ListRxNormInferenceJobs)

-- | Identifies the next page of results to return.
listRxNormInferenceJobs_maxResults :: Lens.Lens' ListRxNormInferenceJobs (Prelude.Maybe Prelude.Natural)
listRxNormInferenceJobs_maxResults = Lens.lens (\ListRxNormInferenceJobs' {maxResults} -> maxResults) (\s@ListRxNormInferenceJobs' {} a -> s {maxResults = a} :: ListRxNormInferenceJobs)

instance Core.AWSRequest ListRxNormInferenceJobs where
  type
    AWSResponse ListRxNormInferenceJobs =
      ListRxNormInferenceJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRxNormInferenceJobsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ComprehendMedicalAsyncJobPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRxNormInferenceJobs where
  hashWithSalt _salt ListRxNormInferenceJobs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListRxNormInferenceJobs where
  rnf ListRxNormInferenceJobs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListRxNormInferenceJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ComprehendMedical_20181030.ListRxNormInferenceJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListRxNormInferenceJobs where
  toJSON ListRxNormInferenceJobs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Filter" Core..=) Prelude.<$> filter',
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListRxNormInferenceJobs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListRxNormInferenceJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRxNormInferenceJobsResponse' smart constructor.
data ListRxNormInferenceJobsResponse = ListRxNormInferenceJobsResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    comprehendMedicalAsyncJobPropertiesList :: Prelude.Maybe [ComprehendMedicalAsyncJobProperties],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRxNormInferenceJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRxNormInferenceJobsResponse_nextToken' - Identifies the next page of results to return.
--
-- 'comprehendMedicalAsyncJobPropertiesList', 'listRxNormInferenceJobsResponse_comprehendMedicalAsyncJobPropertiesList' - The maximum number of results to return in each page. The default is
-- 100.
--
-- 'httpStatus', 'listRxNormInferenceJobsResponse_httpStatus' - The response's http status code.
newListRxNormInferenceJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRxNormInferenceJobsResponse
newListRxNormInferenceJobsResponse pHttpStatus_ =
  ListRxNormInferenceJobsResponse'
    { nextToken =
        Prelude.Nothing,
      comprehendMedicalAsyncJobPropertiesList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifies the next page of results to return.
listRxNormInferenceJobsResponse_nextToken :: Lens.Lens' ListRxNormInferenceJobsResponse (Prelude.Maybe Prelude.Text)
listRxNormInferenceJobsResponse_nextToken = Lens.lens (\ListRxNormInferenceJobsResponse' {nextToken} -> nextToken) (\s@ListRxNormInferenceJobsResponse' {} a -> s {nextToken = a} :: ListRxNormInferenceJobsResponse)

-- | The maximum number of results to return in each page. The default is
-- 100.
listRxNormInferenceJobsResponse_comprehendMedicalAsyncJobPropertiesList :: Lens.Lens' ListRxNormInferenceJobsResponse (Prelude.Maybe [ComprehendMedicalAsyncJobProperties])
listRxNormInferenceJobsResponse_comprehendMedicalAsyncJobPropertiesList = Lens.lens (\ListRxNormInferenceJobsResponse' {comprehendMedicalAsyncJobPropertiesList} -> comprehendMedicalAsyncJobPropertiesList) (\s@ListRxNormInferenceJobsResponse' {} a -> s {comprehendMedicalAsyncJobPropertiesList = a} :: ListRxNormInferenceJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRxNormInferenceJobsResponse_httpStatus :: Lens.Lens' ListRxNormInferenceJobsResponse Prelude.Int
listRxNormInferenceJobsResponse_httpStatus = Lens.lens (\ListRxNormInferenceJobsResponse' {httpStatus} -> httpStatus) (\s@ListRxNormInferenceJobsResponse' {} a -> s {httpStatus = a} :: ListRxNormInferenceJobsResponse)

instance
  Prelude.NFData
    ListRxNormInferenceJobsResponse
  where
  rnf ListRxNormInferenceJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf comprehendMedicalAsyncJobPropertiesList
      `Prelude.seq` Prelude.rnf httpStatus
