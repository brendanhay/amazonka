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
-- Module      : Amazonka.ComprehendMedical.ListICD10CMInferenceJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of InferICD10CM jobs that you have submitted.
module Amazonka.ComprehendMedical.ListICD10CMInferenceJobs
  ( -- * Creating a Request
    ListICD10CMInferenceJobs (..),
    newListICD10CMInferenceJobs,

    -- * Request Lenses
    listICD10CMInferenceJobs_nextToken,
    listICD10CMInferenceJobs_filter,
    listICD10CMInferenceJobs_maxResults,

    -- * Destructuring the Response
    ListICD10CMInferenceJobsResponse (..),
    newListICD10CMInferenceJobsResponse,

    -- * Response Lenses
    listICD10CMInferenceJobsResponse_nextToken,
    listICD10CMInferenceJobsResponse_comprehendMedicalAsyncJobPropertiesList,
    listICD10CMInferenceJobsResponse_httpStatus,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListICD10CMInferenceJobs' smart constructor.
data ListICD10CMInferenceJobs = ListICD10CMInferenceJobs'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters the jobs that are returned. You can filter jobs based on their
    -- names, status, or the date and time that they were submitted. You can
    -- only set one filter at a time.
    filter' :: Prelude.Maybe ComprehendMedicalAsyncJobFilter,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListICD10CMInferenceJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listICD10CMInferenceJobs_nextToken' - Identifies the next page of results to return.
--
-- 'filter'', 'listICD10CMInferenceJobs_filter' - Filters the jobs that are returned. You can filter jobs based on their
-- names, status, or the date and time that they were submitted. You can
-- only set one filter at a time.
--
-- 'maxResults', 'listICD10CMInferenceJobs_maxResults' - The maximum number of results to return in each page. The default is
-- 100.
newListICD10CMInferenceJobs ::
  ListICD10CMInferenceJobs
newListICD10CMInferenceJobs =
  ListICD10CMInferenceJobs'
    { nextToken =
        Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Identifies the next page of results to return.
listICD10CMInferenceJobs_nextToken :: Lens.Lens' ListICD10CMInferenceJobs (Prelude.Maybe Prelude.Text)
listICD10CMInferenceJobs_nextToken = Lens.lens (\ListICD10CMInferenceJobs' {nextToken} -> nextToken) (\s@ListICD10CMInferenceJobs' {} a -> s {nextToken = a} :: ListICD10CMInferenceJobs)

-- | Filters the jobs that are returned. You can filter jobs based on their
-- names, status, or the date and time that they were submitted. You can
-- only set one filter at a time.
listICD10CMInferenceJobs_filter :: Lens.Lens' ListICD10CMInferenceJobs (Prelude.Maybe ComprehendMedicalAsyncJobFilter)
listICD10CMInferenceJobs_filter = Lens.lens (\ListICD10CMInferenceJobs' {filter'} -> filter') (\s@ListICD10CMInferenceJobs' {} a -> s {filter' = a} :: ListICD10CMInferenceJobs)

-- | The maximum number of results to return in each page. The default is
-- 100.
listICD10CMInferenceJobs_maxResults :: Lens.Lens' ListICD10CMInferenceJobs (Prelude.Maybe Prelude.Natural)
listICD10CMInferenceJobs_maxResults = Lens.lens (\ListICD10CMInferenceJobs' {maxResults} -> maxResults) (\s@ListICD10CMInferenceJobs' {} a -> s {maxResults = a} :: ListICD10CMInferenceJobs)

instance Core.AWSRequest ListICD10CMInferenceJobs where
  type
    AWSResponse ListICD10CMInferenceJobs =
      ListICD10CMInferenceJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListICD10CMInferenceJobsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ComprehendMedicalAsyncJobPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListICD10CMInferenceJobs where
  hashWithSalt _salt ListICD10CMInferenceJobs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListICD10CMInferenceJobs where
  rnf ListICD10CMInferenceJobs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListICD10CMInferenceJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ComprehendMedical_20181030.ListICD10CMInferenceJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListICD10CMInferenceJobs where
  toJSON ListICD10CMInferenceJobs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Filter" Core..=) Prelude.<$> filter',
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListICD10CMInferenceJobs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListICD10CMInferenceJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListICD10CMInferenceJobsResponse' smart constructor.
data ListICD10CMInferenceJobsResponse = ListICD10CMInferenceJobsResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list containing the properties of each job that is returned.
    comprehendMedicalAsyncJobPropertiesList :: Prelude.Maybe [ComprehendMedicalAsyncJobProperties],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListICD10CMInferenceJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listICD10CMInferenceJobsResponse_nextToken' - Identifies the next page of results to return.
--
-- 'comprehendMedicalAsyncJobPropertiesList', 'listICD10CMInferenceJobsResponse_comprehendMedicalAsyncJobPropertiesList' - A list containing the properties of each job that is returned.
--
-- 'httpStatus', 'listICD10CMInferenceJobsResponse_httpStatus' - The response's http status code.
newListICD10CMInferenceJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListICD10CMInferenceJobsResponse
newListICD10CMInferenceJobsResponse pHttpStatus_ =
  ListICD10CMInferenceJobsResponse'
    { nextToken =
        Prelude.Nothing,
      comprehendMedicalAsyncJobPropertiesList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifies the next page of results to return.
listICD10CMInferenceJobsResponse_nextToken :: Lens.Lens' ListICD10CMInferenceJobsResponse (Prelude.Maybe Prelude.Text)
listICD10CMInferenceJobsResponse_nextToken = Lens.lens (\ListICD10CMInferenceJobsResponse' {nextToken} -> nextToken) (\s@ListICD10CMInferenceJobsResponse' {} a -> s {nextToken = a} :: ListICD10CMInferenceJobsResponse)

-- | A list containing the properties of each job that is returned.
listICD10CMInferenceJobsResponse_comprehendMedicalAsyncJobPropertiesList :: Lens.Lens' ListICD10CMInferenceJobsResponse (Prelude.Maybe [ComprehendMedicalAsyncJobProperties])
listICD10CMInferenceJobsResponse_comprehendMedicalAsyncJobPropertiesList = Lens.lens (\ListICD10CMInferenceJobsResponse' {comprehendMedicalAsyncJobPropertiesList} -> comprehendMedicalAsyncJobPropertiesList) (\s@ListICD10CMInferenceJobsResponse' {} a -> s {comprehendMedicalAsyncJobPropertiesList = a} :: ListICD10CMInferenceJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listICD10CMInferenceJobsResponse_httpStatus :: Lens.Lens' ListICD10CMInferenceJobsResponse Prelude.Int
listICD10CMInferenceJobsResponse_httpStatus = Lens.lens (\ListICD10CMInferenceJobsResponse' {httpStatus} -> httpStatus) (\s@ListICD10CMInferenceJobsResponse' {} a -> s {httpStatus = a} :: ListICD10CMInferenceJobsResponse)

instance
  Prelude.NFData
    ListICD10CMInferenceJobsResponse
  where
  rnf ListICD10CMInferenceJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf comprehendMedicalAsyncJobPropertiesList
      `Prelude.seq` Prelude.rnf httpStatus
