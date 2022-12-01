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
-- Module      : Amazonka.ComprehendMedical.ListPHIDetectionJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of protected health information (PHI) detection jobs that
-- you have submitted.
module Amazonka.ComprehendMedical.ListPHIDetectionJobs
  ( -- * Creating a Request
    ListPHIDetectionJobs (..),
    newListPHIDetectionJobs,

    -- * Request Lenses
    listPHIDetectionJobs_nextToken,
    listPHIDetectionJobs_filter,
    listPHIDetectionJobs_maxResults,

    -- * Destructuring the Response
    ListPHIDetectionJobsResponse (..),
    newListPHIDetectionJobsResponse,

    -- * Response Lenses
    listPHIDetectionJobsResponse_nextToken,
    listPHIDetectionJobsResponse_comprehendMedicalAsyncJobPropertiesList,
    listPHIDetectionJobsResponse_httpStatus,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPHIDetectionJobs' smart constructor.
data ListPHIDetectionJobs = ListPHIDetectionJobs'
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
-- Create a value of 'ListPHIDetectionJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPHIDetectionJobs_nextToken' - Identifies the next page of results to return.
--
-- 'filter'', 'listPHIDetectionJobs_filter' - Filters the jobs that are returned. You can filter jobs based on their
-- names, status, or the date and time that they were submitted. You can
-- only set one filter at a time.
--
-- 'maxResults', 'listPHIDetectionJobs_maxResults' - The maximum number of results to return in each page. The default is
-- 100.
newListPHIDetectionJobs ::
  ListPHIDetectionJobs
newListPHIDetectionJobs =
  ListPHIDetectionJobs'
    { nextToken = Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Identifies the next page of results to return.
listPHIDetectionJobs_nextToken :: Lens.Lens' ListPHIDetectionJobs (Prelude.Maybe Prelude.Text)
listPHIDetectionJobs_nextToken = Lens.lens (\ListPHIDetectionJobs' {nextToken} -> nextToken) (\s@ListPHIDetectionJobs' {} a -> s {nextToken = a} :: ListPHIDetectionJobs)

-- | Filters the jobs that are returned. You can filter jobs based on their
-- names, status, or the date and time that they were submitted. You can
-- only set one filter at a time.
listPHIDetectionJobs_filter :: Lens.Lens' ListPHIDetectionJobs (Prelude.Maybe ComprehendMedicalAsyncJobFilter)
listPHIDetectionJobs_filter = Lens.lens (\ListPHIDetectionJobs' {filter'} -> filter') (\s@ListPHIDetectionJobs' {} a -> s {filter' = a} :: ListPHIDetectionJobs)

-- | The maximum number of results to return in each page. The default is
-- 100.
listPHIDetectionJobs_maxResults :: Lens.Lens' ListPHIDetectionJobs (Prelude.Maybe Prelude.Natural)
listPHIDetectionJobs_maxResults = Lens.lens (\ListPHIDetectionJobs' {maxResults} -> maxResults) (\s@ListPHIDetectionJobs' {} a -> s {maxResults = a} :: ListPHIDetectionJobs)

instance Core.AWSRequest ListPHIDetectionJobs where
  type
    AWSResponse ListPHIDetectionJobs =
      ListPHIDetectionJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPHIDetectionJobsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ComprehendMedicalAsyncJobPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPHIDetectionJobs where
  hashWithSalt _salt ListPHIDetectionJobs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListPHIDetectionJobs where
  rnf ListPHIDetectionJobs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListPHIDetectionJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ComprehendMedical_20181030.ListPHIDetectionJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListPHIDetectionJobs where
  toJSON ListPHIDetectionJobs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Filter" Core..=) Prelude.<$> filter',
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListPHIDetectionJobs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListPHIDetectionJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPHIDetectionJobsResponse' smart constructor.
data ListPHIDetectionJobsResponse = ListPHIDetectionJobsResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list containing the properties of each job returned.
    comprehendMedicalAsyncJobPropertiesList :: Prelude.Maybe [ComprehendMedicalAsyncJobProperties],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPHIDetectionJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPHIDetectionJobsResponse_nextToken' - Identifies the next page of results to return.
--
-- 'comprehendMedicalAsyncJobPropertiesList', 'listPHIDetectionJobsResponse_comprehendMedicalAsyncJobPropertiesList' - A list containing the properties of each job returned.
--
-- 'httpStatus', 'listPHIDetectionJobsResponse_httpStatus' - The response's http status code.
newListPHIDetectionJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPHIDetectionJobsResponse
newListPHIDetectionJobsResponse pHttpStatus_ =
  ListPHIDetectionJobsResponse'
    { nextToken =
        Prelude.Nothing,
      comprehendMedicalAsyncJobPropertiesList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifies the next page of results to return.
listPHIDetectionJobsResponse_nextToken :: Lens.Lens' ListPHIDetectionJobsResponse (Prelude.Maybe Prelude.Text)
listPHIDetectionJobsResponse_nextToken = Lens.lens (\ListPHIDetectionJobsResponse' {nextToken} -> nextToken) (\s@ListPHIDetectionJobsResponse' {} a -> s {nextToken = a} :: ListPHIDetectionJobsResponse)

-- | A list containing the properties of each job returned.
listPHIDetectionJobsResponse_comprehendMedicalAsyncJobPropertiesList :: Lens.Lens' ListPHIDetectionJobsResponse (Prelude.Maybe [ComprehendMedicalAsyncJobProperties])
listPHIDetectionJobsResponse_comprehendMedicalAsyncJobPropertiesList = Lens.lens (\ListPHIDetectionJobsResponse' {comprehendMedicalAsyncJobPropertiesList} -> comprehendMedicalAsyncJobPropertiesList) (\s@ListPHIDetectionJobsResponse' {} a -> s {comprehendMedicalAsyncJobPropertiesList = a} :: ListPHIDetectionJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPHIDetectionJobsResponse_httpStatus :: Lens.Lens' ListPHIDetectionJobsResponse Prelude.Int
listPHIDetectionJobsResponse_httpStatus = Lens.lens (\ListPHIDetectionJobsResponse' {httpStatus} -> httpStatus) (\s@ListPHIDetectionJobsResponse' {} a -> s {httpStatus = a} :: ListPHIDetectionJobsResponse)

instance Prelude.NFData ListPHIDetectionJobsResponse where
  rnf ListPHIDetectionJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf comprehendMedicalAsyncJobPropertiesList
      `Prelude.seq` Prelude.rnf httpStatus
