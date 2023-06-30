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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    listPHIDetectionJobs_filter,
    listPHIDetectionJobs_maxResults,
    listPHIDetectionJobs_nextToken,

    -- * Destructuring the Response
    ListPHIDetectionJobsResponse (..),
    newListPHIDetectionJobsResponse,

    -- * Response Lenses
    listPHIDetectionJobsResponse_comprehendMedicalAsyncJobPropertiesList,
    listPHIDetectionJobsResponse_nextToken,
    listPHIDetectionJobsResponse_httpStatus,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPHIDetectionJobs' smart constructor.
data ListPHIDetectionJobs = ListPHIDetectionJobs'
  { -- | Filters the jobs that are returned. You can filter jobs based on their
    -- names, status, or the date and time that they were submitted. You can
    -- only set one filter at a time.
    filter' :: Prelude.Maybe ComprehendMedicalAsyncJobFilter,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'filter'', 'listPHIDetectionJobs_filter' - Filters the jobs that are returned. You can filter jobs based on their
-- names, status, or the date and time that they were submitted. You can
-- only set one filter at a time.
--
-- 'maxResults', 'listPHIDetectionJobs_maxResults' - The maximum number of results to return in each page. The default is
-- 100.
--
-- 'nextToken', 'listPHIDetectionJobs_nextToken' - Identifies the next page of results to return.
newListPHIDetectionJobs ::
  ListPHIDetectionJobs
newListPHIDetectionJobs =
  ListPHIDetectionJobs'
    { filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filters the jobs that are returned. You can filter jobs based on their
-- names, status, or the date and time that they were submitted. You can
-- only set one filter at a time.
listPHIDetectionJobs_filter :: Lens.Lens' ListPHIDetectionJobs (Prelude.Maybe ComprehendMedicalAsyncJobFilter)
listPHIDetectionJobs_filter = Lens.lens (\ListPHIDetectionJobs' {filter'} -> filter') (\s@ListPHIDetectionJobs' {} a -> s {filter' = a} :: ListPHIDetectionJobs)

-- | The maximum number of results to return in each page. The default is
-- 100.
listPHIDetectionJobs_maxResults :: Lens.Lens' ListPHIDetectionJobs (Prelude.Maybe Prelude.Natural)
listPHIDetectionJobs_maxResults = Lens.lens (\ListPHIDetectionJobs' {maxResults} -> maxResults) (\s@ListPHIDetectionJobs' {} a -> s {maxResults = a} :: ListPHIDetectionJobs)

-- | Identifies the next page of results to return.
listPHIDetectionJobs_nextToken :: Lens.Lens' ListPHIDetectionJobs (Prelude.Maybe Prelude.Text)
listPHIDetectionJobs_nextToken = Lens.lens (\ListPHIDetectionJobs' {nextToken} -> nextToken) (\s@ListPHIDetectionJobs' {} a -> s {nextToken = a} :: ListPHIDetectionJobs)

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
            Prelude.<$> ( x
                            Data..?> "ComprehendMedicalAsyncJobPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPHIDetectionJobs where
  hashWithSalt _salt ListPHIDetectionJobs' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListPHIDetectionJobs where
  rnf ListPHIDetectionJobs' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListPHIDetectionJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComprehendMedical_20181030.ListPHIDetectionJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPHIDetectionJobs where
  toJSON ListPHIDetectionJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListPHIDetectionJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListPHIDetectionJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPHIDetectionJobsResponse' smart constructor.
data ListPHIDetectionJobsResponse = ListPHIDetectionJobsResponse'
  { -- | A list containing the properties of each job returned.
    comprehendMedicalAsyncJobPropertiesList :: Prelude.Maybe [ComprehendMedicalAsyncJobProperties],
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'comprehendMedicalAsyncJobPropertiesList', 'listPHIDetectionJobsResponse_comprehendMedicalAsyncJobPropertiesList' - A list containing the properties of each job returned.
--
-- 'nextToken', 'listPHIDetectionJobsResponse_nextToken' - Identifies the next page of results to return.
--
-- 'httpStatus', 'listPHIDetectionJobsResponse_httpStatus' - The response's http status code.
newListPHIDetectionJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPHIDetectionJobsResponse
newListPHIDetectionJobsResponse pHttpStatus_ =
  ListPHIDetectionJobsResponse'
    { comprehendMedicalAsyncJobPropertiesList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list containing the properties of each job returned.
listPHIDetectionJobsResponse_comprehendMedicalAsyncJobPropertiesList :: Lens.Lens' ListPHIDetectionJobsResponse (Prelude.Maybe [ComprehendMedicalAsyncJobProperties])
listPHIDetectionJobsResponse_comprehendMedicalAsyncJobPropertiesList = Lens.lens (\ListPHIDetectionJobsResponse' {comprehendMedicalAsyncJobPropertiesList} -> comprehendMedicalAsyncJobPropertiesList) (\s@ListPHIDetectionJobsResponse' {} a -> s {comprehendMedicalAsyncJobPropertiesList = a} :: ListPHIDetectionJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Identifies the next page of results to return.
listPHIDetectionJobsResponse_nextToken :: Lens.Lens' ListPHIDetectionJobsResponse (Prelude.Maybe Prelude.Text)
listPHIDetectionJobsResponse_nextToken = Lens.lens (\ListPHIDetectionJobsResponse' {nextToken} -> nextToken) (\s@ListPHIDetectionJobsResponse' {} a -> s {nextToken = a} :: ListPHIDetectionJobsResponse)

-- | The response's http status code.
listPHIDetectionJobsResponse_httpStatus :: Lens.Lens' ListPHIDetectionJobsResponse Prelude.Int
listPHIDetectionJobsResponse_httpStatus = Lens.lens (\ListPHIDetectionJobsResponse' {httpStatus} -> httpStatus) (\s@ListPHIDetectionJobsResponse' {} a -> s {httpStatus = a} :: ListPHIDetectionJobsResponse)

instance Prelude.NFData ListPHIDetectionJobsResponse where
  rnf ListPHIDetectionJobsResponse' {..} =
    Prelude.rnf comprehendMedicalAsyncJobPropertiesList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
