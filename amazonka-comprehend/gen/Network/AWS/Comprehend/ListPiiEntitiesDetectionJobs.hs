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
-- Module      : Network.AWS.Comprehend.ListPiiEntitiesDetectionJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the PII entity detection jobs that you have submitted.
module Network.AWS.Comprehend.ListPiiEntitiesDetectionJobs
  ( -- * Creating a Request
    ListPiiEntitiesDetectionJobs (..),
    newListPiiEntitiesDetectionJobs,

    -- * Request Lenses
    listPiiEntitiesDetectionJobs_nextToken,
    listPiiEntitiesDetectionJobs_maxResults,
    listPiiEntitiesDetectionJobs_filter,

    -- * Destructuring the Response
    ListPiiEntitiesDetectionJobsResponse (..),
    newListPiiEntitiesDetectionJobsResponse,

    -- * Response Lenses
    listPiiEntitiesDetectionJobsResponse_nextToken,
    listPiiEntitiesDetectionJobsResponse_piiEntitiesDetectionJobPropertiesList,
    listPiiEntitiesDetectionJobsResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListPiiEntitiesDetectionJobs' smart constructor.
data ListPiiEntitiesDetectionJobs = ListPiiEntitiesDetectionJobs'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in each page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Filters the jobs that are returned. You can filter jobs on their name,
    -- status, or the date and time that they were submitted. You can only set
    -- one filter at a time.
    filter' :: Prelude.Maybe PiiEntitiesDetectionJobFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPiiEntitiesDetectionJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPiiEntitiesDetectionJobs_nextToken' - Identifies the next page of results to return.
--
-- 'maxResults', 'listPiiEntitiesDetectionJobs_maxResults' - The maximum number of results to return in each page.
--
-- 'filter'', 'listPiiEntitiesDetectionJobs_filter' - Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
newListPiiEntitiesDetectionJobs ::
  ListPiiEntitiesDetectionJobs
newListPiiEntitiesDetectionJobs =
  ListPiiEntitiesDetectionJobs'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filter' = Prelude.Nothing
    }

-- | Identifies the next page of results to return.
listPiiEntitiesDetectionJobs_nextToken :: Lens.Lens' ListPiiEntitiesDetectionJobs (Prelude.Maybe Prelude.Text)
listPiiEntitiesDetectionJobs_nextToken = Lens.lens (\ListPiiEntitiesDetectionJobs' {nextToken} -> nextToken) (\s@ListPiiEntitiesDetectionJobs' {} a -> s {nextToken = a} :: ListPiiEntitiesDetectionJobs)

-- | The maximum number of results to return in each page.
listPiiEntitiesDetectionJobs_maxResults :: Lens.Lens' ListPiiEntitiesDetectionJobs (Prelude.Maybe Prelude.Natural)
listPiiEntitiesDetectionJobs_maxResults = Lens.lens (\ListPiiEntitiesDetectionJobs' {maxResults} -> maxResults) (\s@ListPiiEntitiesDetectionJobs' {} a -> s {maxResults = a} :: ListPiiEntitiesDetectionJobs)

-- | Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
listPiiEntitiesDetectionJobs_filter :: Lens.Lens' ListPiiEntitiesDetectionJobs (Prelude.Maybe PiiEntitiesDetectionJobFilter)
listPiiEntitiesDetectionJobs_filter = Lens.lens (\ListPiiEntitiesDetectionJobs' {filter'} -> filter') (\s@ListPiiEntitiesDetectionJobs' {} a -> s {filter' = a} :: ListPiiEntitiesDetectionJobs)

instance Core.AWSRequest ListPiiEntitiesDetectionJobs where
  type
    AWSResponse ListPiiEntitiesDetectionJobs =
      ListPiiEntitiesDetectionJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPiiEntitiesDetectionJobsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "PiiEntitiesDetectionJobPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListPiiEntitiesDetectionJobs

instance Prelude.NFData ListPiiEntitiesDetectionJobs

instance Core.ToHeaders ListPiiEntitiesDetectionJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.ListPiiEntitiesDetectionJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListPiiEntitiesDetectionJobs where
  toJSON ListPiiEntitiesDetectionJobs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("Filter" Core..=) Prelude.<$> filter'
          ]
      )

instance Core.ToPath ListPiiEntitiesDetectionJobs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListPiiEntitiesDetectionJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPiiEntitiesDetectionJobsResponse' smart constructor.
data ListPiiEntitiesDetectionJobsResponse = ListPiiEntitiesDetectionJobsResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list containing the properties of each job that is returned.
    piiEntitiesDetectionJobPropertiesList :: Prelude.Maybe [PiiEntitiesDetectionJobProperties],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPiiEntitiesDetectionJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPiiEntitiesDetectionJobsResponse_nextToken' - Identifies the next page of results to return.
--
-- 'piiEntitiesDetectionJobPropertiesList', 'listPiiEntitiesDetectionJobsResponse_piiEntitiesDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
--
-- 'httpStatus', 'listPiiEntitiesDetectionJobsResponse_httpStatus' - The response's http status code.
newListPiiEntitiesDetectionJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPiiEntitiesDetectionJobsResponse
newListPiiEntitiesDetectionJobsResponse pHttpStatus_ =
  ListPiiEntitiesDetectionJobsResponse'
    { nextToken =
        Prelude.Nothing,
      piiEntitiesDetectionJobPropertiesList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifies the next page of results to return.
listPiiEntitiesDetectionJobsResponse_nextToken :: Lens.Lens' ListPiiEntitiesDetectionJobsResponse (Prelude.Maybe Prelude.Text)
listPiiEntitiesDetectionJobsResponse_nextToken = Lens.lens (\ListPiiEntitiesDetectionJobsResponse' {nextToken} -> nextToken) (\s@ListPiiEntitiesDetectionJobsResponse' {} a -> s {nextToken = a} :: ListPiiEntitiesDetectionJobsResponse)

-- | A list containing the properties of each job that is returned.
listPiiEntitiesDetectionJobsResponse_piiEntitiesDetectionJobPropertiesList :: Lens.Lens' ListPiiEntitiesDetectionJobsResponse (Prelude.Maybe [PiiEntitiesDetectionJobProperties])
listPiiEntitiesDetectionJobsResponse_piiEntitiesDetectionJobPropertiesList = Lens.lens (\ListPiiEntitiesDetectionJobsResponse' {piiEntitiesDetectionJobPropertiesList} -> piiEntitiesDetectionJobPropertiesList) (\s@ListPiiEntitiesDetectionJobsResponse' {} a -> s {piiEntitiesDetectionJobPropertiesList = a} :: ListPiiEntitiesDetectionJobsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listPiiEntitiesDetectionJobsResponse_httpStatus :: Lens.Lens' ListPiiEntitiesDetectionJobsResponse Prelude.Int
listPiiEntitiesDetectionJobsResponse_httpStatus = Lens.lens (\ListPiiEntitiesDetectionJobsResponse' {httpStatus} -> httpStatus) (\s@ListPiiEntitiesDetectionJobsResponse' {} a -> s {httpStatus = a} :: ListPiiEntitiesDetectionJobsResponse)

instance
  Prelude.NFData
    ListPiiEntitiesDetectionJobsResponse
