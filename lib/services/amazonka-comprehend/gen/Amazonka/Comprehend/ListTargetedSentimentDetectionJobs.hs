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
-- Module      : Amazonka.Comprehend.ListTargetedSentimentDetectionJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of targeted sentiment detection jobs that you have
-- submitted.
module Amazonka.Comprehend.ListTargetedSentimentDetectionJobs
  ( -- * Creating a Request
    ListTargetedSentimentDetectionJobs (..),
    newListTargetedSentimentDetectionJobs,

    -- * Request Lenses
    listTargetedSentimentDetectionJobs_nextToken,
    listTargetedSentimentDetectionJobs_filter,
    listTargetedSentimentDetectionJobs_maxResults,

    -- * Destructuring the Response
    ListTargetedSentimentDetectionJobsResponse (..),
    newListTargetedSentimentDetectionJobsResponse,

    -- * Response Lenses
    listTargetedSentimentDetectionJobsResponse_nextToken,
    listTargetedSentimentDetectionJobsResponse_targetedSentimentDetectionJobPropertiesList,
    listTargetedSentimentDetectionJobsResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTargetedSentimentDetectionJobs' smart constructor.
data ListTargetedSentimentDetectionJobs = ListTargetedSentimentDetectionJobs'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters the jobs that are returned. You can filter jobs on their name,
    -- status, or the date and time that they were submitted. You can only set
    -- one filter at a time.
    filter' :: Prelude.Maybe TargetedSentimentDetectionJobFilter,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTargetedSentimentDetectionJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTargetedSentimentDetectionJobs_nextToken' - Identifies the next page of results to return.
--
-- 'filter'', 'listTargetedSentimentDetectionJobs_filter' - Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
--
-- 'maxResults', 'listTargetedSentimentDetectionJobs_maxResults' - The maximum number of results to return in each page. The default is
-- 100.
newListTargetedSentimentDetectionJobs ::
  ListTargetedSentimentDetectionJobs
newListTargetedSentimentDetectionJobs =
  ListTargetedSentimentDetectionJobs'
    { nextToken =
        Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Identifies the next page of results to return.
listTargetedSentimentDetectionJobs_nextToken :: Lens.Lens' ListTargetedSentimentDetectionJobs (Prelude.Maybe Prelude.Text)
listTargetedSentimentDetectionJobs_nextToken = Lens.lens (\ListTargetedSentimentDetectionJobs' {nextToken} -> nextToken) (\s@ListTargetedSentimentDetectionJobs' {} a -> s {nextToken = a} :: ListTargetedSentimentDetectionJobs)

-- | Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
listTargetedSentimentDetectionJobs_filter :: Lens.Lens' ListTargetedSentimentDetectionJobs (Prelude.Maybe TargetedSentimentDetectionJobFilter)
listTargetedSentimentDetectionJobs_filter = Lens.lens (\ListTargetedSentimentDetectionJobs' {filter'} -> filter') (\s@ListTargetedSentimentDetectionJobs' {} a -> s {filter' = a} :: ListTargetedSentimentDetectionJobs)

-- | The maximum number of results to return in each page. The default is
-- 100.
listTargetedSentimentDetectionJobs_maxResults :: Lens.Lens' ListTargetedSentimentDetectionJobs (Prelude.Maybe Prelude.Natural)
listTargetedSentimentDetectionJobs_maxResults = Lens.lens (\ListTargetedSentimentDetectionJobs' {maxResults} -> maxResults) (\s@ListTargetedSentimentDetectionJobs' {} a -> s {maxResults = a} :: ListTargetedSentimentDetectionJobs)

instance
  Core.AWSRequest
    ListTargetedSentimentDetectionJobs
  where
  type
    AWSResponse ListTargetedSentimentDetectionJobs =
      ListTargetedSentimentDetectionJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTargetedSentimentDetectionJobsResponse'
            Prelude.<$> (x Data..?> "NextToken")
              Prelude.<*> ( x
                              Data..?> "TargetedSentimentDetectionJobPropertiesList"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListTargetedSentimentDetectionJobs
  where
  hashWithSalt
    _salt
    ListTargetedSentimentDetectionJobs' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` filter'
        `Prelude.hashWithSalt` maxResults

instance
  Prelude.NFData
    ListTargetedSentimentDetectionJobs
  where
  rnf ListTargetedSentimentDetectionJobs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults

instance
  Data.ToHeaders
    ListTargetedSentimentDetectionJobs
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.ListTargetedSentimentDetectionJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListTargetedSentimentDetectionJobs
  where
  toJSON ListTargetedSentimentDetectionJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance
  Data.ToPath
    ListTargetedSentimentDetectionJobs
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListTargetedSentimentDetectionJobs
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTargetedSentimentDetectionJobsResponse' smart constructor.
data ListTargetedSentimentDetectionJobsResponse = ListTargetedSentimentDetectionJobsResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list containing the properties of each job that is returned.
    targetedSentimentDetectionJobPropertiesList :: Prelude.Maybe [TargetedSentimentDetectionJobProperties],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTargetedSentimentDetectionJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTargetedSentimentDetectionJobsResponse_nextToken' - Identifies the next page of results to return.
--
-- 'targetedSentimentDetectionJobPropertiesList', 'listTargetedSentimentDetectionJobsResponse_targetedSentimentDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
--
-- 'httpStatus', 'listTargetedSentimentDetectionJobsResponse_httpStatus' - The response's http status code.
newListTargetedSentimentDetectionJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTargetedSentimentDetectionJobsResponse
newListTargetedSentimentDetectionJobsResponse
  pHttpStatus_ =
    ListTargetedSentimentDetectionJobsResponse'
      { nextToken =
          Prelude.Nothing,
        targetedSentimentDetectionJobPropertiesList =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Identifies the next page of results to return.
listTargetedSentimentDetectionJobsResponse_nextToken :: Lens.Lens' ListTargetedSentimentDetectionJobsResponse (Prelude.Maybe Prelude.Text)
listTargetedSentimentDetectionJobsResponse_nextToken = Lens.lens (\ListTargetedSentimentDetectionJobsResponse' {nextToken} -> nextToken) (\s@ListTargetedSentimentDetectionJobsResponse' {} a -> s {nextToken = a} :: ListTargetedSentimentDetectionJobsResponse)

-- | A list containing the properties of each job that is returned.
listTargetedSentimentDetectionJobsResponse_targetedSentimentDetectionJobPropertiesList :: Lens.Lens' ListTargetedSentimentDetectionJobsResponse (Prelude.Maybe [TargetedSentimentDetectionJobProperties])
listTargetedSentimentDetectionJobsResponse_targetedSentimentDetectionJobPropertiesList = Lens.lens (\ListTargetedSentimentDetectionJobsResponse' {targetedSentimentDetectionJobPropertiesList} -> targetedSentimentDetectionJobPropertiesList) (\s@ListTargetedSentimentDetectionJobsResponse' {} a -> s {targetedSentimentDetectionJobPropertiesList = a} :: ListTargetedSentimentDetectionJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTargetedSentimentDetectionJobsResponse_httpStatus :: Lens.Lens' ListTargetedSentimentDetectionJobsResponse Prelude.Int
listTargetedSentimentDetectionJobsResponse_httpStatus = Lens.lens (\ListTargetedSentimentDetectionJobsResponse' {httpStatus} -> httpStatus) (\s@ListTargetedSentimentDetectionJobsResponse' {} a -> s {httpStatus = a} :: ListTargetedSentimentDetectionJobsResponse)

instance
  Prelude.NFData
    ListTargetedSentimentDetectionJobsResponse
  where
  rnf ListTargetedSentimentDetectionJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf
        targetedSentimentDetectionJobPropertiesList
      `Prelude.seq` Prelude.rnf httpStatus
