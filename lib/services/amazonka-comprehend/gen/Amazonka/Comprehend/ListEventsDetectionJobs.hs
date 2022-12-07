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
-- Module      : Amazonka.Comprehend.ListEventsDetectionJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the events detection jobs that you have submitted.
module Amazonka.Comprehend.ListEventsDetectionJobs
  ( -- * Creating a Request
    ListEventsDetectionJobs (..),
    newListEventsDetectionJobs,

    -- * Request Lenses
    listEventsDetectionJobs_nextToken,
    listEventsDetectionJobs_filter,
    listEventsDetectionJobs_maxResults,

    -- * Destructuring the Response
    ListEventsDetectionJobsResponse (..),
    newListEventsDetectionJobsResponse,

    -- * Response Lenses
    listEventsDetectionJobsResponse_nextToken,
    listEventsDetectionJobsResponse_eventsDetectionJobPropertiesList,
    listEventsDetectionJobsResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEventsDetectionJobs' smart constructor.
data ListEventsDetectionJobs = ListEventsDetectionJobs'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters the jobs that are returned. You can filter jobs on their name,
    -- status, or the date and time that they were submitted. You can only set
    -- one filter at a time.
    filter' :: Prelude.Maybe EventsDetectionJobFilter,
    -- | The maximum number of results to return in each page.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventsDetectionJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEventsDetectionJobs_nextToken' - Identifies the next page of results to return.
--
-- 'filter'', 'listEventsDetectionJobs_filter' - Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
--
-- 'maxResults', 'listEventsDetectionJobs_maxResults' - The maximum number of results to return in each page.
newListEventsDetectionJobs ::
  ListEventsDetectionJobs
newListEventsDetectionJobs =
  ListEventsDetectionJobs'
    { nextToken =
        Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Identifies the next page of results to return.
listEventsDetectionJobs_nextToken :: Lens.Lens' ListEventsDetectionJobs (Prelude.Maybe Prelude.Text)
listEventsDetectionJobs_nextToken = Lens.lens (\ListEventsDetectionJobs' {nextToken} -> nextToken) (\s@ListEventsDetectionJobs' {} a -> s {nextToken = a} :: ListEventsDetectionJobs)

-- | Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
listEventsDetectionJobs_filter :: Lens.Lens' ListEventsDetectionJobs (Prelude.Maybe EventsDetectionJobFilter)
listEventsDetectionJobs_filter = Lens.lens (\ListEventsDetectionJobs' {filter'} -> filter') (\s@ListEventsDetectionJobs' {} a -> s {filter' = a} :: ListEventsDetectionJobs)

-- | The maximum number of results to return in each page.
listEventsDetectionJobs_maxResults :: Lens.Lens' ListEventsDetectionJobs (Prelude.Maybe Prelude.Natural)
listEventsDetectionJobs_maxResults = Lens.lens (\ListEventsDetectionJobs' {maxResults} -> maxResults) (\s@ListEventsDetectionJobs' {} a -> s {maxResults = a} :: ListEventsDetectionJobs)

instance Core.AWSRequest ListEventsDetectionJobs where
  type
    AWSResponse ListEventsDetectionJobs =
      ListEventsDetectionJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEventsDetectionJobsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "EventsDetectionJobPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEventsDetectionJobs where
  hashWithSalt _salt ListEventsDetectionJobs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListEventsDetectionJobs where
  rnf ListEventsDetectionJobs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListEventsDetectionJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.ListEventsDetectionJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEventsDetectionJobs where
  toJSON ListEventsDetectionJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListEventsDetectionJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListEventsDetectionJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEventsDetectionJobsResponse' smart constructor.
data ListEventsDetectionJobsResponse = ListEventsDetectionJobsResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list containing the properties of each job that is returned.
    eventsDetectionJobPropertiesList :: Prelude.Maybe [EventsDetectionJobProperties],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventsDetectionJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEventsDetectionJobsResponse_nextToken' - Identifies the next page of results to return.
--
-- 'eventsDetectionJobPropertiesList', 'listEventsDetectionJobsResponse_eventsDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
--
-- 'httpStatus', 'listEventsDetectionJobsResponse_httpStatus' - The response's http status code.
newListEventsDetectionJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEventsDetectionJobsResponse
newListEventsDetectionJobsResponse pHttpStatus_ =
  ListEventsDetectionJobsResponse'
    { nextToken =
        Prelude.Nothing,
      eventsDetectionJobPropertiesList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifies the next page of results to return.
listEventsDetectionJobsResponse_nextToken :: Lens.Lens' ListEventsDetectionJobsResponse (Prelude.Maybe Prelude.Text)
listEventsDetectionJobsResponse_nextToken = Lens.lens (\ListEventsDetectionJobsResponse' {nextToken} -> nextToken) (\s@ListEventsDetectionJobsResponse' {} a -> s {nextToken = a} :: ListEventsDetectionJobsResponse)

-- | A list containing the properties of each job that is returned.
listEventsDetectionJobsResponse_eventsDetectionJobPropertiesList :: Lens.Lens' ListEventsDetectionJobsResponse (Prelude.Maybe [EventsDetectionJobProperties])
listEventsDetectionJobsResponse_eventsDetectionJobPropertiesList = Lens.lens (\ListEventsDetectionJobsResponse' {eventsDetectionJobPropertiesList} -> eventsDetectionJobPropertiesList) (\s@ListEventsDetectionJobsResponse' {} a -> s {eventsDetectionJobPropertiesList = a} :: ListEventsDetectionJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listEventsDetectionJobsResponse_httpStatus :: Lens.Lens' ListEventsDetectionJobsResponse Prelude.Int
listEventsDetectionJobsResponse_httpStatus = Lens.lens (\ListEventsDetectionJobsResponse' {httpStatus} -> httpStatus) (\s@ListEventsDetectionJobsResponse' {} a -> s {httpStatus = a} :: ListEventsDetectionJobsResponse)

instance
  Prelude.NFData
    ListEventsDetectionJobsResponse
  where
  rnf ListEventsDetectionJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf eventsDetectionJobPropertiesList
      `Prelude.seq` Prelude.rnf httpStatus
