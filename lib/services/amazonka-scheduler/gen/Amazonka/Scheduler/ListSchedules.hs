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
-- Module      : Amazonka.Scheduler.ListSchedules
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of your EventBridge Scheduler schedules.
--
-- This operation returns paginated results.
module Amazonka.Scheduler.ListSchedules
  ( -- * Creating a Request
    ListSchedules (..),
    newListSchedules,

    -- * Request Lenses
    listSchedules_groupName,
    listSchedules_maxResults,
    listSchedules_namePrefix,
    listSchedules_nextToken,
    listSchedules_state,

    -- * Destructuring the Response
    ListSchedulesResponse (..),
    newListSchedulesResponse,

    -- * Response Lenses
    listSchedulesResponse_nextToken,
    listSchedulesResponse_httpStatus,
    listSchedulesResponse_schedules,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Scheduler.Types

-- | /See:/ 'newListSchedules' smart constructor.
data ListSchedules = ListSchedules'
  { -- | If specified, only lists the schedules whose associated schedule group
    -- matches the given filter.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | If specified, limits the number of results returned by this operation.
    -- The operation also returns a @NextToken@ which you can use in a
    -- subsequent operation to retrieve the next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Schedule name prefix to return the filtered list of resources.
    namePrefix :: Prelude.Maybe Prelude.Text,
    -- | The token returned by a previous call to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | If specified, only lists the schedules whose current state matches the
    -- given filter.
    state :: Prelude.Maybe ScheduleState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSchedules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'listSchedules_groupName' - If specified, only lists the schedules whose associated schedule group
-- matches the given filter.
--
-- 'maxResults', 'listSchedules_maxResults' - If specified, limits the number of results returned by this operation.
-- The operation also returns a @NextToken@ which you can use in a
-- subsequent operation to retrieve the next set of results.
--
-- 'namePrefix', 'listSchedules_namePrefix' - Schedule name prefix to return the filtered list of resources.
--
-- 'nextToken', 'listSchedules_nextToken' - The token returned by a previous call to retrieve the next set of
-- results.
--
-- 'state', 'listSchedules_state' - If specified, only lists the schedules whose current state matches the
-- given filter.
newListSchedules ::
  ListSchedules
newListSchedules =
  ListSchedules'
    { groupName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      namePrefix = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | If specified, only lists the schedules whose associated schedule group
-- matches the given filter.
listSchedules_groupName :: Lens.Lens' ListSchedules (Prelude.Maybe Prelude.Text)
listSchedules_groupName = Lens.lens (\ListSchedules' {groupName} -> groupName) (\s@ListSchedules' {} a -> s {groupName = a} :: ListSchedules)

-- | If specified, limits the number of results returned by this operation.
-- The operation also returns a @NextToken@ which you can use in a
-- subsequent operation to retrieve the next set of results.
listSchedules_maxResults :: Lens.Lens' ListSchedules (Prelude.Maybe Prelude.Natural)
listSchedules_maxResults = Lens.lens (\ListSchedules' {maxResults} -> maxResults) (\s@ListSchedules' {} a -> s {maxResults = a} :: ListSchedules)

-- | Schedule name prefix to return the filtered list of resources.
listSchedules_namePrefix :: Lens.Lens' ListSchedules (Prelude.Maybe Prelude.Text)
listSchedules_namePrefix = Lens.lens (\ListSchedules' {namePrefix} -> namePrefix) (\s@ListSchedules' {} a -> s {namePrefix = a} :: ListSchedules)

-- | The token returned by a previous call to retrieve the next set of
-- results.
listSchedules_nextToken :: Lens.Lens' ListSchedules (Prelude.Maybe Prelude.Text)
listSchedules_nextToken = Lens.lens (\ListSchedules' {nextToken} -> nextToken) (\s@ListSchedules' {} a -> s {nextToken = a} :: ListSchedules)

-- | If specified, only lists the schedules whose current state matches the
-- given filter.
listSchedules_state :: Lens.Lens' ListSchedules (Prelude.Maybe ScheduleState)
listSchedules_state = Lens.lens (\ListSchedules' {state} -> state) (\s@ListSchedules' {} a -> s {state = a} :: ListSchedules)

instance Core.AWSPager ListSchedules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSchedulesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listSchedulesResponse_schedules) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSchedules_nextToken
          Lens..~ rs
          Lens.^? listSchedulesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListSchedules where
  type
    AWSResponse ListSchedules =
      ListSchedulesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSchedulesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Schedules" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListSchedules where
  hashWithSalt _salt ListSchedules' {..} =
    _salt `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` namePrefix
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` state

instance Prelude.NFData ListSchedules where
  rnf ListSchedules' {..} =
    Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf namePrefix
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf state

instance Data.ToHeaders ListSchedules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSchedules where
  toPath = Prelude.const "/schedules"

instance Data.ToQuery ListSchedules where
  toQuery ListSchedules' {..} =
    Prelude.mconcat
      [ "ScheduleGroup" Data.=: groupName,
        "MaxResults" Data.=: maxResults,
        "NamePrefix" Data.=: namePrefix,
        "NextToken" Data.=: nextToken,
        "State" Data.=: state
      ]

-- | /See:/ 'newListSchedulesResponse' smart constructor.
data ListSchedulesResponse = ListSchedulesResponse'
  { -- | Indicates whether there are additional results to retrieve. If the value
    -- is null, there are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The schedules that match the specified criteria.
    schedules :: [ScheduleSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSchedulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSchedulesResponse_nextToken' - Indicates whether there are additional results to retrieve. If the value
-- is null, there are no more results.
--
-- 'httpStatus', 'listSchedulesResponse_httpStatus' - The response's http status code.
--
-- 'schedules', 'listSchedulesResponse_schedules' - The schedules that match the specified criteria.
newListSchedulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSchedulesResponse
newListSchedulesResponse pHttpStatus_ =
  ListSchedulesResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      schedules = Prelude.mempty
    }

-- | Indicates whether there are additional results to retrieve. If the value
-- is null, there are no more results.
listSchedulesResponse_nextToken :: Lens.Lens' ListSchedulesResponse (Prelude.Maybe Prelude.Text)
listSchedulesResponse_nextToken = Lens.lens (\ListSchedulesResponse' {nextToken} -> nextToken) (\s@ListSchedulesResponse' {} a -> s {nextToken = a} :: ListSchedulesResponse)

-- | The response's http status code.
listSchedulesResponse_httpStatus :: Lens.Lens' ListSchedulesResponse Prelude.Int
listSchedulesResponse_httpStatus = Lens.lens (\ListSchedulesResponse' {httpStatus} -> httpStatus) (\s@ListSchedulesResponse' {} a -> s {httpStatus = a} :: ListSchedulesResponse)

-- | The schedules that match the specified criteria.
listSchedulesResponse_schedules :: Lens.Lens' ListSchedulesResponse [ScheduleSummary]
listSchedulesResponse_schedules = Lens.lens (\ListSchedulesResponse' {schedules} -> schedules) (\s@ListSchedulesResponse' {} a -> s {schedules = a} :: ListSchedulesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListSchedulesResponse where
  rnf ListSchedulesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf schedules
