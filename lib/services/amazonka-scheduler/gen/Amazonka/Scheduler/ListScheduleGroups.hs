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
-- Module      : Amazonka.Scheduler.ListScheduleGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of your schedule groups.
--
-- This operation returns paginated results.
module Amazonka.Scheduler.ListScheduleGroups
  ( -- * Creating a Request
    ListScheduleGroups (..),
    newListScheduleGroups,

    -- * Request Lenses
    listScheduleGroups_maxResults,
    listScheduleGroups_namePrefix,
    listScheduleGroups_nextToken,

    -- * Destructuring the Response
    ListScheduleGroupsResponse (..),
    newListScheduleGroupsResponse,

    -- * Response Lenses
    listScheduleGroupsResponse_nextToken,
    listScheduleGroupsResponse_httpStatus,
    listScheduleGroupsResponse_scheduleGroups,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Scheduler.Types

-- | /See:/ 'newListScheduleGroups' smart constructor.
data ListScheduleGroups = ListScheduleGroups'
  { -- | If specified, limits the number of results returned by this operation.
    -- The operation also returns a @NextToken@ which you can use in a
    -- subsequent operation to retrieve the next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name prefix that you can use to return a filtered list of your
    -- schedule groups.
    namePrefix :: Prelude.Maybe Prelude.Text,
    -- | The token returned by a previous call to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListScheduleGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listScheduleGroups_maxResults' - If specified, limits the number of results returned by this operation.
-- The operation also returns a @NextToken@ which you can use in a
-- subsequent operation to retrieve the next set of results.
--
-- 'namePrefix', 'listScheduleGroups_namePrefix' - The name prefix that you can use to return a filtered list of your
-- schedule groups.
--
-- 'nextToken', 'listScheduleGroups_nextToken' - The token returned by a previous call to retrieve the next set of
-- results.
newListScheduleGroups ::
  ListScheduleGroups
newListScheduleGroups =
  ListScheduleGroups'
    { maxResults = Prelude.Nothing,
      namePrefix = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | If specified, limits the number of results returned by this operation.
-- The operation also returns a @NextToken@ which you can use in a
-- subsequent operation to retrieve the next set of results.
listScheduleGroups_maxResults :: Lens.Lens' ListScheduleGroups (Prelude.Maybe Prelude.Natural)
listScheduleGroups_maxResults = Lens.lens (\ListScheduleGroups' {maxResults} -> maxResults) (\s@ListScheduleGroups' {} a -> s {maxResults = a} :: ListScheduleGroups)

-- | The name prefix that you can use to return a filtered list of your
-- schedule groups.
listScheduleGroups_namePrefix :: Lens.Lens' ListScheduleGroups (Prelude.Maybe Prelude.Text)
listScheduleGroups_namePrefix = Lens.lens (\ListScheduleGroups' {namePrefix} -> namePrefix) (\s@ListScheduleGroups' {} a -> s {namePrefix = a} :: ListScheduleGroups)

-- | The token returned by a previous call to retrieve the next set of
-- results.
listScheduleGroups_nextToken :: Lens.Lens' ListScheduleGroups (Prelude.Maybe Prelude.Text)
listScheduleGroups_nextToken = Lens.lens (\ListScheduleGroups' {nextToken} -> nextToken) (\s@ListScheduleGroups' {} a -> s {nextToken = a} :: ListScheduleGroups)

instance Core.AWSPager ListScheduleGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listScheduleGroupsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listScheduleGroupsResponse_scheduleGroups
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listScheduleGroups_nextToken
          Lens..~ rs
          Lens.^? listScheduleGroupsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListScheduleGroups where
  type
    AWSResponse ListScheduleGroups =
      ListScheduleGroupsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListScheduleGroupsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "ScheduleGroups"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListScheduleGroups where
  hashWithSalt _salt ListScheduleGroups' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` namePrefix
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListScheduleGroups where
  rnf ListScheduleGroups' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf namePrefix
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListScheduleGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListScheduleGroups where
  toPath = Prelude.const "/schedule-groups"

instance Data.ToQuery ListScheduleGroups where
  toQuery ListScheduleGroups' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NamePrefix" Data.=: namePrefix,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListScheduleGroupsResponse' smart constructor.
data ListScheduleGroupsResponse = ListScheduleGroupsResponse'
  { -- | Indicates whether there are additional results to retrieve. If the value
    -- is null, there are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The schedule groups that match the specified criteria.
    scheduleGroups :: [ScheduleGroupSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListScheduleGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listScheduleGroupsResponse_nextToken' - Indicates whether there are additional results to retrieve. If the value
-- is null, there are no more results.
--
-- 'httpStatus', 'listScheduleGroupsResponse_httpStatus' - The response's http status code.
--
-- 'scheduleGroups', 'listScheduleGroupsResponse_scheduleGroups' - The schedule groups that match the specified criteria.
newListScheduleGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListScheduleGroupsResponse
newListScheduleGroupsResponse pHttpStatus_ =
  ListScheduleGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      scheduleGroups = Prelude.mempty
    }

-- | Indicates whether there are additional results to retrieve. If the value
-- is null, there are no more results.
listScheduleGroupsResponse_nextToken :: Lens.Lens' ListScheduleGroupsResponse (Prelude.Maybe Prelude.Text)
listScheduleGroupsResponse_nextToken = Lens.lens (\ListScheduleGroupsResponse' {nextToken} -> nextToken) (\s@ListScheduleGroupsResponse' {} a -> s {nextToken = a} :: ListScheduleGroupsResponse)

-- | The response's http status code.
listScheduleGroupsResponse_httpStatus :: Lens.Lens' ListScheduleGroupsResponse Prelude.Int
listScheduleGroupsResponse_httpStatus = Lens.lens (\ListScheduleGroupsResponse' {httpStatus} -> httpStatus) (\s@ListScheduleGroupsResponse' {} a -> s {httpStatus = a} :: ListScheduleGroupsResponse)

-- | The schedule groups that match the specified criteria.
listScheduleGroupsResponse_scheduleGroups :: Lens.Lens' ListScheduleGroupsResponse [ScheduleGroupSummary]
listScheduleGroupsResponse_scheduleGroups = Lens.lens (\ListScheduleGroupsResponse' {scheduleGroups} -> scheduleGroups) (\s@ListScheduleGroupsResponse' {} a -> s {scheduleGroups = a} :: ListScheduleGroupsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListScheduleGroupsResponse where
  rnf ListScheduleGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf scheduleGroups
