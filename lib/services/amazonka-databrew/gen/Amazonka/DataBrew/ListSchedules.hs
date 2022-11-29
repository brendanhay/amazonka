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
-- Module      : Amazonka.DataBrew.ListSchedules
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the DataBrew schedules that are defined.
--
-- This operation returns paginated results.
module Amazonka.DataBrew.ListSchedules
  ( -- * Creating a Request
    ListSchedules (..),
    newListSchedules,

    -- * Request Lenses
    listSchedules_nextToken,
    listSchedules_jobName,
    listSchedules_maxResults,

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
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSchedules' smart constructor.
data ListSchedules = ListSchedules'
  { -- | The token returned by a previous call to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the job that these schedules apply to.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in this request.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'listSchedules_nextToken' - The token returned by a previous call to retrieve the next set of
-- results.
--
-- 'jobName', 'listSchedules_jobName' - The name of the job that these schedules apply to.
--
-- 'maxResults', 'listSchedules_maxResults' - The maximum number of results to return in this request.
newListSchedules ::
  ListSchedules
newListSchedules =
  ListSchedules'
    { nextToken = Prelude.Nothing,
      jobName = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token returned by a previous call to retrieve the next set of
-- results.
listSchedules_nextToken :: Lens.Lens' ListSchedules (Prelude.Maybe Prelude.Text)
listSchedules_nextToken = Lens.lens (\ListSchedules' {nextToken} -> nextToken) (\s@ListSchedules' {} a -> s {nextToken = a} :: ListSchedules)

-- | The name of the job that these schedules apply to.
listSchedules_jobName :: Lens.Lens' ListSchedules (Prelude.Maybe Prelude.Text)
listSchedules_jobName = Lens.lens (\ListSchedules' {jobName} -> jobName) (\s@ListSchedules' {} a -> s {jobName = a} :: ListSchedules)

-- | The maximum number of results to return in this request.
listSchedules_maxResults :: Lens.Lens' ListSchedules (Prelude.Maybe Prelude.Natural)
listSchedules_maxResults = Lens.lens (\ListSchedules' {maxResults} -> maxResults) (\s@ListSchedules' {} a -> s {maxResults = a} :: ListSchedules)

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
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "Schedules" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListSchedules where
  hashWithSalt _salt ListSchedules' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListSchedules where
  rnf ListSchedules' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListSchedules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListSchedules where
  toPath = Prelude.const "/schedules"

instance Core.ToQuery ListSchedules where
  toQuery ListSchedules' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "jobName" Core.=: jobName,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListSchedulesResponse' smart constructor.
data ListSchedulesResponse = ListSchedulesResponse'
  { -- | A token that you can use in a subsequent call to retrieve the next set
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of schedules that are defined.
    schedules :: [Schedule]
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
-- 'nextToken', 'listSchedulesResponse_nextToken' - A token that you can use in a subsequent call to retrieve the next set
-- of results.
--
-- 'httpStatus', 'listSchedulesResponse_httpStatus' - The response's http status code.
--
-- 'schedules', 'listSchedulesResponse_schedules' - A list of schedules that are defined.
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

-- | A token that you can use in a subsequent call to retrieve the next set
-- of results.
listSchedulesResponse_nextToken :: Lens.Lens' ListSchedulesResponse (Prelude.Maybe Prelude.Text)
listSchedulesResponse_nextToken = Lens.lens (\ListSchedulesResponse' {nextToken} -> nextToken) (\s@ListSchedulesResponse' {} a -> s {nextToken = a} :: ListSchedulesResponse)

-- | The response's http status code.
listSchedulesResponse_httpStatus :: Lens.Lens' ListSchedulesResponse Prelude.Int
listSchedulesResponse_httpStatus = Lens.lens (\ListSchedulesResponse' {httpStatus} -> httpStatus) (\s@ListSchedulesResponse' {} a -> s {httpStatus = a} :: ListSchedulesResponse)

-- | A list of schedules that are defined.
listSchedulesResponse_schedules :: Lens.Lens' ListSchedulesResponse [Schedule]
listSchedulesResponse_schedules = Lens.lens (\ListSchedulesResponse' {schedules} -> schedules) (\s@ListSchedulesResponse' {} a -> s {schedules = a} :: ListSchedulesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListSchedulesResponse where
  rnf ListSchedulesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf schedules
