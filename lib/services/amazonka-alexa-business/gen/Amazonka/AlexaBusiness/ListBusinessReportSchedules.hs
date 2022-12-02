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
-- Module      : Amazonka.AlexaBusiness.ListBusinessReportSchedules
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the details of the schedules that a user configured. A download
-- URL of the report associated with each schedule is returned every time
-- this action is called. A new download URL is returned each time, and is
-- valid for 24 hours.
--
-- This operation returns paginated results.
module Amazonka.AlexaBusiness.ListBusinessReportSchedules
  ( -- * Creating a Request
    ListBusinessReportSchedules (..),
    newListBusinessReportSchedules,

    -- * Request Lenses
    listBusinessReportSchedules_nextToken,
    listBusinessReportSchedules_maxResults,

    -- * Destructuring the Response
    ListBusinessReportSchedulesResponse (..),
    newListBusinessReportSchedulesResponse,

    -- * Response Lenses
    listBusinessReportSchedulesResponse_businessReportSchedules,
    listBusinessReportSchedulesResponse_nextToken,
    listBusinessReportSchedulesResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBusinessReportSchedules' smart constructor.
data ListBusinessReportSchedules = ListBusinessReportSchedules'
  { -- | The token used to list the remaining schedules from the previous API
    -- call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of schedules listed in the call.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBusinessReportSchedules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBusinessReportSchedules_nextToken' - The token used to list the remaining schedules from the previous API
-- call.
--
-- 'maxResults', 'listBusinessReportSchedules_maxResults' - The maximum number of schedules listed in the call.
newListBusinessReportSchedules ::
  ListBusinessReportSchedules
newListBusinessReportSchedules =
  ListBusinessReportSchedules'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token used to list the remaining schedules from the previous API
-- call.
listBusinessReportSchedules_nextToken :: Lens.Lens' ListBusinessReportSchedules (Prelude.Maybe Prelude.Text)
listBusinessReportSchedules_nextToken = Lens.lens (\ListBusinessReportSchedules' {nextToken} -> nextToken) (\s@ListBusinessReportSchedules' {} a -> s {nextToken = a} :: ListBusinessReportSchedules)

-- | The maximum number of schedules listed in the call.
listBusinessReportSchedules_maxResults :: Lens.Lens' ListBusinessReportSchedules (Prelude.Maybe Prelude.Natural)
listBusinessReportSchedules_maxResults = Lens.lens (\ListBusinessReportSchedules' {maxResults} -> maxResults) (\s@ListBusinessReportSchedules' {} a -> s {maxResults = a} :: ListBusinessReportSchedules)

instance Core.AWSPager ListBusinessReportSchedules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBusinessReportSchedulesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listBusinessReportSchedulesResponse_businessReportSchedules
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listBusinessReportSchedules_nextToken
          Lens..~ rs
          Lens.^? listBusinessReportSchedulesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListBusinessReportSchedules where
  type
    AWSResponse ListBusinessReportSchedules =
      ListBusinessReportSchedulesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBusinessReportSchedulesResponse'
            Prelude.<$> ( x Data..?> "BusinessReportSchedules"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBusinessReportSchedules where
  hashWithSalt _salt ListBusinessReportSchedules' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListBusinessReportSchedules where
  rnf ListBusinessReportSchedules' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListBusinessReportSchedules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.ListBusinessReportSchedules" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListBusinessReportSchedules where
  toJSON ListBusinessReportSchedules' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListBusinessReportSchedules where
  toPath = Prelude.const "/"

instance Data.ToQuery ListBusinessReportSchedules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBusinessReportSchedulesResponse' smart constructor.
data ListBusinessReportSchedulesResponse = ListBusinessReportSchedulesResponse'
  { -- | The schedule of the reports.
    businessReportSchedules :: Prelude.Maybe [BusinessReportSchedule],
    -- | The token used to list the remaining schedules from the previous API
    -- call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBusinessReportSchedulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'businessReportSchedules', 'listBusinessReportSchedulesResponse_businessReportSchedules' - The schedule of the reports.
--
-- 'nextToken', 'listBusinessReportSchedulesResponse_nextToken' - The token used to list the remaining schedules from the previous API
-- call.
--
-- 'httpStatus', 'listBusinessReportSchedulesResponse_httpStatus' - The response's http status code.
newListBusinessReportSchedulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBusinessReportSchedulesResponse
newListBusinessReportSchedulesResponse pHttpStatus_ =
  ListBusinessReportSchedulesResponse'
    { businessReportSchedules =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The schedule of the reports.
listBusinessReportSchedulesResponse_businessReportSchedules :: Lens.Lens' ListBusinessReportSchedulesResponse (Prelude.Maybe [BusinessReportSchedule])
listBusinessReportSchedulesResponse_businessReportSchedules = Lens.lens (\ListBusinessReportSchedulesResponse' {businessReportSchedules} -> businessReportSchedules) (\s@ListBusinessReportSchedulesResponse' {} a -> s {businessReportSchedules = a} :: ListBusinessReportSchedulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token used to list the remaining schedules from the previous API
-- call.
listBusinessReportSchedulesResponse_nextToken :: Lens.Lens' ListBusinessReportSchedulesResponse (Prelude.Maybe Prelude.Text)
listBusinessReportSchedulesResponse_nextToken = Lens.lens (\ListBusinessReportSchedulesResponse' {nextToken} -> nextToken) (\s@ListBusinessReportSchedulesResponse' {} a -> s {nextToken = a} :: ListBusinessReportSchedulesResponse)

-- | The response's http status code.
listBusinessReportSchedulesResponse_httpStatus :: Lens.Lens' ListBusinessReportSchedulesResponse Prelude.Int
listBusinessReportSchedulesResponse_httpStatus = Lens.lens (\ListBusinessReportSchedulesResponse' {httpStatus} -> httpStatus) (\s@ListBusinessReportSchedulesResponse' {} a -> s {httpStatus = a} :: ListBusinessReportSchedulesResponse)

instance
  Prelude.NFData
    ListBusinessReportSchedulesResponse
  where
  rnf ListBusinessReportSchedulesResponse' {..} =
    Prelude.rnf businessReportSchedules
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
