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
-- Module      : Network.AWS.AlexaBusiness.ListBusinessReportSchedules
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.AlexaBusiness.ListBusinessReportSchedules
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
    listBusinessReportSchedulesResponse_nextToken,
    listBusinessReportSchedulesResponse_businessReportSchedules,
    listBusinessReportSchedulesResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBusinessReportSchedulesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "BusinessReportSchedules"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBusinessReportSchedules

instance Prelude.NFData ListBusinessReportSchedules

instance Core.ToHeaders ListBusinessReportSchedules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.ListBusinessReportSchedules" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListBusinessReportSchedules where
  toJSON ListBusinessReportSchedules' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListBusinessReportSchedules where
  toPath = Prelude.const "/"

instance Core.ToQuery ListBusinessReportSchedules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBusinessReportSchedulesResponse' smart constructor.
data ListBusinessReportSchedulesResponse = ListBusinessReportSchedulesResponse'
  { -- | The token used to list the remaining schedules from the previous API
    -- call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The schedule of the reports.
    businessReportSchedules :: Prelude.Maybe [BusinessReportSchedule],
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
-- 'nextToken', 'listBusinessReportSchedulesResponse_nextToken' - The token used to list the remaining schedules from the previous API
-- call.
--
-- 'businessReportSchedules', 'listBusinessReportSchedulesResponse_businessReportSchedules' - The schedule of the reports.
--
-- 'httpStatus', 'listBusinessReportSchedulesResponse_httpStatus' - The response's http status code.
newListBusinessReportSchedulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBusinessReportSchedulesResponse
newListBusinessReportSchedulesResponse pHttpStatus_ =
  ListBusinessReportSchedulesResponse'
    { nextToken =
        Prelude.Nothing,
      businessReportSchedules =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token used to list the remaining schedules from the previous API
-- call.
listBusinessReportSchedulesResponse_nextToken :: Lens.Lens' ListBusinessReportSchedulesResponse (Prelude.Maybe Prelude.Text)
listBusinessReportSchedulesResponse_nextToken = Lens.lens (\ListBusinessReportSchedulesResponse' {nextToken} -> nextToken) (\s@ListBusinessReportSchedulesResponse' {} a -> s {nextToken = a} :: ListBusinessReportSchedulesResponse)

-- | The schedule of the reports.
listBusinessReportSchedulesResponse_businessReportSchedules :: Lens.Lens' ListBusinessReportSchedulesResponse (Prelude.Maybe [BusinessReportSchedule])
listBusinessReportSchedulesResponse_businessReportSchedules = Lens.lens (\ListBusinessReportSchedulesResponse' {businessReportSchedules} -> businessReportSchedules) (\s@ListBusinessReportSchedulesResponse' {} a -> s {businessReportSchedules = a} :: ListBusinessReportSchedulesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listBusinessReportSchedulesResponse_httpStatus :: Lens.Lens' ListBusinessReportSchedulesResponse Prelude.Int
listBusinessReportSchedulesResponse_httpStatus = Lens.lens (\ListBusinessReportSchedulesResponse' {httpStatus} -> httpStatus) (\s@ListBusinessReportSchedulesResponse' {} a -> s {httpStatus = a} :: ListBusinessReportSchedulesResponse)

instance
  Prelude.NFData
    ListBusinessReportSchedulesResponse
