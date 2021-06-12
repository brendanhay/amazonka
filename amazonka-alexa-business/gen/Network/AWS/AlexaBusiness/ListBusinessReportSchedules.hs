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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListBusinessReportSchedules' smart constructor.
data ListBusinessReportSchedules = ListBusinessReportSchedules'
  { -- | The token used to list the remaining schedules from the previous API
    -- call.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of schedules listed in the call.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The token used to list the remaining schedules from the previous API
-- call.
listBusinessReportSchedules_nextToken :: Lens.Lens' ListBusinessReportSchedules (Core.Maybe Core.Text)
listBusinessReportSchedules_nextToken = Lens.lens (\ListBusinessReportSchedules' {nextToken} -> nextToken) (\s@ListBusinessReportSchedules' {} a -> s {nextToken = a} :: ListBusinessReportSchedules)

-- | The maximum number of schedules listed in the call.
listBusinessReportSchedules_maxResults :: Lens.Lens' ListBusinessReportSchedules (Core.Maybe Core.Natural)
listBusinessReportSchedules_maxResults = Lens.lens (\ListBusinessReportSchedules' {maxResults} -> maxResults) (\s@ListBusinessReportSchedules' {} a -> s {maxResults = a} :: ListBusinessReportSchedules)

instance Core.AWSPager ListBusinessReportSchedules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBusinessReportSchedulesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listBusinessReportSchedulesResponse_businessReportSchedules
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listBusinessReportSchedules_nextToken
          Lens..~ rs
          Lens.^? listBusinessReportSchedulesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListBusinessReportSchedules where
  type
    AWSResponse ListBusinessReportSchedules =
      ListBusinessReportSchedulesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBusinessReportSchedulesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "BusinessReportSchedules"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListBusinessReportSchedules

instance Core.NFData ListBusinessReportSchedules

instance Core.ToHeaders ListBusinessReportSchedules where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.ListBusinessReportSchedules" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListBusinessReportSchedules where
  toJSON ListBusinessReportSchedules' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListBusinessReportSchedules where
  toPath = Core.const "/"

instance Core.ToQuery ListBusinessReportSchedules where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListBusinessReportSchedulesResponse' smart constructor.
data ListBusinessReportSchedulesResponse = ListBusinessReportSchedulesResponse'
  { -- | The token used to list the remaining schedules from the previous API
    -- call.
    nextToken :: Core.Maybe Core.Text,
    -- | The schedule of the reports.
    businessReportSchedules :: Core.Maybe [BusinessReportSchedule],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListBusinessReportSchedulesResponse
newListBusinessReportSchedulesResponse pHttpStatus_ =
  ListBusinessReportSchedulesResponse'
    { nextToken =
        Core.Nothing,
      businessReportSchedules = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token used to list the remaining schedules from the previous API
-- call.
listBusinessReportSchedulesResponse_nextToken :: Lens.Lens' ListBusinessReportSchedulesResponse (Core.Maybe Core.Text)
listBusinessReportSchedulesResponse_nextToken = Lens.lens (\ListBusinessReportSchedulesResponse' {nextToken} -> nextToken) (\s@ListBusinessReportSchedulesResponse' {} a -> s {nextToken = a} :: ListBusinessReportSchedulesResponse)

-- | The schedule of the reports.
listBusinessReportSchedulesResponse_businessReportSchedules :: Lens.Lens' ListBusinessReportSchedulesResponse (Core.Maybe [BusinessReportSchedule])
listBusinessReportSchedulesResponse_businessReportSchedules = Lens.lens (\ListBusinessReportSchedulesResponse' {businessReportSchedules} -> businessReportSchedules) (\s@ListBusinessReportSchedulesResponse' {} a -> s {businessReportSchedules = a} :: ListBusinessReportSchedulesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listBusinessReportSchedulesResponse_httpStatus :: Lens.Lens' ListBusinessReportSchedulesResponse Core.Int
listBusinessReportSchedulesResponse_httpStatus = Lens.lens (\ListBusinessReportSchedulesResponse' {httpStatus} -> httpStatus) (\s@ListBusinessReportSchedulesResponse' {} a -> s {httpStatus = a} :: ListBusinessReportSchedulesResponse)

instance
  Core.NFData
    ListBusinessReportSchedulesResponse
