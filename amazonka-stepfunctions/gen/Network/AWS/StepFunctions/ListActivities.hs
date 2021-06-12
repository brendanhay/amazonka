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
-- Module      : Network.AWS.StepFunctions.ListActivities
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the existing activities.
--
-- If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
--
-- This operation is eventually consistent. The results are best effort and
-- may not reflect very recent updates and changes.
--
-- This operation returns paginated results.
module Network.AWS.StepFunctions.ListActivities
  ( -- * Creating a Request
    ListActivities (..),
    newListActivities,

    -- * Request Lenses
    listActivities_nextToken,
    listActivities_maxResults,

    -- * Destructuring the Response
    ListActivitiesResponse (..),
    newListActivitiesResponse,

    -- * Response Lenses
    listActivitiesResponse_nextToken,
    listActivitiesResponse_httpStatus,
    listActivitiesResponse_activities,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'newListActivities' smart constructor.
data ListActivities = ListActivities'
  { -- | If @nextToken@ is returned, there are more results available. The value
    -- of @nextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours. Using
    -- an expired pagination token will return an /HTTP 400 InvalidToken/
    -- error.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results that are returned per call. You can use
    -- @nextToken@ to obtain further pages of results. The default is 100 and
    -- the maximum allowed page size is 1000. A value of 0 uses the default.
    --
    -- This is only an upper limit. The actual number of results returned per
    -- call might be fewer than the specified maximum.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListActivities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listActivities_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
--
-- 'maxResults', 'listActivities_maxResults' - The maximum number of results that are returned per call. You can use
-- @nextToken@ to obtain further pages of results. The default is 100 and
-- the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per
-- call might be fewer than the specified maximum.
newListActivities ::
  ListActivities
newListActivities =
  ListActivities'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
listActivities_nextToken :: Lens.Lens' ListActivities (Core.Maybe Core.Text)
listActivities_nextToken = Lens.lens (\ListActivities' {nextToken} -> nextToken) (\s@ListActivities' {} a -> s {nextToken = a} :: ListActivities)

-- | The maximum number of results that are returned per call. You can use
-- @nextToken@ to obtain further pages of results. The default is 100 and
-- the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per
-- call might be fewer than the specified maximum.
listActivities_maxResults :: Lens.Lens' ListActivities (Core.Maybe Core.Natural)
listActivities_maxResults = Lens.lens (\ListActivities' {maxResults} -> maxResults) (\s@ListActivities' {} a -> s {maxResults = a} :: ListActivities)

instance Core.AWSPager ListActivities where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listActivitiesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^. listActivitiesResponse_activities) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listActivities_nextToken
          Lens..~ rs
          Lens.^? listActivitiesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListActivities where
  type
    AWSResponse ListActivities =
      ListActivitiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListActivitiesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "activities" Core..!@ Core.mempty)
      )

instance Core.Hashable ListActivities

instance Core.NFData ListActivities

instance Core.ToHeaders ListActivities where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSStepFunctions.ListActivities" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListActivities where
  toJSON ListActivities' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListActivities where
  toPath = Core.const "/"

instance Core.ToQuery ListActivities where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListActivitiesResponse' smart constructor.
data ListActivitiesResponse = ListActivitiesResponse'
  { -- | If @nextToken@ is returned, there are more results available. The value
    -- of @nextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours. Using
    -- an expired pagination token will return an /HTTP 400 InvalidToken/
    -- error.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The list of activities.
    activities :: [ActivityListItem]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListActivitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listActivitiesResponse_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
--
-- 'httpStatus', 'listActivitiesResponse_httpStatus' - The response's http status code.
--
-- 'activities', 'listActivitiesResponse_activities' - The list of activities.
newListActivitiesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListActivitiesResponse
newListActivitiesResponse pHttpStatus_ =
  ListActivitiesResponse'
    { nextToken = Core.Nothing,
      httpStatus = pHttpStatus_,
      activities = Core.mempty
    }

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
listActivitiesResponse_nextToken :: Lens.Lens' ListActivitiesResponse (Core.Maybe Core.Text)
listActivitiesResponse_nextToken = Lens.lens (\ListActivitiesResponse' {nextToken} -> nextToken) (\s@ListActivitiesResponse' {} a -> s {nextToken = a} :: ListActivitiesResponse)

-- | The response's http status code.
listActivitiesResponse_httpStatus :: Lens.Lens' ListActivitiesResponse Core.Int
listActivitiesResponse_httpStatus = Lens.lens (\ListActivitiesResponse' {httpStatus} -> httpStatus) (\s@ListActivitiesResponse' {} a -> s {httpStatus = a} :: ListActivitiesResponse)

-- | The list of activities.
listActivitiesResponse_activities :: Lens.Lens' ListActivitiesResponse [ActivityListItem]
listActivitiesResponse_activities = Lens.lens (\ListActivitiesResponse' {activities} -> activities) (\s@ListActivitiesResponse' {} a -> s {activities = a} :: ListActivitiesResponse) Core.. Lens._Coerce

instance Core.NFData ListActivitiesResponse
