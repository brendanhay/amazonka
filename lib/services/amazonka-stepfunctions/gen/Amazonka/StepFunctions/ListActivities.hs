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
-- Module      : Amazonka.StepFunctions.ListActivities
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.StepFunctions.ListActivities
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newListActivities' smart constructor.
data ListActivities = ListActivities'
  { -- | If @nextToken@ is returned, there are more results available. The value
    -- of @nextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours. Using
    -- an expired pagination token will return an /HTTP 400 InvalidToken/
    -- error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results that are returned per call. You can use
    -- @nextToken@ to obtain further pages of results. The default is 100 and
    -- the maximum allowed page size is 1000. A value of 0 uses the default.
    --
    -- This is only an upper limit. The actual number of results returned per
    -- call might be fewer than the specified maximum.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
listActivities_nextToken :: Lens.Lens' ListActivities (Prelude.Maybe Prelude.Text)
listActivities_nextToken = Lens.lens (\ListActivities' {nextToken} -> nextToken) (\s@ListActivities' {} a -> s {nextToken = a} :: ListActivities)

-- | The maximum number of results that are returned per call. You can use
-- @nextToken@ to obtain further pages of results. The default is 100 and
-- the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per
-- call might be fewer than the specified maximum.
listActivities_maxResults :: Lens.Lens' ListActivities (Prelude.Maybe Prelude.Natural)
listActivities_maxResults = Lens.lens (\ListActivities' {maxResults} -> maxResults) (\s@ListActivities' {} a -> s {maxResults = a} :: ListActivities)

instance Core.AWSPager ListActivities where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listActivitiesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listActivitiesResponse_activities) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listActivities_nextToken
          Lens..~ rs
          Lens.^? listActivitiesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListActivities where
  type
    AWSResponse ListActivities =
      ListActivitiesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListActivitiesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "activities" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListActivities where
  hashWithSalt _salt ListActivities' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListActivities where
  rnf ListActivities' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListActivities where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSStepFunctions.ListActivities" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListActivities where
  toJSON ListActivities' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListActivities where
  toPath = Prelude.const "/"

instance Core.ToQuery ListActivities where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListActivitiesResponse' smart constructor.
data ListActivitiesResponse = ListActivitiesResponse'
  { -- | If @nextToken@ is returned, there are more results available. The value
    -- of @nextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours. Using
    -- an expired pagination token will return an /HTTP 400 InvalidToken/
    -- error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of activities.
    activities :: [ActivityListItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListActivitiesResponse
newListActivitiesResponse pHttpStatus_ =
  ListActivitiesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      activities = Prelude.mempty
    }

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
listActivitiesResponse_nextToken :: Lens.Lens' ListActivitiesResponse (Prelude.Maybe Prelude.Text)
listActivitiesResponse_nextToken = Lens.lens (\ListActivitiesResponse' {nextToken} -> nextToken) (\s@ListActivitiesResponse' {} a -> s {nextToken = a} :: ListActivitiesResponse)

-- | The response's http status code.
listActivitiesResponse_httpStatus :: Lens.Lens' ListActivitiesResponse Prelude.Int
listActivitiesResponse_httpStatus = Lens.lens (\ListActivitiesResponse' {httpStatus} -> httpStatus) (\s@ListActivitiesResponse' {} a -> s {httpStatus = a} :: ListActivitiesResponse)

-- | The list of activities.
listActivitiesResponse_activities :: Lens.Lens' ListActivitiesResponse [ActivityListItem]
listActivitiesResponse_activities = Lens.lens (\ListActivitiesResponse' {activities} -> activities) (\s@ListActivitiesResponse' {} a -> s {activities = a} :: ListActivitiesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListActivitiesResponse where
  rnf ListActivitiesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf activities
