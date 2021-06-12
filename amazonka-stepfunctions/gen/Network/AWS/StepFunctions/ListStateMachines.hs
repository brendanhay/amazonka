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
-- Module      : Network.AWS.StepFunctions.ListStateMachines
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the existing state machines.
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
module Network.AWS.StepFunctions.ListStateMachines
  ( -- * Creating a Request
    ListStateMachines (..),
    newListStateMachines,

    -- * Request Lenses
    listStateMachines_nextToken,
    listStateMachines_maxResults,

    -- * Destructuring the Response
    ListStateMachinesResponse (..),
    newListStateMachinesResponse,

    -- * Response Lenses
    listStateMachinesResponse_nextToken,
    listStateMachinesResponse_httpStatus,
    listStateMachinesResponse_stateMachines,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'newListStateMachines' smart constructor.
data ListStateMachines = ListStateMachines'
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
-- Create a value of 'ListStateMachines' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStateMachines_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
--
-- 'maxResults', 'listStateMachines_maxResults' - The maximum number of results that are returned per call. You can use
-- @nextToken@ to obtain further pages of results. The default is 100 and
-- the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per
-- call might be fewer than the specified maximum.
newListStateMachines ::
  ListStateMachines
newListStateMachines =
  ListStateMachines'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
listStateMachines_nextToken :: Lens.Lens' ListStateMachines (Core.Maybe Core.Text)
listStateMachines_nextToken = Lens.lens (\ListStateMachines' {nextToken} -> nextToken) (\s@ListStateMachines' {} a -> s {nextToken = a} :: ListStateMachines)

-- | The maximum number of results that are returned per call. You can use
-- @nextToken@ to obtain further pages of results. The default is 100 and
-- the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per
-- call might be fewer than the specified maximum.
listStateMachines_maxResults :: Lens.Lens' ListStateMachines (Core.Maybe Core.Natural)
listStateMachines_maxResults = Lens.lens (\ListStateMachines' {maxResults} -> maxResults) (\s@ListStateMachines' {} a -> s {maxResults = a} :: ListStateMachines)

instance Core.AWSPager ListStateMachines where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStateMachinesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^. listStateMachinesResponse_stateMachines) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listStateMachines_nextToken
          Lens..~ rs
          Lens.^? listStateMachinesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListStateMachines where
  type
    AWSResponse ListStateMachines =
      ListStateMachinesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStateMachinesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "stateMachines" Core..!@ Core.mempty)
      )

instance Core.Hashable ListStateMachines

instance Core.NFData ListStateMachines

instance Core.ToHeaders ListStateMachines where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSStepFunctions.ListStateMachines" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListStateMachines where
  toJSON ListStateMachines' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListStateMachines where
  toPath = Core.const "/"

instance Core.ToQuery ListStateMachines where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListStateMachinesResponse' smart constructor.
data ListStateMachinesResponse = ListStateMachinesResponse'
  { -- | If @nextToken@ is returned, there are more results available. The value
    -- of @nextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours. Using
    -- an expired pagination token will return an /HTTP 400 InvalidToken/
    -- error.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    stateMachines :: [StateMachineListItem]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListStateMachinesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStateMachinesResponse_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
--
-- 'httpStatus', 'listStateMachinesResponse_httpStatus' - The response's http status code.
--
-- 'stateMachines', 'listStateMachinesResponse_stateMachines' - Undocumented member.
newListStateMachinesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListStateMachinesResponse
newListStateMachinesResponse pHttpStatus_ =
  ListStateMachinesResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      stateMachines = Core.mempty
    }

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
listStateMachinesResponse_nextToken :: Lens.Lens' ListStateMachinesResponse (Core.Maybe Core.Text)
listStateMachinesResponse_nextToken = Lens.lens (\ListStateMachinesResponse' {nextToken} -> nextToken) (\s@ListStateMachinesResponse' {} a -> s {nextToken = a} :: ListStateMachinesResponse)

-- | The response's http status code.
listStateMachinesResponse_httpStatus :: Lens.Lens' ListStateMachinesResponse Core.Int
listStateMachinesResponse_httpStatus = Lens.lens (\ListStateMachinesResponse' {httpStatus} -> httpStatus) (\s@ListStateMachinesResponse' {} a -> s {httpStatus = a} :: ListStateMachinesResponse)

-- | Undocumented member.
listStateMachinesResponse_stateMachines :: Lens.Lens' ListStateMachinesResponse [StateMachineListItem]
listStateMachinesResponse_stateMachines = Lens.lens (\ListStateMachinesResponse' {stateMachines} -> stateMachines) (\s@ListStateMachinesResponse' {} a -> s {stateMachines = a} :: ListStateMachinesResponse) Core.. Lens._Coerce

instance Core.NFData ListStateMachinesResponse
