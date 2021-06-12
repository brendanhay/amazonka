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
-- Module      : Network.AWS.MediaLive.ListMultiplexes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a list of the existing multiplexes.
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListMultiplexes
  ( -- * Creating a Request
    ListMultiplexes (..),
    newListMultiplexes,

    -- * Request Lenses
    listMultiplexes_nextToken,
    listMultiplexes_maxResults,

    -- * Destructuring the Response
    ListMultiplexesResponse (..),
    newListMultiplexesResponse,

    -- * Response Lenses
    listMultiplexesResponse_nextToken,
    listMultiplexesResponse_multiplexes,
    listMultiplexesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for ListMultiplexesRequest
--
-- /See:/ 'newListMultiplexes' smart constructor.
data ListMultiplexes = ListMultiplexes'
  { -- | The token to retrieve the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListMultiplexes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMultiplexes_nextToken' - The token to retrieve the next page of results.
--
-- 'maxResults', 'listMultiplexes_maxResults' - The maximum number of items to return.
newListMultiplexes ::
  ListMultiplexes
newListMultiplexes =
  ListMultiplexes'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The token to retrieve the next page of results.
listMultiplexes_nextToken :: Lens.Lens' ListMultiplexes (Core.Maybe Core.Text)
listMultiplexes_nextToken = Lens.lens (\ListMultiplexes' {nextToken} -> nextToken) (\s@ListMultiplexes' {} a -> s {nextToken = a} :: ListMultiplexes)

-- | The maximum number of items to return.
listMultiplexes_maxResults :: Lens.Lens' ListMultiplexes (Core.Maybe Core.Natural)
listMultiplexes_maxResults = Lens.lens (\ListMultiplexes' {maxResults} -> maxResults) (\s@ListMultiplexes' {} a -> s {maxResults = a} :: ListMultiplexes)

instance Core.AWSPager ListMultiplexes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMultiplexesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listMultiplexesResponse_multiplexes
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listMultiplexes_nextToken
          Lens..~ rs
          Lens.^? listMultiplexesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListMultiplexes where
  type
    AWSResponse ListMultiplexes =
      ListMultiplexesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMultiplexesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "multiplexes" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListMultiplexes

instance Core.NFData ListMultiplexes

instance Core.ToHeaders ListMultiplexes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListMultiplexes where
  toPath = Core.const "/prod/multiplexes"

instance Core.ToQuery ListMultiplexes where
  toQuery ListMultiplexes' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | Placeholder documentation for ListMultiplexesResponse
--
-- /See:/ 'newListMultiplexesResponse' smart constructor.
data ListMultiplexesResponse = ListMultiplexesResponse'
  { -- | Token for the next ListMultiplexes request.
    nextToken :: Core.Maybe Core.Text,
    -- | List of multiplexes.
    multiplexes :: Core.Maybe [MultiplexSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListMultiplexesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMultiplexesResponse_nextToken' - Token for the next ListMultiplexes request.
--
-- 'multiplexes', 'listMultiplexesResponse_multiplexes' - List of multiplexes.
--
-- 'httpStatus', 'listMultiplexesResponse_httpStatus' - The response's http status code.
newListMultiplexesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListMultiplexesResponse
newListMultiplexesResponse pHttpStatus_ =
  ListMultiplexesResponse'
    { nextToken = Core.Nothing,
      multiplexes = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token for the next ListMultiplexes request.
listMultiplexesResponse_nextToken :: Lens.Lens' ListMultiplexesResponse (Core.Maybe Core.Text)
listMultiplexesResponse_nextToken = Lens.lens (\ListMultiplexesResponse' {nextToken} -> nextToken) (\s@ListMultiplexesResponse' {} a -> s {nextToken = a} :: ListMultiplexesResponse)

-- | List of multiplexes.
listMultiplexesResponse_multiplexes :: Lens.Lens' ListMultiplexesResponse (Core.Maybe [MultiplexSummary])
listMultiplexesResponse_multiplexes = Lens.lens (\ListMultiplexesResponse' {multiplexes} -> multiplexes) (\s@ListMultiplexesResponse' {} a -> s {multiplexes = a} :: ListMultiplexesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listMultiplexesResponse_httpStatus :: Lens.Lens' ListMultiplexesResponse Core.Int
listMultiplexesResponse_httpStatus = Lens.lens (\ListMultiplexesResponse' {httpStatus} -> httpStatus) (\s@ListMultiplexesResponse' {} a -> s {httpStatus = a} :: ListMultiplexesResponse)

instance Core.NFData ListMultiplexesResponse
