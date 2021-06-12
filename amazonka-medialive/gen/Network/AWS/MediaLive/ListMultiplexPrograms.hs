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
-- Module      : Network.AWS.MediaLive.ListMultiplexPrograms
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the programs that currently exist for a specific multiplex.
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListMultiplexPrograms
  ( -- * Creating a Request
    ListMultiplexPrograms (..),
    newListMultiplexPrograms,

    -- * Request Lenses
    listMultiplexPrograms_nextToken,
    listMultiplexPrograms_maxResults,
    listMultiplexPrograms_multiplexId,

    -- * Destructuring the Response
    ListMultiplexProgramsResponse (..),
    newListMultiplexProgramsResponse,

    -- * Response Lenses
    listMultiplexProgramsResponse_multiplexPrograms,
    listMultiplexProgramsResponse_nextToken,
    listMultiplexProgramsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for ListMultiplexProgramsRequest
--
-- /See:/ 'newListMultiplexPrograms' smart constructor.
data ListMultiplexPrograms = ListMultiplexPrograms'
  { -- | The token to retrieve the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | The ID of the multiplex that the programs belong to.
    multiplexId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListMultiplexPrograms' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMultiplexPrograms_nextToken' - The token to retrieve the next page of results.
--
-- 'maxResults', 'listMultiplexPrograms_maxResults' - The maximum number of items to return.
--
-- 'multiplexId', 'listMultiplexPrograms_multiplexId' - The ID of the multiplex that the programs belong to.
newListMultiplexPrograms ::
  -- | 'multiplexId'
  Core.Text ->
  ListMultiplexPrograms
newListMultiplexPrograms pMultiplexId_ =
  ListMultiplexPrograms'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      multiplexId = pMultiplexId_
    }

-- | The token to retrieve the next page of results.
listMultiplexPrograms_nextToken :: Lens.Lens' ListMultiplexPrograms (Core.Maybe Core.Text)
listMultiplexPrograms_nextToken = Lens.lens (\ListMultiplexPrograms' {nextToken} -> nextToken) (\s@ListMultiplexPrograms' {} a -> s {nextToken = a} :: ListMultiplexPrograms)

-- | The maximum number of items to return.
listMultiplexPrograms_maxResults :: Lens.Lens' ListMultiplexPrograms (Core.Maybe Core.Natural)
listMultiplexPrograms_maxResults = Lens.lens (\ListMultiplexPrograms' {maxResults} -> maxResults) (\s@ListMultiplexPrograms' {} a -> s {maxResults = a} :: ListMultiplexPrograms)

-- | The ID of the multiplex that the programs belong to.
listMultiplexPrograms_multiplexId :: Lens.Lens' ListMultiplexPrograms Core.Text
listMultiplexPrograms_multiplexId = Lens.lens (\ListMultiplexPrograms' {multiplexId} -> multiplexId) (\s@ListMultiplexPrograms' {} a -> s {multiplexId = a} :: ListMultiplexPrograms)

instance Core.AWSPager ListMultiplexPrograms where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMultiplexProgramsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listMultiplexProgramsResponse_multiplexPrograms
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listMultiplexPrograms_nextToken
          Lens..~ rs
          Lens.^? listMultiplexProgramsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListMultiplexPrograms where
  type
    AWSResponse ListMultiplexPrograms =
      ListMultiplexProgramsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMultiplexProgramsResponse'
            Core.<$> (x Core..?> "multiplexPrograms" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListMultiplexPrograms

instance Core.NFData ListMultiplexPrograms

instance Core.ToHeaders ListMultiplexPrograms where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListMultiplexPrograms where
  toPath ListMultiplexPrograms' {..} =
    Core.mconcat
      [ "/prod/multiplexes/",
        Core.toBS multiplexId,
        "/programs"
      ]

instance Core.ToQuery ListMultiplexPrograms where
  toQuery ListMultiplexPrograms' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | Placeholder documentation for ListMultiplexProgramsResponse
--
-- /See:/ 'newListMultiplexProgramsResponse' smart constructor.
data ListMultiplexProgramsResponse = ListMultiplexProgramsResponse'
  { -- | List of multiplex programs.
    multiplexPrograms :: Core.Maybe [MultiplexProgramSummary],
    -- | Token for the next ListMultiplexProgram request.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListMultiplexProgramsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiplexPrograms', 'listMultiplexProgramsResponse_multiplexPrograms' - List of multiplex programs.
--
-- 'nextToken', 'listMultiplexProgramsResponse_nextToken' - Token for the next ListMultiplexProgram request.
--
-- 'httpStatus', 'listMultiplexProgramsResponse_httpStatus' - The response's http status code.
newListMultiplexProgramsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListMultiplexProgramsResponse
newListMultiplexProgramsResponse pHttpStatus_ =
  ListMultiplexProgramsResponse'
    { multiplexPrograms =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of multiplex programs.
listMultiplexProgramsResponse_multiplexPrograms :: Lens.Lens' ListMultiplexProgramsResponse (Core.Maybe [MultiplexProgramSummary])
listMultiplexProgramsResponse_multiplexPrograms = Lens.lens (\ListMultiplexProgramsResponse' {multiplexPrograms} -> multiplexPrograms) (\s@ListMultiplexProgramsResponse' {} a -> s {multiplexPrograms = a} :: ListMultiplexProgramsResponse) Core.. Lens.mapping Lens._Coerce

-- | Token for the next ListMultiplexProgram request.
listMultiplexProgramsResponse_nextToken :: Lens.Lens' ListMultiplexProgramsResponse (Core.Maybe Core.Text)
listMultiplexProgramsResponse_nextToken = Lens.lens (\ListMultiplexProgramsResponse' {nextToken} -> nextToken) (\s@ListMultiplexProgramsResponse' {} a -> s {nextToken = a} :: ListMultiplexProgramsResponse)

-- | The response's http status code.
listMultiplexProgramsResponse_httpStatus :: Lens.Lens' ListMultiplexProgramsResponse Core.Int
listMultiplexProgramsResponse_httpStatus = Lens.lens (\ListMultiplexProgramsResponse' {httpStatus} -> httpStatus) (\s@ListMultiplexProgramsResponse' {} a -> s {httpStatus = a} :: ListMultiplexProgramsResponse)

instance Core.NFData ListMultiplexProgramsResponse
