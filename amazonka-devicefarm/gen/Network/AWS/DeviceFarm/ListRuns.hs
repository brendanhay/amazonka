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
-- Module      : Network.AWS.DeviceFarm.ListRuns
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about runs, given an AWS Device Farm project ARN.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListRuns
  ( -- * Creating a Request
    ListRuns (..),
    newListRuns,

    -- * Request Lenses
    listRuns_nextToken,
    listRuns_arn,

    -- * Destructuring the Response
    ListRunsResponse (..),
    newListRunsResponse,

    -- * Response Lenses
    listRunsResponse_nextToken,
    listRunsResponse_runs,
    listRunsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the list runs operation.
--
-- /See:/ 'newListRuns' smart constructor.
data ListRuns = ListRuns'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the project for which you want to list
    -- runs.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListRuns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRuns_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'arn', 'listRuns_arn' - The Amazon Resource Name (ARN) of the project for which you want to list
-- runs.
newListRuns ::
  -- | 'arn'
  Core.Text ->
  ListRuns
newListRuns pArn_ =
  ListRuns' {nextToken = Core.Nothing, arn = pArn_}

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listRuns_nextToken :: Lens.Lens' ListRuns (Core.Maybe Core.Text)
listRuns_nextToken = Lens.lens (\ListRuns' {nextToken} -> nextToken) (\s@ListRuns' {} a -> s {nextToken = a} :: ListRuns)

-- | The Amazon Resource Name (ARN) of the project for which you want to list
-- runs.
listRuns_arn :: Lens.Lens' ListRuns Core.Text
listRuns_arn = Lens.lens (\ListRuns' {arn} -> arn) (\s@ListRuns' {} a -> s {arn = a} :: ListRuns)

instance Core.AWSPager ListRuns where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRunsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^? listRunsResponse_runs Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listRuns_nextToken
          Lens..~ rs
          Lens.^? listRunsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListRuns where
  type AWSResponse ListRuns = ListRunsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRunsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "runs" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListRuns

instance Core.NFData ListRuns

instance Core.ToHeaders ListRuns where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("DeviceFarm_20150623.ListRuns" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListRuns where
  toJSON ListRuns' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            Core.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath ListRuns where
  toPath = Core.const "/"

instance Core.ToQuery ListRuns where
  toQuery = Core.const Core.mempty

-- | Represents the result of a list runs request.
--
-- /See:/ 'newListRunsResponse' smart constructor.
data ListRunsResponse = ListRunsResponse'
  { -- | If the number of items that are returned is significantly large, this is
    -- an identifier that is also returned. It can be used in a subsequent call
    -- to this operation to return the next set of items in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the runs.
    runs :: Core.Maybe [Run],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListRunsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRunsResponse_nextToken' - If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
--
-- 'runs', 'listRunsResponse_runs' - Information about the runs.
--
-- 'httpStatus', 'listRunsResponse_httpStatus' - The response's http status code.
newListRunsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListRunsResponse
newListRunsResponse pHttpStatus_ =
  ListRunsResponse'
    { nextToken = Core.Nothing,
      runs = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
listRunsResponse_nextToken :: Lens.Lens' ListRunsResponse (Core.Maybe Core.Text)
listRunsResponse_nextToken = Lens.lens (\ListRunsResponse' {nextToken} -> nextToken) (\s@ListRunsResponse' {} a -> s {nextToken = a} :: ListRunsResponse)

-- | Information about the runs.
listRunsResponse_runs :: Lens.Lens' ListRunsResponse (Core.Maybe [Run])
listRunsResponse_runs = Lens.lens (\ListRunsResponse' {runs} -> runs) (\s@ListRunsResponse' {} a -> s {runs = a} :: ListRunsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listRunsResponse_httpStatus :: Lens.Lens' ListRunsResponse Core.Int
listRunsResponse_httpStatus = Lens.lens (\ListRunsResponse' {httpStatus} -> httpStatus) (\s@ListRunsResponse' {} a -> s {httpStatus = a} :: ListRunsResponse)

instance Core.NFData ListRunsResponse
