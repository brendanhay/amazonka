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
-- Module      : Network.AWS.DeviceFarm.ListUniqueProblems
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about unique problems, such as exceptions or crashes.
--
-- Unique problems are defined as a single instance of an error across a
-- run, job, or suite. For example, if a call in your application
-- consistently raises an exception
-- (@OutOfBoundsException in MyActivity.java:386@), @ListUniqueProblems@
-- returns a single entry instead of many individual entries for that
-- exception.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListUniqueProblems
  ( -- * Creating a Request
    ListUniqueProblems (..),
    newListUniqueProblems,

    -- * Request Lenses
    listUniqueProblems_nextToken,
    listUniqueProblems_arn,

    -- * Destructuring the Response
    ListUniqueProblemsResponse (..),
    newListUniqueProblemsResponse,

    -- * Response Lenses
    listUniqueProblemsResponse_nextToken,
    listUniqueProblemsResponse_uniqueProblems,
    listUniqueProblemsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the list unique problems operation.
--
-- /See:/ 'newListUniqueProblems' smart constructor.
data ListUniqueProblems = ListUniqueProblems'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | The unique problems\' ARNs.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListUniqueProblems' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUniqueProblems_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'arn', 'listUniqueProblems_arn' - The unique problems\' ARNs.
newListUniqueProblems ::
  -- | 'arn'
  Core.Text ->
  ListUniqueProblems
newListUniqueProblems pArn_ =
  ListUniqueProblems'
    { nextToken = Core.Nothing,
      arn = pArn_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listUniqueProblems_nextToken :: Lens.Lens' ListUniqueProblems (Core.Maybe Core.Text)
listUniqueProblems_nextToken = Lens.lens (\ListUniqueProblems' {nextToken} -> nextToken) (\s@ListUniqueProblems' {} a -> s {nextToken = a} :: ListUniqueProblems)

-- | The unique problems\' ARNs.
listUniqueProblems_arn :: Lens.Lens' ListUniqueProblems Core.Text
listUniqueProblems_arn = Lens.lens (\ListUniqueProblems' {arn} -> arn) (\s@ListUniqueProblems' {} a -> s {arn = a} :: ListUniqueProblems)

instance Core.AWSPager ListUniqueProblems where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUniqueProblemsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listUniqueProblemsResponse_uniqueProblems
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listUniqueProblems_nextToken
          Lens..~ rs
          Lens.^? listUniqueProblemsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListUniqueProblems where
  type
    AWSResponse ListUniqueProblems =
      ListUniqueProblemsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUniqueProblemsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "uniqueProblems" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListUniqueProblems

instance Core.NFData ListUniqueProblems

instance Core.ToHeaders ListUniqueProblems where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListUniqueProblems" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListUniqueProblems where
  toJSON ListUniqueProblems' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            Core.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath ListUniqueProblems where
  toPath = Core.const "/"

instance Core.ToQuery ListUniqueProblems where
  toQuery = Core.const Core.mempty

-- | Represents the result of a list unique problems request.
--
-- /See:/ 'newListUniqueProblemsResponse' smart constructor.
data ListUniqueProblemsResponse = ListUniqueProblemsResponse'
  { -- | If the number of items that are returned is significantly large, this is
    -- an identifier that is also returned. It can be used in a subsequent call
    -- to this operation to return the next set of items in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the unique problems.
    --
    -- Allowed values include:
    --
    -- -   PENDING
    --
    -- -   PASSED
    --
    -- -   WARNED
    --
    -- -   FAILED
    --
    -- -   SKIPPED
    --
    -- -   ERRORED
    --
    -- -   STOPPED
    uniqueProblems :: Core.Maybe (Core.HashMap ExecutionResult [UniqueProblem]),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListUniqueProblemsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUniqueProblemsResponse_nextToken' - If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
--
-- 'uniqueProblems', 'listUniqueProblemsResponse_uniqueProblems' - Information about the unique problems.
--
-- Allowed values include:
--
-- -   PENDING
--
-- -   PASSED
--
-- -   WARNED
--
-- -   FAILED
--
-- -   SKIPPED
--
-- -   ERRORED
--
-- -   STOPPED
--
-- 'httpStatus', 'listUniqueProblemsResponse_httpStatus' - The response's http status code.
newListUniqueProblemsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListUniqueProblemsResponse
newListUniqueProblemsResponse pHttpStatus_ =
  ListUniqueProblemsResponse'
    { nextToken =
        Core.Nothing,
      uniqueProblems = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
listUniqueProblemsResponse_nextToken :: Lens.Lens' ListUniqueProblemsResponse (Core.Maybe Core.Text)
listUniqueProblemsResponse_nextToken = Lens.lens (\ListUniqueProblemsResponse' {nextToken} -> nextToken) (\s@ListUniqueProblemsResponse' {} a -> s {nextToken = a} :: ListUniqueProblemsResponse)

-- | Information about the unique problems.
--
-- Allowed values include:
--
-- -   PENDING
--
-- -   PASSED
--
-- -   WARNED
--
-- -   FAILED
--
-- -   SKIPPED
--
-- -   ERRORED
--
-- -   STOPPED
listUniqueProblemsResponse_uniqueProblems :: Lens.Lens' ListUniqueProblemsResponse (Core.Maybe (Core.HashMap ExecutionResult [UniqueProblem]))
listUniqueProblemsResponse_uniqueProblems = Lens.lens (\ListUniqueProblemsResponse' {uniqueProblems} -> uniqueProblems) (\s@ListUniqueProblemsResponse' {} a -> s {uniqueProblems = a} :: ListUniqueProblemsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listUniqueProblemsResponse_httpStatus :: Lens.Lens' ListUniqueProblemsResponse Core.Int
listUniqueProblemsResponse_httpStatus = Lens.lens (\ListUniqueProblemsResponse' {httpStatus} -> httpStatus) (\s@ListUniqueProblemsResponse' {} a -> s {httpStatus = a} :: ListUniqueProblemsResponse)

instance Core.NFData ListUniqueProblemsResponse
