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
-- Module      : Network.AWS.CodeBuild.ListBuilds
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of build IDs, with each build ID representing a single
-- build.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListBuilds
  ( -- * Creating a Request
    ListBuilds (..),
    newListBuilds,

    -- * Request Lenses
    listBuilds_sortOrder,
    listBuilds_nextToken,

    -- * Destructuring the Response
    ListBuildsResponse (..),
    newListBuildsResponse,

    -- * Response Lenses
    listBuildsResponse_nextToken,
    listBuildsResponse_ids,
    listBuildsResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListBuilds' smart constructor.
data ListBuilds = ListBuilds'
  { -- | The order to list build IDs. Valid values include:
    --
    -- -   @ASCENDING@: List the build IDs in ascending order by build ID.
    --
    -- -   @DESCENDING@: List the build IDs in descending order by build ID.
    sortOrder :: Core.Maybe SortOrderType,
    -- | During a previous call, if there are more than 100 items in the list,
    -- only the first 100 items are returned, along with a unique string called
    -- a /nextToken/. To get the next batch of items in the list, call this
    -- operation again, adding the next token to the call. To get all of the
    -- items in the list, keep calling this operation with each subsequent next
    -- token that is returned, until no more next tokens are returned.
    nextToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBuilds' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listBuilds_sortOrder' - The order to list build IDs. Valid values include:
--
-- -   @ASCENDING@: List the build IDs in ascending order by build ID.
--
-- -   @DESCENDING@: List the build IDs in descending order by build ID.
--
-- 'nextToken', 'listBuilds_nextToken' - During a previous call, if there are more than 100 items in the list,
-- only the first 100 items are returned, along with a unique string called
-- a /nextToken/. To get the next batch of items in the list, call this
-- operation again, adding the next token to the call. To get all of the
-- items in the list, keep calling this operation with each subsequent next
-- token that is returned, until no more next tokens are returned.
newListBuilds ::
  ListBuilds
newListBuilds =
  ListBuilds'
    { sortOrder = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The order to list build IDs. Valid values include:
--
-- -   @ASCENDING@: List the build IDs in ascending order by build ID.
--
-- -   @DESCENDING@: List the build IDs in descending order by build ID.
listBuilds_sortOrder :: Lens.Lens' ListBuilds (Core.Maybe SortOrderType)
listBuilds_sortOrder = Lens.lens (\ListBuilds' {sortOrder} -> sortOrder) (\s@ListBuilds' {} a -> s {sortOrder = a} :: ListBuilds)

-- | During a previous call, if there are more than 100 items in the list,
-- only the first 100 items are returned, along with a unique string called
-- a /nextToken/. To get the next batch of items in the list, call this
-- operation again, adding the next token to the call. To get all of the
-- items in the list, keep calling this operation with each subsequent next
-- token that is returned, until no more next tokens are returned.
listBuilds_nextToken :: Lens.Lens' ListBuilds (Core.Maybe Core.Text)
listBuilds_nextToken = Lens.lens (\ListBuilds' {nextToken} -> nextToken) (\s@ListBuilds' {} a -> s {nextToken = a} :: ListBuilds)

instance Core.AWSPager ListBuilds where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBuildsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listBuildsResponse_ids Core.. Lens._Just
              Core.. Lens.to Core.toList
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listBuilds_nextToken
          Lens..~ rs
          Lens.^? listBuildsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListBuilds where
  type AWSResponse ListBuilds = ListBuildsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBuildsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "ids")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListBuilds

instance Core.NFData ListBuilds

instance Core.ToHeaders ListBuilds where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("CodeBuild_20161006.ListBuilds" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListBuilds where
  toJSON ListBuilds' {..} =
    Core.object
      ( Core.catMaybes
          [ ("sortOrder" Core..=) Core.<$> sortOrder,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.ToPath ListBuilds where
  toPath = Core.const "/"

instance Core.ToQuery ListBuilds where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListBuildsResponse' smart constructor.
data ListBuildsResponse = ListBuildsResponse'
  { -- | If there are more than 100 items in the list, only the first 100 items
    -- are returned, along with a unique string called a /nextToken/. To get
    -- the next batch of items in the list, call this operation again, adding
    -- the next token to the call.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of build IDs, with each build ID representing a single build.
    ids :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBuildsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBuildsResponse_nextToken' - If there are more than 100 items in the list, only the first 100 items
-- are returned, along with a unique string called a /nextToken/. To get
-- the next batch of items in the list, call this operation again, adding
-- the next token to the call.
--
-- 'ids', 'listBuildsResponse_ids' - A list of build IDs, with each build ID representing a single build.
--
-- 'httpStatus', 'listBuildsResponse_httpStatus' - The response's http status code.
newListBuildsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListBuildsResponse
newListBuildsResponse pHttpStatus_ =
  ListBuildsResponse'
    { nextToken = Core.Nothing,
      ids = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are more than 100 items in the list, only the first 100 items
-- are returned, along with a unique string called a /nextToken/. To get
-- the next batch of items in the list, call this operation again, adding
-- the next token to the call.
listBuildsResponse_nextToken :: Lens.Lens' ListBuildsResponse (Core.Maybe Core.Text)
listBuildsResponse_nextToken = Lens.lens (\ListBuildsResponse' {nextToken} -> nextToken) (\s@ListBuildsResponse' {} a -> s {nextToken = a} :: ListBuildsResponse)

-- | A list of build IDs, with each build ID representing a single build.
listBuildsResponse_ids :: Lens.Lens' ListBuildsResponse (Core.Maybe (Core.NonEmpty Core.Text))
listBuildsResponse_ids = Lens.lens (\ListBuildsResponse' {ids} -> ids) (\s@ListBuildsResponse' {} a -> s {ids = a} :: ListBuildsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listBuildsResponse_httpStatus :: Lens.Lens' ListBuildsResponse Core.Int
listBuildsResponse_httpStatus = Lens.lens (\ListBuildsResponse' {httpStatus} -> httpStatus) (\s@ListBuildsResponse' {} a -> s {httpStatus = a} :: ListBuildsResponse)

instance Core.NFData ListBuildsResponse
