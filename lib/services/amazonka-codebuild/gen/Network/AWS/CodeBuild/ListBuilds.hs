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
    listBuildsResponse_ids,
    listBuildsResponse_nextToken,
    listBuildsResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListBuilds' smart constructor.
data ListBuilds = ListBuilds'
  { -- | The order to list build IDs. Valid values include:
    --
    -- -   @ASCENDING@: List the build IDs in ascending order by build ID.
    --
    -- -   @DESCENDING@: List the build IDs in descending order by build ID.
    sortOrder :: Prelude.Maybe SortOrderType,
    -- | During a previous call, if there are more than 100 items in the list,
    -- only the first 100 items are returned, along with a unique string called
    -- a /nextToken/. To get the next batch of items in the list, call this
    -- operation again, adding the next token to the call. To get all of the
    -- items in the list, keep calling this operation with each subsequent next
    -- token that is returned, until no more next tokens are returned.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The order to list build IDs. Valid values include:
--
-- -   @ASCENDING@: List the build IDs in ascending order by build ID.
--
-- -   @DESCENDING@: List the build IDs in descending order by build ID.
listBuilds_sortOrder :: Lens.Lens' ListBuilds (Prelude.Maybe SortOrderType)
listBuilds_sortOrder = Lens.lens (\ListBuilds' {sortOrder} -> sortOrder) (\s@ListBuilds' {} a -> s {sortOrder = a} :: ListBuilds)

-- | During a previous call, if there are more than 100 items in the list,
-- only the first 100 items are returned, along with a unique string called
-- a /nextToken/. To get the next batch of items in the list, call this
-- operation again, adding the next token to the call. To get all of the
-- items in the list, keep calling this operation with each subsequent next
-- token that is returned, until no more next tokens are returned.
listBuilds_nextToken :: Lens.Lens' ListBuilds (Prelude.Maybe Prelude.Text)
listBuilds_nextToken = Lens.lens (\ListBuilds' {nextToken} -> nextToken) (\s@ListBuilds' {} a -> s {nextToken = a} :: ListBuilds)

instance Core.AWSPager ListBuilds where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBuildsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listBuildsResponse_ids Prelude.. Lens._Just
              Prelude.. Lens.to Prelude.toList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listBuilds_nextToken
          Lens..~ rs
          Lens.^? listBuildsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListBuilds where
  type AWSResponse ListBuilds = ListBuildsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBuildsResponse'
            Prelude.<$> (x Core..?> "ids")
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBuilds

instance Prelude.NFData ListBuilds

instance Core.ToHeaders ListBuilds where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.ListBuilds" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListBuilds where
  toJSON ListBuilds' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("sortOrder" Core..=) Prelude.<$> sortOrder,
            ("nextToken" Core..=) Prelude.<$> nextToken
          ]
      )

instance Core.ToPath ListBuilds where
  toPath = Prelude.const "/"

instance Core.ToQuery ListBuilds where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBuildsResponse' smart constructor.
data ListBuildsResponse = ListBuildsResponse'
  { -- | A list of build IDs, with each build ID representing a single build.
    ids :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | If there are more than 100 items in the list, only the first 100 items
    -- are returned, along with a unique string called a /nextToken/. To get
    -- the next batch of items in the list, call this operation again, adding
    -- the next token to the call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBuildsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ids', 'listBuildsResponse_ids' - A list of build IDs, with each build ID representing a single build.
--
-- 'nextToken', 'listBuildsResponse_nextToken' - If there are more than 100 items in the list, only the first 100 items
-- are returned, along with a unique string called a /nextToken/. To get
-- the next batch of items in the list, call this operation again, adding
-- the next token to the call.
--
-- 'httpStatus', 'listBuildsResponse_httpStatus' - The response's http status code.
newListBuildsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBuildsResponse
newListBuildsResponse pHttpStatus_ =
  ListBuildsResponse'
    { ids = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of build IDs, with each build ID representing a single build.
listBuildsResponse_ids :: Lens.Lens' ListBuildsResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listBuildsResponse_ids = Lens.lens (\ListBuildsResponse' {ids} -> ids) (\s@ListBuildsResponse' {} a -> s {ids = a} :: ListBuildsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are more than 100 items in the list, only the first 100 items
-- are returned, along with a unique string called a /nextToken/. To get
-- the next batch of items in the list, call this operation again, adding
-- the next token to the call.
listBuildsResponse_nextToken :: Lens.Lens' ListBuildsResponse (Prelude.Maybe Prelude.Text)
listBuildsResponse_nextToken = Lens.lens (\ListBuildsResponse' {nextToken} -> nextToken) (\s@ListBuildsResponse' {} a -> s {nextToken = a} :: ListBuildsResponse)

-- | The response's http status code.
listBuildsResponse_httpStatus :: Lens.Lens' ListBuildsResponse Prelude.Int
listBuildsResponse_httpStatus = Lens.lens (\ListBuildsResponse' {httpStatus} -> httpStatus) (\s@ListBuildsResponse' {} a -> s {httpStatus = a} :: ListBuildsResponse)

instance Prelude.NFData ListBuildsResponse
