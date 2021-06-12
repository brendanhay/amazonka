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
-- Module      : Network.AWS.IoT.ListIndices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the search indices.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListIndices
  ( -- * Creating a Request
    ListIndices (..),
    newListIndices,

    -- * Request Lenses
    listIndices_nextToken,
    listIndices_maxResults,

    -- * Destructuring the Response
    ListIndicesResponse (..),
    newListIndicesResponse,

    -- * Response Lenses
    listIndicesResponse_nextToken,
    listIndicesResponse_indexNames,
    listIndicesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListIndices' smart constructor.
data ListIndices = ListIndices'
  { -- | The token used to get the next set of results, or @null@ if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListIndices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listIndices_nextToken' - The token used to get the next set of results, or @null@ if there are no
-- additional results.
--
-- 'maxResults', 'listIndices_maxResults' - The maximum number of results to return at one time.
newListIndices ::
  ListIndices
newListIndices =
  ListIndices'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The token used to get the next set of results, or @null@ if there are no
-- additional results.
listIndices_nextToken :: Lens.Lens' ListIndices (Core.Maybe Core.Text)
listIndices_nextToken = Lens.lens (\ListIndices' {nextToken} -> nextToken) (\s@ListIndices' {} a -> s {nextToken = a} :: ListIndices)

-- | The maximum number of results to return at one time.
listIndices_maxResults :: Lens.Lens' ListIndices (Core.Maybe Core.Natural)
listIndices_maxResults = Lens.lens (\ListIndices' {maxResults} -> maxResults) (\s@ListIndices' {} a -> s {maxResults = a} :: ListIndices)

instance Core.AWSPager ListIndices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listIndicesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listIndicesResponse_indexNames Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listIndices_nextToken
          Lens..~ rs
          Lens.^? listIndicesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListIndices where
  type AWSResponse ListIndices = ListIndicesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIndicesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "indexNames" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListIndices

instance Core.NFData ListIndices

instance Core.ToHeaders ListIndices where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListIndices where
  toPath = Core.const "/indices"

instance Core.ToQuery ListIndices where
  toQuery ListIndices' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListIndicesResponse' smart constructor.
data ListIndicesResponse = ListIndicesResponse'
  { -- | The token used to get the next set of results, or @null@ if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The index names.
    indexNames :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListIndicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listIndicesResponse_nextToken' - The token used to get the next set of results, or @null@ if there are no
-- additional results.
--
-- 'indexNames', 'listIndicesResponse_indexNames' - The index names.
--
-- 'httpStatus', 'listIndicesResponse_httpStatus' - The response's http status code.
newListIndicesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListIndicesResponse
newListIndicesResponse pHttpStatus_ =
  ListIndicesResponse'
    { nextToken = Core.Nothing,
      indexNames = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token used to get the next set of results, or @null@ if there are no
-- additional results.
listIndicesResponse_nextToken :: Lens.Lens' ListIndicesResponse (Core.Maybe Core.Text)
listIndicesResponse_nextToken = Lens.lens (\ListIndicesResponse' {nextToken} -> nextToken) (\s@ListIndicesResponse' {} a -> s {nextToken = a} :: ListIndicesResponse)

-- | The index names.
listIndicesResponse_indexNames :: Lens.Lens' ListIndicesResponse (Core.Maybe [Core.Text])
listIndicesResponse_indexNames = Lens.lens (\ListIndicesResponse' {indexNames} -> indexNames) (\s@ListIndicesResponse' {} a -> s {indexNames = a} :: ListIndicesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listIndicesResponse_httpStatus :: Lens.Lens' ListIndicesResponse Core.Int
listIndicesResponse_httpStatus = Lens.lens (\ListIndicesResponse' {httpStatus} -> httpStatus) (\s@ListIndicesResponse' {} a -> s {httpStatus = a} :: ListIndicesResponse)

instance Core.NFData ListIndicesResponse
