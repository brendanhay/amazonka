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
-- Module      : Network.AWS.IoTAnalytics.ListDatastores
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of data stores.
--
-- This operation returns paginated results.
module Network.AWS.IoTAnalytics.ListDatastores
  ( -- * Creating a Request
    ListDatastores (..),
    newListDatastores,

    -- * Request Lenses
    listDatastores_nextToken,
    listDatastores_maxResults,

    -- * Destructuring the Response
    ListDatastoresResponse (..),
    newListDatastoresResponse,

    -- * Response Lenses
    listDatastoresResponse_nextToken,
    listDatastoresResponse_datastoreSummaries,
    listDatastoresResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDatastores' smart constructor.
data ListDatastores = ListDatastores'
  { -- | The token for the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in this request.
    --
    -- The default value is 100.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDatastores' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatastores_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'listDatastores_maxResults' - The maximum number of results to return in this request.
--
-- The default value is 100.
newListDatastores ::
  ListDatastores
newListDatastores =
  ListDatastores'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The token for the next set of results.
listDatastores_nextToken :: Lens.Lens' ListDatastores (Core.Maybe Core.Text)
listDatastores_nextToken = Lens.lens (\ListDatastores' {nextToken} -> nextToken) (\s@ListDatastores' {} a -> s {nextToken = a} :: ListDatastores)

-- | The maximum number of results to return in this request.
--
-- The default value is 100.
listDatastores_maxResults :: Lens.Lens' ListDatastores (Core.Maybe Core.Natural)
listDatastores_maxResults = Lens.lens (\ListDatastores' {maxResults} -> maxResults) (\s@ListDatastores' {} a -> s {maxResults = a} :: ListDatastores)

instance Core.AWSPager ListDatastores where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDatastoresResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDatastoresResponse_datastoreSummaries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDatastores_nextToken
          Lens..~ rs
          Lens.^? listDatastoresResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListDatastores where
  type
    AWSResponse ListDatastores =
      ListDatastoresResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDatastoresResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> ( x Core..?> "datastoreSummaries"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDatastores

instance Core.NFData ListDatastores

instance Core.ToHeaders ListDatastores where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListDatastores where
  toPath = Core.const "/datastores"

instance Core.ToQuery ListDatastores where
  toQuery ListDatastores' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListDatastoresResponse' smart constructor.
data ListDatastoresResponse = ListDatastoresResponse'
  { -- | The token to retrieve the next set of results, or @null@ if there are no
    -- more results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of @DatastoreSummary@ objects.
    datastoreSummaries :: Core.Maybe [DatastoreSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDatastoresResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatastoresResponse_nextToken' - The token to retrieve the next set of results, or @null@ if there are no
-- more results.
--
-- 'datastoreSummaries', 'listDatastoresResponse_datastoreSummaries' - A list of @DatastoreSummary@ objects.
--
-- 'httpStatus', 'listDatastoresResponse_httpStatus' - The response's http status code.
newListDatastoresResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDatastoresResponse
newListDatastoresResponse pHttpStatus_ =
  ListDatastoresResponse'
    { nextToken = Core.Nothing,
      datastoreSummaries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to retrieve the next set of results, or @null@ if there are no
-- more results.
listDatastoresResponse_nextToken :: Lens.Lens' ListDatastoresResponse (Core.Maybe Core.Text)
listDatastoresResponse_nextToken = Lens.lens (\ListDatastoresResponse' {nextToken} -> nextToken) (\s@ListDatastoresResponse' {} a -> s {nextToken = a} :: ListDatastoresResponse)

-- | A list of @DatastoreSummary@ objects.
listDatastoresResponse_datastoreSummaries :: Lens.Lens' ListDatastoresResponse (Core.Maybe [DatastoreSummary])
listDatastoresResponse_datastoreSummaries = Lens.lens (\ListDatastoresResponse' {datastoreSummaries} -> datastoreSummaries) (\s@ListDatastoresResponse' {} a -> s {datastoreSummaries = a} :: ListDatastoresResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDatastoresResponse_httpStatus :: Lens.Lens' ListDatastoresResponse Core.Int
listDatastoresResponse_httpStatus = Lens.lens (\ListDatastoresResponse' {httpStatus} -> httpStatus) (\s@ListDatastoresResponse' {} a -> s {httpStatus = a} :: ListDatastoresResponse)

instance Core.NFData ListDatastoresResponse
