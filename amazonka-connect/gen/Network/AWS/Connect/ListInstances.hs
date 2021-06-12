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
-- Module      : Network.AWS.Connect.ListInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Return a list of instances which are in active state,
-- creation-in-progress state, and failed state. Instances that aren\'t
-- successfully created (they are in a failed state) are returned only for
-- 24 hours after the CreateInstance API was invoked.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListInstances
  ( -- * Creating a Request
    ListInstances (..),
    newListInstances,

    -- * Request Lenses
    listInstances_nextToken,
    listInstances_maxResults,

    -- * Destructuring the Response
    ListInstancesResponse (..),
    newListInstancesResponse,

    -- * Response Lenses
    listInstancesResponse_nextToken,
    listInstancesResponse_instanceSummaryList,
    listInstancesResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListInstances' smart constructor.
data ListInstances = ListInstances'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInstances_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listInstances_maxResults' - The maximum number of results to return per page.
newListInstances ::
  ListInstances
newListInstances =
  ListInstances'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listInstances_nextToken :: Lens.Lens' ListInstances (Core.Maybe Core.Text)
listInstances_nextToken = Lens.lens (\ListInstances' {nextToken} -> nextToken) (\s@ListInstances' {} a -> s {nextToken = a} :: ListInstances)

-- | The maximum number of results to return per page.
listInstances_maxResults :: Lens.Lens' ListInstances (Core.Maybe Core.Natural)
listInstances_maxResults = Lens.lens (\ListInstances' {maxResults} -> maxResults) (\s@ListInstances' {} a -> s {maxResults = a} :: ListInstances)

instance Core.AWSPager ListInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInstancesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listInstancesResponse_instanceSummaryList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listInstances_nextToken
          Lens..~ rs
          Lens.^? listInstancesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListInstances where
  type
    AWSResponse ListInstances =
      ListInstancesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstancesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "InstanceSummaryList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListInstances

instance Core.NFData ListInstances

instance Core.ToHeaders ListInstances where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListInstances where
  toPath = Core.const "/instance"

instance Core.ToQuery ListInstances where
  toQuery ListInstances' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListInstancesResponse' smart constructor.
data ListInstancesResponse = ListInstancesResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the instances.
    instanceSummaryList :: Core.Maybe [InstanceSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInstancesResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'instanceSummaryList', 'listInstancesResponse_instanceSummaryList' - Information about the instances.
--
-- 'httpStatus', 'listInstancesResponse_httpStatus' - The response's http status code.
newListInstancesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListInstancesResponse
newListInstancesResponse pHttpStatus_ =
  ListInstancesResponse'
    { nextToken = Core.Nothing,
      instanceSummaryList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listInstancesResponse_nextToken :: Lens.Lens' ListInstancesResponse (Core.Maybe Core.Text)
listInstancesResponse_nextToken = Lens.lens (\ListInstancesResponse' {nextToken} -> nextToken) (\s@ListInstancesResponse' {} a -> s {nextToken = a} :: ListInstancesResponse)

-- | Information about the instances.
listInstancesResponse_instanceSummaryList :: Lens.Lens' ListInstancesResponse (Core.Maybe [InstanceSummary])
listInstancesResponse_instanceSummaryList = Lens.lens (\ListInstancesResponse' {instanceSummaryList} -> instanceSummaryList) (\s@ListInstancesResponse' {} a -> s {instanceSummaryList = a} :: ListInstancesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listInstancesResponse_httpStatus :: Lens.Lens' ListInstancesResponse Core.Int
listInstancesResponse_httpStatus = Lens.lens (\ListInstancesResponse' {httpStatus} -> httpStatus) (\s@ListInstancesResponse' {} a -> s {httpStatus = a} :: ListInstancesResponse)

instance Core.NFData ListInstancesResponse
