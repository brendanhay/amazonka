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
-- Module      : Network.AWS.Greengrass.ListDeployments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a history of deployments for the group.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListDeployments
  ( -- * Creating a Request
    ListDeployments (..),
    newListDeployments,

    -- * Request Lenses
    listDeployments_nextToken,
    listDeployments_maxResults,
    listDeployments_groupId,

    -- * Destructuring the Response
    ListDeploymentsResponse (..),
    newListDeploymentsResponse,

    -- * Response Lenses
    listDeploymentsResponse_nextToken,
    listDeploymentsResponse_deployments,
    listDeploymentsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDeployments' smart constructor.
data ListDeployments = ListDeployments'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text,
    -- | The ID of the Greengrass group.
    groupId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDeployments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeployments_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'maxResults', 'listDeployments_maxResults' - The maximum number of results to be returned per request.
--
-- 'groupId', 'listDeployments_groupId' - The ID of the Greengrass group.
newListDeployments ::
  -- | 'groupId'
  Core.Text ->
  ListDeployments
newListDeployments pGroupId_ =
  ListDeployments'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      groupId = pGroupId_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listDeployments_nextToken :: Lens.Lens' ListDeployments (Core.Maybe Core.Text)
listDeployments_nextToken = Lens.lens (\ListDeployments' {nextToken} -> nextToken) (\s@ListDeployments' {} a -> s {nextToken = a} :: ListDeployments)

-- | The maximum number of results to be returned per request.
listDeployments_maxResults :: Lens.Lens' ListDeployments (Core.Maybe Core.Text)
listDeployments_maxResults = Lens.lens (\ListDeployments' {maxResults} -> maxResults) (\s@ListDeployments' {} a -> s {maxResults = a} :: ListDeployments)

-- | The ID of the Greengrass group.
listDeployments_groupId :: Lens.Lens' ListDeployments Core.Text
listDeployments_groupId = Lens.lens (\ListDeployments' {groupId} -> groupId) (\s@ListDeployments' {} a -> s {groupId = a} :: ListDeployments)

instance Core.AWSPager ListDeployments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeploymentsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDeploymentsResponse_deployments
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDeployments_nextToken
          Lens..~ rs
          Lens.^? listDeploymentsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListDeployments where
  type
    AWSResponse ListDeployments =
      ListDeploymentsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeploymentsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Deployments" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDeployments

instance Core.NFData ListDeployments

instance Core.ToHeaders ListDeployments where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListDeployments where
  toPath ListDeployments' {..} =
    Core.mconcat
      [ "/greengrass/groups/",
        Core.toBS groupId,
        "/deployments"
      ]

instance Core.ToQuery ListDeployments where
  toQuery ListDeployments' {..} =
    Core.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListDeploymentsResponse' smart constructor.
data ListDeploymentsResponse = ListDeploymentsResponse'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of deployments for the requested groups.
    deployments :: Core.Maybe [Deployment],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDeploymentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeploymentsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'deployments', 'listDeploymentsResponse_deployments' - A list of deployments for the requested groups.
--
-- 'httpStatus', 'listDeploymentsResponse_httpStatus' - The response's http status code.
newListDeploymentsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDeploymentsResponse
newListDeploymentsResponse pHttpStatus_ =
  ListDeploymentsResponse'
    { nextToken = Core.Nothing,
      deployments = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listDeploymentsResponse_nextToken :: Lens.Lens' ListDeploymentsResponse (Core.Maybe Core.Text)
listDeploymentsResponse_nextToken = Lens.lens (\ListDeploymentsResponse' {nextToken} -> nextToken) (\s@ListDeploymentsResponse' {} a -> s {nextToken = a} :: ListDeploymentsResponse)

-- | A list of deployments for the requested groups.
listDeploymentsResponse_deployments :: Lens.Lens' ListDeploymentsResponse (Core.Maybe [Deployment])
listDeploymentsResponse_deployments = Lens.lens (\ListDeploymentsResponse' {deployments} -> deployments) (\s@ListDeploymentsResponse' {} a -> s {deployments = a} :: ListDeploymentsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDeploymentsResponse_httpStatus :: Lens.Lens' ListDeploymentsResponse Core.Int
listDeploymentsResponse_httpStatus = Lens.lens (\ListDeploymentsResponse' {httpStatus} -> httpStatus) (\s@ListDeploymentsResponse' {} a -> s {httpStatus = a} :: ListDeploymentsResponse)

instance Core.NFData ListDeploymentsResponse
