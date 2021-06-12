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
-- Module      : Network.AWS.Greengrass.ListBulkDeployments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of bulk deployments.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListBulkDeployments
  ( -- * Creating a Request
    ListBulkDeployments (..),
    newListBulkDeployments,

    -- * Request Lenses
    listBulkDeployments_nextToken,
    listBulkDeployments_maxResults,

    -- * Destructuring the Response
    ListBulkDeploymentsResponse (..),
    newListBulkDeploymentsResponse,

    -- * Response Lenses
    listBulkDeploymentsResponse_nextToken,
    listBulkDeploymentsResponse_bulkDeployments,
    listBulkDeploymentsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListBulkDeployments' smart constructor.
data ListBulkDeployments = ListBulkDeployments'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBulkDeployments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBulkDeployments_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'maxResults', 'listBulkDeployments_maxResults' - The maximum number of results to be returned per request.
newListBulkDeployments ::
  ListBulkDeployments
newListBulkDeployments =
  ListBulkDeployments'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listBulkDeployments_nextToken :: Lens.Lens' ListBulkDeployments (Core.Maybe Core.Text)
listBulkDeployments_nextToken = Lens.lens (\ListBulkDeployments' {nextToken} -> nextToken) (\s@ListBulkDeployments' {} a -> s {nextToken = a} :: ListBulkDeployments)

-- | The maximum number of results to be returned per request.
listBulkDeployments_maxResults :: Lens.Lens' ListBulkDeployments (Core.Maybe Core.Text)
listBulkDeployments_maxResults = Lens.lens (\ListBulkDeployments' {maxResults} -> maxResults) (\s@ListBulkDeployments' {} a -> s {maxResults = a} :: ListBulkDeployments)

instance Core.AWSPager ListBulkDeployments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBulkDeploymentsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listBulkDeploymentsResponse_bulkDeployments
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listBulkDeployments_nextToken
          Lens..~ rs
          Lens.^? listBulkDeploymentsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListBulkDeployments where
  type
    AWSResponse ListBulkDeployments =
      ListBulkDeploymentsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBulkDeploymentsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "BulkDeployments" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListBulkDeployments

instance Core.NFData ListBulkDeployments

instance Core.ToHeaders ListBulkDeployments where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListBulkDeployments where
  toPath = Core.const "/greengrass/bulk/deployments"

instance Core.ToQuery ListBulkDeployments where
  toQuery ListBulkDeployments' {..} =
    Core.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListBulkDeploymentsResponse' smart constructor.
data ListBulkDeploymentsResponse = ListBulkDeploymentsResponse'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of bulk deployments.
    bulkDeployments :: Core.Maybe [BulkDeployment],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBulkDeploymentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBulkDeploymentsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'bulkDeployments', 'listBulkDeploymentsResponse_bulkDeployments' - A list of bulk deployments.
--
-- 'httpStatus', 'listBulkDeploymentsResponse_httpStatus' - The response's http status code.
newListBulkDeploymentsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListBulkDeploymentsResponse
newListBulkDeploymentsResponse pHttpStatus_ =
  ListBulkDeploymentsResponse'
    { nextToken =
        Core.Nothing,
      bulkDeployments = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listBulkDeploymentsResponse_nextToken :: Lens.Lens' ListBulkDeploymentsResponse (Core.Maybe Core.Text)
listBulkDeploymentsResponse_nextToken = Lens.lens (\ListBulkDeploymentsResponse' {nextToken} -> nextToken) (\s@ListBulkDeploymentsResponse' {} a -> s {nextToken = a} :: ListBulkDeploymentsResponse)

-- | A list of bulk deployments.
listBulkDeploymentsResponse_bulkDeployments :: Lens.Lens' ListBulkDeploymentsResponse (Core.Maybe [BulkDeployment])
listBulkDeploymentsResponse_bulkDeployments = Lens.lens (\ListBulkDeploymentsResponse' {bulkDeployments} -> bulkDeployments) (\s@ListBulkDeploymentsResponse' {} a -> s {bulkDeployments = a} :: ListBulkDeploymentsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listBulkDeploymentsResponse_httpStatus :: Lens.Lens' ListBulkDeploymentsResponse Core.Int
listBulkDeploymentsResponse_httpStatus = Lens.lens (\ListBulkDeploymentsResponse' {httpStatus} -> httpStatus) (\s@ListBulkDeploymentsResponse' {} a -> s {httpStatus = a} :: ListBulkDeploymentsResponse)

instance Core.NFData ListBulkDeploymentsResponse
