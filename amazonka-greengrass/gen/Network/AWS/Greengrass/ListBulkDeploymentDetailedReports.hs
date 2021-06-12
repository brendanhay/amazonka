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
-- Module      : Network.AWS.Greengrass.ListBulkDeploymentDetailedReports
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a paginated list of the deployments that have been started in a
-- bulk deployment operation, and their current deployment status.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListBulkDeploymentDetailedReports
  ( -- * Creating a Request
    ListBulkDeploymentDetailedReports (..),
    newListBulkDeploymentDetailedReports,

    -- * Request Lenses
    listBulkDeploymentDetailedReports_nextToken,
    listBulkDeploymentDetailedReports_maxResults,
    listBulkDeploymentDetailedReports_bulkDeploymentId,

    -- * Destructuring the Response
    ListBulkDeploymentDetailedReportsResponse (..),
    newListBulkDeploymentDetailedReportsResponse,

    -- * Response Lenses
    listBulkDeploymentDetailedReportsResponse_nextToken,
    listBulkDeploymentDetailedReportsResponse_deployments,
    listBulkDeploymentDetailedReportsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListBulkDeploymentDetailedReports' smart constructor.
data ListBulkDeploymentDetailedReports = ListBulkDeploymentDetailedReports'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text,
    -- | The ID of the bulk deployment.
    bulkDeploymentId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBulkDeploymentDetailedReports' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBulkDeploymentDetailedReports_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'maxResults', 'listBulkDeploymentDetailedReports_maxResults' - The maximum number of results to be returned per request.
--
-- 'bulkDeploymentId', 'listBulkDeploymentDetailedReports_bulkDeploymentId' - The ID of the bulk deployment.
newListBulkDeploymentDetailedReports ::
  -- | 'bulkDeploymentId'
  Core.Text ->
  ListBulkDeploymentDetailedReports
newListBulkDeploymentDetailedReports
  pBulkDeploymentId_ =
    ListBulkDeploymentDetailedReports'
      { nextToken =
          Core.Nothing,
        maxResults = Core.Nothing,
        bulkDeploymentId = pBulkDeploymentId_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listBulkDeploymentDetailedReports_nextToken :: Lens.Lens' ListBulkDeploymentDetailedReports (Core.Maybe Core.Text)
listBulkDeploymentDetailedReports_nextToken = Lens.lens (\ListBulkDeploymentDetailedReports' {nextToken} -> nextToken) (\s@ListBulkDeploymentDetailedReports' {} a -> s {nextToken = a} :: ListBulkDeploymentDetailedReports)

-- | The maximum number of results to be returned per request.
listBulkDeploymentDetailedReports_maxResults :: Lens.Lens' ListBulkDeploymentDetailedReports (Core.Maybe Core.Text)
listBulkDeploymentDetailedReports_maxResults = Lens.lens (\ListBulkDeploymentDetailedReports' {maxResults} -> maxResults) (\s@ListBulkDeploymentDetailedReports' {} a -> s {maxResults = a} :: ListBulkDeploymentDetailedReports)

-- | The ID of the bulk deployment.
listBulkDeploymentDetailedReports_bulkDeploymentId :: Lens.Lens' ListBulkDeploymentDetailedReports Core.Text
listBulkDeploymentDetailedReports_bulkDeploymentId = Lens.lens (\ListBulkDeploymentDetailedReports' {bulkDeploymentId} -> bulkDeploymentId) (\s@ListBulkDeploymentDetailedReports' {} a -> s {bulkDeploymentId = a} :: ListBulkDeploymentDetailedReports)

instance
  Core.AWSPager
    ListBulkDeploymentDetailedReports
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBulkDeploymentDetailedReportsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listBulkDeploymentDetailedReportsResponse_deployments
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listBulkDeploymentDetailedReports_nextToken
          Lens..~ rs
          Lens.^? listBulkDeploymentDetailedReportsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    ListBulkDeploymentDetailedReports
  where
  type
    AWSResponse ListBulkDeploymentDetailedReports =
      ListBulkDeploymentDetailedReportsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBulkDeploymentDetailedReportsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Deployments" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListBulkDeploymentDetailedReports

instance
  Core.NFData
    ListBulkDeploymentDetailedReports

instance
  Core.ToHeaders
    ListBulkDeploymentDetailedReports
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToPath
    ListBulkDeploymentDetailedReports
  where
  toPath ListBulkDeploymentDetailedReports' {..} =
    Core.mconcat
      [ "/greengrass/bulk/deployments/",
        Core.toBS bulkDeploymentId,
        "/detailed-reports"
      ]

instance
  Core.ToQuery
    ListBulkDeploymentDetailedReports
  where
  toQuery ListBulkDeploymentDetailedReports' {..} =
    Core.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListBulkDeploymentDetailedReportsResponse' smart constructor.
data ListBulkDeploymentDetailedReportsResponse = ListBulkDeploymentDetailedReportsResponse'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of the individual group deployments in the bulk deployment
    -- operation.
    deployments :: Core.Maybe [BulkDeploymentResult],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBulkDeploymentDetailedReportsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBulkDeploymentDetailedReportsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'deployments', 'listBulkDeploymentDetailedReportsResponse_deployments' - A list of the individual group deployments in the bulk deployment
-- operation.
--
-- 'httpStatus', 'listBulkDeploymentDetailedReportsResponse_httpStatus' - The response's http status code.
newListBulkDeploymentDetailedReportsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListBulkDeploymentDetailedReportsResponse
newListBulkDeploymentDetailedReportsResponse
  pHttpStatus_ =
    ListBulkDeploymentDetailedReportsResponse'
      { nextToken =
          Core.Nothing,
        deployments = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listBulkDeploymentDetailedReportsResponse_nextToken :: Lens.Lens' ListBulkDeploymentDetailedReportsResponse (Core.Maybe Core.Text)
listBulkDeploymentDetailedReportsResponse_nextToken = Lens.lens (\ListBulkDeploymentDetailedReportsResponse' {nextToken} -> nextToken) (\s@ListBulkDeploymentDetailedReportsResponse' {} a -> s {nextToken = a} :: ListBulkDeploymentDetailedReportsResponse)

-- | A list of the individual group deployments in the bulk deployment
-- operation.
listBulkDeploymentDetailedReportsResponse_deployments :: Lens.Lens' ListBulkDeploymentDetailedReportsResponse (Core.Maybe [BulkDeploymentResult])
listBulkDeploymentDetailedReportsResponse_deployments = Lens.lens (\ListBulkDeploymentDetailedReportsResponse' {deployments} -> deployments) (\s@ListBulkDeploymentDetailedReportsResponse' {} a -> s {deployments = a} :: ListBulkDeploymentDetailedReportsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listBulkDeploymentDetailedReportsResponse_httpStatus :: Lens.Lens' ListBulkDeploymentDetailedReportsResponse Core.Int
listBulkDeploymentDetailedReportsResponse_httpStatus = Lens.lens (\ListBulkDeploymentDetailedReportsResponse' {httpStatus} -> httpStatus) (\s@ListBulkDeploymentDetailedReportsResponse' {} a -> s {httpStatus = a} :: ListBulkDeploymentDetailedReportsResponse)

instance
  Core.NFData
    ListBulkDeploymentDetailedReportsResponse
