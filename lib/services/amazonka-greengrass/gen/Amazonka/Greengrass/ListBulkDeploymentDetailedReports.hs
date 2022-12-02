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
-- Module      : Amazonka.Greengrass.ListBulkDeploymentDetailedReports
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a paginated list of the deployments that have been started in a
-- bulk deployment operation, and their current deployment status.
--
-- This operation returns paginated results.
module Amazonka.Greengrass.ListBulkDeploymentDetailedReports
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBulkDeploymentDetailedReports' smart constructor.
data ListBulkDeploymentDetailedReports = ListBulkDeploymentDetailedReports'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The ID of the bulk deployment.
    bulkDeploymentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListBulkDeploymentDetailedReports
newListBulkDeploymentDetailedReports
  pBulkDeploymentId_ =
    ListBulkDeploymentDetailedReports'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        bulkDeploymentId = pBulkDeploymentId_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listBulkDeploymentDetailedReports_nextToken :: Lens.Lens' ListBulkDeploymentDetailedReports (Prelude.Maybe Prelude.Text)
listBulkDeploymentDetailedReports_nextToken = Lens.lens (\ListBulkDeploymentDetailedReports' {nextToken} -> nextToken) (\s@ListBulkDeploymentDetailedReports' {} a -> s {nextToken = a} :: ListBulkDeploymentDetailedReports)

-- | The maximum number of results to be returned per request.
listBulkDeploymentDetailedReports_maxResults :: Lens.Lens' ListBulkDeploymentDetailedReports (Prelude.Maybe Prelude.Text)
listBulkDeploymentDetailedReports_maxResults = Lens.lens (\ListBulkDeploymentDetailedReports' {maxResults} -> maxResults) (\s@ListBulkDeploymentDetailedReports' {} a -> s {maxResults = a} :: ListBulkDeploymentDetailedReports)

-- | The ID of the bulk deployment.
listBulkDeploymentDetailedReports_bulkDeploymentId :: Lens.Lens' ListBulkDeploymentDetailedReports Prelude.Text
listBulkDeploymentDetailedReports_bulkDeploymentId = Lens.lens (\ListBulkDeploymentDetailedReports' {bulkDeploymentId} -> bulkDeploymentId) (\s@ListBulkDeploymentDetailedReports' {} a -> s {bulkDeploymentId = a} :: ListBulkDeploymentDetailedReports)

instance
  Core.AWSPager
    ListBulkDeploymentDetailedReports
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBulkDeploymentDetailedReportsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listBulkDeploymentDetailedReportsResponse_deployments
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listBulkDeploymentDetailedReports_nextToken
          Lens..~ rs
          Lens.^? listBulkDeploymentDetailedReportsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListBulkDeploymentDetailedReports
  where
  type
    AWSResponse ListBulkDeploymentDetailedReports =
      ListBulkDeploymentDetailedReportsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBulkDeploymentDetailedReportsResponse'
            Prelude.<$> (x Data..?> "NextToken")
              Prelude.<*> (x Data..?> "Deployments" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListBulkDeploymentDetailedReports
  where
  hashWithSalt
    _salt
    ListBulkDeploymentDetailedReports' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` bulkDeploymentId

instance
  Prelude.NFData
    ListBulkDeploymentDetailedReports
  where
  rnf ListBulkDeploymentDetailedReports' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf bulkDeploymentId

instance
  Data.ToHeaders
    ListBulkDeploymentDetailedReports
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    ListBulkDeploymentDetailedReports
  where
  toPath ListBulkDeploymentDetailedReports' {..} =
    Prelude.mconcat
      [ "/greengrass/bulk/deployments/",
        Data.toBS bulkDeploymentId,
        "/detailed-reports"
      ]

instance
  Data.ToQuery
    ListBulkDeploymentDetailedReports
  where
  toQuery ListBulkDeploymentDetailedReports' {..} =
    Prelude.mconcat
      [ "NextToken" Data.=: nextToken,
        "MaxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListBulkDeploymentDetailedReportsResponse' smart constructor.
data ListBulkDeploymentDetailedReportsResponse = ListBulkDeploymentDetailedReportsResponse'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the individual group deployments in the bulk deployment
    -- operation.
    deployments :: Prelude.Maybe [BulkDeploymentResult],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListBulkDeploymentDetailedReportsResponse
newListBulkDeploymentDetailedReportsResponse
  pHttpStatus_ =
    ListBulkDeploymentDetailedReportsResponse'
      { nextToken =
          Prelude.Nothing,
        deployments = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listBulkDeploymentDetailedReportsResponse_nextToken :: Lens.Lens' ListBulkDeploymentDetailedReportsResponse (Prelude.Maybe Prelude.Text)
listBulkDeploymentDetailedReportsResponse_nextToken = Lens.lens (\ListBulkDeploymentDetailedReportsResponse' {nextToken} -> nextToken) (\s@ListBulkDeploymentDetailedReportsResponse' {} a -> s {nextToken = a} :: ListBulkDeploymentDetailedReportsResponse)

-- | A list of the individual group deployments in the bulk deployment
-- operation.
listBulkDeploymentDetailedReportsResponse_deployments :: Lens.Lens' ListBulkDeploymentDetailedReportsResponse (Prelude.Maybe [BulkDeploymentResult])
listBulkDeploymentDetailedReportsResponse_deployments = Lens.lens (\ListBulkDeploymentDetailedReportsResponse' {deployments} -> deployments) (\s@ListBulkDeploymentDetailedReportsResponse' {} a -> s {deployments = a} :: ListBulkDeploymentDetailedReportsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listBulkDeploymentDetailedReportsResponse_httpStatus :: Lens.Lens' ListBulkDeploymentDetailedReportsResponse Prelude.Int
listBulkDeploymentDetailedReportsResponse_httpStatus = Lens.lens (\ListBulkDeploymentDetailedReportsResponse' {httpStatus} -> httpStatus) (\s@ListBulkDeploymentDetailedReportsResponse' {} a -> s {httpStatus = a} :: ListBulkDeploymentDetailedReportsResponse)

instance
  Prelude.NFData
    ListBulkDeploymentDetailedReportsResponse
  where
  rnf ListBulkDeploymentDetailedReportsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf deployments
      `Prelude.seq` Prelude.rnf httpStatus
