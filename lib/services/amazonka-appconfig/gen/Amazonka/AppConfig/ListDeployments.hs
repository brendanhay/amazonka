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
-- Module      : Amazonka.AppConfig.ListDeployments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the deployments for an environment in descending deployment number
-- order.
module Amazonka.AppConfig.ListDeployments
  ( -- * Creating a Request
    ListDeployments (..),
    newListDeployments,

    -- * Request Lenses
    listDeployments_maxResults,
    listDeployments_nextToken,
    listDeployments_applicationId,
    listDeployments_environmentId,

    -- * Destructuring the Response
    ListDeploymentsResponse (..),
    newListDeploymentsResponse,

    -- * Response Lenses
    listDeploymentsResponse_items,
    listDeploymentsResponse_nextToken,
    listDeploymentsResponse_httpStatus,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDeployments' smart constructor.
data ListDeployments = ListDeployments'
  { -- | The maximum number of items that may be returned for this call. If there
    -- are items that have not yet been returned, the response will include a
    -- non-null @NextToken@ that you can provide in a subsequent call to get
    -- the next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token returned by a prior call to this operation indicating the next
    -- set of results to be returned. If not specified, the operation will
    -- return the first set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The application ID.
    applicationId :: Prelude.Text,
    -- | The environment ID.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeployments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDeployments_maxResults' - The maximum number of items that may be returned for this call. If there
-- are items that have not yet been returned, the response will include a
-- non-null @NextToken@ that you can provide in a subsequent call to get
-- the next set of results.
--
-- 'nextToken', 'listDeployments_nextToken' - The token returned by a prior call to this operation indicating the next
-- set of results to be returned. If not specified, the operation will
-- return the first set of results.
--
-- 'applicationId', 'listDeployments_applicationId' - The application ID.
--
-- 'environmentId', 'listDeployments_environmentId' - The environment ID.
newListDeployments ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'environmentId'
  Prelude.Text ->
  ListDeployments
newListDeployments pApplicationId_ pEnvironmentId_ =
  ListDeployments'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      applicationId = pApplicationId_,
      environmentId = pEnvironmentId_
    }

-- | The maximum number of items that may be returned for this call. If there
-- are items that have not yet been returned, the response will include a
-- non-null @NextToken@ that you can provide in a subsequent call to get
-- the next set of results.
listDeployments_maxResults :: Lens.Lens' ListDeployments (Prelude.Maybe Prelude.Natural)
listDeployments_maxResults = Lens.lens (\ListDeployments' {maxResults} -> maxResults) (\s@ListDeployments' {} a -> s {maxResults = a} :: ListDeployments)

-- | The token returned by a prior call to this operation indicating the next
-- set of results to be returned. If not specified, the operation will
-- return the first set of results.
listDeployments_nextToken :: Lens.Lens' ListDeployments (Prelude.Maybe Prelude.Text)
listDeployments_nextToken = Lens.lens (\ListDeployments' {nextToken} -> nextToken) (\s@ListDeployments' {} a -> s {nextToken = a} :: ListDeployments)

-- | The application ID.
listDeployments_applicationId :: Lens.Lens' ListDeployments Prelude.Text
listDeployments_applicationId = Lens.lens (\ListDeployments' {applicationId} -> applicationId) (\s@ListDeployments' {} a -> s {applicationId = a} :: ListDeployments)

-- | The environment ID.
listDeployments_environmentId :: Lens.Lens' ListDeployments Prelude.Text
listDeployments_environmentId = Lens.lens (\ListDeployments' {environmentId} -> environmentId) (\s@ListDeployments' {} a -> s {environmentId = a} :: ListDeployments)

instance Core.AWSRequest ListDeployments where
  type
    AWSResponse ListDeployments =
      ListDeploymentsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeploymentsResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDeployments where
  hashWithSalt _salt ListDeployments' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` environmentId

instance Prelude.NFData ListDeployments where
  rnf ListDeployments' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf environmentId

instance Data.ToHeaders ListDeployments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDeployments where
  toPath ListDeployments' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/environments/",
        Data.toBS environmentId,
        "/deployments"
      ]

instance Data.ToQuery ListDeployments where
  toQuery ListDeployments' {..} =
    Prelude.mconcat
      [ "max_results" Data.=: maxResults,
        "next_token" Data.=: nextToken
      ]

-- | /See:/ 'newListDeploymentsResponse' smart constructor.
data ListDeploymentsResponse = ListDeploymentsResponse'
  { -- | The elements from this collection.
    items :: Prelude.Maybe [DeploymentSummary],
    -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeploymentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listDeploymentsResponse_items' - The elements from this collection.
--
-- 'nextToken', 'listDeploymentsResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'httpStatus', 'listDeploymentsResponse_httpStatus' - The response's http status code.
newListDeploymentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDeploymentsResponse
newListDeploymentsResponse pHttpStatus_ =
  ListDeploymentsResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The elements from this collection.
listDeploymentsResponse_items :: Lens.Lens' ListDeploymentsResponse (Prelude.Maybe [DeploymentSummary])
listDeploymentsResponse_items = Lens.lens (\ListDeploymentsResponse' {items} -> items) (\s@ListDeploymentsResponse' {} a -> s {items = a} :: ListDeploymentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
listDeploymentsResponse_nextToken :: Lens.Lens' ListDeploymentsResponse (Prelude.Maybe Prelude.Text)
listDeploymentsResponse_nextToken = Lens.lens (\ListDeploymentsResponse' {nextToken} -> nextToken) (\s@ListDeploymentsResponse' {} a -> s {nextToken = a} :: ListDeploymentsResponse)

-- | The response's http status code.
listDeploymentsResponse_httpStatus :: Lens.Lens' ListDeploymentsResponse Prelude.Int
listDeploymentsResponse_httpStatus = Lens.lens (\ListDeploymentsResponse' {httpStatus} -> httpStatus) (\s@ListDeploymentsResponse' {} a -> s {httpStatus = a} :: ListDeploymentsResponse)

instance Prelude.NFData ListDeploymentsResponse where
  rnf ListDeploymentsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
