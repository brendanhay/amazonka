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
-- Module      : Amazonka.Greengrass.ListDeployments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a history of deployments for the group.
--
-- This operation returns paginated results.
module Amazonka.Greengrass.ListDeployments
  ( -- * Creating a Request
    ListDeployments (..),
    newListDeployments,

    -- * Request Lenses
    listDeployments_maxResults,
    listDeployments_nextToken,
    listDeployments_groupId,

    -- * Destructuring the Response
    ListDeploymentsResponse (..),
    newListDeploymentsResponse,

    -- * Response Lenses
    listDeploymentsResponse_deployments,
    listDeploymentsResponse_nextToken,
    listDeploymentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDeployments' smart constructor.
data ListDeployments = ListDeployments'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Greengrass group.
    groupId :: Prelude.Text
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
-- 'maxResults', 'listDeployments_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listDeployments_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'groupId', 'listDeployments_groupId' - The ID of the Greengrass group.
newListDeployments ::
  -- | 'groupId'
  Prelude.Text ->
  ListDeployments
newListDeployments pGroupId_ =
  ListDeployments'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      groupId = pGroupId_
    }

-- | The maximum number of results to be returned per request.
listDeployments_maxResults :: Lens.Lens' ListDeployments (Prelude.Maybe Prelude.Text)
listDeployments_maxResults = Lens.lens (\ListDeployments' {maxResults} -> maxResults) (\s@ListDeployments' {} a -> s {maxResults = a} :: ListDeployments)

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listDeployments_nextToken :: Lens.Lens' ListDeployments (Prelude.Maybe Prelude.Text)
listDeployments_nextToken = Lens.lens (\ListDeployments' {nextToken} -> nextToken) (\s@ListDeployments' {} a -> s {nextToken = a} :: ListDeployments)

-- | The ID of the Greengrass group.
listDeployments_groupId :: Lens.Lens' ListDeployments Prelude.Text
listDeployments_groupId = Lens.lens (\ListDeployments' {groupId} -> groupId) (\s@ListDeployments' {} a -> s {groupId = a} :: ListDeployments)

instance Core.AWSPager ListDeployments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeploymentsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDeploymentsResponse_deployments
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDeployments_nextToken
          Lens..~ rs
          Lens.^? listDeploymentsResponse_nextToken
          Prelude.. Lens._Just

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
            Prelude.<$> (x Data..?> "Deployments" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDeployments where
  hashWithSalt _salt ListDeployments' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` groupId

instance Prelude.NFData ListDeployments where
  rnf ListDeployments' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf groupId

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
      [ "/greengrass/groups/",
        Data.toBS groupId,
        "/deployments"
      ]

instance Data.ToQuery ListDeployments where
  toQuery ListDeployments' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListDeploymentsResponse' smart constructor.
data ListDeploymentsResponse = ListDeploymentsResponse'
  { -- | A list of deployments for the requested groups.
    deployments :: Prelude.Maybe [Deployment],
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
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
-- 'deployments', 'listDeploymentsResponse_deployments' - A list of deployments for the requested groups.
--
-- 'nextToken', 'listDeploymentsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'httpStatus', 'listDeploymentsResponse_httpStatus' - The response's http status code.
newListDeploymentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDeploymentsResponse
newListDeploymentsResponse pHttpStatus_ =
  ListDeploymentsResponse'
    { deployments =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of deployments for the requested groups.
listDeploymentsResponse_deployments :: Lens.Lens' ListDeploymentsResponse (Prelude.Maybe [Deployment])
listDeploymentsResponse_deployments = Lens.lens (\ListDeploymentsResponse' {deployments} -> deployments) (\s@ListDeploymentsResponse' {} a -> s {deployments = a} :: ListDeploymentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listDeploymentsResponse_nextToken :: Lens.Lens' ListDeploymentsResponse (Prelude.Maybe Prelude.Text)
listDeploymentsResponse_nextToken = Lens.lens (\ListDeploymentsResponse' {nextToken} -> nextToken) (\s@ListDeploymentsResponse' {} a -> s {nextToken = a} :: ListDeploymentsResponse)

-- | The response's http status code.
listDeploymentsResponse_httpStatus :: Lens.Lens' ListDeploymentsResponse Prelude.Int
listDeploymentsResponse_httpStatus = Lens.lens (\ListDeploymentsResponse' {httpStatus} -> httpStatus) (\s@ListDeploymentsResponse' {} a -> s {httpStatus = a} :: ListDeploymentsResponse)

instance Prelude.NFData ListDeploymentsResponse where
  rnf ListDeploymentsResponse' {..} =
    Prelude.rnf deployments
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
