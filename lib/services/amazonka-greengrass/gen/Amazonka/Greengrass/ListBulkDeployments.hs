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
-- Module      : Amazonka.Greengrass.ListBulkDeployments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of bulk deployments.
--
-- This operation returns paginated results.
module Amazonka.Greengrass.ListBulkDeployments
  ( -- * Creating a Request
    ListBulkDeployments (..),
    newListBulkDeployments,

    -- * Request Lenses
    listBulkDeployments_maxResults,
    listBulkDeployments_nextToken,

    -- * Destructuring the Response
    ListBulkDeploymentsResponse (..),
    newListBulkDeploymentsResponse,

    -- * Response Lenses
    listBulkDeploymentsResponse_bulkDeployments,
    listBulkDeploymentsResponse_nextToken,
    listBulkDeploymentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBulkDeployments' smart constructor.
data ListBulkDeployments = ListBulkDeployments'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBulkDeployments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listBulkDeployments_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listBulkDeployments_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
newListBulkDeployments ::
  ListBulkDeployments
newListBulkDeployments =
  ListBulkDeployments'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to be returned per request.
listBulkDeployments_maxResults :: Lens.Lens' ListBulkDeployments (Prelude.Maybe Prelude.Text)
listBulkDeployments_maxResults = Lens.lens (\ListBulkDeployments' {maxResults} -> maxResults) (\s@ListBulkDeployments' {} a -> s {maxResults = a} :: ListBulkDeployments)

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listBulkDeployments_nextToken :: Lens.Lens' ListBulkDeployments (Prelude.Maybe Prelude.Text)
listBulkDeployments_nextToken = Lens.lens (\ListBulkDeployments' {nextToken} -> nextToken) (\s@ListBulkDeployments' {} a -> s {nextToken = a} :: ListBulkDeployments)

instance Core.AWSPager ListBulkDeployments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBulkDeploymentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listBulkDeploymentsResponse_bulkDeployments
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listBulkDeployments_nextToken
          Lens..~ rs
          Lens.^? listBulkDeploymentsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListBulkDeployments where
  type
    AWSResponse ListBulkDeployments =
      ListBulkDeploymentsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBulkDeploymentsResponse'
            Prelude.<$> ( x Data..?> "BulkDeployments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBulkDeployments where
  hashWithSalt _salt ListBulkDeployments' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListBulkDeployments where
  rnf ListBulkDeployments' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListBulkDeployments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListBulkDeployments where
  toPath = Prelude.const "/greengrass/bulk/deployments"

instance Data.ToQuery ListBulkDeployments where
  toQuery ListBulkDeployments' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListBulkDeploymentsResponse' smart constructor.
data ListBulkDeploymentsResponse = ListBulkDeploymentsResponse'
  { -- | A list of bulk deployments.
    bulkDeployments :: Prelude.Maybe [BulkDeployment],
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBulkDeploymentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bulkDeployments', 'listBulkDeploymentsResponse_bulkDeployments' - A list of bulk deployments.
--
-- 'nextToken', 'listBulkDeploymentsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'httpStatus', 'listBulkDeploymentsResponse_httpStatus' - The response's http status code.
newListBulkDeploymentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBulkDeploymentsResponse
newListBulkDeploymentsResponse pHttpStatus_ =
  ListBulkDeploymentsResponse'
    { bulkDeployments =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of bulk deployments.
listBulkDeploymentsResponse_bulkDeployments :: Lens.Lens' ListBulkDeploymentsResponse (Prelude.Maybe [BulkDeployment])
listBulkDeploymentsResponse_bulkDeployments = Lens.lens (\ListBulkDeploymentsResponse' {bulkDeployments} -> bulkDeployments) (\s@ListBulkDeploymentsResponse' {} a -> s {bulkDeployments = a} :: ListBulkDeploymentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listBulkDeploymentsResponse_nextToken :: Lens.Lens' ListBulkDeploymentsResponse (Prelude.Maybe Prelude.Text)
listBulkDeploymentsResponse_nextToken = Lens.lens (\ListBulkDeploymentsResponse' {nextToken} -> nextToken) (\s@ListBulkDeploymentsResponse' {} a -> s {nextToken = a} :: ListBulkDeploymentsResponse)

-- | The response's http status code.
listBulkDeploymentsResponse_httpStatus :: Lens.Lens' ListBulkDeploymentsResponse Prelude.Int
listBulkDeploymentsResponse_httpStatus = Lens.lens (\ListBulkDeploymentsResponse' {httpStatus} -> httpStatus) (\s@ListBulkDeploymentsResponse' {} a -> s {httpStatus = a} :: ListBulkDeploymentsResponse)

instance Prelude.NFData ListBulkDeploymentsResponse where
  rnf ListBulkDeploymentsResponse' {..} =
    Prelude.rnf bulkDeployments
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
