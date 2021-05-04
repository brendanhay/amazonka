{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListBulkDeployments' smart constructor.
data ListBulkDeployments = ListBulkDeployments'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listBulkDeployments_nextToken :: Lens.Lens' ListBulkDeployments (Prelude.Maybe Prelude.Text)
listBulkDeployments_nextToken = Lens.lens (\ListBulkDeployments' {nextToken} -> nextToken) (\s@ListBulkDeployments' {} a -> s {nextToken = a} :: ListBulkDeployments)

-- | The maximum number of results to be returned per request.
listBulkDeployments_maxResults :: Lens.Lens' ListBulkDeployments (Prelude.Maybe Prelude.Text)
listBulkDeployments_maxResults = Lens.lens (\ListBulkDeployments' {maxResults} -> maxResults) (\s@ListBulkDeployments' {} a -> s {maxResults = a} :: ListBulkDeployments)

instance Pager.AWSPager ListBulkDeployments where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listBulkDeploymentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listBulkDeploymentsResponse_bulkDeployments
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listBulkDeployments_nextToken
          Lens..~ rs
          Lens.^? listBulkDeploymentsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListBulkDeployments where
  type
    Rs ListBulkDeployments =
      ListBulkDeploymentsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBulkDeploymentsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "BulkDeployments"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBulkDeployments

instance Prelude.NFData ListBulkDeployments

instance Prelude.ToHeaders ListBulkDeployments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath ListBulkDeployments where
  toPath = Prelude.const "/greengrass/bulk/deployments"

instance Prelude.ToQuery ListBulkDeployments where
  toQuery ListBulkDeployments' {..} =
    Prelude.mconcat
      [ "NextToken" Prelude.=: nextToken,
        "MaxResults" Prelude.=: maxResults
      ]

-- | /See:/ 'newListBulkDeploymentsResponse' smart constructor.
data ListBulkDeploymentsResponse = ListBulkDeploymentsResponse'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of bulk deployments.
    bulkDeployments :: Prelude.Maybe [BulkDeployment],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListBulkDeploymentsResponse
newListBulkDeploymentsResponse pHttpStatus_ =
  ListBulkDeploymentsResponse'
    { nextToken =
        Prelude.Nothing,
      bulkDeployments = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listBulkDeploymentsResponse_nextToken :: Lens.Lens' ListBulkDeploymentsResponse (Prelude.Maybe Prelude.Text)
listBulkDeploymentsResponse_nextToken = Lens.lens (\ListBulkDeploymentsResponse' {nextToken} -> nextToken) (\s@ListBulkDeploymentsResponse' {} a -> s {nextToken = a} :: ListBulkDeploymentsResponse)

-- | A list of bulk deployments.
listBulkDeploymentsResponse_bulkDeployments :: Lens.Lens' ListBulkDeploymentsResponse (Prelude.Maybe [BulkDeployment])
listBulkDeploymentsResponse_bulkDeployments = Lens.lens (\ListBulkDeploymentsResponse' {bulkDeployments} -> bulkDeployments) (\s@ListBulkDeploymentsResponse' {} a -> s {bulkDeployments = a} :: ListBulkDeploymentsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listBulkDeploymentsResponse_httpStatus :: Lens.Lens' ListBulkDeploymentsResponse Prelude.Int
listBulkDeploymentsResponse_httpStatus = Lens.lens (\ListBulkDeploymentsResponse' {httpStatus} -> httpStatus) (\s@ListBulkDeploymentsResponse' {} a -> s {httpStatus = a} :: ListBulkDeploymentsResponse)

instance Prelude.NFData ListBulkDeploymentsResponse
