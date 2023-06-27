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
-- Module      : Amazonka.MediaConnect.ListGatewayInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays a list of instances associated with the AWS account. This
-- request returns a paginated result. You can use the filterArn property
-- to display only the instances associated with the selected Gateway
-- Amazon Resource Name (ARN).
--
-- This operation returns paginated results.
module Amazonka.MediaConnect.ListGatewayInstances
  ( -- * Creating a Request
    ListGatewayInstances (..),
    newListGatewayInstances,

    -- * Request Lenses
    listGatewayInstances_filterArn,
    listGatewayInstances_maxResults,
    listGatewayInstances_nextToken,

    -- * Destructuring the Response
    ListGatewayInstancesResponse (..),
    newListGatewayInstancesResponse,

    -- * Response Lenses
    listGatewayInstancesResponse_instances,
    listGatewayInstancesResponse_nextToken,
    listGatewayInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListGatewayInstances' smart constructor.
data ListGatewayInstances = ListGatewayInstances'
  { -- | Filter the list results to display only the instances associated with
    -- the selected Gateway Amazon Resource Name (ARN).
    filterArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per API request. For example,
    -- you submit a ListInstances request with MaxResults set at 5. Although 20
    -- items match your request, the service returns no more than the first 5
    -- items. (The service also returns a NextToken value that you can use to
    -- fetch the next batch of results.) The service might return fewer results
    -- than the MaxResults value. If MaxResults is not included in the request,
    -- the service defaults to pagination with a maximum of 10 results per
    -- page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that identifies which batch of results that you want to see.
    -- For example, you submit a ListInstances request with MaxResults set at
    -- 5. The service returns the first batch of results (up to 5) and a
    -- NextToken value. To see the next batch of results, you can submit the
    -- ListInstances request a second time and specify the NextToken value.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGatewayInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterArn', 'listGatewayInstances_filterArn' - Filter the list results to display only the instances associated with
-- the selected Gateway Amazon Resource Name (ARN).
--
-- 'maxResults', 'listGatewayInstances_maxResults' - The maximum number of results to return per API request. For example,
-- you submit a ListInstances request with MaxResults set at 5. Although 20
-- items match your request, the service returns no more than the first 5
-- items. (The service also returns a NextToken value that you can use to
-- fetch the next batch of results.) The service might return fewer results
-- than the MaxResults value. If MaxResults is not included in the request,
-- the service defaults to pagination with a maximum of 10 results per
-- page.
--
-- 'nextToken', 'listGatewayInstances_nextToken' - The token that identifies which batch of results that you want to see.
-- For example, you submit a ListInstances request with MaxResults set at
-- 5. The service returns the first batch of results (up to 5) and a
-- NextToken value. To see the next batch of results, you can submit the
-- ListInstances request a second time and specify the NextToken value.
newListGatewayInstances ::
  ListGatewayInstances
newListGatewayInstances =
  ListGatewayInstances'
    { filterArn = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filter the list results to display only the instances associated with
-- the selected Gateway Amazon Resource Name (ARN).
listGatewayInstances_filterArn :: Lens.Lens' ListGatewayInstances (Prelude.Maybe Prelude.Text)
listGatewayInstances_filterArn = Lens.lens (\ListGatewayInstances' {filterArn} -> filterArn) (\s@ListGatewayInstances' {} a -> s {filterArn = a} :: ListGatewayInstances)

-- | The maximum number of results to return per API request. For example,
-- you submit a ListInstances request with MaxResults set at 5. Although 20
-- items match your request, the service returns no more than the first 5
-- items. (The service also returns a NextToken value that you can use to
-- fetch the next batch of results.) The service might return fewer results
-- than the MaxResults value. If MaxResults is not included in the request,
-- the service defaults to pagination with a maximum of 10 results per
-- page.
listGatewayInstances_maxResults :: Lens.Lens' ListGatewayInstances (Prelude.Maybe Prelude.Natural)
listGatewayInstances_maxResults = Lens.lens (\ListGatewayInstances' {maxResults} -> maxResults) (\s@ListGatewayInstances' {} a -> s {maxResults = a} :: ListGatewayInstances)

-- | The token that identifies which batch of results that you want to see.
-- For example, you submit a ListInstances request with MaxResults set at
-- 5. The service returns the first batch of results (up to 5) and a
-- NextToken value. To see the next batch of results, you can submit the
-- ListInstances request a second time and specify the NextToken value.
listGatewayInstances_nextToken :: Lens.Lens' ListGatewayInstances (Prelude.Maybe Prelude.Text)
listGatewayInstances_nextToken = Lens.lens (\ListGatewayInstances' {nextToken} -> nextToken) (\s@ListGatewayInstances' {} a -> s {nextToken = a} :: ListGatewayInstances)

instance Core.AWSPager ListGatewayInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGatewayInstancesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listGatewayInstancesResponse_instances
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listGatewayInstances_nextToken
          Lens..~ rs
          Lens.^? listGatewayInstancesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListGatewayInstances where
  type
    AWSResponse ListGatewayInstances =
      ListGatewayInstancesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGatewayInstancesResponse'
            Prelude.<$> (x Data..?> "instances" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGatewayInstances where
  hashWithSalt _salt ListGatewayInstances' {..} =
    _salt
      `Prelude.hashWithSalt` filterArn
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListGatewayInstances where
  rnf ListGatewayInstances' {..} =
    Prelude.rnf filterArn
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListGatewayInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListGatewayInstances where
  toPath = Prelude.const "/v1/gateway-instances"

instance Data.ToQuery ListGatewayInstances where
  toQuery ListGatewayInstances' {..} =
    Prelude.mconcat
      [ "filterArn" Data.=: filterArn,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListGatewayInstancesResponse' smart constructor.
data ListGatewayInstancesResponse = ListGatewayInstancesResponse'
  { -- | A list of instance summaries.
    instances :: Prelude.Maybe [ListedGatewayInstance],
    -- | The token that identifies which batch of results that you want to see.
    -- For example, you submit a ListInstances request with MaxResults set at
    -- 5. The service returns the first batch of results (up to 5) and a
    -- NextToken value. To see the next batch of results, you can submit the
    -- ListInstances request a second time and specify the NextToken value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGatewayInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instances', 'listGatewayInstancesResponse_instances' - A list of instance summaries.
--
-- 'nextToken', 'listGatewayInstancesResponse_nextToken' - The token that identifies which batch of results that you want to see.
-- For example, you submit a ListInstances request with MaxResults set at
-- 5. The service returns the first batch of results (up to 5) and a
-- NextToken value. To see the next batch of results, you can submit the
-- ListInstances request a second time and specify the NextToken value.
--
-- 'httpStatus', 'listGatewayInstancesResponse_httpStatus' - The response's http status code.
newListGatewayInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGatewayInstancesResponse
newListGatewayInstancesResponse pHttpStatus_ =
  ListGatewayInstancesResponse'
    { instances =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of instance summaries.
listGatewayInstancesResponse_instances :: Lens.Lens' ListGatewayInstancesResponse (Prelude.Maybe [ListedGatewayInstance])
listGatewayInstancesResponse_instances = Lens.lens (\ListGatewayInstancesResponse' {instances} -> instances) (\s@ListGatewayInstancesResponse' {} a -> s {instances = a} :: ListGatewayInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token that identifies which batch of results that you want to see.
-- For example, you submit a ListInstances request with MaxResults set at
-- 5. The service returns the first batch of results (up to 5) and a
-- NextToken value. To see the next batch of results, you can submit the
-- ListInstances request a second time and specify the NextToken value.
listGatewayInstancesResponse_nextToken :: Lens.Lens' ListGatewayInstancesResponse (Prelude.Maybe Prelude.Text)
listGatewayInstancesResponse_nextToken = Lens.lens (\ListGatewayInstancesResponse' {nextToken} -> nextToken) (\s@ListGatewayInstancesResponse' {} a -> s {nextToken = a} :: ListGatewayInstancesResponse)

-- | The response's http status code.
listGatewayInstancesResponse_httpStatus :: Lens.Lens' ListGatewayInstancesResponse Prelude.Int
listGatewayInstancesResponse_httpStatus = Lens.lens (\ListGatewayInstancesResponse' {httpStatus} -> httpStatus) (\s@ListGatewayInstancesResponse' {} a -> s {httpStatus = a} :: ListGatewayInstancesResponse)

instance Prelude.NFData ListGatewayInstancesResponse where
  rnf ListGatewayInstancesResponse' {..} =
    Prelude.rnf instances
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
