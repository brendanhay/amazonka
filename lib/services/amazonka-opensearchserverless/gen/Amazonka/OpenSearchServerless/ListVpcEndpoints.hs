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
-- Module      : Amazonka.OpenSearchServerless.ListVpcEndpoints
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the OpenSearch Serverless-managed interface VPC endpoints
-- associated with the current account. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-vpc.html Access Amazon OpenSearch Serverless using an interface endpoint>.
module Amazonka.OpenSearchServerless.ListVpcEndpoints
  ( -- * Creating a Request
    ListVpcEndpoints (..),
    newListVpcEndpoints,

    -- * Request Lenses
    listVpcEndpoints_maxResults,
    listVpcEndpoints_nextToken,
    listVpcEndpoints_vpcEndpointFilters,

    -- * Destructuring the Response
    ListVpcEndpointsResponse (..),
    newListVpcEndpointsResponse,

    -- * Response Lenses
    listVpcEndpointsResponse_nextToken,
    listVpcEndpointsResponse_vpcEndpointSummaries,
    listVpcEndpointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListVpcEndpoints' smart constructor.
data ListVpcEndpoints = ListVpcEndpoints'
  { -- | An optional parameter that specifies the maximum number of results to
    -- return. You can use @nextToken@ to get the next page of results. The
    -- default is 20.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If your initial @ListVpcEndpoints@ operation returns a @nextToken@, you
    -- can include the returned @nextToken@ in subsequent @ListVpcEndpoints@
    -- operations, which returns results in the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filter the results according to the current status of the VPC endpoint.
    -- Possible statuses are @CREATING@, @DELETING@, @UPDATING@, @ACTIVE@, and
    -- @FAILED@.
    vpcEndpointFilters :: Prelude.Maybe VpcEndpointFilters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVpcEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listVpcEndpoints_maxResults' - An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results. The
-- default is 20.
--
-- 'nextToken', 'listVpcEndpoints_nextToken' - If your initial @ListVpcEndpoints@ operation returns a @nextToken@, you
-- can include the returned @nextToken@ in subsequent @ListVpcEndpoints@
-- operations, which returns results in the next page.
--
-- 'vpcEndpointFilters', 'listVpcEndpoints_vpcEndpointFilters' - Filter the results according to the current status of the VPC endpoint.
-- Possible statuses are @CREATING@, @DELETING@, @UPDATING@, @ACTIVE@, and
-- @FAILED@.
newListVpcEndpoints ::
  ListVpcEndpoints
newListVpcEndpoints =
  ListVpcEndpoints'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      vpcEndpointFilters = Prelude.Nothing
    }

-- | An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results. The
-- default is 20.
listVpcEndpoints_maxResults :: Lens.Lens' ListVpcEndpoints (Prelude.Maybe Prelude.Natural)
listVpcEndpoints_maxResults = Lens.lens (\ListVpcEndpoints' {maxResults} -> maxResults) (\s@ListVpcEndpoints' {} a -> s {maxResults = a} :: ListVpcEndpoints)

-- | If your initial @ListVpcEndpoints@ operation returns a @nextToken@, you
-- can include the returned @nextToken@ in subsequent @ListVpcEndpoints@
-- operations, which returns results in the next page.
listVpcEndpoints_nextToken :: Lens.Lens' ListVpcEndpoints (Prelude.Maybe Prelude.Text)
listVpcEndpoints_nextToken = Lens.lens (\ListVpcEndpoints' {nextToken} -> nextToken) (\s@ListVpcEndpoints' {} a -> s {nextToken = a} :: ListVpcEndpoints)

-- | Filter the results according to the current status of the VPC endpoint.
-- Possible statuses are @CREATING@, @DELETING@, @UPDATING@, @ACTIVE@, and
-- @FAILED@.
listVpcEndpoints_vpcEndpointFilters :: Lens.Lens' ListVpcEndpoints (Prelude.Maybe VpcEndpointFilters)
listVpcEndpoints_vpcEndpointFilters = Lens.lens (\ListVpcEndpoints' {vpcEndpointFilters} -> vpcEndpointFilters) (\s@ListVpcEndpoints' {} a -> s {vpcEndpointFilters = a} :: ListVpcEndpoints)

instance Core.AWSRequest ListVpcEndpoints where
  type
    AWSResponse ListVpcEndpoints =
      ListVpcEndpointsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVpcEndpointsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "vpcEndpointSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVpcEndpoints where
  hashWithSalt _salt ListVpcEndpoints' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` vpcEndpointFilters

instance Prelude.NFData ListVpcEndpoints where
  rnf ListVpcEndpoints' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf vpcEndpointFilters

instance Data.ToHeaders ListVpcEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.ListVpcEndpoints" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListVpcEndpoints where
  toJSON ListVpcEndpoints' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("vpcEndpointFilters" Data..=)
              Prelude.<$> vpcEndpointFilters
          ]
      )

instance Data.ToPath ListVpcEndpoints where
  toPath = Prelude.const "/"

instance Data.ToQuery ListVpcEndpoints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListVpcEndpointsResponse' smart constructor.
data ListVpcEndpointsResponse = ListVpcEndpointsResponse'
  { -- | When @nextToken@ is returned, there are more results available. The
    -- value of @nextToken@ is a unique pagination token for each page. Make
    -- the call again using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Details about each VPC endpoint, including the name and current status.
    vpcEndpointSummaries :: Prelude.Maybe [VpcEndpointSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVpcEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVpcEndpointsResponse_nextToken' - When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
--
-- 'vpcEndpointSummaries', 'listVpcEndpointsResponse_vpcEndpointSummaries' - Details about each VPC endpoint, including the name and current status.
--
-- 'httpStatus', 'listVpcEndpointsResponse_httpStatus' - The response's http status code.
newListVpcEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVpcEndpointsResponse
newListVpcEndpointsResponse pHttpStatus_ =
  ListVpcEndpointsResponse'
    { nextToken =
        Prelude.Nothing,
      vpcEndpointSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
listVpcEndpointsResponse_nextToken :: Lens.Lens' ListVpcEndpointsResponse (Prelude.Maybe Prelude.Text)
listVpcEndpointsResponse_nextToken = Lens.lens (\ListVpcEndpointsResponse' {nextToken} -> nextToken) (\s@ListVpcEndpointsResponse' {} a -> s {nextToken = a} :: ListVpcEndpointsResponse)

-- | Details about each VPC endpoint, including the name and current status.
listVpcEndpointsResponse_vpcEndpointSummaries :: Lens.Lens' ListVpcEndpointsResponse (Prelude.Maybe [VpcEndpointSummary])
listVpcEndpointsResponse_vpcEndpointSummaries = Lens.lens (\ListVpcEndpointsResponse' {vpcEndpointSummaries} -> vpcEndpointSummaries) (\s@ListVpcEndpointsResponse' {} a -> s {vpcEndpointSummaries = a} :: ListVpcEndpointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listVpcEndpointsResponse_httpStatus :: Lens.Lens' ListVpcEndpointsResponse Prelude.Int
listVpcEndpointsResponse_httpStatus = Lens.lens (\ListVpcEndpointsResponse' {httpStatus} -> httpStatus) (\s@ListVpcEndpointsResponse' {} a -> s {httpStatus = a} :: ListVpcEndpointsResponse)

instance Prelude.NFData ListVpcEndpointsResponse where
  rnf ListVpcEndpointsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf vpcEndpointSummaries
      `Prelude.seq` Prelude.rnf httpStatus
