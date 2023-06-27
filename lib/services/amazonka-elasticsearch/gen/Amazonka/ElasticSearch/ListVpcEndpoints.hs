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
-- Module      : Amazonka.ElasticSearch.ListVpcEndpoints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all Amazon OpenSearch Service-managed VPC endpoints in the
-- current account and Region.
module Amazonka.ElasticSearch.ListVpcEndpoints
  ( -- * Creating a Request
    ListVpcEndpoints (..),
    newListVpcEndpoints,

    -- * Request Lenses
    listVpcEndpoints_nextToken,

    -- * Destructuring the Response
    ListVpcEndpointsResponse (..),
    newListVpcEndpointsResponse,

    -- * Response Lenses
    listVpcEndpointsResponse_httpStatus,
    listVpcEndpointsResponse_vpcEndpointSummaryList,
    listVpcEndpointsResponse_nextToken,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for request parameters to the @ListVpcEndpoints@ operation.
--
-- /See:/ 'newListVpcEndpoints' smart constructor.
data ListVpcEndpoints = ListVpcEndpoints'
  { -- | Identifier to allow retrieval of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'nextToken', 'listVpcEndpoints_nextToken' - Identifier to allow retrieval of paginated results.
newListVpcEndpoints ::
  ListVpcEndpoints
newListVpcEndpoints =
  ListVpcEndpoints' {nextToken = Prelude.Nothing}

-- | Identifier to allow retrieval of paginated results.
listVpcEndpoints_nextToken :: Lens.Lens' ListVpcEndpoints (Prelude.Maybe Prelude.Text)
listVpcEndpoints_nextToken = Lens.lens (\ListVpcEndpoints' {nextToken} -> nextToken) (\s@ListVpcEndpoints' {} a -> s {nextToken = a} :: ListVpcEndpoints)

instance Core.AWSRequest ListVpcEndpoints where
  type
    AWSResponse ListVpcEndpoints =
      ListVpcEndpointsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVpcEndpointsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "VpcEndpointSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..:> "NextToken")
      )

instance Prelude.Hashable ListVpcEndpoints where
  hashWithSalt _salt ListVpcEndpoints' {..} =
    _salt `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListVpcEndpoints where
  rnf ListVpcEndpoints' {..} = Prelude.rnf nextToken

instance Data.ToHeaders ListVpcEndpoints where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListVpcEndpoints where
  toPath = Prelude.const "/2015-01-01/es/vpcEndpoints"

instance Data.ToQuery ListVpcEndpoints where
  toQuery ListVpcEndpoints' {..} =
    Prelude.mconcat ["nextToken" Data.=: nextToken]

-- | Container for response parameters to the @ListVpcEndpoints@ operation.
-- Returns a list containing summarized details of the VPC endpoints.
--
-- /See:/ 'newListVpcEndpointsResponse' smart constructor.
data ListVpcEndpointsResponse = ListVpcEndpointsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about each endpoint.
    vpcEndpointSummaryList :: [VpcEndpointSummary],
    -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Prelude.Text
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
-- 'httpStatus', 'listVpcEndpointsResponse_httpStatus' - The response's http status code.
--
-- 'vpcEndpointSummaryList', 'listVpcEndpointsResponse_vpcEndpointSummaryList' - Information about each endpoint.
--
-- 'nextToken', 'listVpcEndpointsResponse_nextToken' - Provides an identifier to allow retrieval of paginated results.
newListVpcEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'nextToken'
  Prelude.Text ->
  ListVpcEndpointsResponse
newListVpcEndpointsResponse pHttpStatus_ pNextToken_ =
  ListVpcEndpointsResponse'
    { httpStatus =
        pHttpStatus_,
      vpcEndpointSummaryList = Prelude.mempty,
      nextToken = pNextToken_
    }

-- | The response's http status code.
listVpcEndpointsResponse_httpStatus :: Lens.Lens' ListVpcEndpointsResponse Prelude.Int
listVpcEndpointsResponse_httpStatus = Lens.lens (\ListVpcEndpointsResponse' {httpStatus} -> httpStatus) (\s@ListVpcEndpointsResponse' {} a -> s {httpStatus = a} :: ListVpcEndpointsResponse)

-- | Information about each endpoint.
listVpcEndpointsResponse_vpcEndpointSummaryList :: Lens.Lens' ListVpcEndpointsResponse [VpcEndpointSummary]
listVpcEndpointsResponse_vpcEndpointSummaryList = Lens.lens (\ListVpcEndpointsResponse' {vpcEndpointSummaryList} -> vpcEndpointSummaryList) (\s@ListVpcEndpointsResponse' {} a -> s {vpcEndpointSummaryList = a} :: ListVpcEndpointsResponse) Prelude.. Lens.coerced

-- | Provides an identifier to allow retrieval of paginated results.
listVpcEndpointsResponse_nextToken :: Lens.Lens' ListVpcEndpointsResponse Prelude.Text
listVpcEndpointsResponse_nextToken = Lens.lens (\ListVpcEndpointsResponse' {nextToken} -> nextToken) (\s@ListVpcEndpointsResponse' {} a -> s {nextToken = a} :: ListVpcEndpointsResponse)

instance Prelude.NFData ListVpcEndpointsResponse where
  rnf ListVpcEndpointsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf vpcEndpointSummaryList
      `Prelude.seq` Prelude.rnf nextToken
