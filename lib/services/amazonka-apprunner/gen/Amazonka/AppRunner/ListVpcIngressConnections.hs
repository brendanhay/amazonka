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
-- Module      : Amazonka.AppRunner.ListVpcIngressConnections
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Return a list of App Runner VPC Ingress Connections in your Amazon Web
-- Services account.
module Amazonka.AppRunner.ListVpcIngressConnections
  ( -- * Creating a Request
    ListVpcIngressConnections (..),
    newListVpcIngressConnections,

    -- * Request Lenses
    listVpcIngressConnections_nextToken,
    listVpcIngressConnections_filter,
    listVpcIngressConnections_maxResults,

    -- * Destructuring the Response
    ListVpcIngressConnectionsResponse (..),
    newListVpcIngressConnectionsResponse,

    -- * Response Lenses
    listVpcIngressConnectionsResponse_nextToken,
    listVpcIngressConnectionsResponse_httpStatus,
    listVpcIngressConnectionsResponse_vpcIngressConnectionSummaryList,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListVpcIngressConnections' smart constructor.
data ListVpcIngressConnections = ListVpcIngressConnections'
  { -- | A token from a previous result page. It\'s used for a paginated request.
    -- The request retrieves the next result page. All other parameter values
    -- must be identical to the ones that are specified in the initial request.
    --
    -- If you don\'t specify @NextToken@, the request retrieves the first
    -- result page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The VPC Ingress Connections to be listed based on either the Service Arn
    -- or Vpc Endpoint Id, or both.
    filter' :: Prelude.Maybe ListVpcIngressConnectionsFilter,
    -- | The maximum number of results to include in each response (result page).
    -- It\'s used for a paginated request.
    --
    -- If you don\'t specify @MaxResults@, the request retrieves all available
    -- results in a single response.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVpcIngressConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVpcIngressConnections_nextToken' - A token from a previous result page. It\'s used for a paginated request.
-- The request retrieves the next result page. All other parameter values
-- must be identical to the ones that are specified in the initial request.
--
-- If you don\'t specify @NextToken@, the request retrieves the first
-- result page.
--
-- 'filter'', 'listVpcIngressConnections_filter' - The VPC Ingress Connections to be listed based on either the Service Arn
-- or Vpc Endpoint Id, or both.
--
-- 'maxResults', 'listVpcIngressConnections_maxResults' - The maximum number of results to include in each response (result page).
-- It\'s used for a paginated request.
--
-- If you don\'t specify @MaxResults@, the request retrieves all available
-- results in a single response.
newListVpcIngressConnections ::
  ListVpcIngressConnections
newListVpcIngressConnections =
  ListVpcIngressConnections'
    { nextToken =
        Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token from a previous result page. It\'s used for a paginated request.
-- The request retrieves the next result page. All other parameter values
-- must be identical to the ones that are specified in the initial request.
--
-- If you don\'t specify @NextToken@, the request retrieves the first
-- result page.
listVpcIngressConnections_nextToken :: Lens.Lens' ListVpcIngressConnections (Prelude.Maybe Prelude.Text)
listVpcIngressConnections_nextToken = Lens.lens (\ListVpcIngressConnections' {nextToken} -> nextToken) (\s@ListVpcIngressConnections' {} a -> s {nextToken = a} :: ListVpcIngressConnections)

-- | The VPC Ingress Connections to be listed based on either the Service Arn
-- or Vpc Endpoint Id, or both.
listVpcIngressConnections_filter :: Lens.Lens' ListVpcIngressConnections (Prelude.Maybe ListVpcIngressConnectionsFilter)
listVpcIngressConnections_filter = Lens.lens (\ListVpcIngressConnections' {filter'} -> filter') (\s@ListVpcIngressConnections' {} a -> s {filter' = a} :: ListVpcIngressConnections)

-- | The maximum number of results to include in each response (result page).
-- It\'s used for a paginated request.
--
-- If you don\'t specify @MaxResults@, the request retrieves all available
-- results in a single response.
listVpcIngressConnections_maxResults :: Lens.Lens' ListVpcIngressConnections (Prelude.Maybe Prelude.Natural)
listVpcIngressConnections_maxResults = Lens.lens (\ListVpcIngressConnections' {maxResults} -> maxResults) (\s@ListVpcIngressConnections' {} a -> s {maxResults = a} :: ListVpcIngressConnections)

instance Core.AWSRequest ListVpcIngressConnections where
  type
    AWSResponse ListVpcIngressConnections =
      ListVpcIngressConnectionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVpcIngressConnectionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "VpcIngressConnectionSummaryList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListVpcIngressConnections where
  hashWithSalt _salt ListVpcIngressConnections' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListVpcIngressConnections where
  rnf ListVpcIngressConnections' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListVpcIngressConnections where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AppRunner.ListVpcIngressConnections" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListVpcIngressConnections where
  toJSON ListVpcIngressConnections' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListVpcIngressConnections where
  toPath = Prelude.const "/"

instance Data.ToQuery ListVpcIngressConnections where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListVpcIngressConnectionsResponse' smart constructor.
data ListVpcIngressConnectionsResponse = ListVpcIngressConnectionsResponse'
  { -- | The token that you can pass in a subsequent request to get the next
    -- result page. It\'s returned in a paginated request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of summary information records for VPC Ingress Connections. In a
    -- paginated request, the request returns up to @MaxResults@ records for
    -- each call.
    vpcIngressConnectionSummaryList :: [VpcIngressConnectionSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVpcIngressConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVpcIngressConnectionsResponse_nextToken' - The token that you can pass in a subsequent request to get the next
-- result page. It\'s returned in a paginated request.
--
-- 'httpStatus', 'listVpcIngressConnectionsResponse_httpStatus' - The response's http status code.
--
-- 'vpcIngressConnectionSummaryList', 'listVpcIngressConnectionsResponse_vpcIngressConnectionSummaryList' - A list of summary information records for VPC Ingress Connections. In a
-- paginated request, the request returns up to @MaxResults@ records for
-- each call.
newListVpcIngressConnectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVpcIngressConnectionsResponse
newListVpcIngressConnectionsResponse pHttpStatus_ =
  ListVpcIngressConnectionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      vpcIngressConnectionSummaryList =
        Prelude.mempty
    }

-- | The token that you can pass in a subsequent request to get the next
-- result page. It\'s returned in a paginated request.
listVpcIngressConnectionsResponse_nextToken :: Lens.Lens' ListVpcIngressConnectionsResponse (Prelude.Maybe Prelude.Text)
listVpcIngressConnectionsResponse_nextToken = Lens.lens (\ListVpcIngressConnectionsResponse' {nextToken} -> nextToken) (\s@ListVpcIngressConnectionsResponse' {} a -> s {nextToken = a} :: ListVpcIngressConnectionsResponse)

-- | The response's http status code.
listVpcIngressConnectionsResponse_httpStatus :: Lens.Lens' ListVpcIngressConnectionsResponse Prelude.Int
listVpcIngressConnectionsResponse_httpStatus = Lens.lens (\ListVpcIngressConnectionsResponse' {httpStatus} -> httpStatus) (\s@ListVpcIngressConnectionsResponse' {} a -> s {httpStatus = a} :: ListVpcIngressConnectionsResponse)

-- | A list of summary information records for VPC Ingress Connections. In a
-- paginated request, the request returns up to @MaxResults@ records for
-- each call.
listVpcIngressConnectionsResponse_vpcIngressConnectionSummaryList :: Lens.Lens' ListVpcIngressConnectionsResponse [VpcIngressConnectionSummary]
listVpcIngressConnectionsResponse_vpcIngressConnectionSummaryList = Lens.lens (\ListVpcIngressConnectionsResponse' {vpcIngressConnectionSummaryList} -> vpcIngressConnectionSummaryList) (\s@ListVpcIngressConnectionsResponse' {} a -> s {vpcIngressConnectionSummaryList = a} :: ListVpcIngressConnectionsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListVpcIngressConnectionsResponse
  where
  rnf ListVpcIngressConnectionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf vpcIngressConnectionSummaryList
