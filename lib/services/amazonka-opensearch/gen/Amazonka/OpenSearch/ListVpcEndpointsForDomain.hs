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
-- Module      : Amazonka.OpenSearch.ListVpcEndpointsForDomain
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all Amazon OpenSearch Service-managed VPC endpoints associated
-- with a particular domain.
module Amazonka.OpenSearch.ListVpcEndpointsForDomain
  ( -- * Creating a Request
    ListVpcEndpointsForDomain (..),
    newListVpcEndpointsForDomain,

    -- * Request Lenses
    listVpcEndpointsForDomain_nextToken,
    listVpcEndpointsForDomain_domainName,

    -- * Destructuring the Response
    ListVpcEndpointsForDomainResponse (..),
    newListVpcEndpointsForDomainResponse,

    -- * Response Lenses
    listVpcEndpointsForDomainResponse_httpStatus,
    listVpcEndpointsForDomainResponse_vpcEndpointSummaryList,
    listVpcEndpointsForDomainResponse_nextToken,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListVpcEndpointsForDomain' smart constructor.
data ListVpcEndpointsForDomain = ListVpcEndpointsForDomain'
  { -- | If your initial @ListEndpointsForDomain@ operation returns a
    -- @nextToken@, you can include the returned @nextToken@ in subsequent
    -- @ListEndpointsForDomain@ operations, which returns results in the next
    -- page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain to list associated VPC endpoints for.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVpcEndpointsForDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVpcEndpointsForDomain_nextToken' - If your initial @ListEndpointsForDomain@ operation returns a
-- @nextToken@, you can include the returned @nextToken@ in subsequent
-- @ListEndpointsForDomain@ operations, which returns results in the next
-- page.
--
-- 'domainName', 'listVpcEndpointsForDomain_domainName' - The name of the domain to list associated VPC endpoints for.
newListVpcEndpointsForDomain ::
  -- | 'domainName'
  Prelude.Text ->
  ListVpcEndpointsForDomain
newListVpcEndpointsForDomain pDomainName_ =
  ListVpcEndpointsForDomain'
    { nextToken =
        Prelude.Nothing,
      domainName = pDomainName_
    }

-- | If your initial @ListEndpointsForDomain@ operation returns a
-- @nextToken@, you can include the returned @nextToken@ in subsequent
-- @ListEndpointsForDomain@ operations, which returns results in the next
-- page.
listVpcEndpointsForDomain_nextToken :: Lens.Lens' ListVpcEndpointsForDomain (Prelude.Maybe Prelude.Text)
listVpcEndpointsForDomain_nextToken = Lens.lens (\ListVpcEndpointsForDomain' {nextToken} -> nextToken) (\s@ListVpcEndpointsForDomain' {} a -> s {nextToken = a} :: ListVpcEndpointsForDomain)

-- | The name of the domain to list associated VPC endpoints for.
listVpcEndpointsForDomain_domainName :: Lens.Lens' ListVpcEndpointsForDomain Prelude.Text
listVpcEndpointsForDomain_domainName = Lens.lens (\ListVpcEndpointsForDomain' {domainName} -> domainName) (\s@ListVpcEndpointsForDomain' {} a -> s {domainName = a} :: ListVpcEndpointsForDomain)

instance Core.AWSRequest ListVpcEndpointsForDomain where
  type
    AWSResponse ListVpcEndpointsForDomain =
      ListVpcEndpointsForDomainResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVpcEndpointsForDomainResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "VpcEndpointSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..:> "NextToken")
      )

instance Prelude.Hashable ListVpcEndpointsForDomain where
  hashWithSalt _salt ListVpcEndpointsForDomain' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData ListVpcEndpointsForDomain where
  rnf ListVpcEndpointsForDomain' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf domainName

instance Core.ToHeaders ListVpcEndpointsForDomain where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListVpcEndpointsForDomain where
  toPath ListVpcEndpointsForDomain' {..} =
    Prelude.mconcat
      [ "/2021-01-01/opensearch/domain/",
        Core.toBS domainName,
        "/vpcEndpoints"
      ]

instance Core.ToQuery ListVpcEndpointsForDomain where
  toQuery ListVpcEndpointsForDomain' {..} =
    Prelude.mconcat ["nextToken" Core.=: nextToken]

-- | /See:/ 'newListVpcEndpointsForDomainResponse' smart constructor.
data ListVpcEndpointsForDomainResponse = ListVpcEndpointsForDomainResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about each endpoint associated with the domain.
    vpcEndpointSummaryList :: [VpcEndpointSummary],
    -- | When @nextToken@ is returned, there are more results available. The
    -- value of @nextToken@ is a unique pagination token for each page. Make
    -- the call again using the returned token to retrieve the next page.
    nextToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVpcEndpointsForDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'listVpcEndpointsForDomainResponse_httpStatus' - The response's http status code.
--
-- 'vpcEndpointSummaryList', 'listVpcEndpointsForDomainResponse_vpcEndpointSummaryList' - Information about each endpoint associated with the domain.
--
-- 'nextToken', 'listVpcEndpointsForDomainResponse_nextToken' - When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
newListVpcEndpointsForDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'nextToken'
  Prelude.Text ->
  ListVpcEndpointsForDomainResponse
newListVpcEndpointsForDomainResponse
  pHttpStatus_
  pNextToken_ =
    ListVpcEndpointsForDomainResponse'
      { httpStatus =
          pHttpStatus_,
        vpcEndpointSummaryList = Prelude.mempty,
        nextToken = pNextToken_
      }

-- | The response's http status code.
listVpcEndpointsForDomainResponse_httpStatus :: Lens.Lens' ListVpcEndpointsForDomainResponse Prelude.Int
listVpcEndpointsForDomainResponse_httpStatus = Lens.lens (\ListVpcEndpointsForDomainResponse' {httpStatus} -> httpStatus) (\s@ListVpcEndpointsForDomainResponse' {} a -> s {httpStatus = a} :: ListVpcEndpointsForDomainResponse)

-- | Information about each endpoint associated with the domain.
listVpcEndpointsForDomainResponse_vpcEndpointSummaryList :: Lens.Lens' ListVpcEndpointsForDomainResponse [VpcEndpointSummary]
listVpcEndpointsForDomainResponse_vpcEndpointSummaryList = Lens.lens (\ListVpcEndpointsForDomainResponse' {vpcEndpointSummaryList} -> vpcEndpointSummaryList) (\s@ListVpcEndpointsForDomainResponse' {} a -> s {vpcEndpointSummaryList = a} :: ListVpcEndpointsForDomainResponse) Prelude.. Lens.coerced

-- | When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
listVpcEndpointsForDomainResponse_nextToken :: Lens.Lens' ListVpcEndpointsForDomainResponse Prelude.Text
listVpcEndpointsForDomainResponse_nextToken = Lens.lens (\ListVpcEndpointsForDomainResponse' {nextToken} -> nextToken) (\s@ListVpcEndpointsForDomainResponse' {} a -> s {nextToken = a} :: ListVpcEndpointsForDomainResponse)

instance
  Prelude.NFData
    ListVpcEndpointsForDomainResponse
  where
  rnf ListVpcEndpointsForDomainResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf vpcEndpointSummaryList
      `Prelude.seq` Prelude.rnf nextToken
