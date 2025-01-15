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
-- Module      : Amazonka.ElasticSearch.ListVpcEndpointsForDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all Amazon OpenSearch Service-managed VPC endpoints associated
-- with a particular domain.
module Amazonka.ElasticSearch.ListVpcEndpointsForDomain
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
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for request parameters to the @ListVpcEndpointsForDomain@
-- operation. Specifies the domain whose VPC endpoints will be listed.
--
-- /See:/ 'newListVpcEndpointsForDomain' smart constructor.
data ListVpcEndpointsForDomain = ListVpcEndpointsForDomain'
  { -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Name of the ElasticSearch domain whose VPC endpoints are to be listed.
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
-- 'nextToken', 'listVpcEndpointsForDomain_nextToken' - Provides an identifier to allow retrieval of paginated results.
--
-- 'domainName', 'listVpcEndpointsForDomain_domainName' - Name of the ElasticSearch domain whose VPC endpoints are to be listed.
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

-- | Provides an identifier to allow retrieval of paginated results.
listVpcEndpointsForDomain_nextToken :: Lens.Lens' ListVpcEndpointsForDomain (Prelude.Maybe Prelude.Text)
listVpcEndpointsForDomain_nextToken = Lens.lens (\ListVpcEndpointsForDomain' {nextToken} -> nextToken) (\s@ListVpcEndpointsForDomain' {} a -> s {nextToken = a} :: ListVpcEndpointsForDomain)

-- | Name of the ElasticSearch domain whose VPC endpoints are to be listed.
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
            Prelude.<*> ( x
                            Data..?> "VpcEndpointSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..:> "NextToken")
      )

instance Prelude.Hashable ListVpcEndpointsForDomain where
  hashWithSalt _salt ListVpcEndpointsForDomain' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData ListVpcEndpointsForDomain where
  rnf ListVpcEndpointsForDomain' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf domainName

instance Data.ToHeaders ListVpcEndpointsForDomain where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListVpcEndpointsForDomain where
  toPath ListVpcEndpointsForDomain' {..} =
    Prelude.mconcat
      [ "/2015-01-01/es/domain/",
        Data.toBS domainName,
        "/vpcEndpoints"
      ]

instance Data.ToQuery ListVpcEndpointsForDomain where
  toQuery ListVpcEndpointsForDomain' {..} =
    Prelude.mconcat ["nextToken" Data.=: nextToken]

-- | Container for response parameters to the @ListVpcEndpointsForDomain@
-- operation. Returns a list containing summarized details of the VPC
-- endpoints.
--
-- /See:/ 'newListVpcEndpointsForDomainResponse' smart constructor.
data ListVpcEndpointsForDomainResponse = ListVpcEndpointsForDomainResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Provides list of @VpcEndpointSummary@ summarizing details of the VPC
    -- endpoints.
    vpcEndpointSummaryList :: [VpcEndpointSummary],
    -- | Information about each endpoint associated with the domain.
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
-- 'vpcEndpointSummaryList', 'listVpcEndpointsForDomainResponse_vpcEndpointSummaryList' - Provides list of @VpcEndpointSummary@ summarizing details of the VPC
-- endpoints.
--
-- 'nextToken', 'listVpcEndpointsForDomainResponse_nextToken' - Information about each endpoint associated with the domain.
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

-- | Provides list of @VpcEndpointSummary@ summarizing details of the VPC
-- endpoints.
listVpcEndpointsForDomainResponse_vpcEndpointSummaryList :: Lens.Lens' ListVpcEndpointsForDomainResponse [VpcEndpointSummary]
listVpcEndpointsForDomainResponse_vpcEndpointSummaryList = Lens.lens (\ListVpcEndpointsForDomainResponse' {vpcEndpointSummaryList} -> vpcEndpointSummaryList) (\s@ListVpcEndpointsForDomainResponse' {} a -> s {vpcEndpointSummaryList = a} :: ListVpcEndpointsForDomainResponse) Prelude.. Lens.coerced

-- | Information about each endpoint associated with the domain.
listVpcEndpointsForDomainResponse_nextToken :: Lens.Lens' ListVpcEndpointsForDomainResponse Prelude.Text
listVpcEndpointsForDomainResponse_nextToken = Lens.lens (\ListVpcEndpointsForDomainResponse' {nextToken} -> nextToken) (\s@ListVpcEndpointsForDomainResponse' {} a -> s {nextToken = a} :: ListVpcEndpointsForDomainResponse)

instance
  Prelude.NFData
    ListVpcEndpointsForDomainResponse
  where
  rnf ListVpcEndpointsForDomainResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf vpcEndpointSummaryList `Prelude.seq`
        Prelude.rnf nextToken
