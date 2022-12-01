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
-- Module      : Amazonka.OpenSearch.ListPackagesForDomain
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all packages associated with an Amazon OpenSearch Service domain.
-- For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/custom-packages.html Custom packages for Amazon OpenSearch Service>.
module Amazonka.OpenSearch.ListPackagesForDomain
  ( -- * Creating a Request
    ListPackagesForDomain (..),
    newListPackagesForDomain,

    -- * Request Lenses
    listPackagesForDomain_nextToken,
    listPackagesForDomain_maxResults,
    listPackagesForDomain_domainName,

    -- * Destructuring the Response
    ListPackagesForDomainResponse (..),
    newListPackagesForDomainResponse,

    -- * Response Lenses
    listPackagesForDomainResponse_nextToken,
    listPackagesForDomainResponse_domainPackageDetailsList,
    listPackagesForDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the request parameters to the @ListPackagesForDomain@
-- operation.
--
-- /See:/ 'newListPackagesForDomain' smart constructor.
data ListPackagesForDomain = ListPackagesForDomain'
  { -- | If your initial @ListPackagesForDomain@ operation returns a @nextToken@,
    -- you can include the returned @nextToken@ in subsequent
    -- @ListPackagesForDomain@ operations, which returns results in the next
    -- page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An optional parameter that specifies the maximum number of results to
    -- return. You can use @nextToken@ to get the next page of results.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The name of the domain for which you want to list associated packages.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPackagesForDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPackagesForDomain_nextToken' - If your initial @ListPackagesForDomain@ operation returns a @nextToken@,
-- you can include the returned @nextToken@ in subsequent
-- @ListPackagesForDomain@ operations, which returns results in the next
-- page.
--
-- 'maxResults', 'listPackagesForDomain_maxResults' - An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
--
-- 'domainName', 'listPackagesForDomain_domainName' - The name of the domain for which you want to list associated packages.
newListPackagesForDomain ::
  -- | 'domainName'
  Prelude.Text ->
  ListPackagesForDomain
newListPackagesForDomain pDomainName_ =
  ListPackagesForDomain'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | If your initial @ListPackagesForDomain@ operation returns a @nextToken@,
-- you can include the returned @nextToken@ in subsequent
-- @ListPackagesForDomain@ operations, which returns results in the next
-- page.
listPackagesForDomain_nextToken :: Lens.Lens' ListPackagesForDomain (Prelude.Maybe Prelude.Text)
listPackagesForDomain_nextToken = Lens.lens (\ListPackagesForDomain' {nextToken} -> nextToken) (\s@ListPackagesForDomain' {} a -> s {nextToken = a} :: ListPackagesForDomain)

-- | An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
listPackagesForDomain_maxResults :: Lens.Lens' ListPackagesForDomain (Prelude.Maybe Prelude.Int)
listPackagesForDomain_maxResults = Lens.lens (\ListPackagesForDomain' {maxResults} -> maxResults) (\s@ListPackagesForDomain' {} a -> s {maxResults = a} :: ListPackagesForDomain)

-- | The name of the domain for which you want to list associated packages.
listPackagesForDomain_domainName :: Lens.Lens' ListPackagesForDomain Prelude.Text
listPackagesForDomain_domainName = Lens.lens (\ListPackagesForDomain' {domainName} -> domainName) (\s@ListPackagesForDomain' {} a -> s {domainName = a} :: ListPackagesForDomain)

instance Core.AWSRequest ListPackagesForDomain where
  type
    AWSResponse ListPackagesForDomain =
      ListPackagesForDomainResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPackagesForDomainResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "DomainPackageDetailsList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPackagesForDomain where
  hashWithSalt _salt ListPackagesForDomain' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData ListPackagesForDomain where
  rnf ListPackagesForDomain' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf domainName

instance Core.ToHeaders ListPackagesForDomain where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListPackagesForDomain where
  toPath ListPackagesForDomain' {..} =
    Prelude.mconcat
      [ "/2021-01-01/domain/",
        Core.toBS domainName,
        "/packages"
      ]

instance Core.ToQuery ListPackagesForDomain where
  toQuery ListPackagesForDomain' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | Container for the response parameters to the @ListPackagesForDomain@
-- operation.
--
-- /See:/ 'newListPackagesForDomainResponse' smart constructor.
data ListPackagesForDomainResponse = ListPackagesForDomainResponse'
  { -- | When @nextToken@ is returned, there are more results available. The
    -- value of @nextToken@ is a unique pagination token for each page. Make
    -- the call again using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of all packages associated with a domain.
    domainPackageDetailsList :: Prelude.Maybe [DomainPackageDetails],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPackagesForDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPackagesForDomainResponse_nextToken' - When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
--
-- 'domainPackageDetailsList', 'listPackagesForDomainResponse_domainPackageDetailsList' - List of all packages associated with a domain.
--
-- 'httpStatus', 'listPackagesForDomainResponse_httpStatus' - The response's http status code.
newListPackagesForDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPackagesForDomainResponse
newListPackagesForDomainResponse pHttpStatus_ =
  ListPackagesForDomainResponse'
    { nextToken =
        Prelude.Nothing,
      domainPackageDetailsList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
listPackagesForDomainResponse_nextToken :: Lens.Lens' ListPackagesForDomainResponse (Prelude.Maybe Prelude.Text)
listPackagesForDomainResponse_nextToken = Lens.lens (\ListPackagesForDomainResponse' {nextToken} -> nextToken) (\s@ListPackagesForDomainResponse' {} a -> s {nextToken = a} :: ListPackagesForDomainResponse)

-- | List of all packages associated with a domain.
listPackagesForDomainResponse_domainPackageDetailsList :: Lens.Lens' ListPackagesForDomainResponse (Prelude.Maybe [DomainPackageDetails])
listPackagesForDomainResponse_domainPackageDetailsList = Lens.lens (\ListPackagesForDomainResponse' {domainPackageDetailsList} -> domainPackageDetailsList) (\s@ListPackagesForDomainResponse' {} a -> s {domainPackageDetailsList = a} :: ListPackagesForDomainResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPackagesForDomainResponse_httpStatus :: Lens.Lens' ListPackagesForDomainResponse Prelude.Int
listPackagesForDomainResponse_httpStatus = Lens.lens (\ListPackagesForDomainResponse' {httpStatus} -> httpStatus) (\s@ListPackagesForDomainResponse' {} a -> s {httpStatus = a} :: ListPackagesForDomainResponse)

instance Prelude.NFData ListPackagesForDomainResponse where
  rnf ListPackagesForDomainResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf domainPackageDetailsList
      `Prelude.seq` Prelude.rnf httpStatus
