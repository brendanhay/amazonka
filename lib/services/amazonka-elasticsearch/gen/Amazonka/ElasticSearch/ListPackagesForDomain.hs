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
-- Module      : Amazonka.ElasticSearch.ListPackagesForDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all packages associated with the Amazon ES domain.
module Amazonka.ElasticSearch.ListPackagesForDomain
  ( -- * Creating a Request
    ListPackagesForDomain (..),
    newListPackagesForDomain,

    -- * Request Lenses
    listPackagesForDomain_maxResults,
    listPackagesForDomain_nextToken,
    listPackagesForDomain_domainName,

    -- * Destructuring the Response
    ListPackagesForDomainResponse (..),
    newListPackagesForDomainResponse,

    -- * Response Lenses
    listPackagesForDomainResponse_domainPackageDetailsList,
    listPackagesForDomainResponse_nextToken,
    listPackagesForDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for request parameters to @ @@ListPackagesForDomain@@ @
-- operation.
--
-- /See:/ 'newListPackagesForDomain' smart constructor.
data ListPackagesForDomain = ListPackagesForDomain'
  { -- | Limits results to a maximum number of packages.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Used for pagination. Only necessary if a previous API call includes a
    -- non-null NextToken value. If provided, returns results for the next
    -- page.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'maxResults', 'listPackagesForDomain_maxResults' - Limits results to a maximum number of packages.
--
-- 'nextToken', 'listPackagesForDomain_nextToken' - Used for pagination. Only necessary if a previous API call includes a
-- non-null NextToken value. If provided, returns results for the next
-- page.
--
-- 'domainName', 'listPackagesForDomain_domainName' - The name of the domain for which you want to list associated packages.
newListPackagesForDomain ::
  -- | 'domainName'
  Prelude.Text ->
  ListPackagesForDomain
newListPackagesForDomain pDomainName_ =
  ListPackagesForDomain'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | Limits results to a maximum number of packages.
listPackagesForDomain_maxResults :: Lens.Lens' ListPackagesForDomain (Prelude.Maybe Prelude.Int)
listPackagesForDomain_maxResults = Lens.lens (\ListPackagesForDomain' {maxResults} -> maxResults) (\s@ListPackagesForDomain' {} a -> s {maxResults = a} :: ListPackagesForDomain)

-- | Used for pagination. Only necessary if a previous API call includes a
-- non-null NextToken value. If provided, returns results for the next
-- page.
listPackagesForDomain_nextToken :: Lens.Lens' ListPackagesForDomain (Prelude.Maybe Prelude.Text)
listPackagesForDomain_nextToken = Lens.lens (\ListPackagesForDomain' {nextToken} -> nextToken) (\s@ListPackagesForDomain' {} a -> s {nextToken = a} :: ListPackagesForDomain)

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
            Prelude.<$> ( x
                            Data..?> "DomainPackageDetailsList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPackagesForDomain where
  hashWithSalt _salt ListPackagesForDomain' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData ListPackagesForDomain where
  rnf ListPackagesForDomain' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders ListPackagesForDomain where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListPackagesForDomain where
  toPath ListPackagesForDomain' {..} =
    Prelude.mconcat
      [ "/2015-01-01/domain/",
        Data.toBS domainName,
        "/packages"
      ]

instance Data.ToQuery ListPackagesForDomain where
  toQuery ListPackagesForDomain' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | Container for response parameters to @ @@ListPackagesForDomain@@ @
-- operation.
--
-- /See:/ 'newListPackagesForDomainResponse' smart constructor.
data ListPackagesForDomainResponse = ListPackagesForDomainResponse'
  { -- | List of @DomainPackageDetails@ objects.
    domainPackageDetailsList :: Prelude.Maybe [DomainPackageDetails],
    -- | Pagination token that needs to be supplied to the next call to get the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'domainPackageDetailsList', 'listPackagesForDomainResponse_domainPackageDetailsList' - List of @DomainPackageDetails@ objects.
--
-- 'nextToken', 'listPackagesForDomainResponse_nextToken' - Pagination token that needs to be supplied to the next call to get the
-- next page of results.
--
-- 'httpStatus', 'listPackagesForDomainResponse_httpStatus' - The response's http status code.
newListPackagesForDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPackagesForDomainResponse
newListPackagesForDomainResponse pHttpStatus_ =
  ListPackagesForDomainResponse'
    { domainPackageDetailsList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of @DomainPackageDetails@ objects.
listPackagesForDomainResponse_domainPackageDetailsList :: Lens.Lens' ListPackagesForDomainResponse (Prelude.Maybe [DomainPackageDetails])
listPackagesForDomainResponse_domainPackageDetailsList = Lens.lens (\ListPackagesForDomainResponse' {domainPackageDetailsList} -> domainPackageDetailsList) (\s@ListPackagesForDomainResponse' {} a -> s {domainPackageDetailsList = a} :: ListPackagesForDomainResponse) Prelude.. Lens.mapping Lens.coerced

-- | Pagination token that needs to be supplied to the next call to get the
-- next page of results.
listPackagesForDomainResponse_nextToken :: Lens.Lens' ListPackagesForDomainResponse (Prelude.Maybe Prelude.Text)
listPackagesForDomainResponse_nextToken = Lens.lens (\ListPackagesForDomainResponse' {nextToken} -> nextToken) (\s@ListPackagesForDomainResponse' {} a -> s {nextToken = a} :: ListPackagesForDomainResponse)

-- | The response's http status code.
listPackagesForDomainResponse_httpStatus :: Lens.Lens' ListPackagesForDomainResponse Prelude.Int
listPackagesForDomainResponse_httpStatus = Lens.lens (\ListPackagesForDomainResponse' {httpStatus} -> httpStatus) (\s@ListPackagesForDomainResponse' {} a -> s {httpStatus = a} :: ListPackagesForDomainResponse)

instance Prelude.NFData ListPackagesForDomainResponse where
  rnf ListPackagesForDomainResponse' {..} =
    Prelude.rnf domainPackageDetailsList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
