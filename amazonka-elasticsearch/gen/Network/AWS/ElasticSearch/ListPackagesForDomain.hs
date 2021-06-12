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
-- Module      : Network.AWS.ElasticSearch.ListPackagesForDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all packages associated with the Amazon ES domain.
module Network.AWS.ElasticSearch.ListPackagesForDomain
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

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @ ListPackagesForDomain @ operation.
--
-- /See:/ 'newListPackagesForDomain' smart constructor.
data ListPackagesForDomain = ListPackagesForDomain'
  { -- | Used for pagination. Only necessary if a previous API call includes a
    -- non-null NextToken value. If provided, returns results for the next
    -- page.
    nextToken :: Core.Maybe Core.Text,
    -- | Limits results to a maximum number of packages.
    maxResults :: Core.Maybe Core.Int,
    -- | The name of the domain for which you want to list associated packages.
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPackagesForDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPackagesForDomain_nextToken' - Used for pagination. Only necessary if a previous API call includes a
-- non-null NextToken value. If provided, returns results for the next
-- page.
--
-- 'maxResults', 'listPackagesForDomain_maxResults' - Limits results to a maximum number of packages.
--
-- 'domainName', 'listPackagesForDomain_domainName' - The name of the domain for which you want to list associated packages.
newListPackagesForDomain ::
  -- | 'domainName'
  Core.Text ->
  ListPackagesForDomain
newListPackagesForDomain pDomainName_ =
  ListPackagesForDomain'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      domainName = pDomainName_
    }

-- | Used for pagination. Only necessary if a previous API call includes a
-- non-null NextToken value. If provided, returns results for the next
-- page.
listPackagesForDomain_nextToken :: Lens.Lens' ListPackagesForDomain (Core.Maybe Core.Text)
listPackagesForDomain_nextToken = Lens.lens (\ListPackagesForDomain' {nextToken} -> nextToken) (\s@ListPackagesForDomain' {} a -> s {nextToken = a} :: ListPackagesForDomain)

-- | Limits results to a maximum number of packages.
listPackagesForDomain_maxResults :: Lens.Lens' ListPackagesForDomain (Core.Maybe Core.Int)
listPackagesForDomain_maxResults = Lens.lens (\ListPackagesForDomain' {maxResults} -> maxResults) (\s@ListPackagesForDomain' {} a -> s {maxResults = a} :: ListPackagesForDomain)

-- | The name of the domain for which you want to list associated packages.
listPackagesForDomain_domainName :: Lens.Lens' ListPackagesForDomain Core.Text
listPackagesForDomain_domainName = Lens.lens (\ListPackagesForDomain' {domainName} -> domainName) (\s@ListPackagesForDomain' {} a -> s {domainName = a} :: ListPackagesForDomain)

instance Core.AWSRequest ListPackagesForDomain where
  type
    AWSResponse ListPackagesForDomain =
      ListPackagesForDomainResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPackagesForDomainResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "DomainPackageDetailsList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListPackagesForDomain

instance Core.NFData ListPackagesForDomain

instance Core.ToHeaders ListPackagesForDomain where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListPackagesForDomain where
  toPath ListPackagesForDomain' {..} =
    Core.mconcat
      [ "/2015-01-01/domain/",
        Core.toBS domainName,
        "/packages"
      ]

instance Core.ToQuery ListPackagesForDomain where
  toQuery ListPackagesForDomain' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | Container for response parameters to @ ListPackagesForDomain @
-- operation.
--
-- /See:/ 'newListPackagesForDomainResponse' smart constructor.
data ListPackagesForDomainResponse = ListPackagesForDomainResponse'
  { -- | Pagination token that needs to be supplied to the next call to get the
    -- next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | List of @DomainPackageDetails@ objects.
    domainPackageDetailsList :: Core.Maybe [DomainPackageDetails],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPackagesForDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPackagesForDomainResponse_nextToken' - Pagination token that needs to be supplied to the next call to get the
-- next page of results.
--
-- 'domainPackageDetailsList', 'listPackagesForDomainResponse_domainPackageDetailsList' - List of @DomainPackageDetails@ objects.
--
-- 'httpStatus', 'listPackagesForDomainResponse_httpStatus' - The response's http status code.
newListPackagesForDomainResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListPackagesForDomainResponse
newListPackagesForDomainResponse pHttpStatus_ =
  ListPackagesForDomainResponse'
    { nextToken =
        Core.Nothing,
      domainPackageDetailsList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Pagination token that needs to be supplied to the next call to get the
-- next page of results.
listPackagesForDomainResponse_nextToken :: Lens.Lens' ListPackagesForDomainResponse (Core.Maybe Core.Text)
listPackagesForDomainResponse_nextToken = Lens.lens (\ListPackagesForDomainResponse' {nextToken} -> nextToken) (\s@ListPackagesForDomainResponse' {} a -> s {nextToken = a} :: ListPackagesForDomainResponse)

-- | List of @DomainPackageDetails@ objects.
listPackagesForDomainResponse_domainPackageDetailsList :: Lens.Lens' ListPackagesForDomainResponse (Core.Maybe [DomainPackageDetails])
listPackagesForDomainResponse_domainPackageDetailsList = Lens.lens (\ListPackagesForDomainResponse' {domainPackageDetailsList} -> domainPackageDetailsList) (\s@ListPackagesForDomainResponse' {} a -> s {domainPackageDetailsList = a} :: ListPackagesForDomainResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listPackagesForDomainResponse_httpStatus :: Lens.Lens' ListPackagesForDomainResponse Core.Int
listPackagesForDomainResponse_httpStatus = Lens.lens (\ListPackagesForDomainResponse' {httpStatus} -> httpStatus) (\s@ListPackagesForDomainResponse' {} a -> s {httpStatus = a} :: ListPackagesForDomainResponse)

instance Core.NFData ListPackagesForDomainResponse
