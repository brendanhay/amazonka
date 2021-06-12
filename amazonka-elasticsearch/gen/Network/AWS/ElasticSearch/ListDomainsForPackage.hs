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
-- Module      : Network.AWS.ElasticSearch.ListDomainsForPackage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all Amazon ES domains associated with the package.
module Network.AWS.ElasticSearch.ListDomainsForPackage
  ( -- * Creating a Request
    ListDomainsForPackage (..),
    newListDomainsForPackage,

    -- * Request Lenses
    listDomainsForPackage_nextToken,
    listDomainsForPackage_maxResults,
    listDomainsForPackage_packageID,

    -- * Destructuring the Response
    ListDomainsForPackageResponse (..),
    newListDomainsForPackageResponse,

    -- * Response Lenses
    listDomainsForPackageResponse_nextToken,
    listDomainsForPackageResponse_domainPackageDetailsList,
    listDomainsForPackageResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @ ListDomainsForPackage @ operation.
--
-- /See:/ 'newListDomainsForPackage' smart constructor.
data ListDomainsForPackage = ListDomainsForPackage'
  { -- | Used for pagination. Only necessary if a previous API call includes a
    -- non-null NextToken value. If provided, returns results for the next
    -- page.
    nextToken :: Core.Maybe Core.Text,
    -- | Limits results to a maximum number of domains.
    maxResults :: Core.Maybe Core.Int,
    -- | The package for which to list domains.
    packageID :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDomainsForPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDomainsForPackage_nextToken' - Used for pagination. Only necessary if a previous API call includes a
-- non-null NextToken value. If provided, returns results for the next
-- page.
--
-- 'maxResults', 'listDomainsForPackage_maxResults' - Limits results to a maximum number of domains.
--
-- 'packageID', 'listDomainsForPackage_packageID' - The package for which to list domains.
newListDomainsForPackage ::
  -- | 'packageID'
  Core.Text ->
  ListDomainsForPackage
newListDomainsForPackage pPackageID_ =
  ListDomainsForPackage'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      packageID = pPackageID_
    }

-- | Used for pagination. Only necessary if a previous API call includes a
-- non-null NextToken value. If provided, returns results for the next
-- page.
listDomainsForPackage_nextToken :: Lens.Lens' ListDomainsForPackage (Core.Maybe Core.Text)
listDomainsForPackage_nextToken = Lens.lens (\ListDomainsForPackage' {nextToken} -> nextToken) (\s@ListDomainsForPackage' {} a -> s {nextToken = a} :: ListDomainsForPackage)

-- | Limits results to a maximum number of domains.
listDomainsForPackage_maxResults :: Lens.Lens' ListDomainsForPackage (Core.Maybe Core.Int)
listDomainsForPackage_maxResults = Lens.lens (\ListDomainsForPackage' {maxResults} -> maxResults) (\s@ListDomainsForPackage' {} a -> s {maxResults = a} :: ListDomainsForPackage)

-- | The package for which to list domains.
listDomainsForPackage_packageID :: Lens.Lens' ListDomainsForPackage Core.Text
listDomainsForPackage_packageID = Lens.lens (\ListDomainsForPackage' {packageID} -> packageID) (\s@ListDomainsForPackage' {} a -> s {packageID = a} :: ListDomainsForPackage)

instance Core.AWSRequest ListDomainsForPackage where
  type
    AWSResponse ListDomainsForPackage =
      ListDomainsForPackageResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDomainsForPackageResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "DomainPackageDetailsList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDomainsForPackage

instance Core.NFData ListDomainsForPackage

instance Core.ToHeaders ListDomainsForPackage where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListDomainsForPackage where
  toPath ListDomainsForPackage' {..} =
    Core.mconcat
      [ "/2015-01-01/packages/",
        Core.toBS packageID,
        "/domains"
      ]

instance Core.ToQuery ListDomainsForPackage where
  toQuery ListDomainsForPackage' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | Container for response parameters to @ ListDomainsForPackage @
-- operation.
--
-- /See:/ 'newListDomainsForPackageResponse' smart constructor.
data ListDomainsForPackageResponse = ListDomainsForPackageResponse'
  { nextToken :: Core.Maybe Core.Text,
    -- | List of @DomainPackageDetails@ objects.
    domainPackageDetailsList :: Core.Maybe [DomainPackageDetails],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDomainsForPackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDomainsForPackageResponse_nextToken' - Undocumented member.
--
-- 'domainPackageDetailsList', 'listDomainsForPackageResponse_domainPackageDetailsList' - List of @DomainPackageDetails@ objects.
--
-- 'httpStatus', 'listDomainsForPackageResponse_httpStatus' - The response's http status code.
newListDomainsForPackageResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDomainsForPackageResponse
newListDomainsForPackageResponse pHttpStatus_ =
  ListDomainsForPackageResponse'
    { nextToken =
        Core.Nothing,
      domainPackageDetailsList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listDomainsForPackageResponse_nextToken :: Lens.Lens' ListDomainsForPackageResponse (Core.Maybe Core.Text)
listDomainsForPackageResponse_nextToken = Lens.lens (\ListDomainsForPackageResponse' {nextToken} -> nextToken) (\s@ListDomainsForPackageResponse' {} a -> s {nextToken = a} :: ListDomainsForPackageResponse)

-- | List of @DomainPackageDetails@ objects.
listDomainsForPackageResponse_domainPackageDetailsList :: Lens.Lens' ListDomainsForPackageResponse (Core.Maybe [DomainPackageDetails])
listDomainsForPackageResponse_domainPackageDetailsList = Lens.lens (\ListDomainsForPackageResponse' {domainPackageDetailsList} -> domainPackageDetailsList) (\s@ListDomainsForPackageResponse' {} a -> s {domainPackageDetailsList = a} :: ListDomainsForPackageResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDomainsForPackageResponse_httpStatus :: Lens.Lens' ListDomainsForPackageResponse Core.Int
listDomainsForPackageResponse_httpStatus = Lens.lens (\ListDomainsForPackageResponse' {httpStatus} -> httpStatus) (\s@ListDomainsForPackageResponse' {} a -> s {httpStatus = a} :: ListDomainsForPackageResponse)

instance Core.NFData ListDomainsForPackageResponse
