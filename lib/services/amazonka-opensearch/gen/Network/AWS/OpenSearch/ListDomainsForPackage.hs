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
-- Module      : Amazonka.OpenSearch.ListDomainsForPackage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all Amazon OpenSearch Service domains associated with the package.
module Amazonka.OpenSearch.ListDomainsForPackage
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
    listDomainsForPackageResponse_domainPackageDetailsList,
    listDomainsForPackageResponse_nextToken,
    listDomainsForPackageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the request parameters to the @ ListDomainsForPackage @
-- operation.
--
-- /See:/ 'newListDomainsForPackage' smart constructor.
data ListDomainsForPackage = ListDomainsForPackage'
  { -- | Used for pagination. Only necessary if a previous API call includes a
    -- non-null NextToken value. If provided, returns results for the next
    -- page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Limits the results to a maximum number of domains.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The package for which to list associated domains.
    packageID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'maxResults', 'listDomainsForPackage_maxResults' - Limits the results to a maximum number of domains.
--
-- 'packageID', 'listDomainsForPackage_packageID' - The package for which to list associated domains.
newListDomainsForPackage ::
  -- | 'packageID'
  Prelude.Text ->
  ListDomainsForPackage
newListDomainsForPackage pPackageID_ =
  ListDomainsForPackage'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      packageID = pPackageID_
    }

-- | Used for pagination. Only necessary if a previous API call includes a
-- non-null NextToken value. If provided, returns results for the next
-- page.
listDomainsForPackage_nextToken :: Lens.Lens' ListDomainsForPackage (Prelude.Maybe Prelude.Text)
listDomainsForPackage_nextToken = Lens.lens (\ListDomainsForPackage' {nextToken} -> nextToken) (\s@ListDomainsForPackage' {} a -> s {nextToken = a} :: ListDomainsForPackage)

-- | Limits the results to a maximum number of domains.
listDomainsForPackage_maxResults :: Lens.Lens' ListDomainsForPackage (Prelude.Maybe Prelude.Int)
listDomainsForPackage_maxResults = Lens.lens (\ListDomainsForPackage' {maxResults} -> maxResults) (\s@ListDomainsForPackage' {} a -> s {maxResults = a} :: ListDomainsForPackage)

-- | The package for which to list associated domains.
listDomainsForPackage_packageID :: Lens.Lens' ListDomainsForPackage Prelude.Text
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
            Prelude.<$> ( x Core..?> "DomainPackageDetailsList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDomainsForPackage

instance Prelude.NFData ListDomainsForPackage

instance Core.ToHeaders ListDomainsForPackage where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListDomainsForPackage where
  toPath ListDomainsForPackage' {..} =
    Prelude.mconcat
      [ "/2021-01-01/packages/",
        Core.toBS packageID,
        "/domains"
      ]

instance Core.ToQuery ListDomainsForPackage where
  toQuery ListDomainsForPackage' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | Container for the response parameters to the @ ListDomainsForPackage @
-- operation.
--
-- /See:/ 'newListDomainsForPackageResponse' smart constructor.
data ListDomainsForPackageResponse = ListDomainsForPackageResponse'
  { -- | List of @DomainPackageDetails@ objects.
    domainPackageDetailsList :: Prelude.Maybe [DomainPackageDetails],
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDomainsForPackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainPackageDetailsList', 'listDomainsForPackageResponse_domainPackageDetailsList' - List of @DomainPackageDetails@ objects.
--
-- 'nextToken', 'listDomainsForPackageResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'listDomainsForPackageResponse_httpStatus' - The response's http status code.
newListDomainsForPackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDomainsForPackageResponse
newListDomainsForPackageResponse pHttpStatus_ =
  ListDomainsForPackageResponse'
    { domainPackageDetailsList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of @DomainPackageDetails@ objects.
listDomainsForPackageResponse_domainPackageDetailsList :: Lens.Lens' ListDomainsForPackageResponse (Prelude.Maybe [DomainPackageDetails])
listDomainsForPackageResponse_domainPackageDetailsList = Lens.lens (\ListDomainsForPackageResponse' {domainPackageDetailsList} -> domainPackageDetailsList) (\s@ListDomainsForPackageResponse' {} a -> s {domainPackageDetailsList = a} :: ListDomainsForPackageResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
listDomainsForPackageResponse_nextToken :: Lens.Lens' ListDomainsForPackageResponse (Prelude.Maybe Prelude.Text)
listDomainsForPackageResponse_nextToken = Lens.lens (\ListDomainsForPackageResponse' {nextToken} -> nextToken) (\s@ListDomainsForPackageResponse' {} a -> s {nextToken = a} :: ListDomainsForPackageResponse)

-- | The response's http status code.
listDomainsForPackageResponse_httpStatus :: Lens.Lens' ListDomainsForPackageResponse Prelude.Int
listDomainsForPackageResponse_httpStatus = Lens.lens (\ListDomainsForPackageResponse' {httpStatus} -> httpStatus) (\s@ListDomainsForPackageResponse' {} a -> s {httpStatus = a} :: ListDomainsForPackageResponse)

instance Prelude.NFData ListDomainsForPackageResponse
