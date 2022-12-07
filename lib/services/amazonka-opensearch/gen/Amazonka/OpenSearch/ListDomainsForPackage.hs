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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all Amazon OpenSearch Service domains associated with a given
-- package. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/custom-packages.html Custom packages for Amazon OpenSearch Service>.
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
    listDomainsForPackageResponse_nextToken,
    listDomainsForPackageResponse_domainPackageDetailsList,
    listDomainsForPackageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the request parameters to the @ListDomainsForPackage@
-- operation.
--
-- /See:/ 'newListDomainsForPackage' smart constructor.
data ListDomainsForPackage = ListDomainsForPackage'
  { -- | If your initial @ListDomainsForPackage@ operation returns a @nextToken@,
    -- you can include the returned @nextToken@ in subsequent
    -- @ListDomainsForPackage@ operations, which returns results in the next
    -- page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An optional parameter that specifies the maximum number of results to
    -- return. You can use @nextToken@ to get the next page of results.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The unique identifier of the package for which to list associated
    -- domains.
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
-- 'nextToken', 'listDomainsForPackage_nextToken' - If your initial @ListDomainsForPackage@ operation returns a @nextToken@,
-- you can include the returned @nextToken@ in subsequent
-- @ListDomainsForPackage@ operations, which returns results in the next
-- page.
--
-- 'maxResults', 'listDomainsForPackage_maxResults' - An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
--
-- 'packageID', 'listDomainsForPackage_packageID' - The unique identifier of the package for which to list associated
-- domains.
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

-- | If your initial @ListDomainsForPackage@ operation returns a @nextToken@,
-- you can include the returned @nextToken@ in subsequent
-- @ListDomainsForPackage@ operations, which returns results in the next
-- page.
listDomainsForPackage_nextToken :: Lens.Lens' ListDomainsForPackage (Prelude.Maybe Prelude.Text)
listDomainsForPackage_nextToken = Lens.lens (\ListDomainsForPackage' {nextToken} -> nextToken) (\s@ListDomainsForPackage' {} a -> s {nextToken = a} :: ListDomainsForPackage)

-- | An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
listDomainsForPackage_maxResults :: Lens.Lens' ListDomainsForPackage (Prelude.Maybe Prelude.Int)
listDomainsForPackage_maxResults = Lens.lens (\ListDomainsForPackage' {maxResults} -> maxResults) (\s@ListDomainsForPackage' {} a -> s {maxResults = a} :: ListDomainsForPackage)

-- | The unique identifier of the package for which to list associated
-- domains.
listDomainsForPackage_packageID :: Lens.Lens' ListDomainsForPackage Prelude.Text
listDomainsForPackage_packageID = Lens.lens (\ListDomainsForPackage' {packageID} -> packageID) (\s@ListDomainsForPackage' {} a -> s {packageID = a} :: ListDomainsForPackage)

instance Core.AWSRequest ListDomainsForPackage where
  type
    AWSResponse ListDomainsForPackage =
      ListDomainsForPackageResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDomainsForPackageResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "DomainPackageDetailsList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDomainsForPackage where
  hashWithSalt _salt ListDomainsForPackage' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` packageID

instance Prelude.NFData ListDomainsForPackage where
  rnf ListDomainsForPackage' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf packageID

instance Data.ToHeaders ListDomainsForPackage where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListDomainsForPackage where
  toPath ListDomainsForPackage' {..} =
    Prelude.mconcat
      [ "/2021-01-01/packages/",
        Data.toBS packageID,
        "/domains"
      ]

instance Data.ToQuery ListDomainsForPackage where
  toQuery ListDomainsForPackage' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | Container for the response parameters to the @ListDomainsForPackage@
-- operation.
--
-- /See:/ 'newListDomainsForPackageResponse' smart constructor.
data ListDomainsForPackageResponse = ListDomainsForPackageResponse'
  { -- | When @nextToken@ is returned, there are more results available. The
    -- value of @nextToken@ is a unique pagination token for each page. Make
    -- the call again using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about all domains associated with a package.
    domainPackageDetailsList :: Prelude.Maybe [DomainPackageDetails],
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
-- 'nextToken', 'listDomainsForPackageResponse_nextToken' - When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
--
-- 'domainPackageDetailsList', 'listDomainsForPackageResponse_domainPackageDetailsList' - Information about all domains associated with a package.
--
-- 'httpStatus', 'listDomainsForPackageResponse_httpStatus' - The response's http status code.
newListDomainsForPackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDomainsForPackageResponse
newListDomainsForPackageResponse pHttpStatus_ =
  ListDomainsForPackageResponse'
    { nextToken =
        Prelude.Nothing,
      domainPackageDetailsList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
listDomainsForPackageResponse_nextToken :: Lens.Lens' ListDomainsForPackageResponse (Prelude.Maybe Prelude.Text)
listDomainsForPackageResponse_nextToken = Lens.lens (\ListDomainsForPackageResponse' {nextToken} -> nextToken) (\s@ListDomainsForPackageResponse' {} a -> s {nextToken = a} :: ListDomainsForPackageResponse)

-- | Information about all domains associated with a package.
listDomainsForPackageResponse_domainPackageDetailsList :: Lens.Lens' ListDomainsForPackageResponse (Prelude.Maybe [DomainPackageDetails])
listDomainsForPackageResponse_domainPackageDetailsList = Lens.lens (\ListDomainsForPackageResponse' {domainPackageDetailsList} -> domainPackageDetailsList) (\s@ListDomainsForPackageResponse' {} a -> s {domainPackageDetailsList = a} :: ListDomainsForPackageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDomainsForPackageResponse_httpStatus :: Lens.Lens' ListDomainsForPackageResponse Prelude.Int
listDomainsForPackageResponse_httpStatus = Lens.lens (\ListDomainsForPackageResponse' {httpStatus} -> httpStatus) (\s@ListDomainsForPackageResponse' {} a -> s {httpStatus = a} :: ListDomainsForPackageResponse)

instance Prelude.NFData ListDomainsForPackageResponse where
  rnf ListDomainsForPackageResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf domainPackageDetailsList
      `Prelude.seq` Prelude.rnf httpStatus
