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
-- Module      : Amazonka.LicenseManager.ListDistributedGrants
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the grants distributed for the specified license.
module Amazonka.LicenseManager.ListDistributedGrants
  ( -- * Creating a Request
    ListDistributedGrants (..),
    newListDistributedGrants,

    -- * Request Lenses
    listDistributedGrants_nextToken,
    listDistributedGrants_filters,
    listDistributedGrants_grantArns,
    listDistributedGrants_maxResults,

    -- * Destructuring the Response
    ListDistributedGrantsResponse (..),
    newListDistributedGrantsResponse,

    -- * Response Lenses
    listDistributedGrantsResponse_nextToken,
    listDistributedGrantsResponse_grants,
    listDistributedGrantsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDistributedGrants' smart constructor.
data ListDistributedGrants = ListDistributedGrants'
  { -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters to scope the results. The following filters are supported:
    --
    -- -   @LicenseArn@
    --
    -- -   @GrantStatus@
    --
    -- -   @GranteePrincipalARN@
    --
    -- -   @ProductSKU@
    --
    -- -   @LicenseIssuerName@
    filters :: Prelude.Maybe [Filter],
    -- | Amazon Resource Names (ARNs) of the grants.
    grantArns :: Prelude.Maybe [Prelude.Text],
    -- | Maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDistributedGrants' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDistributedGrants_nextToken' - Token for the next set of results.
--
-- 'filters', 'listDistributedGrants_filters' - Filters to scope the results. The following filters are supported:
--
-- -   @LicenseArn@
--
-- -   @GrantStatus@
--
-- -   @GranteePrincipalARN@
--
-- -   @ProductSKU@
--
-- -   @LicenseIssuerName@
--
-- 'grantArns', 'listDistributedGrants_grantArns' - Amazon Resource Names (ARNs) of the grants.
--
-- 'maxResults', 'listDistributedGrants_maxResults' - Maximum number of results to return in a single call.
newListDistributedGrants ::
  ListDistributedGrants
newListDistributedGrants =
  ListDistributedGrants'
    { nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      grantArns = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Token for the next set of results.
listDistributedGrants_nextToken :: Lens.Lens' ListDistributedGrants (Prelude.Maybe Prelude.Text)
listDistributedGrants_nextToken = Lens.lens (\ListDistributedGrants' {nextToken} -> nextToken) (\s@ListDistributedGrants' {} a -> s {nextToken = a} :: ListDistributedGrants)

-- | Filters to scope the results. The following filters are supported:
--
-- -   @LicenseArn@
--
-- -   @GrantStatus@
--
-- -   @GranteePrincipalARN@
--
-- -   @ProductSKU@
--
-- -   @LicenseIssuerName@
listDistributedGrants_filters :: Lens.Lens' ListDistributedGrants (Prelude.Maybe [Filter])
listDistributedGrants_filters = Lens.lens (\ListDistributedGrants' {filters} -> filters) (\s@ListDistributedGrants' {} a -> s {filters = a} :: ListDistributedGrants) Prelude.. Lens.mapping Lens.coerced

-- | Amazon Resource Names (ARNs) of the grants.
listDistributedGrants_grantArns :: Lens.Lens' ListDistributedGrants (Prelude.Maybe [Prelude.Text])
listDistributedGrants_grantArns = Lens.lens (\ListDistributedGrants' {grantArns} -> grantArns) (\s@ListDistributedGrants' {} a -> s {grantArns = a} :: ListDistributedGrants) Prelude.. Lens.mapping Lens.coerced

-- | Maximum number of results to return in a single call.
listDistributedGrants_maxResults :: Lens.Lens' ListDistributedGrants (Prelude.Maybe Prelude.Natural)
listDistributedGrants_maxResults = Lens.lens (\ListDistributedGrants' {maxResults} -> maxResults) (\s@ListDistributedGrants' {} a -> s {maxResults = a} :: ListDistributedGrants)

instance Core.AWSRequest ListDistributedGrants where
  type
    AWSResponse ListDistributedGrants =
      ListDistributedGrantsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDistributedGrantsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Grants" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDistributedGrants where
  hashWithSalt _salt ListDistributedGrants' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` grantArns
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListDistributedGrants where
  rnf ListDistributedGrants' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf grantArns
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListDistributedGrants where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.ListDistributedGrants" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDistributedGrants where
  toJSON ListDistributedGrants' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Filters" Data..=) Prelude.<$> filters,
            ("GrantArns" Data..=) Prelude.<$> grantArns,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListDistributedGrants where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDistributedGrants where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDistributedGrantsResponse' smart constructor.
data ListDistributedGrantsResponse = ListDistributedGrantsResponse'
  { -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Distributed grant details.
    grants :: Prelude.Maybe [Grant],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDistributedGrantsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDistributedGrantsResponse_nextToken' - Token for the next set of results.
--
-- 'grants', 'listDistributedGrantsResponse_grants' - Distributed grant details.
--
-- 'httpStatus', 'listDistributedGrantsResponse_httpStatus' - The response's http status code.
newListDistributedGrantsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDistributedGrantsResponse
newListDistributedGrantsResponse pHttpStatus_ =
  ListDistributedGrantsResponse'
    { nextToken =
        Prelude.Nothing,
      grants = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token for the next set of results.
listDistributedGrantsResponse_nextToken :: Lens.Lens' ListDistributedGrantsResponse (Prelude.Maybe Prelude.Text)
listDistributedGrantsResponse_nextToken = Lens.lens (\ListDistributedGrantsResponse' {nextToken} -> nextToken) (\s@ListDistributedGrantsResponse' {} a -> s {nextToken = a} :: ListDistributedGrantsResponse)

-- | Distributed grant details.
listDistributedGrantsResponse_grants :: Lens.Lens' ListDistributedGrantsResponse (Prelude.Maybe [Grant])
listDistributedGrantsResponse_grants = Lens.lens (\ListDistributedGrantsResponse' {grants} -> grants) (\s@ListDistributedGrantsResponse' {} a -> s {grants = a} :: ListDistributedGrantsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDistributedGrantsResponse_httpStatus :: Lens.Lens' ListDistributedGrantsResponse Prelude.Int
listDistributedGrantsResponse_httpStatus = Lens.lens (\ListDistributedGrantsResponse' {httpStatus} -> httpStatus) (\s@ListDistributedGrantsResponse' {} a -> s {httpStatus = a} :: ListDistributedGrantsResponse)

instance Prelude.NFData ListDistributedGrantsResponse where
  rnf ListDistributedGrantsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf grants
      `Prelude.seq` Prelude.rnf httpStatus
