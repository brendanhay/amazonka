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
-- Module      : Amazonka.LicenseManager.ListReceivedGrants
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists grants that are received but not accepted.
module Amazonka.LicenseManager.ListReceivedGrants
  ( -- * Creating a Request
    ListReceivedGrants (..),
    newListReceivedGrants,

    -- * Request Lenses
    listReceivedGrants_nextToken,
    listReceivedGrants_filters,
    listReceivedGrants_grantArns,
    listReceivedGrants_maxResults,

    -- * Destructuring the Response
    ListReceivedGrantsResponse (..),
    newListReceivedGrantsResponse,

    -- * Response Lenses
    listReceivedGrantsResponse_nextToken,
    listReceivedGrantsResponse_grants,
    listReceivedGrantsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListReceivedGrants' smart constructor.
data ListReceivedGrants = ListReceivedGrants'
  { -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters to scope the results. The following filters are supported:
    --
    -- -   @ProductSKU@
    --
    -- -   @LicenseIssuerName@
    --
    -- -   @LicenseArn@
    --
    -- -   @GrantStatus@
    --
    -- -   @GranterAccountId@
    filters :: Prelude.Maybe [Filter],
    -- | Amazon Resource Names (ARNs) of the grants.
    grantArns :: Prelude.Maybe [Prelude.Text],
    -- | Maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReceivedGrants' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReceivedGrants_nextToken' - Token for the next set of results.
--
-- 'filters', 'listReceivedGrants_filters' - Filters to scope the results. The following filters are supported:
--
-- -   @ProductSKU@
--
-- -   @LicenseIssuerName@
--
-- -   @LicenseArn@
--
-- -   @GrantStatus@
--
-- -   @GranterAccountId@
--
-- 'grantArns', 'listReceivedGrants_grantArns' - Amazon Resource Names (ARNs) of the grants.
--
-- 'maxResults', 'listReceivedGrants_maxResults' - Maximum number of results to return in a single call.
newListReceivedGrants ::
  ListReceivedGrants
newListReceivedGrants =
  ListReceivedGrants'
    { nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      grantArns = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Token for the next set of results.
listReceivedGrants_nextToken :: Lens.Lens' ListReceivedGrants (Prelude.Maybe Prelude.Text)
listReceivedGrants_nextToken = Lens.lens (\ListReceivedGrants' {nextToken} -> nextToken) (\s@ListReceivedGrants' {} a -> s {nextToken = a} :: ListReceivedGrants)

-- | Filters to scope the results. The following filters are supported:
--
-- -   @ProductSKU@
--
-- -   @LicenseIssuerName@
--
-- -   @LicenseArn@
--
-- -   @GrantStatus@
--
-- -   @GranterAccountId@
listReceivedGrants_filters :: Lens.Lens' ListReceivedGrants (Prelude.Maybe [Filter])
listReceivedGrants_filters = Lens.lens (\ListReceivedGrants' {filters} -> filters) (\s@ListReceivedGrants' {} a -> s {filters = a} :: ListReceivedGrants) Prelude.. Lens.mapping Lens.coerced

-- | Amazon Resource Names (ARNs) of the grants.
listReceivedGrants_grantArns :: Lens.Lens' ListReceivedGrants (Prelude.Maybe [Prelude.Text])
listReceivedGrants_grantArns = Lens.lens (\ListReceivedGrants' {grantArns} -> grantArns) (\s@ListReceivedGrants' {} a -> s {grantArns = a} :: ListReceivedGrants) Prelude.. Lens.mapping Lens.coerced

-- | Maximum number of results to return in a single call.
listReceivedGrants_maxResults :: Lens.Lens' ListReceivedGrants (Prelude.Maybe Prelude.Natural)
listReceivedGrants_maxResults = Lens.lens (\ListReceivedGrants' {maxResults} -> maxResults) (\s@ListReceivedGrants' {} a -> s {maxResults = a} :: ListReceivedGrants)

instance Core.AWSRequest ListReceivedGrants where
  type
    AWSResponse ListReceivedGrants =
      ListReceivedGrantsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReceivedGrantsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Grants" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListReceivedGrants where
  hashWithSalt _salt ListReceivedGrants' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` grantArns
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListReceivedGrants where
  rnf ListReceivedGrants' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf grantArns
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListReceivedGrants where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.ListReceivedGrants" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListReceivedGrants where
  toJSON ListReceivedGrants' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Filters" Data..=) Prelude.<$> filters,
            ("GrantArns" Data..=) Prelude.<$> grantArns,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListReceivedGrants where
  toPath = Prelude.const "/"

instance Data.ToQuery ListReceivedGrants where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListReceivedGrantsResponse' smart constructor.
data ListReceivedGrantsResponse = ListReceivedGrantsResponse'
  { -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Received grant details.
    grants :: Prelude.Maybe [Grant],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReceivedGrantsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReceivedGrantsResponse_nextToken' - Token for the next set of results.
--
-- 'grants', 'listReceivedGrantsResponse_grants' - Received grant details.
--
-- 'httpStatus', 'listReceivedGrantsResponse_httpStatus' - The response's http status code.
newListReceivedGrantsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReceivedGrantsResponse
newListReceivedGrantsResponse pHttpStatus_ =
  ListReceivedGrantsResponse'
    { nextToken =
        Prelude.Nothing,
      grants = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token for the next set of results.
listReceivedGrantsResponse_nextToken :: Lens.Lens' ListReceivedGrantsResponse (Prelude.Maybe Prelude.Text)
listReceivedGrantsResponse_nextToken = Lens.lens (\ListReceivedGrantsResponse' {nextToken} -> nextToken) (\s@ListReceivedGrantsResponse' {} a -> s {nextToken = a} :: ListReceivedGrantsResponse)

-- | Received grant details.
listReceivedGrantsResponse_grants :: Lens.Lens' ListReceivedGrantsResponse (Prelude.Maybe [Grant])
listReceivedGrantsResponse_grants = Lens.lens (\ListReceivedGrantsResponse' {grants} -> grants) (\s@ListReceivedGrantsResponse' {} a -> s {grants = a} :: ListReceivedGrantsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listReceivedGrantsResponse_httpStatus :: Lens.Lens' ListReceivedGrantsResponse Prelude.Int
listReceivedGrantsResponse_httpStatus = Lens.lens (\ListReceivedGrantsResponse' {httpStatus} -> httpStatus) (\s@ListReceivedGrantsResponse' {} a -> s {httpStatus = a} :: ListReceivedGrantsResponse)

instance Prelude.NFData ListReceivedGrantsResponse where
  rnf ListReceivedGrantsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf grants
      `Prelude.seq` Prelude.rnf httpStatus
