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
-- Module      : Amazonka.LicenseManager.ListReceivedLicensesForOrganization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the licenses received for all accounts in the organization.
module Amazonka.LicenseManager.ListReceivedLicensesForOrganization
  ( -- * Creating a Request
    ListReceivedLicensesForOrganization (..),
    newListReceivedLicensesForOrganization,

    -- * Request Lenses
    listReceivedLicensesForOrganization_nextToken,
    listReceivedLicensesForOrganization_filters,
    listReceivedLicensesForOrganization_maxResults,

    -- * Destructuring the Response
    ListReceivedLicensesForOrganizationResponse (..),
    newListReceivedLicensesForOrganizationResponse,

    -- * Response Lenses
    listReceivedLicensesForOrganizationResponse_nextToken,
    listReceivedLicensesForOrganizationResponse_licenses,
    listReceivedLicensesForOrganizationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListReceivedLicensesForOrganization' smart constructor.
data ListReceivedLicensesForOrganization = ListReceivedLicensesForOrganization'
  { -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters to scope the results. The following filters are supported:
    --
    -- -   @Beneficiary@
    --
    -- -   @ProductSKU@
    filters :: Prelude.Maybe [Filter],
    -- | Maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReceivedLicensesForOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReceivedLicensesForOrganization_nextToken' - Token for the next set of results.
--
-- 'filters', 'listReceivedLicensesForOrganization_filters' - Filters to scope the results. The following filters are supported:
--
-- -   @Beneficiary@
--
-- -   @ProductSKU@
--
-- 'maxResults', 'listReceivedLicensesForOrganization_maxResults' - Maximum number of results to return in a single call.
newListReceivedLicensesForOrganization ::
  ListReceivedLicensesForOrganization
newListReceivedLicensesForOrganization =
  ListReceivedLicensesForOrganization'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Token for the next set of results.
listReceivedLicensesForOrganization_nextToken :: Lens.Lens' ListReceivedLicensesForOrganization (Prelude.Maybe Prelude.Text)
listReceivedLicensesForOrganization_nextToken = Lens.lens (\ListReceivedLicensesForOrganization' {nextToken} -> nextToken) (\s@ListReceivedLicensesForOrganization' {} a -> s {nextToken = a} :: ListReceivedLicensesForOrganization)

-- | Filters to scope the results. The following filters are supported:
--
-- -   @Beneficiary@
--
-- -   @ProductSKU@
listReceivedLicensesForOrganization_filters :: Lens.Lens' ListReceivedLicensesForOrganization (Prelude.Maybe [Filter])
listReceivedLicensesForOrganization_filters = Lens.lens (\ListReceivedLicensesForOrganization' {filters} -> filters) (\s@ListReceivedLicensesForOrganization' {} a -> s {filters = a} :: ListReceivedLicensesForOrganization) Prelude.. Lens.mapping Lens.coerced

-- | Maximum number of results to return in a single call.
listReceivedLicensesForOrganization_maxResults :: Lens.Lens' ListReceivedLicensesForOrganization (Prelude.Maybe Prelude.Natural)
listReceivedLicensesForOrganization_maxResults = Lens.lens (\ListReceivedLicensesForOrganization' {maxResults} -> maxResults) (\s@ListReceivedLicensesForOrganization' {} a -> s {maxResults = a} :: ListReceivedLicensesForOrganization)

instance
  Core.AWSRequest
    ListReceivedLicensesForOrganization
  where
  type
    AWSResponse ListReceivedLicensesForOrganization =
      ListReceivedLicensesForOrganizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReceivedLicensesForOrganizationResponse'
            Prelude.<$> (x Data..?> "NextToken")
              Prelude.<*> (x Data..?> "Licenses" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListReceivedLicensesForOrganization
  where
  hashWithSalt
    _salt
    ListReceivedLicensesForOrganization' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults

instance
  Prelude.NFData
    ListReceivedLicensesForOrganization
  where
  rnf ListReceivedLicensesForOrganization' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance
  Data.ToHeaders
    ListReceivedLicensesForOrganization
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.ListReceivedLicensesForOrganization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListReceivedLicensesForOrganization
  where
  toJSON ListReceivedLicensesForOrganization' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance
  Data.ToPath
    ListReceivedLicensesForOrganization
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListReceivedLicensesForOrganization
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListReceivedLicensesForOrganizationResponse' smart constructor.
data ListReceivedLicensesForOrganizationResponse = ListReceivedLicensesForOrganizationResponse'
  { -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Lists the licenses the organization has received.
    licenses :: Prelude.Maybe [GrantedLicense],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReceivedLicensesForOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReceivedLicensesForOrganizationResponse_nextToken' - Token for the next set of results.
--
-- 'licenses', 'listReceivedLicensesForOrganizationResponse_licenses' - Lists the licenses the organization has received.
--
-- 'httpStatus', 'listReceivedLicensesForOrganizationResponse_httpStatus' - The response's http status code.
newListReceivedLicensesForOrganizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReceivedLicensesForOrganizationResponse
newListReceivedLicensesForOrganizationResponse
  pHttpStatus_ =
    ListReceivedLicensesForOrganizationResponse'
      { nextToken =
          Prelude.Nothing,
        licenses = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Token for the next set of results.
listReceivedLicensesForOrganizationResponse_nextToken :: Lens.Lens' ListReceivedLicensesForOrganizationResponse (Prelude.Maybe Prelude.Text)
listReceivedLicensesForOrganizationResponse_nextToken = Lens.lens (\ListReceivedLicensesForOrganizationResponse' {nextToken} -> nextToken) (\s@ListReceivedLicensesForOrganizationResponse' {} a -> s {nextToken = a} :: ListReceivedLicensesForOrganizationResponse)

-- | Lists the licenses the organization has received.
listReceivedLicensesForOrganizationResponse_licenses :: Lens.Lens' ListReceivedLicensesForOrganizationResponse (Prelude.Maybe [GrantedLicense])
listReceivedLicensesForOrganizationResponse_licenses = Lens.lens (\ListReceivedLicensesForOrganizationResponse' {licenses} -> licenses) (\s@ListReceivedLicensesForOrganizationResponse' {} a -> s {licenses = a} :: ListReceivedLicensesForOrganizationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listReceivedLicensesForOrganizationResponse_httpStatus :: Lens.Lens' ListReceivedLicensesForOrganizationResponse Prelude.Int
listReceivedLicensesForOrganizationResponse_httpStatus = Lens.lens (\ListReceivedLicensesForOrganizationResponse' {httpStatus} -> httpStatus) (\s@ListReceivedLicensesForOrganizationResponse' {} a -> s {httpStatus = a} :: ListReceivedLicensesForOrganizationResponse)

instance
  Prelude.NFData
    ListReceivedLicensesForOrganizationResponse
  where
  rnf ListReceivedLicensesForOrganizationResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf licenses
      `Prelude.seq` Prelude.rnf httpStatus
