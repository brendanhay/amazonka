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
-- Module      : Amazonka.LicenseManager.ListReceivedLicenses
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists received licenses.
module Amazonka.LicenseManager.ListReceivedLicenses
  ( -- * Creating a Request
    ListReceivedLicenses (..),
    newListReceivedLicenses,

    -- * Request Lenses
    listReceivedLicenses_filters,
    listReceivedLicenses_licenseArns,
    listReceivedLicenses_maxResults,
    listReceivedLicenses_nextToken,

    -- * Destructuring the Response
    ListReceivedLicensesResponse (..),
    newListReceivedLicensesResponse,

    -- * Response Lenses
    listReceivedLicensesResponse_licenses,
    listReceivedLicensesResponse_nextToken,
    listReceivedLicensesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListReceivedLicenses' smart constructor.
data ListReceivedLicenses = ListReceivedLicenses'
  { -- | Filters to scope the results. The following filters are supported:
    --
    -- -   @ProductSKU@
    --
    -- -   @Status@
    --
    -- -   @Fingerprint@
    --
    -- -   @IssuerName@
    --
    -- -   @Beneficiary@
    filters :: Prelude.Maybe [Filter],
    -- | Amazon Resource Names (ARNs) of the licenses.
    licenseArns :: Prelude.Maybe [Prelude.Text],
    -- | Maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReceivedLicenses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listReceivedLicenses_filters' - Filters to scope the results. The following filters are supported:
--
-- -   @ProductSKU@
--
-- -   @Status@
--
-- -   @Fingerprint@
--
-- -   @IssuerName@
--
-- -   @Beneficiary@
--
-- 'licenseArns', 'listReceivedLicenses_licenseArns' - Amazon Resource Names (ARNs) of the licenses.
--
-- 'maxResults', 'listReceivedLicenses_maxResults' - Maximum number of results to return in a single call.
--
-- 'nextToken', 'listReceivedLicenses_nextToken' - Token for the next set of results.
newListReceivedLicenses ::
  ListReceivedLicenses
newListReceivedLicenses =
  ListReceivedLicenses'
    { filters = Prelude.Nothing,
      licenseArns = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filters to scope the results. The following filters are supported:
--
-- -   @ProductSKU@
--
-- -   @Status@
--
-- -   @Fingerprint@
--
-- -   @IssuerName@
--
-- -   @Beneficiary@
listReceivedLicenses_filters :: Lens.Lens' ListReceivedLicenses (Prelude.Maybe [Filter])
listReceivedLicenses_filters = Lens.lens (\ListReceivedLicenses' {filters} -> filters) (\s@ListReceivedLicenses' {} a -> s {filters = a} :: ListReceivedLicenses) Prelude.. Lens.mapping Lens.coerced

-- | Amazon Resource Names (ARNs) of the licenses.
listReceivedLicenses_licenseArns :: Lens.Lens' ListReceivedLicenses (Prelude.Maybe [Prelude.Text])
listReceivedLicenses_licenseArns = Lens.lens (\ListReceivedLicenses' {licenseArns} -> licenseArns) (\s@ListReceivedLicenses' {} a -> s {licenseArns = a} :: ListReceivedLicenses) Prelude.. Lens.mapping Lens.coerced

-- | Maximum number of results to return in a single call.
listReceivedLicenses_maxResults :: Lens.Lens' ListReceivedLicenses (Prelude.Maybe Prelude.Natural)
listReceivedLicenses_maxResults = Lens.lens (\ListReceivedLicenses' {maxResults} -> maxResults) (\s@ListReceivedLicenses' {} a -> s {maxResults = a} :: ListReceivedLicenses)

-- | Token for the next set of results.
listReceivedLicenses_nextToken :: Lens.Lens' ListReceivedLicenses (Prelude.Maybe Prelude.Text)
listReceivedLicenses_nextToken = Lens.lens (\ListReceivedLicenses' {nextToken} -> nextToken) (\s@ListReceivedLicenses' {} a -> s {nextToken = a} :: ListReceivedLicenses)

instance Core.AWSRequest ListReceivedLicenses where
  type
    AWSResponse ListReceivedLicenses =
      ListReceivedLicensesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReceivedLicensesResponse'
            Prelude.<$> (x Data..?> "Licenses" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListReceivedLicenses where
  hashWithSalt _salt ListReceivedLicenses' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` licenseArns
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListReceivedLicenses where
  rnf ListReceivedLicenses' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf licenseArns
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListReceivedLicenses where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.ListReceivedLicenses" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListReceivedLicenses where
  toJSON ListReceivedLicenses' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("LicenseArns" Data..=) Prelude.<$> licenseArns,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListReceivedLicenses where
  toPath = Prelude.const "/"

instance Data.ToQuery ListReceivedLicenses where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListReceivedLicensesResponse' smart constructor.
data ListReceivedLicensesResponse = ListReceivedLicensesResponse'
  { -- | Received license details.
    licenses :: Prelude.Maybe [GrantedLicense],
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReceivedLicensesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenses', 'listReceivedLicensesResponse_licenses' - Received license details.
--
-- 'nextToken', 'listReceivedLicensesResponse_nextToken' - Token for the next set of results.
--
-- 'httpStatus', 'listReceivedLicensesResponse_httpStatus' - The response's http status code.
newListReceivedLicensesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReceivedLicensesResponse
newListReceivedLicensesResponse pHttpStatus_ =
  ListReceivedLicensesResponse'
    { licenses =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Received license details.
listReceivedLicensesResponse_licenses :: Lens.Lens' ListReceivedLicensesResponse (Prelude.Maybe [GrantedLicense])
listReceivedLicensesResponse_licenses = Lens.lens (\ListReceivedLicensesResponse' {licenses} -> licenses) (\s@ListReceivedLicensesResponse' {} a -> s {licenses = a} :: ListReceivedLicensesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Token for the next set of results.
listReceivedLicensesResponse_nextToken :: Lens.Lens' ListReceivedLicensesResponse (Prelude.Maybe Prelude.Text)
listReceivedLicensesResponse_nextToken = Lens.lens (\ListReceivedLicensesResponse' {nextToken} -> nextToken) (\s@ListReceivedLicensesResponse' {} a -> s {nextToken = a} :: ListReceivedLicensesResponse)

-- | The response's http status code.
listReceivedLicensesResponse_httpStatus :: Lens.Lens' ListReceivedLicensesResponse Prelude.Int
listReceivedLicensesResponse_httpStatus = Lens.lens (\ListReceivedLicensesResponse' {httpStatus} -> httpStatus) (\s@ListReceivedLicensesResponse' {} a -> s {httpStatus = a} :: ListReceivedLicensesResponse)

instance Prelude.NFData ListReceivedLicensesResponse where
  rnf ListReceivedLicensesResponse' {..} =
    Prelude.rnf licenses
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
