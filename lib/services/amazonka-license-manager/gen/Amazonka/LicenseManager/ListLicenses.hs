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
-- Module      : Amazonka.LicenseManager.ListLicenses
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the licenses for your account.
module Amazonka.LicenseManager.ListLicenses
  ( -- * Creating a Request
    ListLicenses (..),
    newListLicenses,

    -- * Request Lenses
    listLicenses_filters,
    listLicenses_licenseArns,
    listLicenses_maxResults,
    listLicenses_nextToken,

    -- * Destructuring the Response
    ListLicensesResponse (..),
    newListLicensesResponse,

    -- * Response Lenses
    listLicensesResponse_licenses,
    listLicensesResponse_nextToken,
    listLicensesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLicenses' smart constructor.
data ListLicenses = ListLicenses'
  { -- | Filters to scope the results. The following filters are supported:
    --
    -- -   @Beneficiary@
    --
    -- -   @ProductSKU@
    --
    -- -   @Fingerprint@
    --
    -- -   @Status@
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
-- Create a value of 'ListLicenses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listLicenses_filters' - Filters to scope the results. The following filters are supported:
--
-- -   @Beneficiary@
--
-- -   @ProductSKU@
--
-- -   @Fingerprint@
--
-- -   @Status@
--
-- 'licenseArns', 'listLicenses_licenseArns' - Amazon Resource Names (ARNs) of the licenses.
--
-- 'maxResults', 'listLicenses_maxResults' - Maximum number of results to return in a single call.
--
-- 'nextToken', 'listLicenses_nextToken' - Token for the next set of results.
newListLicenses ::
  ListLicenses
newListLicenses =
  ListLicenses'
    { filters = Prelude.Nothing,
      licenseArns = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filters to scope the results. The following filters are supported:
--
-- -   @Beneficiary@
--
-- -   @ProductSKU@
--
-- -   @Fingerprint@
--
-- -   @Status@
listLicenses_filters :: Lens.Lens' ListLicenses (Prelude.Maybe [Filter])
listLicenses_filters = Lens.lens (\ListLicenses' {filters} -> filters) (\s@ListLicenses' {} a -> s {filters = a} :: ListLicenses) Prelude.. Lens.mapping Lens.coerced

-- | Amazon Resource Names (ARNs) of the licenses.
listLicenses_licenseArns :: Lens.Lens' ListLicenses (Prelude.Maybe [Prelude.Text])
listLicenses_licenseArns = Lens.lens (\ListLicenses' {licenseArns} -> licenseArns) (\s@ListLicenses' {} a -> s {licenseArns = a} :: ListLicenses) Prelude.. Lens.mapping Lens.coerced

-- | Maximum number of results to return in a single call.
listLicenses_maxResults :: Lens.Lens' ListLicenses (Prelude.Maybe Prelude.Natural)
listLicenses_maxResults = Lens.lens (\ListLicenses' {maxResults} -> maxResults) (\s@ListLicenses' {} a -> s {maxResults = a} :: ListLicenses)

-- | Token for the next set of results.
listLicenses_nextToken :: Lens.Lens' ListLicenses (Prelude.Maybe Prelude.Text)
listLicenses_nextToken = Lens.lens (\ListLicenses' {nextToken} -> nextToken) (\s@ListLicenses' {} a -> s {nextToken = a} :: ListLicenses)

instance Core.AWSRequest ListLicenses where
  type AWSResponse ListLicenses = ListLicensesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLicensesResponse'
            Prelude.<$> (x Data..?> "Licenses" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLicenses where
  hashWithSalt _salt ListLicenses' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` licenseArns
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListLicenses where
  rnf ListLicenses' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf licenseArns
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListLicenses where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.ListLicenses" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListLicenses where
  toJSON ListLicenses' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("LicenseArns" Data..=) Prelude.<$> licenseArns,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListLicenses where
  toPath = Prelude.const "/"

instance Data.ToQuery ListLicenses where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLicensesResponse' smart constructor.
data ListLicensesResponse = ListLicensesResponse'
  { -- | License details.
    licenses :: Prelude.Maybe [License],
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLicensesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenses', 'listLicensesResponse_licenses' - License details.
--
-- 'nextToken', 'listLicensesResponse_nextToken' - Token for the next set of results.
--
-- 'httpStatus', 'listLicensesResponse_httpStatus' - The response's http status code.
newListLicensesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLicensesResponse
newListLicensesResponse pHttpStatus_ =
  ListLicensesResponse'
    { licenses = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | License details.
listLicensesResponse_licenses :: Lens.Lens' ListLicensesResponse (Prelude.Maybe [License])
listLicensesResponse_licenses = Lens.lens (\ListLicensesResponse' {licenses} -> licenses) (\s@ListLicensesResponse' {} a -> s {licenses = a} :: ListLicensesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Token for the next set of results.
listLicensesResponse_nextToken :: Lens.Lens' ListLicensesResponse (Prelude.Maybe Prelude.Text)
listLicensesResponse_nextToken = Lens.lens (\ListLicensesResponse' {nextToken} -> nextToken) (\s@ListLicensesResponse' {} a -> s {nextToken = a} :: ListLicensesResponse)

-- | The response's http status code.
listLicensesResponse_httpStatus :: Lens.Lens' ListLicensesResponse Prelude.Int
listLicensesResponse_httpStatus = Lens.lens (\ListLicensesResponse' {httpStatus} -> httpStatus) (\s@ListLicensesResponse' {} a -> s {httpStatus = a} :: ListLicensesResponse)

instance Prelude.NFData ListLicensesResponse where
  rnf ListLicensesResponse' {..} =
    Prelude.rnf licenses
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
