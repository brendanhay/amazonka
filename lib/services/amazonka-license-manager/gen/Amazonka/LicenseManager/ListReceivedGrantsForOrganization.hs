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
-- Module      : Amazonka.LicenseManager.ListReceivedGrantsForOrganization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the grants received for all accounts in the organization.
module Amazonka.LicenseManager.ListReceivedGrantsForOrganization
  ( -- * Creating a Request
    ListReceivedGrantsForOrganization (..),
    newListReceivedGrantsForOrganization,

    -- * Request Lenses
    listReceivedGrantsForOrganization_filters,
    listReceivedGrantsForOrganization_maxResults,
    listReceivedGrantsForOrganization_nextToken,
    listReceivedGrantsForOrganization_licenseArn,

    -- * Destructuring the Response
    ListReceivedGrantsForOrganizationResponse (..),
    newListReceivedGrantsForOrganizationResponse,

    -- * Response Lenses
    listReceivedGrantsForOrganizationResponse_grants,
    listReceivedGrantsForOrganizationResponse_nextToken,
    listReceivedGrantsForOrganizationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListReceivedGrantsForOrganization' smart constructor.
data ListReceivedGrantsForOrganization = ListReceivedGrantsForOrganization'
  { -- | Filters to scope the results. The following filters are supported:
    --
    -- -   @ParentArn@
    --
    -- -   @GranteePrincipalArn@
    filters :: Prelude.Maybe [Filter],
    -- | Maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the received license.
    licenseArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReceivedGrantsForOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listReceivedGrantsForOrganization_filters' - Filters to scope the results. The following filters are supported:
--
-- -   @ParentArn@
--
-- -   @GranteePrincipalArn@
--
-- 'maxResults', 'listReceivedGrantsForOrganization_maxResults' - Maximum number of results to return in a single call.
--
-- 'nextToken', 'listReceivedGrantsForOrganization_nextToken' - Token for the next set of results.
--
-- 'licenseArn', 'listReceivedGrantsForOrganization_licenseArn' - The Amazon Resource Name (ARN) of the received license.
newListReceivedGrantsForOrganization ::
  -- | 'licenseArn'
  Prelude.Text ->
  ListReceivedGrantsForOrganization
newListReceivedGrantsForOrganization pLicenseArn_ =
  ListReceivedGrantsForOrganization'
    { filters =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      licenseArn = pLicenseArn_
    }

-- | Filters to scope the results. The following filters are supported:
--
-- -   @ParentArn@
--
-- -   @GranteePrincipalArn@
listReceivedGrantsForOrganization_filters :: Lens.Lens' ListReceivedGrantsForOrganization (Prelude.Maybe [Filter])
listReceivedGrantsForOrganization_filters = Lens.lens (\ListReceivedGrantsForOrganization' {filters} -> filters) (\s@ListReceivedGrantsForOrganization' {} a -> s {filters = a} :: ListReceivedGrantsForOrganization) Prelude.. Lens.mapping Lens.coerced

-- | Maximum number of results to return in a single call.
listReceivedGrantsForOrganization_maxResults :: Lens.Lens' ListReceivedGrantsForOrganization (Prelude.Maybe Prelude.Natural)
listReceivedGrantsForOrganization_maxResults = Lens.lens (\ListReceivedGrantsForOrganization' {maxResults} -> maxResults) (\s@ListReceivedGrantsForOrganization' {} a -> s {maxResults = a} :: ListReceivedGrantsForOrganization)

-- | Token for the next set of results.
listReceivedGrantsForOrganization_nextToken :: Lens.Lens' ListReceivedGrantsForOrganization (Prelude.Maybe Prelude.Text)
listReceivedGrantsForOrganization_nextToken = Lens.lens (\ListReceivedGrantsForOrganization' {nextToken} -> nextToken) (\s@ListReceivedGrantsForOrganization' {} a -> s {nextToken = a} :: ListReceivedGrantsForOrganization)

-- | The Amazon Resource Name (ARN) of the received license.
listReceivedGrantsForOrganization_licenseArn :: Lens.Lens' ListReceivedGrantsForOrganization Prelude.Text
listReceivedGrantsForOrganization_licenseArn = Lens.lens (\ListReceivedGrantsForOrganization' {licenseArn} -> licenseArn) (\s@ListReceivedGrantsForOrganization' {} a -> s {licenseArn = a} :: ListReceivedGrantsForOrganization)

instance
  Core.AWSRequest
    ListReceivedGrantsForOrganization
  where
  type
    AWSResponse ListReceivedGrantsForOrganization =
      ListReceivedGrantsForOrganizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReceivedGrantsForOrganizationResponse'
            Prelude.<$> (x Data..?> "Grants" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListReceivedGrantsForOrganization
  where
  hashWithSalt
    _salt
    ListReceivedGrantsForOrganization' {..} =
      _salt
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` licenseArn

instance
  Prelude.NFData
    ListReceivedGrantsForOrganization
  where
  rnf ListReceivedGrantsForOrganization' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf licenseArn

instance
  Data.ToHeaders
    ListReceivedGrantsForOrganization
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.ListReceivedGrantsForOrganization" ::
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
    ListReceivedGrantsForOrganization
  where
  toJSON ListReceivedGrantsForOrganization' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("LicenseArn" Data..= licenseArn)
          ]
      )

instance
  Data.ToPath
    ListReceivedGrantsForOrganization
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListReceivedGrantsForOrganization
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListReceivedGrantsForOrganizationResponse' smart constructor.
data ListReceivedGrantsForOrganizationResponse = ListReceivedGrantsForOrganizationResponse'
  { -- | Lists the grants the organization has received.
    grants :: Prelude.Maybe [Grant],
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReceivedGrantsForOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grants', 'listReceivedGrantsForOrganizationResponse_grants' - Lists the grants the organization has received.
--
-- 'nextToken', 'listReceivedGrantsForOrganizationResponse_nextToken' - Token for the next set of results.
--
-- 'httpStatus', 'listReceivedGrantsForOrganizationResponse_httpStatus' - The response's http status code.
newListReceivedGrantsForOrganizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReceivedGrantsForOrganizationResponse
newListReceivedGrantsForOrganizationResponse
  pHttpStatus_ =
    ListReceivedGrantsForOrganizationResponse'
      { grants =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Lists the grants the organization has received.
listReceivedGrantsForOrganizationResponse_grants :: Lens.Lens' ListReceivedGrantsForOrganizationResponse (Prelude.Maybe [Grant])
listReceivedGrantsForOrganizationResponse_grants = Lens.lens (\ListReceivedGrantsForOrganizationResponse' {grants} -> grants) (\s@ListReceivedGrantsForOrganizationResponse' {} a -> s {grants = a} :: ListReceivedGrantsForOrganizationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Token for the next set of results.
listReceivedGrantsForOrganizationResponse_nextToken :: Lens.Lens' ListReceivedGrantsForOrganizationResponse (Prelude.Maybe Prelude.Text)
listReceivedGrantsForOrganizationResponse_nextToken = Lens.lens (\ListReceivedGrantsForOrganizationResponse' {nextToken} -> nextToken) (\s@ListReceivedGrantsForOrganizationResponse' {} a -> s {nextToken = a} :: ListReceivedGrantsForOrganizationResponse)

-- | The response's http status code.
listReceivedGrantsForOrganizationResponse_httpStatus :: Lens.Lens' ListReceivedGrantsForOrganizationResponse Prelude.Int
listReceivedGrantsForOrganizationResponse_httpStatus = Lens.lens (\ListReceivedGrantsForOrganizationResponse' {httpStatus} -> httpStatus) (\s@ListReceivedGrantsForOrganizationResponse' {} a -> s {httpStatus = a} :: ListReceivedGrantsForOrganizationResponse)

instance
  Prelude.NFData
    ListReceivedGrantsForOrganizationResponse
  where
  rnf ListReceivedGrantsForOrganizationResponse' {..} =
    Prelude.rnf grants
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
