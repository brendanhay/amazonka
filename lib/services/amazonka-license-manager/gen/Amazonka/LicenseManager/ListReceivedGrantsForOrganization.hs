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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    listReceivedGrantsForOrganization_nextToken,
    listReceivedGrantsForOrganization_filters,
    listReceivedGrantsForOrganization_maxResults,
    listReceivedGrantsForOrganization_licenseArn,

    -- * Destructuring the Response
    ListReceivedGrantsForOrganizationResponse (..),
    newListReceivedGrantsForOrganizationResponse,

    -- * Response Lenses
    listReceivedGrantsForOrganizationResponse_nextToken,
    listReceivedGrantsForOrganizationResponse_grants,
    listReceivedGrantsForOrganizationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListReceivedGrantsForOrganization' smart constructor.
data ListReceivedGrantsForOrganization = ListReceivedGrantsForOrganization'
  { -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters to scope the results. The following filters are supported:
    --
    -- -   @ParentArn@
    --
    -- -   @GranteePrincipalArn@
    filters :: Prelude.Maybe [Filter],
    -- | Maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'nextToken', 'listReceivedGrantsForOrganization_nextToken' - Token for the next set of results.
--
-- 'filters', 'listReceivedGrantsForOrganization_filters' - Filters to scope the results. The following filters are supported:
--
-- -   @ParentArn@
--
-- -   @GranteePrincipalArn@
--
-- 'maxResults', 'listReceivedGrantsForOrganization_maxResults' - Maximum number of results to return in a single call.
--
-- 'licenseArn', 'listReceivedGrantsForOrganization_licenseArn' - The Amazon Resource Name (ARN) of the received license.
newListReceivedGrantsForOrganization ::
  -- | 'licenseArn'
  Prelude.Text ->
  ListReceivedGrantsForOrganization
newListReceivedGrantsForOrganization pLicenseArn_ =
  ListReceivedGrantsForOrganization'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      licenseArn = pLicenseArn_
    }

-- | Token for the next set of results.
listReceivedGrantsForOrganization_nextToken :: Lens.Lens' ListReceivedGrantsForOrganization (Prelude.Maybe Prelude.Text)
listReceivedGrantsForOrganization_nextToken = Lens.lens (\ListReceivedGrantsForOrganization' {nextToken} -> nextToken) (\s@ListReceivedGrantsForOrganization' {} a -> s {nextToken = a} :: ListReceivedGrantsForOrganization)

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
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> (x Core..?> "Grants" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListReceivedGrantsForOrganization
  where
  hashWithSalt
    _salt
    ListReceivedGrantsForOrganization' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` licenseArn

instance
  Prelude.NFData
    ListReceivedGrantsForOrganization
  where
  rnf ListReceivedGrantsForOrganization' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf licenseArn

instance
  Core.ToHeaders
    ListReceivedGrantsForOrganization
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLicenseManager.ListReceivedGrantsForOrganization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    ListReceivedGrantsForOrganization
  where
  toJSON ListReceivedGrantsForOrganization' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Filters" Core..=) Prelude.<$> filters,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("LicenseArn" Core..= licenseArn)
          ]
      )

instance
  Core.ToPath
    ListReceivedGrantsForOrganization
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    ListReceivedGrantsForOrganization
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListReceivedGrantsForOrganizationResponse' smart constructor.
data ListReceivedGrantsForOrganizationResponse = ListReceivedGrantsForOrganizationResponse'
  { -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Lists the grants the organization has received.
    grants :: Prelude.Maybe [Grant],
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
-- 'nextToken', 'listReceivedGrantsForOrganizationResponse_nextToken' - Token for the next set of results.
--
-- 'grants', 'listReceivedGrantsForOrganizationResponse_grants' - Lists the grants the organization has received.
--
-- 'httpStatus', 'listReceivedGrantsForOrganizationResponse_httpStatus' - The response's http status code.
newListReceivedGrantsForOrganizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReceivedGrantsForOrganizationResponse
newListReceivedGrantsForOrganizationResponse
  pHttpStatus_ =
    ListReceivedGrantsForOrganizationResponse'
      { nextToken =
          Prelude.Nothing,
        grants = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Token for the next set of results.
listReceivedGrantsForOrganizationResponse_nextToken :: Lens.Lens' ListReceivedGrantsForOrganizationResponse (Prelude.Maybe Prelude.Text)
listReceivedGrantsForOrganizationResponse_nextToken = Lens.lens (\ListReceivedGrantsForOrganizationResponse' {nextToken} -> nextToken) (\s@ListReceivedGrantsForOrganizationResponse' {} a -> s {nextToken = a} :: ListReceivedGrantsForOrganizationResponse)

-- | Lists the grants the organization has received.
listReceivedGrantsForOrganizationResponse_grants :: Lens.Lens' ListReceivedGrantsForOrganizationResponse (Prelude.Maybe [Grant])
listReceivedGrantsForOrganizationResponse_grants = Lens.lens (\ListReceivedGrantsForOrganizationResponse' {grants} -> grants) (\s@ListReceivedGrantsForOrganizationResponse' {} a -> s {grants = a} :: ListReceivedGrantsForOrganizationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listReceivedGrantsForOrganizationResponse_httpStatus :: Lens.Lens' ListReceivedGrantsForOrganizationResponse Prelude.Int
listReceivedGrantsForOrganizationResponse_httpStatus = Lens.lens (\ListReceivedGrantsForOrganizationResponse' {httpStatus} -> httpStatus) (\s@ListReceivedGrantsForOrganizationResponse' {} a -> s {httpStatus = a} :: ListReceivedGrantsForOrganizationResponse)

instance
  Prelude.NFData
    ListReceivedGrantsForOrganizationResponse
  where
  rnf ListReceivedGrantsForOrganizationResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf grants
      `Prelude.seq` Prelude.rnf httpStatus
