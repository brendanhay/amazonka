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
-- Module      : Amazonka.LicenseManagerUserSubscriptions.ListIdentityProviders
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the identity providers for user-based subscriptions.
--
-- This operation returns paginated results.
module Amazonka.LicenseManagerUserSubscriptions.ListIdentityProviders
  ( -- * Creating a Request
    ListIdentityProviders (..),
    newListIdentityProviders,

    -- * Request Lenses
    listIdentityProviders_maxResults,
    listIdentityProviders_nextToken,

    -- * Destructuring the Response
    ListIdentityProvidersResponse (..),
    newListIdentityProvidersResponse,

    -- * Response Lenses
    listIdentityProvidersResponse_nextToken,
    listIdentityProvidersResponse_httpStatus,
    listIdentityProvidersResponse_identityProviderSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManagerUserSubscriptions.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListIdentityProviders' smart constructor.
data ListIdentityProviders = ListIdentityProviders'
  { -- | Maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIdentityProviders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listIdentityProviders_maxResults' - Maximum number of results to return in a single call.
--
-- 'nextToken', 'listIdentityProviders_nextToken' - Token for the next set of results.
newListIdentityProviders ::
  ListIdentityProviders
newListIdentityProviders =
  ListIdentityProviders'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Maximum number of results to return in a single call.
listIdentityProviders_maxResults :: Lens.Lens' ListIdentityProviders (Prelude.Maybe Prelude.Int)
listIdentityProviders_maxResults = Lens.lens (\ListIdentityProviders' {maxResults} -> maxResults) (\s@ListIdentityProviders' {} a -> s {maxResults = a} :: ListIdentityProviders)

-- | Token for the next set of results.
listIdentityProviders_nextToken :: Lens.Lens' ListIdentityProviders (Prelude.Maybe Prelude.Text)
listIdentityProviders_nextToken = Lens.lens (\ListIdentityProviders' {nextToken} -> nextToken) (\s@ListIdentityProviders' {} a -> s {nextToken = a} :: ListIdentityProviders)

instance Core.AWSPager ListIdentityProviders where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listIdentityProvidersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listIdentityProvidersResponse_identityProviderSummaries
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listIdentityProviders_nextToken
          Lens..~ rs
          Lens.^? listIdentityProvidersResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListIdentityProviders where
  type
    AWSResponse ListIdentityProviders =
      ListIdentityProvidersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIdentityProvidersResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "IdentityProviderSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListIdentityProviders where
  hashWithSalt _salt ListIdentityProviders' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListIdentityProviders where
  rnf ListIdentityProviders' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListIdentityProviders where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListIdentityProviders where
  toJSON ListIdentityProviders' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListIdentityProviders where
  toPath =
    Prelude.const
      "/identity-provider/ListIdentityProviders"

instance Data.ToQuery ListIdentityProviders where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListIdentityProvidersResponse' smart constructor.
data ListIdentityProvidersResponse = ListIdentityProvidersResponse'
  { -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Metadata that describes the list identity providers operation.
    identityProviderSummaries :: [IdentityProviderSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIdentityProvidersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listIdentityProvidersResponse_nextToken' - Token for the next set of results.
--
-- 'httpStatus', 'listIdentityProvidersResponse_httpStatus' - The response's http status code.
--
-- 'identityProviderSummaries', 'listIdentityProvidersResponse_identityProviderSummaries' - Metadata that describes the list identity providers operation.
newListIdentityProvidersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListIdentityProvidersResponse
newListIdentityProvidersResponse pHttpStatus_ =
  ListIdentityProvidersResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      identityProviderSummaries = Prelude.mempty
    }

-- | Token for the next set of results.
listIdentityProvidersResponse_nextToken :: Lens.Lens' ListIdentityProvidersResponse (Prelude.Maybe Prelude.Text)
listIdentityProvidersResponse_nextToken = Lens.lens (\ListIdentityProvidersResponse' {nextToken} -> nextToken) (\s@ListIdentityProvidersResponse' {} a -> s {nextToken = a} :: ListIdentityProvidersResponse)

-- | The response's http status code.
listIdentityProvidersResponse_httpStatus :: Lens.Lens' ListIdentityProvidersResponse Prelude.Int
listIdentityProvidersResponse_httpStatus = Lens.lens (\ListIdentityProvidersResponse' {httpStatus} -> httpStatus) (\s@ListIdentityProvidersResponse' {} a -> s {httpStatus = a} :: ListIdentityProvidersResponse)

-- | Metadata that describes the list identity providers operation.
listIdentityProvidersResponse_identityProviderSummaries :: Lens.Lens' ListIdentityProvidersResponse [IdentityProviderSummary]
listIdentityProvidersResponse_identityProviderSummaries = Lens.lens (\ListIdentityProvidersResponse' {identityProviderSummaries} -> identityProviderSummaries) (\s@ListIdentityProvidersResponse' {} a -> s {identityProviderSummaries = a} :: ListIdentityProvidersResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListIdentityProvidersResponse where
  rnf ListIdentityProvidersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf identityProviderSummaries
