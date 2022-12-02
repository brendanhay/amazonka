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
-- Module      : Amazonka.CognitoIdentityProvider.ListIdentityProviders
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about all IdPs for a user pool.
--
-- This operation returns paginated results.
module Amazonka.CognitoIdentityProvider.ListIdentityProviders
  ( -- * Creating a Request
    ListIdentityProviders (..),
    newListIdentityProviders,

    -- * Request Lenses
    listIdentityProviders_nextToken,
    listIdentityProviders_maxResults,
    listIdentityProviders_userPoolId,

    -- * Destructuring the Response
    ListIdentityProvidersResponse (..),
    newListIdentityProvidersResponse,

    -- * Response Lenses
    listIdentityProvidersResponse_nextToken,
    listIdentityProvidersResponse_httpStatus,
    listIdentityProvidersResponse_providers,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListIdentityProviders' smart constructor.
data ListIdentityProviders = ListIdentityProviders'
  { -- | A pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of IdPs to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The user pool ID.
    userPoolId :: Prelude.Text
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
-- 'nextToken', 'listIdentityProviders_nextToken' - A pagination token.
--
-- 'maxResults', 'listIdentityProviders_maxResults' - The maximum number of IdPs to return.
--
-- 'userPoolId', 'listIdentityProviders_userPoolId' - The user pool ID.
newListIdentityProviders ::
  -- | 'userPoolId'
  Prelude.Text ->
  ListIdentityProviders
newListIdentityProviders pUserPoolId_ =
  ListIdentityProviders'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      userPoolId = pUserPoolId_
    }

-- | A pagination token.
listIdentityProviders_nextToken :: Lens.Lens' ListIdentityProviders (Prelude.Maybe Prelude.Text)
listIdentityProviders_nextToken = Lens.lens (\ListIdentityProviders' {nextToken} -> nextToken) (\s@ListIdentityProviders' {} a -> s {nextToken = a} :: ListIdentityProviders)

-- | The maximum number of IdPs to return.
listIdentityProviders_maxResults :: Lens.Lens' ListIdentityProviders (Prelude.Maybe Prelude.Natural)
listIdentityProviders_maxResults = Lens.lens (\ListIdentityProviders' {maxResults} -> maxResults) (\s@ListIdentityProviders' {} a -> s {maxResults = a} :: ListIdentityProviders)

-- | The user pool ID.
listIdentityProviders_userPoolId :: Lens.Lens' ListIdentityProviders Prelude.Text
listIdentityProviders_userPoolId = Lens.lens (\ListIdentityProviders' {userPoolId} -> userPoolId) (\s@ListIdentityProviders' {} a -> s {userPoolId = a} :: ListIdentityProviders)

instance Core.AWSPager ListIdentityProviders where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listIdentityProvidersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listIdentityProvidersResponse_providers) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
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
            Prelude.<*> (x Data..?> "Providers" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListIdentityProviders where
  hashWithSalt _salt ListIdentityProviders' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` userPoolId

instance Prelude.NFData ListIdentityProviders where
  rnf ListIdentityProviders' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf userPoolId

instance Data.ToHeaders ListIdentityProviders where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.ListIdentityProviders" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListIdentityProviders where
  toJSON ListIdentityProviders' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("UserPoolId" Data..= userPoolId)
          ]
      )

instance Data.ToPath ListIdentityProviders where
  toPath = Prelude.const "/"

instance Data.ToQuery ListIdentityProviders where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListIdentityProvidersResponse' smart constructor.
data ListIdentityProvidersResponse = ListIdentityProvidersResponse'
  { -- | A pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of IdP objects.
    providers :: [ProviderDescription]
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
-- 'nextToken', 'listIdentityProvidersResponse_nextToken' - A pagination token.
--
-- 'httpStatus', 'listIdentityProvidersResponse_httpStatus' - The response's http status code.
--
-- 'providers', 'listIdentityProvidersResponse_providers' - A list of IdP objects.
newListIdentityProvidersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListIdentityProvidersResponse
newListIdentityProvidersResponse pHttpStatus_ =
  ListIdentityProvidersResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      providers = Prelude.mempty
    }

-- | A pagination token.
listIdentityProvidersResponse_nextToken :: Lens.Lens' ListIdentityProvidersResponse (Prelude.Maybe Prelude.Text)
listIdentityProvidersResponse_nextToken = Lens.lens (\ListIdentityProvidersResponse' {nextToken} -> nextToken) (\s@ListIdentityProvidersResponse' {} a -> s {nextToken = a} :: ListIdentityProvidersResponse)

-- | The response's http status code.
listIdentityProvidersResponse_httpStatus :: Lens.Lens' ListIdentityProvidersResponse Prelude.Int
listIdentityProvidersResponse_httpStatus = Lens.lens (\ListIdentityProvidersResponse' {httpStatus} -> httpStatus) (\s@ListIdentityProvidersResponse' {} a -> s {httpStatus = a} :: ListIdentityProvidersResponse)

-- | A list of IdP objects.
listIdentityProvidersResponse_providers :: Lens.Lens' ListIdentityProvidersResponse [ProviderDescription]
listIdentityProvidersResponse_providers = Lens.lens (\ListIdentityProvidersResponse' {providers} -> providers) (\s@ListIdentityProvidersResponse' {} a -> s {providers = a} :: ListIdentityProvidersResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListIdentityProvidersResponse where
  rnf ListIdentityProvidersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf providers
