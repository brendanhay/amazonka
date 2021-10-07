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
-- Module      : Network.AWS.CognitoIdentityProvider.ListIdentityProviders
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about all identity providers for a user pool.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.ListIdentityProviders
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

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListIdentityProviders' smart constructor.
data ListIdentityProviders = ListIdentityProviders'
  { -- | A pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of identity providers to return.
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
-- 'maxResults', 'listIdentityProviders_maxResults' - The maximum number of identity providers to return.
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

-- | The maximum number of identity providers to return.
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIdentityProvidersResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "Providers" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListIdentityProviders

instance Prelude.NFData ListIdentityProviders

instance Core.ToHeaders ListIdentityProviders where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.ListIdentityProviders" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListIdentityProviders where
  toJSON ListIdentityProviders' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("UserPoolId" Core..= userPoolId)
          ]
      )

instance Core.ToPath ListIdentityProviders where
  toPath = Prelude.const "/"

instance Core.ToQuery ListIdentityProviders where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListIdentityProvidersResponse' smart constructor.
data ListIdentityProvidersResponse = ListIdentityProvidersResponse'
  { -- | A pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of identity provider objects.
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
-- 'providers', 'listIdentityProvidersResponse_providers' - A list of identity provider objects.
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

-- | A list of identity provider objects.
listIdentityProvidersResponse_providers :: Lens.Lens' ListIdentityProvidersResponse [ProviderDescription]
listIdentityProvidersResponse_providers = Lens.lens (\ListIdentityProvidersResponse' {providers} -> providers) (\s@ListIdentityProvidersResponse' {} a -> s {providers = a} :: ListIdentityProvidersResponse) Prelude.. Lens._Coerce

instance Prelude.NFData ListIdentityProvidersResponse
