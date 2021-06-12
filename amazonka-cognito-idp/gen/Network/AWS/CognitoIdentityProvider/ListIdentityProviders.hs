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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListIdentityProviders' smart constructor.
data ListIdentityProviders = ListIdentityProviders'
  { -- | A pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of identity providers to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | The user pool ID.
    userPoolId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ListIdentityProviders
newListIdentityProviders pUserPoolId_ =
  ListIdentityProviders'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      userPoolId = pUserPoolId_
    }

-- | A pagination token.
listIdentityProviders_nextToken :: Lens.Lens' ListIdentityProviders (Core.Maybe Core.Text)
listIdentityProviders_nextToken = Lens.lens (\ListIdentityProviders' {nextToken} -> nextToken) (\s@ListIdentityProviders' {} a -> s {nextToken = a} :: ListIdentityProviders)

-- | The maximum number of identity providers to return.
listIdentityProviders_maxResults :: Lens.Lens' ListIdentityProviders (Core.Maybe Core.Natural)
listIdentityProviders_maxResults = Lens.lens (\ListIdentityProviders' {maxResults} -> maxResults) (\s@ListIdentityProviders' {} a -> s {maxResults = a} :: ListIdentityProviders)

-- | The user pool ID.
listIdentityProviders_userPoolId :: Lens.Lens' ListIdentityProviders Core.Text
listIdentityProviders_userPoolId = Lens.lens (\ListIdentityProviders' {userPoolId} -> userPoolId) (\s@ListIdentityProviders' {} a -> s {userPoolId = a} :: ListIdentityProviders)

instance Core.AWSPager ListIdentityProviders where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listIdentityProvidersResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^. listIdentityProvidersResponse_providers) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listIdentityProviders_nextToken
          Lens..~ rs
          Lens.^? listIdentityProvidersResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListIdentityProviders where
  type
    AWSResponse ListIdentityProviders =
      ListIdentityProvidersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIdentityProvidersResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "Providers" Core..!@ Core.mempty)
      )

instance Core.Hashable ListIdentityProviders

instance Core.NFData ListIdentityProviders

instance Core.ToHeaders ListIdentityProviders where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.ListIdentityProviders" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListIdentityProviders where
  toJSON ListIdentityProviders' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("UserPoolId" Core..= userPoolId)
          ]
      )

instance Core.ToPath ListIdentityProviders where
  toPath = Core.const "/"

instance Core.ToQuery ListIdentityProviders where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListIdentityProvidersResponse' smart constructor.
data ListIdentityProvidersResponse = ListIdentityProvidersResponse'
  { -- | A pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of identity provider objects.
    providers :: [ProviderDescription]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListIdentityProvidersResponse
newListIdentityProvidersResponse pHttpStatus_ =
  ListIdentityProvidersResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      providers = Core.mempty
    }

-- | A pagination token.
listIdentityProvidersResponse_nextToken :: Lens.Lens' ListIdentityProvidersResponse (Core.Maybe Core.Text)
listIdentityProvidersResponse_nextToken = Lens.lens (\ListIdentityProvidersResponse' {nextToken} -> nextToken) (\s@ListIdentityProvidersResponse' {} a -> s {nextToken = a} :: ListIdentityProvidersResponse)

-- | The response's http status code.
listIdentityProvidersResponse_httpStatus :: Lens.Lens' ListIdentityProvidersResponse Core.Int
listIdentityProvidersResponse_httpStatus = Lens.lens (\ListIdentityProvidersResponse' {httpStatus} -> httpStatus) (\s@ListIdentityProvidersResponse' {} a -> s {httpStatus = a} :: ListIdentityProvidersResponse)

-- | A list of identity provider objects.
listIdentityProvidersResponse_providers :: Lens.Lens' ListIdentityProvidersResponse [ProviderDescription]
listIdentityProvidersResponse_providers = Lens.lens (\ListIdentityProvidersResponse' {providers} -> providers) (\s@ListIdentityProvidersResponse' {} a -> s {providers = a} :: ListIdentityProvidersResponse) Core.. Lens._Coerce

instance Core.NFData ListIdentityProvidersResponse
