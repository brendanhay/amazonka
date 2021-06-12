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
-- Module      : Network.AWS.CognitoIdentity.ListIdentityPools
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the Cognito identity pools registered for your account.
--
-- You must use AWS Developer credentials to call this API.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentity.ListIdentityPools
  ( -- * Creating a Request
    ListIdentityPools (..),
    newListIdentityPools,

    -- * Request Lenses
    listIdentityPools_nextToken,
    listIdentityPools_maxResults,

    -- * Destructuring the Response
    ListIdentityPoolsResponse (..),
    newListIdentityPoolsResponse,

    -- * Response Lenses
    listIdentityPoolsResponse_nextToken,
    listIdentityPoolsResponse_identityPools,
    listIdentityPoolsResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the ListIdentityPools action.
--
-- /See:/ 'newListIdentityPools' smart constructor.
data ListIdentityPools = ListIdentityPools'
  { -- | A pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of identities to return.
    maxResults :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListIdentityPools' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listIdentityPools_nextToken' - A pagination token.
--
-- 'maxResults', 'listIdentityPools_maxResults' - The maximum number of identities to return.
newListIdentityPools ::
  -- | 'maxResults'
  Core.Natural ->
  ListIdentityPools
newListIdentityPools pMaxResults_ =
  ListIdentityPools'
    { nextToken = Core.Nothing,
      maxResults = pMaxResults_
    }

-- | A pagination token.
listIdentityPools_nextToken :: Lens.Lens' ListIdentityPools (Core.Maybe Core.Text)
listIdentityPools_nextToken = Lens.lens (\ListIdentityPools' {nextToken} -> nextToken) (\s@ListIdentityPools' {} a -> s {nextToken = a} :: ListIdentityPools)

-- | The maximum number of identities to return.
listIdentityPools_maxResults :: Lens.Lens' ListIdentityPools Core.Natural
listIdentityPools_maxResults = Lens.lens (\ListIdentityPools' {maxResults} -> maxResults) (\s@ListIdentityPools' {} a -> s {maxResults = a} :: ListIdentityPools)

instance Core.AWSPager ListIdentityPools where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listIdentityPoolsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listIdentityPoolsResponse_identityPools
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listIdentityPools_nextToken
          Lens..~ rs
          Lens.^? listIdentityPoolsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListIdentityPools where
  type
    AWSResponse ListIdentityPools =
      ListIdentityPoolsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIdentityPoolsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "IdentityPools" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListIdentityPools

instance Core.NFData ListIdentityPools

instance Core.ToHeaders ListIdentityPools where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityService.ListIdentityPools" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListIdentityPools where
  toJSON ListIdentityPools' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            Core.Just ("MaxResults" Core..= maxResults)
          ]
      )

instance Core.ToPath ListIdentityPools where
  toPath = Core.const "/"

instance Core.ToQuery ListIdentityPools where
  toQuery = Core.const Core.mempty

-- | The result of a successful ListIdentityPools action.
--
-- /See:/ 'newListIdentityPoolsResponse' smart constructor.
data ListIdentityPoolsResponse = ListIdentityPoolsResponse'
  { -- | A pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The identity pools returned by the ListIdentityPools action.
    identityPools :: Core.Maybe [IdentityPoolShortDescription],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListIdentityPoolsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listIdentityPoolsResponse_nextToken' - A pagination token.
--
-- 'identityPools', 'listIdentityPoolsResponse_identityPools' - The identity pools returned by the ListIdentityPools action.
--
-- 'httpStatus', 'listIdentityPoolsResponse_httpStatus' - The response's http status code.
newListIdentityPoolsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListIdentityPoolsResponse
newListIdentityPoolsResponse pHttpStatus_ =
  ListIdentityPoolsResponse'
    { nextToken =
        Core.Nothing,
      identityPools = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token.
listIdentityPoolsResponse_nextToken :: Lens.Lens' ListIdentityPoolsResponse (Core.Maybe Core.Text)
listIdentityPoolsResponse_nextToken = Lens.lens (\ListIdentityPoolsResponse' {nextToken} -> nextToken) (\s@ListIdentityPoolsResponse' {} a -> s {nextToken = a} :: ListIdentityPoolsResponse)

-- | The identity pools returned by the ListIdentityPools action.
listIdentityPoolsResponse_identityPools :: Lens.Lens' ListIdentityPoolsResponse (Core.Maybe [IdentityPoolShortDescription])
listIdentityPoolsResponse_identityPools = Lens.lens (\ListIdentityPoolsResponse' {identityPools} -> identityPools) (\s@ListIdentityPoolsResponse' {} a -> s {identityPools = a} :: ListIdentityPoolsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listIdentityPoolsResponse_httpStatus :: Lens.Lens' ListIdentityPoolsResponse Core.Int
listIdentityPoolsResponse_httpStatus = Lens.lens (\ListIdentityPoolsResponse' {httpStatus} -> httpStatus) (\s@ListIdentityPoolsResponse' {} a -> s {httpStatus = a} :: ListIdentityPoolsResponse)

instance Core.NFData ListIdentityPoolsResponse
