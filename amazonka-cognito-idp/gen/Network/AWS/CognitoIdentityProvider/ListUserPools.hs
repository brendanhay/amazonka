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
-- Module      : Network.AWS.CognitoIdentityProvider.ListUserPools
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the user pools associated with an AWS account.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.ListUserPools
  ( -- * Creating a Request
    ListUserPools (..),
    newListUserPools,

    -- * Request Lenses
    listUserPools_nextToken,
    listUserPools_maxResults,

    -- * Destructuring the Response
    ListUserPoolsResponse (..),
    newListUserPoolsResponse,

    -- * Response Lenses
    listUserPoolsResponse_nextToken,
    listUserPoolsResponse_userPools,
    listUserPoolsResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to list user pools.
--
-- /See:/ 'newListUserPools' smart constructor.
data ListUserPools = ListUserPools'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results you want the request to return when
    -- listing the user pools.
    maxResults :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListUserPools' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUserPools_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'maxResults', 'listUserPools_maxResults' - The maximum number of results you want the request to return when
-- listing the user pools.
newListUserPools ::
  -- | 'maxResults'
  Core.Natural ->
  ListUserPools
newListUserPools pMaxResults_ =
  ListUserPools'
    { nextToken = Core.Nothing,
      maxResults = pMaxResults_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listUserPools_nextToken :: Lens.Lens' ListUserPools (Core.Maybe Core.Text)
listUserPools_nextToken = Lens.lens (\ListUserPools' {nextToken} -> nextToken) (\s@ListUserPools' {} a -> s {nextToken = a} :: ListUserPools)

-- | The maximum number of results you want the request to return when
-- listing the user pools.
listUserPools_maxResults :: Lens.Lens' ListUserPools Core.Natural
listUserPools_maxResults = Lens.lens (\ListUserPools' {maxResults} -> maxResults) (\s@ListUserPools' {} a -> s {maxResults = a} :: ListUserPools)

instance Core.AWSPager ListUserPools where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUserPoolsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listUserPoolsResponse_userPools Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listUserPools_nextToken
          Lens..~ rs
          Lens.^? listUserPoolsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListUserPools where
  type
    AWSResponse ListUserPools =
      ListUserPoolsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUserPoolsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "UserPools" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListUserPools

instance Core.NFData ListUserPools

instance Core.ToHeaders ListUserPools where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.ListUserPools" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListUserPools where
  toJSON ListUserPools' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            Core.Just ("MaxResults" Core..= maxResults)
          ]
      )

instance Core.ToPath ListUserPools where
  toPath = Core.const "/"

instance Core.ToQuery ListUserPools where
  toQuery = Core.const Core.mempty

-- | Represents the response to list user pools.
--
-- /See:/ 'newListUserPoolsResponse' smart constructor.
data ListUserPoolsResponse = ListUserPoolsResponse'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | The user pools from the response to list users.
    userPools :: Core.Maybe [UserPoolDescriptionType],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListUserPoolsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUserPoolsResponse_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'userPools', 'listUserPoolsResponse_userPools' - The user pools from the response to list users.
--
-- 'httpStatus', 'listUserPoolsResponse_httpStatus' - The response's http status code.
newListUserPoolsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListUserPoolsResponse
newListUserPoolsResponse pHttpStatus_ =
  ListUserPoolsResponse'
    { nextToken = Core.Nothing,
      userPools = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listUserPoolsResponse_nextToken :: Lens.Lens' ListUserPoolsResponse (Core.Maybe Core.Text)
listUserPoolsResponse_nextToken = Lens.lens (\ListUserPoolsResponse' {nextToken} -> nextToken) (\s@ListUserPoolsResponse' {} a -> s {nextToken = a} :: ListUserPoolsResponse)

-- | The user pools from the response to list users.
listUserPoolsResponse_userPools :: Lens.Lens' ListUserPoolsResponse (Core.Maybe [UserPoolDescriptionType])
listUserPoolsResponse_userPools = Lens.lens (\ListUserPoolsResponse' {userPools} -> userPools) (\s@ListUserPoolsResponse' {} a -> s {userPools = a} :: ListUserPoolsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listUserPoolsResponse_httpStatus :: Lens.Lens' ListUserPoolsResponse Core.Int
listUserPoolsResponse_httpStatus = Lens.lens (\ListUserPoolsResponse' {httpStatus} -> httpStatus) (\s@ListUserPoolsResponse' {} a -> s {httpStatus = a} :: ListUserPoolsResponse)

instance Core.NFData ListUserPoolsResponse
