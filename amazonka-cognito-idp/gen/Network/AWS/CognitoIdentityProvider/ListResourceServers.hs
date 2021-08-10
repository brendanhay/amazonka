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
-- Module      : Network.AWS.CognitoIdentityProvider.ListResourceServers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resource servers for a user pool.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.ListResourceServers
  ( -- * Creating a Request
    ListResourceServers (..),
    newListResourceServers,

    -- * Request Lenses
    listResourceServers_nextToken,
    listResourceServers_maxResults,
    listResourceServers_userPoolId,

    -- * Destructuring the Response
    ListResourceServersResponse (..),
    newListResourceServersResponse,

    -- * Response Lenses
    listResourceServersResponse_nextToken,
    listResourceServersResponse_httpStatus,
    listResourceServersResponse_resourceServers,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListResourceServers' smart constructor.
data ListResourceServers = ListResourceServers'
  { -- | A pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of resource servers to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The user pool ID for the user pool.
    userPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceServers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceServers_nextToken' - A pagination token.
--
-- 'maxResults', 'listResourceServers_maxResults' - The maximum number of resource servers to return.
--
-- 'userPoolId', 'listResourceServers_userPoolId' - The user pool ID for the user pool.
newListResourceServers ::
  -- | 'userPoolId'
  Prelude.Text ->
  ListResourceServers
newListResourceServers pUserPoolId_ =
  ListResourceServers'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      userPoolId = pUserPoolId_
    }

-- | A pagination token.
listResourceServers_nextToken :: Lens.Lens' ListResourceServers (Prelude.Maybe Prelude.Text)
listResourceServers_nextToken = Lens.lens (\ListResourceServers' {nextToken} -> nextToken) (\s@ListResourceServers' {} a -> s {nextToken = a} :: ListResourceServers)

-- | The maximum number of resource servers to return.
listResourceServers_maxResults :: Lens.Lens' ListResourceServers (Prelude.Maybe Prelude.Natural)
listResourceServers_maxResults = Lens.lens (\ListResourceServers' {maxResults} -> maxResults) (\s@ListResourceServers' {} a -> s {maxResults = a} :: ListResourceServers)

-- | The user pool ID for the user pool.
listResourceServers_userPoolId :: Lens.Lens' ListResourceServers Prelude.Text
listResourceServers_userPoolId = Lens.lens (\ListResourceServers' {userPoolId} -> userPoolId) (\s@ListResourceServers' {} a -> s {userPoolId = a} :: ListResourceServers)

instance Core.AWSPager ListResourceServers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourceServersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listResourceServersResponse_resourceServers
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listResourceServers_nextToken
          Lens..~ rs
          Lens.^? listResourceServersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListResourceServers where
  type
    AWSResponse ListResourceServers =
      ListResourceServersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceServersResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "ResourceServers"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListResourceServers

instance Prelude.NFData ListResourceServers

instance Core.ToHeaders ListResourceServers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.ListResourceServers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListResourceServers where
  toJSON ListResourceServers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("UserPoolId" Core..= userPoolId)
          ]
      )

instance Core.ToPath ListResourceServers where
  toPath = Prelude.const "/"

instance Core.ToQuery ListResourceServers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourceServersResponse' smart constructor.
data ListResourceServersResponse = ListResourceServersResponse'
  { -- | A pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The resource servers.
    resourceServers :: [ResourceServerType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceServersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceServersResponse_nextToken' - A pagination token.
--
-- 'httpStatus', 'listResourceServersResponse_httpStatus' - The response's http status code.
--
-- 'resourceServers', 'listResourceServersResponse_resourceServers' - The resource servers.
newListResourceServersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourceServersResponse
newListResourceServersResponse pHttpStatus_ =
  ListResourceServersResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      resourceServers = Prelude.mempty
    }

-- | A pagination token.
listResourceServersResponse_nextToken :: Lens.Lens' ListResourceServersResponse (Prelude.Maybe Prelude.Text)
listResourceServersResponse_nextToken = Lens.lens (\ListResourceServersResponse' {nextToken} -> nextToken) (\s@ListResourceServersResponse' {} a -> s {nextToken = a} :: ListResourceServersResponse)

-- | The response's http status code.
listResourceServersResponse_httpStatus :: Lens.Lens' ListResourceServersResponse Prelude.Int
listResourceServersResponse_httpStatus = Lens.lens (\ListResourceServersResponse' {httpStatus} -> httpStatus) (\s@ListResourceServersResponse' {} a -> s {httpStatus = a} :: ListResourceServersResponse)

-- | The resource servers.
listResourceServersResponse_resourceServers :: Lens.Lens' ListResourceServersResponse [ResourceServerType]
listResourceServersResponse_resourceServers = Lens.lens (\ListResourceServersResponse' {resourceServers} -> resourceServers) (\s@ListResourceServersResponse' {} a -> s {resourceServers = a} :: ListResourceServersResponse) Prelude.. Lens._Coerce

instance Prelude.NFData ListResourceServersResponse
