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
-- Module      : Network.AWS.EKS.ListIdentityProviderConfigs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A list of identity provider configurations.
--
-- This operation returns paginated results.
module Network.AWS.EKS.ListIdentityProviderConfigs
  ( -- * Creating a Request
    ListIdentityProviderConfigs (..),
    newListIdentityProviderConfigs,

    -- * Request Lenses
    listIdentityProviderConfigs_nextToken,
    listIdentityProviderConfigs_maxResults,
    listIdentityProviderConfigs_clusterName,

    -- * Destructuring the Response
    ListIdentityProviderConfigsResponse (..),
    newListIdentityProviderConfigsResponse,

    -- * Response Lenses
    listIdentityProviderConfigsResponse_nextToken,
    listIdentityProviderConfigsResponse_identityProviderConfigs,
    listIdentityProviderConfigsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListIdentityProviderConfigs' smart constructor.
data ListIdentityProviderConfigs = ListIdentityProviderConfigs'
  { -- | The @nextToken@ value returned from a previous paginated
    -- @IdentityProviderConfigsRequest@ where @maxResults@ was used and the
    -- results exceeded the value of that parameter. Pagination continues from
    -- the end of the previous results that returned the @nextToken@ value.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of identity provider configurations returned by
    -- @ListIdentityProviderConfigs@ in paginated output. When you use this
    -- parameter, @ListIdentityProviderConfigs@ returns only @maxResults@
    -- results in a single page along with a @nextToken@ response element. You
    -- can see the remaining results of the initial request by sending another
    -- @ListIdentityProviderConfigs@ request with the returned @nextToken@
    -- value. This value can be between 1 and 100. If you don\'t use this
    -- parameter, @ListIdentityProviderConfigs@ returns up to 100 results and a
    -- @nextToken@ value, if applicable.
    maxResults :: Core.Maybe Core.Natural,
    -- | The cluster name that you want to list identity provider configurations
    -- for.
    clusterName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListIdentityProviderConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listIdentityProviderConfigs_nextToken' - The @nextToken@ value returned from a previous paginated
-- @IdentityProviderConfigsRequest@ where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
--
-- 'maxResults', 'listIdentityProviderConfigs_maxResults' - The maximum number of identity provider configurations returned by
-- @ListIdentityProviderConfigs@ in paginated output. When you use this
-- parameter, @ListIdentityProviderConfigs@ returns only @maxResults@
-- results in a single page along with a @nextToken@ response element. You
-- can see the remaining results of the initial request by sending another
-- @ListIdentityProviderConfigs@ request with the returned @nextToken@
-- value. This value can be between 1 and 100. If you don\'t use this
-- parameter, @ListIdentityProviderConfigs@ returns up to 100 results and a
-- @nextToken@ value, if applicable.
--
-- 'clusterName', 'listIdentityProviderConfigs_clusterName' - The cluster name that you want to list identity provider configurations
-- for.
newListIdentityProviderConfigs ::
  -- | 'clusterName'
  Core.Text ->
  ListIdentityProviderConfigs
newListIdentityProviderConfigs pClusterName_ =
  ListIdentityProviderConfigs'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      clusterName = pClusterName_
    }

-- | The @nextToken@ value returned from a previous paginated
-- @IdentityProviderConfigsRequest@ where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
listIdentityProviderConfigs_nextToken :: Lens.Lens' ListIdentityProviderConfigs (Core.Maybe Core.Text)
listIdentityProviderConfigs_nextToken = Lens.lens (\ListIdentityProviderConfigs' {nextToken} -> nextToken) (\s@ListIdentityProviderConfigs' {} a -> s {nextToken = a} :: ListIdentityProviderConfigs)

-- | The maximum number of identity provider configurations returned by
-- @ListIdentityProviderConfigs@ in paginated output. When you use this
-- parameter, @ListIdentityProviderConfigs@ returns only @maxResults@
-- results in a single page along with a @nextToken@ response element. You
-- can see the remaining results of the initial request by sending another
-- @ListIdentityProviderConfigs@ request with the returned @nextToken@
-- value. This value can be between 1 and 100. If you don\'t use this
-- parameter, @ListIdentityProviderConfigs@ returns up to 100 results and a
-- @nextToken@ value, if applicable.
listIdentityProviderConfigs_maxResults :: Lens.Lens' ListIdentityProviderConfigs (Core.Maybe Core.Natural)
listIdentityProviderConfigs_maxResults = Lens.lens (\ListIdentityProviderConfigs' {maxResults} -> maxResults) (\s@ListIdentityProviderConfigs' {} a -> s {maxResults = a} :: ListIdentityProviderConfigs)

-- | The cluster name that you want to list identity provider configurations
-- for.
listIdentityProviderConfigs_clusterName :: Lens.Lens' ListIdentityProviderConfigs Core.Text
listIdentityProviderConfigs_clusterName = Lens.lens (\ListIdentityProviderConfigs' {clusterName} -> clusterName) (\s@ListIdentityProviderConfigs' {} a -> s {clusterName = a} :: ListIdentityProviderConfigs)

instance Core.AWSPager ListIdentityProviderConfigs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listIdentityProviderConfigsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listIdentityProviderConfigsResponse_identityProviderConfigs
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listIdentityProviderConfigs_nextToken
          Lens..~ rs
          Lens.^? listIdentityProviderConfigsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListIdentityProviderConfigs where
  type
    AWSResponse ListIdentityProviderConfigs =
      ListIdentityProviderConfigsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIdentityProviderConfigsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> ( x Core..?> "identityProviderConfigs"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListIdentityProviderConfigs

instance Core.NFData ListIdentityProviderConfigs

instance Core.ToHeaders ListIdentityProviderConfigs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListIdentityProviderConfigs where
  toPath ListIdentityProviderConfigs' {..} =
    Core.mconcat
      [ "/clusters/",
        Core.toBS clusterName,
        "/identity-provider-configs"
      ]

instance Core.ToQuery ListIdentityProviderConfigs where
  toQuery ListIdentityProviderConfigs' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListIdentityProviderConfigsResponse' smart constructor.
data ListIdentityProviderConfigsResponse = ListIdentityProviderConfigsResponse'
  { -- | The @nextToken@ value returned from a previous paginated
    -- @ListIdentityProviderConfigsResponse@ where @maxResults@ was used and
    -- the results exceeded the value of that parameter. Pagination continues
    -- from the end of the previous results that returned the @nextToken@
    -- value.
    nextToken :: Core.Maybe Core.Text,
    -- | The identity provider configurations for the cluster.
    identityProviderConfigs :: Core.Maybe [IdentityProviderConfig],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListIdentityProviderConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listIdentityProviderConfigsResponse_nextToken' - The @nextToken@ value returned from a previous paginated
-- @ListIdentityProviderConfigsResponse@ where @maxResults@ was used and
-- the results exceeded the value of that parameter. Pagination continues
-- from the end of the previous results that returned the @nextToken@
-- value.
--
-- 'identityProviderConfigs', 'listIdentityProviderConfigsResponse_identityProviderConfigs' - The identity provider configurations for the cluster.
--
-- 'httpStatus', 'listIdentityProviderConfigsResponse_httpStatus' - The response's http status code.
newListIdentityProviderConfigsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListIdentityProviderConfigsResponse
newListIdentityProviderConfigsResponse pHttpStatus_ =
  ListIdentityProviderConfigsResponse'
    { nextToken =
        Core.Nothing,
      identityProviderConfigs = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value returned from a previous paginated
-- @ListIdentityProviderConfigsResponse@ where @maxResults@ was used and
-- the results exceeded the value of that parameter. Pagination continues
-- from the end of the previous results that returned the @nextToken@
-- value.
listIdentityProviderConfigsResponse_nextToken :: Lens.Lens' ListIdentityProviderConfigsResponse (Core.Maybe Core.Text)
listIdentityProviderConfigsResponse_nextToken = Lens.lens (\ListIdentityProviderConfigsResponse' {nextToken} -> nextToken) (\s@ListIdentityProviderConfigsResponse' {} a -> s {nextToken = a} :: ListIdentityProviderConfigsResponse)

-- | The identity provider configurations for the cluster.
listIdentityProviderConfigsResponse_identityProviderConfigs :: Lens.Lens' ListIdentityProviderConfigsResponse (Core.Maybe [IdentityProviderConfig])
listIdentityProviderConfigsResponse_identityProviderConfigs = Lens.lens (\ListIdentityProviderConfigsResponse' {identityProviderConfigs} -> identityProviderConfigs) (\s@ListIdentityProviderConfigsResponse' {} a -> s {identityProviderConfigs = a} :: ListIdentityProviderConfigsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listIdentityProviderConfigsResponse_httpStatus :: Lens.Lens' ListIdentityProviderConfigsResponse Core.Int
listIdentityProviderConfigsResponse_httpStatus = Lens.lens (\ListIdentityProviderConfigsResponse' {httpStatus} -> httpStatus) (\s@ListIdentityProviderConfigsResponse' {} a -> s {httpStatus = a} :: ListIdentityProviderConfigsResponse)

instance
  Core.NFData
    ListIdentityProviderConfigsResponse
