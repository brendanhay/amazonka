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
-- Module      : Amazonka.EKS.ListIdentityProviderConfigs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A list of identity provider configurations.
--
-- This operation returns paginated results.
module Amazonka.EKS.ListIdentityProviderConfigs
  ( -- * Creating a Request
    ListIdentityProviderConfigs (..),
    newListIdentityProviderConfigs,

    -- * Request Lenses
    listIdentityProviderConfigs_maxResults,
    listIdentityProviderConfigs_nextToken,
    listIdentityProviderConfigs_clusterName,

    -- * Destructuring the Response
    ListIdentityProviderConfigsResponse (..),
    newListIdentityProviderConfigsResponse,

    -- * Response Lenses
    listIdentityProviderConfigsResponse_identityProviderConfigs,
    listIdentityProviderConfigsResponse_nextToken,
    listIdentityProviderConfigsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListIdentityProviderConfigs' smart constructor.
data ListIdentityProviderConfigs = ListIdentityProviderConfigs'
  { -- | The maximum number of identity provider configurations returned by
    -- @ListIdentityProviderConfigs@ in paginated output. When you use this
    -- parameter, @ListIdentityProviderConfigs@ returns only @maxResults@
    -- results in a single page along with a @nextToken@ response element. You
    -- can see the remaining results of the initial request by sending another
    -- @ListIdentityProviderConfigs@ request with the returned @nextToken@
    -- value. This value can be between 1 and 100. If you don\'t use this
    -- parameter, @ListIdentityProviderConfigs@ returns up to 100 results and a
    -- @nextToken@ value, if applicable.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ value returned from a previous paginated
    -- @IdentityProviderConfigsRequest@ where @maxResults@ was used and the
    -- results exceeded the value of that parameter. Pagination continues from
    -- the end of the previous results that returned the @nextToken@ value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The cluster name that you want to list identity provider configurations
    -- for.
    clusterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIdentityProviderConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'nextToken', 'listIdentityProviderConfigs_nextToken' - The @nextToken@ value returned from a previous paginated
-- @IdentityProviderConfigsRequest@ where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
--
-- 'clusterName', 'listIdentityProviderConfigs_clusterName' - The cluster name that you want to list identity provider configurations
-- for.
newListIdentityProviderConfigs ::
  -- | 'clusterName'
  Prelude.Text ->
  ListIdentityProviderConfigs
newListIdentityProviderConfigs pClusterName_ =
  ListIdentityProviderConfigs'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      clusterName = pClusterName_
    }

-- | The maximum number of identity provider configurations returned by
-- @ListIdentityProviderConfigs@ in paginated output. When you use this
-- parameter, @ListIdentityProviderConfigs@ returns only @maxResults@
-- results in a single page along with a @nextToken@ response element. You
-- can see the remaining results of the initial request by sending another
-- @ListIdentityProviderConfigs@ request with the returned @nextToken@
-- value. This value can be between 1 and 100. If you don\'t use this
-- parameter, @ListIdentityProviderConfigs@ returns up to 100 results and a
-- @nextToken@ value, if applicable.
listIdentityProviderConfigs_maxResults :: Lens.Lens' ListIdentityProviderConfigs (Prelude.Maybe Prelude.Natural)
listIdentityProviderConfigs_maxResults = Lens.lens (\ListIdentityProviderConfigs' {maxResults} -> maxResults) (\s@ListIdentityProviderConfigs' {} a -> s {maxResults = a} :: ListIdentityProviderConfigs)

-- | The @nextToken@ value returned from a previous paginated
-- @IdentityProviderConfigsRequest@ where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
listIdentityProviderConfigs_nextToken :: Lens.Lens' ListIdentityProviderConfigs (Prelude.Maybe Prelude.Text)
listIdentityProviderConfigs_nextToken = Lens.lens (\ListIdentityProviderConfigs' {nextToken} -> nextToken) (\s@ListIdentityProviderConfigs' {} a -> s {nextToken = a} :: ListIdentityProviderConfigs)

-- | The cluster name that you want to list identity provider configurations
-- for.
listIdentityProviderConfigs_clusterName :: Lens.Lens' ListIdentityProviderConfigs Prelude.Text
listIdentityProviderConfigs_clusterName = Lens.lens (\ListIdentityProviderConfigs' {clusterName} -> clusterName) (\s@ListIdentityProviderConfigs' {} a -> s {clusterName = a} :: ListIdentityProviderConfigs)

instance Core.AWSPager ListIdentityProviderConfigs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listIdentityProviderConfigsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listIdentityProviderConfigsResponse_identityProviderConfigs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listIdentityProviderConfigs_nextToken
          Lens..~ rs
          Lens.^? listIdentityProviderConfigsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListIdentityProviderConfigs where
  type
    AWSResponse ListIdentityProviderConfigs =
      ListIdentityProviderConfigsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIdentityProviderConfigsResponse'
            Prelude.<$> ( x
                            Data..?> "identityProviderConfigs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListIdentityProviderConfigs where
  hashWithSalt _salt ListIdentityProviderConfigs' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` clusterName

instance Prelude.NFData ListIdentityProviderConfigs where
  rnf ListIdentityProviderConfigs' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf clusterName

instance Data.ToHeaders ListIdentityProviderConfigs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListIdentityProviderConfigs where
  toPath ListIdentityProviderConfigs' {..} =
    Prelude.mconcat
      [ "/clusters/",
        Data.toBS clusterName,
        "/identity-provider-configs"
      ]

instance Data.ToQuery ListIdentityProviderConfigs where
  toQuery ListIdentityProviderConfigs' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListIdentityProviderConfigsResponse' smart constructor.
data ListIdentityProviderConfigsResponse = ListIdentityProviderConfigsResponse'
  { -- | The identity provider configurations for the cluster.
    identityProviderConfigs :: Prelude.Maybe [IdentityProviderConfig],
    -- | The @nextToken@ value returned from a previous paginated
    -- @ListIdentityProviderConfigsResponse@ where @maxResults@ was used and
    -- the results exceeded the value of that parameter. Pagination continues
    -- from the end of the previous results that returned the @nextToken@
    -- value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIdentityProviderConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityProviderConfigs', 'listIdentityProviderConfigsResponse_identityProviderConfigs' - The identity provider configurations for the cluster.
--
-- 'nextToken', 'listIdentityProviderConfigsResponse_nextToken' - The @nextToken@ value returned from a previous paginated
-- @ListIdentityProviderConfigsResponse@ where @maxResults@ was used and
-- the results exceeded the value of that parameter. Pagination continues
-- from the end of the previous results that returned the @nextToken@
-- value.
--
-- 'httpStatus', 'listIdentityProviderConfigsResponse_httpStatus' - The response's http status code.
newListIdentityProviderConfigsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListIdentityProviderConfigsResponse
newListIdentityProviderConfigsResponse pHttpStatus_ =
  ListIdentityProviderConfigsResponse'
    { identityProviderConfigs =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identity provider configurations for the cluster.
listIdentityProviderConfigsResponse_identityProviderConfigs :: Lens.Lens' ListIdentityProviderConfigsResponse (Prelude.Maybe [IdentityProviderConfig])
listIdentityProviderConfigsResponse_identityProviderConfigs = Lens.lens (\ListIdentityProviderConfigsResponse' {identityProviderConfigs} -> identityProviderConfigs) (\s@ListIdentityProviderConfigsResponse' {} a -> s {identityProviderConfigs = a} :: ListIdentityProviderConfigsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ value returned from a previous paginated
-- @ListIdentityProviderConfigsResponse@ where @maxResults@ was used and
-- the results exceeded the value of that parameter. Pagination continues
-- from the end of the previous results that returned the @nextToken@
-- value.
listIdentityProviderConfigsResponse_nextToken :: Lens.Lens' ListIdentityProviderConfigsResponse (Prelude.Maybe Prelude.Text)
listIdentityProviderConfigsResponse_nextToken = Lens.lens (\ListIdentityProviderConfigsResponse' {nextToken} -> nextToken) (\s@ListIdentityProviderConfigsResponse' {} a -> s {nextToken = a} :: ListIdentityProviderConfigsResponse)

-- | The response's http status code.
listIdentityProviderConfigsResponse_httpStatus :: Lens.Lens' ListIdentityProviderConfigsResponse Prelude.Int
listIdentityProviderConfigsResponse_httpStatus = Lens.lens (\ListIdentityProviderConfigsResponse' {httpStatus} -> httpStatus) (\s@ListIdentityProviderConfigsResponse' {} a -> s {httpStatus = a} :: ListIdentityProviderConfigsResponse)

instance
  Prelude.NFData
    ListIdentityProviderConfigsResponse
  where
  rnf ListIdentityProviderConfigsResponse' {..} =
    Prelude.rnf identityProviderConfigs
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
