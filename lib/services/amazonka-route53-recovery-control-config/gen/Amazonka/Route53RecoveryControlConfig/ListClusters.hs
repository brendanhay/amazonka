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
-- Module      : Amazonka.Route53RecoveryControlConfig.ListClusters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of all the clusters in an account.
--
-- This operation returns paginated results.
module Amazonka.Route53RecoveryControlConfig.ListClusters
  ( -- * Creating a Request
    ListClusters (..),
    newListClusters,

    -- * Request Lenses
    listClusters_maxResults,
    listClusters_nextToken,

    -- * Destructuring the Response
    ListClustersResponse (..),
    newListClustersResponse,

    -- * Response Lenses
    listClustersResponse_clusters,
    listClustersResponse_nextToken,
    listClustersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryControlConfig.Types

-- | /See:/ 'newListClusters' smart constructor.
data ListClusters = ListClusters'
  { -- | The number of objects that you want to return with this call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListClusters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listClusters_maxResults' - The number of objects that you want to return with this call.
--
-- 'nextToken', 'listClusters_nextToken' - The token that identifies which batch of results you want to see.
newListClusters ::
  ListClusters
newListClusters =
  ListClusters'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The number of objects that you want to return with this call.
listClusters_maxResults :: Lens.Lens' ListClusters (Prelude.Maybe Prelude.Natural)
listClusters_maxResults = Lens.lens (\ListClusters' {maxResults} -> maxResults) (\s@ListClusters' {} a -> s {maxResults = a} :: ListClusters)

-- | The token that identifies which batch of results you want to see.
listClusters_nextToken :: Lens.Lens' ListClusters (Prelude.Maybe Prelude.Text)
listClusters_nextToken = Lens.lens (\ListClusters' {nextToken} -> nextToken) (\s@ListClusters' {} a -> s {nextToken = a} :: ListClusters)

instance Core.AWSPager ListClusters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listClustersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listClustersResponse_clusters
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listClusters_nextToken
              Lens..~ rs
              Lens.^? listClustersResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListClusters where
  type AWSResponse ListClusters = ListClustersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListClustersResponse'
            Prelude.<$> (x Data..?> "Clusters" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListClusters where
  hashWithSalt _salt ListClusters' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListClusters where
  rnf ListClusters' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListClusters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListClusters where
  toPath = Prelude.const "/cluster"

instance Data.ToQuery ListClusters where
  toQuery ListClusters' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListClustersResponse' smart constructor.
data ListClustersResponse = ListClustersResponse'
  { -- | An array of the clusters in an account.
    clusters :: Prelude.Maybe [Cluster],
    -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListClustersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusters', 'listClustersResponse_clusters' - An array of the clusters in an account.
--
-- 'nextToken', 'listClustersResponse_nextToken' - The token that identifies which batch of results you want to see.
--
-- 'httpStatus', 'listClustersResponse_httpStatus' - The response's http status code.
newListClustersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListClustersResponse
newListClustersResponse pHttpStatus_ =
  ListClustersResponse'
    { clusters = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of the clusters in an account.
listClustersResponse_clusters :: Lens.Lens' ListClustersResponse (Prelude.Maybe [Cluster])
listClustersResponse_clusters = Lens.lens (\ListClustersResponse' {clusters} -> clusters) (\s@ListClustersResponse' {} a -> s {clusters = a} :: ListClustersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token that identifies which batch of results you want to see.
listClustersResponse_nextToken :: Lens.Lens' ListClustersResponse (Prelude.Maybe Prelude.Text)
listClustersResponse_nextToken = Lens.lens (\ListClustersResponse' {nextToken} -> nextToken) (\s@ListClustersResponse' {} a -> s {nextToken = a} :: ListClustersResponse)

-- | The response's http status code.
listClustersResponse_httpStatus :: Lens.Lens' ListClustersResponse Prelude.Int
listClustersResponse_httpStatus = Lens.lens (\ListClustersResponse' {httpStatus} -> httpStatus) (\s@ListClustersResponse' {} a -> s {httpStatus = a} :: ListClustersResponse)

instance Prelude.NFData ListClustersResponse where
  rnf ListClustersResponse' {..} =
    Prelude.rnf clusters `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
