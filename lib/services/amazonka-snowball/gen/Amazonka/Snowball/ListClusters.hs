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
-- Module      : Amazonka.Snowball.ListClusters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @ClusterListEntry@ objects of the specified length.
-- Each @ClusterListEntry@ object contains a cluster\'s state, a cluster\'s
-- ID, and other important status information.
--
-- This operation returns paginated results.
module Amazonka.Snowball.ListClusters
  ( -- * Creating a Request
    ListClusters (..),
    newListClusters,

    -- * Request Lenses
    listClusters_nextToken,
    listClusters_maxResults,

    -- * Destructuring the Response
    ListClustersResponse (..),
    newListClustersResponse,

    -- * Response Lenses
    listClustersResponse_nextToken,
    listClustersResponse_clusterListEntries,
    listClustersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Snowball.Types

-- | /See:/ 'newListClusters' smart constructor.
data ListClusters = ListClusters'
  { -- | HTTP requests are stateless. To identify what object comes \"next\" in
    -- the list of @ClusterListEntry@ objects, you have the option of
    -- specifying @NextToken@ as the starting point for your returned list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of @ClusterListEntry@ objects to return.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'listClusters_nextToken' - HTTP requests are stateless. To identify what object comes \"next\" in
-- the list of @ClusterListEntry@ objects, you have the option of
-- specifying @NextToken@ as the starting point for your returned list.
--
-- 'maxResults', 'listClusters_maxResults' - The number of @ClusterListEntry@ objects to return.
newListClusters ::
  ListClusters
newListClusters =
  ListClusters'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | HTTP requests are stateless. To identify what object comes \"next\" in
-- the list of @ClusterListEntry@ objects, you have the option of
-- specifying @NextToken@ as the starting point for your returned list.
listClusters_nextToken :: Lens.Lens' ListClusters (Prelude.Maybe Prelude.Text)
listClusters_nextToken = Lens.lens (\ListClusters' {nextToken} -> nextToken) (\s@ListClusters' {} a -> s {nextToken = a} :: ListClusters)

-- | The number of @ClusterListEntry@ objects to return.
listClusters_maxResults :: Lens.Lens' ListClusters (Prelude.Maybe Prelude.Natural)
listClusters_maxResults = Lens.lens (\ListClusters' {maxResults} -> maxResults) (\s@ListClusters' {} a -> s {maxResults = a} :: ListClusters)

instance Core.AWSPager ListClusters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listClustersResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listClustersResponse_clusterListEntries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listClusters_nextToken
          Lens..~ rs
          Lens.^? listClustersResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListClusters where
  type AWSResponse ListClusters = ListClustersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListClustersResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "ClusterListEntries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListClusters where
  hashWithSalt _salt ListClusters' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListClusters where
  rnf ListClusters' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListClusters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIESnowballJobManagementService.ListClusters" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListClusters where
  toJSON ListClusters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListClusters where
  toPath = Prelude.const "/"

instance Data.ToQuery ListClusters where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListClustersResponse' smart constructor.
data ListClustersResponse = ListClustersResponse'
  { -- | HTTP requests are stateless. If you use the automatically generated
    -- @NextToken@ value in your next @ClusterListEntry@ call, your list of
    -- returned clusters will start from this point in the array.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Each @ClusterListEntry@ object contains a cluster\'s state, a cluster\'s
    -- ID, and other important status information.
    clusterListEntries :: Prelude.Maybe [ClusterListEntry],
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
-- 'nextToken', 'listClustersResponse_nextToken' - HTTP requests are stateless. If you use the automatically generated
-- @NextToken@ value in your next @ClusterListEntry@ call, your list of
-- returned clusters will start from this point in the array.
--
-- 'clusterListEntries', 'listClustersResponse_clusterListEntries' - Each @ClusterListEntry@ object contains a cluster\'s state, a cluster\'s
-- ID, and other important status information.
--
-- 'httpStatus', 'listClustersResponse_httpStatus' - The response's http status code.
newListClustersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListClustersResponse
newListClustersResponse pHttpStatus_ =
  ListClustersResponse'
    { nextToken = Prelude.Nothing,
      clusterListEntries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | HTTP requests are stateless. If you use the automatically generated
-- @NextToken@ value in your next @ClusterListEntry@ call, your list of
-- returned clusters will start from this point in the array.
listClustersResponse_nextToken :: Lens.Lens' ListClustersResponse (Prelude.Maybe Prelude.Text)
listClustersResponse_nextToken = Lens.lens (\ListClustersResponse' {nextToken} -> nextToken) (\s@ListClustersResponse' {} a -> s {nextToken = a} :: ListClustersResponse)

-- | Each @ClusterListEntry@ object contains a cluster\'s state, a cluster\'s
-- ID, and other important status information.
listClustersResponse_clusterListEntries :: Lens.Lens' ListClustersResponse (Prelude.Maybe [ClusterListEntry])
listClustersResponse_clusterListEntries = Lens.lens (\ListClustersResponse' {clusterListEntries} -> clusterListEntries) (\s@ListClustersResponse' {} a -> s {clusterListEntries = a} :: ListClustersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listClustersResponse_httpStatus :: Lens.Lens' ListClustersResponse Prelude.Int
listClustersResponse_httpStatus = Lens.lens (\ListClustersResponse' {httpStatus} -> httpStatus) (\s@ListClustersResponse' {} a -> s {httpStatus = a} :: ListClustersResponse)

instance Prelude.NFData ListClustersResponse where
  rnf ListClustersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf clusterListEntries
      `Prelude.seq` Prelude.rnf httpStatus
