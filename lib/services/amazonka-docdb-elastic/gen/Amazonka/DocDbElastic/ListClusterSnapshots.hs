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
-- Module      : Amazonka.DocDbElastic.ListClusterSnapshots
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about Elastic DocumentDB snapshots for a specified
-- cluster.
--
-- This operation returns paginated results.
module Amazonka.DocDbElastic.ListClusterSnapshots
  ( -- * Creating a Request
    ListClusterSnapshots (..),
    newListClusterSnapshots,

    -- * Request Lenses
    listClusterSnapshots_clusterArn,
    listClusterSnapshots_maxResults,
    listClusterSnapshots_nextToken,

    -- * Destructuring the Response
    ListClusterSnapshotsResponse (..),
    newListClusterSnapshotsResponse,

    -- * Response Lenses
    listClusterSnapshotsResponse_nextToken,
    listClusterSnapshotsResponse_snapshots,
    listClusterSnapshotsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocDbElastic.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListClusterSnapshots' smart constructor.
data ListClusterSnapshots = ListClusterSnapshots'
  { -- | The arn of the Elastic DocumentDB cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of entries to recieve in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The nextToken which is used the get the next page of data.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListClusterSnapshots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'listClusterSnapshots_clusterArn' - The arn of the Elastic DocumentDB cluster.
--
-- 'maxResults', 'listClusterSnapshots_maxResults' - The maximum number of entries to recieve in the response.
--
-- 'nextToken', 'listClusterSnapshots_nextToken' - The nextToken which is used the get the next page of data.
newListClusterSnapshots ::
  ListClusterSnapshots
newListClusterSnapshots =
  ListClusterSnapshots'
    { clusterArn = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The arn of the Elastic DocumentDB cluster.
listClusterSnapshots_clusterArn :: Lens.Lens' ListClusterSnapshots (Prelude.Maybe Prelude.Text)
listClusterSnapshots_clusterArn = Lens.lens (\ListClusterSnapshots' {clusterArn} -> clusterArn) (\s@ListClusterSnapshots' {} a -> s {clusterArn = a} :: ListClusterSnapshots)

-- | The maximum number of entries to recieve in the response.
listClusterSnapshots_maxResults :: Lens.Lens' ListClusterSnapshots (Prelude.Maybe Prelude.Natural)
listClusterSnapshots_maxResults = Lens.lens (\ListClusterSnapshots' {maxResults} -> maxResults) (\s@ListClusterSnapshots' {} a -> s {maxResults = a} :: ListClusterSnapshots)

-- | The nextToken which is used the get the next page of data.
listClusterSnapshots_nextToken :: Lens.Lens' ListClusterSnapshots (Prelude.Maybe Prelude.Text)
listClusterSnapshots_nextToken = Lens.lens (\ListClusterSnapshots' {nextToken} -> nextToken) (\s@ListClusterSnapshots' {} a -> s {nextToken = a} :: ListClusterSnapshots)

instance Core.AWSPager ListClusterSnapshots where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listClusterSnapshotsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listClusterSnapshotsResponse_snapshots
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listClusterSnapshots_nextToken
              Lens..~ rs
              Lens.^? listClusterSnapshotsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListClusterSnapshots where
  type
    AWSResponse ListClusterSnapshots =
      ListClusterSnapshotsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListClusterSnapshotsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "snapshots" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListClusterSnapshots where
  hashWithSalt _salt ListClusterSnapshots' {..} =
    _salt
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListClusterSnapshots where
  rnf ListClusterSnapshots' {..} =
    Prelude.rnf clusterArn `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders ListClusterSnapshots where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListClusterSnapshots where
  toPath = Prelude.const "/cluster-snapshots"

instance Data.ToQuery ListClusterSnapshots where
  toQuery ListClusterSnapshots' {..} =
    Prelude.mconcat
      [ "clusterArn" Data.=: clusterArn,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListClusterSnapshotsResponse' smart constructor.
data ListClusterSnapshotsResponse = ListClusterSnapshotsResponse'
  { -- | The response will provide a nextToken if there is more data beyond the
    -- maxResults.
    --
    -- If there is no more data in the responce, the nextToken will not be
    -- returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of Elastic DocumentDB snapshots for a specified cluster.
    snapshots :: Prelude.Maybe [ClusterSnapshotInList],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListClusterSnapshotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listClusterSnapshotsResponse_nextToken' - The response will provide a nextToken if there is more data beyond the
-- maxResults.
--
-- If there is no more data in the responce, the nextToken will not be
-- returned.
--
-- 'snapshots', 'listClusterSnapshotsResponse_snapshots' - A list of Elastic DocumentDB snapshots for a specified cluster.
--
-- 'httpStatus', 'listClusterSnapshotsResponse_httpStatus' - The response's http status code.
newListClusterSnapshotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListClusterSnapshotsResponse
newListClusterSnapshotsResponse pHttpStatus_ =
  ListClusterSnapshotsResponse'
    { nextToken =
        Prelude.Nothing,
      snapshots = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The response will provide a nextToken if there is more data beyond the
-- maxResults.
--
-- If there is no more data in the responce, the nextToken will not be
-- returned.
listClusterSnapshotsResponse_nextToken :: Lens.Lens' ListClusterSnapshotsResponse (Prelude.Maybe Prelude.Text)
listClusterSnapshotsResponse_nextToken = Lens.lens (\ListClusterSnapshotsResponse' {nextToken} -> nextToken) (\s@ListClusterSnapshotsResponse' {} a -> s {nextToken = a} :: ListClusterSnapshotsResponse)

-- | A list of Elastic DocumentDB snapshots for a specified cluster.
listClusterSnapshotsResponse_snapshots :: Lens.Lens' ListClusterSnapshotsResponse (Prelude.Maybe [ClusterSnapshotInList])
listClusterSnapshotsResponse_snapshots = Lens.lens (\ListClusterSnapshotsResponse' {snapshots} -> snapshots) (\s@ListClusterSnapshotsResponse' {} a -> s {snapshots = a} :: ListClusterSnapshotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listClusterSnapshotsResponse_httpStatus :: Lens.Lens' ListClusterSnapshotsResponse Prelude.Int
listClusterSnapshotsResponse_httpStatus = Lens.lens (\ListClusterSnapshotsResponse' {httpStatus} -> httpStatus) (\s@ListClusterSnapshotsResponse' {} a -> s {httpStatus = a} :: ListClusterSnapshotsResponse)

instance Prelude.NFData ListClusterSnapshotsResponse where
  rnf ListClusterSnapshotsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf snapshots `Prelude.seq`
        Prelude.rnf httpStatus
