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
-- Module      : Amazonka.FinSpace.ListKxClusters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of clusters.
module Amazonka.FinSpace.ListKxClusters
  ( -- * Creating a Request
    ListKxClusters (..),
    newListKxClusters,

    -- * Request Lenses
    listKxClusters_clusterType,
    listKxClusters_maxResults,
    listKxClusters_nextToken,
    listKxClusters_environmentId,

    -- * Destructuring the Response
    ListKxClustersResponse (..),
    newListKxClustersResponse,

    -- * Response Lenses
    listKxClustersResponse_kxClusterSummaries,
    listKxClustersResponse_nextToken,
    listKxClustersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListKxClusters' smart constructor.
data ListKxClusters = ListKxClusters'
  { -- | Specifies the type of KDB database that is being created. The following
    -- types are available:
    --
    -- -   HDB – A Historical Database. The data is only accessible with
    --     read-only permissions from one of the FinSpace managed kdb databases
    --     mounted to the cluster.
    --
    -- -   RDB – A Realtime Database. This type of database captures all the
    --     data from a ticker plant and stores it in memory until the end of
    --     day, after which it writes all of its data to a disk and reloads the
    --     HDB. This cluster type requires local storage for temporary storage
    --     of data during the savedown process. If you specify this field in
    --     your request, you must provide the @savedownStorageConfiguration@
    --     parameter.
    --
    -- -   GATEWAY – A gateway cluster allows you to access data across
    --     processes in kdb systems. It allows you to create your own routing
    --     logic using the initialization scripts and custom code. This type of
    --     cluster does not require a writable local storage.
    clusterType :: Prelude.Maybe KxClusterType,
    -- | The maximum number of results to return in this request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKxClusters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterType', 'listKxClusters_clusterType' - Specifies the type of KDB database that is being created. The following
-- types are available:
--
-- -   HDB – A Historical Database. The data is only accessible with
--     read-only permissions from one of the FinSpace managed kdb databases
--     mounted to the cluster.
--
-- -   RDB – A Realtime Database. This type of database captures all the
--     data from a ticker plant and stores it in memory until the end of
--     day, after which it writes all of its data to a disk and reloads the
--     HDB. This cluster type requires local storage for temporary storage
--     of data during the savedown process. If you specify this field in
--     your request, you must provide the @savedownStorageConfiguration@
--     parameter.
--
-- -   GATEWAY – A gateway cluster allows you to access data across
--     processes in kdb systems. It allows you to create your own routing
--     logic using the initialization scripts and custom code. This type of
--     cluster does not require a writable local storage.
--
-- 'maxResults', 'listKxClusters_maxResults' - The maximum number of results to return in this request.
--
-- 'nextToken', 'listKxClusters_nextToken' - A token that indicates where a results page should begin.
--
-- 'environmentId', 'listKxClusters_environmentId' - A unique identifier for the kdb environment.
newListKxClusters ::
  -- | 'environmentId'
  Prelude.Text ->
  ListKxClusters
newListKxClusters pEnvironmentId_ =
  ListKxClusters'
    { clusterType = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      environmentId = pEnvironmentId_
    }

-- | Specifies the type of KDB database that is being created. The following
-- types are available:
--
-- -   HDB – A Historical Database. The data is only accessible with
--     read-only permissions from one of the FinSpace managed kdb databases
--     mounted to the cluster.
--
-- -   RDB – A Realtime Database. This type of database captures all the
--     data from a ticker plant and stores it in memory until the end of
--     day, after which it writes all of its data to a disk and reloads the
--     HDB. This cluster type requires local storage for temporary storage
--     of data during the savedown process. If you specify this field in
--     your request, you must provide the @savedownStorageConfiguration@
--     parameter.
--
-- -   GATEWAY – A gateway cluster allows you to access data across
--     processes in kdb systems. It allows you to create your own routing
--     logic using the initialization scripts and custom code. This type of
--     cluster does not require a writable local storage.
listKxClusters_clusterType :: Lens.Lens' ListKxClusters (Prelude.Maybe KxClusterType)
listKxClusters_clusterType = Lens.lens (\ListKxClusters' {clusterType} -> clusterType) (\s@ListKxClusters' {} a -> s {clusterType = a} :: ListKxClusters)

-- | The maximum number of results to return in this request.
listKxClusters_maxResults :: Lens.Lens' ListKxClusters (Prelude.Maybe Prelude.Natural)
listKxClusters_maxResults = Lens.lens (\ListKxClusters' {maxResults} -> maxResults) (\s@ListKxClusters' {} a -> s {maxResults = a} :: ListKxClusters)

-- | A token that indicates where a results page should begin.
listKxClusters_nextToken :: Lens.Lens' ListKxClusters (Prelude.Maybe Prelude.Text)
listKxClusters_nextToken = Lens.lens (\ListKxClusters' {nextToken} -> nextToken) (\s@ListKxClusters' {} a -> s {nextToken = a} :: ListKxClusters)

-- | A unique identifier for the kdb environment.
listKxClusters_environmentId :: Lens.Lens' ListKxClusters Prelude.Text
listKxClusters_environmentId = Lens.lens (\ListKxClusters' {environmentId} -> environmentId) (\s@ListKxClusters' {} a -> s {environmentId = a} :: ListKxClusters)

instance Core.AWSRequest ListKxClusters where
  type
    AWSResponse ListKxClusters =
      ListKxClustersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListKxClustersResponse'
            Prelude.<$> ( x
                            Data..?> "kxClusterSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListKxClusters where
  hashWithSalt _salt ListKxClusters' {..} =
    _salt
      `Prelude.hashWithSalt` clusterType
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` environmentId

instance Prelude.NFData ListKxClusters where
  rnf ListKxClusters' {..} =
    Prelude.rnf clusterType
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf environmentId

instance Data.ToHeaders ListKxClusters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListKxClusters where
  toPath ListKxClusters' {..} =
    Prelude.mconcat
      [ "/kx/environments/",
        Data.toBS environmentId,
        "/clusters"
      ]

instance Data.ToQuery ListKxClusters where
  toQuery ListKxClusters' {..} =
    Prelude.mconcat
      [ "clusterType" Data.=: clusterType,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListKxClustersResponse' smart constructor.
data ListKxClustersResponse = ListKxClustersResponse'
  { -- | Lists the cluster details.
    kxClusterSummaries :: Prelude.Maybe [KxCluster],
    -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKxClustersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kxClusterSummaries', 'listKxClustersResponse_kxClusterSummaries' - Lists the cluster details.
--
-- 'nextToken', 'listKxClustersResponse_nextToken' - A token that indicates where a results page should begin.
--
-- 'httpStatus', 'listKxClustersResponse_httpStatus' - The response's http status code.
newListKxClustersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListKxClustersResponse
newListKxClustersResponse pHttpStatus_ =
  ListKxClustersResponse'
    { kxClusterSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Lists the cluster details.
listKxClustersResponse_kxClusterSummaries :: Lens.Lens' ListKxClustersResponse (Prelude.Maybe [KxCluster])
listKxClustersResponse_kxClusterSummaries = Lens.lens (\ListKxClustersResponse' {kxClusterSummaries} -> kxClusterSummaries) (\s@ListKxClustersResponse' {} a -> s {kxClusterSummaries = a} :: ListKxClustersResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates where a results page should begin.
listKxClustersResponse_nextToken :: Lens.Lens' ListKxClustersResponse (Prelude.Maybe Prelude.Text)
listKxClustersResponse_nextToken = Lens.lens (\ListKxClustersResponse' {nextToken} -> nextToken) (\s@ListKxClustersResponse' {} a -> s {nextToken = a} :: ListKxClustersResponse)

-- | The response's http status code.
listKxClustersResponse_httpStatus :: Lens.Lens' ListKxClustersResponse Prelude.Int
listKxClustersResponse_httpStatus = Lens.lens (\ListKxClustersResponse' {httpStatus} -> httpStatus) (\s@ListKxClustersResponse' {} a -> s {httpStatus = a} :: ListKxClustersResponse)

instance Prelude.NFData ListKxClustersResponse where
  rnf ListKxClustersResponse' {..} =
    Prelude.rnf kxClusterSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
