{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EMR.ListClusters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the status of all clusters visible to this AWS account. Allows
-- you to filter the list of clusters based on certain criteria; for
-- example, filtering by cluster creation date and time or by status. This
-- call returns a maximum of 50 clusters per call, but returns a marker to
-- track the paging of the cluster list across multiple ListClusters calls.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListClusters
  ( -- * Creating a Request
    ListClusters (..),
    newListClusters,

    -- * Request Lenses
    listClusters_createdAfter,
    listClusters_createdBefore,
    listClusters_clusterStates,
    listClusters_marker,

    -- * Destructuring the Response
    ListClustersResponse (..),
    newListClustersResponse,

    -- * Response Lenses
    listClustersResponse_clusters,
    listClustersResponse_marker,
    listClustersResponse_httpStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | This input determines how the ListClusters action filters the list of
-- clusters that it returns.
--
-- /See:/ 'newListClusters' smart constructor.
data ListClusters = ListClusters'
  { -- | The creation date and time beginning value filter for listing clusters.
    createdAfter :: Prelude.Maybe Prelude.POSIX,
    -- | The creation date and time end value filter for listing clusters.
    createdBefore :: Prelude.Maybe Prelude.POSIX,
    -- | The cluster state filters to apply when listing clusters.
    clusterStates :: Prelude.Maybe [ClusterState],
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListClusters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAfter', 'listClusters_createdAfter' - The creation date and time beginning value filter for listing clusters.
--
-- 'createdBefore', 'listClusters_createdBefore' - The creation date and time end value filter for listing clusters.
--
-- 'clusterStates', 'listClusters_clusterStates' - The cluster state filters to apply when listing clusters.
--
-- 'marker', 'listClusters_marker' - The pagination token that indicates the next set of results to retrieve.
newListClusters ::
  ListClusters
newListClusters =
  ListClusters'
    { createdAfter = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      clusterStates = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The creation date and time beginning value filter for listing clusters.
listClusters_createdAfter :: Lens.Lens' ListClusters (Prelude.Maybe Prelude.UTCTime)
listClusters_createdAfter = Lens.lens (\ListClusters' {createdAfter} -> createdAfter) (\s@ListClusters' {} a -> s {createdAfter = a} :: ListClusters) Prelude.. Lens.mapping Prelude._Time

-- | The creation date and time end value filter for listing clusters.
listClusters_createdBefore :: Lens.Lens' ListClusters (Prelude.Maybe Prelude.UTCTime)
listClusters_createdBefore = Lens.lens (\ListClusters' {createdBefore} -> createdBefore) (\s@ListClusters' {} a -> s {createdBefore = a} :: ListClusters) Prelude.. Lens.mapping Prelude._Time

-- | The cluster state filters to apply when listing clusters.
listClusters_clusterStates :: Lens.Lens' ListClusters (Prelude.Maybe [ClusterState])
listClusters_clusterStates = Lens.lens (\ListClusters' {clusterStates} -> clusterStates) (\s@ListClusters' {} a -> s {clusterStates = a} :: ListClusters) Prelude.. Lens.mapping Prelude._Coerce

-- | The pagination token that indicates the next set of results to retrieve.
listClusters_marker :: Lens.Lens' ListClusters (Prelude.Maybe Prelude.Text)
listClusters_marker = Lens.lens (\ListClusters' {marker} -> marker) (\s@ListClusters' {} a -> s {marker = a} :: ListClusters)

instance Pager.AWSPager ListClusters where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listClustersResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listClustersResponse_clusters Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listClusters_marker
          Lens..~ rs
          Lens.^? listClustersResponse_marker Prelude.. Lens._Just

instance Prelude.AWSRequest ListClusters where
  type Rs ListClusters = ListClustersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListClustersResponse'
            Prelude.<$> (x Prelude..?> "Clusters" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListClusters

instance Prelude.NFData ListClusters

instance Prelude.ToHeaders ListClusters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.ListClusters" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListClusters where
  toJSON ListClusters' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CreatedAfter" Prelude..=)
              Prelude.<$> createdAfter,
            ("CreatedBefore" Prelude..=)
              Prelude.<$> createdBefore,
            ("ClusterStates" Prelude..=)
              Prelude.<$> clusterStates,
            ("Marker" Prelude..=) Prelude.<$> marker
          ]
      )

instance Prelude.ToPath ListClusters where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListClusters where
  toQuery = Prelude.const Prelude.mempty

-- | This contains a ClusterSummaryList with the cluster details; for
-- example, the cluster IDs, names, and status.
--
-- /See:/ 'newListClustersResponse' smart constructor.
data ListClustersResponse = ListClustersResponse'
  { -- | The list of clusters for the account based on the given filters.
    clusters :: Prelude.Maybe [ClusterSummary],
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListClustersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusters', 'listClustersResponse_clusters' - The list of clusters for the account based on the given filters.
--
-- 'marker', 'listClustersResponse_marker' - The pagination token that indicates the next set of results to retrieve.
--
-- 'httpStatus', 'listClustersResponse_httpStatus' - The response's http status code.
newListClustersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListClustersResponse
newListClustersResponse pHttpStatus_ =
  ListClustersResponse'
    { clusters = Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of clusters for the account based on the given filters.
listClustersResponse_clusters :: Lens.Lens' ListClustersResponse (Prelude.Maybe [ClusterSummary])
listClustersResponse_clusters = Lens.lens (\ListClustersResponse' {clusters} -> clusters) (\s@ListClustersResponse' {} a -> s {clusters = a} :: ListClustersResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The pagination token that indicates the next set of results to retrieve.
listClustersResponse_marker :: Lens.Lens' ListClustersResponse (Prelude.Maybe Prelude.Text)
listClustersResponse_marker = Lens.lens (\ListClustersResponse' {marker} -> marker) (\s@ListClustersResponse' {} a -> s {marker = a} :: ListClustersResponse)

-- | The response's http status code.
listClustersResponse_httpStatus :: Lens.Lens' ListClustersResponse Prelude.Int
listClustersResponse_httpStatus = Lens.lens (\ListClustersResponse' {httpStatus} -> httpStatus) (\s@ListClustersResponse' {} a -> s {httpStatus = a} :: ListClustersResponse)

instance Prelude.NFData ListClustersResponse
