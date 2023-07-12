{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DocDbElastic.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocDbElastic.Lens
  ( -- * Operations

    -- ** CreateCluster
    createCluster_clientToken,
    createCluster_kmsKeyId,
    createCluster_preferredMaintenanceWindow,
    createCluster_subnetIds,
    createCluster_tags,
    createCluster_vpcSecurityGroupIds,
    createCluster_adminUserName,
    createCluster_adminUserPassword,
    createCluster_authType,
    createCluster_clusterName,
    createCluster_shardCapacity,
    createCluster_shardCount,
    createClusterResponse_httpStatus,
    createClusterResponse_cluster,

    -- ** CreateClusterSnapshot
    createClusterSnapshot_tags,
    createClusterSnapshot_clusterArn,
    createClusterSnapshot_snapshotName,
    createClusterSnapshotResponse_httpStatus,
    createClusterSnapshotResponse_snapshot,

    -- ** DeleteCluster
    deleteCluster_clusterArn,
    deleteClusterResponse_httpStatus,
    deleteClusterResponse_cluster,

    -- ** DeleteClusterSnapshot
    deleteClusterSnapshot_snapshotArn,
    deleteClusterSnapshotResponse_httpStatus,
    deleteClusterSnapshotResponse_snapshot,

    -- ** GetCluster
    getCluster_clusterArn,
    getClusterResponse_httpStatus,
    getClusterResponse_cluster,

    -- ** GetClusterSnapshot
    getClusterSnapshot_snapshotArn,
    getClusterSnapshotResponse_httpStatus,
    getClusterSnapshotResponse_snapshot,

    -- ** ListClusterSnapshots
    listClusterSnapshots_clusterArn,
    listClusterSnapshots_maxResults,
    listClusterSnapshots_nextToken,
    listClusterSnapshotsResponse_nextToken,
    listClusterSnapshotsResponse_snapshots,
    listClusterSnapshotsResponse_httpStatus,

    -- ** ListClusters
    listClusters_maxResults,
    listClusters_nextToken,
    listClustersResponse_clusters,
    listClustersResponse_nextToken,
    listClustersResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** RestoreClusterFromSnapshot
    restoreClusterFromSnapshot_kmsKeyId,
    restoreClusterFromSnapshot_subnetIds,
    restoreClusterFromSnapshot_tags,
    restoreClusterFromSnapshot_vpcSecurityGroupIds,
    restoreClusterFromSnapshot_clusterName,
    restoreClusterFromSnapshot_snapshotArn,
    restoreClusterFromSnapshotResponse_httpStatus,
    restoreClusterFromSnapshotResponse_cluster,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateCluster
    updateCluster_adminUserPassword,
    updateCluster_authType,
    updateCluster_clientToken,
    updateCluster_preferredMaintenanceWindow,
    updateCluster_shardCapacity,
    updateCluster_shardCount,
    updateCluster_subnetIds,
    updateCluster_vpcSecurityGroupIds,
    updateCluster_clusterArn,
    updateClusterResponse_httpStatus,
    updateClusterResponse_cluster,

    -- * Types

    -- ** Cluster
    cluster_adminUserName,
    cluster_authType,
    cluster_clusterArn,
    cluster_clusterEndpoint,
    cluster_clusterName,
    cluster_createTime,
    cluster_kmsKeyId,
    cluster_preferredMaintenanceWindow,
    cluster_shardCapacity,
    cluster_shardCount,
    cluster_status,
    cluster_subnetIds,
    cluster_vpcSecurityGroupIds,

    -- ** ClusterInList
    clusterInList_clusterArn,
    clusterInList_clusterName,
    clusterInList_status,

    -- ** ClusterSnapshot
    clusterSnapshot_adminUserName,
    clusterSnapshot_clusterArn,
    clusterSnapshot_clusterCreationTime,
    clusterSnapshot_kmsKeyId,
    clusterSnapshot_snapshotArn,
    clusterSnapshot_snapshotCreationTime,
    clusterSnapshot_snapshotName,
    clusterSnapshot_status,
    clusterSnapshot_subnetIds,
    clusterSnapshot_vpcSecurityGroupIds,

    -- ** ClusterSnapshotInList
    clusterSnapshotInList_clusterArn,
    clusterSnapshotInList_snapshotArn,
    clusterSnapshotInList_snapshotCreationTime,
    clusterSnapshotInList_snapshotName,
    clusterSnapshotInList_status,
  )
where

import Amazonka.DocDbElastic.CreateCluster
import Amazonka.DocDbElastic.CreateClusterSnapshot
import Amazonka.DocDbElastic.DeleteCluster
import Amazonka.DocDbElastic.DeleteClusterSnapshot
import Amazonka.DocDbElastic.GetCluster
import Amazonka.DocDbElastic.GetClusterSnapshot
import Amazonka.DocDbElastic.ListClusterSnapshots
import Amazonka.DocDbElastic.ListClusters
import Amazonka.DocDbElastic.ListTagsForResource
import Amazonka.DocDbElastic.RestoreClusterFromSnapshot
import Amazonka.DocDbElastic.TagResource
import Amazonka.DocDbElastic.Types.Cluster
import Amazonka.DocDbElastic.Types.ClusterInList
import Amazonka.DocDbElastic.Types.ClusterSnapshot
import Amazonka.DocDbElastic.Types.ClusterSnapshotInList
import Amazonka.DocDbElastic.UntagResource
import Amazonka.DocDbElastic.UpdateCluster
