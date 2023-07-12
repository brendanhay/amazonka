{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.DocDbElastic
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2022-11-28@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The new Amazon Elastic DocumentDB service endpoint.
module Amazonka.DocDbElastic
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateCluster
    CreateCluster (CreateCluster'),
    newCreateCluster,
    CreateClusterResponse (CreateClusterResponse'),
    newCreateClusterResponse,

    -- ** CreateClusterSnapshot
    CreateClusterSnapshot (CreateClusterSnapshot'),
    newCreateClusterSnapshot,
    CreateClusterSnapshotResponse (CreateClusterSnapshotResponse'),
    newCreateClusterSnapshotResponse,

    -- ** DeleteCluster
    DeleteCluster (DeleteCluster'),
    newDeleteCluster,
    DeleteClusterResponse (DeleteClusterResponse'),
    newDeleteClusterResponse,

    -- ** DeleteClusterSnapshot
    DeleteClusterSnapshot (DeleteClusterSnapshot'),
    newDeleteClusterSnapshot,
    DeleteClusterSnapshotResponse (DeleteClusterSnapshotResponse'),
    newDeleteClusterSnapshotResponse,

    -- ** GetCluster
    GetCluster (GetCluster'),
    newGetCluster,
    GetClusterResponse (GetClusterResponse'),
    newGetClusterResponse,

    -- ** GetClusterSnapshot
    GetClusterSnapshot (GetClusterSnapshot'),
    newGetClusterSnapshot,
    GetClusterSnapshotResponse (GetClusterSnapshotResponse'),
    newGetClusterSnapshotResponse,

    -- ** ListClusterSnapshots (Paginated)
    ListClusterSnapshots (ListClusterSnapshots'),
    newListClusterSnapshots,
    ListClusterSnapshotsResponse (ListClusterSnapshotsResponse'),
    newListClusterSnapshotsResponse,

    -- ** ListClusters (Paginated)
    ListClusters (ListClusters'),
    newListClusters,
    ListClustersResponse (ListClustersResponse'),
    newListClustersResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** RestoreClusterFromSnapshot
    RestoreClusterFromSnapshot (RestoreClusterFromSnapshot'),
    newRestoreClusterFromSnapshot,
    RestoreClusterFromSnapshotResponse (RestoreClusterFromSnapshotResponse'),
    newRestoreClusterFromSnapshotResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateCluster
    UpdateCluster (UpdateCluster'),
    newUpdateCluster,
    UpdateClusterResponse (UpdateClusterResponse'),
    newUpdateClusterResponse,

    -- * Types

    -- ** Auth
    Auth (..),

    -- ** Status
    Status (..),

    -- ** Cluster
    Cluster (Cluster'),
    newCluster,

    -- ** ClusterInList
    ClusterInList (ClusterInList'),
    newClusterInList,

    -- ** ClusterSnapshot
    ClusterSnapshot (ClusterSnapshot'),
    newClusterSnapshot,

    -- ** ClusterSnapshotInList
    ClusterSnapshotInList (ClusterSnapshotInList'),
    newClusterSnapshotInList,
  )
where

import Amazonka.DocDbElastic.CreateCluster
import Amazonka.DocDbElastic.CreateClusterSnapshot
import Amazonka.DocDbElastic.DeleteCluster
import Amazonka.DocDbElastic.DeleteClusterSnapshot
import Amazonka.DocDbElastic.GetCluster
import Amazonka.DocDbElastic.GetClusterSnapshot
import Amazonka.DocDbElastic.Lens
import Amazonka.DocDbElastic.ListClusterSnapshots
import Amazonka.DocDbElastic.ListClusters
import Amazonka.DocDbElastic.ListTagsForResource
import Amazonka.DocDbElastic.RestoreClusterFromSnapshot
import Amazonka.DocDbElastic.TagResource
import Amazonka.DocDbElastic.Types
import Amazonka.DocDbElastic.UntagResource
import Amazonka.DocDbElastic.UpdateCluster
import Amazonka.DocDbElastic.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'DocDbElastic'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
