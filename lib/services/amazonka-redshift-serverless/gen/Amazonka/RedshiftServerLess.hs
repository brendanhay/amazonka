{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.RedshiftServerLess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-04-21@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This is an interface reference for Amazon Redshift Serverless. It
-- contains documentation for one of the programming or command line
-- interfaces you can use to manage Amazon Redshift Serverless.
--
-- Amazon Redshift Serverless automatically provisions data warehouse
-- capacity and intelligently scales the underlying resources based on
-- workload demands. Amazon Redshift Serverless adjusts capacity in seconds
-- to deliver consistently high performance and simplified operations for
-- even the most demanding and volatile workloads. Amazon Redshift
-- Serverless lets you focus on using your data to acquire new insights for
-- your business and customers.
--
-- To learn more about Amazon Redshift Serverless, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/serverless-whatis.html What is Amazon Redshift Serverless>.
module Amazonka.RedshiftServerLess
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InsufficientCapacityException
    _InsufficientCapacityException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** InvalidPaginationException
    _InvalidPaginationException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ConvertRecoveryPointToSnapshot
    ConvertRecoveryPointToSnapshot (ConvertRecoveryPointToSnapshot'),
    newConvertRecoveryPointToSnapshot,
    ConvertRecoveryPointToSnapshotResponse (ConvertRecoveryPointToSnapshotResponse'),
    newConvertRecoveryPointToSnapshotResponse,

    -- ** CreateEndpointAccess
    CreateEndpointAccess (CreateEndpointAccess'),
    newCreateEndpointAccess,
    CreateEndpointAccessResponse (CreateEndpointAccessResponse'),
    newCreateEndpointAccessResponse,

    -- ** CreateNamespace
    CreateNamespace (CreateNamespace'),
    newCreateNamespace,
    CreateNamespaceResponse (CreateNamespaceResponse'),
    newCreateNamespaceResponse,

    -- ** CreateSnapshot
    CreateSnapshot (CreateSnapshot'),
    newCreateSnapshot,
    CreateSnapshotResponse (CreateSnapshotResponse'),
    newCreateSnapshotResponse,

    -- ** CreateUsageLimit
    CreateUsageLimit (CreateUsageLimit'),
    newCreateUsageLimit,
    CreateUsageLimitResponse (CreateUsageLimitResponse'),
    newCreateUsageLimitResponse,

    -- ** CreateWorkgroup
    CreateWorkgroup (CreateWorkgroup'),
    newCreateWorkgroup,
    CreateWorkgroupResponse (CreateWorkgroupResponse'),
    newCreateWorkgroupResponse,

    -- ** DeleteEndpointAccess
    DeleteEndpointAccess (DeleteEndpointAccess'),
    newDeleteEndpointAccess,
    DeleteEndpointAccessResponse (DeleteEndpointAccessResponse'),
    newDeleteEndpointAccessResponse,

    -- ** DeleteNamespace
    DeleteNamespace (DeleteNamespace'),
    newDeleteNamespace,
    DeleteNamespaceResponse (DeleteNamespaceResponse'),
    newDeleteNamespaceResponse,

    -- ** DeleteResourcePolicy
    DeleteResourcePolicy (DeleteResourcePolicy'),
    newDeleteResourcePolicy,
    DeleteResourcePolicyResponse (DeleteResourcePolicyResponse'),
    newDeleteResourcePolicyResponse,

    -- ** DeleteSnapshot
    DeleteSnapshot (DeleteSnapshot'),
    newDeleteSnapshot,
    DeleteSnapshotResponse (DeleteSnapshotResponse'),
    newDeleteSnapshotResponse,

    -- ** DeleteUsageLimit
    DeleteUsageLimit (DeleteUsageLimit'),
    newDeleteUsageLimit,
    DeleteUsageLimitResponse (DeleteUsageLimitResponse'),
    newDeleteUsageLimitResponse,

    -- ** DeleteWorkgroup
    DeleteWorkgroup (DeleteWorkgroup'),
    newDeleteWorkgroup,
    DeleteWorkgroupResponse (DeleteWorkgroupResponse'),
    newDeleteWorkgroupResponse,

    -- ** GetCredentials
    GetCredentials (GetCredentials'),
    newGetCredentials,
    GetCredentialsResponse (GetCredentialsResponse'),
    newGetCredentialsResponse,

    -- ** GetEndpointAccess
    GetEndpointAccess (GetEndpointAccess'),
    newGetEndpointAccess,
    GetEndpointAccessResponse (GetEndpointAccessResponse'),
    newGetEndpointAccessResponse,

    -- ** GetNamespace
    GetNamespace (GetNamespace'),
    newGetNamespace,
    GetNamespaceResponse (GetNamespaceResponse'),
    newGetNamespaceResponse,

    -- ** GetRecoveryPoint
    GetRecoveryPoint (GetRecoveryPoint'),
    newGetRecoveryPoint,
    GetRecoveryPointResponse (GetRecoveryPointResponse'),
    newGetRecoveryPointResponse,

    -- ** GetResourcePolicy
    GetResourcePolicy (GetResourcePolicy'),
    newGetResourcePolicy,
    GetResourcePolicyResponse (GetResourcePolicyResponse'),
    newGetResourcePolicyResponse,

    -- ** GetSnapshot
    GetSnapshot (GetSnapshot'),
    newGetSnapshot,
    GetSnapshotResponse (GetSnapshotResponse'),
    newGetSnapshotResponse,

    -- ** GetTableRestoreStatus
    GetTableRestoreStatus (GetTableRestoreStatus'),
    newGetTableRestoreStatus,
    GetTableRestoreStatusResponse (GetTableRestoreStatusResponse'),
    newGetTableRestoreStatusResponse,

    -- ** GetUsageLimit
    GetUsageLimit (GetUsageLimit'),
    newGetUsageLimit,
    GetUsageLimitResponse (GetUsageLimitResponse'),
    newGetUsageLimitResponse,

    -- ** GetWorkgroup
    GetWorkgroup (GetWorkgroup'),
    newGetWorkgroup,
    GetWorkgroupResponse (GetWorkgroupResponse'),
    newGetWorkgroupResponse,

    -- ** ListEndpointAccess (Paginated)
    ListEndpointAccess (ListEndpointAccess'),
    newListEndpointAccess,
    ListEndpointAccessResponse (ListEndpointAccessResponse'),
    newListEndpointAccessResponse,

    -- ** ListNamespaces (Paginated)
    ListNamespaces (ListNamespaces'),
    newListNamespaces,
    ListNamespacesResponse (ListNamespacesResponse'),
    newListNamespacesResponse,

    -- ** ListRecoveryPoints (Paginated)
    ListRecoveryPoints (ListRecoveryPoints'),
    newListRecoveryPoints,
    ListRecoveryPointsResponse (ListRecoveryPointsResponse'),
    newListRecoveryPointsResponse,

    -- ** ListSnapshots (Paginated)
    ListSnapshots (ListSnapshots'),
    newListSnapshots,
    ListSnapshotsResponse (ListSnapshotsResponse'),
    newListSnapshotsResponse,

    -- ** ListTableRestoreStatus (Paginated)
    ListTableRestoreStatus (ListTableRestoreStatus'),
    newListTableRestoreStatus,
    ListTableRestoreStatusResponse (ListTableRestoreStatusResponse'),
    newListTableRestoreStatusResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListUsageLimits (Paginated)
    ListUsageLimits (ListUsageLimits'),
    newListUsageLimits,
    ListUsageLimitsResponse (ListUsageLimitsResponse'),
    newListUsageLimitsResponse,

    -- ** ListWorkgroups (Paginated)
    ListWorkgroups (ListWorkgroups'),
    newListWorkgroups,
    ListWorkgroupsResponse (ListWorkgroupsResponse'),
    newListWorkgroupsResponse,

    -- ** PutResourcePolicy
    PutResourcePolicy (PutResourcePolicy'),
    newPutResourcePolicy,
    PutResourcePolicyResponse (PutResourcePolicyResponse'),
    newPutResourcePolicyResponse,

    -- ** RestoreFromRecoveryPoint
    RestoreFromRecoveryPoint (RestoreFromRecoveryPoint'),
    newRestoreFromRecoveryPoint,
    RestoreFromRecoveryPointResponse (RestoreFromRecoveryPointResponse'),
    newRestoreFromRecoveryPointResponse,

    -- ** RestoreFromSnapshot
    RestoreFromSnapshot (RestoreFromSnapshot'),
    newRestoreFromSnapshot,
    RestoreFromSnapshotResponse (RestoreFromSnapshotResponse'),
    newRestoreFromSnapshotResponse,

    -- ** RestoreTableFromSnapshot
    RestoreTableFromSnapshot (RestoreTableFromSnapshot'),
    newRestoreTableFromSnapshot,
    RestoreTableFromSnapshotResponse (RestoreTableFromSnapshotResponse'),
    newRestoreTableFromSnapshotResponse,

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

    -- ** UpdateEndpointAccess
    UpdateEndpointAccess (UpdateEndpointAccess'),
    newUpdateEndpointAccess,
    UpdateEndpointAccessResponse (UpdateEndpointAccessResponse'),
    newUpdateEndpointAccessResponse,

    -- ** UpdateNamespace
    UpdateNamespace (UpdateNamespace'),
    newUpdateNamespace,
    UpdateNamespaceResponse (UpdateNamespaceResponse'),
    newUpdateNamespaceResponse,

    -- ** UpdateSnapshot
    UpdateSnapshot (UpdateSnapshot'),
    newUpdateSnapshot,
    UpdateSnapshotResponse (UpdateSnapshotResponse'),
    newUpdateSnapshotResponse,

    -- ** UpdateUsageLimit
    UpdateUsageLimit (UpdateUsageLimit'),
    newUpdateUsageLimit,
    UpdateUsageLimitResponse (UpdateUsageLimitResponse'),
    newUpdateUsageLimitResponse,

    -- ** UpdateWorkgroup
    UpdateWorkgroup (UpdateWorkgroup'),
    newUpdateWorkgroup,
    UpdateWorkgroupResponse (UpdateWorkgroupResponse'),
    newUpdateWorkgroupResponse,

    -- * Types

    -- ** LogExport
    LogExport (..),

    -- ** NamespaceStatus
    NamespaceStatus (..),

    -- ** SnapshotStatus
    SnapshotStatus (..),

    -- ** UsageLimitBreachAction
    UsageLimitBreachAction (..),

    -- ** UsageLimitPeriod
    UsageLimitPeriod (..),

    -- ** UsageLimitUsageType
    UsageLimitUsageType (..),

    -- ** WorkgroupStatus
    WorkgroupStatus (..),

    -- ** ConfigParameter
    ConfigParameter (ConfigParameter'),
    newConfigParameter,

    -- ** Endpoint
    Endpoint (Endpoint'),
    newEndpoint,

    -- ** EndpointAccess
    EndpointAccess (EndpointAccess'),
    newEndpointAccess,

    -- ** Namespace
    Namespace (Namespace'),
    newNamespace,

    -- ** NetworkInterface
    NetworkInterface (NetworkInterface'),
    newNetworkInterface,

    -- ** RecoveryPoint
    RecoveryPoint (RecoveryPoint'),
    newRecoveryPoint,

    -- ** ResourcePolicy
    ResourcePolicy (ResourcePolicy'),
    newResourcePolicy,

    -- ** Snapshot
    Snapshot (Snapshot'),
    newSnapshot,

    -- ** TableRestoreStatus
    TableRestoreStatus (TableRestoreStatus'),
    newTableRestoreStatus,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** UsageLimit
    UsageLimit (UsageLimit'),
    newUsageLimit,

    -- ** VpcEndpoint
    VpcEndpoint (VpcEndpoint'),
    newVpcEndpoint,

    -- ** VpcSecurityGroupMembership
    VpcSecurityGroupMembership (VpcSecurityGroupMembership'),
    newVpcSecurityGroupMembership,

    -- ** Workgroup
    Workgroup (Workgroup'),
    newWorkgroup,
  )
where

import Amazonka.RedshiftServerLess.ConvertRecoveryPointToSnapshot
import Amazonka.RedshiftServerLess.CreateEndpointAccess
import Amazonka.RedshiftServerLess.CreateNamespace
import Amazonka.RedshiftServerLess.CreateSnapshot
import Amazonka.RedshiftServerLess.CreateUsageLimit
import Amazonka.RedshiftServerLess.CreateWorkgroup
import Amazonka.RedshiftServerLess.DeleteEndpointAccess
import Amazonka.RedshiftServerLess.DeleteNamespace
import Amazonka.RedshiftServerLess.DeleteResourcePolicy
import Amazonka.RedshiftServerLess.DeleteSnapshot
import Amazonka.RedshiftServerLess.DeleteUsageLimit
import Amazonka.RedshiftServerLess.DeleteWorkgroup
import Amazonka.RedshiftServerLess.GetCredentials
import Amazonka.RedshiftServerLess.GetEndpointAccess
import Amazonka.RedshiftServerLess.GetNamespace
import Amazonka.RedshiftServerLess.GetRecoveryPoint
import Amazonka.RedshiftServerLess.GetResourcePolicy
import Amazonka.RedshiftServerLess.GetSnapshot
import Amazonka.RedshiftServerLess.GetTableRestoreStatus
import Amazonka.RedshiftServerLess.GetUsageLimit
import Amazonka.RedshiftServerLess.GetWorkgroup
import Amazonka.RedshiftServerLess.Lens
import Amazonka.RedshiftServerLess.ListEndpointAccess
import Amazonka.RedshiftServerLess.ListNamespaces
import Amazonka.RedshiftServerLess.ListRecoveryPoints
import Amazonka.RedshiftServerLess.ListSnapshots
import Amazonka.RedshiftServerLess.ListTableRestoreStatus
import Amazonka.RedshiftServerLess.ListTagsForResource
import Amazonka.RedshiftServerLess.ListUsageLimits
import Amazonka.RedshiftServerLess.ListWorkgroups
import Amazonka.RedshiftServerLess.PutResourcePolicy
import Amazonka.RedshiftServerLess.RestoreFromRecoveryPoint
import Amazonka.RedshiftServerLess.RestoreFromSnapshot
import Amazonka.RedshiftServerLess.RestoreTableFromSnapshot
import Amazonka.RedshiftServerLess.TagResource
import Amazonka.RedshiftServerLess.Types
import Amazonka.RedshiftServerLess.UntagResource
import Amazonka.RedshiftServerLess.UpdateEndpointAccess
import Amazonka.RedshiftServerLess.UpdateNamespace
import Amazonka.RedshiftServerLess.UpdateSnapshot
import Amazonka.RedshiftServerLess.UpdateUsageLimit
import Amazonka.RedshiftServerLess.UpdateWorkgroup
import Amazonka.RedshiftServerLess.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'RedshiftServerLess'.

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
