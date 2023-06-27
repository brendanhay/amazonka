{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.FinSpace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-03-12@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The FinSpace management service provides the APIs for managing FinSpace
-- environments.
module Amazonka.FinSpace
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

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

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

    -- ** CreateEnvironment
    CreateEnvironment (CreateEnvironment'),
    newCreateEnvironment,
    CreateEnvironmentResponse (CreateEnvironmentResponse'),
    newCreateEnvironmentResponse,

    -- ** CreateKxChangeset
    CreateKxChangeset (CreateKxChangeset'),
    newCreateKxChangeset,
    CreateKxChangesetResponse (CreateKxChangesetResponse'),
    newCreateKxChangesetResponse,

    -- ** CreateKxCluster
    CreateKxCluster (CreateKxCluster'),
    newCreateKxCluster,
    CreateKxClusterResponse (CreateKxClusterResponse'),
    newCreateKxClusterResponse,

    -- ** CreateKxDatabase
    CreateKxDatabase (CreateKxDatabase'),
    newCreateKxDatabase,
    CreateKxDatabaseResponse (CreateKxDatabaseResponse'),
    newCreateKxDatabaseResponse,

    -- ** CreateKxEnvironment
    CreateKxEnvironment (CreateKxEnvironment'),
    newCreateKxEnvironment,
    CreateKxEnvironmentResponse (CreateKxEnvironmentResponse'),
    newCreateKxEnvironmentResponse,

    -- ** CreateKxUser
    CreateKxUser (CreateKxUser'),
    newCreateKxUser,
    CreateKxUserResponse (CreateKxUserResponse'),
    newCreateKxUserResponse,

    -- ** DeleteEnvironment
    DeleteEnvironment (DeleteEnvironment'),
    newDeleteEnvironment,
    DeleteEnvironmentResponse (DeleteEnvironmentResponse'),
    newDeleteEnvironmentResponse,

    -- ** DeleteKxCluster
    DeleteKxCluster (DeleteKxCluster'),
    newDeleteKxCluster,
    DeleteKxClusterResponse (DeleteKxClusterResponse'),
    newDeleteKxClusterResponse,

    -- ** DeleteKxDatabase
    DeleteKxDatabase (DeleteKxDatabase'),
    newDeleteKxDatabase,
    DeleteKxDatabaseResponse (DeleteKxDatabaseResponse'),
    newDeleteKxDatabaseResponse,

    -- ** DeleteKxEnvironment
    DeleteKxEnvironment (DeleteKxEnvironment'),
    newDeleteKxEnvironment,
    DeleteKxEnvironmentResponse (DeleteKxEnvironmentResponse'),
    newDeleteKxEnvironmentResponse,

    -- ** DeleteKxUser
    DeleteKxUser (DeleteKxUser'),
    newDeleteKxUser,
    DeleteKxUserResponse (DeleteKxUserResponse'),
    newDeleteKxUserResponse,

    -- ** GetEnvironment
    GetEnvironment (GetEnvironment'),
    newGetEnvironment,
    GetEnvironmentResponse (GetEnvironmentResponse'),
    newGetEnvironmentResponse,

    -- ** GetKxChangeset
    GetKxChangeset (GetKxChangeset'),
    newGetKxChangeset,
    GetKxChangesetResponse (GetKxChangesetResponse'),
    newGetKxChangesetResponse,

    -- ** GetKxCluster
    GetKxCluster (GetKxCluster'),
    newGetKxCluster,
    GetKxClusterResponse (GetKxClusterResponse'),
    newGetKxClusterResponse,

    -- ** GetKxConnectionString
    GetKxConnectionString (GetKxConnectionString'),
    newGetKxConnectionString,
    GetKxConnectionStringResponse (GetKxConnectionStringResponse'),
    newGetKxConnectionStringResponse,

    -- ** GetKxDatabase
    GetKxDatabase (GetKxDatabase'),
    newGetKxDatabase,
    GetKxDatabaseResponse (GetKxDatabaseResponse'),
    newGetKxDatabaseResponse,

    -- ** GetKxEnvironment
    GetKxEnvironment (GetKxEnvironment'),
    newGetKxEnvironment,
    GetKxEnvironmentResponse (GetKxEnvironmentResponse'),
    newGetKxEnvironmentResponse,

    -- ** GetKxUser
    GetKxUser (GetKxUser'),
    newGetKxUser,
    GetKxUserResponse (GetKxUserResponse'),
    newGetKxUserResponse,

    -- ** ListEnvironments
    ListEnvironments (ListEnvironments'),
    newListEnvironments,
    ListEnvironmentsResponse (ListEnvironmentsResponse'),
    newListEnvironmentsResponse,

    -- ** ListKxChangesets
    ListKxChangesets (ListKxChangesets'),
    newListKxChangesets,
    ListKxChangesetsResponse (ListKxChangesetsResponse'),
    newListKxChangesetsResponse,

    -- ** ListKxClusterNodes
    ListKxClusterNodes (ListKxClusterNodes'),
    newListKxClusterNodes,
    ListKxClusterNodesResponse (ListKxClusterNodesResponse'),
    newListKxClusterNodesResponse,

    -- ** ListKxClusters
    ListKxClusters (ListKxClusters'),
    newListKxClusters,
    ListKxClustersResponse (ListKxClustersResponse'),
    newListKxClustersResponse,

    -- ** ListKxDatabases
    ListKxDatabases (ListKxDatabases'),
    newListKxDatabases,
    ListKxDatabasesResponse (ListKxDatabasesResponse'),
    newListKxDatabasesResponse,

    -- ** ListKxEnvironments (Paginated)
    ListKxEnvironments (ListKxEnvironments'),
    newListKxEnvironments,
    ListKxEnvironmentsResponse (ListKxEnvironmentsResponse'),
    newListKxEnvironmentsResponse,

    -- ** ListKxUsers
    ListKxUsers (ListKxUsers'),
    newListKxUsers,
    ListKxUsersResponse (ListKxUsersResponse'),
    newListKxUsersResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

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

    -- ** UpdateEnvironment
    UpdateEnvironment (UpdateEnvironment'),
    newUpdateEnvironment,
    UpdateEnvironmentResponse (UpdateEnvironmentResponse'),
    newUpdateEnvironmentResponse,

    -- ** UpdateKxClusterDatabases
    UpdateKxClusterDatabases (UpdateKxClusterDatabases'),
    newUpdateKxClusterDatabases,
    UpdateKxClusterDatabasesResponse (UpdateKxClusterDatabasesResponse'),
    newUpdateKxClusterDatabasesResponse,

    -- ** UpdateKxDatabase
    UpdateKxDatabase (UpdateKxDatabase'),
    newUpdateKxDatabase,
    UpdateKxDatabaseResponse (UpdateKxDatabaseResponse'),
    newUpdateKxDatabaseResponse,

    -- ** UpdateKxEnvironment
    UpdateKxEnvironment (UpdateKxEnvironment'),
    newUpdateKxEnvironment,
    UpdateKxEnvironmentResponse (UpdateKxEnvironmentResponse'),
    newUpdateKxEnvironmentResponse,

    -- ** UpdateKxEnvironmentNetwork
    UpdateKxEnvironmentNetwork (UpdateKxEnvironmentNetwork'),
    newUpdateKxEnvironmentNetwork,
    UpdateKxEnvironmentNetworkResponse (UpdateKxEnvironmentNetworkResponse'),
    newUpdateKxEnvironmentNetworkResponse,

    -- ** UpdateKxUser
    UpdateKxUser (UpdateKxUser'),
    newUpdateKxUser,
    UpdateKxUserResponse (UpdateKxUserResponse'),
    newUpdateKxUserResponse,

    -- * Types

    -- ** AutoScalingMetric
    AutoScalingMetric (..),

    -- ** ChangeType
    ChangeType (..),

    -- ** ChangesetStatus
    ChangesetStatus (..),

    -- ** DnsStatus
    DnsStatus (..),

    -- ** EnvironmentStatus
    EnvironmentStatus (..),

    -- ** ErrorDetails
    ErrorDetails (..),

    -- ** FederationMode
    FederationMode (..),

    -- ** IPAddressType
    IPAddressType (..),

    -- ** KxAzMode
    KxAzMode (..),

    -- ** KxClusterStatus
    KxClusterStatus (..),

    -- ** KxClusterType
    KxClusterType (..),

    -- ** KxSavedownStorageType
    KxSavedownStorageType (..),

    -- ** TgwStatus
    TgwStatus (..),

    -- ** AutoScalingConfiguration
    AutoScalingConfiguration (AutoScalingConfiguration'),
    newAutoScalingConfiguration,

    -- ** CapacityConfiguration
    CapacityConfiguration (CapacityConfiguration'),
    newCapacityConfiguration,

    -- ** ChangeRequest
    ChangeRequest (ChangeRequest'),
    newChangeRequest,

    -- ** CodeConfiguration
    CodeConfiguration (CodeConfiguration'),
    newCodeConfiguration,

    -- ** CustomDNSServer
    CustomDNSServer (CustomDNSServer'),
    newCustomDNSServer,

    -- ** Environment
    Environment (Environment'),
    newEnvironment,

    -- ** ErrorInfo
    ErrorInfo (ErrorInfo'),
    newErrorInfo,

    -- ** FederationParameters
    FederationParameters (FederationParameters'),
    newFederationParameters,

    -- ** KxCacheStorageConfiguration
    KxCacheStorageConfiguration (KxCacheStorageConfiguration'),
    newKxCacheStorageConfiguration,

    -- ** KxChangesetListEntry
    KxChangesetListEntry (KxChangesetListEntry'),
    newKxChangesetListEntry,

    -- ** KxCluster
    KxCluster (KxCluster'),
    newKxCluster,

    -- ** KxCommandLineArgument
    KxCommandLineArgument (KxCommandLineArgument'),
    newKxCommandLineArgument,

    -- ** KxDatabaseCacheConfiguration
    KxDatabaseCacheConfiguration (KxDatabaseCacheConfiguration'),
    newKxDatabaseCacheConfiguration,

    -- ** KxDatabaseConfiguration
    KxDatabaseConfiguration (KxDatabaseConfiguration'),
    newKxDatabaseConfiguration,

    -- ** KxDatabaseListEntry
    KxDatabaseListEntry (KxDatabaseListEntry'),
    newKxDatabaseListEntry,

    -- ** KxEnvironment
    KxEnvironment (KxEnvironment'),
    newKxEnvironment,

    -- ** KxNode
    KxNode (KxNode'),
    newKxNode,

    -- ** KxSavedownStorageConfiguration
    KxSavedownStorageConfiguration (KxSavedownStorageConfiguration'),
    newKxSavedownStorageConfiguration,

    -- ** KxUser
    KxUser (KxUser'),
    newKxUser,

    -- ** SuperuserParameters
    SuperuserParameters (SuperuserParameters'),
    newSuperuserParameters,

    -- ** TransitGatewayConfiguration
    TransitGatewayConfiguration (TransitGatewayConfiguration'),
    newTransitGatewayConfiguration,

    -- ** VpcConfiguration
    VpcConfiguration (VpcConfiguration'),
    newVpcConfiguration,
  )
where

import Amazonka.FinSpace.CreateEnvironment
import Amazonka.FinSpace.CreateKxChangeset
import Amazonka.FinSpace.CreateKxCluster
import Amazonka.FinSpace.CreateKxDatabase
import Amazonka.FinSpace.CreateKxEnvironment
import Amazonka.FinSpace.CreateKxUser
import Amazonka.FinSpace.DeleteEnvironment
import Amazonka.FinSpace.DeleteKxCluster
import Amazonka.FinSpace.DeleteKxDatabase
import Amazonka.FinSpace.DeleteKxEnvironment
import Amazonka.FinSpace.DeleteKxUser
import Amazonka.FinSpace.GetEnvironment
import Amazonka.FinSpace.GetKxChangeset
import Amazonka.FinSpace.GetKxCluster
import Amazonka.FinSpace.GetKxConnectionString
import Amazonka.FinSpace.GetKxDatabase
import Amazonka.FinSpace.GetKxEnvironment
import Amazonka.FinSpace.GetKxUser
import Amazonka.FinSpace.Lens
import Amazonka.FinSpace.ListEnvironments
import Amazonka.FinSpace.ListKxChangesets
import Amazonka.FinSpace.ListKxClusterNodes
import Amazonka.FinSpace.ListKxClusters
import Amazonka.FinSpace.ListKxDatabases
import Amazonka.FinSpace.ListKxEnvironments
import Amazonka.FinSpace.ListKxUsers
import Amazonka.FinSpace.ListTagsForResource
import Amazonka.FinSpace.TagResource
import Amazonka.FinSpace.Types
import Amazonka.FinSpace.UntagResource
import Amazonka.FinSpace.UpdateEnvironment
import Amazonka.FinSpace.UpdateKxClusterDatabases
import Amazonka.FinSpace.UpdateKxDatabase
import Amazonka.FinSpace.UpdateKxEnvironment
import Amazonka.FinSpace.UpdateKxEnvironmentNetwork
import Amazonka.FinSpace.UpdateKxUser
import Amazonka.FinSpace.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'FinSpace'.

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
