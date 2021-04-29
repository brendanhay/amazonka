{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Cloud Map lets you configure public DNS, private DNS, or HTTP
-- namespaces that your microservice applications run in. When an instance
-- of the service becomes available, you can call the AWS Cloud Map API to
-- register the instance with AWS Cloud Map. For public or private DNS
-- namespaces, AWS Cloud Map automatically creates DNS records and an
-- optional health check. Clients that submit public or private DNS
-- queries, or HTTP requests, for the service receive an answer that
-- contains up to eight healthy records.
module Network.AWS.Route53AutoNaming
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidInput
    _InvalidInput,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** DuplicateRequest
    _DuplicateRequest,

    -- ** ResourceInUse
    _ResourceInUse,

    -- ** ServiceAlreadyExists
    _ServiceAlreadyExists,

    -- ** RequestLimitExceeded
    _RequestLimitExceeded,

    -- ** ResourceLimitExceeded
    _ResourceLimitExceeded,

    -- ** CustomHealthNotFound
    _CustomHealthNotFound,

    -- ** OperationNotFound
    _OperationNotFound,

    -- ** ServiceNotFound
    _ServiceNotFound,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** NamespaceNotFound
    _NamespaceNotFound,

    -- ** NamespaceAlreadyExists
    _NamespaceAlreadyExists,

    -- ** InstanceNotFound
    _InstanceNotFound,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreatePublicDnsNamespace
    CreatePublicDnsNamespace (CreatePublicDnsNamespace'),
    newCreatePublicDnsNamespace,
    CreatePublicDnsNamespaceResponse (CreatePublicDnsNamespaceResponse'),
    newCreatePublicDnsNamespaceResponse,

    -- ** ListServices (Paginated)
    ListServices (ListServices'),
    newListServices,
    ListServicesResponse (ListServicesResponse'),
    newListServicesResponse,

    -- ** ListOperations (Paginated)
    ListOperations (ListOperations'),
    newListOperations,
    ListOperationsResponse (ListOperationsResponse'),
    newListOperationsResponse,

    -- ** CreateService
    CreateService (CreateService'),
    newCreateService,
    CreateServiceResponse (CreateServiceResponse'),
    newCreateServiceResponse,

    -- ** CreatePrivateDnsNamespace
    CreatePrivateDnsNamespace (CreatePrivateDnsNamespace'),
    newCreatePrivateDnsNamespace,
    CreatePrivateDnsNamespaceResponse (CreatePrivateDnsNamespaceResponse'),
    newCreatePrivateDnsNamespaceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** ListInstances (Paginated)
    ListInstances (ListInstances'),
    newListInstances,
    ListInstancesResponse (ListInstancesResponse'),
    newListInstancesResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** GetNamespace
    GetNamespace (GetNamespace'),
    newGetNamespace,
    GetNamespaceResponse (GetNamespaceResponse'),
    newGetNamespaceResponse,

    -- ** ListNamespaces (Paginated)
    ListNamespaces (ListNamespaces'),
    newListNamespaces,
    ListNamespacesResponse (ListNamespacesResponse'),
    newListNamespacesResponse,

    -- ** CreateHttpNamespace
    CreateHttpNamespace (CreateHttpNamespace'),
    newCreateHttpNamespace,
    CreateHttpNamespaceResponse (CreateHttpNamespaceResponse'),
    newCreateHttpNamespaceResponse,

    -- ** GetInstance
    GetInstance (GetInstance'),
    newGetInstance,
    GetInstanceResponse (GetInstanceResponse'),
    newGetInstanceResponse,

    -- ** GetInstancesHealthStatus
    GetInstancesHealthStatus (GetInstancesHealthStatus'),
    newGetInstancesHealthStatus,
    GetInstancesHealthStatusResponse (GetInstancesHealthStatusResponse'),
    newGetInstancesHealthStatusResponse,

    -- ** DeleteService
    DeleteService (DeleteService'),
    newDeleteService,
    DeleteServiceResponse (DeleteServiceResponse'),
    newDeleteServiceResponse,

    -- ** UpdateService
    UpdateService (UpdateService'),
    newUpdateService,
    UpdateServiceResponse (UpdateServiceResponse'),
    newUpdateServiceResponse,

    -- ** DiscoverInstances
    DiscoverInstances (DiscoverInstances'),
    newDiscoverInstances,
    DiscoverInstancesResponse (DiscoverInstancesResponse'),
    newDiscoverInstancesResponse,

    -- ** DeregisterInstance
    DeregisterInstance (DeregisterInstance'),
    newDeregisterInstance,
    DeregisterInstanceResponse (DeregisterInstanceResponse'),
    newDeregisterInstanceResponse,

    -- ** GetOperation
    GetOperation (GetOperation'),
    newGetOperation,
    GetOperationResponse (GetOperationResponse'),
    newGetOperationResponse,

    -- ** GetService
    GetService (GetService'),
    newGetService,
    GetServiceResponse (GetServiceResponse'),
    newGetServiceResponse,

    -- ** UpdateInstanceCustomHealthStatus
    UpdateInstanceCustomHealthStatus (UpdateInstanceCustomHealthStatus'),
    newUpdateInstanceCustomHealthStatus,
    UpdateInstanceCustomHealthStatusResponse (UpdateInstanceCustomHealthStatusResponse'),
    newUpdateInstanceCustomHealthStatusResponse,

    -- ** RegisterInstance
    RegisterInstance (RegisterInstance'),
    newRegisterInstance,
    RegisterInstanceResponse (RegisterInstanceResponse'),
    newRegisterInstanceResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DeleteNamespace
    DeleteNamespace (DeleteNamespace'),
    newDeleteNamespace,
    DeleteNamespaceResponse (DeleteNamespaceResponse'),
    newDeleteNamespaceResponse,

    -- * Types

    -- ** CustomHealthStatus
    CustomHealthStatus (..),

    -- ** FilterCondition
    FilterCondition (..),

    -- ** HealthCheckType
    HealthCheckType (..),

    -- ** HealthStatus
    HealthStatus (..),

    -- ** HealthStatusFilter
    HealthStatusFilter (..),

    -- ** NamespaceFilterName
    NamespaceFilterName (..),

    -- ** NamespaceType
    NamespaceType (..),

    -- ** OperationFilterName
    OperationFilterName (..),

    -- ** OperationStatus
    OperationStatus (..),

    -- ** OperationTargetType
    OperationTargetType (..),

    -- ** OperationType
    OperationType (..),

    -- ** RecordType
    RecordType (..),

    -- ** RoutingPolicy
    RoutingPolicy (..),

    -- ** ServiceFilterName
    ServiceFilterName (..),

    -- ** ServiceType
    ServiceType (..),

    -- ** ServiceTypeOption
    ServiceTypeOption (..),

    -- ** DnsConfig
    DnsConfig (DnsConfig'),
    newDnsConfig,

    -- ** DnsConfigChange
    DnsConfigChange (DnsConfigChange'),
    newDnsConfigChange,

    -- ** DnsProperties
    DnsProperties (DnsProperties'),
    newDnsProperties,

    -- ** DnsRecord
    DnsRecord (DnsRecord'),
    newDnsRecord,

    -- ** HealthCheckConfig
    HealthCheckConfig (HealthCheckConfig'),
    newHealthCheckConfig,

    -- ** HealthCheckCustomConfig
    HealthCheckCustomConfig (HealthCheckCustomConfig'),
    newHealthCheckCustomConfig,

    -- ** HttpInstanceSummary
    HttpInstanceSummary (HttpInstanceSummary'),
    newHttpInstanceSummary,

    -- ** HttpProperties
    HttpProperties (HttpProperties'),
    newHttpProperties,

    -- ** Instance
    Instance (Instance'),
    newInstance,

    -- ** InstanceSummary
    InstanceSummary (InstanceSummary'),
    newInstanceSummary,

    -- ** Namespace
    Namespace (Namespace'),
    newNamespace,

    -- ** NamespaceFilter
    NamespaceFilter (NamespaceFilter'),
    newNamespaceFilter,

    -- ** NamespaceProperties
    NamespaceProperties (NamespaceProperties'),
    newNamespaceProperties,

    -- ** NamespaceSummary
    NamespaceSummary (NamespaceSummary'),
    newNamespaceSummary,

    -- ** Operation
    Operation (Operation'),
    newOperation,

    -- ** OperationFilter
    OperationFilter (OperationFilter'),
    newOperationFilter,

    -- ** OperationSummary
    OperationSummary (OperationSummary'),
    newOperationSummary,

    -- ** ServiceChange
    ServiceChange (ServiceChange'),
    newServiceChange,

    -- ** ServiceFilter
    ServiceFilter (ServiceFilter'),
    newServiceFilter,

    -- ** ServiceInfo
    ServiceInfo (ServiceInfo'),
    newServiceInfo,

    -- ** ServiceSummary
    ServiceSummary (ServiceSummary'),
    newServiceSummary,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Network.AWS.Route53AutoNaming.CreateHttpNamespace
import Network.AWS.Route53AutoNaming.CreatePrivateDnsNamespace
import Network.AWS.Route53AutoNaming.CreatePublicDnsNamespace
import Network.AWS.Route53AutoNaming.CreateService
import Network.AWS.Route53AutoNaming.DeleteNamespace
import Network.AWS.Route53AutoNaming.DeleteService
import Network.AWS.Route53AutoNaming.DeregisterInstance
import Network.AWS.Route53AutoNaming.DiscoverInstances
import Network.AWS.Route53AutoNaming.GetInstance
import Network.AWS.Route53AutoNaming.GetInstancesHealthStatus
import Network.AWS.Route53AutoNaming.GetNamespace
import Network.AWS.Route53AutoNaming.GetOperation
import Network.AWS.Route53AutoNaming.GetService
import Network.AWS.Route53AutoNaming.Lens
import Network.AWS.Route53AutoNaming.ListInstances
import Network.AWS.Route53AutoNaming.ListNamespaces
import Network.AWS.Route53AutoNaming.ListOperations
import Network.AWS.Route53AutoNaming.ListServices
import Network.AWS.Route53AutoNaming.ListTagsForResource
import Network.AWS.Route53AutoNaming.RegisterInstance
import Network.AWS.Route53AutoNaming.TagResource
import Network.AWS.Route53AutoNaming.Types
import Network.AWS.Route53AutoNaming.UntagResource
import Network.AWS.Route53AutoNaming.UpdateInstanceCustomHealthStatus
import Network.AWS.Route53AutoNaming.UpdateService
import Network.AWS.Route53AutoNaming.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Route53AutoNaming'.

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
