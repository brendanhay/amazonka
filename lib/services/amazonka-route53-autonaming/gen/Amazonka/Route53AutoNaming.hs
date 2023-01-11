{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Route53AutoNaming
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-03-14@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Cloud Map
--
-- With Cloud Map, you can configure public DNS, private DNS, or HTTP
-- namespaces that your microservice applications run in. When an instance
-- becomes available, you can call the Cloud Map API to register the
-- instance with Cloud Map. For public or private DNS namespaces, Cloud Map
-- automatically creates DNS records and an optional health check. Clients
-- that submit public or private DNS queries, or HTTP requests, for the
-- service receive an answer that contains up to eight healthy records.
module Amazonka.Route53AutoNaming
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** CustomHealthNotFound
    _CustomHealthNotFound,

    -- ** DuplicateRequest
    _DuplicateRequest,

    -- ** InstanceNotFound
    _InstanceNotFound,

    -- ** InvalidInput
    _InvalidInput,

    -- ** NamespaceAlreadyExists
    _NamespaceAlreadyExists,

    -- ** NamespaceNotFound
    _NamespaceNotFound,

    -- ** OperationNotFound
    _OperationNotFound,

    -- ** RequestLimitExceeded
    _RequestLimitExceeded,

    -- ** ResourceInUse
    _ResourceInUse,

    -- ** ResourceLimitExceeded
    _ResourceLimitExceeded,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceAlreadyExists
    _ServiceAlreadyExists,

    -- ** ServiceNotFound
    _ServiceNotFound,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateHttpNamespace
    CreateHttpNamespace (CreateHttpNamespace'),
    newCreateHttpNamespace,
    CreateHttpNamespaceResponse (CreateHttpNamespaceResponse'),
    newCreateHttpNamespaceResponse,

    -- ** CreatePrivateDnsNamespace
    CreatePrivateDnsNamespace (CreatePrivateDnsNamespace'),
    newCreatePrivateDnsNamespace,
    CreatePrivateDnsNamespaceResponse (CreatePrivateDnsNamespaceResponse'),
    newCreatePrivateDnsNamespaceResponse,

    -- ** CreatePublicDnsNamespace
    CreatePublicDnsNamespace (CreatePublicDnsNamespace'),
    newCreatePublicDnsNamespace,
    CreatePublicDnsNamespaceResponse (CreatePublicDnsNamespaceResponse'),
    newCreatePublicDnsNamespaceResponse,

    -- ** CreateService
    CreateService (CreateService'),
    newCreateService,
    CreateServiceResponse (CreateServiceResponse'),
    newCreateServiceResponse,

    -- ** DeleteNamespace
    DeleteNamespace (DeleteNamespace'),
    newDeleteNamespace,
    DeleteNamespaceResponse (DeleteNamespaceResponse'),
    newDeleteNamespaceResponse,

    -- ** DeleteService
    DeleteService (DeleteService'),
    newDeleteService,
    DeleteServiceResponse (DeleteServiceResponse'),
    newDeleteServiceResponse,

    -- ** DeregisterInstance
    DeregisterInstance (DeregisterInstance'),
    newDeregisterInstance,
    DeregisterInstanceResponse (DeregisterInstanceResponse'),
    newDeregisterInstanceResponse,

    -- ** DiscoverInstances
    DiscoverInstances (DiscoverInstances'),
    newDiscoverInstances,
    DiscoverInstancesResponse (DiscoverInstancesResponse'),
    newDiscoverInstancesResponse,

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

    -- ** GetNamespace
    GetNamespace (GetNamespace'),
    newGetNamespace,
    GetNamespaceResponse (GetNamespaceResponse'),
    newGetNamespaceResponse,

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

    -- ** ListInstances (Paginated)
    ListInstances (ListInstances'),
    newListInstances,
    ListInstancesResponse (ListInstancesResponse'),
    newListInstancesResponse,

    -- ** ListNamespaces (Paginated)
    ListNamespaces (ListNamespaces'),
    newListNamespaces,
    ListNamespacesResponse (ListNamespacesResponse'),
    newListNamespacesResponse,

    -- ** ListOperations (Paginated)
    ListOperations (ListOperations'),
    newListOperations,
    ListOperationsResponse (ListOperationsResponse'),
    newListOperationsResponse,

    -- ** ListServices (Paginated)
    ListServices (ListServices'),
    newListServices,
    ListServicesResponse (ListServicesResponse'),
    newListServicesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** RegisterInstance
    RegisterInstance (RegisterInstance'),
    newRegisterInstance,
    RegisterInstanceResponse (RegisterInstanceResponse'),
    newRegisterInstanceResponse,

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

    -- ** UpdateHttpNamespace
    UpdateHttpNamespace (UpdateHttpNamespace'),
    newUpdateHttpNamespace,
    UpdateHttpNamespaceResponse (UpdateHttpNamespaceResponse'),
    newUpdateHttpNamespaceResponse,

    -- ** UpdateInstanceCustomHealthStatus
    UpdateInstanceCustomHealthStatus (UpdateInstanceCustomHealthStatus'),
    newUpdateInstanceCustomHealthStatus,
    UpdateInstanceCustomHealthStatusResponse (UpdateInstanceCustomHealthStatusResponse'),
    newUpdateInstanceCustomHealthStatusResponse,

    -- ** UpdatePrivateDnsNamespace
    UpdatePrivateDnsNamespace (UpdatePrivateDnsNamespace'),
    newUpdatePrivateDnsNamespace,
    UpdatePrivateDnsNamespaceResponse (UpdatePrivateDnsNamespaceResponse'),
    newUpdatePrivateDnsNamespaceResponse,

    -- ** UpdatePublicDnsNamespace
    UpdatePublicDnsNamespace (UpdatePublicDnsNamespace'),
    newUpdatePublicDnsNamespace,
    UpdatePublicDnsNamespaceResponse (UpdatePublicDnsNamespaceResponse'),
    newUpdatePublicDnsNamespaceResponse,

    -- ** UpdateService
    UpdateService (UpdateService'),
    newUpdateService,
    UpdateServiceResponse (UpdateServiceResponse'),
    newUpdateServiceResponse,

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

    -- ** HttpNamespaceChange
    HttpNamespaceChange (HttpNamespaceChange'),
    newHttpNamespaceChange,

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

    -- ** PrivateDnsNamespaceChange
    PrivateDnsNamespaceChange (PrivateDnsNamespaceChange'),
    newPrivateDnsNamespaceChange,

    -- ** PrivateDnsNamespaceProperties
    PrivateDnsNamespaceProperties (PrivateDnsNamespaceProperties'),
    newPrivateDnsNamespaceProperties,

    -- ** PrivateDnsNamespacePropertiesChange
    PrivateDnsNamespacePropertiesChange (PrivateDnsNamespacePropertiesChange'),
    newPrivateDnsNamespacePropertiesChange,

    -- ** PrivateDnsPropertiesMutable
    PrivateDnsPropertiesMutable (PrivateDnsPropertiesMutable'),
    newPrivateDnsPropertiesMutable,

    -- ** PrivateDnsPropertiesMutableChange
    PrivateDnsPropertiesMutableChange (PrivateDnsPropertiesMutableChange'),
    newPrivateDnsPropertiesMutableChange,

    -- ** PublicDnsNamespaceChange
    PublicDnsNamespaceChange (PublicDnsNamespaceChange'),
    newPublicDnsNamespaceChange,

    -- ** PublicDnsNamespaceProperties
    PublicDnsNamespaceProperties (PublicDnsNamespaceProperties'),
    newPublicDnsNamespaceProperties,

    -- ** PublicDnsNamespacePropertiesChange
    PublicDnsNamespacePropertiesChange (PublicDnsNamespacePropertiesChange'),
    newPublicDnsNamespacePropertiesChange,

    -- ** PublicDnsPropertiesMutable
    PublicDnsPropertiesMutable (PublicDnsPropertiesMutable'),
    newPublicDnsPropertiesMutable,

    -- ** PublicDnsPropertiesMutableChange
    PublicDnsPropertiesMutableChange (PublicDnsPropertiesMutableChange'),
    newPublicDnsPropertiesMutableChange,

    -- ** SOA
    SOA (SOA'),
    newSOA,

    -- ** SOAChange
    SOAChange (SOAChange'),
    newSOAChange,

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

import Amazonka.Route53AutoNaming.CreateHttpNamespace
import Amazonka.Route53AutoNaming.CreatePrivateDnsNamespace
import Amazonka.Route53AutoNaming.CreatePublicDnsNamespace
import Amazonka.Route53AutoNaming.CreateService
import Amazonka.Route53AutoNaming.DeleteNamespace
import Amazonka.Route53AutoNaming.DeleteService
import Amazonka.Route53AutoNaming.DeregisterInstance
import Amazonka.Route53AutoNaming.DiscoverInstances
import Amazonka.Route53AutoNaming.GetInstance
import Amazonka.Route53AutoNaming.GetInstancesHealthStatus
import Amazonka.Route53AutoNaming.GetNamespace
import Amazonka.Route53AutoNaming.GetOperation
import Amazonka.Route53AutoNaming.GetService
import Amazonka.Route53AutoNaming.Lens
import Amazonka.Route53AutoNaming.ListInstances
import Amazonka.Route53AutoNaming.ListNamespaces
import Amazonka.Route53AutoNaming.ListOperations
import Amazonka.Route53AutoNaming.ListServices
import Amazonka.Route53AutoNaming.ListTagsForResource
import Amazonka.Route53AutoNaming.RegisterInstance
import Amazonka.Route53AutoNaming.TagResource
import Amazonka.Route53AutoNaming.Types
import Amazonka.Route53AutoNaming.UntagResource
import Amazonka.Route53AutoNaming.UpdateHttpNamespace
import Amazonka.Route53AutoNaming.UpdateInstanceCustomHealthStatus
import Amazonka.Route53AutoNaming.UpdatePrivateDnsNamespace
import Amazonka.Route53AutoNaming.UpdatePublicDnsNamespace
import Amazonka.Route53AutoNaming.UpdateService
import Amazonka.Route53AutoNaming.Waiters

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
