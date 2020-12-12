{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Cloud Map lets you configure public DNS, private DNS, or HTTP namespaces that your microservice applications run in. When an instance of the service becomes available, you can call the AWS Cloud Map API to register the instance with AWS Cloud Map. For public or private DNS namespaces, AWS Cloud Map automatically creates DNS records and an optional health check. Clients that submit public or private DNS queries, or HTTP requests, for the service receive an answer that contains up to eight healthy records.
module Network.AWS.Route53AutoNaming
  ( -- * Service configuration
    route53AutoNamingService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListServices (Paginated)
    module Network.AWS.Route53AutoNaming.ListServices,

    -- ** DeleteService
    module Network.AWS.Route53AutoNaming.DeleteService,

    -- ** UpdateService
    module Network.AWS.Route53AutoNaming.UpdateService,

    -- ** ListOperations (Paginated)
    module Network.AWS.Route53AutoNaming.ListOperations,

    -- ** CreateHTTPNamespace
    module Network.AWS.Route53AutoNaming.CreateHTTPNamespace,

    -- ** CreatePublicDNSNamespace
    module Network.AWS.Route53AutoNaming.CreatePublicDNSNamespace,

    -- ** GetInstance
    module Network.AWS.Route53AutoNaming.GetInstance,

    -- ** ListNamespaces (Paginated)
    module Network.AWS.Route53AutoNaming.ListNamespaces,

    -- ** DeleteNamespace
    module Network.AWS.Route53AutoNaming.DeleteNamespace,

    -- ** ListTagsForResource
    module Network.AWS.Route53AutoNaming.ListTagsForResource,

    -- ** DiscoverInstances
    module Network.AWS.Route53AutoNaming.DiscoverInstances,

    -- ** GetInstancesHealthStatus
    module Network.AWS.Route53AutoNaming.GetInstancesHealthStatus,

    -- ** GetNamespace
    module Network.AWS.Route53AutoNaming.GetNamespace,

    -- ** RegisterInstance
    module Network.AWS.Route53AutoNaming.RegisterInstance,

    -- ** TagResource
    module Network.AWS.Route53AutoNaming.TagResource,

    -- ** ListInstances (Paginated)
    module Network.AWS.Route53AutoNaming.ListInstances,

    -- ** GetOperation
    module Network.AWS.Route53AutoNaming.GetOperation,

    -- ** UpdateInstanceCustomHealthStatus
    module Network.AWS.Route53AutoNaming.UpdateInstanceCustomHealthStatus,

    -- ** GetService
    module Network.AWS.Route53AutoNaming.GetService,

    -- ** CreatePrivateDNSNamespace
    module Network.AWS.Route53AutoNaming.CreatePrivateDNSNamespace,

    -- ** UntagResource
    module Network.AWS.Route53AutoNaming.UntagResource,

    -- ** CreateService
    module Network.AWS.Route53AutoNaming.CreateService,

    -- ** DeregisterInstance
    module Network.AWS.Route53AutoNaming.DeregisterInstance,

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

    -- ** DNSConfig
    DNSConfig (..),
    mkDNSConfig,
    dcRoutingPolicy,
    dcNamespaceId,
    dcDNSRecords,

    -- ** DNSConfigChange
    DNSConfigChange (..),
    mkDNSConfigChange,
    dccDNSRecords,

    -- ** DNSProperties
    DNSProperties (..),
    mkDNSProperties,
    dpHostedZoneId,

    -- ** DNSRecord
    DNSRecord (..),
    mkDNSRecord,
    drType,
    drTTL,

    -- ** HTTPInstanceSummary
    HTTPInstanceSummary (..),
    mkHTTPInstanceSummary,
    httpisInstanceId,
    httpisNamespaceName,
    httpisAttributes,
    httpisServiceName,
    httpisHealthStatus,

    -- ** HTTPProperties
    HTTPProperties (..),
    mkHTTPProperties,
    httppHTTPName,

    -- ** HealthCheckConfig
    HealthCheckConfig (..),
    mkHealthCheckConfig,
    hccFailureThreshold,
    hccResourcePath,
    hccType,

    -- ** HealthCheckCustomConfig
    HealthCheckCustomConfig (..),
    mkHealthCheckCustomConfig,
    hcccFailureThreshold,

    -- ** Instance
    Instance (..),
    mkInstance,
    iCreatorRequestId,
    iAttributes,
    iId,

    -- ** InstanceSummary
    InstanceSummary (..),
    mkInstanceSummary,
    isAttributes,
    isId,

    -- ** Namespace
    Namespace (..),
    mkNamespace,
    nARN,
    nCreatorRequestId,
    nCreateDate,
    nServiceCount,
    nName,
    nId,
    nType,
    nDescription,
    nProperties,

    -- ** NamespaceFilter
    NamespaceFilter (..),
    mkNamespaceFilter,
    nfCondition,
    nfName,
    nfValues,

    -- ** NamespaceProperties
    NamespaceProperties (..),
    mkNamespaceProperties,
    npDNSProperties,
    npHTTPProperties,

    -- ** NamespaceSummary
    NamespaceSummary (..),
    mkNamespaceSummary,
    nsARN,
    nsCreateDate,
    nsServiceCount,
    nsName,
    nsId,
    nsType,
    nsDescription,
    nsProperties,

    -- ** Operation
    Operation (..),
    mkOperation,
    oStatus,
    oUpdateDate,
    oCreateDate,
    oTargets,
    oErrorCode,
    oId,
    oType,
    oErrorMessage,

    -- ** OperationFilter
    OperationFilter (..),
    mkOperationFilter,
    ofCondition,
    ofName,
    ofValues,

    -- ** OperationSummary
    OperationSummary (..),
    mkOperationSummary,
    osStatus,
    osId,

    -- ** ServiceChange
    ServiceChange (..),
    mkServiceChange,
    scHealthCheckConfig,
    scDNSConfig,
    scDescription,

    -- ** ServiceFilter
    ServiceFilter (..),
    mkServiceFilter,
    sfCondition,
    sfName,
    sfValues,

    -- ** ServiceInfo
    ServiceInfo (..),
    mkServiceInfo,
    siInstanceCount,
    siARN,
    siHealthCheckConfig,
    siCreatorRequestId,
    siCreateDate,
    siHealthCheckCustomConfig,
    siNamespaceId,
    siName,
    siId,
    siDNSConfig,
    siDescription,

    -- ** ServiceSummary
    ServiceSummary (..),
    mkServiceSummary,
    ssInstanceCount,
    ssARN,
    ssHealthCheckConfig,
    ssCreateDate,
    ssHealthCheckCustomConfig,
    ssName,
    ssId,
    ssDNSConfig,
    ssDescription,

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53AutoNaming.CreateHTTPNamespace
import Network.AWS.Route53AutoNaming.CreatePrivateDNSNamespace
import Network.AWS.Route53AutoNaming.CreatePublicDNSNamespace
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
