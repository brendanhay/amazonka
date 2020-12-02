{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Route 53 auto naming lets you configure public or private namespaces that your microservice applications run in. When instances of the service become available, you can call the auto naming API to register the instance, and Route 53 automatically creates up to five DNS records and an optional health check. Clients that submit DNS queries for the service receive an answer that contains up to eight healthy records.
--
--
module Network.AWS.Route53AutoNaming
    (
    -- * Service Configuration
      route53AutoNaming

    -- * Errors
    -- $errors

    -- ** ResourceLimitExceeded
    , _ResourceLimitExceeded

    -- ** InvalidInput
    , _InvalidInput

    -- ** NamespaceAlreadyExists
    , _NamespaceAlreadyExists

    -- ** NamespaceNotFound
    , _NamespaceNotFound

    -- ** ServiceAlreadyExists
    , _ServiceAlreadyExists

    -- ** ResourceInUse
    , _ResourceInUse

    -- ** CustomHealthNotFound
    , _CustomHealthNotFound

    -- ** InstanceNotFound
    , _InstanceNotFound

    -- ** DuplicateRequest
    , _DuplicateRequest

    -- ** ServiceNotFound
    , _ServiceNotFound

    -- ** OperationNotFound
    , _OperationNotFound

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListServices (Paginated)
    , module Network.AWS.Route53AutoNaming.ListServices

    -- ** DeleteService
    , module Network.AWS.Route53AutoNaming.DeleteService

    -- ** UpdateService
    , module Network.AWS.Route53AutoNaming.UpdateService

    -- ** ListOperations (Paginated)
    , module Network.AWS.Route53AutoNaming.ListOperations

    -- ** CreatePublicDNSNamespace
    , module Network.AWS.Route53AutoNaming.CreatePublicDNSNamespace

    -- ** GetInstance
    , module Network.AWS.Route53AutoNaming.GetInstance

    -- ** ListNamespaces (Paginated)
    , module Network.AWS.Route53AutoNaming.ListNamespaces

    -- ** DeleteNamespace
    , module Network.AWS.Route53AutoNaming.DeleteNamespace

    -- ** GetInstancesHealthStatus
    , module Network.AWS.Route53AutoNaming.GetInstancesHealthStatus

    -- ** GetNamespace
    , module Network.AWS.Route53AutoNaming.GetNamespace

    -- ** RegisterInstance
    , module Network.AWS.Route53AutoNaming.RegisterInstance

    -- ** ListInstances (Paginated)
    , module Network.AWS.Route53AutoNaming.ListInstances

    -- ** GetOperation
    , module Network.AWS.Route53AutoNaming.GetOperation

    -- ** UpdateInstanceCustomHealthStatus
    , module Network.AWS.Route53AutoNaming.UpdateInstanceCustomHealthStatus

    -- ** GetService
    , module Network.AWS.Route53AutoNaming.GetService

    -- ** CreatePrivateDNSNamespace
    , module Network.AWS.Route53AutoNaming.CreatePrivateDNSNamespace

    -- ** CreateService
    , module Network.AWS.Route53AutoNaming.CreateService

    -- ** DeregisterInstance
    , module Network.AWS.Route53AutoNaming.DeregisterInstance

    -- * Types

    -- ** CustomHealthStatus
    , CustomHealthStatus (..)

    -- ** FilterCondition
    , FilterCondition (..)

    -- ** HealthCheckType
    , HealthCheckType (..)

    -- ** HealthStatus
    , HealthStatus (..)

    -- ** NamespaceFilterName
    , NamespaceFilterName (..)

    -- ** NamespaceType
    , NamespaceType (..)

    -- ** OperationFilterName
    , OperationFilterName (..)

    -- ** OperationStatus
    , OperationStatus (..)

    -- ** OperationTargetType
    , OperationTargetType (..)

    -- ** OperationType
    , OperationType (..)

    -- ** RecordType
    , RecordType (..)

    -- ** RoutingPolicy
    , RoutingPolicy (..)

    -- ** ServiceFilterName
    , ServiceFilterName (..)

    -- ** DNSConfig
    , DNSConfig
    , dnsConfig
    , dcRoutingPolicy
    , dcNamespaceId
    , dcDNSRecords

    -- ** DNSConfigChange
    , DNSConfigChange
    , dnsConfigChange
    , dccDNSRecords

    -- ** DNSProperties
    , DNSProperties
    , dnsProperties
    , dpHostedZoneId

    -- ** DNSRecord
    , DNSRecord
    , dnsRecord
    , drType
    , drTTL

    -- ** HealthCheckConfig
    , HealthCheckConfig
    , healthCheckConfig
    , hccFailureThreshold
    , hccResourcePath
    , hccType

    -- ** HealthCheckCustomConfig
    , HealthCheckCustomConfig
    , healthCheckCustomConfig
    , hcccFailureThreshold

    -- ** Instance
    , Instance
    , instance'
    , iCreatorRequestId
    , iAttributes
    , iId

    -- ** InstanceSummary
    , InstanceSummary
    , instanceSummary
    , isAttributes
    , isId

    -- ** Namespace
    , Namespace
    , namespace
    , nARN
    , nCreatorRequestId
    , nCreateDate
    , nServiceCount
    , nName
    , nId
    , nType
    , nDescription
    , nProperties

    -- ** NamespaceFilter
    , NamespaceFilter
    , namespaceFilter
    , nfCondition
    , nfName
    , nfValues

    -- ** NamespaceProperties
    , NamespaceProperties
    , namespaceProperties
    , npDNSProperties

    -- ** NamespaceSummary
    , NamespaceSummary
    , namespaceSummary
    , nsARN
    , nsName
    , nsId
    , nsType

    -- ** Operation
    , Operation
    , operation
    , oStatus
    , oUpdateDate
    , oCreateDate
    , oTargets
    , oErrorCode
    , oId
    , oType
    , oErrorMessage

    -- ** OperationFilter
    , OperationFilter
    , operationFilter
    , ofCondition
    , ofName
    , ofValues

    -- ** OperationSummary
    , OperationSummary
    , operationSummary
    , osStatus
    , osId

    -- ** ServiceChange
    , ServiceChange
    , serviceChange
    , scHealthCheckConfig
    , scDescription
    , scDNSConfig

    -- ** ServiceFilter
    , ServiceFilter
    , serviceFilter
    , sfCondition
    , sfName
    , sfValues

    -- ** ServiceInfo
    , ServiceInfo
    , serviceInfo
    , siInstanceCount
    , siARN
    , siHealthCheckConfig
    , siCreatorRequestId
    , siCreateDate
    , siHealthCheckCustomConfig
    , siName
    , siId
    , siDNSConfig
    , siDescription

    -- ** ServiceSummary
    , ServiceSummary
    , serviceSummary
    , ssInstanceCount
    , ssARN
    , ssName
    , ssId
    , ssDescription
    ) where

import Network.AWS.Route53AutoNaming.CreatePrivateDNSNamespace
import Network.AWS.Route53AutoNaming.CreatePublicDNSNamespace
import Network.AWS.Route53AutoNaming.CreateService
import Network.AWS.Route53AutoNaming.DeleteNamespace
import Network.AWS.Route53AutoNaming.DeleteService
import Network.AWS.Route53AutoNaming.DeregisterInstance
import Network.AWS.Route53AutoNaming.GetInstance
import Network.AWS.Route53AutoNaming.GetInstancesHealthStatus
import Network.AWS.Route53AutoNaming.GetNamespace
import Network.AWS.Route53AutoNaming.GetOperation
import Network.AWS.Route53AutoNaming.GetService
import Network.AWS.Route53AutoNaming.ListInstances
import Network.AWS.Route53AutoNaming.ListNamespaces
import Network.AWS.Route53AutoNaming.ListOperations
import Network.AWS.Route53AutoNaming.ListServices
import Network.AWS.Route53AutoNaming.RegisterInstance
import Network.AWS.Route53AutoNaming.Types
import Network.AWS.Route53AutoNaming.UpdateInstanceCustomHealthStatus
import Network.AWS.Route53AutoNaming.UpdateService
import Network.AWS.Route53AutoNaming.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Route53AutoNaming'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
