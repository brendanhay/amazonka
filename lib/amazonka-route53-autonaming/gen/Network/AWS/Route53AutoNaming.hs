{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

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
    (
    -- * Service configuration
      mkServiceConfig

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

    -- ** TooManyTagsException
    , _TooManyTagsException

    -- ** CustomHealthNotFound
    , _CustomHealthNotFound

    -- ** RequestLimitExceeded
    , _RequestLimitExceeded

    -- ** InstanceNotFound
    , _InstanceNotFound

    -- ** DuplicateRequest
    , _DuplicateRequest

    -- ** ServiceNotFound
    , _ServiceNotFound

    -- ** OperationNotFound
    , _OperationNotFound

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

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

    -- ** CreateHttpNamespace 
    , module Network.AWS.Route53AutoNaming.CreateHttpNamespace

    -- ** CreatePublicDnsNamespace 
    , module Network.AWS.Route53AutoNaming.CreatePublicDnsNamespace

    -- ** GetInstance 
    , module Network.AWS.Route53AutoNaming.GetInstance

    -- ** ListNamespaces (Paginated)
    , module Network.AWS.Route53AutoNaming.ListNamespaces

    -- ** DeleteNamespace 
    , module Network.AWS.Route53AutoNaming.DeleteNamespace

    -- ** ListTagsForResource 
    , module Network.AWS.Route53AutoNaming.ListTagsForResource

    -- ** DiscoverInstances 
    , module Network.AWS.Route53AutoNaming.DiscoverInstances

    -- ** GetInstancesHealthStatus 
    , module Network.AWS.Route53AutoNaming.GetInstancesHealthStatus

    -- ** GetNamespace 
    , module Network.AWS.Route53AutoNaming.GetNamespace

    -- ** RegisterInstance 
    , module Network.AWS.Route53AutoNaming.RegisterInstance

    -- ** TagResource 
    , module Network.AWS.Route53AutoNaming.TagResource

    -- ** ListInstances (Paginated)
    , module Network.AWS.Route53AutoNaming.ListInstances

    -- ** GetOperation 
    , module Network.AWS.Route53AutoNaming.GetOperation

    -- ** UpdateInstanceCustomHealthStatus 
    , module Network.AWS.Route53AutoNaming.UpdateInstanceCustomHealthStatus

    -- ** GetService 
    , module Network.AWS.Route53AutoNaming.GetService

    -- ** CreatePrivateDnsNamespace 
    , module Network.AWS.Route53AutoNaming.CreatePrivateDnsNamespace

    -- ** UntagResource 
    , module Network.AWS.Route53AutoNaming.UntagResource

    -- ** CreateService 
    , module Network.AWS.Route53AutoNaming.CreateService

    -- ** DeregisterInstance 
    , module Network.AWS.Route53AutoNaming.DeregisterInstance

    -- * Types

    -- ** NamespaceFilterName
    , NamespaceFilterName (..)

    -- ** RoutingPolicy
    , RoutingPolicy (..)

    -- ** DnsRecord
    , DnsRecord (..)
    , mkDnsRecord
    , drType
    , drTTL

    -- ** AttrKey
    , AttrKey (..)

    -- ** OperationTargetType
    , OperationTargetType (..)

    -- ** ResourceId
    , ResourceId (..)

    -- ** ServiceFilterName
    , ServiceFilterName (..)

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** OperationFilterName
    , OperationFilterName (..)

    -- ** InstanceSummary
    , InstanceSummary (..)
    , mkInstanceSummary
    , isAttributes
    , isId

    -- ** Arn
    , Arn (..)

    -- ** ServiceInfo
    , ServiceInfo (..)
    , mkServiceInfo
    , siArn
    , siCreateDate
    , siCreatorRequestId
    , siDescription
    , siDnsConfig
    , siHealthCheckConfig
    , siHealthCheckCustomConfig
    , siId
    , siInstanceCount
    , siName
    , siNamespaceId

    -- ** Operation
    , Operation (..)
    , mkOperation
    , oCreateDate
    , oErrorCode
    , oErrorMessage
    , oId
    , oStatus
    , oTargets
    , oType
    , oUpdateDate

    -- ** RecordType
    , RecordType (..)

    -- ** DnsConfigChange
    , DnsConfigChange (..)
    , mkDnsConfigChange
    , dccDnsRecords

    -- ** HealthStatusFilter
    , HealthStatusFilter (..)

    -- ** HealthCheckConfig
    , HealthCheckConfig (..)
    , mkHealthCheckConfig
    , hccType
    , hccFailureThreshold
    , hccResourcePath

    -- ** DnsProperties
    , DnsProperties (..)
    , mkDnsProperties
    , dpHostedZoneId

    -- ** FilterCondition
    , FilterCondition (..)

    -- ** Namespace
    , Namespace (..)
    , mkNamespace
    , nArn
    , nCreateDate
    , nCreatorRequestId
    , nDescription
    , nId
    , nName
    , nProperties
    , nServiceCount
    , nType

    -- ** HealthCheckCustomConfig
    , HealthCheckCustomConfig (..)
    , mkHealthCheckCustomConfig
    , hcccFailureThreshold

    -- ** OperationStatus
    , OperationStatus (..)

    -- ** HttpProperties
    , HttpProperties (..)
    , mkHttpProperties
    , hpHttpName

    -- ** NextToken
    , NextToken (..)

    -- ** ResourceDescription
    , ResourceDescription (..)

    -- ** NamespaceName
    , NamespaceName (..)

    -- ** NamespaceSummary
    , NamespaceSummary (..)
    , mkNamespaceSummary
    , nsArn
    , nsCreateDate
    , nsDescription
    , nsId
    , nsName
    , nsProperties
    , nsServiceCount
    , nsType

    -- ** ResourcePath
    , ResourcePath (..)

    -- ** ServiceName
    , ServiceName (..)

    -- ** NamespaceProperties
    , NamespaceProperties (..)
    , mkNamespaceProperties
    , npDnsProperties
    , npHttpProperties

    -- ** HealthCheckType
    , HealthCheckType (..)

    -- ** OperationFilter
    , OperationFilter (..)
    , mkOperationFilter
    , ofName
    , ofValues
    , ofCondition

    -- ** OperationType
    , OperationType (..)

    -- ** HttpInstanceSummary
    , HttpInstanceSummary (..)
    , mkHttpInstanceSummary
    , hisAttributes
    , hisHealthStatus
    , hisInstanceId
    , hisNamespaceName
    , hisServiceName

    -- ** ServiceFilter
    , ServiceFilter (..)
    , mkServiceFilter
    , sfName
    , sfValues
    , sfCondition

    -- ** TagKey
    , TagKey (..)

    -- ** OperationId
    , OperationId (..)

    -- ** AttrValue
    , AttrValue (..)

    -- ** HealthStatus
    , HealthStatus (..)

    -- ** FilterValue
    , FilterValue (..)

    -- ** DnsConfig
    , DnsConfig (..)
    , mkDnsConfig
    , dcDnsRecords
    , dcNamespaceId
    , dcRoutingPolicy

    -- ** ErrorMessage
    , ErrorMessage (..)

    -- ** AmazonResourceName
    , AmazonResourceName (..)

    -- ** NamespaceFilter
    , NamespaceFilter (..)
    , mkNamespaceFilter
    , nfName
    , nfValues
    , nfCondition

    -- ** CustomHealthStatus
    , CustomHealthStatus (..)

    -- ** ServiceChange
    , ServiceChange (..)
    , mkServiceChange
    , scDescription
    , scDnsConfig
    , scHealthCheckConfig

    -- ** NamespaceType
    , NamespaceType (..)

    -- ** Instance
    , Instance (..)
    , mkInstance
    , iId
    , iAttributes
    , iCreatorRequestId

    -- ** OperationSummary
    , OperationSummary (..)
    , mkOperationSummary
    , osId
    , osStatus

    -- ** ServiceSummary
    , ServiceSummary (..)
    , mkServiceSummary
    , ssArn
    , ssCreateDate
    , ssDescription
    , ssDnsConfig
    , ssHealthCheckConfig
    , ssHealthCheckCustomConfig
    , ssId
    , ssInstanceCount
    , ssName

    -- ** Key
    , Key (..)

    -- ** Value
    , Value (..)

    -- ** Description
    , Description (..)

    -- ** Name
    , Name (..)

    -- ** ErrorCode
    , ErrorCode (..)

    -- ** Id
    , Id (..)

    -- ** ResourceARN
    , ResourceARN (..)

    -- ** HttpName
    , HttpName (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.Route53AutoNaming.Types
import Network.AWS.Route53AutoNaming.Waiters
import Network.AWS.Route53AutoNaming.ListServices
import Network.AWS.Route53AutoNaming.DeleteService
import Network.AWS.Route53AutoNaming.UpdateService
import Network.AWS.Route53AutoNaming.ListOperations
import Network.AWS.Route53AutoNaming.CreateHttpNamespace
import Network.AWS.Route53AutoNaming.CreatePublicDnsNamespace
import Network.AWS.Route53AutoNaming.GetInstance
import Network.AWS.Route53AutoNaming.ListNamespaces
import Network.AWS.Route53AutoNaming.DeleteNamespace
import Network.AWS.Route53AutoNaming.ListTagsForResource
import Network.AWS.Route53AutoNaming.DiscoverInstances
import Network.AWS.Route53AutoNaming.GetInstancesHealthStatus
import Network.AWS.Route53AutoNaming.GetNamespace
import Network.AWS.Route53AutoNaming.RegisterInstance
import Network.AWS.Route53AutoNaming.TagResource
import Network.AWS.Route53AutoNaming.ListInstances
import Network.AWS.Route53AutoNaming.GetOperation
import Network.AWS.Route53AutoNaming.UpdateInstanceCustomHealthStatus
import Network.AWS.Route53AutoNaming.GetService
import Network.AWS.Route53AutoNaming.CreatePrivateDnsNamespace
import Network.AWS.Route53AutoNaming.UntagResource
import Network.AWS.Route53AutoNaming.CreateService
import Network.AWS.Route53AutoNaming.DeregisterInstance
import qualified Network.AWS.Prelude as Lude

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
