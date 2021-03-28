-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _ResourceLimitExceeded
    , _InvalidInput
    , _NamespaceAlreadyExists
    , _NamespaceNotFound
    , _ServiceAlreadyExists
    , _ResourceInUse
    , _TooManyTagsException
    , _CustomHealthNotFound
    , _RequestLimitExceeded
    , _InstanceNotFound
    , _DuplicateRequest
    , _ServiceNotFound
    , _OperationNotFound
    , _ResourceNotFoundException

    -- * NamespaceFilterName
    , NamespaceFilterName (..)

    -- * RoutingPolicy
    , RoutingPolicy (..)

    -- * DnsRecord
    , DnsRecord (..)
    , mkDnsRecord
    , drType
    , drTTL

    -- * AttrKey
    , AttrKey (..)

    -- * OperationTargetType
    , OperationTargetType (..)

    -- * ResourceId
    , ResourceId (..)

    -- * ServiceFilterName
    , ServiceFilterName (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * OperationFilterName
    , OperationFilterName (..)

    -- * InstanceSummary
    , InstanceSummary (..)
    , mkInstanceSummary
    , isAttributes
    , isId

    -- * Arn
    , Arn (..)

    -- * ServiceInfo
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

    -- * Operation
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

    -- * RecordType
    , RecordType (..)

    -- * DnsConfigChange
    , DnsConfigChange (..)
    , mkDnsConfigChange
    , dccDnsRecords

    -- * HealthStatusFilter
    , HealthStatusFilter (..)

    -- * HealthCheckConfig
    , HealthCheckConfig (..)
    , mkHealthCheckConfig
    , hccType
    , hccFailureThreshold
    , hccResourcePath

    -- * DnsProperties
    , DnsProperties (..)
    , mkDnsProperties
    , dpHostedZoneId

    -- * FilterCondition
    , FilterCondition (..)

    -- * Namespace
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

    -- * HealthCheckCustomConfig
    , HealthCheckCustomConfig (..)
    , mkHealthCheckCustomConfig
    , hcccFailureThreshold

    -- * OperationStatus
    , OperationStatus (..)

    -- * HttpProperties
    , HttpProperties (..)
    , mkHttpProperties
    , hpHttpName

    -- * NextToken
    , NextToken (..)

    -- * ResourceDescription
    , ResourceDescription (..)

    -- * NamespaceName
    , NamespaceName (..)

    -- * NamespaceSummary
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

    -- * ResourcePath
    , ResourcePath (..)

    -- * ServiceName
    , ServiceName (..)

    -- * NamespaceProperties
    , NamespaceProperties (..)
    , mkNamespaceProperties
    , npDnsProperties
    , npHttpProperties

    -- * HealthCheckType
    , HealthCheckType (..)

    -- * OperationFilter
    , OperationFilter (..)
    , mkOperationFilter
    , ofName
    , ofValues
    , ofCondition

    -- * OperationType
    , OperationType (..)

    -- * HttpInstanceSummary
    , HttpInstanceSummary (..)
    , mkHttpInstanceSummary
    , hisAttributes
    , hisHealthStatus
    , hisInstanceId
    , hisNamespaceName
    , hisServiceName

    -- * ServiceFilter
    , ServiceFilter (..)
    , mkServiceFilter
    , sfName
    , sfValues
    , sfCondition

    -- * TagKey
    , TagKey (..)

    -- * OperationId
    , OperationId (..)

    -- * AttrValue
    , AttrValue (..)

    -- * HealthStatus
    , HealthStatus (..)

    -- * FilterValue
    , FilterValue (..)

    -- * DnsConfig
    , DnsConfig (..)
    , mkDnsConfig
    , dcDnsRecords
    , dcNamespaceId
    , dcRoutingPolicy

    -- * ErrorMessage
    , ErrorMessage (..)

    -- * AmazonResourceName
    , AmazonResourceName (..)

    -- * NamespaceFilter
    , NamespaceFilter (..)
    , mkNamespaceFilter
    , nfName
    , nfValues
    , nfCondition

    -- * CustomHealthStatus
    , CustomHealthStatus (..)

    -- * ServiceChange
    , ServiceChange (..)
    , mkServiceChange
    , scDescription
    , scDnsConfig
    , scHealthCheckConfig

    -- * NamespaceType
    , NamespaceType (..)

    -- * Instance
    , Instance (..)
    , mkInstance
    , iId
    , iAttributes
    , iCreatorRequestId

    -- * OperationSummary
    , OperationSummary (..)
    , mkOperationSummary
    , osId
    , osStatus

    -- * ServiceSummary
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

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * Description
    , Description (..)

    -- * Name
    , Name (..)

    -- * ErrorCode
    , ErrorCode (..)

    -- * Id
    , Id (..)

    -- * ResourceARN
    , ResourceARN (..)

    -- * HttpName
    , HttpName (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.Route53AutoNaming.Types.NamespaceFilterName
  
import Network.AWS.Route53AutoNaming.Types.RoutingPolicy
  
import Network.AWS.Route53AutoNaming.Types.DnsRecord
  
import Network.AWS.Route53AutoNaming.Types.AttrKey
  
import Network.AWS.Route53AutoNaming.Types.OperationTargetType
  
  
  
import Network.AWS.Route53AutoNaming.Types.ResourceId
  
import Network.AWS.Route53AutoNaming.Types.ServiceFilterName
  
import Network.AWS.Route53AutoNaming.Types.Tag
  
import Network.AWS.Route53AutoNaming.Types.OperationFilterName
  
  
import Network.AWS.Route53AutoNaming.Types.InstanceSummary
  
import Network.AWS.Route53AutoNaming.Types.Arn
  
import Network.AWS.Route53AutoNaming.Types.ServiceInfo
  
import Network.AWS.Route53AutoNaming.Types.Operation
  
import Network.AWS.Route53AutoNaming.Types.RecordType
  
import Network.AWS.Route53AutoNaming.Types.DnsConfigChange
  
import Network.AWS.Route53AutoNaming.Types.HealthStatusFilter
  
  
import Network.AWS.Route53AutoNaming.Types.HealthCheckConfig
  
import Network.AWS.Route53AutoNaming.Types.DnsProperties
  
import Network.AWS.Route53AutoNaming.Types.FilterCondition
  
import Network.AWS.Route53AutoNaming.Types.Namespace
  
import Network.AWS.Route53AutoNaming.Types.HealthCheckCustomConfig
  
  
  
import Network.AWS.Route53AutoNaming.Types.OperationStatus
  
  
import Network.AWS.Route53AutoNaming.Types.HttpProperties
  
import Network.AWS.Route53AutoNaming.Types.NextToken
  
import Network.AWS.Route53AutoNaming.Types.ResourceDescription
  
  
import Network.AWS.Route53AutoNaming.Types.NamespaceName
  
import Network.AWS.Route53AutoNaming.Types.NamespaceSummary
  
import Network.AWS.Route53AutoNaming.Types.ResourcePath
  
  
  
import Network.AWS.Route53AutoNaming.Types.ServiceName
  
import Network.AWS.Route53AutoNaming.Types.NamespaceProperties
  
import Network.AWS.Route53AutoNaming.Types.HealthCheckType
  
import Network.AWS.Route53AutoNaming.Types.OperationFilter
  
import Network.AWS.Route53AutoNaming.Types.OperationType
  
import Network.AWS.Route53AutoNaming.Types.HttpInstanceSummary
  
import Network.AWS.Route53AutoNaming.Types.ServiceFilter
  
import Network.AWS.Route53AutoNaming.Types.TagKey
  
import Network.AWS.Route53AutoNaming.Types.OperationId
  
import Network.AWS.Route53AutoNaming.Types.AttrValue
  
import Network.AWS.Route53AutoNaming.Types.HealthStatus
  
import Network.AWS.Route53AutoNaming.Types.FilterValue
  
import Network.AWS.Route53AutoNaming.Types.DnsConfig
  
import Network.AWS.Route53AutoNaming.Types.ErrorMessage
  
import Network.AWS.Route53AutoNaming.Types.AmazonResourceName
  
import Network.AWS.Route53AutoNaming.Types.NamespaceFilter
  
import Network.AWS.Route53AutoNaming.Types.CustomHealthStatus
  
import Network.AWS.Route53AutoNaming.Types.ServiceChange
  
import Network.AWS.Route53AutoNaming.Types.NamespaceType
  
  
  
  
  
import Network.AWS.Route53AutoNaming.Types.Instance
  
import Network.AWS.Route53AutoNaming.Types.OperationSummary
  
import Network.AWS.Route53AutoNaming.Types.ServiceSummary
  
import Network.AWS.Route53AutoNaming.Types.Key
  
import Network.AWS.Route53AutoNaming.Types.Value
  
import Network.AWS.Route53AutoNaming.Types.Description
  
import Network.AWS.Route53AutoNaming.Types.Name
  
import Network.AWS.Route53AutoNaming.Types.ErrorCode
  
import Network.AWS.Route53AutoNaming.Types.Id
  
import Network.AWS.Route53AutoNaming.Types.ResourceARN
  
import Network.AWS.Route53AutoNaming.Types.HttpName
  

-- | API version @2017-03-14@ of the Amazon Cloud Map SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "Route53AutoNaming",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "servicediscovery",
                 Core._svcVersion = "2017-03-14", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "Route53AutoNaming",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The resource can't be created because you've reached the quota on the number of resources.
_ResourceLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceLimitExceeded
  = Core._MatchServiceError mkServiceConfig "ResourceLimitExceeded"
{-# INLINEABLE _ResourceLimitExceeded #-}
{-# DEPRECATED _ResourceLimitExceeded "Use generic-lens or generic-optics instead"  #-}

-- | One or more specified values aren't valid. For example, a required value might be missing, a numeric value might be outside the allowed range, or a string value might exceed length constraints.
_InvalidInput :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInput
  = Core._MatchServiceError mkServiceConfig "InvalidInput"
{-# INLINEABLE _InvalidInput #-}
{-# DEPRECATED _InvalidInput "Use generic-lens or generic-optics instead"  #-}

-- | The namespace that you're trying to create already exists.
_NamespaceAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NamespaceAlreadyExists
  = Core._MatchServiceError mkServiceConfig "NamespaceAlreadyExists"
{-# INLINEABLE _NamespaceAlreadyExists #-}
{-# DEPRECATED _NamespaceAlreadyExists "Use generic-lens or generic-optics instead"  #-}

-- | No namespace exists with the specified ID.
_NamespaceNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NamespaceNotFound
  = Core._MatchServiceError mkServiceConfig "NamespaceNotFound"
{-# INLINEABLE _NamespaceNotFound #-}
{-# DEPRECATED _NamespaceNotFound "Use generic-lens or generic-optics instead"  #-}

-- | The service can't be created because a service with the same name already exists.
_ServiceAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceAlreadyExists
  = Core._MatchServiceError mkServiceConfig "ServiceAlreadyExists"
{-# INLINEABLE _ServiceAlreadyExists #-}
{-# DEPRECATED _ServiceAlreadyExists "Use generic-lens or generic-optics instead"  #-}

-- | The specified resource can't be deleted because it contains other resources. For example, you can't delete a service that contains any instances.
_ResourceInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUse
  = Core._MatchServiceError mkServiceConfig "ResourceInUse"
{-# INLINEABLE _ResourceInUse #-}
{-# DEPRECATED _ResourceInUse "Use generic-lens or generic-optics instead"  #-}

-- | The list of tags on the resource is over the quota. The maximum number of tags that can be applied to a resource is 50.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException
  = Core._MatchServiceError mkServiceConfig "TooManyTagsException"
{-# INLINEABLE _TooManyTagsException #-}
{-# DEPRECATED _TooManyTagsException "Use generic-lens or generic-optics instead"  #-}

-- | The health check for the instance that is specified by @ServiceId@ and @InstanceId@ is not a custom health check. 
_CustomHealthNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CustomHealthNotFound
  = Core._MatchServiceError mkServiceConfig "CustomHealthNotFound"
{-# INLINEABLE _CustomHealthNotFound #-}
{-# DEPRECATED _CustomHealthNotFound "Use generic-lens or generic-optics instead"  #-}

-- | The operation can't be completed because you've reached the quota for the number of requests. For more information, see <https://docs.aws.amazon.com/cloud-map/latest/dg/throttling.html AWS Cloud Map API request throttling quota> in the /AWS Cloud Map Developer Guide/ .
_RequestLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RequestLimitExceeded
  = Core._MatchServiceError mkServiceConfig "RequestLimitExceeded"
{-# INLINEABLE _RequestLimitExceeded #-}
{-# DEPRECATED _RequestLimitExceeded "Use generic-lens or generic-optics instead"  #-}

-- | No instance exists with the specified ID, or the instance was recently registered, and information about the instance hasn't propagated yet.
_InstanceNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InstanceNotFound
  = Core._MatchServiceError mkServiceConfig "InstanceNotFound"
{-# INLINEABLE _InstanceNotFound #-}
{-# DEPRECATED _InstanceNotFound "Use generic-lens or generic-optics instead"  #-}

-- | The operation is already in progress.
_DuplicateRequest :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateRequest
  = Core._MatchServiceError mkServiceConfig "DuplicateRequest"
{-# INLINEABLE _DuplicateRequest #-}
{-# DEPRECATED _DuplicateRequest "Use generic-lens or generic-optics instead"  #-}

-- | No service exists with the specified ID.
_ServiceNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceNotFound
  = Core._MatchServiceError mkServiceConfig "ServiceNotFound"
{-# INLINEABLE _ServiceNotFound #-}
{-# DEPRECATED _ServiceNotFound "Use generic-lens or generic-optics instead"  #-}

-- | No operation exists with the specified ID.
_OperationNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationNotFound
  = Core._MatchServiceError mkServiceConfig "OperationNotFound"
{-# INLINEABLE _OperationNotFound #-}
{-# DEPRECATED _OperationNotFound "Use generic-lens or generic-optics instead"  #-}

-- | The operation can't be completed because the resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}
