-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types
  ( -- * Service configuration
    route53AutoNamingService,

    -- * Errors

    -- * CustomHealthStatus
    CustomHealthStatus (..),

    -- * FilterCondition
    FilterCondition (..),

    -- * HealthCheckType
    HealthCheckType (..),

    -- * HealthStatus
    HealthStatus (..),

    -- * HealthStatusFilter
    HealthStatusFilter (..),

    -- * NamespaceFilterName
    NamespaceFilterName (..),

    -- * NamespaceType
    NamespaceType (..),

    -- * OperationFilterName
    OperationFilterName (..),

    -- * OperationStatus
    OperationStatus (..),

    -- * OperationTargetType
    OperationTargetType (..),

    -- * OperationType
    OperationType (..),

    -- * RecordType
    RecordType (..),

    -- * RoutingPolicy
    RoutingPolicy (..),

    -- * ServiceFilterName
    ServiceFilterName (..),

    -- * DNSConfig
    DNSConfig (..),
    mkDNSConfig,
    dcRoutingPolicy,
    dcNamespaceId,
    dcDNSRecords,

    -- * DNSConfigChange
    DNSConfigChange (..),
    mkDNSConfigChange,
    dccDNSRecords,

    -- * DNSProperties
    DNSProperties (..),
    mkDNSProperties,
    dpHostedZoneId,

    -- * DNSRecord
    DNSRecord (..),
    mkDNSRecord,
    drType,
    drTTL,

    -- * HTTPInstanceSummary
    HTTPInstanceSummary (..),
    mkHTTPInstanceSummary,
    httpisInstanceId,
    httpisNamespaceName,
    httpisAttributes,
    httpisServiceName,
    httpisHealthStatus,

    -- * HTTPProperties
    HTTPProperties (..),
    mkHTTPProperties,
    httppHTTPName,

    -- * HealthCheckConfig
    HealthCheckConfig (..),
    mkHealthCheckConfig,
    hccFailureThreshold,
    hccResourcePath,
    hccType,

    -- * HealthCheckCustomConfig
    HealthCheckCustomConfig (..),
    mkHealthCheckCustomConfig,
    hcccFailureThreshold,

    -- * Instance
    Instance (..),
    mkInstance,
    iCreatorRequestId,
    iAttributes,
    iId,

    -- * InstanceSummary
    InstanceSummary (..),
    mkInstanceSummary,
    isAttributes,
    isId,

    -- * Namespace
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

    -- * NamespaceFilter
    NamespaceFilter (..),
    mkNamespaceFilter,
    nfCondition,
    nfName,
    nfValues,

    -- * NamespaceProperties
    NamespaceProperties (..),
    mkNamespaceProperties,
    npDNSProperties,
    npHTTPProperties,

    -- * NamespaceSummary
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

    -- * Operation
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

    -- * OperationFilter
    OperationFilter (..),
    mkOperationFilter,
    ofCondition,
    ofName,
    ofValues,

    -- * OperationSummary
    OperationSummary (..),
    mkOperationSummary,
    osStatus,
    osId,

    -- * ServiceChange
    ServiceChange (..),
    mkServiceChange,
    scHealthCheckConfig,
    scDNSConfig,
    scDescription,

    -- * ServiceFilter
    ServiceFilter (..),
    mkServiceFilter,
    sfCondition,
    sfName,
    sfValues,

    -- * ServiceInfo
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

    -- * ServiceSummary
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

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53AutoNaming.Types.CustomHealthStatus
import Network.AWS.Route53AutoNaming.Types.DNSConfig
import Network.AWS.Route53AutoNaming.Types.DNSConfigChange
import Network.AWS.Route53AutoNaming.Types.DNSProperties
import Network.AWS.Route53AutoNaming.Types.DNSRecord
import Network.AWS.Route53AutoNaming.Types.FilterCondition
import Network.AWS.Route53AutoNaming.Types.HTTPInstanceSummary
import Network.AWS.Route53AutoNaming.Types.HTTPProperties
import Network.AWS.Route53AutoNaming.Types.HealthCheckConfig
import Network.AWS.Route53AutoNaming.Types.HealthCheckCustomConfig
import Network.AWS.Route53AutoNaming.Types.HealthCheckType
import Network.AWS.Route53AutoNaming.Types.HealthStatus
import Network.AWS.Route53AutoNaming.Types.HealthStatusFilter
import Network.AWS.Route53AutoNaming.Types.Instance
import Network.AWS.Route53AutoNaming.Types.InstanceSummary
import Network.AWS.Route53AutoNaming.Types.Namespace
import Network.AWS.Route53AutoNaming.Types.NamespaceFilter
import Network.AWS.Route53AutoNaming.Types.NamespaceFilterName
import Network.AWS.Route53AutoNaming.Types.NamespaceProperties
import Network.AWS.Route53AutoNaming.Types.NamespaceSummary
import Network.AWS.Route53AutoNaming.Types.NamespaceType
import Network.AWS.Route53AutoNaming.Types.Operation
import Network.AWS.Route53AutoNaming.Types.OperationFilter
import Network.AWS.Route53AutoNaming.Types.OperationFilterName
import Network.AWS.Route53AutoNaming.Types.OperationStatus
import Network.AWS.Route53AutoNaming.Types.OperationSummary
import Network.AWS.Route53AutoNaming.Types.OperationTargetType
import Network.AWS.Route53AutoNaming.Types.OperationType
import Network.AWS.Route53AutoNaming.Types.RecordType
import Network.AWS.Route53AutoNaming.Types.RoutingPolicy
import Network.AWS.Route53AutoNaming.Types.ServiceChange
import Network.AWS.Route53AutoNaming.Types.ServiceFilter
import Network.AWS.Route53AutoNaming.Types.ServiceFilterName
import Network.AWS.Route53AutoNaming.Types.ServiceInfo
import Network.AWS.Route53AutoNaming.Types.ServiceSummary
import Network.AWS.Route53AutoNaming.Types.Tag
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-03-14@ of the Amazon Cloud Map SDK configuration.
route53AutoNamingService :: Lude.Service
route53AutoNamingService =
  Lude.Service
    { Lude._svcAbbrev = "Route53AutoNaming",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "servicediscovery",
      Lude._svcVersion = "2017-03-14",
      Lude._svcEndpoint = Lude.defaultEndpoint route53AutoNamingService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Route53AutoNaming",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
