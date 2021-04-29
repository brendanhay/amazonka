{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidInput,
    _TooManyTagsException,
    _DuplicateRequest,
    _ResourceInUse,
    _ServiceAlreadyExists,
    _RequestLimitExceeded,
    _ResourceLimitExceeded,
    _CustomHealthNotFound,
    _OperationNotFound,
    _ServiceNotFound,
    _ResourceNotFoundException,
    _NamespaceNotFound,
    _NamespaceAlreadyExists,
    _InstanceNotFound,

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

    -- * ServiceType
    ServiceType (..),

    -- * ServiceTypeOption
    ServiceTypeOption (..),

    -- * DnsConfig
    DnsConfig (..),
    newDnsConfig,
    dnsConfig_namespaceId,
    dnsConfig_routingPolicy,
    dnsConfig_dnsRecords,

    -- * DnsConfigChange
    DnsConfigChange (..),
    newDnsConfigChange,
    dnsConfigChange_dnsRecords,

    -- * DnsProperties
    DnsProperties (..),
    newDnsProperties,
    dnsProperties_hostedZoneId,

    -- * DnsRecord
    DnsRecord (..),
    newDnsRecord,
    dnsRecord_type,
    dnsRecord_ttl,

    -- * HealthCheckConfig
    HealthCheckConfig (..),
    newHealthCheckConfig,
    healthCheckConfig_failureThreshold,
    healthCheckConfig_resourcePath,
    healthCheckConfig_type,

    -- * HealthCheckCustomConfig
    HealthCheckCustomConfig (..),
    newHealthCheckCustomConfig,
    healthCheckCustomConfig_failureThreshold,

    -- * HttpInstanceSummary
    HttpInstanceSummary (..),
    newHttpInstanceSummary,
    httpInstanceSummary_namespaceName,
    httpInstanceSummary_instanceId,
    httpInstanceSummary_serviceName,
    httpInstanceSummary_attributes,
    httpInstanceSummary_healthStatus,

    -- * HttpProperties
    HttpProperties (..),
    newHttpProperties,
    httpProperties_httpName,

    -- * Instance
    Instance (..),
    newInstance,
    instance_creatorRequestId,
    instance_attributes,
    instance_id,

    -- * InstanceSummary
    InstanceSummary (..),
    newInstanceSummary,
    instanceSummary_id,
    instanceSummary_attributes,

    -- * Namespace
    Namespace (..),
    newNamespace,
    namespace_createDate,
    namespace_creatorRequestId,
    namespace_arn,
    namespace_id,
    namespace_name,
    namespace_properties,
    namespace_serviceCount,
    namespace_description,
    namespace_type,

    -- * NamespaceFilter
    NamespaceFilter (..),
    newNamespaceFilter,
    namespaceFilter_condition,
    namespaceFilter_name,
    namespaceFilter_values,

    -- * NamespaceProperties
    NamespaceProperties (..),
    newNamespaceProperties,
    namespaceProperties_httpProperties,
    namespaceProperties_dnsProperties,

    -- * NamespaceSummary
    NamespaceSummary (..),
    newNamespaceSummary,
    namespaceSummary_createDate,
    namespaceSummary_arn,
    namespaceSummary_id,
    namespaceSummary_name,
    namespaceSummary_properties,
    namespaceSummary_serviceCount,
    namespaceSummary_description,
    namespaceSummary_type,

    -- * Operation
    Operation (..),
    newOperation,
    operation_status,
    operation_createDate,
    operation_id,
    operation_targets,
    operation_errorMessage,
    operation_type,
    operation_errorCode,
    operation_updateDate,

    -- * OperationFilter
    OperationFilter (..),
    newOperationFilter,
    operationFilter_condition,
    operationFilter_name,
    operationFilter_values,

    -- * OperationSummary
    OperationSummary (..),
    newOperationSummary,
    operationSummary_status,
    operationSummary_id,

    -- * ServiceChange
    ServiceChange (..),
    newServiceChange,
    serviceChange_dnsConfig,
    serviceChange_description,
    serviceChange_healthCheckConfig,

    -- * ServiceFilter
    ServiceFilter (..),
    newServiceFilter,
    serviceFilter_condition,
    serviceFilter_name,
    serviceFilter_values,

    -- * ServiceInfo
    ServiceInfo (..),
    newServiceInfo,
    serviceInfo_namespaceId,
    serviceInfo_dnsConfig,
    serviceInfo_createDate,
    serviceInfo_creatorRequestId,
    serviceInfo_arn,
    serviceInfo_id,
    serviceInfo_name,
    serviceInfo_description,
    serviceInfo_healthCheckCustomConfig,
    serviceInfo_type,
    serviceInfo_healthCheckConfig,
    serviceInfo_instanceCount,

    -- * ServiceSummary
    ServiceSummary (..),
    newServiceSummary,
    serviceSummary_dnsConfig,
    serviceSummary_createDate,
    serviceSummary_arn,
    serviceSummary_id,
    serviceSummary_name,
    serviceSummary_description,
    serviceSummary_healthCheckCustomConfig,
    serviceSummary_type,
    serviceSummary_healthCheckConfig,
    serviceSummary_instanceCount,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53AutoNaming.Types.CustomHealthStatus
import Network.AWS.Route53AutoNaming.Types.DnsConfig
import Network.AWS.Route53AutoNaming.Types.DnsConfigChange
import Network.AWS.Route53AutoNaming.Types.DnsProperties
import Network.AWS.Route53AutoNaming.Types.DnsRecord
import Network.AWS.Route53AutoNaming.Types.FilterCondition
import Network.AWS.Route53AutoNaming.Types.HealthCheckConfig
import Network.AWS.Route53AutoNaming.Types.HealthCheckCustomConfig
import Network.AWS.Route53AutoNaming.Types.HealthCheckType
import Network.AWS.Route53AutoNaming.Types.HealthStatus
import Network.AWS.Route53AutoNaming.Types.HealthStatusFilter
import Network.AWS.Route53AutoNaming.Types.HttpInstanceSummary
import Network.AWS.Route53AutoNaming.Types.HttpProperties
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
import Network.AWS.Route53AutoNaming.Types.ServiceType
import Network.AWS.Route53AutoNaming.Types.ServiceTypeOption
import Network.AWS.Route53AutoNaming.Types.Tag
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-03-14@ of the Amazon Cloud Map SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "Route53AutoNaming",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "servicediscovery",
      Prelude._svcVersion = "2017-03-14",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "Route53AutoNaming",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | One or more specified values aren\'t valid. For example, a required
-- value might be missing, a numeric value might be outside the allowed
-- range, or a string value might exceed length constraints.
_InvalidInput :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidInput =
  Prelude._MatchServiceError
    defaultService
    "InvalidInput"

-- | The list of tags on the resource is over the quota. The maximum number
-- of tags that can be applied to a resource is 50.
_TooManyTagsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyTagsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The operation is already in progress.
_DuplicateRequest :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DuplicateRequest =
  Prelude._MatchServiceError
    defaultService
    "DuplicateRequest"

-- | The specified resource can\'t be deleted because it contains other
-- resources. For example, you can\'t delete a service that contains any
-- instances.
_ResourceInUse :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceInUse =
  Prelude._MatchServiceError
    defaultService
    "ResourceInUse"

-- | The service can\'t be created because a service with the same name
-- already exists.
_ServiceAlreadyExists :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ServiceAlreadyExists =
  Prelude._MatchServiceError
    defaultService
    "ServiceAlreadyExists"

-- | The operation can\'t be completed because you\'ve reached the quota for
-- the number of requests. For more information, see
-- <https://docs.aws.amazon.com/cloud-map/latest/dg/throttling.html AWS Cloud Map API request throttling quota>
-- in the /AWS Cloud Map Developer Guide/.
_RequestLimitExceeded :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RequestLimitExceeded =
  Prelude._MatchServiceError
    defaultService
    "RequestLimitExceeded"

-- | The resource can\'t be created because you\'ve reached the quota on the
-- number of resources.
_ResourceLimitExceeded :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceLimitExceeded =
  Prelude._MatchServiceError
    defaultService
    "ResourceLimitExceeded"

-- | The health check for the instance that is specified by @ServiceId@ and
-- @InstanceId@ is not a custom health check.
_CustomHealthNotFound :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CustomHealthNotFound =
  Prelude._MatchServiceError
    defaultService
    "CustomHealthNotFound"

-- | No operation exists with the specified ID.
_OperationNotFound :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_OperationNotFound =
  Prelude._MatchServiceError
    defaultService
    "OperationNotFound"

-- | No service exists with the specified ID.
_ServiceNotFound :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ServiceNotFound =
  Prelude._MatchServiceError
    defaultService
    "ServiceNotFound"

-- | The operation can\'t be completed because the resource was not found.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | No namespace exists with the specified ID.
_NamespaceNotFound :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NamespaceNotFound =
  Prelude._MatchServiceError
    defaultService
    "NamespaceNotFound"

-- | The namespace that you\'re trying to create already exists.
_NamespaceAlreadyExists :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NamespaceAlreadyExists =
  Prelude._MatchServiceError
    defaultService
    "NamespaceAlreadyExists"

-- | No instance exists with the specified ID, or the instance was recently
-- registered, and information about the instance hasn\'t propagated yet.
_InstanceNotFound :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InstanceNotFound =
  Prelude._MatchServiceError
    defaultService
    "InstanceNotFound"
