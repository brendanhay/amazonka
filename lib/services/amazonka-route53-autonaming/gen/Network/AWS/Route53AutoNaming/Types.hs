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
    _CustomHealthNotFound,
    _ResourceLimitExceeded,
    _ResourceNotFoundException,
    _OperationNotFound,
    _ServiceNotFound,
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
    dnsProperties_soa,

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
    httpInstanceSummary_instanceId,
    httpInstanceSummary_namespaceName,
    httpInstanceSummary_serviceName,
    httpInstanceSummary_attributes,
    httpInstanceSummary_healthStatus,

    -- * HttpNamespaceChange
    HttpNamespaceChange (..),
    newHttpNamespaceChange,
    httpNamespaceChange_description,

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
    namespace_description,
    namespace_serviceCount,
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
    namespaceSummary_description,
    namespaceSummary_serviceCount,
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

    -- * PrivateDnsNamespaceChange
    PrivateDnsNamespaceChange (..),
    newPrivateDnsNamespaceChange,
    privateDnsNamespaceChange_properties,
    privateDnsNamespaceChange_description,

    -- * PrivateDnsNamespaceProperties
    PrivateDnsNamespaceProperties (..),
    newPrivateDnsNamespaceProperties,
    privateDnsNamespaceProperties_dnsProperties,

    -- * PrivateDnsNamespacePropertiesChange
    PrivateDnsNamespacePropertiesChange (..),
    newPrivateDnsNamespacePropertiesChange,
    privateDnsNamespacePropertiesChange_dnsProperties,

    -- * PrivateDnsPropertiesMutable
    PrivateDnsPropertiesMutable (..),
    newPrivateDnsPropertiesMutable,
    privateDnsPropertiesMutable_soa,

    -- * PrivateDnsPropertiesMutableChange
    PrivateDnsPropertiesMutableChange (..),
    newPrivateDnsPropertiesMutableChange,
    privateDnsPropertiesMutableChange_soa,

    -- * PublicDnsNamespaceChange
    PublicDnsNamespaceChange (..),
    newPublicDnsNamespaceChange,
    publicDnsNamespaceChange_properties,
    publicDnsNamespaceChange_description,

    -- * PublicDnsNamespaceProperties
    PublicDnsNamespaceProperties (..),
    newPublicDnsNamespaceProperties,
    publicDnsNamespaceProperties_dnsProperties,

    -- * PublicDnsNamespacePropertiesChange
    PublicDnsNamespacePropertiesChange (..),
    newPublicDnsNamespacePropertiesChange,
    publicDnsNamespacePropertiesChange_dnsProperties,

    -- * PublicDnsPropertiesMutable
    PublicDnsPropertiesMutable (..),
    newPublicDnsPropertiesMutable,
    publicDnsPropertiesMutable_soa,

    -- * PublicDnsPropertiesMutableChange
    PublicDnsPropertiesMutableChange (..),
    newPublicDnsPropertiesMutableChange,
    publicDnsPropertiesMutableChange_soa,

    -- * SOA
    SOA (..),
    newSOA,
    soa_ttl,

    -- * SOAChange
    SOAChange (..),
    newSOAChange,
    sOAChange_ttl,

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
    serviceInfo_healthCheckConfig,
    serviceInfo_type,
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
    serviceSummary_healthCheckConfig,
    serviceSummary_type,
    serviceSummary_instanceCount,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Network.AWS.Core as Core
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
import Network.AWS.Route53AutoNaming.Types.HttpNamespaceChange
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
import Network.AWS.Route53AutoNaming.Types.PrivateDnsNamespaceChange
import Network.AWS.Route53AutoNaming.Types.PrivateDnsNamespaceProperties
import Network.AWS.Route53AutoNaming.Types.PrivateDnsNamespacePropertiesChange
import Network.AWS.Route53AutoNaming.Types.PrivateDnsPropertiesMutable
import Network.AWS.Route53AutoNaming.Types.PrivateDnsPropertiesMutableChange
import Network.AWS.Route53AutoNaming.Types.PublicDnsNamespaceChange
import Network.AWS.Route53AutoNaming.Types.PublicDnsNamespaceProperties
import Network.AWS.Route53AutoNaming.Types.PublicDnsNamespacePropertiesChange
import Network.AWS.Route53AutoNaming.Types.PublicDnsPropertiesMutable
import Network.AWS.Route53AutoNaming.Types.PublicDnsPropertiesMutableChange
import Network.AWS.Route53AutoNaming.Types.RecordType
import Network.AWS.Route53AutoNaming.Types.RoutingPolicy
import Network.AWS.Route53AutoNaming.Types.SOA
import Network.AWS.Route53AutoNaming.Types.SOAChange
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
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "Route53AutoNaming",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "servicediscovery",
      Core._serviceSigningName = "servicediscovery",
      Core._serviceVersion = "2017-03-14",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "Route53AutoNaming",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | One or more specified values aren\'t valid. For example, a required
-- value might be missing, a numeric value might be outside the allowed
-- range, or a string value might exceed length constraints.
_InvalidInput :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInput =
  Core._MatchServiceError
    defaultService
    "InvalidInput"

-- | The list of tags on the resource is over the quota. The maximum number
-- of tags that can be applied to a resource is 50.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The operation is already in progress.
_DuplicateRequest :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateRequest =
  Core._MatchServiceError
    defaultService
    "DuplicateRequest"

-- | The specified resource can\'t be deleted because it contains other
-- resources. For example, you can\'t delete a service that contains any
-- instances.
_ResourceInUse :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUse =
  Core._MatchServiceError
    defaultService
    "ResourceInUse"

-- | The service can\'t be created because a service with the same name
-- already exists.
_ServiceAlreadyExists :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceAlreadyExists =
  Core._MatchServiceError
    defaultService
    "ServiceAlreadyExists"

-- | The operation can\'t be completed because you\'ve reached the quota for
-- the number of requests. For more information, see
-- <https://docs.aws.amazon.com/cloud-map/latest/dg/throttling.html Cloud Map API request throttling quota>
-- in the /Cloud Map Developer Guide/.
_RequestLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RequestLimitExceeded =
  Core._MatchServiceError
    defaultService
    "RequestLimitExceeded"

-- | The health check for the instance that\'s specified by @ServiceId@ and
-- @InstanceId@ isn\'t a custom health check.
_CustomHealthNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CustomHealthNotFound =
  Core._MatchServiceError
    defaultService
    "CustomHealthNotFound"

-- | The resource can\'t be created because you\'ve reached the quota on the
-- number of resources.
_ResourceLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceeded"

-- | The operation can\'t be completed because the resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | No operation exists with the specified ID.
_OperationNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationNotFound =
  Core._MatchServiceError
    defaultService
    "OperationNotFound"

-- | No service exists with the specified ID.
_ServiceNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceNotFound =
  Core._MatchServiceError
    defaultService
    "ServiceNotFound"

-- | No namespace exists with the specified ID.
_NamespaceNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NamespaceNotFound =
  Core._MatchServiceError
    defaultService
    "NamespaceNotFound"

-- | The namespace that you\'re trying to create already exists.
_NamespaceAlreadyExists :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NamespaceAlreadyExists =
  Core._MatchServiceError
    defaultService
    "NamespaceAlreadyExists"

-- | No instance exists with the specified ID, or the instance was recently
-- registered, and information about the instance hasn\'t propagated yet.
_InstanceNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InstanceNotFound =
  Core._MatchServiceError
    defaultService
    "InstanceNotFound"
