{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Route53AutoNaming.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _CustomHealthNotFound,
    _DuplicateRequest,
    _InstanceNotFound,
    _InvalidInput,
    _NamespaceAlreadyExists,
    _NamespaceNotFound,
    _OperationNotFound,
    _RequestLimitExceeded,
    _ResourceInUse,
    _ResourceLimitExceeded,
    _ResourceNotFoundException,
    _ServiceAlreadyExists,
    _ServiceNotFound,
    _TooManyTagsException,

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
    httpInstanceSummary_attributes,
    httpInstanceSummary_healthStatus,
    httpInstanceSummary_instanceId,
    httpInstanceSummary_namespaceName,
    httpInstanceSummary_serviceName,

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
    instance_attributes,
    instance_creatorRequestId,
    instance_id,

    -- * InstanceSummary
    InstanceSummary (..),
    newInstanceSummary,
    instanceSummary_attributes,
    instanceSummary_id,

    -- * Namespace
    Namespace (..),
    newNamespace,
    namespace_arn,
    namespace_createDate,
    namespace_creatorRequestId,
    namespace_description,
    namespace_id,
    namespace_name,
    namespace_properties,
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
    namespaceProperties_dnsProperties,
    namespaceProperties_httpProperties,

    -- * NamespaceSummary
    NamespaceSummary (..),
    newNamespaceSummary,
    namespaceSummary_arn,
    namespaceSummary_createDate,
    namespaceSummary_description,
    namespaceSummary_id,
    namespaceSummary_name,
    namespaceSummary_properties,
    namespaceSummary_serviceCount,
    namespaceSummary_type,

    -- * Operation
    Operation (..),
    newOperation,
    operation_createDate,
    operation_errorCode,
    operation_errorMessage,
    operation_id,
    operation_status,
    operation_targets,
    operation_type,
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
    operationSummary_id,
    operationSummary_status,

    -- * PrivateDnsNamespaceChange
    PrivateDnsNamespaceChange (..),
    newPrivateDnsNamespaceChange,
    privateDnsNamespaceChange_description,
    privateDnsNamespaceChange_properties,

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
    publicDnsNamespaceChange_description,
    publicDnsNamespaceChange_properties,

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
    serviceChange_description,
    serviceChange_dnsConfig,
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
    serviceInfo_arn,
    serviceInfo_createDate,
    serviceInfo_creatorRequestId,
    serviceInfo_description,
    serviceInfo_dnsConfig,
    serviceInfo_healthCheckConfig,
    serviceInfo_healthCheckCustomConfig,
    serviceInfo_id,
    serviceInfo_instanceCount,
    serviceInfo_name,
    serviceInfo_namespaceId,
    serviceInfo_type,

    -- * ServiceSummary
    ServiceSummary (..),
    newServiceSummary,
    serviceSummary_arn,
    serviceSummary_createDate,
    serviceSummary_description,
    serviceSummary_dnsConfig,
    serviceSummary_healthCheckConfig,
    serviceSummary_healthCheckCustomConfig,
    serviceSummary_id,
    serviceSummary_instanceCount,
    serviceSummary_name,
    serviceSummary_type,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53AutoNaming.Types.CustomHealthStatus
import Amazonka.Route53AutoNaming.Types.DnsConfig
import Amazonka.Route53AutoNaming.Types.DnsConfigChange
import Amazonka.Route53AutoNaming.Types.DnsProperties
import Amazonka.Route53AutoNaming.Types.DnsRecord
import Amazonka.Route53AutoNaming.Types.FilterCondition
import Amazonka.Route53AutoNaming.Types.HealthCheckConfig
import Amazonka.Route53AutoNaming.Types.HealthCheckCustomConfig
import Amazonka.Route53AutoNaming.Types.HealthCheckType
import Amazonka.Route53AutoNaming.Types.HealthStatus
import Amazonka.Route53AutoNaming.Types.HealthStatusFilter
import Amazonka.Route53AutoNaming.Types.HttpInstanceSummary
import Amazonka.Route53AutoNaming.Types.HttpNamespaceChange
import Amazonka.Route53AutoNaming.Types.HttpProperties
import Amazonka.Route53AutoNaming.Types.Instance
import Amazonka.Route53AutoNaming.Types.InstanceSummary
import Amazonka.Route53AutoNaming.Types.Namespace
import Amazonka.Route53AutoNaming.Types.NamespaceFilter
import Amazonka.Route53AutoNaming.Types.NamespaceFilterName
import Amazonka.Route53AutoNaming.Types.NamespaceProperties
import Amazonka.Route53AutoNaming.Types.NamespaceSummary
import Amazonka.Route53AutoNaming.Types.NamespaceType
import Amazonka.Route53AutoNaming.Types.Operation
import Amazonka.Route53AutoNaming.Types.OperationFilter
import Amazonka.Route53AutoNaming.Types.OperationFilterName
import Amazonka.Route53AutoNaming.Types.OperationStatus
import Amazonka.Route53AutoNaming.Types.OperationSummary
import Amazonka.Route53AutoNaming.Types.OperationTargetType
import Amazonka.Route53AutoNaming.Types.OperationType
import Amazonka.Route53AutoNaming.Types.PrivateDnsNamespaceChange
import Amazonka.Route53AutoNaming.Types.PrivateDnsNamespaceProperties
import Amazonka.Route53AutoNaming.Types.PrivateDnsNamespacePropertiesChange
import Amazonka.Route53AutoNaming.Types.PrivateDnsPropertiesMutable
import Amazonka.Route53AutoNaming.Types.PrivateDnsPropertiesMutableChange
import Amazonka.Route53AutoNaming.Types.PublicDnsNamespaceChange
import Amazonka.Route53AutoNaming.Types.PublicDnsNamespaceProperties
import Amazonka.Route53AutoNaming.Types.PublicDnsNamespacePropertiesChange
import Amazonka.Route53AutoNaming.Types.PublicDnsPropertiesMutable
import Amazonka.Route53AutoNaming.Types.PublicDnsPropertiesMutableChange
import Amazonka.Route53AutoNaming.Types.RecordType
import Amazonka.Route53AutoNaming.Types.RoutingPolicy
import Amazonka.Route53AutoNaming.Types.SOA
import Amazonka.Route53AutoNaming.Types.SOAChange
import Amazonka.Route53AutoNaming.Types.ServiceChange
import Amazonka.Route53AutoNaming.Types.ServiceFilter
import Amazonka.Route53AutoNaming.Types.ServiceFilterName
import Amazonka.Route53AutoNaming.Types.ServiceInfo
import Amazonka.Route53AutoNaming.Types.ServiceSummary
import Amazonka.Route53AutoNaming.Types.ServiceType
import Amazonka.Route53AutoNaming.Types.ServiceTypeOption
import Amazonka.Route53AutoNaming.Types.Tag
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-03-14@ of the Amazon Cloud Map SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Route53AutoNaming",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "servicediscovery",
      Core.signingName = "servicediscovery",
      Core.version = "2017-03-14",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Route53AutoNaming",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The health check for the instance that\'s specified by @ServiceId@ and
-- @InstanceId@ isn\'t a custom health check.
_CustomHealthNotFound :: Core.AsError a => Lens.Fold a Core.ServiceError
_CustomHealthNotFound =
  Core._MatchServiceError
    defaultService
    "CustomHealthNotFound"

-- | The operation is already in progress.
_DuplicateRequest :: Core.AsError a => Lens.Fold a Core.ServiceError
_DuplicateRequest =
  Core._MatchServiceError
    defaultService
    "DuplicateRequest"

-- | No instance exists with the specified ID, or the instance was recently
-- registered, and information about the instance hasn\'t propagated yet.
_InstanceNotFound :: Core.AsError a => Lens.Fold a Core.ServiceError
_InstanceNotFound =
  Core._MatchServiceError
    defaultService
    "InstanceNotFound"

-- | One or more specified values aren\'t valid. For example, a required
-- value might be missing, a numeric value might be outside the allowed
-- range, or a string value might exceed length constraints.
_InvalidInput :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidInput =
  Core._MatchServiceError
    defaultService
    "InvalidInput"

-- | The namespace that you\'re trying to create already exists.
_NamespaceAlreadyExists :: Core.AsError a => Lens.Fold a Core.ServiceError
_NamespaceAlreadyExists =
  Core._MatchServiceError
    defaultService
    "NamespaceAlreadyExists"

-- | No namespace exists with the specified ID.
_NamespaceNotFound :: Core.AsError a => Lens.Fold a Core.ServiceError
_NamespaceNotFound =
  Core._MatchServiceError
    defaultService
    "NamespaceNotFound"

-- | No operation exists with the specified ID.
_OperationNotFound :: Core.AsError a => Lens.Fold a Core.ServiceError
_OperationNotFound =
  Core._MatchServiceError
    defaultService
    "OperationNotFound"

-- | The operation can\'t be completed because you\'ve reached the quota for
-- the number of requests. For more information, see
-- <https://docs.aws.amazon.com/cloud-map/latest/dg/throttling.html Cloud Map API request throttling quota>
-- in the /Cloud Map Developer Guide/.
_RequestLimitExceeded :: Core.AsError a => Lens.Fold a Core.ServiceError
_RequestLimitExceeded =
  Core._MatchServiceError
    defaultService
    "RequestLimitExceeded"

-- | The specified resource can\'t be deleted because it contains other
-- resources. For example, you can\'t delete a service that contains any
-- instances.
_ResourceInUse :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceInUse =
  Core._MatchServiceError
    defaultService
    "ResourceInUse"

-- | The resource can\'t be created because you\'ve reached the quota on the
-- number of resources.
_ResourceLimitExceeded :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceeded"

-- | The operation can\'t be completed because the resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The service can\'t be created because a service with the same name
-- already exists.
_ServiceAlreadyExists :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceAlreadyExists =
  Core._MatchServiceError
    defaultService
    "ServiceAlreadyExists"

-- | No service exists with the specified ID.
_ServiceNotFound :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceNotFound =
  Core._MatchServiceError
    defaultService
    "ServiceNotFound"

-- | The list of tags on the resource is over the quota. The maximum number
-- of tags that can be applied to a resource is 50.
_TooManyTagsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"
