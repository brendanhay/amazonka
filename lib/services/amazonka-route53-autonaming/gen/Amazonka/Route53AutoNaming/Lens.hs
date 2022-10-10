{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Route53AutoNaming.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Lens
  ( -- * Operations

    -- ** CreateHttpNamespace
    createHttpNamespace_tags,
    createHttpNamespace_description,
    createHttpNamespace_creatorRequestId,
    createHttpNamespace_name,
    createHttpNamespaceResponse_operationId,
    createHttpNamespaceResponse_httpStatus,

    -- ** CreatePrivateDnsNamespace
    createPrivateDnsNamespace_tags,
    createPrivateDnsNamespace_properties,
    createPrivateDnsNamespace_description,
    createPrivateDnsNamespace_creatorRequestId,
    createPrivateDnsNamespace_name,
    createPrivateDnsNamespace_vpc,
    createPrivateDnsNamespaceResponse_operationId,
    createPrivateDnsNamespaceResponse_httpStatus,

    -- ** CreatePublicDnsNamespace
    createPublicDnsNamespace_tags,
    createPublicDnsNamespace_properties,
    createPublicDnsNamespace_description,
    createPublicDnsNamespace_creatorRequestId,
    createPublicDnsNamespace_name,
    createPublicDnsNamespaceResponse_operationId,
    createPublicDnsNamespaceResponse_httpStatus,

    -- ** CreateService
    createService_tags,
    createService_type,
    createService_dnsConfig,
    createService_healthCheckCustomConfig,
    createService_description,
    createService_creatorRequestId,
    createService_namespaceId,
    createService_healthCheckConfig,
    createService_name,
    createServiceResponse_service,
    createServiceResponse_httpStatus,

    -- ** DeleteNamespace
    deleteNamespace_id,
    deleteNamespaceResponse_operationId,
    deleteNamespaceResponse_httpStatus,

    -- ** DeleteService
    deleteService_id,
    deleteServiceResponse_httpStatus,

    -- ** DeregisterInstance
    deregisterInstance_serviceId,
    deregisterInstance_instanceId,
    deregisterInstanceResponse_operationId,
    deregisterInstanceResponse_httpStatus,

    -- ** DiscoverInstances
    discoverInstances_optionalParameters,
    discoverInstances_healthStatus,
    discoverInstances_queryParameters,
    discoverInstances_maxResults,
    discoverInstances_namespaceName,
    discoverInstances_serviceName,
    discoverInstancesResponse_instances,
    discoverInstancesResponse_httpStatus,

    -- ** GetInstance
    getInstance_serviceId,
    getInstance_instanceId,
    getInstanceResponse_instance,
    getInstanceResponse_httpStatus,

    -- ** GetInstancesHealthStatus
    getInstancesHealthStatus_instances,
    getInstancesHealthStatus_nextToken,
    getInstancesHealthStatus_maxResults,
    getInstancesHealthStatus_serviceId,
    getInstancesHealthStatusResponse_nextToken,
    getInstancesHealthStatusResponse_status,
    getInstancesHealthStatusResponse_httpStatus,

    -- ** GetNamespace
    getNamespace_id,
    getNamespaceResponse_namespace,
    getNamespaceResponse_httpStatus,

    -- ** GetOperation
    getOperation_operationId,
    getOperationResponse_operation,
    getOperationResponse_httpStatus,

    -- ** GetService
    getService_id,
    getServiceResponse_service,
    getServiceResponse_httpStatus,

    -- ** ListInstances
    listInstances_nextToken,
    listInstances_maxResults,
    listInstances_serviceId,
    listInstancesResponse_instances,
    listInstancesResponse_nextToken,
    listInstancesResponse_httpStatus,

    -- ** ListNamespaces
    listNamespaces_nextToken,
    listNamespaces_filters,
    listNamespaces_maxResults,
    listNamespacesResponse_nextToken,
    listNamespacesResponse_namespaces,
    listNamespacesResponse_httpStatus,

    -- ** ListOperations
    listOperations_nextToken,
    listOperations_filters,
    listOperations_maxResults,
    listOperationsResponse_nextToken,
    listOperationsResponse_operations,
    listOperationsResponse_httpStatus,

    -- ** ListServices
    listServices_nextToken,
    listServices_filters,
    listServices_maxResults,
    listServicesResponse_nextToken,
    listServicesResponse_services,
    listServicesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** RegisterInstance
    registerInstance_creatorRequestId,
    registerInstance_serviceId,
    registerInstance_instanceId,
    registerInstance_attributes,
    registerInstanceResponse_operationId,
    registerInstanceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateHttpNamespace
    updateHttpNamespace_updaterRequestId,
    updateHttpNamespace_id,
    updateHttpNamespace_namespace,
    updateHttpNamespaceResponse_operationId,
    updateHttpNamespaceResponse_httpStatus,

    -- ** UpdateInstanceCustomHealthStatus
    updateInstanceCustomHealthStatus_serviceId,
    updateInstanceCustomHealthStatus_instanceId,
    updateInstanceCustomHealthStatus_status,

    -- ** UpdatePrivateDnsNamespace
    updatePrivateDnsNamespace_updaterRequestId,
    updatePrivateDnsNamespace_id,
    updatePrivateDnsNamespace_namespace,
    updatePrivateDnsNamespaceResponse_operationId,
    updatePrivateDnsNamespaceResponse_httpStatus,

    -- ** UpdatePublicDnsNamespace
    updatePublicDnsNamespace_updaterRequestId,
    updatePublicDnsNamespace_id,
    updatePublicDnsNamespace_namespace,
    updatePublicDnsNamespaceResponse_operationId,
    updatePublicDnsNamespaceResponse_httpStatus,

    -- ** UpdateService
    updateService_id,
    updateService_service,
    updateServiceResponse_operationId,
    updateServiceResponse_httpStatus,

    -- * Types

    -- ** DnsConfig
    dnsConfig_routingPolicy,
    dnsConfig_namespaceId,
    dnsConfig_dnsRecords,

    -- ** DnsConfigChange
    dnsConfigChange_dnsRecords,

    -- ** DnsProperties
    dnsProperties_hostedZoneId,
    dnsProperties_soa,

    -- ** DnsRecord
    dnsRecord_type,
    dnsRecord_ttl,

    -- ** HealthCheckConfig
    healthCheckConfig_failureThreshold,
    healthCheckConfig_resourcePath,
    healthCheckConfig_type,

    -- ** HealthCheckCustomConfig
    healthCheckCustomConfig_failureThreshold,

    -- ** HttpInstanceSummary
    httpInstanceSummary_namespaceName,
    httpInstanceSummary_healthStatus,
    httpInstanceSummary_instanceId,
    httpInstanceSummary_attributes,
    httpInstanceSummary_serviceName,

    -- ** HttpNamespaceChange
    httpNamespaceChange_description,

    -- ** HttpProperties
    httpProperties_httpName,

    -- ** Instance
    instance_creatorRequestId,
    instance_attributes,
    instance_id,

    -- ** InstanceSummary
    instanceSummary_id,
    instanceSummary_attributes,

    -- ** Namespace
    namespace_name,
    namespace_type,
    namespace_properties,
    namespace_arn,
    namespace_id,
    namespace_description,
    namespace_creatorRequestId,
    namespace_createDate,
    namespace_serviceCount,

    -- ** NamespaceFilter
    namespaceFilter_condition,
    namespaceFilter_name,
    namespaceFilter_values,

    -- ** NamespaceProperties
    namespaceProperties_dnsProperties,
    namespaceProperties_httpProperties,

    -- ** NamespaceSummary
    namespaceSummary_name,
    namespaceSummary_type,
    namespaceSummary_properties,
    namespaceSummary_arn,
    namespaceSummary_id,
    namespaceSummary_description,
    namespaceSummary_createDate,
    namespaceSummary_serviceCount,

    -- ** Operation
    operation_type,
    operation_errorMessage,
    operation_status,
    operation_updateDate,
    operation_targets,
    operation_id,
    operation_errorCode,
    operation_createDate,

    -- ** OperationFilter
    operationFilter_condition,
    operationFilter_name,
    operationFilter_values,

    -- ** OperationSummary
    operationSummary_status,
    operationSummary_id,

    -- ** PrivateDnsNamespaceChange
    privateDnsNamespaceChange_properties,
    privateDnsNamespaceChange_description,

    -- ** PrivateDnsNamespaceProperties
    privateDnsNamespaceProperties_dnsProperties,

    -- ** PrivateDnsNamespacePropertiesChange
    privateDnsNamespacePropertiesChange_dnsProperties,

    -- ** PrivateDnsPropertiesMutable
    privateDnsPropertiesMutable_soa,

    -- ** PrivateDnsPropertiesMutableChange
    privateDnsPropertiesMutableChange_soa,

    -- ** PublicDnsNamespaceChange
    publicDnsNamespaceChange_properties,
    publicDnsNamespaceChange_description,

    -- ** PublicDnsNamespaceProperties
    publicDnsNamespaceProperties_dnsProperties,

    -- ** PublicDnsNamespacePropertiesChange
    publicDnsNamespacePropertiesChange_dnsProperties,

    -- ** PublicDnsPropertiesMutable
    publicDnsPropertiesMutable_soa,

    -- ** PublicDnsPropertiesMutableChange
    publicDnsPropertiesMutableChange_soa,

    -- ** SOA
    soa_ttl,

    -- ** SOAChange
    sOAChange_ttl,

    -- ** ServiceChange
    serviceChange_dnsConfig,
    serviceChange_description,
    serviceChange_healthCheckConfig,

    -- ** ServiceFilter
    serviceFilter_condition,
    serviceFilter_name,
    serviceFilter_values,

    -- ** ServiceInfo
    serviceInfo_name,
    serviceInfo_type,
    serviceInfo_dnsConfig,
    serviceInfo_healthCheckCustomConfig,
    serviceInfo_arn,
    serviceInfo_id,
    serviceInfo_description,
    serviceInfo_creatorRequestId,
    serviceInfo_instanceCount,
    serviceInfo_createDate,
    serviceInfo_namespaceId,
    serviceInfo_healthCheckConfig,

    -- ** ServiceSummary
    serviceSummary_name,
    serviceSummary_type,
    serviceSummary_dnsConfig,
    serviceSummary_healthCheckCustomConfig,
    serviceSummary_arn,
    serviceSummary_id,
    serviceSummary_description,
    serviceSummary_instanceCount,
    serviceSummary_createDate,
    serviceSummary_healthCheckConfig,

    -- ** Tag
    tag_key,
    tag_value,
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
import Amazonka.Route53AutoNaming.ListInstances
import Amazonka.Route53AutoNaming.ListNamespaces
import Amazonka.Route53AutoNaming.ListOperations
import Amazonka.Route53AutoNaming.ListServices
import Amazonka.Route53AutoNaming.ListTagsForResource
import Amazonka.Route53AutoNaming.RegisterInstance
import Amazonka.Route53AutoNaming.TagResource
import Amazonka.Route53AutoNaming.Types.DnsConfig
import Amazonka.Route53AutoNaming.Types.DnsConfigChange
import Amazonka.Route53AutoNaming.Types.DnsProperties
import Amazonka.Route53AutoNaming.Types.DnsRecord
import Amazonka.Route53AutoNaming.Types.HealthCheckConfig
import Amazonka.Route53AutoNaming.Types.HealthCheckCustomConfig
import Amazonka.Route53AutoNaming.Types.HttpInstanceSummary
import Amazonka.Route53AutoNaming.Types.HttpNamespaceChange
import Amazonka.Route53AutoNaming.Types.HttpProperties
import Amazonka.Route53AutoNaming.Types.Instance
import Amazonka.Route53AutoNaming.Types.InstanceSummary
import Amazonka.Route53AutoNaming.Types.Namespace
import Amazonka.Route53AutoNaming.Types.NamespaceFilter
import Amazonka.Route53AutoNaming.Types.NamespaceProperties
import Amazonka.Route53AutoNaming.Types.NamespaceSummary
import Amazonka.Route53AutoNaming.Types.Operation
import Amazonka.Route53AutoNaming.Types.OperationFilter
import Amazonka.Route53AutoNaming.Types.OperationSummary
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
import Amazonka.Route53AutoNaming.Types.SOA
import Amazonka.Route53AutoNaming.Types.SOAChange
import Amazonka.Route53AutoNaming.Types.ServiceChange
import Amazonka.Route53AutoNaming.Types.ServiceFilter
import Amazonka.Route53AutoNaming.Types.ServiceInfo
import Amazonka.Route53AutoNaming.Types.ServiceSummary
import Amazonka.Route53AutoNaming.Types.Tag
import Amazonka.Route53AutoNaming.UntagResource
import Amazonka.Route53AutoNaming.UpdateHttpNamespace
import Amazonka.Route53AutoNaming.UpdateInstanceCustomHealthStatus
import Amazonka.Route53AutoNaming.UpdatePrivateDnsNamespace
import Amazonka.Route53AutoNaming.UpdatePublicDnsNamespace
import Amazonka.Route53AutoNaming.UpdateService
