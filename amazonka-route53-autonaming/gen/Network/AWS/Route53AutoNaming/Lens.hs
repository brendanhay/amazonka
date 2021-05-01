{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Lens
  ( -- * Operations

    -- ** CreatePublicDnsNamespace
    createPublicDnsNamespace_creatorRequestId,
    createPublicDnsNamespace_tags,
    createPublicDnsNamespace_description,
    createPublicDnsNamespace_name,
    createPublicDnsNamespaceResponse_operationId,
    createPublicDnsNamespaceResponse_httpStatus,

    -- ** ListServices
    listServices_nextToken,
    listServices_maxResults,
    listServices_filters,
    listServicesResponse_nextToken,
    listServicesResponse_services,
    listServicesResponse_httpStatus,

    -- ** ListOperations
    listOperations_nextToken,
    listOperations_maxResults,
    listOperations_filters,
    listOperationsResponse_nextToken,
    listOperationsResponse_operations,
    listOperationsResponse_httpStatus,

    -- ** CreateService
    createService_namespaceId,
    createService_dnsConfig,
    createService_creatorRequestId,
    createService_tags,
    createService_description,
    createService_healthCheckCustomConfig,
    createService_type,
    createService_healthCheckConfig,
    createService_name,
    createServiceResponse_service,
    createServiceResponse_httpStatus,

    -- ** CreatePrivateDnsNamespace
    createPrivateDnsNamespace_creatorRequestId,
    createPrivateDnsNamespace_tags,
    createPrivateDnsNamespace_description,
    createPrivateDnsNamespace_name,
    createPrivateDnsNamespace_vpc,
    createPrivateDnsNamespaceResponse_operationId,
    createPrivateDnsNamespaceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** ListInstances
    listInstances_nextToken,
    listInstances_maxResults,
    listInstances_serviceId,
    listInstancesResponse_nextToken,
    listInstancesResponse_instances,
    listInstancesResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** GetNamespace
    getNamespace_id,
    getNamespaceResponse_namespace,
    getNamespaceResponse_httpStatus,

    -- ** ListNamespaces
    listNamespaces_nextToken,
    listNamespaces_maxResults,
    listNamespaces_filters,
    listNamespacesResponse_nextToken,
    listNamespacesResponse_namespaces,
    listNamespacesResponse_httpStatus,

    -- ** CreateHttpNamespace
    createHttpNamespace_creatorRequestId,
    createHttpNamespace_tags,
    createHttpNamespace_description,
    createHttpNamespace_name,
    createHttpNamespaceResponse_operationId,
    createHttpNamespaceResponse_httpStatus,

    -- ** GetInstance
    getInstance_serviceId,
    getInstance_instanceId,
    getInstanceResponse_instance,
    getInstanceResponse_httpStatus,

    -- ** GetInstancesHealthStatus
    getInstancesHealthStatus_nextToken,
    getInstancesHealthStatus_maxResults,
    getInstancesHealthStatus_instances,
    getInstancesHealthStatus_serviceId,
    getInstancesHealthStatusResponse_status,
    getInstancesHealthStatusResponse_nextToken,
    getInstancesHealthStatusResponse_httpStatus,

    -- ** DeleteService
    deleteService_id,
    deleteServiceResponse_httpStatus,

    -- ** UpdateService
    updateService_id,
    updateService_service,
    updateServiceResponse_operationId,
    updateServiceResponse_httpStatus,

    -- ** DiscoverInstances
    discoverInstances_maxResults,
    discoverInstances_optionalParameters,
    discoverInstances_healthStatus,
    discoverInstances_queryParameters,
    discoverInstances_namespaceName,
    discoverInstances_serviceName,
    discoverInstancesResponse_instances,
    discoverInstancesResponse_httpStatus,

    -- ** DeregisterInstance
    deregisterInstance_serviceId,
    deregisterInstance_instanceId,
    deregisterInstanceResponse_operationId,
    deregisterInstanceResponse_httpStatus,

    -- ** GetOperation
    getOperation_operationId,
    getOperationResponse_operation,
    getOperationResponse_httpStatus,

    -- ** GetService
    getService_id,
    getServiceResponse_service,
    getServiceResponse_httpStatus,

    -- ** UpdateInstanceCustomHealthStatus
    updateInstanceCustomHealthStatus_serviceId,
    updateInstanceCustomHealthStatus_instanceId,
    updateInstanceCustomHealthStatus_status,

    -- ** RegisterInstance
    registerInstance_creatorRequestId,
    registerInstance_serviceId,
    registerInstance_instanceId,
    registerInstance_attributes,
    registerInstanceResponse_operationId,
    registerInstanceResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DeleteNamespace
    deleteNamespace_id,
    deleteNamespaceResponse_operationId,
    deleteNamespaceResponse_httpStatus,

    -- * Types

    -- ** DnsConfig
    dnsConfig_namespaceId,
    dnsConfig_routingPolicy,
    dnsConfig_dnsRecords,

    -- ** DnsConfigChange
    dnsConfigChange_dnsRecords,

    -- ** DnsProperties
    dnsProperties_hostedZoneId,

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
    httpInstanceSummary_instanceId,
    httpInstanceSummary_serviceName,
    httpInstanceSummary_attributes,
    httpInstanceSummary_healthStatus,

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
    namespace_createDate,
    namespace_creatorRequestId,
    namespace_arn,
    namespace_id,
    namespace_name,
    namespace_properties,
    namespace_serviceCount,
    namespace_description,
    namespace_type,

    -- ** NamespaceFilter
    namespaceFilter_condition,
    namespaceFilter_name,
    namespaceFilter_values,

    -- ** NamespaceProperties
    namespaceProperties_httpProperties,
    namespaceProperties_dnsProperties,

    -- ** NamespaceSummary
    namespaceSummary_createDate,
    namespaceSummary_arn,
    namespaceSummary_id,
    namespaceSummary_name,
    namespaceSummary_properties,
    namespaceSummary_serviceCount,
    namespaceSummary_description,
    namespaceSummary_type,

    -- ** Operation
    operation_status,
    operation_createDate,
    operation_id,
    operation_targets,
    operation_errorMessage,
    operation_type,
    operation_errorCode,
    operation_updateDate,

    -- ** OperationFilter
    operationFilter_condition,
    operationFilter_name,
    operationFilter_values,

    -- ** OperationSummary
    operationSummary_status,
    operationSummary_id,

    -- ** ServiceChange
    serviceChange_dnsConfig,
    serviceChange_description,
    serviceChange_healthCheckConfig,

    -- ** ServiceFilter
    serviceFilter_condition,
    serviceFilter_name,
    serviceFilter_values,

    -- ** ServiceInfo
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

    -- ** ServiceSummary
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

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Network.AWS.Route53AutoNaming.CreateHttpNamespace
import Network.AWS.Route53AutoNaming.CreatePrivateDnsNamespace
import Network.AWS.Route53AutoNaming.CreatePublicDnsNamespace
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
import Network.AWS.Route53AutoNaming.Types.DnsConfig
import Network.AWS.Route53AutoNaming.Types.DnsConfigChange
import Network.AWS.Route53AutoNaming.Types.DnsProperties
import Network.AWS.Route53AutoNaming.Types.DnsRecord
import Network.AWS.Route53AutoNaming.Types.HealthCheckConfig
import Network.AWS.Route53AutoNaming.Types.HealthCheckCustomConfig
import Network.AWS.Route53AutoNaming.Types.HttpInstanceSummary
import Network.AWS.Route53AutoNaming.Types.HttpProperties
import Network.AWS.Route53AutoNaming.Types.Instance
import Network.AWS.Route53AutoNaming.Types.InstanceSummary
import Network.AWS.Route53AutoNaming.Types.Namespace
import Network.AWS.Route53AutoNaming.Types.NamespaceFilter
import Network.AWS.Route53AutoNaming.Types.NamespaceProperties
import Network.AWS.Route53AutoNaming.Types.NamespaceSummary
import Network.AWS.Route53AutoNaming.Types.Operation
import Network.AWS.Route53AutoNaming.Types.OperationFilter
import Network.AWS.Route53AutoNaming.Types.OperationSummary
import Network.AWS.Route53AutoNaming.Types.ServiceChange
import Network.AWS.Route53AutoNaming.Types.ServiceFilter
import Network.AWS.Route53AutoNaming.Types.ServiceInfo
import Network.AWS.Route53AutoNaming.Types.ServiceSummary
import Network.AWS.Route53AutoNaming.Types.Tag
import Network.AWS.Route53AutoNaming.UntagResource
import Network.AWS.Route53AutoNaming.UpdateInstanceCustomHealthStatus
import Network.AWS.Route53AutoNaming.UpdateService
