{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppMesh.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Lens
  ( -- * Operations

    -- ** DescribeVirtualNode
    describeVirtualNode_meshOwner,
    describeVirtualNode_meshName,
    describeVirtualNode_virtualNodeName,
    describeVirtualNodeResponse_httpStatus,
    describeVirtualNodeResponse_virtualNode,

    -- ** DescribeVirtualGateway
    describeVirtualGateway_meshOwner,
    describeVirtualGateway_meshName,
    describeVirtualGateway_virtualGatewayName,
    describeVirtualGatewayResponse_httpStatus,
    describeVirtualGatewayResponse_virtualGateway,

    -- ** DescribeRoute
    describeRoute_meshOwner,
    describeRoute_meshName,
    describeRoute_routeName,
    describeRoute_virtualRouterName,
    describeRouteResponse_httpStatus,
    describeRouteResponse_route,

    -- ** DescribeVirtualRouter
    describeVirtualRouter_meshOwner,
    describeVirtualRouter_meshName,
    describeVirtualRouter_virtualRouterName,
    describeVirtualRouterResponse_httpStatus,
    describeVirtualRouterResponse_virtualRouter,

    -- ** ListMeshes
    listMeshes_nextToken,
    listMeshes_limit,
    listMeshesResponse_nextToken,
    listMeshesResponse_httpStatus,
    listMeshesResponse_meshes,

    -- ** CreateMesh
    createMesh_clientToken,
    createMesh_spec,
    createMesh_tags,
    createMesh_meshName,
    createMeshResponse_httpStatus,
    createMeshResponse_mesh,

    -- ** UpdateMesh
    updateMesh_clientToken,
    updateMesh_spec,
    updateMesh_meshName,
    updateMeshResponse_httpStatus,
    updateMeshResponse_mesh,

    -- ** DeleteMesh
    deleteMesh_meshName,
    deleteMeshResponse_httpStatus,
    deleteMeshResponse_mesh,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_limit,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_tags,

    -- ** CreateVirtualGateway
    createVirtualGateway_clientToken,
    createVirtualGateway_meshOwner,
    createVirtualGateway_tags,
    createVirtualGateway_meshName,
    createVirtualGateway_spec,
    createVirtualGateway_virtualGatewayName,
    createVirtualGatewayResponse_httpStatus,
    createVirtualGatewayResponse_virtualGateway,

    -- ** ListVirtualServices
    listVirtualServices_meshOwner,
    listVirtualServices_nextToken,
    listVirtualServices_limit,
    listVirtualServices_meshName,
    listVirtualServicesResponse_nextToken,
    listVirtualServicesResponse_httpStatus,
    listVirtualServicesResponse_virtualServices,

    -- ** DeleteVirtualService
    deleteVirtualService_meshOwner,
    deleteVirtualService_meshName,
    deleteVirtualService_virtualServiceName,
    deleteVirtualServiceResponse_httpStatus,
    deleteVirtualServiceResponse_virtualService,

    -- ** UpdateVirtualService
    updateVirtualService_clientToken,
    updateVirtualService_meshOwner,
    updateVirtualService_meshName,
    updateVirtualService_spec,
    updateVirtualService_virtualServiceName,
    updateVirtualServiceResponse_httpStatus,
    updateVirtualServiceResponse_virtualService,

    -- ** UpdateVirtualGateway
    updateVirtualGateway_clientToken,
    updateVirtualGateway_meshOwner,
    updateVirtualGateway_meshName,
    updateVirtualGateway_spec,
    updateVirtualGateway_virtualGatewayName,
    updateVirtualGatewayResponse_httpStatus,
    updateVirtualGatewayResponse_virtualGateway,

    -- ** DeleteVirtualGateway
    deleteVirtualGateway_meshOwner,
    deleteVirtualGateway_meshName,
    deleteVirtualGateway_virtualGatewayName,
    deleteVirtualGatewayResponse_httpStatus,
    deleteVirtualGatewayResponse_virtualGateway,

    -- ** DeleteRoute
    deleteRoute_meshOwner,
    deleteRoute_meshName,
    deleteRoute_routeName,
    deleteRoute_virtualRouterName,
    deleteRouteResponse_httpStatus,
    deleteRouteResponse_route,

    -- ** UpdateRoute
    updateRoute_clientToken,
    updateRoute_meshOwner,
    updateRoute_meshName,
    updateRoute_routeName,
    updateRoute_spec,
    updateRoute_virtualRouterName,
    updateRouteResponse_httpStatus,
    updateRouteResponse_route,

    -- ** CreateVirtualService
    createVirtualService_clientToken,
    createVirtualService_meshOwner,
    createVirtualService_tags,
    createVirtualService_meshName,
    createVirtualService_spec,
    createVirtualService_virtualServiceName,
    createVirtualServiceResponse_httpStatus,
    createVirtualServiceResponse_virtualService,

    -- ** DeleteVirtualNode
    deleteVirtualNode_meshOwner,
    deleteVirtualNode_meshName,
    deleteVirtualNode_virtualNodeName,
    deleteVirtualNodeResponse_httpStatus,
    deleteVirtualNodeResponse_virtualNode,

    -- ** UpdateVirtualNode
    updateVirtualNode_clientToken,
    updateVirtualNode_meshOwner,
    updateVirtualNode_meshName,
    updateVirtualNode_spec,
    updateVirtualNode_virtualNodeName,
    updateVirtualNodeResponse_httpStatus,
    updateVirtualNodeResponse_virtualNode,

    -- ** ListGatewayRoutes
    listGatewayRoutes_meshOwner,
    listGatewayRoutes_nextToken,
    listGatewayRoutes_limit,
    listGatewayRoutes_meshName,
    listGatewayRoutes_virtualGatewayName,
    listGatewayRoutesResponse_nextToken,
    listGatewayRoutesResponse_httpStatus,
    listGatewayRoutesResponse_gatewayRoutes,

    -- ** ListRoutes
    listRoutes_meshOwner,
    listRoutes_nextToken,
    listRoutes_limit,
    listRoutes_meshName,
    listRoutes_virtualRouterName,
    listRoutesResponse_nextToken,
    listRoutesResponse_httpStatus,
    listRoutesResponse_routes,

    -- ** ListVirtualNodes
    listVirtualNodes_meshOwner,
    listVirtualNodes_nextToken,
    listVirtualNodes_limit,
    listVirtualNodes_meshName,
    listVirtualNodesResponse_nextToken,
    listVirtualNodesResponse_httpStatus,
    listVirtualNodesResponse_virtualNodes,

    -- ** DeleteVirtualRouter
    deleteVirtualRouter_meshOwner,
    deleteVirtualRouter_meshName,
    deleteVirtualRouter_virtualRouterName,
    deleteVirtualRouterResponse_httpStatus,
    deleteVirtualRouterResponse_virtualRouter,

    -- ** UpdateVirtualRouter
    updateVirtualRouter_clientToken,
    updateVirtualRouter_meshOwner,
    updateVirtualRouter_meshName,
    updateVirtualRouter_spec,
    updateVirtualRouter_virtualRouterName,
    updateVirtualRouterResponse_httpStatus,
    updateVirtualRouterResponse_virtualRouter,

    -- ** CreateVirtualRouter
    createVirtualRouter_clientToken,
    createVirtualRouter_meshOwner,
    createVirtualRouter_tags,
    createVirtualRouter_meshName,
    createVirtualRouter_spec,
    createVirtualRouter_virtualRouterName,
    createVirtualRouterResponse_httpStatus,
    createVirtualRouterResponse_virtualRouter,

    -- ** DescribeVirtualService
    describeVirtualService_meshOwner,
    describeVirtualService_meshName,
    describeVirtualService_virtualServiceName,
    describeVirtualServiceResponse_httpStatus,
    describeVirtualServiceResponse_virtualService,

    -- ** DescribeGatewayRoute
    describeGatewayRoute_meshOwner,
    describeGatewayRoute_gatewayRouteName,
    describeGatewayRoute_meshName,
    describeGatewayRoute_virtualGatewayName,
    describeGatewayRouteResponse_httpStatus,
    describeGatewayRouteResponse_gatewayRoute,

    -- ** CreateRoute
    createRoute_clientToken,
    createRoute_meshOwner,
    createRoute_tags,
    createRoute_meshName,
    createRoute_routeName,
    createRoute_spec,
    createRoute_virtualRouterName,
    createRouteResponse_httpStatus,
    createRouteResponse_route,

    -- ** CreateVirtualNode
    createVirtualNode_clientToken,
    createVirtualNode_meshOwner,
    createVirtualNode_tags,
    createVirtualNode_meshName,
    createVirtualNode_spec,
    createVirtualNode_virtualNodeName,
    createVirtualNodeResponse_httpStatus,
    createVirtualNodeResponse_virtualNode,

    -- ** CreateGatewayRoute
    createGatewayRoute_clientToken,
    createGatewayRoute_meshOwner,
    createGatewayRoute_tags,
    createGatewayRoute_gatewayRouteName,
    createGatewayRoute_meshName,
    createGatewayRoute_spec,
    createGatewayRoute_virtualGatewayName,
    createGatewayRouteResponse_httpStatus,
    createGatewayRouteResponse_gatewayRoute,

    -- ** UpdateGatewayRoute
    updateGatewayRoute_clientToken,
    updateGatewayRoute_meshOwner,
    updateGatewayRoute_gatewayRouteName,
    updateGatewayRoute_meshName,
    updateGatewayRoute_spec,
    updateGatewayRoute_virtualGatewayName,
    updateGatewayRouteResponse_httpStatus,
    updateGatewayRouteResponse_gatewayRoute,

    -- ** DeleteGatewayRoute
    deleteGatewayRoute_meshOwner,
    deleteGatewayRoute_gatewayRouteName,
    deleteGatewayRoute_meshName,
    deleteGatewayRoute_virtualGatewayName,
    deleteGatewayRouteResponse_httpStatus,
    deleteGatewayRouteResponse_gatewayRoute,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListVirtualGateways
    listVirtualGateways_meshOwner,
    listVirtualGateways_nextToken,
    listVirtualGateways_limit,
    listVirtualGateways_meshName,
    listVirtualGatewaysResponse_nextToken,
    listVirtualGatewaysResponse_httpStatus,
    listVirtualGatewaysResponse_virtualGateways,

    -- ** ListVirtualRouters
    listVirtualRouters_meshOwner,
    listVirtualRouters_nextToken,
    listVirtualRouters_limit,
    listVirtualRouters_meshName,
    listVirtualRoutersResponse_nextToken,
    listVirtualRoutersResponse_httpStatus,
    listVirtualRoutersResponse_virtualRouters,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DescribeMesh
    describeMesh_meshOwner,
    describeMesh_meshName,
    describeMeshResponse_httpStatus,
    describeMeshResponse_mesh,

    -- * Types

    -- ** AccessLog
    accessLog_file,

    -- ** AwsCloudMapInstanceAttribute
    awsCloudMapInstanceAttribute_key,
    awsCloudMapInstanceAttribute_value,

    -- ** AwsCloudMapServiceDiscovery
    awsCloudMapServiceDiscovery_attributes,
    awsCloudMapServiceDiscovery_namespaceName,
    awsCloudMapServiceDiscovery_serviceName,

    -- ** Backend
    backend_virtualService,

    -- ** BackendDefaults
    backendDefaults_clientPolicy,

    -- ** ClientPolicy
    clientPolicy_tls,

    -- ** ClientPolicyTls
    clientPolicyTls_ports,
    clientPolicyTls_certificate,
    clientPolicyTls_enforce,
    clientPolicyTls_validation,

    -- ** ClientTlsCertificate
    clientTlsCertificate_sds,
    clientTlsCertificate_file,

    -- ** DnsServiceDiscovery
    dnsServiceDiscovery_responseType,
    dnsServiceDiscovery_hostname,

    -- ** Duration
    duration_value,
    duration_unit,

    -- ** EgressFilter
    egressFilter_type,

    -- ** FileAccessLog
    fileAccessLog_path,

    -- ** GatewayRouteData
    gatewayRouteData_gatewayRouteName,
    gatewayRouteData_meshName,
    gatewayRouteData_metadata,
    gatewayRouteData_spec,
    gatewayRouteData_status,
    gatewayRouteData_virtualGatewayName,

    -- ** GatewayRouteHostnameMatch
    gatewayRouteHostnameMatch_suffix,
    gatewayRouteHostnameMatch_exact,

    -- ** GatewayRouteHostnameRewrite
    gatewayRouteHostnameRewrite_defaultTargetHostname,

    -- ** GatewayRouteRef
    gatewayRouteRef_arn,
    gatewayRouteRef_createdAt,
    gatewayRouteRef_gatewayRouteName,
    gatewayRouteRef_lastUpdatedAt,
    gatewayRouteRef_meshName,
    gatewayRouteRef_meshOwner,
    gatewayRouteRef_resourceOwner,
    gatewayRouteRef_version,
    gatewayRouteRef_virtualGatewayName,

    -- ** GatewayRouteSpec
    gatewayRouteSpec_priority,
    gatewayRouteSpec_http2Route,
    gatewayRouteSpec_grpcRoute,
    gatewayRouteSpec_httpRoute,

    -- ** GatewayRouteStatus
    gatewayRouteStatus_status,

    -- ** GatewayRouteTarget
    gatewayRouteTarget_virtualService,

    -- ** GatewayRouteVirtualService
    gatewayRouteVirtualService_virtualServiceName,

    -- ** GrpcGatewayRoute
    grpcGatewayRoute_action,
    grpcGatewayRoute_match,

    -- ** GrpcGatewayRouteAction
    grpcGatewayRouteAction_rewrite,
    grpcGatewayRouteAction_target,

    -- ** GrpcGatewayRouteMatch
    grpcGatewayRouteMatch_hostname,
    grpcGatewayRouteMatch_serviceName,
    grpcGatewayRouteMatch_metadata,

    -- ** GrpcGatewayRouteMetadata
    grpcGatewayRouteMetadata_invert,
    grpcGatewayRouteMetadata_match,
    grpcGatewayRouteMetadata_name,

    -- ** GrpcGatewayRouteRewrite
    grpcGatewayRouteRewrite_hostname,

    -- ** GrpcMetadataMatchMethod
    grpcMetadataMatchMethod_suffix,
    grpcMetadataMatchMethod_regex,
    grpcMetadataMatchMethod_prefix,
    grpcMetadataMatchMethod_range,
    grpcMetadataMatchMethod_exact,

    -- ** GrpcRetryPolicy
    grpcRetryPolicy_httpRetryEvents,
    grpcRetryPolicy_grpcRetryEvents,
    grpcRetryPolicy_tcpRetryEvents,
    grpcRetryPolicy_maxRetries,
    grpcRetryPolicy_perRetryTimeout,

    -- ** GrpcRoute
    grpcRoute_retryPolicy,
    grpcRoute_timeout,
    grpcRoute_action,
    grpcRoute_match,

    -- ** GrpcRouteAction
    grpcRouteAction_weightedTargets,

    -- ** GrpcRouteMatch
    grpcRouteMatch_methodName,
    grpcRouteMatch_serviceName,
    grpcRouteMatch_metadata,

    -- ** GrpcRouteMetadata
    grpcRouteMetadata_invert,
    grpcRouteMetadata_match,
    grpcRouteMetadata_name,

    -- ** GrpcRouteMetadataMatchMethod
    grpcRouteMetadataMatchMethod_suffix,
    grpcRouteMetadataMatchMethod_regex,
    grpcRouteMetadataMatchMethod_prefix,
    grpcRouteMetadataMatchMethod_range,
    grpcRouteMetadataMatchMethod_exact,

    -- ** GrpcTimeout
    grpcTimeout_idle,
    grpcTimeout_perRequest,

    -- ** HeaderMatchMethod
    headerMatchMethod_suffix,
    headerMatchMethod_regex,
    headerMatchMethod_prefix,
    headerMatchMethod_range,
    headerMatchMethod_exact,

    -- ** HealthCheckPolicy
    healthCheckPolicy_path,
    healthCheckPolicy_port,
    healthCheckPolicy_healthyThreshold,
    healthCheckPolicy_intervalMillis,
    healthCheckPolicy_protocol,
    healthCheckPolicy_timeoutMillis,
    healthCheckPolicy_unhealthyThreshold,

    -- ** HttpGatewayRoute
    httpGatewayRoute_action,
    httpGatewayRoute_match,

    -- ** HttpGatewayRouteAction
    httpGatewayRouteAction_rewrite,
    httpGatewayRouteAction_target,

    -- ** HttpGatewayRouteHeader
    httpGatewayRouteHeader_invert,
    httpGatewayRouteHeader_match,
    httpGatewayRouteHeader_name,

    -- ** HttpGatewayRouteMatch
    httpGatewayRouteMatch_hostname,
    httpGatewayRouteMatch_path,
    httpGatewayRouteMatch_prefix,
    httpGatewayRouteMatch_queryParameters,
    httpGatewayRouteMatch_headers,
    httpGatewayRouteMatch_method,

    -- ** HttpGatewayRoutePathRewrite
    httpGatewayRoutePathRewrite_exact,

    -- ** HttpGatewayRoutePrefixRewrite
    httpGatewayRoutePrefixRewrite_value,
    httpGatewayRoutePrefixRewrite_defaultPrefix,

    -- ** HttpGatewayRouteRewrite
    httpGatewayRouteRewrite_hostname,
    httpGatewayRouteRewrite_path,
    httpGatewayRouteRewrite_prefix,

    -- ** HttpPathMatch
    httpPathMatch_regex,
    httpPathMatch_exact,

    -- ** HttpQueryParameter
    httpQueryParameter_match,
    httpQueryParameter_name,

    -- ** HttpRetryPolicy
    httpRetryPolicy_httpRetryEvents,
    httpRetryPolicy_tcpRetryEvents,
    httpRetryPolicy_maxRetries,
    httpRetryPolicy_perRetryTimeout,

    -- ** HttpRoute
    httpRoute_retryPolicy,
    httpRoute_timeout,
    httpRoute_action,
    httpRoute_match,

    -- ** HttpRouteAction
    httpRouteAction_weightedTargets,

    -- ** HttpRouteHeader
    httpRouteHeader_invert,
    httpRouteHeader_match,
    httpRouteHeader_name,

    -- ** HttpRouteMatch
    httpRouteMatch_path,
    httpRouteMatch_prefix,
    httpRouteMatch_queryParameters,
    httpRouteMatch_headers,
    httpRouteMatch_method,
    httpRouteMatch_scheme,

    -- ** HttpTimeout
    httpTimeout_idle,
    httpTimeout_perRequest,

    -- ** Listener
    listener_healthCheck,
    listener_connectionPool,
    listener_tls,
    listener_outlierDetection,
    listener_timeout,
    listener_portMapping,

    -- ** ListenerTimeout
    listenerTimeout_http2,
    listenerTimeout_grpc,
    listenerTimeout_tcp,
    listenerTimeout_http,

    -- ** ListenerTls
    listenerTls_validation,
    listenerTls_certificate,
    listenerTls_mode,

    -- ** ListenerTlsAcmCertificate
    listenerTlsAcmCertificate_certificateArn,

    -- ** ListenerTlsCertificate
    listenerTlsCertificate_acm,
    listenerTlsCertificate_sds,
    listenerTlsCertificate_file,

    -- ** ListenerTlsFileCertificate
    listenerTlsFileCertificate_certificateChain,
    listenerTlsFileCertificate_privateKey,

    -- ** ListenerTlsSdsCertificate
    listenerTlsSdsCertificate_secretName,

    -- ** ListenerTlsValidationContext
    listenerTlsValidationContext_subjectAlternativeNames,
    listenerTlsValidationContext_trust,

    -- ** ListenerTlsValidationContextTrust
    listenerTlsValidationContextTrust_sds,
    listenerTlsValidationContextTrust_file,

    -- ** Logging
    logging_accessLog,

    -- ** MatchRange
    matchRange_end,
    matchRange_start,

    -- ** MeshData
    meshData_meshName,
    meshData_metadata,
    meshData_spec,
    meshData_status,

    -- ** MeshRef
    meshRef_arn,
    meshRef_createdAt,
    meshRef_lastUpdatedAt,
    meshRef_meshName,
    meshRef_meshOwner,
    meshRef_resourceOwner,
    meshRef_version,

    -- ** MeshSpec
    meshSpec_egressFilter,

    -- ** MeshStatus
    meshStatus_status,

    -- ** OutlierDetection
    outlierDetection_baseEjectionDuration,
    outlierDetection_interval,
    outlierDetection_maxEjectionPercent,
    outlierDetection_maxServerErrors,

    -- ** PortMapping
    portMapping_port,
    portMapping_protocol,

    -- ** QueryParameterMatch
    queryParameterMatch_exact,

    -- ** ResourceMetadata
    resourceMetadata_arn,
    resourceMetadata_createdAt,
    resourceMetadata_lastUpdatedAt,
    resourceMetadata_meshOwner,
    resourceMetadata_resourceOwner,
    resourceMetadata_uid,
    resourceMetadata_version,

    -- ** RouteData
    routeData_meshName,
    routeData_metadata,
    routeData_routeName,
    routeData_spec,
    routeData_status,
    routeData_virtualRouterName,

    -- ** RouteRef
    routeRef_arn,
    routeRef_createdAt,
    routeRef_lastUpdatedAt,
    routeRef_meshName,
    routeRef_meshOwner,
    routeRef_resourceOwner,
    routeRef_routeName,
    routeRef_version,
    routeRef_virtualRouterName,

    -- ** RouteSpec
    routeSpec_priority,
    routeSpec_http2Route,
    routeSpec_grpcRoute,
    routeSpec_tcpRoute,
    routeSpec_httpRoute,

    -- ** RouteStatus
    routeStatus_status,

    -- ** ServiceDiscovery
    serviceDiscovery_awsCloudMap,
    serviceDiscovery_dns,

    -- ** SubjectAlternativeNameMatchers
    subjectAlternativeNameMatchers_exact,

    -- ** SubjectAlternativeNames
    subjectAlternativeNames_match,

    -- ** TagRef
    tagRef_key,
    tagRef_value,

    -- ** TcpRoute
    tcpRoute_timeout,
    tcpRoute_action,

    -- ** TcpRouteAction
    tcpRouteAction_weightedTargets,

    -- ** TcpTimeout
    tcpTimeout_idle,

    -- ** TlsValidationContext
    tlsValidationContext_subjectAlternativeNames,
    tlsValidationContext_trust,

    -- ** TlsValidationContextAcmTrust
    tlsValidationContextAcmTrust_certificateAuthorityArns,

    -- ** TlsValidationContextFileTrust
    tlsValidationContextFileTrust_certificateChain,

    -- ** TlsValidationContextSdsTrust
    tlsValidationContextSdsTrust_secretName,

    -- ** TlsValidationContextTrust
    tlsValidationContextTrust_acm,
    tlsValidationContextTrust_sds,
    tlsValidationContextTrust_file,

    -- ** VirtualGatewayAccessLog
    virtualGatewayAccessLog_file,

    -- ** VirtualGatewayBackendDefaults
    virtualGatewayBackendDefaults_clientPolicy,

    -- ** VirtualGatewayClientPolicy
    virtualGatewayClientPolicy_tls,

    -- ** VirtualGatewayClientPolicyTls
    virtualGatewayClientPolicyTls_ports,
    virtualGatewayClientPolicyTls_certificate,
    virtualGatewayClientPolicyTls_enforce,
    virtualGatewayClientPolicyTls_validation,

    -- ** VirtualGatewayClientTlsCertificate
    virtualGatewayClientTlsCertificate_sds,
    virtualGatewayClientTlsCertificate_file,

    -- ** VirtualGatewayConnectionPool
    virtualGatewayConnectionPool_http2,
    virtualGatewayConnectionPool_grpc,
    virtualGatewayConnectionPool_http,

    -- ** VirtualGatewayData
    virtualGatewayData_meshName,
    virtualGatewayData_metadata,
    virtualGatewayData_spec,
    virtualGatewayData_status,
    virtualGatewayData_virtualGatewayName,

    -- ** VirtualGatewayFileAccessLog
    virtualGatewayFileAccessLog_path,

    -- ** VirtualGatewayGrpcConnectionPool
    virtualGatewayGrpcConnectionPool_maxRequests,

    -- ** VirtualGatewayHealthCheckPolicy
    virtualGatewayHealthCheckPolicy_path,
    virtualGatewayHealthCheckPolicy_port,
    virtualGatewayHealthCheckPolicy_healthyThreshold,
    virtualGatewayHealthCheckPolicy_intervalMillis,
    virtualGatewayHealthCheckPolicy_protocol,
    virtualGatewayHealthCheckPolicy_timeoutMillis,
    virtualGatewayHealthCheckPolicy_unhealthyThreshold,

    -- ** VirtualGatewayHttp2ConnectionPool
    virtualGatewayHttp2ConnectionPool_maxRequests,

    -- ** VirtualGatewayHttpConnectionPool
    virtualGatewayHttpConnectionPool_maxPendingRequests,
    virtualGatewayHttpConnectionPool_maxConnections,

    -- ** VirtualGatewayListener
    virtualGatewayListener_healthCheck,
    virtualGatewayListener_connectionPool,
    virtualGatewayListener_tls,
    virtualGatewayListener_portMapping,

    -- ** VirtualGatewayListenerTls
    virtualGatewayListenerTls_validation,
    virtualGatewayListenerTls_certificate,
    virtualGatewayListenerTls_mode,

    -- ** VirtualGatewayListenerTlsAcmCertificate
    virtualGatewayListenerTlsAcmCertificate_certificateArn,

    -- ** VirtualGatewayListenerTlsCertificate
    virtualGatewayListenerTlsCertificate_acm,
    virtualGatewayListenerTlsCertificate_sds,
    virtualGatewayListenerTlsCertificate_file,

    -- ** VirtualGatewayListenerTlsFileCertificate
    virtualGatewayListenerTlsFileCertificate_certificateChain,
    virtualGatewayListenerTlsFileCertificate_privateKey,

    -- ** VirtualGatewayListenerTlsSdsCertificate
    virtualGatewayListenerTlsSdsCertificate_secretName,

    -- ** VirtualGatewayListenerTlsValidationContext
    virtualGatewayListenerTlsValidationContext_subjectAlternativeNames,
    virtualGatewayListenerTlsValidationContext_trust,

    -- ** VirtualGatewayListenerTlsValidationContextTrust
    virtualGatewayListenerTlsValidationContextTrust_sds,
    virtualGatewayListenerTlsValidationContextTrust_file,

    -- ** VirtualGatewayLogging
    virtualGatewayLogging_accessLog,

    -- ** VirtualGatewayPortMapping
    virtualGatewayPortMapping_port,
    virtualGatewayPortMapping_protocol,

    -- ** VirtualGatewayRef
    virtualGatewayRef_arn,
    virtualGatewayRef_createdAt,
    virtualGatewayRef_lastUpdatedAt,
    virtualGatewayRef_meshName,
    virtualGatewayRef_meshOwner,
    virtualGatewayRef_resourceOwner,
    virtualGatewayRef_version,
    virtualGatewayRef_virtualGatewayName,

    -- ** VirtualGatewaySpec
    virtualGatewaySpec_backendDefaults,
    virtualGatewaySpec_logging,
    virtualGatewaySpec_listeners,

    -- ** VirtualGatewayStatus
    virtualGatewayStatus_status,

    -- ** VirtualGatewayTlsValidationContext
    virtualGatewayTlsValidationContext_subjectAlternativeNames,
    virtualGatewayTlsValidationContext_trust,

    -- ** VirtualGatewayTlsValidationContextAcmTrust
    virtualGatewayTlsValidationContextAcmTrust_certificateAuthorityArns,

    -- ** VirtualGatewayTlsValidationContextFileTrust
    virtualGatewayTlsValidationContextFileTrust_certificateChain,

    -- ** VirtualGatewayTlsValidationContextSdsTrust
    virtualGatewayTlsValidationContextSdsTrust_secretName,

    -- ** VirtualGatewayTlsValidationContextTrust
    virtualGatewayTlsValidationContextTrust_acm,
    virtualGatewayTlsValidationContextTrust_sds,
    virtualGatewayTlsValidationContextTrust_file,

    -- ** VirtualNodeConnectionPool
    virtualNodeConnectionPool_http2,
    virtualNodeConnectionPool_grpc,
    virtualNodeConnectionPool_tcp,
    virtualNodeConnectionPool_http,

    -- ** VirtualNodeData
    virtualNodeData_meshName,
    virtualNodeData_metadata,
    virtualNodeData_spec,
    virtualNodeData_status,
    virtualNodeData_virtualNodeName,

    -- ** VirtualNodeGrpcConnectionPool
    virtualNodeGrpcConnectionPool_maxRequests,

    -- ** VirtualNodeHttp2ConnectionPool
    virtualNodeHttp2ConnectionPool_maxRequests,

    -- ** VirtualNodeHttpConnectionPool
    virtualNodeHttpConnectionPool_maxPendingRequests,
    virtualNodeHttpConnectionPool_maxConnections,

    -- ** VirtualNodeRef
    virtualNodeRef_arn,
    virtualNodeRef_createdAt,
    virtualNodeRef_lastUpdatedAt,
    virtualNodeRef_meshName,
    virtualNodeRef_meshOwner,
    virtualNodeRef_resourceOwner,
    virtualNodeRef_version,
    virtualNodeRef_virtualNodeName,

    -- ** VirtualNodeServiceProvider
    virtualNodeServiceProvider_virtualNodeName,

    -- ** VirtualNodeSpec
    virtualNodeSpec_backends,
    virtualNodeSpec_backendDefaults,
    virtualNodeSpec_serviceDiscovery,
    virtualNodeSpec_listeners,
    virtualNodeSpec_logging,

    -- ** VirtualNodeStatus
    virtualNodeStatus_status,

    -- ** VirtualNodeTcpConnectionPool
    virtualNodeTcpConnectionPool_maxConnections,

    -- ** VirtualRouterData
    virtualRouterData_meshName,
    virtualRouterData_metadata,
    virtualRouterData_spec,
    virtualRouterData_status,
    virtualRouterData_virtualRouterName,

    -- ** VirtualRouterListener
    virtualRouterListener_portMapping,

    -- ** VirtualRouterRef
    virtualRouterRef_arn,
    virtualRouterRef_createdAt,
    virtualRouterRef_lastUpdatedAt,
    virtualRouterRef_meshName,
    virtualRouterRef_meshOwner,
    virtualRouterRef_resourceOwner,
    virtualRouterRef_version,
    virtualRouterRef_virtualRouterName,

    -- ** VirtualRouterServiceProvider
    virtualRouterServiceProvider_virtualRouterName,

    -- ** VirtualRouterSpec
    virtualRouterSpec_listeners,

    -- ** VirtualRouterStatus
    virtualRouterStatus_status,

    -- ** VirtualServiceBackend
    virtualServiceBackend_clientPolicy,
    virtualServiceBackend_virtualServiceName,

    -- ** VirtualServiceData
    virtualServiceData_meshName,
    virtualServiceData_metadata,
    virtualServiceData_spec,
    virtualServiceData_status,
    virtualServiceData_virtualServiceName,

    -- ** VirtualServiceProvider
    virtualServiceProvider_virtualRouter,
    virtualServiceProvider_virtualNode,

    -- ** VirtualServiceRef
    virtualServiceRef_arn,
    virtualServiceRef_createdAt,
    virtualServiceRef_lastUpdatedAt,
    virtualServiceRef_meshName,
    virtualServiceRef_meshOwner,
    virtualServiceRef_resourceOwner,
    virtualServiceRef_version,
    virtualServiceRef_virtualServiceName,

    -- ** VirtualServiceSpec
    virtualServiceSpec_provider,

    -- ** VirtualServiceStatus
    virtualServiceStatus_status,

    -- ** WeightedTarget
    weightedTarget_virtualNode,
    weightedTarget_weight,
  )
where

import Amazonka.AppMesh.CreateGatewayRoute
import Amazonka.AppMesh.CreateMesh
import Amazonka.AppMesh.CreateRoute
import Amazonka.AppMesh.CreateVirtualGateway
import Amazonka.AppMesh.CreateVirtualNode
import Amazonka.AppMesh.CreateVirtualRouter
import Amazonka.AppMesh.CreateVirtualService
import Amazonka.AppMesh.DeleteGatewayRoute
import Amazonka.AppMesh.DeleteMesh
import Amazonka.AppMesh.DeleteRoute
import Amazonka.AppMesh.DeleteVirtualGateway
import Amazonka.AppMesh.DeleteVirtualNode
import Amazonka.AppMesh.DeleteVirtualRouter
import Amazonka.AppMesh.DeleteVirtualService
import Amazonka.AppMesh.DescribeGatewayRoute
import Amazonka.AppMesh.DescribeMesh
import Amazonka.AppMesh.DescribeRoute
import Amazonka.AppMesh.DescribeVirtualGateway
import Amazonka.AppMesh.DescribeVirtualNode
import Amazonka.AppMesh.DescribeVirtualRouter
import Amazonka.AppMesh.DescribeVirtualService
import Amazonka.AppMesh.ListGatewayRoutes
import Amazonka.AppMesh.ListMeshes
import Amazonka.AppMesh.ListRoutes
import Amazonka.AppMesh.ListTagsForResource
import Amazonka.AppMesh.ListVirtualGateways
import Amazonka.AppMesh.ListVirtualNodes
import Amazonka.AppMesh.ListVirtualRouters
import Amazonka.AppMesh.ListVirtualServices
import Amazonka.AppMesh.TagResource
import Amazonka.AppMesh.Types.AccessLog
import Amazonka.AppMesh.Types.AwsCloudMapInstanceAttribute
import Amazonka.AppMesh.Types.AwsCloudMapServiceDiscovery
import Amazonka.AppMesh.Types.Backend
import Amazonka.AppMesh.Types.BackendDefaults
import Amazonka.AppMesh.Types.ClientPolicy
import Amazonka.AppMesh.Types.ClientPolicyTls
import Amazonka.AppMesh.Types.ClientTlsCertificate
import Amazonka.AppMesh.Types.DnsServiceDiscovery
import Amazonka.AppMesh.Types.Duration
import Amazonka.AppMesh.Types.EgressFilter
import Amazonka.AppMesh.Types.FileAccessLog
import Amazonka.AppMesh.Types.GatewayRouteData
import Amazonka.AppMesh.Types.GatewayRouteHostnameMatch
import Amazonka.AppMesh.Types.GatewayRouteHostnameRewrite
import Amazonka.AppMesh.Types.GatewayRouteRef
import Amazonka.AppMesh.Types.GatewayRouteSpec
import Amazonka.AppMesh.Types.GatewayRouteStatus
import Amazonka.AppMesh.Types.GatewayRouteTarget
import Amazonka.AppMesh.Types.GatewayRouteVirtualService
import Amazonka.AppMesh.Types.GrpcGatewayRoute
import Amazonka.AppMesh.Types.GrpcGatewayRouteAction
import Amazonka.AppMesh.Types.GrpcGatewayRouteMatch
import Amazonka.AppMesh.Types.GrpcGatewayRouteMetadata
import Amazonka.AppMesh.Types.GrpcGatewayRouteRewrite
import Amazonka.AppMesh.Types.GrpcMetadataMatchMethod
import Amazonka.AppMesh.Types.GrpcRetryPolicy
import Amazonka.AppMesh.Types.GrpcRoute
import Amazonka.AppMesh.Types.GrpcRouteAction
import Amazonka.AppMesh.Types.GrpcRouteMatch
import Amazonka.AppMesh.Types.GrpcRouteMetadata
import Amazonka.AppMesh.Types.GrpcRouteMetadataMatchMethod
import Amazonka.AppMesh.Types.GrpcTimeout
import Amazonka.AppMesh.Types.HeaderMatchMethod
import Amazonka.AppMesh.Types.HealthCheckPolicy
import Amazonka.AppMesh.Types.HttpGatewayRoute
import Amazonka.AppMesh.Types.HttpGatewayRouteAction
import Amazonka.AppMesh.Types.HttpGatewayRouteHeader
import Amazonka.AppMesh.Types.HttpGatewayRouteMatch
import Amazonka.AppMesh.Types.HttpGatewayRoutePathRewrite
import Amazonka.AppMesh.Types.HttpGatewayRoutePrefixRewrite
import Amazonka.AppMesh.Types.HttpGatewayRouteRewrite
import Amazonka.AppMesh.Types.HttpPathMatch
import Amazonka.AppMesh.Types.HttpQueryParameter
import Amazonka.AppMesh.Types.HttpRetryPolicy
import Amazonka.AppMesh.Types.HttpRoute
import Amazonka.AppMesh.Types.HttpRouteAction
import Amazonka.AppMesh.Types.HttpRouteHeader
import Amazonka.AppMesh.Types.HttpRouteMatch
import Amazonka.AppMesh.Types.HttpTimeout
import Amazonka.AppMesh.Types.Listener
import Amazonka.AppMesh.Types.ListenerTimeout
import Amazonka.AppMesh.Types.ListenerTls
import Amazonka.AppMesh.Types.ListenerTlsAcmCertificate
import Amazonka.AppMesh.Types.ListenerTlsCertificate
import Amazonka.AppMesh.Types.ListenerTlsFileCertificate
import Amazonka.AppMesh.Types.ListenerTlsSdsCertificate
import Amazonka.AppMesh.Types.ListenerTlsValidationContext
import Amazonka.AppMesh.Types.ListenerTlsValidationContextTrust
import Amazonka.AppMesh.Types.Logging
import Amazonka.AppMesh.Types.MatchRange
import Amazonka.AppMesh.Types.MeshData
import Amazonka.AppMesh.Types.MeshRef
import Amazonka.AppMesh.Types.MeshSpec
import Amazonka.AppMesh.Types.MeshStatus
import Amazonka.AppMesh.Types.OutlierDetection
import Amazonka.AppMesh.Types.PortMapping
import Amazonka.AppMesh.Types.QueryParameterMatch
import Amazonka.AppMesh.Types.ResourceMetadata
import Amazonka.AppMesh.Types.RouteData
import Amazonka.AppMesh.Types.RouteRef
import Amazonka.AppMesh.Types.RouteSpec
import Amazonka.AppMesh.Types.RouteStatus
import Amazonka.AppMesh.Types.ServiceDiscovery
import Amazonka.AppMesh.Types.SubjectAlternativeNameMatchers
import Amazonka.AppMesh.Types.SubjectAlternativeNames
import Amazonka.AppMesh.Types.TagRef
import Amazonka.AppMesh.Types.TcpRoute
import Amazonka.AppMesh.Types.TcpRouteAction
import Amazonka.AppMesh.Types.TcpTimeout
import Amazonka.AppMesh.Types.TlsValidationContext
import Amazonka.AppMesh.Types.TlsValidationContextAcmTrust
import Amazonka.AppMesh.Types.TlsValidationContextFileTrust
import Amazonka.AppMesh.Types.TlsValidationContextSdsTrust
import Amazonka.AppMesh.Types.TlsValidationContextTrust
import Amazonka.AppMesh.Types.VirtualGatewayAccessLog
import Amazonka.AppMesh.Types.VirtualGatewayBackendDefaults
import Amazonka.AppMesh.Types.VirtualGatewayClientPolicy
import Amazonka.AppMesh.Types.VirtualGatewayClientPolicyTls
import Amazonka.AppMesh.Types.VirtualGatewayClientTlsCertificate
import Amazonka.AppMesh.Types.VirtualGatewayConnectionPool
import Amazonka.AppMesh.Types.VirtualGatewayData
import Amazonka.AppMesh.Types.VirtualGatewayFileAccessLog
import Amazonka.AppMesh.Types.VirtualGatewayGrpcConnectionPool
import Amazonka.AppMesh.Types.VirtualGatewayHealthCheckPolicy
import Amazonka.AppMesh.Types.VirtualGatewayHttp2ConnectionPool
import Amazonka.AppMesh.Types.VirtualGatewayHttpConnectionPool
import Amazonka.AppMesh.Types.VirtualGatewayListener
import Amazonka.AppMesh.Types.VirtualGatewayListenerTls
import Amazonka.AppMesh.Types.VirtualGatewayListenerTlsAcmCertificate
import Amazonka.AppMesh.Types.VirtualGatewayListenerTlsCertificate
import Amazonka.AppMesh.Types.VirtualGatewayListenerTlsFileCertificate
import Amazonka.AppMesh.Types.VirtualGatewayListenerTlsSdsCertificate
import Amazonka.AppMesh.Types.VirtualGatewayListenerTlsValidationContext
import Amazonka.AppMesh.Types.VirtualGatewayListenerTlsValidationContextTrust
import Amazonka.AppMesh.Types.VirtualGatewayLogging
import Amazonka.AppMesh.Types.VirtualGatewayPortMapping
import Amazonka.AppMesh.Types.VirtualGatewayRef
import Amazonka.AppMesh.Types.VirtualGatewaySpec
import Amazonka.AppMesh.Types.VirtualGatewayStatus
import Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContext
import Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContextAcmTrust
import Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContextFileTrust
import Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContextSdsTrust
import Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContextTrust
import Amazonka.AppMesh.Types.VirtualNodeConnectionPool
import Amazonka.AppMesh.Types.VirtualNodeData
import Amazonka.AppMesh.Types.VirtualNodeGrpcConnectionPool
import Amazonka.AppMesh.Types.VirtualNodeHttp2ConnectionPool
import Amazonka.AppMesh.Types.VirtualNodeHttpConnectionPool
import Amazonka.AppMesh.Types.VirtualNodeRef
import Amazonka.AppMesh.Types.VirtualNodeServiceProvider
import Amazonka.AppMesh.Types.VirtualNodeSpec
import Amazonka.AppMesh.Types.VirtualNodeStatus
import Amazonka.AppMesh.Types.VirtualNodeTcpConnectionPool
import Amazonka.AppMesh.Types.VirtualRouterData
import Amazonka.AppMesh.Types.VirtualRouterListener
import Amazonka.AppMesh.Types.VirtualRouterRef
import Amazonka.AppMesh.Types.VirtualRouterServiceProvider
import Amazonka.AppMesh.Types.VirtualRouterSpec
import Amazonka.AppMesh.Types.VirtualRouterStatus
import Amazonka.AppMesh.Types.VirtualServiceBackend
import Amazonka.AppMesh.Types.VirtualServiceData
import Amazonka.AppMesh.Types.VirtualServiceProvider
import Amazonka.AppMesh.Types.VirtualServiceRef
import Amazonka.AppMesh.Types.VirtualServiceSpec
import Amazonka.AppMesh.Types.VirtualServiceStatus
import Amazonka.AppMesh.Types.WeightedTarget
import Amazonka.AppMesh.UntagResource
import Amazonka.AppMesh.UpdateGatewayRoute
import Amazonka.AppMesh.UpdateMesh
import Amazonka.AppMesh.UpdateRoute
import Amazonka.AppMesh.UpdateVirtualGateway
import Amazonka.AppMesh.UpdateVirtualNode
import Amazonka.AppMesh.UpdateVirtualRouter
import Amazonka.AppMesh.UpdateVirtualService
