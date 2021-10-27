{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppMesh.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppMesh.Lens
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

import Network.AWS.AppMesh.CreateGatewayRoute
import Network.AWS.AppMesh.CreateMesh
import Network.AWS.AppMesh.CreateRoute
import Network.AWS.AppMesh.CreateVirtualGateway
import Network.AWS.AppMesh.CreateVirtualNode
import Network.AWS.AppMesh.CreateVirtualRouter
import Network.AWS.AppMesh.CreateVirtualService
import Network.AWS.AppMesh.DeleteGatewayRoute
import Network.AWS.AppMesh.DeleteMesh
import Network.AWS.AppMesh.DeleteRoute
import Network.AWS.AppMesh.DeleteVirtualGateway
import Network.AWS.AppMesh.DeleteVirtualNode
import Network.AWS.AppMesh.DeleteVirtualRouter
import Network.AWS.AppMesh.DeleteVirtualService
import Network.AWS.AppMesh.DescribeGatewayRoute
import Network.AWS.AppMesh.DescribeMesh
import Network.AWS.AppMesh.DescribeRoute
import Network.AWS.AppMesh.DescribeVirtualGateway
import Network.AWS.AppMesh.DescribeVirtualNode
import Network.AWS.AppMesh.DescribeVirtualRouter
import Network.AWS.AppMesh.DescribeVirtualService
import Network.AWS.AppMesh.ListGatewayRoutes
import Network.AWS.AppMesh.ListMeshes
import Network.AWS.AppMesh.ListRoutes
import Network.AWS.AppMesh.ListTagsForResource
import Network.AWS.AppMesh.ListVirtualGateways
import Network.AWS.AppMesh.ListVirtualNodes
import Network.AWS.AppMesh.ListVirtualRouters
import Network.AWS.AppMesh.ListVirtualServices
import Network.AWS.AppMesh.TagResource
import Network.AWS.AppMesh.Types.AccessLog
import Network.AWS.AppMesh.Types.AwsCloudMapInstanceAttribute
import Network.AWS.AppMesh.Types.AwsCloudMapServiceDiscovery
import Network.AWS.AppMesh.Types.Backend
import Network.AWS.AppMesh.Types.BackendDefaults
import Network.AWS.AppMesh.Types.ClientPolicy
import Network.AWS.AppMesh.Types.ClientPolicyTls
import Network.AWS.AppMesh.Types.ClientTlsCertificate
import Network.AWS.AppMesh.Types.DnsServiceDiscovery
import Network.AWS.AppMesh.Types.Duration
import Network.AWS.AppMesh.Types.EgressFilter
import Network.AWS.AppMesh.Types.FileAccessLog
import Network.AWS.AppMesh.Types.GatewayRouteData
import Network.AWS.AppMesh.Types.GatewayRouteHostnameMatch
import Network.AWS.AppMesh.Types.GatewayRouteHostnameRewrite
import Network.AWS.AppMesh.Types.GatewayRouteRef
import Network.AWS.AppMesh.Types.GatewayRouteSpec
import Network.AWS.AppMesh.Types.GatewayRouteStatus
import Network.AWS.AppMesh.Types.GatewayRouteTarget
import Network.AWS.AppMesh.Types.GatewayRouteVirtualService
import Network.AWS.AppMesh.Types.GrpcGatewayRoute
import Network.AWS.AppMesh.Types.GrpcGatewayRouteAction
import Network.AWS.AppMesh.Types.GrpcGatewayRouteMatch
import Network.AWS.AppMesh.Types.GrpcGatewayRouteMetadata
import Network.AWS.AppMesh.Types.GrpcGatewayRouteRewrite
import Network.AWS.AppMesh.Types.GrpcMetadataMatchMethod
import Network.AWS.AppMesh.Types.GrpcRetryPolicy
import Network.AWS.AppMesh.Types.GrpcRoute
import Network.AWS.AppMesh.Types.GrpcRouteAction
import Network.AWS.AppMesh.Types.GrpcRouteMatch
import Network.AWS.AppMesh.Types.GrpcRouteMetadata
import Network.AWS.AppMesh.Types.GrpcRouteMetadataMatchMethod
import Network.AWS.AppMesh.Types.GrpcTimeout
import Network.AWS.AppMesh.Types.HeaderMatchMethod
import Network.AWS.AppMesh.Types.HealthCheckPolicy
import Network.AWS.AppMesh.Types.HttpGatewayRoute
import Network.AWS.AppMesh.Types.HttpGatewayRouteAction
import Network.AWS.AppMesh.Types.HttpGatewayRouteHeader
import Network.AWS.AppMesh.Types.HttpGatewayRouteMatch
import Network.AWS.AppMesh.Types.HttpGatewayRoutePathRewrite
import Network.AWS.AppMesh.Types.HttpGatewayRoutePrefixRewrite
import Network.AWS.AppMesh.Types.HttpGatewayRouteRewrite
import Network.AWS.AppMesh.Types.HttpPathMatch
import Network.AWS.AppMesh.Types.HttpQueryParameter
import Network.AWS.AppMesh.Types.HttpRetryPolicy
import Network.AWS.AppMesh.Types.HttpRoute
import Network.AWS.AppMesh.Types.HttpRouteAction
import Network.AWS.AppMesh.Types.HttpRouteHeader
import Network.AWS.AppMesh.Types.HttpRouteMatch
import Network.AWS.AppMesh.Types.HttpTimeout
import Network.AWS.AppMesh.Types.Listener
import Network.AWS.AppMesh.Types.ListenerTimeout
import Network.AWS.AppMesh.Types.ListenerTls
import Network.AWS.AppMesh.Types.ListenerTlsAcmCertificate
import Network.AWS.AppMesh.Types.ListenerTlsCertificate
import Network.AWS.AppMesh.Types.ListenerTlsFileCertificate
import Network.AWS.AppMesh.Types.ListenerTlsSdsCertificate
import Network.AWS.AppMesh.Types.ListenerTlsValidationContext
import Network.AWS.AppMesh.Types.ListenerTlsValidationContextTrust
import Network.AWS.AppMesh.Types.Logging
import Network.AWS.AppMesh.Types.MatchRange
import Network.AWS.AppMesh.Types.MeshData
import Network.AWS.AppMesh.Types.MeshRef
import Network.AWS.AppMesh.Types.MeshSpec
import Network.AWS.AppMesh.Types.MeshStatus
import Network.AWS.AppMesh.Types.OutlierDetection
import Network.AWS.AppMesh.Types.PortMapping
import Network.AWS.AppMesh.Types.QueryParameterMatch
import Network.AWS.AppMesh.Types.ResourceMetadata
import Network.AWS.AppMesh.Types.RouteData
import Network.AWS.AppMesh.Types.RouteRef
import Network.AWS.AppMesh.Types.RouteSpec
import Network.AWS.AppMesh.Types.RouteStatus
import Network.AWS.AppMesh.Types.ServiceDiscovery
import Network.AWS.AppMesh.Types.SubjectAlternativeNameMatchers
import Network.AWS.AppMesh.Types.SubjectAlternativeNames
import Network.AWS.AppMesh.Types.TagRef
import Network.AWS.AppMesh.Types.TcpRoute
import Network.AWS.AppMesh.Types.TcpRouteAction
import Network.AWS.AppMesh.Types.TcpTimeout
import Network.AWS.AppMesh.Types.TlsValidationContext
import Network.AWS.AppMesh.Types.TlsValidationContextAcmTrust
import Network.AWS.AppMesh.Types.TlsValidationContextFileTrust
import Network.AWS.AppMesh.Types.TlsValidationContextSdsTrust
import Network.AWS.AppMesh.Types.TlsValidationContextTrust
import Network.AWS.AppMesh.Types.VirtualGatewayAccessLog
import Network.AWS.AppMesh.Types.VirtualGatewayBackendDefaults
import Network.AWS.AppMesh.Types.VirtualGatewayClientPolicy
import Network.AWS.AppMesh.Types.VirtualGatewayClientPolicyTls
import Network.AWS.AppMesh.Types.VirtualGatewayClientTlsCertificate
import Network.AWS.AppMesh.Types.VirtualGatewayConnectionPool
import Network.AWS.AppMesh.Types.VirtualGatewayData
import Network.AWS.AppMesh.Types.VirtualGatewayFileAccessLog
import Network.AWS.AppMesh.Types.VirtualGatewayGrpcConnectionPool
import Network.AWS.AppMesh.Types.VirtualGatewayHealthCheckPolicy
import Network.AWS.AppMesh.Types.VirtualGatewayHttp2ConnectionPool
import Network.AWS.AppMesh.Types.VirtualGatewayHttpConnectionPool
import Network.AWS.AppMesh.Types.VirtualGatewayListener
import Network.AWS.AppMesh.Types.VirtualGatewayListenerTls
import Network.AWS.AppMesh.Types.VirtualGatewayListenerTlsAcmCertificate
import Network.AWS.AppMesh.Types.VirtualGatewayListenerTlsCertificate
import Network.AWS.AppMesh.Types.VirtualGatewayListenerTlsFileCertificate
import Network.AWS.AppMesh.Types.VirtualGatewayListenerTlsSdsCertificate
import Network.AWS.AppMesh.Types.VirtualGatewayListenerTlsValidationContext
import Network.AWS.AppMesh.Types.VirtualGatewayListenerTlsValidationContextTrust
import Network.AWS.AppMesh.Types.VirtualGatewayLogging
import Network.AWS.AppMesh.Types.VirtualGatewayPortMapping
import Network.AWS.AppMesh.Types.VirtualGatewayRef
import Network.AWS.AppMesh.Types.VirtualGatewaySpec
import Network.AWS.AppMesh.Types.VirtualGatewayStatus
import Network.AWS.AppMesh.Types.VirtualGatewayTlsValidationContext
import Network.AWS.AppMesh.Types.VirtualGatewayTlsValidationContextAcmTrust
import Network.AWS.AppMesh.Types.VirtualGatewayTlsValidationContextFileTrust
import Network.AWS.AppMesh.Types.VirtualGatewayTlsValidationContextSdsTrust
import Network.AWS.AppMesh.Types.VirtualGatewayTlsValidationContextTrust
import Network.AWS.AppMesh.Types.VirtualNodeConnectionPool
import Network.AWS.AppMesh.Types.VirtualNodeData
import Network.AWS.AppMesh.Types.VirtualNodeGrpcConnectionPool
import Network.AWS.AppMesh.Types.VirtualNodeHttp2ConnectionPool
import Network.AWS.AppMesh.Types.VirtualNodeHttpConnectionPool
import Network.AWS.AppMesh.Types.VirtualNodeRef
import Network.AWS.AppMesh.Types.VirtualNodeServiceProvider
import Network.AWS.AppMesh.Types.VirtualNodeSpec
import Network.AWS.AppMesh.Types.VirtualNodeStatus
import Network.AWS.AppMesh.Types.VirtualNodeTcpConnectionPool
import Network.AWS.AppMesh.Types.VirtualRouterData
import Network.AWS.AppMesh.Types.VirtualRouterListener
import Network.AWS.AppMesh.Types.VirtualRouterRef
import Network.AWS.AppMesh.Types.VirtualRouterServiceProvider
import Network.AWS.AppMesh.Types.VirtualRouterSpec
import Network.AWS.AppMesh.Types.VirtualRouterStatus
import Network.AWS.AppMesh.Types.VirtualServiceBackend
import Network.AWS.AppMesh.Types.VirtualServiceData
import Network.AWS.AppMesh.Types.VirtualServiceProvider
import Network.AWS.AppMesh.Types.VirtualServiceRef
import Network.AWS.AppMesh.Types.VirtualServiceSpec
import Network.AWS.AppMesh.Types.VirtualServiceStatus
import Network.AWS.AppMesh.Types.WeightedTarget
import Network.AWS.AppMesh.UntagResource
import Network.AWS.AppMesh.UpdateGatewayRoute
import Network.AWS.AppMesh.UpdateMesh
import Network.AWS.AppMesh.UpdateRoute
import Network.AWS.AppMesh.UpdateVirtualGateway
import Network.AWS.AppMesh.UpdateVirtualNode
import Network.AWS.AppMesh.UpdateVirtualRouter
import Network.AWS.AppMesh.UpdateVirtualService
