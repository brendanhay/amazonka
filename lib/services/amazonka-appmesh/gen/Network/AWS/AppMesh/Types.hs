{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppMesh.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppMesh.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _TooManyTagsException,
    _ConflictException,
    _ForbiddenException,
    _NotFoundException,
    _TooManyRequestsException,
    _InternalServerErrorException,
    _ServiceUnavailableException,
    _BadRequestException,
    _LimitExceededException,
    _ResourceInUseException,

    -- * DefaultGatewayRouteRewrite
    DefaultGatewayRouteRewrite (..),

    -- * DnsResponseType
    DnsResponseType (..),

    -- * DurationUnit
    DurationUnit (..),

    -- * EgressFilterType
    EgressFilterType (..),

    -- * GatewayRouteStatusCode
    GatewayRouteStatusCode (..),

    -- * GrpcRetryPolicyEvent
    GrpcRetryPolicyEvent (..),

    -- * HttpMethod
    HttpMethod (..),

    -- * HttpScheme
    HttpScheme (..),

    -- * ListenerTlsMode
    ListenerTlsMode (..),

    -- * MeshStatusCode
    MeshStatusCode (..),

    -- * PortProtocol
    PortProtocol (..),

    -- * RouteStatusCode
    RouteStatusCode (..),

    -- * TcpRetryPolicyEvent
    TcpRetryPolicyEvent (..),

    -- * VirtualGatewayListenerTlsMode
    VirtualGatewayListenerTlsMode (..),

    -- * VirtualGatewayPortProtocol
    VirtualGatewayPortProtocol (..),

    -- * VirtualGatewayStatusCode
    VirtualGatewayStatusCode (..),

    -- * VirtualNodeStatusCode
    VirtualNodeStatusCode (..),

    -- * VirtualRouterStatusCode
    VirtualRouterStatusCode (..),

    -- * VirtualServiceStatusCode
    VirtualServiceStatusCode (..),

    -- * AccessLog
    AccessLog (..),
    newAccessLog,
    accessLog_file,

    -- * AwsCloudMapInstanceAttribute
    AwsCloudMapInstanceAttribute (..),
    newAwsCloudMapInstanceAttribute,
    awsCloudMapInstanceAttribute_key,
    awsCloudMapInstanceAttribute_value,

    -- * AwsCloudMapServiceDiscovery
    AwsCloudMapServiceDiscovery (..),
    newAwsCloudMapServiceDiscovery,
    awsCloudMapServiceDiscovery_attributes,
    awsCloudMapServiceDiscovery_namespaceName,
    awsCloudMapServiceDiscovery_serviceName,

    -- * Backend
    Backend (..),
    newBackend,
    backend_virtualService,

    -- * BackendDefaults
    BackendDefaults (..),
    newBackendDefaults,
    backendDefaults_clientPolicy,

    -- * ClientPolicy
    ClientPolicy (..),
    newClientPolicy,
    clientPolicy_tls,

    -- * ClientPolicyTls
    ClientPolicyTls (..),
    newClientPolicyTls,
    clientPolicyTls_ports,
    clientPolicyTls_certificate,
    clientPolicyTls_enforce,
    clientPolicyTls_validation,

    -- * ClientTlsCertificate
    ClientTlsCertificate (..),
    newClientTlsCertificate,
    clientTlsCertificate_sds,
    clientTlsCertificate_file,

    -- * DnsServiceDiscovery
    DnsServiceDiscovery (..),
    newDnsServiceDiscovery,
    dnsServiceDiscovery_responseType,
    dnsServiceDiscovery_hostname,

    -- * Duration
    Duration (..),
    newDuration,
    duration_value,
    duration_unit,

    -- * EgressFilter
    EgressFilter (..),
    newEgressFilter,
    egressFilter_type,

    -- * FileAccessLog
    FileAccessLog (..),
    newFileAccessLog,
    fileAccessLog_path,

    -- * GatewayRouteData
    GatewayRouteData (..),
    newGatewayRouteData,
    gatewayRouteData_gatewayRouteName,
    gatewayRouteData_meshName,
    gatewayRouteData_metadata,
    gatewayRouteData_spec,
    gatewayRouteData_status,
    gatewayRouteData_virtualGatewayName,

    -- * GatewayRouteHostnameMatch
    GatewayRouteHostnameMatch (..),
    newGatewayRouteHostnameMatch,
    gatewayRouteHostnameMatch_suffix,
    gatewayRouteHostnameMatch_exact,

    -- * GatewayRouteHostnameRewrite
    GatewayRouteHostnameRewrite (..),
    newGatewayRouteHostnameRewrite,
    gatewayRouteHostnameRewrite_defaultTargetHostname,

    -- * GatewayRouteRef
    GatewayRouteRef (..),
    newGatewayRouteRef,
    gatewayRouteRef_arn,
    gatewayRouteRef_createdAt,
    gatewayRouteRef_gatewayRouteName,
    gatewayRouteRef_lastUpdatedAt,
    gatewayRouteRef_meshName,
    gatewayRouteRef_meshOwner,
    gatewayRouteRef_resourceOwner,
    gatewayRouteRef_version,
    gatewayRouteRef_virtualGatewayName,

    -- * GatewayRouteSpec
    GatewayRouteSpec (..),
    newGatewayRouteSpec,
    gatewayRouteSpec_priority,
    gatewayRouteSpec_http2Route,
    gatewayRouteSpec_grpcRoute,
    gatewayRouteSpec_httpRoute,

    -- * GatewayRouteStatus
    GatewayRouteStatus (..),
    newGatewayRouteStatus,
    gatewayRouteStatus_status,

    -- * GatewayRouteTarget
    GatewayRouteTarget (..),
    newGatewayRouteTarget,
    gatewayRouteTarget_virtualService,

    -- * GatewayRouteVirtualService
    GatewayRouteVirtualService (..),
    newGatewayRouteVirtualService,
    gatewayRouteVirtualService_virtualServiceName,

    -- * GrpcGatewayRoute
    GrpcGatewayRoute (..),
    newGrpcGatewayRoute,
    grpcGatewayRoute_action,
    grpcGatewayRoute_match,

    -- * GrpcGatewayRouteAction
    GrpcGatewayRouteAction (..),
    newGrpcGatewayRouteAction,
    grpcGatewayRouteAction_rewrite,
    grpcGatewayRouteAction_target,

    -- * GrpcGatewayRouteMatch
    GrpcGatewayRouteMatch (..),
    newGrpcGatewayRouteMatch,
    grpcGatewayRouteMatch_hostname,
    grpcGatewayRouteMatch_serviceName,
    grpcGatewayRouteMatch_metadata,

    -- * GrpcGatewayRouteMetadata
    GrpcGatewayRouteMetadata (..),
    newGrpcGatewayRouteMetadata,
    grpcGatewayRouteMetadata_invert,
    grpcGatewayRouteMetadata_match,
    grpcGatewayRouteMetadata_name,

    -- * GrpcGatewayRouteRewrite
    GrpcGatewayRouteRewrite (..),
    newGrpcGatewayRouteRewrite,
    grpcGatewayRouteRewrite_hostname,

    -- * GrpcMetadataMatchMethod
    GrpcMetadataMatchMethod (..),
    newGrpcMetadataMatchMethod,
    grpcMetadataMatchMethod_suffix,
    grpcMetadataMatchMethod_regex,
    grpcMetadataMatchMethod_prefix,
    grpcMetadataMatchMethod_range,
    grpcMetadataMatchMethod_exact,

    -- * GrpcRetryPolicy
    GrpcRetryPolicy (..),
    newGrpcRetryPolicy,
    grpcRetryPolicy_httpRetryEvents,
    grpcRetryPolicy_grpcRetryEvents,
    grpcRetryPolicy_tcpRetryEvents,
    grpcRetryPolicy_maxRetries,
    grpcRetryPolicy_perRetryTimeout,

    -- * GrpcRoute
    GrpcRoute (..),
    newGrpcRoute,
    grpcRoute_retryPolicy,
    grpcRoute_timeout,
    grpcRoute_action,
    grpcRoute_match,

    -- * GrpcRouteAction
    GrpcRouteAction (..),
    newGrpcRouteAction,
    grpcRouteAction_weightedTargets,

    -- * GrpcRouteMatch
    GrpcRouteMatch (..),
    newGrpcRouteMatch,
    grpcRouteMatch_methodName,
    grpcRouteMatch_serviceName,
    grpcRouteMatch_metadata,

    -- * GrpcRouteMetadata
    GrpcRouteMetadata (..),
    newGrpcRouteMetadata,
    grpcRouteMetadata_invert,
    grpcRouteMetadata_match,
    grpcRouteMetadata_name,

    -- * GrpcRouteMetadataMatchMethod
    GrpcRouteMetadataMatchMethod (..),
    newGrpcRouteMetadataMatchMethod,
    grpcRouteMetadataMatchMethod_suffix,
    grpcRouteMetadataMatchMethod_regex,
    grpcRouteMetadataMatchMethod_prefix,
    grpcRouteMetadataMatchMethod_range,
    grpcRouteMetadataMatchMethod_exact,

    -- * GrpcTimeout
    GrpcTimeout (..),
    newGrpcTimeout,
    grpcTimeout_idle,
    grpcTimeout_perRequest,

    -- * HeaderMatchMethod
    HeaderMatchMethod (..),
    newHeaderMatchMethod,
    headerMatchMethod_suffix,
    headerMatchMethod_regex,
    headerMatchMethod_prefix,
    headerMatchMethod_range,
    headerMatchMethod_exact,

    -- * HealthCheckPolicy
    HealthCheckPolicy (..),
    newHealthCheckPolicy,
    healthCheckPolicy_path,
    healthCheckPolicy_port,
    healthCheckPolicy_healthyThreshold,
    healthCheckPolicy_intervalMillis,
    healthCheckPolicy_protocol,
    healthCheckPolicy_timeoutMillis,
    healthCheckPolicy_unhealthyThreshold,

    -- * HttpGatewayRoute
    HttpGatewayRoute (..),
    newHttpGatewayRoute,
    httpGatewayRoute_action,
    httpGatewayRoute_match,

    -- * HttpGatewayRouteAction
    HttpGatewayRouteAction (..),
    newHttpGatewayRouteAction,
    httpGatewayRouteAction_rewrite,
    httpGatewayRouteAction_target,

    -- * HttpGatewayRouteHeader
    HttpGatewayRouteHeader (..),
    newHttpGatewayRouteHeader,
    httpGatewayRouteHeader_invert,
    httpGatewayRouteHeader_match,
    httpGatewayRouteHeader_name,

    -- * HttpGatewayRouteMatch
    HttpGatewayRouteMatch (..),
    newHttpGatewayRouteMatch,
    httpGatewayRouteMatch_hostname,
    httpGatewayRouteMatch_path,
    httpGatewayRouteMatch_prefix,
    httpGatewayRouteMatch_queryParameters,
    httpGatewayRouteMatch_headers,
    httpGatewayRouteMatch_method,

    -- * HttpGatewayRoutePathRewrite
    HttpGatewayRoutePathRewrite (..),
    newHttpGatewayRoutePathRewrite,
    httpGatewayRoutePathRewrite_exact,

    -- * HttpGatewayRoutePrefixRewrite
    HttpGatewayRoutePrefixRewrite (..),
    newHttpGatewayRoutePrefixRewrite,
    httpGatewayRoutePrefixRewrite_value,
    httpGatewayRoutePrefixRewrite_defaultPrefix,

    -- * HttpGatewayRouteRewrite
    HttpGatewayRouteRewrite (..),
    newHttpGatewayRouteRewrite,
    httpGatewayRouteRewrite_hostname,
    httpGatewayRouteRewrite_path,
    httpGatewayRouteRewrite_prefix,

    -- * HttpPathMatch
    HttpPathMatch (..),
    newHttpPathMatch,
    httpPathMatch_regex,
    httpPathMatch_exact,

    -- * HttpQueryParameter
    HttpQueryParameter (..),
    newHttpQueryParameter,
    httpQueryParameter_match,
    httpQueryParameter_name,

    -- * HttpRetryPolicy
    HttpRetryPolicy (..),
    newHttpRetryPolicy,
    httpRetryPolicy_httpRetryEvents,
    httpRetryPolicy_tcpRetryEvents,
    httpRetryPolicy_maxRetries,
    httpRetryPolicy_perRetryTimeout,

    -- * HttpRoute
    HttpRoute (..),
    newHttpRoute,
    httpRoute_retryPolicy,
    httpRoute_timeout,
    httpRoute_action,
    httpRoute_match,

    -- * HttpRouteAction
    HttpRouteAction (..),
    newHttpRouteAction,
    httpRouteAction_weightedTargets,

    -- * HttpRouteHeader
    HttpRouteHeader (..),
    newHttpRouteHeader,
    httpRouteHeader_invert,
    httpRouteHeader_match,
    httpRouteHeader_name,

    -- * HttpRouteMatch
    HttpRouteMatch (..),
    newHttpRouteMatch,
    httpRouteMatch_path,
    httpRouteMatch_prefix,
    httpRouteMatch_queryParameters,
    httpRouteMatch_headers,
    httpRouteMatch_method,
    httpRouteMatch_scheme,

    -- * HttpTimeout
    HttpTimeout (..),
    newHttpTimeout,
    httpTimeout_idle,
    httpTimeout_perRequest,

    -- * Listener
    Listener (..),
    newListener,
    listener_healthCheck,
    listener_connectionPool,
    listener_tls,
    listener_outlierDetection,
    listener_timeout,
    listener_portMapping,

    -- * ListenerTimeout
    ListenerTimeout (..),
    newListenerTimeout,
    listenerTimeout_http2,
    listenerTimeout_grpc,
    listenerTimeout_tcp,
    listenerTimeout_http,

    -- * ListenerTls
    ListenerTls (..),
    newListenerTls,
    listenerTls_validation,
    listenerTls_certificate,
    listenerTls_mode,

    -- * ListenerTlsAcmCertificate
    ListenerTlsAcmCertificate (..),
    newListenerTlsAcmCertificate,
    listenerTlsAcmCertificate_certificateArn,

    -- * ListenerTlsCertificate
    ListenerTlsCertificate (..),
    newListenerTlsCertificate,
    listenerTlsCertificate_acm,
    listenerTlsCertificate_sds,
    listenerTlsCertificate_file,

    -- * ListenerTlsFileCertificate
    ListenerTlsFileCertificate (..),
    newListenerTlsFileCertificate,
    listenerTlsFileCertificate_certificateChain,
    listenerTlsFileCertificate_privateKey,

    -- * ListenerTlsSdsCertificate
    ListenerTlsSdsCertificate (..),
    newListenerTlsSdsCertificate,
    listenerTlsSdsCertificate_secretName,

    -- * ListenerTlsValidationContext
    ListenerTlsValidationContext (..),
    newListenerTlsValidationContext,
    listenerTlsValidationContext_subjectAlternativeNames,
    listenerTlsValidationContext_trust,

    -- * ListenerTlsValidationContextTrust
    ListenerTlsValidationContextTrust (..),
    newListenerTlsValidationContextTrust,
    listenerTlsValidationContextTrust_sds,
    listenerTlsValidationContextTrust_file,

    -- * Logging
    Logging (..),
    newLogging,
    logging_accessLog,

    -- * MatchRange
    MatchRange (..),
    newMatchRange,
    matchRange_end,
    matchRange_start,

    -- * MeshData
    MeshData (..),
    newMeshData,
    meshData_meshName,
    meshData_metadata,
    meshData_spec,
    meshData_status,

    -- * MeshRef
    MeshRef (..),
    newMeshRef,
    meshRef_arn,
    meshRef_createdAt,
    meshRef_lastUpdatedAt,
    meshRef_meshName,
    meshRef_meshOwner,
    meshRef_resourceOwner,
    meshRef_version,

    -- * MeshSpec
    MeshSpec (..),
    newMeshSpec,
    meshSpec_egressFilter,

    -- * MeshStatus
    MeshStatus (..),
    newMeshStatus,
    meshStatus_status,

    -- * OutlierDetection
    OutlierDetection (..),
    newOutlierDetection,
    outlierDetection_baseEjectionDuration,
    outlierDetection_interval,
    outlierDetection_maxEjectionPercent,
    outlierDetection_maxServerErrors,

    -- * PortMapping
    PortMapping (..),
    newPortMapping,
    portMapping_port,
    portMapping_protocol,

    -- * QueryParameterMatch
    QueryParameterMatch (..),
    newQueryParameterMatch,
    queryParameterMatch_exact,

    -- * ResourceMetadata
    ResourceMetadata (..),
    newResourceMetadata,
    resourceMetadata_arn,
    resourceMetadata_createdAt,
    resourceMetadata_lastUpdatedAt,
    resourceMetadata_meshOwner,
    resourceMetadata_resourceOwner,
    resourceMetadata_uid,
    resourceMetadata_version,

    -- * RouteData
    RouteData (..),
    newRouteData,
    routeData_meshName,
    routeData_metadata,
    routeData_routeName,
    routeData_spec,
    routeData_status,
    routeData_virtualRouterName,

    -- * RouteRef
    RouteRef (..),
    newRouteRef,
    routeRef_arn,
    routeRef_createdAt,
    routeRef_lastUpdatedAt,
    routeRef_meshName,
    routeRef_meshOwner,
    routeRef_resourceOwner,
    routeRef_routeName,
    routeRef_version,
    routeRef_virtualRouterName,

    -- * RouteSpec
    RouteSpec (..),
    newRouteSpec,
    routeSpec_priority,
    routeSpec_http2Route,
    routeSpec_grpcRoute,
    routeSpec_tcpRoute,
    routeSpec_httpRoute,

    -- * RouteStatus
    RouteStatus (..),
    newRouteStatus,
    routeStatus_status,

    -- * ServiceDiscovery
    ServiceDiscovery (..),
    newServiceDiscovery,
    serviceDiscovery_awsCloudMap,
    serviceDiscovery_dns,

    -- * SubjectAlternativeNameMatchers
    SubjectAlternativeNameMatchers (..),
    newSubjectAlternativeNameMatchers,
    subjectAlternativeNameMatchers_exact,

    -- * SubjectAlternativeNames
    SubjectAlternativeNames (..),
    newSubjectAlternativeNames,
    subjectAlternativeNames_match,

    -- * TagRef
    TagRef (..),
    newTagRef,
    tagRef_key,
    tagRef_value,

    -- * TcpRoute
    TcpRoute (..),
    newTcpRoute,
    tcpRoute_timeout,
    tcpRoute_action,

    -- * TcpRouteAction
    TcpRouteAction (..),
    newTcpRouteAction,
    tcpRouteAction_weightedTargets,

    -- * TcpTimeout
    TcpTimeout (..),
    newTcpTimeout,
    tcpTimeout_idle,

    -- * TlsValidationContext
    TlsValidationContext (..),
    newTlsValidationContext,
    tlsValidationContext_subjectAlternativeNames,
    tlsValidationContext_trust,

    -- * TlsValidationContextAcmTrust
    TlsValidationContextAcmTrust (..),
    newTlsValidationContextAcmTrust,
    tlsValidationContextAcmTrust_certificateAuthorityArns,

    -- * TlsValidationContextFileTrust
    TlsValidationContextFileTrust (..),
    newTlsValidationContextFileTrust,
    tlsValidationContextFileTrust_certificateChain,

    -- * TlsValidationContextSdsTrust
    TlsValidationContextSdsTrust (..),
    newTlsValidationContextSdsTrust,
    tlsValidationContextSdsTrust_secretName,

    -- * TlsValidationContextTrust
    TlsValidationContextTrust (..),
    newTlsValidationContextTrust,
    tlsValidationContextTrust_acm,
    tlsValidationContextTrust_sds,
    tlsValidationContextTrust_file,

    -- * VirtualGatewayAccessLog
    VirtualGatewayAccessLog (..),
    newVirtualGatewayAccessLog,
    virtualGatewayAccessLog_file,

    -- * VirtualGatewayBackendDefaults
    VirtualGatewayBackendDefaults (..),
    newVirtualGatewayBackendDefaults,
    virtualGatewayBackendDefaults_clientPolicy,

    -- * VirtualGatewayClientPolicy
    VirtualGatewayClientPolicy (..),
    newVirtualGatewayClientPolicy,
    virtualGatewayClientPolicy_tls,

    -- * VirtualGatewayClientPolicyTls
    VirtualGatewayClientPolicyTls (..),
    newVirtualGatewayClientPolicyTls,
    virtualGatewayClientPolicyTls_ports,
    virtualGatewayClientPolicyTls_certificate,
    virtualGatewayClientPolicyTls_enforce,
    virtualGatewayClientPolicyTls_validation,

    -- * VirtualGatewayClientTlsCertificate
    VirtualGatewayClientTlsCertificate (..),
    newVirtualGatewayClientTlsCertificate,
    virtualGatewayClientTlsCertificate_sds,
    virtualGatewayClientTlsCertificate_file,

    -- * VirtualGatewayConnectionPool
    VirtualGatewayConnectionPool (..),
    newVirtualGatewayConnectionPool,
    virtualGatewayConnectionPool_http2,
    virtualGatewayConnectionPool_grpc,
    virtualGatewayConnectionPool_http,

    -- * VirtualGatewayData
    VirtualGatewayData (..),
    newVirtualGatewayData,
    virtualGatewayData_meshName,
    virtualGatewayData_metadata,
    virtualGatewayData_spec,
    virtualGatewayData_status,
    virtualGatewayData_virtualGatewayName,

    -- * VirtualGatewayFileAccessLog
    VirtualGatewayFileAccessLog (..),
    newVirtualGatewayFileAccessLog,
    virtualGatewayFileAccessLog_path,

    -- * VirtualGatewayGrpcConnectionPool
    VirtualGatewayGrpcConnectionPool (..),
    newVirtualGatewayGrpcConnectionPool,
    virtualGatewayGrpcConnectionPool_maxRequests,

    -- * VirtualGatewayHealthCheckPolicy
    VirtualGatewayHealthCheckPolicy (..),
    newVirtualGatewayHealthCheckPolicy,
    virtualGatewayHealthCheckPolicy_path,
    virtualGatewayHealthCheckPolicy_port,
    virtualGatewayHealthCheckPolicy_healthyThreshold,
    virtualGatewayHealthCheckPolicy_intervalMillis,
    virtualGatewayHealthCheckPolicy_protocol,
    virtualGatewayHealthCheckPolicy_timeoutMillis,
    virtualGatewayHealthCheckPolicy_unhealthyThreshold,

    -- * VirtualGatewayHttp2ConnectionPool
    VirtualGatewayHttp2ConnectionPool (..),
    newVirtualGatewayHttp2ConnectionPool,
    virtualGatewayHttp2ConnectionPool_maxRequests,

    -- * VirtualGatewayHttpConnectionPool
    VirtualGatewayHttpConnectionPool (..),
    newVirtualGatewayHttpConnectionPool,
    virtualGatewayHttpConnectionPool_maxPendingRequests,
    virtualGatewayHttpConnectionPool_maxConnections,

    -- * VirtualGatewayListener
    VirtualGatewayListener (..),
    newVirtualGatewayListener,
    virtualGatewayListener_healthCheck,
    virtualGatewayListener_connectionPool,
    virtualGatewayListener_tls,
    virtualGatewayListener_portMapping,

    -- * VirtualGatewayListenerTls
    VirtualGatewayListenerTls (..),
    newVirtualGatewayListenerTls,
    virtualGatewayListenerTls_validation,
    virtualGatewayListenerTls_certificate,
    virtualGatewayListenerTls_mode,

    -- * VirtualGatewayListenerTlsAcmCertificate
    VirtualGatewayListenerTlsAcmCertificate (..),
    newVirtualGatewayListenerTlsAcmCertificate,
    virtualGatewayListenerTlsAcmCertificate_certificateArn,

    -- * VirtualGatewayListenerTlsCertificate
    VirtualGatewayListenerTlsCertificate (..),
    newVirtualGatewayListenerTlsCertificate,
    virtualGatewayListenerTlsCertificate_acm,
    virtualGatewayListenerTlsCertificate_sds,
    virtualGatewayListenerTlsCertificate_file,

    -- * VirtualGatewayListenerTlsFileCertificate
    VirtualGatewayListenerTlsFileCertificate (..),
    newVirtualGatewayListenerTlsFileCertificate,
    virtualGatewayListenerTlsFileCertificate_certificateChain,
    virtualGatewayListenerTlsFileCertificate_privateKey,

    -- * VirtualGatewayListenerTlsSdsCertificate
    VirtualGatewayListenerTlsSdsCertificate (..),
    newVirtualGatewayListenerTlsSdsCertificate,
    virtualGatewayListenerTlsSdsCertificate_secretName,

    -- * VirtualGatewayListenerTlsValidationContext
    VirtualGatewayListenerTlsValidationContext (..),
    newVirtualGatewayListenerTlsValidationContext,
    virtualGatewayListenerTlsValidationContext_subjectAlternativeNames,
    virtualGatewayListenerTlsValidationContext_trust,

    -- * VirtualGatewayListenerTlsValidationContextTrust
    VirtualGatewayListenerTlsValidationContextTrust (..),
    newVirtualGatewayListenerTlsValidationContextTrust,
    virtualGatewayListenerTlsValidationContextTrust_sds,
    virtualGatewayListenerTlsValidationContextTrust_file,

    -- * VirtualGatewayLogging
    VirtualGatewayLogging (..),
    newVirtualGatewayLogging,
    virtualGatewayLogging_accessLog,

    -- * VirtualGatewayPortMapping
    VirtualGatewayPortMapping (..),
    newVirtualGatewayPortMapping,
    virtualGatewayPortMapping_port,
    virtualGatewayPortMapping_protocol,

    -- * VirtualGatewayRef
    VirtualGatewayRef (..),
    newVirtualGatewayRef,
    virtualGatewayRef_arn,
    virtualGatewayRef_createdAt,
    virtualGatewayRef_lastUpdatedAt,
    virtualGatewayRef_meshName,
    virtualGatewayRef_meshOwner,
    virtualGatewayRef_resourceOwner,
    virtualGatewayRef_version,
    virtualGatewayRef_virtualGatewayName,

    -- * VirtualGatewaySpec
    VirtualGatewaySpec (..),
    newVirtualGatewaySpec,
    virtualGatewaySpec_backendDefaults,
    virtualGatewaySpec_logging,
    virtualGatewaySpec_listeners,

    -- * VirtualGatewayStatus
    VirtualGatewayStatus (..),
    newVirtualGatewayStatus,
    virtualGatewayStatus_status,

    -- * VirtualGatewayTlsValidationContext
    VirtualGatewayTlsValidationContext (..),
    newVirtualGatewayTlsValidationContext,
    virtualGatewayTlsValidationContext_subjectAlternativeNames,
    virtualGatewayTlsValidationContext_trust,

    -- * VirtualGatewayTlsValidationContextAcmTrust
    VirtualGatewayTlsValidationContextAcmTrust (..),
    newVirtualGatewayTlsValidationContextAcmTrust,
    virtualGatewayTlsValidationContextAcmTrust_certificateAuthorityArns,

    -- * VirtualGatewayTlsValidationContextFileTrust
    VirtualGatewayTlsValidationContextFileTrust (..),
    newVirtualGatewayTlsValidationContextFileTrust,
    virtualGatewayTlsValidationContextFileTrust_certificateChain,

    -- * VirtualGatewayTlsValidationContextSdsTrust
    VirtualGatewayTlsValidationContextSdsTrust (..),
    newVirtualGatewayTlsValidationContextSdsTrust,
    virtualGatewayTlsValidationContextSdsTrust_secretName,

    -- * VirtualGatewayTlsValidationContextTrust
    VirtualGatewayTlsValidationContextTrust (..),
    newVirtualGatewayTlsValidationContextTrust,
    virtualGatewayTlsValidationContextTrust_acm,
    virtualGatewayTlsValidationContextTrust_sds,
    virtualGatewayTlsValidationContextTrust_file,

    -- * VirtualNodeConnectionPool
    VirtualNodeConnectionPool (..),
    newVirtualNodeConnectionPool,
    virtualNodeConnectionPool_http2,
    virtualNodeConnectionPool_grpc,
    virtualNodeConnectionPool_tcp,
    virtualNodeConnectionPool_http,

    -- * VirtualNodeData
    VirtualNodeData (..),
    newVirtualNodeData,
    virtualNodeData_meshName,
    virtualNodeData_metadata,
    virtualNodeData_spec,
    virtualNodeData_status,
    virtualNodeData_virtualNodeName,

    -- * VirtualNodeGrpcConnectionPool
    VirtualNodeGrpcConnectionPool (..),
    newVirtualNodeGrpcConnectionPool,
    virtualNodeGrpcConnectionPool_maxRequests,

    -- * VirtualNodeHttp2ConnectionPool
    VirtualNodeHttp2ConnectionPool (..),
    newVirtualNodeHttp2ConnectionPool,
    virtualNodeHttp2ConnectionPool_maxRequests,

    -- * VirtualNodeHttpConnectionPool
    VirtualNodeHttpConnectionPool (..),
    newVirtualNodeHttpConnectionPool,
    virtualNodeHttpConnectionPool_maxPendingRequests,
    virtualNodeHttpConnectionPool_maxConnections,

    -- * VirtualNodeRef
    VirtualNodeRef (..),
    newVirtualNodeRef,
    virtualNodeRef_arn,
    virtualNodeRef_createdAt,
    virtualNodeRef_lastUpdatedAt,
    virtualNodeRef_meshName,
    virtualNodeRef_meshOwner,
    virtualNodeRef_resourceOwner,
    virtualNodeRef_version,
    virtualNodeRef_virtualNodeName,

    -- * VirtualNodeServiceProvider
    VirtualNodeServiceProvider (..),
    newVirtualNodeServiceProvider,
    virtualNodeServiceProvider_virtualNodeName,

    -- * VirtualNodeSpec
    VirtualNodeSpec (..),
    newVirtualNodeSpec,
    virtualNodeSpec_backends,
    virtualNodeSpec_backendDefaults,
    virtualNodeSpec_serviceDiscovery,
    virtualNodeSpec_listeners,
    virtualNodeSpec_logging,

    -- * VirtualNodeStatus
    VirtualNodeStatus (..),
    newVirtualNodeStatus,
    virtualNodeStatus_status,

    -- * VirtualNodeTcpConnectionPool
    VirtualNodeTcpConnectionPool (..),
    newVirtualNodeTcpConnectionPool,
    virtualNodeTcpConnectionPool_maxConnections,

    -- * VirtualRouterData
    VirtualRouterData (..),
    newVirtualRouterData,
    virtualRouterData_meshName,
    virtualRouterData_metadata,
    virtualRouterData_spec,
    virtualRouterData_status,
    virtualRouterData_virtualRouterName,

    -- * VirtualRouterListener
    VirtualRouterListener (..),
    newVirtualRouterListener,
    virtualRouterListener_portMapping,

    -- * VirtualRouterRef
    VirtualRouterRef (..),
    newVirtualRouterRef,
    virtualRouterRef_arn,
    virtualRouterRef_createdAt,
    virtualRouterRef_lastUpdatedAt,
    virtualRouterRef_meshName,
    virtualRouterRef_meshOwner,
    virtualRouterRef_resourceOwner,
    virtualRouterRef_version,
    virtualRouterRef_virtualRouterName,

    -- * VirtualRouterServiceProvider
    VirtualRouterServiceProvider (..),
    newVirtualRouterServiceProvider,
    virtualRouterServiceProvider_virtualRouterName,

    -- * VirtualRouterSpec
    VirtualRouterSpec (..),
    newVirtualRouterSpec,
    virtualRouterSpec_listeners,

    -- * VirtualRouterStatus
    VirtualRouterStatus (..),
    newVirtualRouterStatus,
    virtualRouterStatus_status,

    -- * VirtualServiceBackend
    VirtualServiceBackend (..),
    newVirtualServiceBackend,
    virtualServiceBackend_clientPolicy,
    virtualServiceBackend_virtualServiceName,

    -- * VirtualServiceData
    VirtualServiceData (..),
    newVirtualServiceData,
    virtualServiceData_meshName,
    virtualServiceData_metadata,
    virtualServiceData_spec,
    virtualServiceData_status,
    virtualServiceData_virtualServiceName,

    -- * VirtualServiceProvider
    VirtualServiceProvider (..),
    newVirtualServiceProvider,
    virtualServiceProvider_virtualRouter,
    virtualServiceProvider_virtualNode,

    -- * VirtualServiceRef
    VirtualServiceRef (..),
    newVirtualServiceRef,
    virtualServiceRef_arn,
    virtualServiceRef_createdAt,
    virtualServiceRef_lastUpdatedAt,
    virtualServiceRef_meshName,
    virtualServiceRef_meshOwner,
    virtualServiceRef_resourceOwner,
    virtualServiceRef_version,
    virtualServiceRef_virtualServiceName,

    -- * VirtualServiceSpec
    VirtualServiceSpec (..),
    newVirtualServiceSpec,
    virtualServiceSpec_provider,

    -- * VirtualServiceStatus
    VirtualServiceStatus (..),
    newVirtualServiceStatus,
    virtualServiceStatus_status,

    -- * WeightedTarget
    WeightedTarget (..),
    newWeightedTarget,
    weightedTarget_virtualNode,
    weightedTarget_weight,
  )
where

import Network.AWS.AppMesh.Types.AccessLog
import Network.AWS.AppMesh.Types.AwsCloudMapInstanceAttribute
import Network.AWS.AppMesh.Types.AwsCloudMapServiceDiscovery
import Network.AWS.AppMesh.Types.Backend
import Network.AWS.AppMesh.Types.BackendDefaults
import Network.AWS.AppMesh.Types.ClientPolicy
import Network.AWS.AppMesh.Types.ClientPolicyTls
import Network.AWS.AppMesh.Types.ClientTlsCertificate
import Network.AWS.AppMesh.Types.DefaultGatewayRouteRewrite
import Network.AWS.AppMesh.Types.DnsResponseType
import Network.AWS.AppMesh.Types.DnsServiceDiscovery
import Network.AWS.AppMesh.Types.Duration
import Network.AWS.AppMesh.Types.DurationUnit
import Network.AWS.AppMesh.Types.EgressFilter
import Network.AWS.AppMesh.Types.EgressFilterType
import Network.AWS.AppMesh.Types.FileAccessLog
import Network.AWS.AppMesh.Types.GatewayRouteData
import Network.AWS.AppMesh.Types.GatewayRouteHostnameMatch
import Network.AWS.AppMesh.Types.GatewayRouteHostnameRewrite
import Network.AWS.AppMesh.Types.GatewayRouteRef
import Network.AWS.AppMesh.Types.GatewayRouteSpec
import Network.AWS.AppMesh.Types.GatewayRouteStatus
import Network.AWS.AppMesh.Types.GatewayRouteStatusCode
import Network.AWS.AppMesh.Types.GatewayRouteTarget
import Network.AWS.AppMesh.Types.GatewayRouteVirtualService
import Network.AWS.AppMesh.Types.GrpcGatewayRoute
import Network.AWS.AppMesh.Types.GrpcGatewayRouteAction
import Network.AWS.AppMesh.Types.GrpcGatewayRouteMatch
import Network.AWS.AppMesh.Types.GrpcGatewayRouteMetadata
import Network.AWS.AppMesh.Types.GrpcGatewayRouteRewrite
import Network.AWS.AppMesh.Types.GrpcMetadataMatchMethod
import Network.AWS.AppMesh.Types.GrpcRetryPolicy
import Network.AWS.AppMesh.Types.GrpcRetryPolicyEvent
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
import Network.AWS.AppMesh.Types.HttpMethod
import Network.AWS.AppMesh.Types.HttpPathMatch
import Network.AWS.AppMesh.Types.HttpQueryParameter
import Network.AWS.AppMesh.Types.HttpRetryPolicy
import Network.AWS.AppMesh.Types.HttpRoute
import Network.AWS.AppMesh.Types.HttpRouteAction
import Network.AWS.AppMesh.Types.HttpRouteHeader
import Network.AWS.AppMesh.Types.HttpRouteMatch
import Network.AWS.AppMesh.Types.HttpScheme
import Network.AWS.AppMesh.Types.HttpTimeout
import Network.AWS.AppMesh.Types.Listener
import Network.AWS.AppMesh.Types.ListenerTimeout
import Network.AWS.AppMesh.Types.ListenerTls
import Network.AWS.AppMesh.Types.ListenerTlsAcmCertificate
import Network.AWS.AppMesh.Types.ListenerTlsCertificate
import Network.AWS.AppMesh.Types.ListenerTlsFileCertificate
import Network.AWS.AppMesh.Types.ListenerTlsMode
import Network.AWS.AppMesh.Types.ListenerTlsSdsCertificate
import Network.AWS.AppMesh.Types.ListenerTlsValidationContext
import Network.AWS.AppMesh.Types.ListenerTlsValidationContextTrust
import Network.AWS.AppMesh.Types.Logging
import Network.AWS.AppMesh.Types.MatchRange
import Network.AWS.AppMesh.Types.MeshData
import Network.AWS.AppMesh.Types.MeshRef
import Network.AWS.AppMesh.Types.MeshSpec
import Network.AWS.AppMesh.Types.MeshStatus
import Network.AWS.AppMesh.Types.MeshStatusCode
import Network.AWS.AppMesh.Types.OutlierDetection
import Network.AWS.AppMesh.Types.PortMapping
import Network.AWS.AppMesh.Types.PortProtocol
import Network.AWS.AppMesh.Types.QueryParameterMatch
import Network.AWS.AppMesh.Types.ResourceMetadata
import Network.AWS.AppMesh.Types.RouteData
import Network.AWS.AppMesh.Types.RouteRef
import Network.AWS.AppMesh.Types.RouteSpec
import Network.AWS.AppMesh.Types.RouteStatus
import Network.AWS.AppMesh.Types.RouteStatusCode
import Network.AWS.AppMesh.Types.ServiceDiscovery
import Network.AWS.AppMesh.Types.SubjectAlternativeNameMatchers
import Network.AWS.AppMesh.Types.SubjectAlternativeNames
import Network.AWS.AppMesh.Types.TagRef
import Network.AWS.AppMesh.Types.TcpRetryPolicyEvent
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
import Network.AWS.AppMesh.Types.VirtualGatewayListenerTlsMode
import Network.AWS.AppMesh.Types.VirtualGatewayListenerTlsSdsCertificate
import Network.AWS.AppMesh.Types.VirtualGatewayListenerTlsValidationContext
import Network.AWS.AppMesh.Types.VirtualGatewayListenerTlsValidationContextTrust
import Network.AWS.AppMesh.Types.VirtualGatewayLogging
import Network.AWS.AppMesh.Types.VirtualGatewayPortMapping
import Network.AWS.AppMesh.Types.VirtualGatewayPortProtocol
import Network.AWS.AppMesh.Types.VirtualGatewayRef
import Network.AWS.AppMesh.Types.VirtualGatewaySpec
import Network.AWS.AppMesh.Types.VirtualGatewayStatus
import Network.AWS.AppMesh.Types.VirtualGatewayStatusCode
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
import Network.AWS.AppMesh.Types.VirtualNodeStatusCode
import Network.AWS.AppMesh.Types.VirtualNodeTcpConnectionPool
import Network.AWS.AppMesh.Types.VirtualRouterData
import Network.AWS.AppMesh.Types.VirtualRouterListener
import Network.AWS.AppMesh.Types.VirtualRouterRef
import Network.AWS.AppMesh.Types.VirtualRouterServiceProvider
import Network.AWS.AppMesh.Types.VirtualRouterSpec
import Network.AWS.AppMesh.Types.VirtualRouterStatus
import Network.AWS.AppMesh.Types.VirtualRouterStatusCode
import Network.AWS.AppMesh.Types.VirtualServiceBackend
import Network.AWS.AppMesh.Types.VirtualServiceData
import Network.AWS.AppMesh.Types.VirtualServiceProvider
import Network.AWS.AppMesh.Types.VirtualServiceRef
import Network.AWS.AppMesh.Types.VirtualServiceSpec
import Network.AWS.AppMesh.Types.VirtualServiceStatus
import Network.AWS.AppMesh.Types.VirtualServiceStatusCode
import Network.AWS.AppMesh.Types.WeightedTarget
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2019-01-25@ of the Amazon App Mesh SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "AppMesh",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "appmesh",
      Core._serviceSigningName = "appmesh",
      Core._serviceVersion = "2019-01-25",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "AppMesh",
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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
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
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The request exceeds the maximum allowed number of tags allowed per
-- resource. The current limit is 50 user tags per resource. You must
-- reduce the number of tags in the request. None of the tags in this
-- request were applied.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"
    Prelude.. Core.hasStatus 400

-- | The request contains a client token that was used for a previous update
-- resource call with different specifications. Try the request again with
-- a new client token.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | You don\'t have permissions to perform this action.
_ForbiddenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | The specified resource doesn\'t exist. Check your request syntax and try
-- again.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | The maximum request rate permitted by the App Mesh APIs has been
-- exceeded for your account. For best results, use an increasing or
-- variable sleep interval between requests.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429

-- | The request processing has failed because of an unknown error,
-- exception, or failure.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

-- | The request has failed due to a temporary failure of the service.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | The request syntax was malformed. Check your request syntax and try
-- again.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | You have exceeded a service limit for your account. For more
-- information, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/service-quotas.html Service Limits>
-- in the /AWS App Mesh User Guide/.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 400

-- | You can\'t delete the specified resource because it\'s in use or
-- required by another resource.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
    Prelude.. Core.hasStatus 409
