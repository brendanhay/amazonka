{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppMesh.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _TooManyTagsException,
    _NotFoundException,
    _ServiceUnavailableException,
    _InternalServerErrorException,
    _ResourceInUseException,
    _LimitExceededException,
    _ForbiddenException,
    _ConflictException,
    _BadRequestException,
    _TooManyRequestsException,

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

    -- * IpPreference
    IpPreference (..),

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
    awsCloudMapServiceDiscovery_ipPreference,
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
    clientPolicyTls_enforce,
    clientPolicyTls_certificate,
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
    dnsServiceDiscovery_ipPreference,
    dnsServiceDiscovery_hostname,

    -- * Duration
    Duration (..),
    newDuration,
    duration_unit,
    duration_value,

    -- * EgressFilter
    EgressFilter (..),
    newEgressFilter,
    egressFilter_type,

    -- * FileAccessLog
    FileAccessLog (..),
    newFileAccessLog,
    fileAccessLog_format,
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
    gatewayRouteHostnameMatch_exact,
    gatewayRouteHostnameMatch_suffix,

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
    gatewayRouteSpec_httpRoute,
    gatewayRouteSpec_http2Route,
    gatewayRouteSpec_grpcRoute,
    gatewayRouteSpec_priority,

    -- * GatewayRouteStatus
    GatewayRouteStatus (..),
    newGatewayRouteStatus,
    gatewayRouteStatus_status,

    -- * GatewayRouteTarget
    GatewayRouteTarget (..),
    newGatewayRouteTarget,
    gatewayRouteTarget_port,
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
    grpcGatewayRouteMatch_port,
    grpcGatewayRouteMatch_metadata,
    grpcGatewayRouteMatch_hostname,
    grpcGatewayRouteMatch_serviceName,

    -- * GrpcGatewayRouteMetadata
    GrpcGatewayRouteMetadata (..),
    newGrpcGatewayRouteMetadata,
    grpcGatewayRouteMetadata_match,
    grpcGatewayRouteMetadata_invert,
    grpcGatewayRouteMetadata_name,

    -- * GrpcGatewayRouteRewrite
    GrpcGatewayRouteRewrite (..),
    newGrpcGatewayRouteRewrite,
    grpcGatewayRouteRewrite_hostname,

    -- * GrpcMetadataMatchMethod
    GrpcMetadataMatchMethod (..),
    newGrpcMetadataMatchMethod,
    grpcMetadataMatchMethod_exact,
    grpcMetadataMatchMethod_regex,
    grpcMetadataMatchMethod_range,
    grpcMetadataMatchMethod_prefix,
    grpcMetadataMatchMethod_suffix,

    -- * GrpcRetryPolicy
    GrpcRetryPolicy (..),
    newGrpcRetryPolicy,
    grpcRetryPolicy_grpcRetryEvents,
    grpcRetryPolicy_httpRetryEvents,
    grpcRetryPolicy_tcpRetryEvents,
    grpcRetryPolicy_maxRetries,
    grpcRetryPolicy_perRetryTimeout,

    -- * GrpcRoute
    GrpcRoute (..),
    newGrpcRoute,
    grpcRoute_timeout,
    grpcRoute_retryPolicy,
    grpcRoute_action,
    grpcRoute_match,

    -- * GrpcRouteAction
    GrpcRouteAction (..),
    newGrpcRouteAction,
    grpcRouteAction_weightedTargets,

    -- * GrpcRouteMatch
    GrpcRouteMatch (..),
    newGrpcRouteMatch,
    grpcRouteMatch_port,
    grpcRouteMatch_methodName,
    grpcRouteMatch_metadata,
    grpcRouteMatch_serviceName,

    -- * GrpcRouteMetadata
    GrpcRouteMetadata (..),
    newGrpcRouteMetadata,
    grpcRouteMetadata_match,
    grpcRouteMetadata_invert,
    grpcRouteMetadata_name,

    -- * GrpcRouteMetadataMatchMethod
    GrpcRouteMetadataMatchMethod (..),
    newGrpcRouteMetadataMatchMethod,
    grpcRouteMetadataMatchMethod_exact,
    grpcRouteMetadataMatchMethod_regex,
    grpcRouteMetadataMatchMethod_range,
    grpcRouteMetadataMatchMethod_prefix,
    grpcRouteMetadataMatchMethod_suffix,

    -- * GrpcTimeout
    GrpcTimeout (..),
    newGrpcTimeout,
    grpcTimeout_perRequest,
    grpcTimeout_idle,

    -- * HeaderMatchMethod
    HeaderMatchMethod (..),
    newHeaderMatchMethod,
    headerMatchMethod_exact,
    headerMatchMethod_regex,
    headerMatchMethod_range,
    headerMatchMethod_prefix,
    headerMatchMethod_suffix,

    -- * HealthCheckPolicy
    HealthCheckPolicy (..),
    newHealthCheckPolicy,
    healthCheckPolicy_port,
    healthCheckPolicy_path,
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
    httpGatewayRouteHeader_match,
    httpGatewayRouteHeader_invert,
    httpGatewayRouteHeader_name,

    -- * HttpGatewayRouteMatch
    HttpGatewayRouteMatch (..),
    newHttpGatewayRouteMatch,
    httpGatewayRouteMatch_port,
    httpGatewayRouteMatch_headers,
    httpGatewayRouteMatch_method,
    httpGatewayRouteMatch_path,
    httpGatewayRouteMatch_hostname,
    httpGatewayRouteMatch_queryParameters,
    httpGatewayRouteMatch_prefix,

    -- * HttpGatewayRoutePathRewrite
    HttpGatewayRoutePathRewrite (..),
    newHttpGatewayRoutePathRewrite,
    httpGatewayRoutePathRewrite_exact,

    -- * HttpGatewayRoutePrefixRewrite
    HttpGatewayRoutePrefixRewrite (..),
    newHttpGatewayRoutePrefixRewrite,
    httpGatewayRoutePrefixRewrite_defaultPrefix,
    httpGatewayRoutePrefixRewrite_value,

    -- * HttpGatewayRouteRewrite
    HttpGatewayRouteRewrite (..),
    newHttpGatewayRouteRewrite,
    httpGatewayRouteRewrite_path,
    httpGatewayRouteRewrite_hostname,
    httpGatewayRouteRewrite_prefix,

    -- * HttpPathMatch
    HttpPathMatch (..),
    newHttpPathMatch,
    httpPathMatch_exact,
    httpPathMatch_regex,

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
    httpRoute_timeout,
    httpRoute_retryPolicy,
    httpRoute_action,
    httpRoute_match,

    -- * HttpRouteAction
    HttpRouteAction (..),
    newHttpRouteAction,
    httpRouteAction_weightedTargets,

    -- * HttpRouteHeader
    HttpRouteHeader (..),
    newHttpRouteHeader,
    httpRouteHeader_match,
    httpRouteHeader_invert,
    httpRouteHeader_name,

    -- * HttpRouteMatch
    HttpRouteMatch (..),
    newHttpRouteMatch,
    httpRouteMatch_port,
    httpRouteMatch_scheme,
    httpRouteMatch_headers,
    httpRouteMatch_method,
    httpRouteMatch_path,
    httpRouteMatch_queryParameters,
    httpRouteMatch_prefix,

    -- * HttpTimeout
    HttpTimeout (..),
    newHttpTimeout,
    httpTimeout_perRequest,
    httpTimeout_idle,

    -- * JsonFormatRef
    JsonFormatRef (..),
    newJsonFormatRef,
    jsonFormatRef_key,
    jsonFormatRef_value,

    -- * Listener
    Listener (..),
    newListener,
    listener_timeout,
    listener_healthCheck,
    listener_tls,
    listener_connectionPool,
    listener_outlierDetection,
    listener_portMapping,

    -- * ListenerTimeout
    ListenerTimeout (..),
    newListenerTimeout,
    listenerTimeout_http,
    listenerTimeout_http2,
    listenerTimeout_tcp,
    listenerTimeout_grpc,

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
    listenerTlsCertificate_sds,
    listenerTlsCertificate_file,
    listenerTlsCertificate_acm,

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

    -- * LoggingFormat
    LoggingFormat (..),
    newLoggingFormat,
    loggingFormat_json,
    loggingFormat_text,

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

    -- * MeshServiceDiscovery
    MeshServiceDiscovery (..),
    newMeshServiceDiscovery,
    meshServiceDiscovery_ipPreference,

    -- * MeshSpec
    MeshSpec (..),
    newMeshSpec,
    meshSpec_egressFilter,
    meshSpec_serviceDiscovery,

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
    routeSpec_httpRoute,
    routeSpec_tcpRoute,
    routeSpec_http2Route,
    routeSpec_grpcRoute,
    routeSpec_priority,

    -- * RouteStatus
    RouteStatus (..),
    newRouteStatus,
    routeStatus_status,

    -- * ServiceDiscovery
    ServiceDiscovery (..),
    newServiceDiscovery,
    serviceDiscovery_dns,
    serviceDiscovery_awsCloudMap,

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
    tcpRoute_match,
    tcpRoute_action,

    -- * TcpRouteAction
    TcpRouteAction (..),
    newTcpRouteAction,
    tcpRouteAction_weightedTargets,

    -- * TcpRouteMatch
    TcpRouteMatch (..),
    newTcpRouteMatch,
    tcpRouteMatch_port,

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
    tlsValidationContextTrust_sds,
    tlsValidationContextTrust_file,
    tlsValidationContextTrust_acm,

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
    virtualGatewayClientPolicyTls_enforce,
    virtualGatewayClientPolicyTls_certificate,
    virtualGatewayClientPolicyTls_validation,

    -- * VirtualGatewayClientTlsCertificate
    VirtualGatewayClientTlsCertificate (..),
    newVirtualGatewayClientTlsCertificate,
    virtualGatewayClientTlsCertificate_sds,
    virtualGatewayClientTlsCertificate_file,

    -- * VirtualGatewayConnectionPool
    VirtualGatewayConnectionPool (..),
    newVirtualGatewayConnectionPool,
    virtualGatewayConnectionPool_http,
    virtualGatewayConnectionPool_http2,
    virtualGatewayConnectionPool_grpc,

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
    virtualGatewayFileAccessLog_format,
    virtualGatewayFileAccessLog_path,

    -- * VirtualGatewayGrpcConnectionPool
    VirtualGatewayGrpcConnectionPool (..),
    newVirtualGatewayGrpcConnectionPool,
    virtualGatewayGrpcConnectionPool_maxRequests,

    -- * VirtualGatewayHealthCheckPolicy
    VirtualGatewayHealthCheckPolicy (..),
    newVirtualGatewayHealthCheckPolicy,
    virtualGatewayHealthCheckPolicy_port,
    virtualGatewayHealthCheckPolicy_path,
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
    virtualGatewayListener_tls,
    virtualGatewayListener_connectionPool,
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
    virtualGatewayListenerTlsCertificate_sds,
    virtualGatewayListenerTlsCertificate_file,
    virtualGatewayListenerTlsCertificate_acm,

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
    virtualGatewaySpec_logging,
    virtualGatewaySpec_backendDefaults,
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
    virtualGatewayTlsValidationContextTrust_sds,
    virtualGatewayTlsValidationContextTrust_file,
    virtualGatewayTlsValidationContextTrust_acm,

    -- * VirtualNodeConnectionPool
    VirtualNodeConnectionPool (..),
    newVirtualNodeConnectionPool,
    virtualNodeConnectionPool_http,
    virtualNodeConnectionPool_http2,
    virtualNodeConnectionPool_tcp,
    virtualNodeConnectionPool_grpc,

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
    virtualNodeSpec_listeners,
    virtualNodeSpec_backends,
    virtualNodeSpec_logging,
    virtualNodeSpec_backendDefaults,
    virtualNodeSpec_serviceDiscovery,

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
    weightedTarget_port,
    weightedTarget_virtualNode,
    weightedTarget_weight,
  )
where

import Amazonka.AppMesh.Types.AccessLog
import Amazonka.AppMesh.Types.AwsCloudMapInstanceAttribute
import Amazonka.AppMesh.Types.AwsCloudMapServiceDiscovery
import Amazonka.AppMesh.Types.Backend
import Amazonka.AppMesh.Types.BackendDefaults
import Amazonka.AppMesh.Types.ClientPolicy
import Amazonka.AppMesh.Types.ClientPolicyTls
import Amazonka.AppMesh.Types.ClientTlsCertificate
import Amazonka.AppMesh.Types.DefaultGatewayRouteRewrite
import Amazonka.AppMesh.Types.DnsResponseType
import Amazonka.AppMesh.Types.DnsServiceDiscovery
import Amazonka.AppMesh.Types.Duration
import Amazonka.AppMesh.Types.DurationUnit
import Amazonka.AppMesh.Types.EgressFilter
import Amazonka.AppMesh.Types.EgressFilterType
import Amazonka.AppMesh.Types.FileAccessLog
import Amazonka.AppMesh.Types.GatewayRouteData
import Amazonka.AppMesh.Types.GatewayRouteHostnameMatch
import Amazonka.AppMesh.Types.GatewayRouteHostnameRewrite
import Amazonka.AppMesh.Types.GatewayRouteRef
import Amazonka.AppMesh.Types.GatewayRouteSpec
import Amazonka.AppMesh.Types.GatewayRouteStatus
import Amazonka.AppMesh.Types.GatewayRouteStatusCode
import Amazonka.AppMesh.Types.GatewayRouteTarget
import Amazonka.AppMesh.Types.GatewayRouteVirtualService
import Amazonka.AppMesh.Types.GrpcGatewayRoute
import Amazonka.AppMesh.Types.GrpcGatewayRouteAction
import Amazonka.AppMesh.Types.GrpcGatewayRouteMatch
import Amazonka.AppMesh.Types.GrpcGatewayRouteMetadata
import Amazonka.AppMesh.Types.GrpcGatewayRouteRewrite
import Amazonka.AppMesh.Types.GrpcMetadataMatchMethod
import Amazonka.AppMesh.Types.GrpcRetryPolicy
import Amazonka.AppMesh.Types.GrpcRetryPolicyEvent
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
import Amazonka.AppMesh.Types.HttpMethod
import Amazonka.AppMesh.Types.HttpPathMatch
import Amazonka.AppMesh.Types.HttpQueryParameter
import Amazonka.AppMesh.Types.HttpRetryPolicy
import Amazonka.AppMesh.Types.HttpRoute
import Amazonka.AppMesh.Types.HttpRouteAction
import Amazonka.AppMesh.Types.HttpRouteHeader
import Amazonka.AppMesh.Types.HttpRouteMatch
import Amazonka.AppMesh.Types.HttpScheme
import Amazonka.AppMesh.Types.HttpTimeout
import Amazonka.AppMesh.Types.IpPreference
import Amazonka.AppMesh.Types.JsonFormatRef
import Amazonka.AppMesh.Types.Listener
import Amazonka.AppMesh.Types.ListenerTimeout
import Amazonka.AppMesh.Types.ListenerTls
import Amazonka.AppMesh.Types.ListenerTlsAcmCertificate
import Amazonka.AppMesh.Types.ListenerTlsCertificate
import Amazonka.AppMesh.Types.ListenerTlsFileCertificate
import Amazonka.AppMesh.Types.ListenerTlsMode
import Amazonka.AppMesh.Types.ListenerTlsSdsCertificate
import Amazonka.AppMesh.Types.ListenerTlsValidationContext
import Amazonka.AppMesh.Types.ListenerTlsValidationContextTrust
import Amazonka.AppMesh.Types.Logging
import Amazonka.AppMesh.Types.LoggingFormat
import Amazonka.AppMesh.Types.MatchRange
import Amazonka.AppMesh.Types.MeshData
import Amazonka.AppMesh.Types.MeshRef
import Amazonka.AppMesh.Types.MeshServiceDiscovery
import Amazonka.AppMesh.Types.MeshSpec
import Amazonka.AppMesh.Types.MeshStatus
import Amazonka.AppMesh.Types.MeshStatusCode
import Amazonka.AppMesh.Types.OutlierDetection
import Amazonka.AppMesh.Types.PortMapping
import Amazonka.AppMesh.Types.PortProtocol
import Amazonka.AppMesh.Types.QueryParameterMatch
import Amazonka.AppMesh.Types.ResourceMetadata
import Amazonka.AppMesh.Types.RouteData
import Amazonka.AppMesh.Types.RouteRef
import Amazonka.AppMesh.Types.RouteSpec
import Amazonka.AppMesh.Types.RouteStatus
import Amazonka.AppMesh.Types.RouteStatusCode
import Amazonka.AppMesh.Types.ServiceDiscovery
import Amazonka.AppMesh.Types.SubjectAlternativeNameMatchers
import Amazonka.AppMesh.Types.SubjectAlternativeNames
import Amazonka.AppMesh.Types.TagRef
import Amazonka.AppMesh.Types.TcpRetryPolicyEvent
import Amazonka.AppMesh.Types.TcpRoute
import Amazonka.AppMesh.Types.TcpRouteAction
import Amazonka.AppMesh.Types.TcpRouteMatch
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
import Amazonka.AppMesh.Types.VirtualGatewayListenerTlsMode
import Amazonka.AppMesh.Types.VirtualGatewayListenerTlsSdsCertificate
import Amazonka.AppMesh.Types.VirtualGatewayListenerTlsValidationContext
import Amazonka.AppMesh.Types.VirtualGatewayListenerTlsValidationContextTrust
import Amazonka.AppMesh.Types.VirtualGatewayLogging
import Amazonka.AppMesh.Types.VirtualGatewayPortMapping
import Amazonka.AppMesh.Types.VirtualGatewayPortProtocol
import Amazonka.AppMesh.Types.VirtualGatewayRef
import Amazonka.AppMesh.Types.VirtualGatewaySpec
import Amazonka.AppMesh.Types.VirtualGatewayStatus
import Amazonka.AppMesh.Types.VirtualGatewayStatusCode
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
import Amazonka.AppMesh.Types.VirtualNodeStatusCode
import Amazonka.AppMesh.Types.VirtualNodeTcpConnectionPool
import Amazonka.AppMesh.Types.VirtualRouterData
import Amazonka.AppMesh.Types.VirtualRouterListener
import Amazonka.AppMesh.Types.VirtualRouterRef
import Amazonka.AppMesh.Types.VirtualRouterServiceProvider
import Amazonka.AppMesh.Types.VirtualRouterSpec
import Amazonka.AppMesh.Types.VirtualRouterStatus
import Amazonka.AppMesh.Types.VirtualRouterStatusCode
import Amazonka.AppMesh.Types.VirtualServiceBackend
import Amazonka.AppMesh.Types.VirtualServiceData
import Amazonka.AppMesh.Types.VirtualServiceProvider
import Amazonka.AppMesh.Types.VirtualServiceRef
import Amazonka.AppMesh.Types.VirtualServiceSpec
import Amazonka.AppMesh.Types.VirtualServiceStatus
import Amazonka.AppMesh.Types.VirtualServiceStatusCode
import Amazonka.AppMesh.Types.WeightedTarget
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-01-25@ of the Amazon App Mesh SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "AppMesh",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "appmesh",
      Core.signingName = "appmesh",
      Core.version = "2019-01-25",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "AppMesh",
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
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

-- | The specified resource doesn\'t exist. Check your request syntax and try
-- again.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request has failed due to a temporary failure of the service.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | The request processing has failed because of an unknown error,
-- exception, or failure.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

-- | You can\'t delete the specified resource because it\'s in use or
-- required by another resource.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
    Prelude.. Core.hasStatus 409

-- | You have exceeded a service limit for your account. For more
-- information, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/service-quotas.html Service Limits>
-- in the /App Mesh User Guide/.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 400

-- | You don\'t have permissions to perform this action.
_ForbiddenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | The request contains a client token that was used for a previous update
-- resource call with different specifications. Try the request again with
-- a new client token.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The request syntax was malformed. Check your request syntax and try
-- again.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | The maximum request rate permitted by the App Mesh APIs has been
-- exceeded for your account. For best results, use an increasing or
-- variable sleep interval between requests.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429
