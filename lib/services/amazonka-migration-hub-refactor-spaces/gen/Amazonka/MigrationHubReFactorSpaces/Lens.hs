{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MigrationHubReFactorSpaces.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Lens
  ( -- * Operations

    -- ** CreateApplication
    createApplication_tags,
    createApplication_clientToken,
    createApplication_apiGatewayProxy,
    createApplication_environmentIdentifier,
    createApplication_name,
    createApplication_proxyType,
    createApplication_vpcId,
    createApplicationResponse_tags,
    createApplicationResponse_name,
    createApplicationResponse_proxyType,
    createApplicationResponse_createdTime,
    createApplicationResponse_createdByAccountId,
    createApplicationResponse_arn,
    createApplicationResponse_state,
    createApplicationResponse_lastUpdatedTime,
    createApplicationResponse_ownerAccountId,
    createApplicationResponse_vpcId,
    createApplicationResponse_environmentId,
    createApplicationResponse_applicationId,
    createApplicationResponse_apiGatewayProxy,
    createApplicationResponse_httpStatus,

    -- ** CreateEnvironment
    createEnvironment_tags,
    createEnvironment_clientToken,
    createEnvironment_description,
    createEnvironment_name,
    createEnvironment_networkFabricType,
    createEnvironmentResponse_tags,
    createEnvironmentResponse_name,
    createEnvironmentResponse_createdTime,
    createEnvironmentResponse_arn,
    createEnvironmentResponse_state,
    createEnvironmentResponse_lastUpdatedTime,
    createEnvironmentResponse_description,
    createEnvironmentResponse_ownerAccountId,
    createEnvironmentResponse_environmentId,
    createEnvironmentResponse_networkFabricType,
    createEnvironmentResponse_httpStatus,

    -- ** CreateRoute
    createRoute_tags,
    createRoute_uriPathRoute,
    createRoute_clientToken,
    createRoute_defaultRoute,
    createRoute_applicationIdentifier,
    createRoute_environmentIdentifier,
    createRoute_routeType,
    createRoute_serviceIdentifier,
    createRouteResponse_tags,
    createRouteResponse_routeType,
    createRouteResponse_uriPathRoute,
    createRouteResponse_createdTime,
    createRouteResponse_createdByAccountId,
    createRouteResponse_arn,
    createRouteResponse_state,
    createRouteResponse_lastUpdatedTime,
    createRouteResponse_ownerAccountId,
    createRouteResponse_routeId,
    createRouteResponse_applicationId,
    createRouteResponse_serviceId,
    createRouteResponse_httpStatus,

    -- ** CreateService
    createService_tags,
    createService_clientToken,
    createService_urlEndpoint,
    createService_description,
    createService_lambdaEndpoint,
    createService_vpcId,
    createService_applicationIdentifier,
    createService_endpointType,
    createService_environmentIdentifier,
    createService_name,
    createServiceResponse_tags,
    createServiceResponse_name,
    createServiceResponse_createdTime,
    createServiceResponse_createdByAccountId,
    createServiceResponse_arn,
    createServiceResponse_state,
    createServiceResponse_urlEndpoint,
    createServiceResponse_lastUpdatedTime,
    createServiceResponse_endpointType,
    createServiceResponse_description,
    createServiceResponse_ownerAccountId,
    createServiceResponse_lambdaEndpoint,
    createServiceResponse_vpcId,
    createServiceResponse_environmentId,
    createServiceResponse_applicationId,
    createServiceResponse_serviceId,
    createServiceResponse_httpStatus,

    -- ** DeleteApplication
    deleteApplication_applicationIdentifier,
    deleteApplication_environmentIdentifier,
    deleteApplicationResponse_name,
    deleteApplicationResponse_arn,
    deleteApplicationResponse_state,
    deleteApplicationResponse_lastUpdatedTime,
    deleteApplicationResponse_environmentId,
    deleteApplicationResponse_applicationId,
    deleteApplicationResponse_httpStatus,

    -- ** DeleteEnvironment
    deleteEnvironment_environmentIdentifier,
    deleteEnvironmentResponse_name,
    deleteEnvironmentResponse_arn,
    deleteEnvironmentResponse_state,
    deleteEnvironmentResponse_lastUpdatedTime,
    deleteEnvironmentResponse_environmentId,
    deleteEnvironmentResponse_httpStatus,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_identifier,
    deleteResourcePolicyResponse_httpStatus,

    -- ** DeleteRoute
    deleteRoute_applicationIdentifier,
    deleteRoute_environmentIdentifier,
    deleteRoute_routeIdentifier,
    deleteRouteResponse_arn,
    deleteRouteResponse_state,
    deleteRouteResponse_lastUpdatedTime,
    deleteRouteResponse_routeId,
    deleteRouteResponse_applicationId,
    deleteRouteResponse_serviceId,
    deleteRouteResponse_httpStatus,

    -- ** DeleteService
    deleteService_applicationIdentifier,
    deleteService_environmentIdentifier,
    deleteService_serviceIdentifier,
    deleteServiceResponse_name,
    deleteServiceResponse_arn,
    deleteServiceResponse_state,
    deleteServiceResponse_lastUpdatedTime,
    deleteServiceResponse_environmentId,
    deleteServiceResponse_applicationId,
    deleteServiceResponse_serviceId,
    deleteServiceResponse_httpStatus,

    -- ** GetApplication
    getApplication_applicationIdentifier,
    getApplication_environmentIdentifier,
    getApplicationResponse_tags,
    getApplicationResponse_name,
    getApplicationResponse_proxyType,
    getApplicationResponse_createdTime,
    getApplicationResponse_createdByAccountId,
    getApplicationResponse_arn,
    getApplicationResponse_state,
    getApplicationResponse_lastUpdatedTime,
    getApplicationResponse_ownerAccountId,
    getApplicationResponse_vpcId,
    getApplicationResponse_environmentId,
    getApplicationResponse_error,
    getApplicationResponse_applicationId,
    getApplicationResponse_apiGatewayProxy,
    getApplicationResponse_httpStatus,

    -- ** GetEnvironment
    getEnvironment_environmentIdentifier,
    getEnvironmentResponse_tags,
    getEnvironmentResponse_name,
    getEnvironmentResponse_createdTime,
    getEnvironmentResponse_transitGatewayId,
    getEnvironmentResponse_arn,
    getEnvironmentResponse_state,
    getEnvironmentResponse_lastUpdatedTime,
    getEnvironmentResponse_description,
    getEnvironmentResponse_ownerAccountId,
    getEnvironmentResponse_environmentId,
    getEnvironmentResponse_error,
    getEnvironmentResponse_networkFabricType,
    getEnvironmentResponse_httpStatus,

    -- ** GetResourcePolicy
    getResourcePolicy_identifier,
    getResourcePolicyResponse_policy,
    getResourcePolicyResponse_httpStatus,

    -- ** GetRoute
    getRoute_applicationIdentifier,
    getRoute_environmentIdentifier,
    getRoute_routeIdentifier,
    getRouteResponse_tags,
    getRouteResponse_routeType,
    getRouteResponse_createdTime,
    getRouteResponse_createdByAccountId,
    getRouteResponse_arn,
    getRouteResponse_state,
    getRouteResponse_pathResourceToId,
    getRouteResponse_lastUpdatedTime,
    getRouteResponse_ownerAccountId,
    getRouteResponse_routeId,
    getRouteResponse_methods,
    getRouteResponse_environmentId,
    getRouteResponse_error,
    getRouteResponse_applicationId,
    getRouteResponse_sourcePath,
    getRouteResponse_includeChildPaths,
    getRouteResponse_serviceId,
    getRouteResponse_httpStatus,

    -- ** GetService
    getService_applicationIdentifier,
    getService_environmentIdentifier,
    getService_serviceIdentifier,
    getServiceResponse_tags,
    getServiceResponse_name,
    getServiceResponse_createdTime,
    getServiceResponse_createdByAccountId,
    getServiceResponse_arn,
    getServiceResponse_state,
    getServiceResponse_urlEndpoint,
    getServiceResponse_lastUpdatedTime,
    getServiceResponse_endpointType,
    getServiceResponse_description,
    getServiceResponse_ownerAccountId,
    getServiceResponse_lambdaEndpoint,
    getServiceResponse_vpcId,
    getServiceResponse_environmentId,
    getServiceResponse_error,
    getServiceResponse_applicationId,
    getServiceResponse_serviceId,
    getServiceResponse_httpStatus,

    -- ** ListApplications
    listApplications_nextToken,
    listApplications_maxResults,
    listApplications_environmentIdentifier,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_applicationSummaryList,
    listApplicationsResponse_httpStatus,

    -- ** ListEnvironmentVpcs
    listEnvironmentVpcs_nextToken,
    listEnvironmentVpcs_maxResults,
    listEnvironmentVpcs_environmentIdentifier,
    listEnvironmentVpcsResponse_nextToken,
    listEnvironmentVpcsResponse_environmentVpcList,
    listEnvironmentVpcsResponse_httpStatus,

    -- ** ListEnvironments
    listEnvironments_nextToken,
    listEnvironments_maxResults,
    listEnvironmentsResponse_environmentSummaryList,
    listEnvironmentsResponse_nextToken,
    listEnvironmentsResponse_httpStatus,

    -- ** ListRoutes
    listRoutes_nextToken,
    listRoutes_maxResults,
    listRoutes_applicationIdentifier,
    listRoutes_environmentIdentifier,
    listRoutesResponse_nextToken,
    listRoutesResponse_routeSummaryList,
    listRoutesResponse_httpStatus,

    -- ** ListServices
    listServices_nextToken,
    listServices_maxResults,
    listServices_applicationIdentifier,
    listServices_environmentIdentifier,
    listServicesResponse_nextToken,
    listServicesResponse_serviceSummaryList,
    listServicesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutResourcePolicy
    putResourcePolicy_policy,
    putResourcePolicy_resourceArn,
    putResourcePolicyResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateRoute
    updateRoute_activationState,
    updateRoute_applicationIdentifier,
    updateRoute_environmentIdentifier,
    updateRoute_routeIdentifier,
    updateRouteResponse_arn,
    updateRouteResponse_state,
    updateRouteResponse_lastUpdatedTime,
    updateRouteResponse_routeId,
    updateRouteResponse_applicationId,
    updateRouteResponse_serviceId,
    updateRouteResponse_httpStatus,

    -- * Types

    -- ** ApiGatewayProxyConfig
    apiGatewayProxyConfig_stageName,
    apiGatewayProxyConfig_proxyUrl,
    apiGatewayProxyConfig_endpointType,
    apiGatewayProxyConfig_apiGatewayId,
    apiGatewayProxyConfig_nlbName,
    apiGatewayProxyConfig_vpcLinkId,
    apiGatewayProxyConfig_nlbArn,

    -- ** ApiGatewayProxyInput
    apiGatewayProxyInput_stageName,
    apiGatewayProxyInput_endpointType,

    -- ** ApiGatewayProxySummary
    apiGatewayProxySummary_stageName,
    apiGatewayProxySummary_proxyUrl,
    apiGatewayProxySummary_endpointType,
    apiGatewayProxySummary_apiGatewayId,
    apiGatewayProxySummary_nlbName,
    apiGatewayProxySummary_vpcLinkId,
    apiGatewayProxySummary_nlbArn,

    -- ** ApplicationSummary
    applicationSummary_tags,
    applicationSummary_name,
    applicationSummary_proxyType,
    applicationSummary_createdTime,
    applicationSummary_createdByAccountId,
    applicationSummary_arn,
    applicationSummary_state,
    applicationSummary_lastUpdatedTime,
    applicationSummary_ownerAccountId,
    applicationSummary_vpcId,
    applicationSummary_environmentId,
    applicationSummary_error,
    applicationSummary_applicationId,
    applicationSummary_apiGatewayProxy,

    -- ** DefaultRouteInput
    defaultRouteInput_activationState,

    -- ** EnvironmentSummary
    environmentSummary_tags,
    environmentSummary_name,
    environmentSummary_createdTime,
    environmentSummary_transitGatewayId,
    environmentSummary_arn,
    environmentSummary_state,
    environmentSummary_lastUpdatedTime,
    environmentSummary_description,
    environmentSummary_ownerAccountId,
    environmentSummary_environmentId,
    environmentSummary_error,
    environmentSummary_networkFabricType,

    -- ** EnvironmentVpc
    environmentVpc_createdTime,
    environmentVpc_vpcName,
    environmentVpc_lastUpdatedTime,
    environmentVpc_accountId,
    environmentVpc_vpcId,
    environmentVpc_environmentId,
    environmentVpc_cidrBlocks,

    -- ** ErrorResponse
    errorResponse_resourceType,
    errorResponse_message,
    errorResponse_additionalDetails,
    errorResponse_code,
    errorResponse_accountId,
    errorResponse_resourceIdentifier,

    -- ** LambdaEndpointConfig
    lambdaEndpointConfig_arn,

    -- ** LambdaEndpointInput
    lambdaEndpointInput_arn,

    -- ** LambdaEndpointSummary
    lambdaEndpointSummary_arn,

    -- ** RouteSummary
    routeSummary_tags,
    routeSummary_routeType,
    routeSummary_createdTime,
    routeSummary_createdByAccountId,
    routeSummary_arn,
    routeSummary_state,
    routeSummary_pathResourceToId,
    routeSummary_lastUpdatedTime,
    routeSummary_ownerAccountId,
    routeSummary_routeId,
    routeSummary_methods,
    routeSummary_environmentId,
    routeSummary_error,
    routeSummary_applicationId,
    routeSummary_sourcePath,
    routeSummary_includeChildPaths,
    routeSummary_serviceId,

    -- ** ServiceSummary
    serviceSummary_tags,
    serviceSummary_name,
    serviceSummary_createdTime,
    serviceSummary_createdByAccountId,
    serviceSummary_arn,
    serviceSummary_state,
    serviceSummary_urlEndpoint,
    serviceSummary_lastUpdatedTime,
    serviceSummary_endpointType,
    serviceSummary_description,
    serviceSummary_ownerAccountId,
    serviceSummary_lambdaEndpoint,
    serviceSummary_vpcId,
    serviceSummary_environmentId,
    serviceSummary_error,
    serviceSummary_applicationId,
    serviceSummary_serviceId,

    -- ** UriPathRouteInput
    uriPathRouteInput_methods,
    uriPathRouteInput_includeChildPaths,
    uriPathRouteInput_activationState,
    uriPathRouteInput_sourcePath,

    -- ** UrlEndpointConfig
    urlEndpointConfig_url,
    urlEndpointConfig_healthUrl,

    -- ** UrlEndpointInput
    urlEndpointInput_healthUrl,
    urlEndpointInput_url,

    -- ** UrlEndpointSummary
    urlEndpointSummary_url,
    urlEndpointSummary_healthUrl,
  )
where

import Amazonka.MigrationHubReFactorSpaces.CreateApplication
import Amazonka.MigrationHubReFactorSpaces.CreateEnvironment
import Amazonka.MigrationHubReFactorSpaces.CreateRoute
import Amazonka.MigrationHubReFactorSpaces.CreateService
import Amazonka.MigrationHubReFactorSpaces.DeleteApplication
import Amazonka.MigrationHubReFactorSpaces.DeleteEnvironment
import Amazonka.MigrationHubReFactorSpaces.DeleteResourcePolicy
import Amazonka.MigrationHubReFactorSpaces.DeleteRoute
import Amazonka.MigrationHubReFactorSpaces.DeleteService
import Amazonka.MigrationHubReFactorSpaces.GetApplication
import Amazonka.MigrationHubReFactorSpaces.GetEnvironment
import Amazonka.MigrationHubReFactorSpaces.GetResourcePolicy
import Amazonka.MigrationHubReFactorSpaces.GetRoute
import Amazonka.MigrationHubReFactorSpaces.GetService
import Amazonka.MigrationHubReFactorSpaces.ListApplications
import Amazonka.MigrationHubReFactorSpaces.ListEnvironmentVpcs
import Amazonka.MigrationHubReFactorSpaces.ListEnvironments
import Amazonka.MigrationHubReFactorSpaces.ListRoutes
import Amazonka.MigrationHubReFactorSpaces.ListServices
import Amazonka.MigrationHubReFactorSpaces.ListTagsForResource
import Amazonka.MigrationHubReFactorSpaces.PutResourcePolicy
import Amazonka.MigrationHubReFactorSpaces.TagResource
import Amazonka.MigrationHubReFactorSpaces.Types.ApiGatewayProxyConfig
import Amazonka.MigrationHubReFactorSpaces.Types.ApiGatewayProxyInput
import Amazonka.MigrationHubReFactorSpaces.Types.ApiGatewayProxySummary
import Amazonka.MigrationHubReFactorSpaces.Types.ApplicationSummary
import Amazonka.MigrationHubReFactorSpaces.Types.DefaultRouteInput
import Amazonka.MigrationHubReFactorSpaces.Types.EnvironmentSummary
import Amazonka.MigrationHubReFactorSpaces.Types.EnvironmentVpc
import Amazonka.MigrationHubReFactorSpaces.Types.ErrorResponse
import Amazonka.MigrationHubReFactorSpaces.Types.LambdaEndpointConfig
import Amazonka.MigrationHubReFactorSpaces.Types.LambdaEndpointInput
import Amazonka.MigrationHubReFactorSpaces.Types.LambdaEndpointSummary
import Amazonka.MigrationHubReFactorSpaces.Types.RouteSummary
import Amazonka.MigrationHubReFactorSpaces.Types.ServiceSummary
import Amazonka.MigrationHubReFactorSpaces.Types.UriPathRouteInput
import Amazonka.MigrationHubReFactorSpaces.Types.UrlEndpointConfig
import Amazonka.MigrationHubReFactorSpaces.Types.UrlEndpointInput
import Amazonka.MigrationHubReFactorSpaces.Types.UrlEndpointSummary
import Amazonka.MigrationHubReFactorSpaces.UntagResource
import Amazonka.MigrationHubReFactorSpaces.UpdateRoute
