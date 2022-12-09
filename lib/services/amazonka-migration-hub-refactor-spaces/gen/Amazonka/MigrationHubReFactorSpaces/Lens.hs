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
    createApplication_apiGatewayProxy,
    createApplication_clientToken,
    createApplication_tags,
    createApplication_environmentIdentifier,
    createApplication_name,
    createApplication_proxyType,
    createApplication_vpcId,
    createApplicationResponse_apiGatewayProxy,
    createApplicationResponse_applicationId,
    createApplicationResponse_arn,
    createApplicationResponse_createdByAccountId,
    createApplicationResponse_createdTime,
    createApplicationResponse_environmentId,
    createApplicationResponse_lastUpdatedTime,
    createApplicationResponse_name,
    createApplicationResponse_ownerAccountId,
    createApplicationResponse_proxyType,
    createApplicationResponse_state,
    createApplicationResponse_tags,
    createApplicationResponse_vpcId,
    createApplicationResponse_httpStatus,

    -- ** CreateEnvironment
    createEnvironment_clientToken,
    createEnvironment_description,
    createEnvironment_tags,
    createEnvironment_name,
    createEnvironment_networkFabricType,
    createEnvironmentResponse_arn,
    createEnvironmentResponse_createdTime,
    createEnvironmentResponse_description,
    createEnvironmentResponse_environmentId,
    createEnvironmentResponse_lastUpdatedTime,
    createEnvironmentResponse_name,
    createEnvironmentResponse_networkFabricType,
    createEnvironmentResponse_ownerAccountId,
    createEnvironmentResponse_state,
    createEnvironmentResponse_tags,
    createEnvironmentResponse_httpStatus,

    -- ** CreateRoute
    createRoute_clientToken,
    createRoute_defaultRoute,
    createRoute_tags,
    createRoute_uriPathRoute,
    createRoute_applicationIdentifier,
    createRoute_environmentIdentifier,
    createRoute_routeType,
    createRoute_serviceIdentifier,
    createRouteResponse_applicationId,
    createRouteResponse_arn,
    createRouteResponse_createdByAccountId,
    createRouteResponse_createdTime,
    createRouteResponse_lastUpdatedTime,
    createRouteResponse_ownerAccountId,
    createRouteResponse_routeId,
    createRouteResponse_routeType,
    createRouteResponse_serviceId,
    createRouteResponse_state,
    createRouteResponse_tags,
    createRouteResponse_uriPathRoute,
    createRouteResponse_httpStatus,

    -- ** CreateService
    createService_clientToken,
    createService_description,
    createService_lambdaEndpoint,
    createService_tags,
    createService_urlEndpoint,
    createService_vpcId,
    createService_applicationIdentifier,
    createService_endpointType,
    createService_environmentIdentifier,
    createService_name,
    createServiceResponse_applicationId,
    createServiceResponse_arn,
    createServiceResponse_createdByAccountId,
    createServiceResponse_createdTime,
    createServiceResponse_description,
    createServiceResponse_endpointType,
    createServiceResponse_environmentId,
    createServiceResponse_lambdaEndpoint,
    createServiceResponse_lastUpdatedTime,
    createServiceResponse_name,
    createServiceResponse_ownerAccountId,
    createServiceResponse_serviceId,
    createServiceResponse_state,
    createServiceResponse_tags,
    createServiceResponse_urlEndpoint,
    createServiceResponse_vpcId,
    createServiceResponse_httpStatus,

    -- ** DeleteApplication
    deleteApplication_applicationIdentifier,
    deleteApplication_environmentIdentifier,
    deleteApplicationResponse_applicationId,
    deleteApplicationResponse_arn,
    deleteApplicationResponse_environmentId,
    deleteApplicationResponse_lastUpdatedTime,
    deleteApplicationResponse_name,
    deleteApplicationResponse_state,
    deleteApplicationResponse_httpStatus,

    -- ** DeleteEnvironment
    deleteEnvironment_environmentIdentifier,
    deleteEnvironmentResponse_arn,
    deleteEnvironmentResponse_environmentId,
    deleteEnvironmentResponse_lastUpdatedTime,
    deleteEnvironmentResponse_name,
    deleteEnvironmentResponse_state,
    deleteEnvironmentResponse_httpStatus,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_identifier,
    deleteResourcePolicyResponse_httpStatus,

    -- ** DeleteRoute
    deleteRoute_applicationIdentifier,
    deleteRoute_environmentIdentifier,
    deleteRoute_routeIdentifier,
    deleteRouteResponse_applicationId,
    deleteRouteResponse_arn,
    deleteRouteResponse_lastUpdatedTime,
    deleteRouteResponse_routeId,
    deleteRouteResponse_serviceId,
    deleteRouteResponse_state,
    deleteRouteResponse_httpStatus,

    -- ** DeleteService
    deleteService_applicationIdentifier,
    deleteService_environmentIdentifier,
    deleteService_serviceIdentifier,
    deleteServiceResponse_applicationId,
    deleteServiceResponse_arn,
    deleteServiceResponse_environmentId,
    deleteServiceResponse_lastUpdatedTime,
    deleteServiceResponse_name,
    deleteServiceResponse_serviceId,
    deleteServiceResponse_state,
    deleteServiceResponse_httpStatus,

    -- ** GetApplication
    getApplication_applicationIdentifier,
    getApplication_environmentIdentifier,
    getApplicationResponse_apiGatewayProxy,
    getApplicationResponse_applicationId,
    getApplicationResponse_arn,
    getApplicationResponse_createdByAccountId,
    getApplicationResponse_createdTime,
    getApplicationResponse_environmentId,
    getApplicationResponse_error,
    getApplicationResponse_lastUpdatedTime,
    getApplicationResponse_name,
    getApplicationResponse_ownerAccountId,
    getApplicationResponse_proxyType,
    getApplicationResponse_state,
    getApplicationResponse_tags,
    getApplicationResponse_vpcId,
    getApplicationResponse_httpStatus,

    -- ** GetEnvironment
    getEnvironment_environmentIdentifier,
    getEnvironmentResponse_arn,
    getEnvironmentResponse_createdTime,
    getEnvironmentResponse_description,
    getEnvironmentResponse_environmentId,
    getEnvironmentResponse_error,
    getEnvironmentResponse_lastUpdatedTime,
    getEnvironmentResponse_name,
    getEnvironmentResponse_networkFabricType,
    getEnvironmentResponse_ownerAccountId,
    getEnvironmentResponse_state,
    getEnvironmentResponse_tags,
    getEnvironmentResponse_transitGatewayId,
    getEnvironmentResponse_httpStatus,

    -- ** GetResourcePolicy
    getResourcePolicy_identifier,
    getResourcePolicyResponse_policy,
    getResourcePolicyResponse_httpStatus,

    -- ** GetRoute
    getRoute_applicationIdentifier,
    getRoute_environmentIdentifier,
    getRoute_routeIdentifier,
    getRouteResponse_applicationId,
    getRouteResponse_arn,
    getRouteResponse_createdByAccountId,
    getRouteResponse_createdTime,
    getRouteResponse_environmentId,
    getRouteResponse_error,
    getRouteResponse_includeChildPaths,
    getRouteResponse_lastUpdatedTime,
    getRouteResponse_methods,
    getRouteResponse_ownerAccountId,
    getRouteResponse_pathResourceToId,
    getRouteResponse_routeId,
    getRouteResponse_routeType,
    getRouteResponse_serviceId,
    getRouteResponse_sourcePath,
    getRouteResponse_state,
    getRouteResponse_tags,
    getRouteResponse_httpStatus,

    -- ** GetService
    getService_applicationIdentifier,
    getService_environmentIdentifier,
    getService_serviceIdentifier,
    getServiceResponse_applicationId,
    getServiceResponse_arn,
    getServiceResponse_createdByAccountId,
    getServiceResponse_createdTime,
    getServiceResponse_description,
    getServiceResponse_endpointType,
    getServiceResponse_environmentId,
    getServiceResponse_error,
    getServiceResponse_lambdaEndpoint,
    getServiceResponse_lastUpdatedTime,
    getServiceResponse_name,
    getServiceResponse_ownerAccountId,
    getServiceResponse_serviceId,
    getServiceResponse_state,
    getServiceResponse_tags,
    getServiceResponse_urlEndpoint,
    getServiceResponse_vpcId,
    getServiceResponse_httpStatus,

    -- ** ListApplications
    listApplications_maxResults,
    listApplications_nextToken,
    listApplications_environmentIdentifier,
    listApplicationsResponse_applicationSummaryList,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_httpStatus,

    -- ** ListEnvironmentVpcs
    listEnvironmentVpcs_maxResults,
    listEnvironmentVpcs_nextToken,
    listEnvironmentVpcs_environmentIdentifier,
    listEnvironmentVpcsResponse_environmentVpcList,
    listEnvironmentVpcsResponse_nextToken,
    listEnvironmentVpcsResponse_httpStatus,

    -- ** ListEnvironments
    listEnvironments_maxResults,
    listEnvironments_nextToken,
    listEnvironmentsResponse_environmentSummaryList,
    listEnvironmentsResponse_nextToken,
    listEnvironmentsResponse_httpStatus,

    -- ** ListRoutes
    listRoutes_maxResults,
    listRoutes_nextToken,
    listRoutes_applicationIdentifier,
    listRoutes_environmentIdentifier,
    listRoutesResponse_nextToken,
    listRoutesResponse_routeSummaryList,
    listRoutesResponse_httpStatus,

    -- ** ListServices
    listServices_maxResults,
    listServices_nextToken,
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
    updateRouteResponse_applicationId,
    updateRouteResponse_arn,
    updateRouteResponse_lastUpdatedTime,
    updateRouteResponse_routeId,
    updateRouteResponse_serviceId,
    updateRouteResponse_state,
    updateRouteResponse_httpStatus,

    -- * Types

    -- ** ApiGatewayProxyConfig
    apiGatewayProxyConfig_apiGatewayId,
    apiGatewayProxyConfig_endpointType,
    apiGatewayProxyConfig_nlbArn,
    apiGatewayProxyConfig_nlbName,
    apiGatewayProxyConfig_proxyUrl,
    apiGatewayProxyConfig_stageName,
    apiGatewayProxyConfig_vpcLinkId,

    -- ** ApiGatewayProxyInput
    apiGatewayProxyInput_endpointType,
    apiGatewayProxyInput_stageName,

    -- ** ApiGatewayProxySummary
    apiGatewayProxySummary_apiGatewayId,
    apiGatewayProxySummary_endpointType,
    apiGatewayProxySummary_nlbArn,
    apiGatewayProxySummary_nlbName,
    apiGatewayProxySummary_proxyUrl,
    apiGatewayProxySummary_stageName,
    apiGatewayProxySummary_vpcLinkId,

    -- ** ApplicationSummary
    applicationSummary_apiGatewayProxy,
    applicationSummary_applicationId,
    applicationSummary_arn,
    applicationSummary_createdByAccountId,
    applicationSummary_createdTime,
    applicationSummary_environmentId,
    applicationSummary_error,
    applicationSummary_lastUpdatedTime,
    applicationSummary_name,
    applicationSummary_ownerAccountId,
    applicationSummary_proxyType,
    applicationSummary_state,
    applicationSummary_tags,
    applicationSummary_vpcId,

    -- ** DefaultRouteInput
    defaultRouteInput_activationState,

    -- ** EnvironmentSummary
    environmentSummary_arn,
    environmentSummary_createdTime,
    environmentSummary_description,
    environmentSummary_environmentId,
    environmentSummary_error,
    environmentSummary_lastUpdatedTime,
    environmentSummary_name,
    environmentSummary_networkFabricType,
    environmentSummary_ownerAccountId,
    environmentSummary_state,
    environmentSummary_tags,
    environmentSummary_transitGatewayId,

    -- ** EnvironmentVpc
    environmentVpc_accountId,
    environmentVpc_cidrBlocks,
    environmentVpc_createdTime,
    environmentVpc_environmentId,
    environmentVpc_lastUpdatedTime,
    environmentVpc_vpcId,
    environmentVpc_vpcName,

    -- ** ErrorResponse
    errorResponse_accountId,
    errorResponse_additionalDetails,
    errorResponse_code,
    errorResponse_message,
    errorResponse_resourceIdentifier,
    errorResponse_resourceType,

    -- ** LambdaEndpointConfig
    lambdaEndpointConfig_arn,

    -- ** LambdaEndpointInput
    lambdaEndpointInput_arn,

    -- ** LambdaEndpointSummary
    lambdaEndpointSummary_arn,

    -- ** RouteSummary
    routeSummary_applicationId,
    routeSummary_arn,
    routeSummary_createdByAccountId,
    routeSummary_createdTime,
    routeSummary_environmentId,
    routeSummary_error,
    routeSummary_includeChildPaths,
    routeSummary_lastUpdatedTime,
    routeSummary_methods,
    routeSummary_ownerAccountId,
    routeSummary_pathResourceToId,
    routeSummary_routeId,
    routeSummary_routeType,
    routeSummary_serviceId,
    routeSummary_sourcePath,
    routeSummary_state,
    routeSummary_tags,

    -- ** ServiceSummary
    serviceSummary_applicationId,
    serviceSummary_arn,
    serviceSummary_createdByAccountId,
    serviceSummary_createdTime,
    serviceSummary_description,
    serviceSummary_endpointType,
    serviceSummary_environmentId,
    serviceSummary_error,
    serviceSummary_lambdaEndpoint,
    serviceSummary_lastUpdatedTime,
    serviceSummary_name,
    serviceSummary_ownerAccountId,
    serviceSummary_serviceId,
    serviceSummary_state,
    serviceSummary_tags,
    serviceSummary_urlEndpoint,
    serviceSummary_vpcId,

    -- ** UriPathRouteInput
    uriPathRouteInput_includeChildPaths,
    uriPathRouteInput_methods,
    uriPathRouteInput_activationState,
    uriPathRouteInput_sourcePath,

    -- ** UrlEndpointConfig
    urlEndpointConfig_healthUrl,
    urlEndpointConfig_url,

    -- ** UrlEndpointInput
    urlEndpointInput_healthUrl,
    urlEndpointInput_url,

    -- ** UrlEndpointSummary
    urlEndpointSummary_healthUrl,
    urlEndpointSummary_url,
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
