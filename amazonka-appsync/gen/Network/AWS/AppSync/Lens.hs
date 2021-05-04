{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Lens
  ( -- * Operations

    -- ** ListResolvers
    listResolvers_nextToken,
    listResolvers_maxResults,
    listResolvers_apiId,
    listResolvers_typeName,
    listResolversResponse_nextToken,
    listResolversResponse_resolvers,
    listResolversResponse_httpStatus,

    -- ** UpdateDataSource
    updateDataSource_relationalDatabaseConfig,
    updateDataSource_serviceRoleArn,
    updateDataSource_elasticsearchConfig,
    updateDataSource_lambdaConfig,
    updateDataSource_dynamodbConfig,
    updateDataSource_description,
    updateDataSource_httpConfig,
    updateDataSource_apiId,
    updateDataSource_name,
    updateDataSource_type,
    updateDataSourceResponse_dataSource,
    updateDataSourceResponse_httpStatus,

    -- ** ListDataSources
    listDataSources_nextToken,
    listDataSources_maxResults,
    listDataSources_apiId,
    listDataSourcesResponse_nextToken,
    listDataSourcesResponse_dataSources,
    listDataSourcesResponse_httpStatus,

    -- ** CreateType
    createType_apiId,
    createType_definition,
    createType_format,
    createTypeResponse_type,
    createTypeResponse_httpStatus,

    -- ** GetFunction
    getFunction_apiId,
    getFunction_functionId,
    getFunctionResponse_functionConfiguration,
    getFunctionResponse_httpStatus,

    -- ** DeleteDataSource
    deleteDataSource_apiId,
    deleteDataSource_name,
    deleteDataSourceResponse_httpStatus,

    -- ** CreateDataSource
    createDataSource_relationalDatabaseConfig,
    createDataSource_serviceRoleArn,
    createDataSource_elasticsearchConfig,
    createDataSource_lambdaConfig,
    createDataSource_dynamodbConfig,
    createDataSource_description,
    createDataSource_httpConfig,
    createDataSource_apiId,
    createDataSource_name,
    createDataSource_type,
    createDataSourceResponse_dataSource,
    createDataSourceResponse_httpStatus,

    -- ** DeleteType
    deleteType_apiId,
    deleteType_typeName,
    deleteTypeResponse_httpStatus,

    -- ** UpdateType
    updateType_definition,
    updateType_apiId,
    updateType_typeName,
    updateType_format,
    updateTypeResponse_type,
    updateTypeResponse_httpStatus,

    -- ** ListResolversByFunction
    listResolversByFunction_nextToken,
    listResolversByFunction_maxResults,
    listResolversByFunction_apiId,
    listResolversByFunction_functionId,
    listResolversByFunctionResponse_nextToken,
    listResolversByFunctionResponse_resolvers,
    listResolversByFunctionResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** ListFunctions
    listFunctions_nextToken,
    listFunctions_maxResults,
    listFunctions_apiId,
    listFunctionsResponse_nextToken,
    listFunctionsResponse_functions,
    listFunctionsResponse_httpStatus,

    -- ** GetIntrospectionSchema
    getIntrospectionSchema_includeDirectives,
    getIntrospectionSchema_apiId,
    getIntrospectionSchema_format,
    getIntrospectionSchemaResponse_schema,
    getIntrospectionSchemaResponse_httpStatus,

    -- ** UpdateGraphqlApi
    updateGraphqlApi_openIDConnectConfig,
    updateGraphqlApi_userPoolConfig,
    updateGraphqlApi_xrayEnabled,
    updateGraphqlApi_logConfig,
    updateGraphqlApi_additionalAuthenticationProviders,
    updateGraphqlApi_authenticationType,
    updateGraphqlApi_apiId,
    updateGraphqlApi_name,
    updateGraphqlApiResponse_graphqlApi,
    updateGraphqlApiResponse_httpStatus,

    -- ** ListGraphqlApis
    listGraphqlApis_nextToken,
    listGraphqlApis_maxResults,
    listGraphqlApisResponse_nextToken,
    listGraphqlApisResponse_graphqlApis,
    listGraphqlApisResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** DeleteGraphqlApi
    deleteGraphqlApi_apiId,
    deleteGraphqlApiResponse_httpStatus,

    -- ** GetResolver
    getResolver_apiId,
    getResolver_typeName,
    getResolver_fieldName,
    getResolverResponse_resolver,
    getResolverResponse_httpStatus,

    -- ** DeleteApiCache
    deleteApiCache_apiId,
    deleteApiCacheResponse_httpStatus,

    -- ** UpdateApiCache
    updateApiCache_apiId,
    updateApiCache_ttl,
    updateApiCache_apiCachingBehavior,
    updateApiCache_type,
    updateApiCacheResponse_apiCache,
    updateApiCacheResponse_httpStatus,

    -- ** GetSchemaCreationStatus
    getSchemaCreationStatus_apiId,
    getSchemaCreationStatusResponse_status,
    getSchemaCreationStatusResponse_details,
    getSchemaCreationStatusResponse_httpStatus,

    -- ** GetApiCache
    getApiCache_apiId,
    getApiCacheResponse_apiCache,
    getApiCacheResponse_httpStatus,

    -- ** UpdateResolver
    updateResolver_responseMappingTemplate,
    updateResolver_kind,
    updateResolver_syncConfig,
    updateResolver_dataSourceName,
    updateResolver_cachingConfig,
    updateResolver_pipelineConfig,
    updateResolver_requestMappingTemplate,
    updateResolver_apiId,
    updateResolver_typeName,
    updateResolver_fieldName,
    updateResolverResponse_resolver,
    updateResolverResponse_httpStatus,

    -- ** DeleteResolver
    deleteResolver_apiId,
    deleteResolver_typeName,
    deleteResolver_fieldName,
    deleteResolverResponse_httpStatus,

    -- ** GetGraphqlApi
    getGraphqlApi_apiId,
    getGraphqlApiResponse_graphqlApi,
    getGraphqlApiResponse_httpStatus,

    -- ** ListTypes
    listTypes_nextToken,
    listTypes_maxResults,
    listTypes_apiId,
    listTypes_format,
    listTypesResponse_nextToken,
    listTypesResponse_types,
    listTypesResponse_httpStatus,

    -- ** GetType
    getType_apiId,
    getType_typeName,
    getType_format,
    getTypeResponse_type,
    getTypeResponse_httpStatus,

    -- ** CreateFunction
    createFunction_responseMappingTemplate,
    createFunction_syncConfig,
    createFunction_description,
    createFunction_requestMappingTemplate,
    createFunction_apiId,
    createFunction_name,
    createFunction_dataSourceName,
    createFunction_functionVersion,
    createFunctionResponse_functionConfiguration,
    createFunctionResponse_httpStatus,

    -- ** ListApiKeys
    listApiKeys_nextToken,
    listApiKeys_maxResults,
    listApiKeys_apiId,
    listApiKeysResponse_nextToken,
    listApiKeysResponse_apiKeys,
    listApiKeysResponse_httpStatus,

    -- ** DeleteApiKey
    deleteApiKey_apiId,
    deleteApiKey_id,
    deleteApiKeyResponse_httpStatus,

    -- ** UpdateApiKey
    updateApiKey_description,
    updateApiKey_expires,
    updateApiKey_apiId,
    updateApiKey_id,
    updateApiKeyResponse_apiKey,
    updateApiKeyResponse_httpStatus,

    -- ** GetDataSource
    getDataSource_apiId,
    getDataSource_name,
    getDataSourceResponse_dataSource,
    getDataSourceResponse_httpStatus,

    -- ** DeleteFunction
    deleteFunction_apiId,
    deleteFunction_functionId,
    deleteFunctionResponse_httpStatus,

    -- ** UpdateFunction
    updateFunction_responseMappingTemplate,
    updateFunction_syncConfig,
    updateFunction_description,
    updateFunction_requestMappingTemplate,
    updateFunction_apiId,
    updateFunction_name,
    updateFunction_functionId,
    updateFunction_dataSourceName,
    updateFunction_functionVersion,
    updateFunctionResponse_functionConfiguration,
    updateFunctionResponse_httpStatus,

    -- ** CreateApiKey
    createApiKey_description,
    createApiKey_expires,
    createApiKey_apiId,
    createApiKeyResponse_apiKey,
    createApiKeyResponse_httpStatus,

    -- ** StartSchemaCreation
    startSchemaCreation_apiId,
    startSchemaCreation_definition,
    startSchemaCreationResponse_status,
    startSchemaCreationResponse_httpStatus,

    -- ** FlushApiCache
    flushApiCache_apiId,
    flushApiCacheResponse_httpStatus,

    -- ** CreateApiCache
    createApiCache_atRestEncryptionEnabled,
    createApiCache_transitEncryptionEnabled,
    createApiCache_apiId,
    createApiCache_ttl,
    createApiCache_apiCachingBehavior,
    createApiCache_type,
    createApiCacheResponse_apiCache,
    createApiCacheResponse_httpStatus,

    -- ** CreateGraphqlApi
    createGraphqlApi_openIDConnectConfig,
    createGraphqlApi_userPoolConfig,
    createGraphqlApi_xrayEnabled,
    createGraphqlApi_tags,
    createGraphqlApi_logConfig,
    createGraphqlApi_additionalAuthenticationProviders,
    createGraphqlApi_name,
    createGraphqlApi_authenticationType,
    createGraphqlApiResponse_graphqlApi,
    createGraphqlApiResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** CreateResolver
    createResolver_responseMappingTemplate,
    createResolver_kind,
    createResolver_syncConfig,
    createResolver_dataSourceName,
    createResolver_cachingConfig,
    createResolver_pipelineConfig,
    createResolver_requestMappingTemplate,
    createResolver_apiId,
    createResolver_typeName,
    createResolver_fieldName,
    createResolverResponse_resolver,
    createResolverResponse_httpStatus,

    -- * Types

    -- ** AdditionalAuthenticationProvider
    additionalAuthenticationProvider_openIDConnectConfig,
    additionalAuthenticationProvider_userPoolConfig,
    additionalAuthenticationProvider_authenticationType,

    -- ** ApiCache
    apiCache_status,
    apiCache_atRestEncryptionEnabled,
    apiCache_ttl,
    apiCache_type,
    apiCache_transitEncryptionEnabled,
    apiCache_apiCachingBehavior,

    -- ** ApiKey
    apiKey_id,
    apiKey_deletes,
    apiKey_description,
    apiKey_expires,

    -- ** AuthorizationConfig
    authorizationConfig_awsIamConfig,
    authorizationConfig_authorizationType,

    -- ** AwsIamConfig
    awsIamConfig_signingServiceName,
    awsIamConfig_signingRegion,

    -- ** CachingConfig
    cachingConfig_ttl,
    cachingConfig_cachingKeys,

    -- ** CognitoUserPoolConfig
    cognitoUserPoolConfig_appIdClientRegex,
    cognitoUserPoolConfig_userPoolId,
    cognitoUserPoolConfig_awsRegion,

    -- ** DataSource
    dataSource_relationalDatabaseConfig,
    dataSource_serviceRoleArn,
    dataSource_elasticsearchConfig,
    dataSource_lambdaConfig,
    dataSource_name,
    dataSource_dynamodbConfig,
    dataSource_description,
    dataSource_dataSourceArn,
    dataSource_type,
    dataSource_httpConfig,

    -- ** DeltaSyncConfig
    deltaSyncConfig_baseTableTTL,
    deltaSyncConfig_deltaSyncTableName,
    deltaSyncConfig_deltaSyncTableTTL,

    -- ** DynamodbDataSourceConfig
    dynamodbDataSourceConfig_useCallerCredentials,
    dynamodbDataSourceConfig_versioned,
    dynamodbDataSourceConfig_deltaSyncConfig,
    dynamodbDataSourceConfig_tableName,
    dynamodbDataSourceConfig_awsRegion,

    -- ** ElasticsearchDataSourceConfig
    elasticsearchDataSourceConfig_endpoint,
    elasticsearchDataSourceConfig_awsRegion,

    -- ** FunctionConfiguration
    functionConfiguration_responseMappingTemplate,
    functionConfiguration_functionVersion,
    functionConfiguration_syncConfig,
    functionConfiguration_dataSourceName,
    functionConfiguration_name,
    functionConfiguration_functionId,
    functionConfiguration_functionArn,
    functionConfiguration_description,
    functionConfiguration_requestMappingTemplate,

    -- ** GraphqlApi
    graphqlApi_wafWebAclArn,
    graphqlApi_openIDConnectConfig,
    graphqlApi_apiId,
    graphqlApi_arn,
    graphqlApi_name,
    graphqlApi_userPoolConfig,
    graphqlApi_xrayEnabled,
    graphqlApi_tags,
    graphqlApi_logConfig,
    graphqlApi_additionalAuthenticationProviders,
    graphqlApi_authenticationType,
    graphqlApi_uris,

    -- ** HttpDataSourceConfig
    httpDataSourceConfig_authorizationConfig,
    httpDataSourceConfig_endpoint,

    -- ** LambdaConflictHandlerConfig
    lambdaConflictHandlerConfig_lambdaConflictHandlerArn,

    -- ** LambdaDataSourceConfig
    lambdaDataSourceConfig_lambdaFunctionArn,

    -- ** LogConfig
    logConfig_excludeVerboseContent,
    logConfig_fieldLogLevel,
    logConfig_cloudWatchLogsRoleArn,

    -- ** OpenIDConnectConfig
    openIDConnectConfig_clientId,
    openIDConnectConfig_authTTL,
    openIDConnectConfig_iatTTL,
    openIDConnectConfig_issuer,

    -- ** PipelineConfig
    pipelineConfig_functions,

    -- ** RdsHttpEndpointConfig
    rdsHttpEndpointConfig_awsSecretStoreArn,
    rdsHttpEndpointConfig_schema,
    rdsHttpEndpointConfig_dbClusterIdentifier,
    rdsHttpEndpointConfig_awsRegion,
    rdsHttpEndpointConfig_databaseName,

    -- ** RelationalDatabaseDataSourceConfig
    relationalDatabaseDataSourceConfig_rdsHttpEndpointConfig,
    relationalDatabaseDataSourceConfig_relationalDatabaseSourceType,

    -- ** Resolver
    resolver_responseMappingTemplate,
    resolver_typeName,
    resolver_kind,
    resolver_syncConfig,
    resolver_dataSourceName,
    resolver_cachingConfig,
    resolver_resolverArn,
    resolver_pipelineConfig,
    resolver_fieldName,
    resolver_requestMappingTemplate,

    -- ** SyncConfig
    syncConfig_conflictHandler,
    syncConfig_lambdaConflictHandlerConfig,
    syncConfig_conflictDetection,

    -- ** Type
    type_format,
    type_arn,
    type_name,
    type_description,
    type_definition,

    -- ** UserPoolConfig
    userPoolConfig_appIdClientRegex,
    userPoolConfig_userPoolId,
    userPoolConfig_awsRegion,
    userPoolConfig_defaultAction,
  )
where

import Network.AWS.AppSync.CreateApiCache
import Network.AWS.AppSync.CreateApiKey
import Network.AWS.AppSync.CreateDataSource
import Network.AWS.AppSync.CreateFunction
import Network.AWS.AppSync.CreateGraphqlApi
import Network.AWS.AppSync.CreateResolver
import Network.AWS.AppSync.CreateType
import Network.AWS.AppSync.DeleteApiCache
import Network.AWS.AppSync.DeleteApiKey
import Network.AWS.AppSync.DeleteDataSource
import Network.AWS.AppSync.DeleteFunction
import Network.AWS.AppSync.DeleteGraphqlApi
import Network.AWS.AppSync.DeleteResolver
import Network.AWS.AppSync.DeleteType
import Network.AWS.AppSync.FlushApiCache
import Network.AWS.AppSync.GetApiCache
import Network.AWS.AppSync.GetDataSource
import Network.AWS.AppSync.GetFunction
import Network.AWS.AppSync.GetGraphqlApi
import Network.AWS.AppSync.GetIntrospectionSchema
import Network.AWS.AppSync.GetResolver
import Network.AWS.AppSync.GetSchemaCreationStatus
import Network.AWS.AppSync.GetType
import Network.AWS.AppSync.ListApiKeys
import Network.AWS.AppSync.ListDataSources
import Network.AWS.AppSync.ListFunctions
import Network.AWS.AppSync.ListGraphqlApis
import Network.AWS.AppSync.ListResolvers
import Network.AWS.AppSync.ListResolversByFunction
import Network.AWS.AppSync.ListTagsForResource
import Network.AWS.AppSync.ListTypes
import Network.AWS.AppSync.StartSchemaCreation
import Network.AWS.AppSync.TagResource
import Network.AWS.AppSync.Types.AdditionalAuthenticationProvider
import Network.AWS.AppSync.Types.ApiCache
import Network.AWS.AppSync.Types.ApiKey
import Network.AWS.AppSync.Types.AuthorizationConfig
import Network.AWS.AppSync.Types.AwsIamConfig
import Network.AWS.AppSync.Types.CachingConfig
import Network.AWS.AppSync.Types.CognitoUserPoolConfig
import Network.AWS.AppSync.Types.DataSource
import Network.AWS.AppSync.Types.DeltaSyncConfig
import Network.AWS.AppSync.Types.DynamodbDataSourceConfig
import Network.AWS.AppSync.Types.ElasticsearchDataSourceConfig
import Network.AWS.AppSync.Types.FunctionConfiguration
import Network.AWS.AppSync.Types.GraphqlApi
import Network.AWS.AppSync.Types.HttpDataSourceConfig
import Network.AWS.AppSync.Types.LambdaConflictHandlerConfig
import Network.AWS.AppSync.Types.LambdaDataSourceConfig
import Network.AWS.AppSync.Types.LogConfig
import Network.AWS.AppSync.Types.OpenIDConnectConfig
import Network.AWS.AppSync.Types.PipelineConfig
import Network.AWS.AppSync.Types.RdsHttpEndpointConfig
import Network.AWS.AppSync.Types.RelationalDatabaseDataSourceConfig
import Network.AWS.AppSync.Types.Resolver
import Network.AWS.AppSync.Types.SyncConfig
import Network.AWS.AppSync.Types.Type
import Network.AWS.AppSync.Types.UserPoolConfig
import Network.AWS.AppSync.UntagResource
import Network.AWS.AppSync.UpdateApiCache
import Network.AWS.AppSync.UpdateApiKey
import Network.AWS.AppSync.UpdateDataSource
import Network.AWS.AppSync.UpdateFunction
import Network.AWS.AppSync.UpdateGraphqlApi
import Network.AWS.AppSync.UpdateResolver
import Network.AWS.AppSync.UpdateType
