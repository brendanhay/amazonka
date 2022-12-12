{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppSync.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Lens
  ( -- * Operations

    -- ** AssociateApi
    associateApi_domainName,
    associateApi_apiId,
    associateApiResponse_apiAssociation,
    associateApiResponse_httpStatus,

    -- ** CreateApiCache
    createApiCache_atRestEncryptionEnabled,
    createApiCache_transitEncryptionEnabled,
    createApiCache_apiId,
    createApiCache_ttl,
    createApiCache_apiCachingBehavior,
    createApiCache_type,
    createApiCacheResponse_apiCache,
    createApiCacheResponse_httpStatus,

    -- ** CreateApiKey
    createApiKey_description,
    createApiKey_expires,
    createApiKey_apiId,
    createApiKeyResponse_apiKey,
    createApiKeyResponse_httpStatus,

    -- ** CreateDataSource
    createDataSource_description,
    createDataSource_dynamodbConfig,
    createDataSource_elasticsearchConfig,
    createDataSource_httpConfig,
    createDataSource_lambdaConfig,
    createDataSource_openSearchServiceConfig,
    createDataSource_relationalDatabaseConfig,
    createDataSource_serviceRoleArn,
    createDataSource_apiId,
    createDataSource_name,
    createDataSource_type,
    createDataSourceResponse_dataSource,
    createDataSourceResponse_httpStatus,

    -- ** CreateDomainName
    createDomainName_description,
    createDomainName_domainName,
    createDomainName_certificateArn,
    createDomainNameResponse_domainNameConfig,
    createDomainNameResponse_httpStatus,

    -- ** CreateFunction
    createFunction_code,
    createFunction_description,
    createFunction_functionVersion,
    createFunction_maxBatchSize,
    createFunction_requestMappingTemplate,
    createFunction_responseMappingTemplate,
    createFunction_runtime,
    createFunction_syncConfig,
    createFunction_apiId,
    createFunction_name,
    createFunction_dataSourceName,
    createFunctionResponse_functionConfiguration,
    createFunctionResponse_httpStatus,

    -- ** CreateGraphqlApi
    createGraphqlApi_additionalAuthenticationProviders,
    createGraphqlApi_lambdaAuthorizerConfig,
    createGraphqlApi_logConfig,
    createGraphqlApi_openIDConnectConfig,
    createGraphqlApi_tags,
    createGraphqlApi_userPoolConfig,
    createGraphqlApi_xrayEnabled,
    createGraphqlApi_name,
    createGraphqlApi_authenticationType,
    createGraphqlApiResponse_graphqlApi,
    createGraphqlApiResponse_httpStatus,

    -- ** CreateResolver
    createResolver_cachingConfig,
    createResolver_code,
    createResolver_dataSourceName,
    createResolver_kind,
    createResolver_maxBatchSize,
    createResolver_pipelineConfig,
    createResolver_requestMappingTemplate,
    createResolver_responseMappingTemplate,
    createResolver_runtime,
    createResolver_syncConfig,
    createResolver_apiId,
    createResolver_typeName,
    createResolver_fieldName,
    createResolverResponse_resolver,
    createResolverResponse_httpStatus,

    -- ** CreateType
    createType_apiId,
    createType_definition,
    createType_format,
    createTypeResponse_type,
    createTypeResponse_httpStatus,

    -- ** DeleteApiCache
    deleteApiCache_apiId,
    deleteApiCacheResponse_httpStatus,

    -- ** DeleteApiKey
    deleteApiKey_apiId,
    deleteApiKey_id,
    deleteApiKeyResponse_httpStatus,

    -- ** DeleteDataSource
    deleteDataSource_apiId,
    deleteDataSource_name,
    deleteDataSourceResponse_httpStatus,

    -- ** DeleteDomainName
    deleteDomainName_domainName,
    deleteDomainNameResponse_httpStatus,

    -- ** DeleteFunction
    deleteFunction_apiId,
    deleteFunction_functionId,
    deleteFunctionResponse_httpStatus,

    -- ** DeleteGraphqlApi
    deleteGraphqlApi_apiId,
    deleteGraphqlApiResponse_httpStatus,

    -- ** DeleteResolver
    deleteResolver_apiId,
    deleteResolver_typeName,
    deleteResolver_fieldName,
    deleteResolverResponse_httpStatus,

    -- ** DeleteType
    deleteType_apiId,
    deleteType_typeName,
    deleteTypeResponse_httpStatus,

    -- ** DisassociateApi
    disassociateApi_domainName,
    disassociateApiResponse_httpStatus,

    -- ** EvaluateCode
    evaluateCode_function,
    evaluateCode_runtime,
    evaluateCode_code,
    evaluateCode_context,
    evaluateCodeResponse_error,
    evaluateCodeResponse_evaluationResult,
    evaluateCodeResponse_logs,
    evaluateCodeResponse_httpStatus,

    -- ** EvaluateMappingTemplate
    evaluateMappingTemplate_template,
    evaluateMappingTemplate_context,
    evaluateMappingTemplateResponse_error,
    evaluateMappingTemplateResponse_evaluationResult,
    evaluateMappingTemplateResponse_logs,
    evaluateMappingTemplateResponse_httpStatus,

    -- ** FlushApiCache
    flushApiCache_apiId,
    flushApiCacheResponse_httpStatus,

    -- ** GetApiAssociation
    getApiAssociation_domainName,
    getApiAssociationResponse_apiAssociation,
    getApiAssociationResponse_httpStatus,

    -- ** GetApiCache
    getApiCache_apiId,
    getApiCacheResponse_apiCache,
    getApiCacheResponse_httpStatus,

    -- ** GetDataSource
    getDataSource_apiId,
    getDataSource_name,
    getDataSourceResponse_dataSource,
    getDataSourceResponse_httpStatus,

    -- ** GetDomainName
    getDomainName_domainName,
    getDomainNameResponse_domainNameConfig,
    getDomainNameResponse_httpStatus,

    -- ** GetFunction
    getFunction_apiId,
    getFunction_functionId,
    getFunctionResponse_functionConfiguration,
    getFunctionResponse_httpStatus,

    -- ** GetGraphqlApi
    getGraphqlApi_apiId,
    getGraphqlApiResponse_graphqlApi,
    getGraphqlApiResponse_httpStatus,

    -- ** GetIntrospectionSchema
    getIntrospectionSchema_includeDirectives,
    getIntrospectionSchema_apiId,
    getIntrospectionSchema_format,
    getIntrospectionSchemaResponse_schema,
    getIntrospectionSchemaResponse_httpStatus,

    -- ** GetResolver
    getResolver_apiId,
    getResolver_typeName,
    getResolver_fieldName,
    getResolverResponse_resolver,
    getResolverResponse_httpStatus,

    -- ** GetSchemaCreationStatus
    getSchemaCreationStatus_apiId,
    getSchemaCreationStatusResponse_details,
    getSchemaCreationStatusResponse_status,
    getSchemaCreationStatusResponse_httpStatus,

    -- ** GetType
    getType_apiId,
    getType_typeName,
    getType_format,
    getTypeResponse_type,
    getTypeResponse_httpStatus,

    -- ** ListApiKeys
    listApiKeys_maxResults,
    listApiKeys_nextToken,
    listApiKeys_apiId,
    listApiKeysResponse_apiKeys,
    listApiKeysResponse_nextToken,
    listApiKeysResponse_httpStatus,

    -- ** ListDataSources
    listDataSources_maxResults,
    listDataSources_nextToken,
    listDataSources_apiId,
    listDataSourcesResponse_dataSources,
    listDataSourcesResponse_nextToken,
    listDataSourcesResponse_httpStatus,

    -- ** ListDomainNames
    listDomainNames_maxResults,
    listDomainNames_nextToken,
    listDomainNamesResponse_domainNameConfigs,
    listDomainNamesResponse_nextToken,
    listDomainNamesResponse_httpStatus,

    -- ** ListFunctions
    listFunctions_maxResults,
    listFunctions_nextToken,
    listFunctions_apiId,
    listFunctionsResponse_functions,
    listFunctionsResponse_nextToken,
    listFunctionsResponse_httpStatus,

    -- ** ListGraphqlApis
    listGraphqlApis_maxResults,
    listGraphqlApis_nextToken,
    listGraphqlApisResponse_graphqlApis,
    listGraphqlApisResponse_nextToken,
    listGraphqlApisResponse_httpStatus,

    -- ** ListResolvers
    listResolvers_maxResults,
    listResolvers_nextToken,
    listResolvers_apiId,
    listResolvers_typeName,
    listResolversResponse_nextToken,
    listResolversResponse_resolvers,
    listResolversResponse_httpStatus,

    -- ** ListResolversByFunction
    listResolversByFunction_maxResults,
    listResolversByFunction_nextToken,
    listResolversByFunction_apiId,
    listResolversByFunction_functionId,
    listResolversByFunctionResponse_nextToken,
    listResolversByFunctionResponse_resolvers,
    listResolversByFunctionResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTypes
    listTypes_maxResults,
    listTypes_nextToken,
    listTypes_apiId,
    listTypes_format,
    listTypesResponse_nextToken,
    listTypesResponse_types,
    listTypesResponse_httpStatus,

    -- ** StartSchemaCreation
    startSchemaCreation_apiId,
    startSchemaCreation_definition,
    startSchemaCreationResponse_status,
    startSchemaCreationResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateApiCache
    updateApiCache_apiId,
    updateApiCache_ttl,
    updateApiCache_apiCachingBehavior,
    updateApiCache_type,
    updateApiCacheResponse_apiCache,
    updateApiCacheResponse_httpStatus,

    -- ** UpdateApiKey
    updateApiKey_description,
    updateApiKey_expires,
    updateApiKey_apiId,
    updateApiKey_id,
    updateApiKeyResponse_apiKey,
    updateApiKeyResponse_httpStatus,

    -- ** UpdateDataSource
    updateDataSource_description,
    updateDataSource_dynamodbConfig,
    updateDataSource_elasticsearchConfig,
    updateDataSource_httpConfig,
    updateDataSource_lambdaConfig,
    updateDataSource_openSearchServiceConfig,
    updateDataSource_relationalDatabaseConfig,
    updateDataSource_serviceRoleArn,
    updateDataSource_apiId,
    updateDataSource_name,
    updateDataSource_type,
    updateDataSourceResponse_dataSource,
    updateDataSourceResponse_httpStatus,

    -- ** UpdateDomainName
    updateDomainName_description,
    updateDomainName_domainName,
    updateDomainNameResponse_domainNameConfig,
    updateDomainNameResponse_httpStatus,

    -- ** UpdateFunction
    updateFunction_code,
    updateFunction_description,
    updateFunction_functionVersion,
    updateFunction_maxBatchSize,
    updateFunction_requestMappingTemplate,
    updateFunction_responseMappingTemplate,
    updateFunction_runtime,
    updateFunction_syncConfig,
    updateFunction_apiId,
    updateFunction_name,
    updateFunction_functionId,
    updateFunction_dataSourceName,
    updateFunctionResponse_functionConfiguration,
    updateFunctionResponse_httpStatus,

    -- ** UpdateGraphqlApi
    updateGraphqlApi_additionalAuthenticationProviders,
    updateGraphqlApi_authenticationType,
    updateGraphqlApi_lambdaAuthorizerConfig,
    updateGraphqlApi_logConfig,
    updateGraphqlApi_openIDConnectConfig,
    updateGraphqlApi_userPoolConfig,
    updateGraphqlApi_xrayEnabled,
    updateGraphqlApi_apiId,
    updateGraphqlApi_name,
    updateGraphqlApiResponse_graphqlApi,
    updateGraphqlApiResponse_httpStatus,

    -- ** UpdateResolver
    updateResolver_cachingConfig,
    updateResolver_code,
    updateResolver_dataSourceName,
    updateResolver_kind,
    updateResolver_maxBatchSize,
    updateResolver_pipelineConfig,
    updateResolver_requestMappingTemplate,
    updateResolver_responseMappingTemplate,
    updateResolver_runtime,
    updateResolver_syncConfig,
    updateResolver_apiId,
    updateResolver_typeName,
    updateResolver_fieldName,
    updateResolverResponse_resolver,
    updateResolverResponse_httpStatus,

    -- ** UpdateType
    updateType_definition,
    updateType_apiId,
    updateType_typeName,
    updateType_format,
    updateTypeResponse_type,
    updateTypeResponse_httpStatus,

    -- * Types

    -- ** AdditionalAuthenticationProvider
    additionalAuthenticationProvider_authenticationType,
    additionalAuthenticationProvider_lambdaAuthorizerConfig,
    additionalAuthenticationProvider_openIDConnectConfig,
    additionalAuthenticationProvider_userPoolConfig,

    -- ** ApiAssociation
    apiAssociation_apiId,
    apiAssociation_associationStatus,
    apiAssociation_deploymentDetail,
    apiAssociation_domainName,

    -- ** ApiCache
    apiCache_apiCachingBehavior,
    apiCache_atRestEncryptionEnabled,
    apiCache_status,
    apiCache_transitEncryptionEnabled,
    apiCache_ttl,
    apiCache_type,

    -- ** ApiKey
    apiKey_deletes,
    apiKey_description,
    apiKey_expires,
    apiKey_id,

    -- ** AppSyncRuntime
    appSyncRuntime_name,
    appSyncRuntime_runtimeVersion,

    -- ** AuthorizationConfig
    authorizationConfig_awsIamConfig,
    authorizationConfig_authorizationType,

    -- ** AwsIamConfig
    awsIamConfig_signingRegion,
    awsIamConfig_signingServiceName,

    -- ** CachingConfig
    cachingConfig_cachingKeys,
    cachingConfig_ttl,

    -- ** CodeError
    codeError_errorType,
    codeError_location,
    codeError_value,

    -- ** CodeErrorLocation
    codeErrorLocation_column,
    codeErrorLocation_line,
    codeErrorLocation_span,

    -- ** CognitoUserPoolConfig
    cognitoUserPoolConfig_appIdClientRegex,
    cognitoUserPoolConfig_userPoolId,
    cognitoUserPoolConfig_awsRegion,

    -- ** DataSource
    dataSource_dataSourceArn,
    dataSource_description,
    dataSource_dynamodbConfig,
    dataSource_elasticsearchConfig,
    dataSource_httpConfig,
    dataSource_lambdaConfig,
    dataSource_name,
    dataSource_openSearchServiceConfig,
    dataSource_relationalDatabaseConfig,
    dataSource_serviceRoleArn,
    dataSource_type,

    -- ** DeltaSyncConfig
    deltaSyncConfig_baseTableTTL,
    deltaSyncConfig_deltaSyncTableName,
    deltaSyncConfig_deltaSyncTableTTL,

    -- ** DomainNameConfig
    domainNameConfig_appsyncDomainName,
    domainNameConfig_certificateArn,
    domainNameConfig_description,
    domainNameConfig_domainName,
    domainNameConfig_hostedZoneId,

    -- ** DynamodbDataSourceConfig
    dynamodbDataSourceConfig_deltaSyncConfig,
    dynamodbDataSourceConfig_useCallerCredentials,
    dynamodbDataSourceConfig_versioned,
    dynamodbDataSourceConfig_tableName,
    dynamodbDataSourceConfig_awsRegion,

    -- ** ElasticsearchDataSourceConfig
    elasticsearchDataSourceConfig_endpoint,
    elasticsearchDataSourceConfig_awsRegion,

    -- ** ErrorDetail
    errorDetail_message,

    -- ** EvaluateCodeErrorDetail
    evaluateCodeErrorDetail_codeErrors,
    evaluateCodeErrorDetail_message,

    -- ** FunctionConfiguration
    functionConfiguration_code,
    functionConfiguration_dataSourceName,
    functionConfiguration_description,
    functionConfiguration_functionArn,
    functionConfiguration_functionId,
    functionConfiguration_functionVersion,
    functionConfiguration_maxBatchSize,
    functionConfiguration_name,
    functionConfiguration_requestMappingTemplate,
    functionConfiguration_responseMappingTemplate,
    functionConfiguration_runtime,
    functionConfiguration_syncConfig,

    -- ** GraphqlApi
    graphqlApi_additionalAuthenticationProviders,
    graphqlApi_apiId,
    graphqlApi_arn,
    graphqlApi_authenticationType,
    graphqlApi_lambdaAuthorizerConfig,
    graphqlApi_logConfig,
    graphqlApi_name,
    graphqlApi_openIDConnectConfig,
    graphqlApi_tags,
    graphqlApi_uris,
    graphqlApi_userPoolConfig,
    graphqlApi_wafWebAclArn,
    graphqlApi_xrayEnabled,

    -- ** HttpDataSourceConfig
    httpDataSourceConfig_authorizationConfig,
    httpDataSourceConfig_endpoint,

    -- ** LambdaAuthorizerConfig
    lambdaAuthorizerConfig_authorizerResultTtlInSeconds,
    lambdaAuthorizerConfig_identityValidationExpression,
    lambdaAuthorizerConfig_authorizerUri,

    -- ** LambdaConflictHandlerConfig
    lambdaConflictHandlerConfig_lambdaConflictHandlerArn,

    -- ** LambdaDataSourceConfig
    lambdaDataSourceConfig_lambdaFunctionArn,

    -- ** LogConfig
    logConfig_excludeVerboseContent,
    logConfig_fieldLogLevel,
    logConfig_cloudWatchLogsRoleArn,

    -- ** OpenIDConnectConfig
    openIDConnectConfig_authTTL,
    openIDConnectConfig_clientId,
    openIDConnectConfig_iatTTL,
    openIDConnectConfig_issuer,

    -- ** OpenSearchServiceDataSourceConfig
    openSearchServiceDataSourceConfig_endpoint,
    openSearchServiceDataSourceConfig_awsRegion,

    -- ** PipelineConfig
    pipelineConfig_functions,

    -- ** RdsHttpEndpointConfig
    rdsHttpEndpointConfig_awsRegion,
    rdsHttpEndpointConfig_awsSecretStoreArn,
    rdsHttpEndpointConfig_databaseName,
    rdsHttpEndpointConfig_dbClusterIdentifier,
    rdsHttpEndpointConfig_schema,

    -- ** RelationalDatabaseDataSourceConfig
    relationalDatabaseDataSourceConfig_rdsHttpEndpointConfig,
    relationalDatabaseDataSourceConfig_relationalDatabaseSourceType,

    -- ** Resolver
    resolver_cachingConfig,
    resolver_code,
    resolver_dataSourceName,
    resolver_fieldName,
    resolver_kind,
    resolver_maxBatchSize,
    resolver_pipelineConfig,
    resolver_requestMappingTemplate,
    resolver_resolverArn,
    resolver_responseMappingTemplate,
    resolver_runtime,
    resolver_syncConfig,
    resolver_typeName,

    -- ** SyncConfig
    syncConfig_conflictDetection,
    syncConfig_conflictHandler,
    syncConfig_lambdaConflictHandlerConfig,

    -- ** Type
    type_arn,
    type_definition,
    type_description,
    type_format,
    type_name,

    -- ** UserPoolConfig
    userPoolConfig_appIdClientRegex,
    userPoolConfig_userPoolId,
    userPoolConfig_awsRegion,
    userPoolConfig_defaultAction,
  )
where

import Amazonka.AppSync.AssociateApi
import Amazonka.AppSync.CreateApiCache
import Amazonka.AppSync.CreateApiKey
import Amazonka.AppSync.CreateDataSource
import Amazonka.AppSync.CreateDomainName
import Amazonka.AppSync.CreateFunction
import Amazonka.AppSync.CreateGraphqlApi
import Amazonka.AppSync.CreateResolver
import Amazonka.AppSync.CreateType
import Amazonka.AppSync.DeleteApiCache
import Amazonka.AppSync.DeleteApiKey
import Amazonka.AppSync.DeleteDataSource
import Amazonka.AppSync.DeleteDomainName
import Amazonka.AppSync.DeleteFunction
import Amazonka.AppSync.DeleteGraphqlApi
import Amazonka.AppSync.DeleteResolver
import Amazonka.AppSync.DeleteType
import Amazonka.AppSync.DisassociateApi
import Amazonka.AppSync.EvaluateCode
import Amazonka.AppSync.EvaluateMappingTemplate
import Amazonka.AppSync.FlushApiCache
import Amazonka.AppSync.GetApiAssociation
import Amazonka.AppSync.GetApiCache
import Amazonka.AppSync.GetDataSource
import Amazonka.AppSync.GetDomainName
import Amazonka.AppSync.GetFunction
import Amazonka.AppSync.GetGraphqlApi
import Amazonka.AppSync.GetIntrospectionSchema
import Amazonka.AppSync.GetResolver
import Amazonka.AppSync.GetSchemaCreationStatus
import Amazonka.AppSync.GetType
import Amazonka.AppSync.ListApiKeys
import Amazonka.AppSync.ListDataSources
import Amazonka.AppSync.ListDomainNames
import Amazonka.AppSync.ListFunctions
import Amazonka.AppSync.ListGraphqlApis
import Amazonka.AppSync.ListResolvers
import Amazonka.AppSync.ListResolversByFunction
import Amazonka.AppSync.ListTagsForResource
import Amazonka.AppSync.ListTypes
import Amazonka.AppSync.StartSchemaCreation
import Amazonka.AppSync.TagResource
import Amazonka.AppSync.Types.AdditionalAuthenticationProvider
import Amazonka.AppSync.Types.ApiAssociation
import Amazonka.AppSync.Types.ApiCache
import Amazonka.AppSync.Types.ApiKey
import Amazonka.AppSync.Types.AppSyncRuntime
import Amazonka.AppSync.Types.AuthorizationConfig
import Amazonka.AppSync.Types.AwsIamConfig
import Amazonka.AppSync.Types.CachingConfig
import Amazonka.AppSync.Types.CodeError
import Amazonka.AppSync.Types.CodeErrorLocation
import Amazonka.AppSync.Types.CognitoUserPoolConfig
import Amazonka.AppSync.Types.DataSource
import Amazonka.AppSync.Types.DeltaSyncConfig
import Amazonka.AppSync.Types.DomainNameConfig
import Amazonka.AppSync.Types.DynamodbDataSourceConfig
import Amazonka.AppSync.Types.ElasticsearchDataSourceConfig
import Amazonka.AppSync.Types.ErrorDetail
import Amazonka.AppSync.Types.EvaluateCodeErrorDetail
import Amazonka.AppSync.Types.FunctionConfiguration
import Amazonka.AppSync.Types.GraphqlApi
import Amazonka.AppSync.Types.HttpDataSourceConfig
import Amazonka.AppSync.Types.LambdaAuthorizerConfig
import Amazonka.AppSync.Types.LambdaConflictHandlerConfig
import Amazonka.AppSync.Types.LambdaDataSourceConfig
import Amazonka.AppSync.Types.LogConfig
import Amazonka.AppSync.Types.OpenIDConnectConfig
import Amazonka.AppSync.Types.OpenSearchServiceDataSourceConfig
import Amazonka.AppSync.Types.PipelineConfig
import Amazonka.AppSync.Types.RdsHttpEndpointConfig
import Amazonka.AppSync.Types.RelationalDatabaseDataSourceConfig
import Amazonka.AppSync.Types.Resolver
import Amazonka.AppSync.Types.SyncConfig
import Amazonka.AppSync.Types.Type
import Amazonka.AppSync.Types.UserPoolConfig
import Amazonka.AppSync.UntagResource
import Amazonka.AppSync.UpdateApiCache
import Amazonka.AppSync.UpdateApiKey
import Amazonka.AppSync.UpdateDataSource
import Amazonka.AppSync.UpdateDomainName
import Amazonka.AppSync.UpdateFunction
import Amazonka.AppSync.UpdateGraphqlApi
import Amazonka.AppSync.UpdateResolver
import Amazonka.AppSync.UpdateType
