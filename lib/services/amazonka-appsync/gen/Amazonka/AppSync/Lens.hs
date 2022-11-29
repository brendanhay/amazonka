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
    createApiCache_transitEncryptionEnabled,
    createApiCache_atRestEncryptionEnabled,
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
    createDataSource_relationalDatabaseConfig,
    createDataSource_serviceRoleArn,
    createDataSource_openSearchServiceConfig,
    createDataSource_description,
    createDataSource_elasticsearchConfig,
    createDataSource_lambdaConfig,
    createDataSource_dynamodbConfig,
    createDataSource_httpConfig,
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
    createFunction_maxBatchSize,
    createFunction_functionVersion,
    createFunction_runtime,
    createFunction_description,
    createFunction_responseMappingTemplate,
    createFunction_syncConfig,
    createFunction_requestMappingTemplate,
    createFunction_apiId,
    createFunction_name,
    createFunction_dataSourceName,
    createFunctionResponse_functionConfiguration,
    createFunctionResponse_httpStatus,

    -- ** CreateGraphqlApi
    createGraphqlApi_tags,
    createGraphqlApi_xrayEnabled,
    createGraphqlApi_openIDConnectConfig,
    createGraphqlApi_userPoolConfig,
    createGraphqlApi_additionalAuthenticationProviders,
    createGraphqlApi_lambdaAuthorizerConfig,
    createGraphqlApi_logConfig,
    createGraphqlApi_name,
    createGraphqlApi_authenticationType,
    createGraphqlApiResponse_graphqlApi,
    createGraphqlApiResponse_httpStatus,

    -- ** CreateResolver
    createResolver_code,
    createResolver_maxBatchSize,
    createResolver_cachingConfig,
    createResolver_pipelineConfig,
    createResolver_kind,
    createResolver_runtime,
    createResolver_dataSourceName,
    createResolver_responseMappingTemplate,
    createResolver_syncConfig,
    createResolver_requestMappingTemplate,
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
    evaluateCodeResponse_logs,
    evaluateCodeResponse_evaluationResult,
    evaluateCodeResponse_error,
    evaluateCodeResponse_httpStatus,

    -- ** EvaluateMappingTemplate
    evaluateMappingTemplate_template,
    evaluateMappingTemplate_context,
    evaluateMappingTemplateResponse_logs,
    evaluateMappingTemplateResponse_evaluationResult,
    evaluateMappingTemplateResponse_error,
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
    getSchemaCreationStatusResponse_status,
    getSchemaCreationStatusResponse_details,
    getSchemaCreationStatusResponse_httpStatus,

    -- ** GetType
    getType_apiId,
    getType_typeName,
    getType_format,
    getTypeResponse_type,
    getTypeResponse_httpStatus,

    -- ** ListApiKeys
    listApiKeys_nextToken,
    listApiKeys_maxResults,
    listApiKeys_apiId,
    listApiKeysResponse_nextToken,
    listApiKeysResponse_apiKeys,
    listApiKeysResponse_httpStatus,

    -- ** ListDataSources
    listDataSources_nextToken,
    listDataSources_maxResults,
    listDataSources_apiId,
    listDataSourcesResponse_nextToken,
    listDataSourcesResponse_dataSources,
    listDataSourcesResponse_httpStatus,

    -- ** ListDomainNames
    listDomainNames_nextToken,
    listDomainNames_maxResults,
    listDomainNamesResponse_nextToken,
    listDomainNamesResponse_domainNameConfigs,
    listDomainNamesResponse_httpStatus,

    -- ** ListFunctions
    listFunctions_nextToken,
    listFunctions_maxResults,
    listFunctions_apiId,
    listFunctionsResponse_functions,
    listFunctionsResponse_nextToken,
    listFunctionsResponse_httpStatus,

    -- ** ListGraphqlApis
    listGraphqlApis_nextToken,
    listGraphqlApis_maxResults,
    listGraphqlApisResponse_nextToken,
    listGraphqlApisResponse_graphqlApis,
    listGraphqlApisResponse_httpStatus,

    -- ** ListResolvers
    listResolvers_nextToken,
    listResolvers_maxResults,
    listResolvers_apiId,
    listResolvers_typeName,
    listResolversResponse_nextToken,
    listResolversResponse_resolvers,
    listResolversResponse_httpStatus,

    -- ** ListResolversByFunction
    listResolversByFunction_nextToken,
    listResolversByFunction_maxResults,
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
    listTypes_nextToken,
    listTypes_maxResults,
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
    updateDataSource_relationalDatabaseConfig,
    updateDataSource_serviceRoleArn,
    updateDataSource_openSearchServiceConfig,
    updateDataSource_description,
    updateDataSource_elasticsearchConfig,
    updateDataSource_lambdaConfig,
    updateDataSource_dynamodbConfig,
    updateDataSource_httpConfig,
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
    updateFunction_maxBatchSize,
    updateFunction_functionVersion,
    updateFunction_runtime,
    updateFunction_description,
    updateFunction_responseMappingTemplate,
    updateFunction_syncConfig,
    updateFunction_requestMappingTemplate,
    updateFunction_apiId,
    updateFunction_name,
    updateFunction_functionId,
    updateFunction_dataSourceName,
    updateFunctionResponse_functionConfiguration,
    updateFunctionResponse_httpStatus,

    -- ** UpdateGraphqlApi
    updateGraphqlApi_xrayEnabled,
    updateGraphqlApi_authenticationType,
    updateGraphqlApi_openIDConnectConfig,
    updateGraphqlApi_userPoolConfig,
    updateGraphqlApi_additionalAuthenticationProviders,
    updateGraphqlApi_lambdaAuthorizerConfig,
    updateGraphqlApi_logConfig,
    updateGraphqlApi_apiId,
    updateGraphqlApi_name,
    updateGraphqlApiResponse_graphqlApi,
    updateGraphqlApiResponse_httpStatus,

    -- ** UpdateResolver
    updateResolver_code,
    updateResolver_maxBatchSize,
    updateResolver_cachingConfig,
    updateResolver_pipelineConfig,
    updateResolver_kind,
    updateResolver_runtime,
    updateResolver_dataSourceName,
    updateResolver_responseMappingTemplate,
    updateResolver_syncConfig,
    updateResolver_requestMappingTemplate,
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
    additionalAuthenticationProvider_openIDConnectConfig,
    additionalAuthenticationProvider_userPoolConfig,
    additionalAuthenticationProvider_lambdaAuthorizerConfig,

    -- ** ApiAssociation
    apiAssociation_domainName,
    apiAssociation_apiId,
    apiAssociation_associationStatus,
    apiAssociation_deploymentDetail,

    -- ** ApiCache
    apiCache_transitEncryptionEnabled,
    apiCache_type,
    apiCache_apiCachingBehavior,
    apiCache_ttl,
    apiCache_status,
    apiCache_atRestEncryptionEnabled,

    -- ** ApiKey
    apiKey_description,
    apiKey_id,
    apiKey_expires,
    apiKey_deletes,

    -- ** AppSyncRuntime
    appSyncRuntime_name,
    appSyncRuntime_runtimeVersion,

    -- ** AuthorizationConfig
    authorizationConfig_awsIamConfig,
    authorizationConfig_authorizationType,

    -- ** AwsIamConfig
    awsIamConfig_signingServiceName,
    awsIamConfig_signingRegion,

    -- ** CachingConfig
    cachingConfig_cachingKeys,
    cachingConfig_ttl,

    -- ** CodeError
    codeError_location,
    codeError_errorType,
    codeError_value,

    -- ** CodeErrorLocation
    codeErrorLocation_line,
    codeErrorLocation_span,
    codeErrorLocation_column,

    -- ** CognitoUserPoolConfig
    cognitoUserPoolConfig_appIdClientRegex,
    cognitoUserPoolConfig_userPoolId,
    cognitoUserPoolConfig_awsRegion,

    -- ** DataSource
    dataSource_name,
    dataSource_dataSourceArn,
    dataSource_type,
    dataSource_relationalDatabaseConfig,
    dataSource_serviceRoleArn,
    dataSource_openSearchServiceConfig,
    dataSource_description,
    dataSource_elasticsearchConfig,
    dataSource_lambdaConfig,
    dataSource_dynamodbConfig,
    dataSource_httpConfig,

    -- ** DeltaSyncConfig
    deltaSyncConfig_baseTableTTL,
    deltaSyncConfig_deltaSyncTableName,
    deltaSyncConfig_deltaSyncTableTTL,

    -- ** DomainNameConfig
    domainNameConfig_hostedZoneId,
    domainNameConfig_domainName,
    domainNameConfig_description,
    domainNameConfig_appsyncDomainName,
    domainNameConfig_certificateArn,

    -- ** DynamodbDataSourceConfig
    dynamodbDataSourceConfig_useCallerCredentials,
    dynamodbDataSourceConfig_versioned,
    dynamodbDataSourceConfig_deltaSyncConfig,
    dynamodbDataSourceConfig_tableName,
    dynamodbDataSourceConfig_awsRegion,

    -- ** ElasticsearchDataSourceConfig
    elasticsearchDataSourceConfig_endpoint,
    elasticsearchDataSourceConfig_awsRegion,

    -- ** ErrorDetail
    errorDetail_message,

    -- ** EvaluateCodeErrorDetail
    evaluateCodeErrorDetail_message,
    evaluateCodeErrorDetail_codeErrors,

    -- ** FunctionConfiguration
    functionConfiguration_functionArn,
    functionConfiguration_name,
    functionConfiguration_code,
    functionConfiguration_maxBatchSize,
    functionConfiguration_functionVersion,
    functionConfiguration_runtime,
    functionConfiguration_description,
    functionConfiguration_dataSourceName,
    functionConfiguration_responseMappingTemplate,
    functionConfiguration_syncConfig,
    functionConfiguration_functionId,
    functionConfiguration_requestMappingTemplate,

    -- ** GraphqlApi
    graphqlApi_tags,
    graphqlApi_xrayEnabled,
    graphqlApi_name,
    graphqlApi_authenticationType,
    graphqlApi_apiId,
    graphqlApi_openIDConnectConfig,
    graphqlApi_arn,
    graphqlApi_uris,
    graphqlApi_userPoolConfig,
    graphqlApi_additionalAuthenticationProviders,
    graphqlApi_lambdaAuthorizerConfig,
    graphqlApi_logConfig,
    graphqlApi_wafWebAclArn,

    -- ** HttpDataSourceConfig
    httpDataSourceConfig_endpoint,
    httpDataSourceConfig_authorizationConfig,

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
    openIDConnectConfig_iatTTL,
    openIDConnectConfig_clientId,
    openIDConnectConfig_authTTL,
    openIDConnectConfig_issuer,

    -- ** OpenSearchServiceDataSourceConfig
    openSearchServiceDataSourceConfig_endpoint,
    openSearchServiceDataSourceConfig_awsRegion,

    -- ** PipelineConfig
    pipelineConfig_functions,

    -- ** RdsHttpEndpointConfig
    rdsHttpEndpointConfig_databaseName,
    rdsHttpEndpointConfig_dbClusterIdentifier,
    rdsHttpEndpointConfig_awsRegion,
    rdsHttpEndpointConfig_schema,
    rdsHttpEndpointConfig_awsSecretStoreArn,

    -- ** RelationalDatabaseDataSourceConfig
    relationalDatabaseDataSourceConfig_relationalDatabaseSourceType,
    relationalDatabaseDataSourceConfig_rdsHttpEndpointConfig,

    -- ** Resolver
    resolver_code,
    resolver_maxBatchSize,
    resolver_resolverArn,
    resolver_fieldName,
    resolver_cachingConfig,
    resolver_pipelineConfig,
    resolver_kind,
    resolver_runtime,
    resolver_typeName,
    resolver_dataSourceName,
    resolver_responseMappingTemplate,
    resolver_syncConfig,
    resolver_requestMappingTemplate,

    -- ** SyncConfig
    syncConfig_conflictHandler,
    syncConfig_conflictDetection,
    syncConfig_lambdaConflictHandlerConfig,

    -- ** Type
    type_name,
    type_format,
    type_arn,
    type_description,
    type_definition,

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
