{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ApiKeyValidityOutOfBoundsException,
    _AccessDeniedException,
    _ApiKeyLimitExceededException,
    _ApiLimitExceededException,
    _NotFoundException,
    _GraphQLSchemaException,
    _ConcurrentModificationException,
    _InternalFailureException,
    _UnauthorizedException,
    _BadRequestException,
    _LimitExceededException,

    -- * ApiCacheStatus
    ApiCacheStatus (..),

    -- * ApiCacheType
    ApiCacheType (..),

    -- * ApiCachingBehavior
    ApiCachingBehavior (..),

    -- * AuthenticationType
    AuthenticationType (..),

    -- * AuthorizationType
    AuthorizationType (..),

    -- * ConflictDetectionType
    ConflictDetectionType (..),

    -- * ConflictHandlerType
    ConflictHandlerType (..),

    -- * DataSourceType
    DataSourceType (..),

    -- * DefaultAction
    DefaultAction (..),

    -- * FieldLogLevel
    FieldLogLevel (..),

    -- * OutputType
    OutputType (..),

    -- * RelationalDatabaseSourceType
    RelationalDatabaseSourceType (..),

    -- * ResolverKind
    ResolverKind (..),

    -- * SchemaStatus
    SchemaStatus (..),

    -- * TypeDefinitionFormat
    TypeDefinitionFormat (..),

    -- * AdditionalAuthenticationProvider
    AdditionalAuthenticationProvider (..),
    newAdditionalAuthenticationProvider,
    additionalAuthenticationProvider_openIDConnectConfig,
    additionalAuthenticationProvider_lambdaAuthorizerConfig,
    additionalAuthenticationProvider_userPoolConfig,
    additionalAuthenticationProvider_authenticationType,

    -- * ApiCache
    ApiCache (..),
    newApiCache,
    apiCache_ttl,
    apiCache_status,
    apiCache_atRestEncryptionEnabled,
    apiCache_transitEncryptionEnabled,
    apiCache_apiCachingBehavior,
    apiCache_type,

    -- * ApiKey
    ApiKey (..),
    newApiKey,
    apiKey_expires,
    apiKey_deletes,
    apiKey_id,
    apiKey_description,

    -- * AuthorizationConfig
    AuthorizationConfig (..),
    newAuthorizationConfig,
    authorizationConfig_awsIamConfig,
    authorizationConfig_authorizationType,

    -- * AwsIamConfig
    AwsIamConfig (..),
    newAwsIamConfig,
    awsIamConfig_signingServiceName,
    awsIamConfig_signingRegion,

    -- * CachingConfig
    CachingConfig (..),
    newCachingConfig,
    cachingConfig_ttl,
    cachingConfig_cachingKeys,

    -- * CognitoUserPoolConfig
    CognitoUserPoolConfig (..),
    newCognitoUserPoolConfig,
    cognitoUserPoolConfig_appIdClientRegex,
    cognitoUserPoolConfig_userPoolId,
    cognitoUserPoolConfig_awsRegion,

    -- * DataSource
    DataSource (..),
    newDataSource,
    dataSource_serviceRoleArn,
    dataSource_relationalDatabaseConfig,
    dataSource_dataSourceArn,
    dataSource_dynamodbConfig,
    dataSource_name,
    dataSource_httpConfig,
    dataSource_openSearchServiceConfig,
    dataSource_lambdaConfig,
    dataSource_type,
    dataSource_description,
    dataSource_elasticsearchConfig,

    -- * DeltaSyncConfig
    DeltaSyncConfig (..),
    newDeltaSyncConfig,
    deltaSyncConfig_baseTableTTL,
    deltaSyncConfig_deltaSyncTableName,
    deltaSyncConfig_deltaSyncTableTTL,

    -- * DynamodbDataSourceConfig
    DynamodbDataSourceConfig (..),
    newDynamodbDataSourceConfig,
    dynamodbDataSourceConfig_versioned,
    dynamodbDataSourceConfig_useCallerCredentials,
    dynamodbDataSourceConfig_deltaSyncConfig,
    dynamodbDataSourceConfig_tableName,
    dynamodbDataSourceConfig_awsRegion,

    -- * ElasticsearchDataSourceConfig
    ElasticsearchDataSourceConfig (..),
    newElasticsearchDataSourceConfig,
    elasticsearchDataSourceConfig_endpoint,
    elasticsearchDataSourceConfig_awsRegion,

    -- * FunctionConfiguration
    FunctionConfiguration (..),
    newFunctionConfiguration,
    functionConfiguration_functionArn,
    functionConfiguration_dataSourceName,
    functionConfiguration_requestMappingTemplate,
    functionConfiguration_name,
    functionConfiguration_functionId,
    functionConfiguration_responseMappingTemplate,
    functionConfiguration_syncConfig,
    functionConfiguration_functionVersion,
    functionConfiguration_description,

    -- * GraphqlApi
    GraphqlApi (..),
    newGraphqlApi,
    graphqlApi_xrayEnabled,
    graphqlApi_arn,
    graphqlApi_apiId,
    graphqlApi_uris,
    graphqlApi_openIDConnectConfig,
    graphqlApi_wafWebAclArn,
    graphqlApi_additionalAuthenticationProviders,
    graphqlApi_lambdaAuthorizerConfig,
    graphqlApi_name,
    graphqlApi_userPoolConfig,
    graphqlApi_authenticationType,
    graphqlApi_logConfig,
    graphqlApi_tags,

    -- * HttpDataSourceConfig
    HttpDataSourceConfig (..),
    newHttpDataSourceConfig,
    httpDataSourceConfig_authorizationConfig,
    httpDataSourceConfig_endpoint,

    -- * LambdaAuthorizerConfig
    LambdaAuthorizerConfig (..),
    newLambdaAuthorizerConfig,
    lambdaAuthorizerConfig_identityValidationExpression,
    lambdaAuthorizerConfig_authorizerResultTtlInSeconds,
    lambdaAuthorizerConfig_authorizerUri,

    -- * LambdaConflictHandlerConfig
    LambdaConflictHandlerConfig (..),
    newLambdaConflictHandlerConfig,
    lambdaConflictHandlerConfig_lambdaConflictHandlerArn,

    -- * LambdaDataSourceConfig
    LambdaDataSourceConfig (..),
    newLambdaDataSourceConfig,
    lambdaDataSourceConfig_lambdaFunctionArn,

    -- * LogConfig
    LogConfig (..),
    newLogConfig,
    logConfig_excludeVerboseContent,
    logConfig_fieldLogLevel,
    logConfig_cloudWatchLogsRoleArn,

    -- * OpenIDConnectConfig
    OpenIDConnectConfig (..),
    newOpenIDConnectConfig,
    openIDConnectConfig_authTTL,
    openIDConnectConfig_clientId,
    openIDConnectConfig_iatTTL,
    openIDConnectConfig_issuer,

    -- * OpenSearchServiceDataSourceConfig
    OpenSearchServiceDataSourceConfig (..),
    newOpenSearchServiceDataSourceConfig,
    openSearchServiceDataSourceConfig_endpoint,
    openSearchServiceDataSourceConfig_awsRegion,

    -- * PipelineConfig
    PipelineConfig (..),
    newPipelineConfig,
    pipelineConfig_functions,

    -- * RdsHttpEndpointConfig
    RdsHttpEndpointConfig (..),
    newRdsHttpEndpointConfig,
    rdsHttpEndpointConfig_dbClusterIdentifier,
    rdsHttpEndpointConfig_schema,
    rdsHttpEndpointConfig_databaseName,
    rdsHttpEndpointConfig_awsRegion,
    rdsHttpEndpointConfig_awsSecretStoreArn,

    -- * RelationalDatabaseDataSourceConfig
    RelationalDatabaseDataSourceConfig (..),
    newRelationalDatabaseDataSourceConfig,
    relationalDatabaseDataSourceConfig_relationalDatabaseSourceType,
    relationalDatabaseDataSourceConfig_rdsHttpEndpointConfig,

    -- * Resolver
    Resolver (..),
    newResolver,
    resolver_typeName,
    resolver_dataSourceName,
    resolver_requestMappingTemplate,
    resolver_kind,
    resolver_resolverArn,
    resolver_cachingConfig,
    resolver_responseMappingTemplate,
    resolver_fieldName,
    resolver_syncConfig,
    resolver_pipelineConfig,

    -- * SyncConfig
    SyncConfig (..),
    newSyncConfig,
    syncConfig_conflictHandler,
    syncConfig_conflictDetection,
    syncConfig_lambdaConflictHandlerConfig,

    -- * Type
    Type (..),
    newType,
    type_arn,
    type_definition,
    type_format,
    type_name,
    type_description,

    -- * UserPoolConfig
    UserPoolConfig (..),
    newUserPoolConfig,
    userPoolConfig_appIdClientRegex,
    userPoolConfig_userPoolId,
    userPoolConfig_awsRegion,
    userPoolConfig_defaultAction,
  )
where

import Network.AWS.AppSync.Types.AdditionalAuthenticationProvider
import Network.AWS.AppSync.Types.ApiCache
import Network.AWS.AppSync.Types.ApiCacheStatus
import Network.AWS.AppSync.Types.ApiCacheType
import Network.AWS.AppSync.Types.ApiCachingBehavior
import Network.AWS.AppSync.Types.ApiKey
import Network.AWS.AppSync.Types.AuthenticationType
import Network.AWS.AppSync.Types.AuthorizationConfig
import Network.AWS.AppSync.Types.AuthorizationType
import Network.AWS.AppSync.Types.AwsIamConfig
import Network.AWS.AppSync.Types.CachingConfig
import Network.AWS.AppSync.Types.CognitoUserPoolConfig
import Network.AWS.AppSync.Types.ConflictDetectionType
import Network.AWS.AppSync.Types.ConflictHandlerType
import Network.AWS.AppSync.Types.DataSource
import Network.AWS.AppSync.Types.DataSourceType
import Network.AWS.AppSync.Types.DefaultAction
import Network.AWS.AppSync.Types.DeltaSyncConfig
import Network.AWS.AppSync.Types.DynamodbDataSourceConfig
import Network.AWS.AppSync.Types.ElasticsearchDataSourceConfig
import Network.AWS.AppSync.Types.FieldLogLevel
import Network.AWS.AppSync.Types.FunctionConfiguration
import Network.AWS.AppSync.Types.GraphqlApi
import Network.AWS.AppSync.Types.HttpDataSourceConfig
import Network.AWS.AppSync.Types.LambdaAuthorizerConfig
import Network.AWS.AppSync.Types.LambdaConflictHandlerConfig
import Network.AWS.AppSync.Types.LambdaDataSourceConfig
import Network.AWS.AppSync.Types.LogConfig
import Network.AWS.AppSync.Types.OpenIDConnectConfig
import Network.AWS.AppSync.Types.OpenSearchServiceDataSourceConfig
import Network.AWS.AppSync.Types.OutputType
import Network.AWS.AppSync.Types.PipelineConfig
import Network.AWS.AppSync.Types.RdsHttpEndpointConfig
import Network.AWS.AppSync.Types.RelationalDatabaseDataSourceConfig
import Network.AWS.AppSync.Types.RelationalDatabaseSourceType
import Network.AWS.AppSync.Types.Resolver
import Network.AWS.AppSync.Types.ResolverKind
import Network.AWS.AppSync.Types.SchemaStatus
import Network.AWS.AppSync.Types.SyncConfig
import Network.AWS.AppSync.Types.Type
import Network.AWS.AppSync.Types.TypeDefinitionFormat
import Network.AWS.AppSync.Types.UserPoolConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-07-25@ of the Amazon AppSync SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "AppSync",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "appsync",
      Core._serviceSigningName = "appsync",
      Core._serviceVersion = "2017-07-25",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "AppSync",
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

-- | The API key expiration must be set to a value between 1 and 365 days
-- from creation (for @CreateApiKey@) or from update (for @UpdateApiKey@).
_ApiKeyValidityOutOfBoundsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApiKeyValidityOutOfBoundsException =
  Core._MatchServiceError
    defaultService
    "ApiKeyValidityOutOfBoundsException"
    Prelude.. Core.hasStatus 400

-- | You do not have access to perform this operation on this resource.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The API key exceeded a limit. Try your request again.
_ApiKeyLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApiKeyLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ApiKeyLimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The GraphQL API exceeded a limit. Try your request again.
_ApiLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApiLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ApiLimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The resource specified in the request was not found. Check the resource,
-- and then try again.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | The GraphQL schema is not valid.
_GraphQLSchemaException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GraphQLSchemaException =
  Core._MatchServiceError
    defaultService
    "GraphQLSchemaException"
    Prelude.. Core.hasStatus 400

-- | Another modification is in progress at this time and it must complete
-- before you can make your change.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"
    Prelude.. Core.hasStatus 409

-- | An internal AppSync error occurred. Try your request again.
_InternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Core.hasStatus 500

-- | You are not authorized to perform this operation.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Core.hasStatus 401

-- | The request is not well formed. For example, a value is invalid or a
-- required field is missing. Check the field values, and then try again.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | The request exceeded a limit. Try your request again.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 429
