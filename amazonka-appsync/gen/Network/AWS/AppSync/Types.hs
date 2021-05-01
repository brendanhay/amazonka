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
    _NotFoundException,
    _BadRequestException,
    _UnauthorizedException,
    _ConcurrentModificationException,
    _GraphQLSchemaException,
    _AccessDeniedException,
    _LimitExceededException,
    _ApiLimitExceededException,
    _ApiKeyLimitExceededException,
    _InternalFailureException,

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
    additionalAuthenticationProvider_userPoolConfig,
    additionalAuthenticationProvider_authenticationType,

    -- * ApiCache
    ApiCache (..),
    newApiCache,
    apiCache_status,
    apiCache_atRestEncryptionEnabled,
    apiCache_ttl,
    apiCache_type,
    apiCache_transitEncryptionEnabled,
    apiCache_apiCachingBehavior,

    -- * ApiKey
    ApiKey (..),
    newApiKey,
    apiKey_id,
    apiKey_deletes,
    apiKey_description,
    apiKey_expires,

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

    -- * DeltaSyncConfig
    DeltaSyncConfig (..),
    newDeltaSyncConfig,
    deltaSyncConfig_baseTableTTL,
    deltaSyncConfig_deltaSyncTableName,
    deltaSyncConfig_deltaSyncTableTTL,

    -- * DynamodbDataSourceConfig
    DynamodbDataSourceConfig (..),
    newDynamodbDataSourceConfig,
    dynamodbDataSourceConfig_useCallerCredentials,
    dynamodbDataSourceConfig_versioned,
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
    functionConfiguration_responseMappingTemplate,
    functionConfiguration_functionVersion,
    functionConfiguration_syncConfig,
    functionConfiguration_dataSourceName,
    functionConfiguration_name,
    functionConfiguration_functionId,
    functionConfiguration_functionArn,
    functionConfiguration_description,
    functionConfiguration_requestMappingTemplate,

    -- * GraphqlApi
    GraphqlApi (..),
    newGraphqlApi,
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

    -- * HttpDataSourceConfig
    HttpDataSourceConfig (..),
    newHttpDataSourceConfig,
    httpDataSourceConfig_authorizationConfig,
    httpDataSourceConfig_endpoint,

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
    openIDConnectConfig_clientId,
    openIDConnectConfig_authTTL,
    openIDConnectConfig_iatTTL,
    openIDConnectConfig_issuer,

    -- * PipelineConfig
    PipelineConfig (..),
    newPipelineConfig,
    pipelineConfig_functions,

    -- * RdsHttpEndpointConfig
    RdsHttpEndpointConfig (..),
    newRdsHttpEndpointConfig,
    rdsHttpEndpointConfig_awsSecretStoreArn,
    rdsHttpEndpointConfig_schema,
    rdsHttpEndpointConfig_dbClusterIdentifier,
    rdsHttpEndpointConfig_awsRegion,
    rdsHttpEndpointConfig_databaseName,

    -- * RelationalDatabaseDataSourceConfig
    RelationalDatabaseDataSourceConfig (..),
    newRelationalDatabaseDataSourceConfig,
    relationalDatabaseDataSourceConfig_rdsHttpEndpointConfig,
    relationalDatabaseDataSourceConfig_relationalDatabaseSourceType,

    -- * Resolver
    Resolver (..),
    newResolver,
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

    -- * SyncConfig
    SyncConfig (..),
    newSyncConfig,
    syncConfig_conflictHandler,
    syncConfig_lambdaConflictHandlerConfig,
    syncConfig_conflictDetection,

    -- * Type
    Type (..),
    newType,
    type_format,
    type_arn,
    type_name,
    type_description,
    type_definition,

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
import Network.AWS.AppSync.Types.LambdaConflictHandlerConfig
import Network.AWS.AppSync.Types.LambdaDataSourceConfig
import Network.AWS.AppSync.Types.LogConfig
import Network.AWS.AppSync.Types.OpenIDConnectConfig
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-07-25@ of the Amazon AppSync SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "AppSync",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "appsync",
      Prelude._svcVersion = "2017-07-25",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseJSONError "AppSync",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The API key expiration must be set to a value between 1 and 365 days
-- from creation (for @CreateApiKey@) or from update (for @UpdateApiKey@).
_ApiKeyValidityOutOfBoundsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ApiKeyValidityOutOfBoundsException =
  Prelude._MatchServiceError
    defaultService
    "ApiKeyValidityOutOfBoundsException"
    Prelude.. Prelude.hasStatus 400

-- | The resource specified in the request was not found. Check the resource,
-- and then try again.
_NotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotFoundException =
  Prelude._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Prelude.hasStatus 404

-- | The request is not well formed. For example, a value is invalid or a
-- required field is missing. Check the field values, and then try again.
_BadRequestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BadRequestException =
  Prelude._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Prelude.hasStatus 400

-- | You are not authorized to perform this operation.
_UnauthorizedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnauthorizedException =
  Prelude._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Prelude.hasStatus 401

-- | Another modification is in progress at this time and it must complete
-- before you can make your change.
_ConcurrentModificationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConcurrentModificationException =
  Prelude._MatchServiceError
    defaultService
    "ConcurrentModificationException"
    Prelude.. Prelude.hasStatus 409

-- | The GraphQL schema is not valid.
_GraphQLSchemaException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_GraphQLSchemaException =
  Prelude._MatchServiceError
    defaultService
    "GraphQLSchemaException"
    Prelude.. Prelude.hasStatus 400

-- | You do not have access to perform this operation on this resource.
_AccessDeniedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AccessDeniedException =
  Prelude._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Prelude.hasStatus 403

-- | The request exceeded a limit. Try your request again.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Prelude.hasStatus 429

-- | The GraphQL API exceeded a limit. Try your request again.
_ApiLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ApiLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "ApiLimitExceededException"
    Prelude.. Prelude.hasStatus 400

-- | The API key exceeded a limit. Try your request again.
_ApiKeyLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ApiKeyLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "ApiKeyLimitExceededException"
    Prelude.. Prelude.hasStatus 400

-- | An internal AWS AppSync error occurred. Try your request again.
_InternalFailureException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalFailureException =
  Prelude._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Prelude.hasStatus 500
