{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppSync.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _GraphQLSchemaException,
    _ConcurrentModificationException,
    _AccessDeniedException,
    _UnauthorizedException,
    _ApiKeyValidityOutOfBoundsException,
    _ApiLimitExceededException,
    _NotFoundException,
    _ApiKeyLimitExceededException,
    _LimitExceededException,
    _BadRequestException,
    _InternalFailureException,

    -- * ApiCacheStatus
    ApiCacheStatus (..),

    -- * ApiCacheType
    ApiCacheType (..),

    -- * ApiCachingBehavior
    ApiCachingBehavior (..),

    -- * AssociationStatus
    AssociationStatus (..),

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

    -- * RuntimeName
    RuntimeName (..),

    -- * SchemaStatus
    SchemaStatus (..),

    -- * TypeDefinitionFormat
    TypeDefinitionFormat (..),

    -- * AdditionalAuthenticationProvider
    AdditionalAuthenticationProvider (..),
    newAdditionalAuthenticationProvider,
    additionalAuthenticationProvider_authenticationType,
    additionalAuthenticationProvider_openIDConnectConfig,
    additionalAuthenticationProvider_userPoolConfig,
    additionalAuthenticationProvider_lambdaAuthorizerConfig,

    -- * ApiAssociation
    ApiAssociation (..),
    newApiAssociation,
    apiAssociation_domainName,
    apiAssociation_apiId,
    apiAssociation_associationStatus,
    apiAssociation_deploymentDetail,

    -- * ApiCache
    ApiCache (..),
    newApiCache,
    apiCache_transitEncryptionEnabled,
    apiCache_type,
    apiCache_apiCachingBehavior,
    apiCache_ttl,
    apiCache_status,
    apiCache_atRestEncryptionEnabled,

    -- * ApiKey
    ApiKey (..),
    newApiKey,
    apiKey_description,
    apiKey_id,
    apiKey_expires,
    apiKey_deletes,

    -- * AppSyncRuntime
    AppSyncRuntime (..),
    newAppSyncRuntime,
    appSyncRuntime_name,
    appSyncRuntime_runtimeVersion,

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
    cachingConfig_cachingKeys,
    cachingConfig_ttl,

    -- * CodeError
    CodeError (..),
    newCodeError,
    codeError_location,
    codeError_errorType,
    codeError_value,

    -- * CodeErrorLocation
    CodeErrorLocation (..),
    newCodeErrorLocation,
    codeErrorLocation_line,
    codeErrorLocation_span,
    codeErrorLocation_column,

    -- * CognitoUserPoolConfig
    CognitoUserPoolConfig (..),
    newCognitoUserPoolConfig,
    cognitoUserPoolConfig_appIdClientRegex,
    cognitoUserPoolConfig_userPoolId,
    cognitoUserPoolConfig_awsRegion,

    -- * DataSource
    DataSource (..),
    newDataSource,
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

    -- * DeltaSyncConfig
    DeltaSyncConfig (..),
    newDeltaSyncConfig,
    deltaSyncConfig_baseTableTTL,
    deltaSyncConfig_deltaSyncTableName,
    deltaSyncConfig_deltaSyncTableTTL,

    -- * DomainNameConfig
    DomainNameConfig (..),
    newDomainNameConfig,
    domainNameConfig_hostedZoneId,
    domainNameConfig_domainName,
    domainNameConfig_description,
    domainNameConfig_appsyncDomainName,
    domainNameConfig_certificateArn,

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

    -- * ErrorDetail
    ErrorDetail (..),
    newErrorDetail,
    errorDetail_message,

    -- * EvaluateCodeErrorDetail
    EvaluateCodeErrorDetail (..),
    newEvaluateCodeErrorDetail,
    evaluateCodeErrorDetail_message,
    evaluateCodeErrorDetail_codeErrors,

    -- * FunctionConfiguration
    FunctionConfiguration (..),
    newFunctionConfiguration,
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

    -- * GraphqlApi
    GraphqlApi (..),
    newGraphqlApi,
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

    -- * HttpDataSourceConfig
    HttpDataSourceConfig (..),
    newHttpDataSourceConfig,
    httpDataSourceConfig_endpoint,
    httpDataSourceConfig_authorizationConfig,

    -- * LambdaAuthorizerConfig
    LambdaAuthorizerConfig (..),
    newLambdaAuthorizerConfig,
    lambdaAuthorizerConfig_authorizerResultTtlInSeconds,
    lambdaAuthorizerConfig_identityValidationExpression,
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
    openIDConnectConfig_iatTTL,
    openIDConnectConfig_clientId,
    openIDConnectConfig_authTTL,
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
    rdsHttpEndpointConfig_databaseName,
    rdsHttpEndpointConfig_dbClusterIdentifier,
    rdsHttpEndpointConfig_awsRegion,
    rdsHttpEndpointConfig_schema,
    rdsHttpEndpointConfig_awsSecretStoreArn,

    -- * RelationalDatabaseDataSourceConfig
    RelationalDatabaseDataSourceConfig (..),
    newRelationalDatabaseDataSourceConfig,
    relationalDatabaseDataSourceConfig_relationalDatabaseSourceType,
    relationalDatabaseDataSourceConfig_rdsHttpEndpointConfig,

    -- * Resolver
    Resolver (..),
    newResolver,
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

    -- * SyncConfig
    SyncConfig (..),
    newSyncConfig,
    syncConfig_conflictHandler,
    syncConfig_conflictDetection,
    syncConfig_lambdaConflictHandlerConfig,

    -- * Type
    Type (..),
    newType,
    type_name,
    type_format,
    type_arn,
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

import Amazonka.AppSync.Types.AdditionalAuthenticationProvider
import Amazonka.AppSync.Types.ApiAssociation
import Amazonka.AppSync.Types.ApiCache
import Amazonka.AppSync.Types.ApiCacheStatus
import Amazonka.AppSync.Types.ApiCacheType
import Amazonka.AppSync.Types.ApiCachingBehavior
import Amazonka.AppSync.Types.ApiKey
import Amazonka.AppSync.Types.AppSyncRuntime
import Amazonka.AppSync.Types.AssociationStatus
import Amazonka.AppSync.Types.AuthenticationType
import Amazonka.AppSync.Types.AuthorizationConfig
import Amazonka.AppSync.Types.AuthorizationType
import Amazonka.AppSync.Types.AwsIamConfig
import Amazonka.AppSync.Types.CachingConfig
import Amazonka.AppSync.Types.CodeError
import Amazonka.AppSync.Types.CodeErrorLocation
import Amazonka.AppSync.Types.CognitoUserPoolConfig
import Amazonka.AppSync.Types.ConflictDetectionType
import Amazonka.AppSync.Types.ConflictHandlerType
import Amazonka.AppSync.Types.DataSource
import Amazonka.AppSync.Types.DataSourceType
import Amazonka.AppSync.Types.DefaultAction
import Amazonka.AppSync.Types.DeltaSyncConfig
import Amazonka.AppSync.Types.DomainNameConfig
import Amazonka.AppSync.Types.DynamodbDataSourceConfig
import Amazonka.AppSync.Types.ElasticsearchDataSourceConfig
import Amazonka.AppSync.Types.ErrorDetail
import Amazonka.AppSync.Types.EvaluateCodeErrorDetail
import Amazonka.AppSync.Types.FieldLogLevel
import Amazonka.AppSync.Types.FunctionConfiguration
import Amazonka.AppSync.Types.GraphqlApi
import Amazonka.AppSync.Types.HttpDataSourceConfig
import Amazonka.AppSync.Types.LambdaAuthorizerConfig
import Amazonka.AppSync.Types.LambdaConflictHandlerConfig
import Amazonka.AppSync.Types.LambdaDataSourceConfig
import Amazonka.AppSync.Types.LogConfig
import Amazonka.AppSync.Types.OpenIDConnectConfig
import Amazonka.AppSync.Types.OpenSearchServiceDataSourceConfig
import Amazonka.AppSync.Types.OutputType
import Amazonka.AppSync.Types.PipelineConfig
import Amazonka.AppSync.Types.RdsHttpEndpointConfig
import Amazonka.AppSync.Types.RelationalDatabaseDataSourceConfig
import Amazonka.AppSync.Types.RelationalDatabaseSourceType
import Amazonka.AppSync.Types.Resolver
import Amazonka.AppSync.Types.ResolverKind
import Amazonka.AppSync.Types.RuntimeName
import Amazonka.AppSync.Types.SchemaStatus
import Amazonka.AppSync.Types.SyncConfig
import Amazonka.AppSync.Types.Type
import Amazonka.AppSync.Types.TypeDefinitionFormat
import Amazonka.AppSync.Types.UserPoolConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-07-25@ of the Amazon AppSync SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "AppSync",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "appsync",
      Core.signingName = "appsync",
      Core.version = "2017-07-25",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "AppSync",
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

-- | You don\'t have access to perform this operation on this resource.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | You aren\'t authorized to perform this operation.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Core.hasStatus 401

-- | The API key expiration must be set to a value between 1 and 365 days
-- from creation (for @CreateApiKey@) or from update (for @UpdateApiKey@).
_ApiKeyValidityOutOfBoundsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApiKeyValidityOutOfBoundsException =
  Core._MatchServiceError
    defaultService
    "ApiKeyValidityOutOfBoundsException"
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

-- | The API key exceeded a limit. Try your request again.
_ApiKeyLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApiKeyLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ApiKeyLimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The request exceeded a limit. Try your request again.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 429

-- | The request is not well formed. For example, a value is invalid or a
-- required field is missing. Check the field values, and then try again.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | An internal AppSync error occurred. Try your request again.
_InternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Core.hasStatus 500
