{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types
  ( -- * Service Configuration
    appSync,

    -- * Errors

    -- * APICacheStatus
    APICacheStatus (..),

    -- * APICacheType
    APICacheType (..),

    -- * APICachingBehavior
    APICachingBehavior (..),

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

    -- * APICache
    APICache,
    apiCache,
    acTtl,
    acStatus,
    acAtRestEncryptionEnabled,
    acTransitEncryptionEnabled,
    acApiCachingBehavior,
    acType,

    -- * APIKey
    APIKey,
    apiKey,
    akExpires,
    akDeletes,
    akId,
    akDescription,

    -- * AWSIAMConfig
    AWSIAMConfig,
    awsIAMConfig,
    aicSigningServiceName,
    aicSigningRegion,

    -- * AdditionalAuthenticationProvider
    AdditionalAuthenticationProvider,
    additionalAuthenticationProvider,
    aapOpenIdConnectConfig,
    aapUserPoolConfig,
    aapAuthenticationType,

    -- * AuthorizationConfig
    AuthorizationConfig,
    authorizationConfig,
    acAwsIAMConfig,
    acAuthorizationType,

    -- * CachingConfig
    CachingConfig,
    cachingConfig,
    ccTtl,
    ccCachingKeys,

    -- * CognitoUserPoolConfig
    CognitoUserPoolConfig,
    cognitoUserPoolConfig,
    cupcAppIdClientRegex,
    cupcUserPoolId,
    cupcAwsRegion,

    -- * DataSource
    DataSource,
    dataSource,
    dsServiceRoleARN,
    dsRelationalDatabaseConfig,
    dsDataSourceARN,
    dsDynamodbConfig,
    dsName,
    dsHttpConfig,
    dsLambdaConfig,
    dsType,
    dsDescription,
    dsElasticsearchConfig,

    -- * DeltaSyncConfig
    DeltaSyncConfig,
    deltaSyncConfig,
    dscBaseTableTTL,
    dscDeltaSyncTableName,
    dscDeltaSyncTableTTL,

    -- * DynamodbDataSourceConfig
    DynamodbDataSourceConfig,
    dynamodbDataSourceConfig,
    ddscVersioned,
    ddscUseCallerCredentials,
    ddscDeltaSyncConfig,
    ddscTableName,
    ddscAwsRegion,

    -- * ElasticsearchDataSourceConfig
    ElasticsearchDataSourceConfig,
    elasticsearchDataSourceConfig,
    edscEndpoint,
    edscAwsRegion,

    -- * FunctionConfiguration
    FunctionConfiguration,
    functionConfiguration,
    fcFunctionARN,
    fcDataSourceName,
    fcRequestMappingTemplate,
    fcName,
    fcFunctionId,
    fcResponseMappingTemplate,
    fcFunctionVersion,
    fcDescription,

    -- * GraphqlAPI
    GraphqlAPI,
    graphqlAPI,
    gaXrayEnabled,
    gaArn,
    gaApiId,
    gaUris,
    gaOpenIdConnectConfig,
    gaWafWebACLARN,
    gaAdditionalAuthenticationProviders,
    gaName,
    gaUserPoolConfig,
    gaAuthenticationType,
    gaLogConfig,
    gaTags,

    -- * HTTPDataSourceConfig
    HTTPDataSourceConfig,
    hTTPDataSourceConfig,
    httpdscAuthorizationConfig,
    httpdscEndpoint,

    -- * LambdaConflictHandlerConfig
    LambdaConflictHandlerConfig,
    lambdaConflictHandlerConfig,
    lchcLambdaConflictHandlerARN,

    -- * LambdaDataSourceConfig
    LambdaDataSourceConfig,
    lambdaDataSourceConfig,
    ldscLambdaFunctionARN,

    -- * LogConfig
    LogConfig,
    logConfig,
    lcExcludeVerboseContent,
    lcFieldLogLevel,
    lcCloudWatchLogsRoleARN,

    -- * OpenIdConnectConfig
    OpenIdConnectConfig,
    openIdConnectConfig,
    oiccAuthTTL,
    oiccClientId,
    oiccIatTTL,
    oiccIssuer,

    -- * PipelineConfig
    PipelineConfig,
    pipelineConfig,
    pcFunctions,

    -- * RDSHTTPEndpointConfig
    RDSHTTPEndpointConfig,
    rdsHTTPEndpointConfig,
    rhttpecDbClusterIdentifier,
    rhttpecSchema,
    rhttpecDatabaseName,
    rhttpecAwsRegion,
    rhttpecAwsSecretStoreARN,

    -- * RelationalDatabaseDataSourceConfig
    RelationalDatabaseDataSourceConfig,
    relationalDatabaseDataSourceConfig,
    rddscRelationalDatabaseSourceType,
    rddscRdsHTTPEndpointConfig,

    -- * Resolver
    Resolver,
    resolver,
    rTypeName,
    rDataSourceName,
    rRequestMappingTemplate,
    rKind,
    rResolverARN,
    rCachingConfig,
    rResponseMappingTemplate,
    rFieldName,
    rSyncConfig,
    rPipelineConfig,

    -- * SyncConfig
    SyncConfig,
    syncConfig,
    scConflictHandler,
    scConflictDetection,
    scLambdaConflictHandlerConfig,

    -- * Type
    Type,
    type',
    tArn,
    tDefinition,
    tFormat,
    tName,
    tDescription,

    -- * UserPoolConfig
    UserPoolConfig,
    userPoolConfig,
    upcAppIdClientRegex,
    upcUserPoolId,
    upcAwsRegion,
    upcDefaultAction,
  )
where

import Network.AWS.AppSync.Types.APICache
import Network.AWS.AppSync.Types.APICacheStatus
import Network.AWS.AppSync.Types.APICacheType
import Network.AWS.AppSync.Types.APICachingBehavior
import Network.AWS.AppSync.Types.APIKey
import Network.AWS.AppSync.Types.AWSIAMConfig
import Network.AWS.AppSync.Types.AdditionalAuthenticationProvider
import Network.AWS.AppSync.Types.AuthenticationType
import Network.AWS.AppSync.Types.AuthorizationConfig
import Network.AWS.AppSync.Types.AuthorizationType
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
import Network.AWS.AppSync.Types.GraphqlAPI
import Network.AWS.AppSync.Types.HTTPDataSourceConfig
import Network.AWS.AppSync.Types.LambdaConflictHandlerConfig
import Network.AWS.AppSync.Types.LambdaDataSourceConfig
import Network.AWS.AppSync.Types.LogConfig
import Network.AWS.AppSync.Types.OpenIdConnectConfig
import Network.AWS.AppSync.Types.OutputType
import Network.AWS.AppSync.Types.PipelineConfig
import Network.AWS.AppSync.Types.RDSHTTPEndpointConfig
import Network.AWS.AppSync.Types.RelationalDatabaseDataSourceConfig
import Network.AWS.AppSync.Types.RelationalDatabaseSourceType
import Network.AWS.AppSync.Types.Resolver
import Network.AWS.AppSync.Types.ResolverKind
import Network.AWS.AppSync.Types.SchemaStatus
import Network.AWS.AppSync.Types.SyncConfig
import Network.AWS.AppSync.Types.Type
import Network.AWS.AppSync.Types.TypeDefinitionFormat
import Network.AWS.AppSync.Types.UserPoolConfig
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-07-25@ of the Amazon AppSync SDK configuration.
appSync :: Service
appSync =
  Service
    { _svcAbbrev = "AppSync",
      _svcSigner = v4,
      _svcPrefix = "appsync",
      _svcVersion = "2017-07-25",
      _svcEndpoint = defaultEndpoint appSync,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "AppSync",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
