-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types
  ( -- * Service configuration
    appSyncService,

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
    APICache (..),
    mkAPICache,
    acTtl,
    acStatus,
    acAtRestEncryptionEnabled,
    acTransitEncryptionEnabled,
    acApiCachingBehavior,
    acType,

    -- * APIKey
    APIKey (..),
    mkAPIKey,
    akExpires,
    akDeletes,
    akId,
    akDescription,

    -- * AWSIAMConfig
    AWSIAMConfig (..),
    mkAWSIAMConfig,
    aicSigningServiceName,
    aicSigningRegion,

    -- * AdditionalAuthenticationProvider
    AdditionalAuthenticationProvider (..),
    mkAdditionalAuthenticationProvider,
    aapOpenIdConnectConfig,
    aapUserPoolConfig,
    aapAuthenticationType,

    -- * AuthorizationConfig
    AuthorizationConfig (..),
    mkAuthorizationConfig,
    acAwsIAMConfig,
    acAuthorizationType,

    -- * CachingConfig
    CachingConfig (..),
    mkCachingConfig,
    ccTtl,
    ccCachingKeys,

    -- * CognitoUserPoolConfig
    CognitoUserPoolConfig (..),
    mkCognitoUserPoolConfig,
    cupcUserPoolId,
    cupcAwsRegion,
    cupcAppIdClientRegex,

    -- * DataSource
    DataSource (..),
    mkDataSource,
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
    DeltaSyncConfig (..),
    mkDeltaSyncConfig,
    dscBaseTableTTL,
    dscDeltaSyncTableName,
    dscDeltaSyncTableTTL,

    -- * DynamodbDataSourceConfig
    DynamodbDataSourceConfig (..),
    mkDynamodbDataSourceConfig,
    ddscVersioned,
    ddscUseCallerCredentials,
    ddscDeltaSyncConfig,
    ddscAwsRegion,
    ddscTableName,

    -- * ElasticsearchDataSourceConfig
    ElasticsearchDataSourceConfig (..),
    mkElasticsearchDataSourceConfig,
    edscAwsRegion,
    edscEndpoint,

    -- * FunctionConfiguration
    FunctionConfiguration (..),
    mkFunctionConfiguration,
    fcFunctionARN,
    fcDataSourceName,
    fcRequestMappingTemplate,
    fcName,
    fcFunctionId,
    fcResponseMappingTemplate,
    fcFunctionVersion,
    fcDescription,

    -- * GraphqlAPI
    GraphqlAPI (..),
    mkGraphqlAPI,
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
    HTTPDataSourceConfig (..),
    mkHTTPDataSourceConfig,
    httpdscAuthorizationConfig,
    httpdscEndpoint,

    -- * LambdaConflictHandlerConfig
    LambdaConflictHandlerConfig (..),
    mkLambdaConflictHandlerConfig,
    lchcLambdaConflictHandlerARN,

    -- * LambdaDataSourceConfig
    LambdaDataSourceConfig (..),
    mkLambdaDataSourceConfig,
    ldscLambdaFunctionARN,

    -- * LogConfig
    LogConfig (..),
    mkLogConfig,
    lcExcludeVerboseContent,
    lcFieldLogLevel,
    lcCloudWatchLogsRoleARN,

    -- * OpenIdConnectConfig
    OpenIdConnectConfig (..),
    mkOpenIdConnectConfig,
    oiccAuthTTL,
    oiccClientId,
    oiccIatTTL,
    oiccIssuer,

    -- * PipelineConfig
    PipelineConfig (..),
    mkPipelineConfig,
    pcFunctions,

    -- * RDSHTTPEndpointConfig
    RDSHTTPEndpointConfig (..),
    mkRDSHTTPEndpointConfig,
    rhttpecDbClusterIdentifier,
    rhttpecSchema,
    rhttpecDatabaseName,
    rhttpecAwsRegion,
    rhttpecAwsSecretStoreARN,

    -- * RelationalDatabaseDataSourceConfig
    RelationalDatabaseDataSourceConfig (..),
    mkRelationalDatabaseDataSourceConfig,
    rddscRelationalDatabaseSourceType,
    rddscRdsHTTPEndpointConfig,

    -- * Resolver
    Resolver (..),
    mkResolver,
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
    SyncConfig (..),
    mkSyncConfig,
    scConflictHandler,
    scConflictDetection,
    scLambdaConflictHandlerConfig,

    -- * Type
    Type (..),
    mkType,
    tArn,
    tDefinition,
    tFormat,
    tName,
    tDescription,

    -- * UserPoolConfig
    UserPoolConfig (..),
    mkUserPoolConfig,
    upcUserPoolId,
    upcDefaultAction,
    upcAwsRegion,
    upcAppIdClientRegex,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-07-25@ of the Amazon AppSync SDK configuration.
appSyncService :: Lude.Service
appSyncService =
  Lude.Service
    { Lude._svcAbbrev = "AppSync",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "appsync",
      Lude._svcVersion = "2017-07-25",
      Lude._svcEndpoint = Lude.defaultEndpoint appSyncService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "AppSync",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
