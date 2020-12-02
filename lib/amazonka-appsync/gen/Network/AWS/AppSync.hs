{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS AppSync provides API actions for creating and interacting with data sources using GraphQL from your application.
module Network.AWS.AppSync
  ( -- * Service Configuration
    appSync,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** UpdateDataSource
    module Network.AWS.AppSync.UpdateDataSource,

    -- ** DeleteDataSource
    module Network.AWS.AppSync.DeleteDataSource,

    -- ** CreateType
    module Network.AWS.AppSync.CreateType,

    -- ** GetGraphqlAPI
    module Network.AWS.AppSync.GetGraphqlAPI,

    -- ** ListTagsForResource
    module Network.AWS.AppSync.ListTagsForResource,

    -- ** CreateGraphqlAPI
    module Network.AWS.AppSync.CreateGraphqlAPI,

    -- ** StartSchemaCreation
    module Network.AWS.AppSync.StartSchemaCreation,

    -- ** FlushAPICache
    module Network.AWS.AppSync.FlushAPICache,

    -- ** DeleteGraphqlAPI
    module Network.AWS.AppSync.DeleteGraphqlAPI,

    -- ** UpdateGraphqlAPI
    module Network.AWS.AppSync.UpdateGraphqlAPI,

    -- ** GetIntrospectionSchema
    module Network.AWS.AppSync.GetIntrospectionSchema,

    -- ** GetDataSource
    module Network.AWS.AppSync.GetDataSource,

    -- ** ListResolversByFunction (Paginated)
    module Network.AWS.AppSync.ListResolversByFunction,

    -- ** CreateFunction
    module Network.AWS.AppSync.CreateFunction,

    -- ** DeleteAPIKey
    module Network.AWS.AppSync.DeleteAPIKey,

    -- ** UpdateAPIKey
    module Network.AWS.AppSync.UpdateAPIKey,

    -- ** UpdateType
    module Network.AWS.AppSync.UpdateType,

    -- ** DeleteType
    module Network.AWS.AppSync.DeleteType,

    -- ** CreateDataSource
    module Network.AWS.AppSync.CreateDataSource,

    -- ** ListTypes (Paginated)
    module Network.AWS.AppSync.ListTypes,

    -- ** GetFunction
    module Network.AWS.AppSync.GetFunction,

    -- ** ListDataSources (Paginated)
    module Network.AWS.AppSync.ListDataSources,

    -- ** UpdateResolver
    module Network.AWS.AppSync.UpdateResolver,

    -- ** DeleteResolver
    module Network.AWS.AppSync.DeleteResolver,

    -- ** ListResolvers (Paginated)
    module Network.AWS.AppSync.ListResolvers,

    -- ** CreateResolver
    module Network.AWS.AppSync.CreateResolver,

    -- ** GetSchemaCreationStatus
    module Network.AWS.AppSync.GetSchemaCreationStatus,

    -- ** GetAPICache
    module Network.AWS.AppSync.GetAPICache,

    -- ** UpdateAPICache
    module Network.AWS.AppSync.UpdateAPICache,

    -- ** DeleteAPICache
    module Network.AWS.AppSync.DeleteAPICache,

    -- ** ListGraphqlAPIs (Paginated)
    module Network.AWS.AppSync.ListGraphqlAPIs,

    -- ** CreateAPICache
    module Network.AWS.AppSync.CreateAPICache,

    -- ** TagResource
    module Network.AWS.AppSync.TagResource,

    -- ** GetResolver
    module Network.AWS.AppSync.GetResolver,

    -- ** UpdateFunction
    module Network.AWS.AppSync.UpdateFunction,

    -- ** DeleteFunction
    module Network.AWS.AppSync.DeleteFunction,

    -- ** UntagResource
    module Network.AWS.AppSync.UntagResource,

    -- ** CreateAPIKey
    module Network.AWS.AppSync.CreateAPIKey,

    -- ** ListFunctions (Paginated)
    module Network.AWS.AppSync.ListFunctions,

    -- ** ListAPIKeys (Paginated)
    module Network.AWS.AppSync.ListAPIKeys,

    -- ** GetType
    module Network.AWS.AppSync.GetType,

    -- * Types

    -- ** APICacheStatus
    APICacheStatus (..),

    -- ** APICacheType
    APICacheType (..),

    -- ** APICachingBehavior
    APICachingBehavior (..),

    -- ** AuthenticationType
    AuthenticationType (..),

    -- ** AuthorizationType
    AuthorizationType (..),

    -- ** ConflictDetectionType
    ConflictDetectionType (..),

    -- ** ConflictHandlerType
    ConflictHandlerType (..),

    -- ** DataSourceType
    DataSourceType (..),

    -- ** DefaultAction
    DefaultAction (..),

    -- ** FieldLogLevel
    FieldLogLevel (..),

    -- ** OutputType
    OutputType (..),

    -- ** RelationalDatabaseSourceType
    RelationalDatabaseSourceType (..),

    -- ** ResolverKind
    ResolverKind (..),

    -- ** SchemaStatus
    SchemaStatus (..),

    -- ** TypeDefinitionFormat
    TypeDefinitionFormat (..),

    -- ** APICache
    APICache,
    apiCache,
    acTtl,
    acStatus,
    acAtRestEncryptionEnabled,
    acTransitEncryptionEnabled,
    acApiCachingBehavior,
    acType,

    -- ** APIKey
    APIKey,
    apiKey,
    akExpires,
    akDeletes,
    akId,
    akDescription,

    -- ** AWSIAMConfig
    AWSIAMConfig,
    awsIAMConfig,
    aicSigningServiceName,
    aicSigningRegion,

    -- ** AdditionalAuthenticationProvider
    AdditionalAuthenticationProvider,
    additionalAuthenticationProvider,
    aapOpenIdConnectConfig,
    aapUserPoolConfig,
    aapAuthenticationType,

    -- ** AuthorizationConfig
    AuthorizationConfig,
    authorizationConfig,
    acAwsIAMConfig,
    acAuthorizationType,

    -- ** CachingConfig
    CachingConfig,
    cachingConfig,
    ccTtl,
    ccCachingKeys,

    -- ** CognitoUserPoolConfig
    CognitoUserPoolConfig,
    cognitoUserPoolConfig,
    cupcAppIdClientRegex,
    cupcUserPoolId,
    cupcAwsRegion,

    -- ** DataSource
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

    -- ** DeltaSyncConfig
    DeltaSyncConfig,
    deltaSyncConfig,
    dscBaseTableTTL,
    dscDeltaSyncTableName,
    dscDeltaSyncTableTTL,

    -- ** DynamodbDataSourceConfig
    DynamodbDataSourceConfig,
    dynamodbDataSourceConfig,
    ddscVersioned,
    ddscUseCallerCredentials,
    ddscDeltaSyncConfig,
    ddscTableName,
    ddscAwsRegion,

    -- ** ElasticsearchDataSourceConfig
    ElasticsearchDataSourceConfig,
    elasticsearchDataSourceConfig,
    edscEndpoint,
    edscAwsRegion,

    -- ** FunctionConfiguration
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

    -- ** GraphqlAPI
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

    -- ** HTTPDataSourceConfig
    HTTPDataSourceConfig,
    hTTPDataSourceConfig,
    httpdscAuthorizationConfig,
    httpdscEndpoint,

    -- ** LambdaConflictHandlerConfig
    LambdaConflictHandlerConfig,
    lambdaConflictHandlerConfig,
    lchcLambdaConflictHandlerARN,

    -- ** LambdaDataSourceConfig
    LambdaDataSourceConfig,
    lambdaDataSourceConfig,
    ldscLambdaFunctionARN,

    -- ** LogConfig
    LogConfig,
    logConfig,
    lcExcludeVerboseContent,
    lcFieldLogLevel,
    lcCloudWatchLogsRoleARN,

    -- ** OpenIdConnectConfig
    OpenIdConnectConfig,
    openIdConnectConfig,
    oiccAuthTTL,
    oiccClientId,
    oiccIatTTL,
    oiccIssuer,

    -- ** PipelineConfig
    PipelineConfig,
    pipelineConfig,
    pcFunctions,

    -- ** RDSHTTPEndpointConfig
    RDSHTTPEndpointConfig,
    rdsHTTPEndpointConfig,
    rhttpecDbClusterIdentifier,
    rhttpecSchema,
    rhttpecDatabaseName,
    rhttpecAwsRegion,
    rhttpecAwsSecretStoreARN,

    -- ** RelationalDatabaseDataSourceConfig
    RelationalDatabaseDataSourceConfig,
    relationalDatabaseDataSourceConfig,
    rddscRelationalDatabaseSourceType,
    rddscRdsHTTPEndpointConfig,

    -- ** Resolver
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

    -- ** SyncConfig
    SyncConfig,
    syncConfig,
    scConflictHandler,
    scConflictDetection,
    scLambdaConflictHandlerConfig,

    -- ** Type
    Type,
    type',
    tArn,
    tDefinition,
    tFormat,
    tName,
    tDescription,

    -- ** UserPoolConfig
    UserPoolConfig,
    userPoolConfig,
    upcAppIdClientRegex,
    upcUserPoolId,
    upcAwsRegion,
    upcDefaultAction,
  )
where

import Network.AWS.AppSync.CreateAPICache
import Network.AWS.AppSync.CreateAPIKey
import Network.AWS.AppSync.CreateDataSource
import Network.AWS.AppSync.CreateFunction
import Network.AWS.AppSync.CreateGraphqlAPI
import Network.AWS.AppSync.CreateResolver
import Network.AWS.AppSync.CreateType
import Network.AWS.AppSync.DeleteAPICache
import Network.AWS.AppSync.DeleteAPIKey
import Network.AWS.AppSync.DeleteDataSource
import Network.AWS.AppSync.DeleteFunction
import Network.AWS.AppSync.DeleteGraphqlAPI
import Network.AWS.AppSync.DeleteResolver
import Network.AWS.AppSync.DeleteType
import Network.AWS.AppSync.FlushAPICache
import Network.AWS.AppSync.GetAPICache
import Network.AWS.AppSync.GetDataSource
import Network.AWS.AppSync.GetFunction
import Network.AWS.AppSync.GetGraphqlAPI
import Network.AWS.AppSync.GetIntrospectionSchema
import Network.AWS.AppSync.GetResolver
import Network.AWS.AppSync.GetSchemaCreationStatus
import Network.AWS.AppSync.GetType
import Network.AWS.AppSync.ListAPIKeys
import Network.AWS.AppSync.ListDataSources
import Network.AWS.AppSync.ListFunctions
import Network.AWS.AppSync.ListGraphqlAPIs
import Network.AWS.AppSync.ListResolvers
import Network.AWS.AppSync.ListResolversByFunction
import Network.AWS.AppSync.ListTagsForResource
import Network.AWS.AppSync.ListTypes
import Network.AWS.AppSync.StartSchemaCreation
import Network.AWS.AppSync.TagResource
import Network.AWS.AppSync.Types
import Network.AWS.AppSync.UntagResource
import Network.AWS.AppSync.UpdateAPICache
import Network.AWS.AppSync.UpdateAPIKey
import Network.AWS.AppSync.UpdateDataSource
import Network.AWS.AppSync.UpdateFunction
import Network.AWS.AppSync.UpdateGraphqlAPI
import Network.AWS.AppSync.UpdateResolver
import Network.AWS.AppSync.UpdateType
import Network.AWS.AppSync.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'AppSync'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
