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
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** ApiKeyValidityOutOfBoundsException
    _ApiKeyValidityOutOfBoundsException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ApiKeyLimitExceededException
    _ApiKeyLimitExceededException,

    -- ** ApiLimitExceededException
    _ApiLimitExceededException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** GraphQLSchemaException
    _GraphQLSchemaException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** LimitExceededException
    _LimitExceededException,

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

    -- ** GetGraphqlApi
    module Network.AWS.AppSync.GetGraphqlApi,

    -- ** ListTagsForResource
    module Network.AWS.AppSync.ListTagsForResource,

    -- ** CreateGraphqlApi
    module Network.AWS.AppSync.CreateGraphqlApi,

    -- ** StartSchemaCreation
    module Network.AWS.AppSync.StartSchemaCreation,

    -- ** FlushApiCache
    module Network.AWS.AppSync.FlushApiCache,

    -- ** DeleteGraphqlApi
    module Network.AWS.AppSync.DeleteGraphqlApi,

    -- ** UpdateGraphqlApi
    module Network.AWS.AppSync.UpdateGraphqlApi,

    -- ** GetIntrospectionSchema
    module Network.AWS.AppSync.GetIntrospectionSchema,

    -- ** GetDataSource
    module Network.AWS.AppSync.GetDataSource,

    -- ** ListResolversByFunction (Paginated)
    module Network.AWS.AppSync.ListResolversByFunction,

    -- ** CreateFunction
    module Network.AWS.AppSync.CreateFunction,

    -- ** DeleteApiKey
    module Network.AWS.AppSync.DeleteApiKey,

    -- ** UpdateApiKey
    module Network.AWS.AppSync.UpdateApiKey,

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

    -- ** GetApiCache
    module Network.AWS.AppSync.GetApiCache,

    -- ** UpdateApiCache
    module Network.AWS.AppSync.UpdateApiCache,

    -- ** DeleteApiCache
    module Network.AWS.AppSync.DeleteApiCache,

    -- ** ListGraphqlApis (Paginated)
    module Network.AWS.AppSync.ListGraphqlApis,

    -- ** CreateApiCache
    module Network.AWS.AppSync.CreateApiCache,

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

    -- ** CreateApiKey
    module Network.AWS.AppSync.CreateApiKey,

    -- ** ListFunctions (Paginated)
    module Network.AWS.AppSync.ListFunctions,

    -- ** ListApiKeys (Paginated)
    module Network.AWS.AppSync.ListApiKeys,

    -- ** GetType
    module Network.AWS.AppSync.GetType,

    -- * Types

    -- ** MappingTemplate
    MappingTemplate (..),

    -- ** ElasticsearchDataSourceConfig
    ElasticsearchDataSourceConfig (..),
    mkElasticsearchDataSourceConfig,
    edscEndpoint,
    edscAwsRegion,

    -- ** ApiCache
    ApiCache (..),
    mkApiCache,
    acApiCachingBehavior,
    acAtRestEncryptionEnabled,
    acStatus,
    acTransitEncryptionEnabled,
    acTtl,
    acType,

    -- ** PaginationToken
    PaginationToken (..),

    -- ** ApiKey
    ApiKey (..),
    mkApiKey,
    akDeletes,
    akDescription,
    akExpires,
    akId,

    -- ** OutputType
    OutputType (..),

    -- ** HttpDataSourceConfig
    HttpDataSourceConfig (..),
    mkHttpDataSourceConfig,
    hdscAuthorizationConfig,
    hdscEndpoint,

    -- ** DataSourceType
    DataSourceType (..),

    -- ** ApiCachingBehavior
    ApiCachingBehavior (..),

    -- ** ResourceName
    ResourceName (..),

    -- ** String
    String (..),

    -- ** SchemaStatus
    SchemaStatus (..),

    -- ** OpenIDConnectConfig
    OpenIDConnectConfig (..),
    mkOpenIDConnectConfig,
    oidccIssuer,
    oidccAuthTTL,
    oidccClientId,
    oidccIatTTL,

    -- ** CognitoUserPoolConfig
    CognitoUserPoolConfig (..),
    mkCognitoUserPoolConfig,
    cupcUserPoolId,
    cupcAwsRegion,
    cupcAppIdClientRegex,

    -- ** LambdaDataSourceConfig
    LambdaDataSourceConfig (..),
    mkLambdaDataSourceConfig,
    ldscLambdaFunctionArn,

    -- ** FieldLogLevel
    FieldLogLevel (..),

    -- ** ApiCacheStatus
    ApiCacheStatus (..),

    -- ** CachingConfig
    CachingConfig (..),
    mkCachingConfig,
    ccCachingKeys,
    ccTtl,

    -- ** RelationalDatabaseSourceType
    RelationalDatabaseSourceType (..),

    -- ** TypeDefinitionFormat
    TypeDefinitionFormat (..),

    -- ** DynamodbDataSourceConfig
    DynamodbDataSourceConfig (..),
    mkDynamodbDataSourceConfig,
    ddscTableName,
    ddscAwsRegion,
    ddscDeltaSyncConfig,
    ddscUseCallerCredentials,
    ddscVersioned,

    -- ** AuthorizationConfig
    AuthorizationConfig (..),
    mkAuthorizationConfig,
    acAuthorizationType,
    acAwsIamConfig,

    -- ** TagValue
    TagValue (..),

    -- ** ApiCacheType
    ApiCacheType (..),

    -- ** RdsHttpEndpointConfig
    RdsHttpEndpointConfig (..),
    mkRdsHttpEndpointConfig,
    rhecAwsRegion,
    rhecAwsSecretStoreArn,
    rhecDatabaseName,
    rhecDbClusterIdentifier,
    rhecSchema,

    -- ** UserPoolConfig
    UserPoolConfig (..),
    mkUserPoolConfig,
    upcUserPoolId,
    upcAwsRegion,
    upcDefaultAction,
    upcAppIdClientRegex,

    -- ** ResourceArn
    ResourceArn (..),

    -- ** AwsIamConfig
    AwsIamConfig (..),
    mkAwsIamConfig,
    aicSigningRegion,
    aicSigningServiceName,

    -- ** FunctionConfiguration
    FunctionConfiguration (..),
    mkFunctionConfiguration,
    fcDataSourceName,
    fcDescription,
    fcFunctionArn,
    fcFunctionId,
    fcFunctionVersion,
    fcName,
    fcRequestMappingTemplate,
    fcResponseMappingTemplate,

    -- ** DefaultAction
    DefaultAction (..),

    -- ** DataSource
    DataSource (..),
    mkDataSource,
    dsDataSourceArn,
    dsDescription,
    dsDynamodbConfig,
    dsElasticsearchConfig,
    dsHttpConfig,
    dsLambdaConfig,
    dsName,
    dsRelationalDatabaseConfig,
    dsServiceRoleArn,
    dsType,

    -- ** SyncConfig
    SyncConfig (..),
    mkSyncConfig,
    scConflictDetection,
    scConflictHandler,
    scLambdaConflictHandlerConfig,

    -- ** DeltaSyncConfig
    DeltaSyncConfig (..),
    mkDeltaSyncConfig,
    dscBaseTableTTL,
    dscDeltaSyncTableName,
    dscDeltaSyncTableTTL,

    -- ** TagKey
    TagKey (..),

    -- ** AuthenticationType
    AuthenticationType (..),

    -- ** Type
    Type (..),
    mkType,
    tArn,
    tDefinition,
    tDescription,
    tFormat,
    tName,

    -- ** PipelineConfig
    PipelineConfig (..),
    mkPipelineConfig,
    pcFunctions,

    -- ** AdditionalAuthenticationProvider
    AdditionalAuthenticationProvider (..),
    mkAdditionalAuthenticationProvider,
    aapAuthenticationType,
    aapOpenIDConnectConfig,
    aapUserPoolConfig,

    -- ** AuthorizationType
    AuthorizationType (..),

    -- ** LambdaConflictHandlerConfig
    LambdaConflictHandlerConfig (..),
    mkLambdaConflictHandlerConfig,
    lchcLambdaConflictHandlerArn,

    -- ** Resolver
    Resolver (..),
    mkResolver,
    rCachingConfig,
    rDataSourceName,
    rFieldName,
    rKind,
    rPipelineConfig,
    rRequestMappingTemplate,
    rResolverArn,
    rResponseMappingTemplate,
    rSyncConfig,
    rTypeName,

    -- ** ResolverKind
    ResolverKind (..),

    -- ** ConflictHandlerType
    ConflictHandlerType (..),

    -- ** LogConfig
    LogConfig (..),
    mkLogConfig,
    lcFieldLogLevel,
    lcCloudWatchLogsRoleArn,
    lcExcludeVerboseContent,

    -- ** RelationalDatabaseDataSourceConfig
    RelationalDatabaseDataSourceConfig (..),
    mkRelationalDatabaseDataSourceConfig,
    rddscRdsHttpEndpointConfig,
    rddscRelationalDatabaseSourceType,

    -- ** ConflictDetectionType
    ConflictDetectionType (..),

    -- ** GraphqlApi
    GraphqlApi (..),
    mkGraphqlApi,
    gaAdditionalAuthenticationProviders,
    gaApiId,
    gaArn,
    gaAuthenticationType,
    gaLogConfig,
    gaName,
    gaOpenIDConnectConfig,
    gaTags,
    gaUris,
    gaUserPoolConfig,
    gaWafWebAclArn,
    gaXrayEnabled,

    -- ** Name
    Name (..),

    -- ** Endpoint
    Endpoint (..),

    -- ** AwsRegion
    AwsRegion (..),

    -- ** ApiId
    ApiId (..),

    -- ** Id
    Id (..),

    -- ** Description
    Description (..),

    -- ** DataSourceName
    DataSourceName (..),

    -- ** FunctionVersion
    FunctionVersion (..),

    -- ** FunctionId
    FunctionId (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
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
import Network.AWS.AppSync.Types
import Network.AWS.AppSync.UntagResource
import Network.AWS.AppSync.UpdateApiCache
import Network.AWS.AppSync.UpdateApiKey
import Network.AWS.AppSync.UpdateDataSource
import Network.AWS.AppSync.UpdateFunction
import Network.AWS.AppSync.UpdateGraphqlApi
import Network.AWS.AppSync.UpdateResolver
import Network.AWS.AppSync.UpdateType
import Network.AWS.AppSync.Waiters
import qualified Network.AWS.Prelude as Lude

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
