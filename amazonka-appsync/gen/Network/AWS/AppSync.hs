{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS AppSync provides API actions for creating and interacting with data
-- sources using GraphQL from your application.
module Network.AWS.AppSync
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ApiKeyValidityOutOfBoundsException
    _ApiKeyValidityOutOfBoundsException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** GraphQLSchemaException
    _GraphQLSchemaException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ApiLimitExceededException
    _ApiLimitExceededException,

    -- ** ApiKeyLimitExceededException
    _ApiKeyLimitExceededException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListResolvers (Paginated)
    ListResolvers (ListResolvers'),
    newListResolvers,
    ListResolversResponse (ListResolversResponse'),
    newListResolversResponse,

    -- ** UpdateDataSource
    UpdateDataSource (UpdateDataSource'),
    newUpdateDataSource,
    UpdateDataSourceResponse (UpdateDataSourceResponse'),
    newUpdateDataSourceResponse,

    -- ** ListDataSources (Paginated)
    ListDataSources (ListDataSources'),
    newListDataSources,
    ListDataSourcesResponse (ListDataSourcesResponse'),
    newListDataSourcesResponse,

    -- ** CreateType
    CreateType (CreateType'),
    newCreateType,
    CreateTypeResponse (CreateTypeResponse'),
    newCreateTypeResponse,

    -- ** GetFunction
    GetFunction (GetFunction'),
    newGetFunction,
    GetFunctionResponse (GetFunctionResponse'),
    newGetFunctionResponse,

    -- ** DeleteDataSource
    DeleteDataSource (DeleteDataSource'),
    newDeleteDataSource,
    DeleteDataSourceResponse (DeleteDataSourceResponse'),
    newDeleteDataSourceResponse,

    -- ** CreateDataSource
    CreateDataSource (CreateDataSource'),
    newCreateDataSource,
    CreateDataSourceResponse (CreateDataSourceResponse'),
    newCreateDataSourceResponse,

    -- ** DeleteType
    DeleteType (DeleteType'),
    newDeleteType,
    DeleteTypeResponse (DeleteTypeResponse'),
    newDeleteTypeResponse,

    -- ** UpdateType
    UpdateType (UpdateType'),
    newUpdateType,
    UpdateTypeResponse (UpdateTypeResponse'),
    newUpdateTypeResponse,

    -- ** ListResolversByFunction (Paginated)
    ListResolversByFunction (ListResolversByFunction'),
    newListResolversByFunction,
    ListResolversByFunctionResponse (ListResolversByFunctionResponse'),
    newListResolversByFunctionResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** ListFunctions (Paginated)
    ListFunctions (ListFunctions'),
    newListFunctions,
    ListFunctionsResponse (ListFunctionsResponse'),
    newListFunctionsResponse,

    -- ** GetIntrospectionSchema
    GetIntrospectionSchema (GetIntrospectionSchema'),
    newGetIntrospectionSchema,
    GetIntrospectionSchemaResponse (GetIntrospectionSchemaResponse'),
    newGetIntrospectionSchemaResponse,

    -- ** UpdateGraphqlApi
    UpdateGraphqlApi (UpdateGraphqlApi'),
    newUpdateGraphqlApi,
    UpdateGraphqlApiResponse (UpdateGraphqlApiResponse'),
    newUpdateGraphqlApiResponse,

    -- ** ListGraphqlApis (Paginated)
    ListGraphqlApis (ListGraphqlApis'),
    newListGraphqlApis,
    ListGraphqlApisResponse (ListGraphqlApisResponse'),
    newListGraphqlApisResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DeleteGraphqlApi
    DeleteGraphqlApi (DeleteGraphqlApi'),
    newDeleteGraphqlApi,
    DeleteGraphqlApiResponse (DeleteGraphqlApiResponse'),
    newDeleteGraphqlApiResponse,

    -- ** GetResolver
    GetResolver (GetResolver'),
    newGetResolver,
    GetResolverResponse (GetResolverResponse'),
    newGetResolverResponse,

    -- ** DeleteApiCache
    DeleteApiCache (DeleteApiCache'),
    newDeleteApiCache,
    DeleteApiCacheResponse (DeleteApiCacheResponse'),
    newDeleteApiCacheResponse,

    -- ** UpdateApiCache
    UpdateApiCache (UpdateApiCache'),
    newUpdateApiCache,
    UpdateApiCacheResponse (UpdateApiCacheResponse'),
    newUpdateApiCacheResponse,

    -- ** GetSchemaCreationStatus
    GetSchemaCreationStatus (GetSchemaCreationStatus'),
    newGetSchemaCreationStatus,
    GetSchemaCreationStatusResponse (GetSchemaCreationStatusResponse'),
    newGetSchemaCreationStatusResponse,

    -- ** GetApiCache
    GetApiCache (GetApiCache'),
    newGetApiCache,
    GetApiCacheResponse (GetApiCacheResponse'),
    newGetApiCacheResponse,

    -- ** UpdateResolver
    UpdateResolver (UpdateResolver'),
    newUpdateResolver,
    UpdateResolverResponse (UpdateResolverResponse'),
    newUpdateResolverResponse,

    -- ** DeleteResolver
    DeleteResolver (DeleteResolver'),
    newDeleteResolver,
    DeleteResolverResponse (DeleteResolverResponse'),
    newDeleteResolverResponse,

    -- ** GetGraphqlApi
    GetGraphqlApi (GetGraphqlApi'),
    newGetGraphqlApi,
    GetGraphqlApiResponse (GetGraphqlApiResponse'),
    newGetGraphqlApiResponse,

    -- ** ListTypes (Paginated)
    ListTypes (ListTypes'),
    newListTypes,
    ListTypesResponse (ListTypesResponse'),
    newListTypesResponse,

    -- ** GetType
    GetType (GetType'),
    newGetType,
    GetTypeResponse (GetTypeResponse'),
    newGetTypeResponse,

    -- ** CreateFunction
    CreateFunction (CreateFunction'),
    newCreateFunction,
    CreateFunctionResponse (CreateFunctionResponse'),
    newCreateFunctionResponse,

    -- ** ListApiKeys (Paginated)
    ListApiKeys (ListApiKeys'),
    newListApiKeys,
    ListApiKeysResponse (ListApiKeysResponse'),
    newListApiKeysResponse,

    -- ** DeleteApiKey
    DeleteApiKey (DeleteApiKey'),
    newDeleteApiKey,
    DeleteApiKeyResponse (DeleteApiKeyResponse'),
    newDeleteApiKeyResponse,

    -- ** UpdateApiKey
    UpdateApiKey (UpdateApiKey'),
    newUpdateApiKey,
    UpdateApiKeyResponse (UpdateApiKeyResponse'),
    newUpdateApiKeyResponse,

    -- ** GetDataSource
    GetDataSource (GetDataSource'),
    newGetDataSource,
    GetDataSourceResponse (GetDataSourceResponse'),
    newGetDataSourceResponse,

    -- ** DeleteFunction
    DeleteFunction (DeleteFunction'),
    newDeleteFunction,
    DeleteFunctionResponse (DeleteFunctionResponse'),
    newDeleteFunctionResponse,

    -- ** UpdateFunction
    UpdateFunction (UpdateFunction'),
    newUpdateFunction,
    UpdateFunctionResponse (UpdateFunctionResponse'),
    newUpdateFunctionResponse,

    -- ** CreateApiKey
    CreateApiKey (CreateApiKey'),
    newCreateApiKey,
    CreateApiKeyResponse (CreateApiKeyResponse'),
    newCreateApiKeyResponse,

    -- ** StartSchemaCreation
    StartSchemaCreation (StartSchemaCreation'),
    newStartSchemaCreation,
    StartSchemaCreationResponse (StartSchemaCreationResponse'),
    newStartSchemaCreationResponse,

    -- ** FlushApiCache
    FlushApiCache (FlushApiCache'),
    newFlushApiCache,
    FlushApiCacheResponse (FlushApiCacheResponse'),
    newFlushApiCacheResponse,

    -- ** CreateApiCache
    CreateApiCache (CreateApiCache'),
    newCreateApiCache,
    CreateApiCacheResponse (CreateApiCacheResponse'),
    newCreateApiCacheResponse,

    -- ** CreateGraphqlApi
    CreateGraphqlApi (CreateGraphqlApi'),
    newCreateGraphqlApi,
    CreateGraphqlApiResponse (CreateGraphqlApiResponse'),
    newCreateGraphqlApiResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** CreateResolver
    CreateResolver (CreateResolver'),
    newCreateResolver,
    CreateResolverResponse (CreateResolverResponse'),
    newCreateResolverResponse,

    -- * Types

    -- ** ApiCacheStatus
    ApiCacheStatus (..),

    -- ** ApiCacheType
    ApiCacheType (..),

    -- ** ApiCachingBehavior
    ApiCachingBehavior (..),

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

    -- ** AdditionalAuthenticationProvider
    AdditionalAuthenticationProvider (AdditionalAuthenticationProvider'),
    newAdditionalAuthenticationProvider,

    -- ** ApiCache
    ApiCache (ApiCache'),
    newApiCache,

    -- ** ApiKey
    ApiKey (ApiKey'),
    newApiKey,

    -- ** AuthorizationConfig
    AuthorizationConfig (AuthorizationConfig'),
    newAuthorizationConfig,

    -- ** AwsIamConfig
    AwsIamConfig (AwsIamConfig'),
    newAwsIamConfig,

    -- ** CachingConfig
    CachingConfig (CachingConfig'),
    newCachingConfig,

    -- ** CognitoUserPoolConfig
    CognitoUserPoolConfig (CognitoUserPoolConfig'),
    newCognitoUserPoolConfig,

    -- ** DataSource
    DataSource (DataSource'),
    newDataSource,

    -- ** DeltaSyncConfig
    DeltaSyncConfig (DeltaSyncConfig'),
    newDeltaSyncConfig,

    -- ** DynamodbDataSourceConfig
    DynamodbDataSourceConfig (DynamodbDataSourceConfig'),
    newDynamodbDataSourceConfig,

    -- ** ElasticsearchDataSourceConfig
    ElasticsearchDataSourceConfig (ElasticsearchDataSourceConfig'),
    newElasticsearchDataSourceConfig,

    -- ** FunctionConfiguration
    FunctionConfiguration (FunctionConfiguration'),
    newFunctionConfiguration,

    -- ** GraphqlApi
    GraphqlApi (GraphqlApi'),
    newGraphqlApi,

    -- ** HttpDataSourceConfig
    HttpDataSourceConfig (HttpDataSourceConfig'),
    newHttpDataSourceConfig,

    -- ** LambdaConflictHandlerConfig
    LambdaConflictHandlerConfig (LambdaConflictHandlerConfig'),
    newLambdaConflictHandlerConfig,

    -- ** LambdaDataSourceConfig
    LambdaDataSourceConfig (LambdaDataSourceConfig'),
    newLambdaDataSourceConfig,

    -- ** LogConfig
    LogConfig (LogConfig'),
    newLogConfig,

    -- ** OpenIDConnectConfig
    OpenIDConnectConfig (OpenIDConnectConfig'),
    newOpenIDConnectConfig,

    -- ** PipelineConfig
    PipelineConfig (PipelineConfig'),
    newPipelineConfig,

    -- ** RdsHttpEndpointConfig
    RdsHttpEndpointConfig (RdsHttpEndpointConfig'),
    newRdsHttpEndpointConfig,

    -- ** RelationalDatabaseDataSourceConfig
    RelationalDatabaseDataSourceConfig (RelationalDatabaseDataSourceConfig'),
    newRelationalDatabaseDataSourceConfig,

    -- ** Resolver
    Resolver (Resolver'),
    newResolver,

    -- ** SyncConfig
    SyncConfig (SyncConfig'),
    newSyncConfig,

    -- ** Type
    Type (Type'),
    newType,

    -- ** UserPoolConfig
    UserPoolConfig (UserPoolConfig'),
    newUserPoolConfig,
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
import Network.AWS.AppSync.Lens
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
