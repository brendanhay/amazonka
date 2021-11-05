{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.AppSync
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-07-25@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AppSync provides API actions for creating and interacting with data
-- sources using GraphQL from your application.
module Amazonka.AppSync
  ( -- * Service Configuration
    defaultService,

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
    UpdateDataSource (UpdateDataSource'),
    newUpdateDataSource,
    UpdateDataSourceResponse (UpdateDataSourceResponse'),
    newUpdateDataSourceResponse,

    -- ** DeleteDataSource
    DeleteDataSource (DeleteDataSource'),
    newDeleteDataSource,
    DeleteDataSourceResponse (DeleteDataSourceResponse'),
    newDeleteDataSourceResponse,

    -- ** CreateType
    CreateType (CreateType'),
    newCreateType,
    CreateTypeResponse (CreateTypeResponse'),
    newCreateTypeResponse,

    -- ** GetGraphqlApi
    GetGraphqlApi (GetGraphqlApi'),
    newGetGraphqlApi,
    GetGraphqlApiResponse (GetGraphqlApiResponse'),
    newGetGraphqlApiResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** CreateGraphqlApi
    CreateGraphqlApi (CreateGraphqlApi'),
    newCreateGraphqlApi,
    CreateGraphqlApiResponse (CreateGraphqlApiResponse'),
    newCreateGraphqlApiResponse,

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

    -- ** DeleteGraphqlApi
    DeleteGraphqlApi (DeleteGraphqlApi'),
    newDeleteGraphqlApi,
    DeleteGraphqlApiResponse (DeleteGraphqlApiResponse'),
    newDeleteGraphqlApiResponse,

    -- ** UpdateGraphqlApi
    UpdateGraphqlApi (UpdateGraphqlApi'),
    newUpdateGraphqlApi,
    UpdateGraphqlApiResponse (UpdateGraphqlApiResponse'),
    newUpdateGraphqlApiResponse,

    -- ** GetIntrospectionSchema
    GetIntrospectionSchema (GetIntrospectionSchema'),
    newGetIntrospectionSchema,
    GetIntrospectionSchemaResponse (GetIntrospectionSchemaResponse'),
    newGetIntrospectionSchemaResponse,

    -- ** GetDataSource
    GetDataSource (GetDataSource'),
    newGetDataSource,
    GetDataSourceResponse (GetDataSourceResponse'),
    newGetDataSourceResponse,

    -- ** ListResolversByFunction (Paginated)
    ListResolversByFunction (ListResolversByFunction'),
    newListResolversByFunction,
    ListResolversByFunctionResponse (ListResolversByFunctionResponse'),
    newListResolversByFunctionResponse,

    -- ** CreateFunction
    CreateFunction (CreateFunction'),
    newCreateFunction,
    CreateFunctionResponse (CreateFunctionResponse'),
    newCreateFunctionResponse,

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

    -- ** UpdateType
    UpdateType (UpdateType'),
    newUpdateType,
    UpdateTypeResponse (UpdateTypeResponse'),
    newUpdateTypeResponse,

    -- ** DeleteType
    DeleteType (DeleteType'),
    newDeleteType,
    DeleteTypeResponse (DeleteTypeResponse'),
    newDeleteTypeResponse,

    -- ** CreateDataSource
    CreateDataSource (CreateDataSource'),
    newCreateDataSource,
    CreateDataSourceResponse (CreateDataSourceResponse'),
    newCreateDataSourceResponse,

    -- ** ListTypes (Paginated)
    ListTypes (ListTypes'),
    newListTypes,
    ListTypesResponse (ListTypesResponse'),
    newListTypesResponse,

    -- ** GetFunction
    GetFunction (GetFunction'),
    newGetFunction,
    GetFunctionResponse (GetFunctionResponse'),
    newGetFunctionResponse,

    -- ** ListDataSources (Paginated)
    ListDataSources (ListDataSources'),
    newListDataSources,
    ListDataSourcesResponse (ListDataSourcesResponse'),
    newListDataSourcesResponse,

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

    -- ** ListResolvers (Paginated)
    ListResolvers (ListResolvers'),
    newListResolvers,
    ListResolversResponse (ListResolversResponse'),
    newListResolversResponse,

    -- ** CreateResolver
    CreateResolver (CreateResolver'),
    newCreateResolver,
    CreateResolverResponse (CreateResolverResponse'),
    newCreateResolverResponse,

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

    -- ** UpdateApiCache
    UpdateApiCache (UpdateApiCache'),
    newUpdateApiCache,
    UpdateApiCacheResponse (UpdateApiCacheResponse'),
    newUpdateApiCacheResponse,

    -- ** DeleteApiCache
    DeleteApiCache (DeleteApiCache'),
    newDeleteApiCache,
    DeleteApiCacheResponse (DeleteApiCacheResponse'),
    newDeleteApiCacheResponse,

    -- ** ListGraphqlApis (Paginated)
    ListGraphqlApis (ListGraphqlApis'),
    newListGraphqlApis,
    ListGraphqlApisResponse (ListGraphqlApisResponse'),
    newListGraphqlApisResponse,

    -- ** CreateApiCache
    CreateApiCache (CreateApiCache'),
    newCreateApiCache,
    CreateApiCacheResponse (CreateApiCacheResponse'),
    newCreateApiCacheResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** GetResolver
    GetResolver (GetResolver'),
    newGetResolver,
    GetResolverResponse (GetResolverResponse'),
    newGetResolverResponse,

    -- ** UpdateFunction
    UpdateFunction (UpdateFunction'),
    newUpdateFunction,
    UpdateFunctionResponse (UpdateFunctionResponse'),
    newUpdateFunctionResponse,

    -- ** DeleteFunction
    DeleteFunction (DeleteFunction'),
    newDeleteFunction,
    DeleteFunctionResponse (DeleteFunctionResponse'),
    newDeleteFunctionResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** CreateApiKey
    CreateApiKey (CreateApiKey'),
    newCreateApiKey,
    CreateApiKeyResponse (CreateApiKeyResponse'),
    newCreateApiKeyResponse,

    -- ** ListFunctions (Paginated)
    ListFunctions (ListFunctions'),
    newListFunctions,
    ListFunctionsResponse (ListFunctionsResponse'),
    newListFunctionsResponse,

    -- ** ListApiKeys (Paginated)
    ListApiKeys (ListApiKeys'),
    newListApiKeys,
    ListApiKeysResponse (ListApiKeysResponse'),
    newListApiKeysResponse,

    -- ** GetType
    GetType (GetType'),
    newGetType,
    GetTypeResponse (GetTypeResponse'),
    newGetTypeResponse,

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

    -- ** LambdaAuthorizerConfig
    LambdaAuthorizerConfig (LambdaAuthorizerConfig'),
    newLambdaAuthorizerConfig,

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

    -- ** OpenSearchServiceDataSourceConfig
    OpenSearchServiceDataSourceConfig (OpenSearchServiceDataSourceConfig'),
    newOpenSearchServiceDataSourceConfig,

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

import Amazonka.AppSync.CreateApiCache
import Amazonka.AppSync.CreateApiKey
import Amazonka.AppSync.CreateDataSource
import Amazonka.AppSync.CreateFunction
import Amazonka.AppSync.CreateGraphqlApi
import Amazonka.AppSync.CreateResolver
import Amazonka.AppSync.CreateType
import Amazonka.AppSync.DeleteApiCache
import Amazonka.AppSync.DeleteApiKey
import Amazonka.AppSync.DeleteDataSource
import Amazonka.AppSync.DeleteFunction
import Amazonka.AppSync.DeleteGraphqlApi
import Amazonka.AppSync.DeleteResolver
import Amazonka.AppSync.DeleteType
import Amazonka.AppSync.FlushApiCache
import Amazonka.AppSync.GetApiCache
import Amazonka.AppSync.GetDataSource
import Amazonka.AppSync.GetFunction
import Amazonka.AppSync.GetGraphqlApi
import Amazonka.AppSync.GetIntrospectionSchema
import Amazonka.AppSync.GetResolver
import Amazonka.AppSync.GetSchemaCreationStatus
import Amazonka.AppSync.GetType
import Amazonka.AppSync.Lens
import Amazonka.AppSync.ListApiKeys
import Amazonka.AppSync.ListDataSources
import Amazonka.AppSync.ListFunctions
import Amazonka.AppSync.ListGraphqlApis
import Amazonka.AppSync.ListResolvers
import Amazonka.AppSync.ListResolversByFunction
import Amazonka.AppSync.ListTagsForResource
import Amazonka.AppSync.ListTypes
import Amazonka.AppSync.StartSchemaCreation
import Amazonka.AppSync.TagResource
import Amazonka.AppSync.Types
import Amazonka.AppSync.UntagResource
import Amazonka.AppSync.UpdateApiCache
import Amazonka.AppSync.UpdateApiKey
import Amazonka.AppSync.UpdateDataSource
import Amazonka.AppSync.UpdateFunction
import Amazonka.AppSync.UpdateGraphqlApi
import Amazonka.AppSync.UpdateResolver
import Amazonka.AppSync.UpdateType
import Amazonka.AppSync.Waiters

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
