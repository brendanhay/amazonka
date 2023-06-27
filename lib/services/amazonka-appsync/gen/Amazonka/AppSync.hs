{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.AppSync
-- Copyright   : (c) 2013-2023 Brendan Hay
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

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ApiKeyLimitExceededException
    _ApiKeyLimitExceededException,

    -- ** ApiKeyValidityOutOfBoundsException
    _ApiKeyValidityOutOfBoundsException,

    -- ** ApiLimitExceededException
    _ApiLimitExceededException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** GraphQLSchemaException
    _GraphQLSchemaException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateApi
    AssociateApi (AssociateApi'),
    newAssociateApi,
    AssociateApiResponse (AssociateApiResponse'),
    newAssociateApiResponse,

    -- ** AssociateMergedGraphqlApi
    AssociateMergedGraphqlApi (AssociateMergedGraphqlApi'),
    newAssociateMergedGraphqlApi,
    AssociateMergedGraphqlApiResponse (AssociateMergedGraphqlApiResponse'),
    newAssociateMergedGraphqlApiResponse,

    -- ** AssociateSourceGraphqlApi
    AssociateSourceGraphqlApi (AssociateSourceGraphqlApi'),
    newAssociateSourceGraphqlApi,
    AssociateSourceGraphqlApiResponse (AssociateSourceGraphqlApiResponse'),
    newAssociateSourceGraphqlApiResponse,

    -- ** CreateApiCache
    CreateApiCache (CreateApiCache'),
    newCreateApiCache,
    CreateApiCacheResponse (CreateApiCacheResponse'),
    newCreateApiCacheResponse,

    -- ** CreateApiKey
    CreateApiKey (CreateApiKey'),
    newCreateApiKey,
    CreateApiKeyResponse (CreateApiKeyResponse'),
    newCreateApiKeyResponse,

    -- ** CreateDataSource
    CreateDataSource (CreateDataSource'),
    newCreateDataSource,
    CreateDataSourceResponse (CreateDataSourceResponse'),
    newCreateDataSourceResponse,

    -- ** CreateDomainName
    CreateDomainName (CreateDomainName'),
    newCreateDomainName,
    CreateDomainNameResponse (CreateDomainNameResponse'),
    newCreateDomainNameResponse,

    -- ** CreateFunction
    CreateFunction (CreateFunction'),
    newCreateFunction,
    CreateFunctionResponse (CreateFunctionResponse'),
    newCreateFunctionResponse,

    -- ** CreateGraphqlApi
    CreateGraphqlApi (CreateGraphqlApi'),
    newCreateGraphqlApi,
    CreateGraphqlApiResponse (CreateGraphqlApiResponse'),
    newCreateGraphqlApiResponse,

    -- ** CreateResolver
    CreateResolver (CreateResolver'),
    newCreateResolver,
    CreateResolverResponse (CreateResolverResponse'),
    newCreateResolverResponse,

    -- ** CreateType
    CreateType (CreateType'),
    newCreateType,
    CreateTypeResponse (CreateTypeResponse'),
    newCreateTypeResponse,

    -- ** DeleteApiCache
    DeleteApiCache (DeleteApiCache'),
    newDeleteApiCache,
    DeleteApiCacheResponse (DeleteApiCacheResponse'),
    newDeleteApiCacheResponse,

    -- ** DeleteApiKey
    DeleteApiKey (DeleteApiKey'),
    newDeleteApiKey,
    DeleteApiKeyResponse (DeleteApiKeyResponse'),
    newDeleteApiKeyResponse,

    -- ** DeleteDataSource
    DeleteDataSource (DeleteDataSource'),
    newDeleteDataSource,
    DeleteDataSourceResponse (DeleteDataSourceResponse'),
    newDeleteDataSourceResponse,

    -- ** DeleteDomainName
    DeleteDomainName (DeleteDomainName'),
    newDeleteDomainName,
    DeleteDomainNameResponse (DeleteDomainNameResponse'),
    newDeleteDomainNameResponse,

    -- ** DeleteFunction
    DeleteFunction (DeleteFunction'),
    newDeleteFunction,
    DeleteFunctionResponse (DeleteFunctionResponse'),
    newDeleteFunctionResponse,

    -- ** DeleteGraphqlApi
    DeleteGraphqlApi (DeleteGraphqlApi'),
    newDeleteGraphqlApi,
    DeleteGraphqlApiResponse (DeleteGraphqlApiResponse'),
    newDeleteGraphqlApiResponse,

    -- ** DeleteResolver
    DeleteResolver (DeleteResolver'),
    newDeleteResolver,
    DeleteResolverResponse (DeleteResolverResponse'),
    newDeleteResolverResponse,

    -- ** DeleteType
    DeleteType (DeleteType'),
    newDeleteType,
    DeleteTypeResponse (DeleteTypeResponse'),
    newDeleteTypeResponse,

    -- ** DisassociateApi
    DisassociateApi (DisassociateApi'),
    newDisassociateApi,
    DisassociateApiResponse (DisassociateApiResponse'),
    newDisassociateApiResponse,

    -- ** DisassociateMergedGraphqlApi
    DisassociateMergedGraphqlApi (DisassociateMergedGraphqlApi'),
    newDisassociateMergedGraphqlApi,
    DisassociateMergedGraphqlApiResponse (DisassociateMergedGraphqlApiResponse'),
    newDisassociateMergedGraphqlApiResponse,

    -- ** DisassociateSourceGraphqlApi
    DisassociateSourceGraphqlApi (DisassociateSourceGraphqlApi'),
    newDisassociateSourceGraphqlApi,
    DisassociateSourceGraphqlApiResponse (DisassociateSourceGraphqlApiResponse'),
    newDisassociateSourceGraphqlApiResponse,

    -- ** EvaluateCode
    EvaluateCode (EvaluateCode'),
    newEvaluateCode,
    EvaluateCodeResponse (EvaluateCodeResponse'),
    newEvaluateCodeResponse,

    -- ** EvaluateMappingTemplate
    EvaluateMappingTemplate (EvaluateMappingTemplate'),
    newEvaluateMappingTemplate,
    EvaluateMappingTemplateResponse (EvaluateMappingTemplateResponse'),
    newEvaluateMappingTemplateResponse,

    -- ** FlushApiCache
    FlushApiCache (FlushApiCache'),
    newFlushApiCache,
    FlushApiCacheResponse (FlushApiCacheResponse'),
    newFlushApiCacheResponse,

    -- ** GetApiAssociation
    GetApiAssociation (GetApiAssociation'),
    newGetApiAssociation,
    GetApiAssociationResponse (GetApiAssociationResponse'),
    newGetApiAssociationResponse,

    -- ** GetApiCache
    GetApiCache (GetApiCache'),
    newGetApiCache,
    GetApiCacheResponse (GetApiCacheResponse'),
    newGetApiCacheResponse,

    -- ** GetDataSource
    GetDataSource (GetDataSource'),
    newGetDataSource,
    GetDataSourceResponse (GetDataSourceResponse'),
    newGetDataSourceResponse,

    -- ** GetDomainName
    GetDomainName (GetDomainName'),
    newGetDomainName,
    GetDomainNameResponse (GetDomainNameResponse'),
    newGetDomainNameResponse,

    -- ** GetFunction
    GetFunction (GetFunction'),
    newGetFunction,
    GetFunctionResponse (GetFunctionResponse'),
    newGetFunctionResponse,

    -- ** GetGraphqlApi
    GetGraphqlApi (GetGraphqlApi'),
    newGetGraphqlApi,
    GetGraphqlApiResponse (GetGraphqlApiResponse'),
    newGetGraphqlApiResponse,

    -- ** GetIntrospectionSchema
    GetIntrospectionSchema (GetIntrospectionSchema'),
    newGetIntrospectionSchema,
    GetIntrospectionSchemaResponse (GetIntrospectionSchemaResponse'),
    newGetIntrospectionSchemaResponse,

    -- ** GetResolver
    GetResolver (GetResolver'),
    newGetResolver,
    GetResolverResponse (GetResolverResponse'),
    newGetResolverResponse,

    -- ** GetSchemaCreationStatus
    GetSchemaCreationStatus (GetSchemaCreationStatus'),
    newGetSchemaCreationStatus,
    GetSchemaCreationStatusResponse (GetSchemaCreationStatusResponse'),
    newGetSchemaCreationStatusResponse,

    -- ** GetSourceApiAssociation
    GetSourceApiAssociation (GetSourceApiAssociation'),
    newGetSourceApiAssociation,
    GetSourceApiAssociationResponse (GetSourceApiAssociationResponse'),
    newGetSourceApiAssociationResponse,

    -- ** GetType
    GetType (GetType'),
    newGetType,
    GetTypeResponse (GetTypeResponse'),
    newGetTypeResponse,

    -- ** ListApiKeys (Paginated)
    ListApiKeys (ListApiKeys'),
    newListApiKeys,
    ListApiKeysResponse (ListApiKeysResponse'),
    newListApiKeysResponse,

    -- ** ListDataSources (Paginated)
    ListDataSources (ListDataSources'),
    newListDataSources,
    ListDataSourcesResponse (ListDataSourcesResponse'),
    newListDataSourcesResponse,

    -- ** ListDomainNames
    ListDomainNames (ListDomainNames'),
    newListDomainNames,
    ListDomainNamesResponse (ListDomainNamesResponse'),
    newListDomainNamesResponse,

    -- ** ListFunctions (Paginated)
    ListFunctions (ListFunctions'),
    newListFunctions,
    ListFunctionsResponse (ListFunctionsResponse'),
    newListFunctionsResponse,

    -- ** ListGraphqlApis (Paginated)
    ListGraphqlApis (ListGraphqlApis'),
    newListGraphqlApis,
    ListGraphqlApisResponse (ListGraphqlApisResponse'),
    newListGraphqlApisResponse,

    -- ** ListResolvers (Paginated)
    ListResolvers (ListResolvers'),
    newListResolvers,
    ListResolversResponse (ListResolversResponse'),
    newListResolversResponse,

    -- ** ListResolversByFunction (Paginated)
    ListResolversByFunction (ListResolversByFunction'),
    newListResolversByFunction,
    ListResolversByFunctionResponse (ListResolversByFunctionResponse'),
    newListResolversByFunctionResponse,

    -- ** ListSourceApiAssociations
    ListSourceApiAssociations (ListSourceApiAssociations'),
    newListSourceApiAssociations,
    ListSourceApiAssociationsResponse (ListSourceApiAssociationsResponse'),
    newListSourceApiAssociationsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTypes (Paginated)
    ListTypes (ListTypes'),
    newListTypes,
    ListTypesResponse (ListTypesResponse'),
    newListTypesResponse,

    -- ** ListTypesByAssociation
    ListTypesByAssociation (ListTypesByAssociation'),
    newListTypesByAssociation,
    ListTypesByAssociationResponse (ListTypesByAssociationResponse'),
    newListTypesByAssociationResponse,

    -- ** StartSchemaCreation
    StartSchemaCreation (StartSchemaCreation'),
    newStartSchemaCreation,
    StartSchemaCreationResponse (StartSchemaCreationResponse'),
    newStartSchemaCreationResponse,

    -- ** StartSchemaMerge
    StartSchemaMerge (StartSchemaMerge'),
    newStartSchemaMerge,
    StartSchemaMergeResponse (StartSchemaMergeResponse'),
    newStartSchemaMergeResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateApiCache
    UpdateApiCache (UpdateApiCache'),
    newUpdateApiCache,
    UpdateApiCacheResponse (UpdateApiCacheResponse'),
    newUpdateApiCacheResponse,

    -- ** UpdateApiKey
    UpdateApiKey (UpdateApiKey'),
    newUpdateApiKey,
    UpdateApiKeyResponse (UpdateApiKeyResponse'),
    newUpdateApiKeyResponse,

    -- ** UpdateDataSource
    UpdateDataSource (UpdateDataSource'),
    newUpdateDataSource,
    UpdateDataSourceResponse (UpdateDataSourceResponse'),
    newUpdateDataSourceResponse,

    -- ** UpdateDomainName
    UpdateDomainName (UpdateDomainName'),
    newUpdateDomainName,
    UpdateDomainNameResponse (UpdateDomainNameResponse'),
    newUpdateDomainNameResponse,

    -- ** UpdateFunction
    UpdateFunction (UpdateFunction'),
    newUpdateFunction,
    UpdateFunctionResponse (UpdateFunctionResponse'),
    newUpdateFunctionResponse,

    -- ** UpdateGraphqlApi
    UpdateGraphqlApi (UpdateGraphqlApi'),
    newUpdateGraphqlApi,
    UpdateGraphqlApiResponse (UpdateGraphqlApiResponse'),
    newUpdateGraphqlApiResponse,

    -- ** UpdateResolver
    UpdateResolver (UpdateResolver'),
    newUpdateResolver,
    UpdateResolverResponse (UpdateResolverResponse'),
    newUpdateResolverResponse,

    -- ** UpdateSourceApiAssociation
    UpdateSourceApiAssociation (UpdateSourceApiAssociation'),
    newUpdateSourceApiAssociation,
    UpdateSourceApiAssociationResponse (UpdateSourceApiAssociationResponse'),
    newUpdateSourceApiAssociationResponse,

    -- ** UpdateType
    UpdateType (UpdateType'),
    newUpdateType,
    UpdateTypeResponse (UpdateTypeResponse'),
    newUpdateTypeResponse,

    -- * Types

    -- ** ApiCacheStatus
    ApiCacheStatus (..),

    -- ** ApiCacheType
    ApiCacheType (..),

    -- ** ApiCachingBehavior
    ApiCachingBehavior (..),

    -- ** AssociationStatus
    AssociationStatus (..),

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

    -- ** GraphQLApiType
    GraphQLApiType (..),

    -- ** GraphQLApiVisibility
    GraphQLApiVisibility (..),

    -- ** MergeType
    MergeType (..),

    -- ** OutputType
    OutputType (..),

    -- ** Ownership
    Ownership (..),

    -- ** RelationalDatabaseSourceType
    RelationalDatabaseSourceType (..),

    -- ** ResolverKind
    ResolverKind (..),

    -- ** RuntimeName
    RuntimeName (..),

    -- ** SchemaStatus
    SchemaStatus (..),

    -- ** SourceApiAssociationStatus
    SourceApiAssociationStatus (..),

    -- ** TypeDefinitionFormat
    TypeDefinitionFormat (..),

    -- ** AdditionalAuthenticationProvider
    AdditionalAuthenticationProvider (AdditionalAuthenticationProvider'),
    newAdditionalAuthenticationProvider,

    -- ** ApiAssociation
    ApiAssociation (ApiAssociation'),
    newApiAssociation,

    -- ** ApiCache
    ApiCache (ApiCache'),
    newApiCache,

    -- ** ApiKey
    ApiKey (ApiKey'),
    newApiKey,

    -- ** AppSyncRuntime
    AppSyncRuntime (AppSyncRuntime'),
    newAppSyncRuntime,

    -- ** AuthorizationConfig
    AuthorizationConfig (AuthorizationConfig'),
    newAuthorizationConfig,

    -- ** AwsIamConfig
    AwsIamConfig (AwsIamConfig'),
    newAwsIamConfig,

    -- ** CachingConfig
    CachingConfig (CachingConfig'),
    newCachingConfig,

    -- ** CodeError
    CodeError (CodeError'),
    newCodeError,

    -- ** CodeErrorLocation
    CodeErrorLocation (CodeErrorLocation'),
    newCodeErrorLocation,

    -- ** CognitoUserPoolConfig
    CognitoUserPoolConfig (CognitoUserPoolConfig'),
    newCognitoUserPoolConfig,

    -- ** DataSource
    DataSource (DataSource'),
    newDataSource,

    -- ** DeltaSyncConfig
    DeltaSyncConfig (DeltaSyncConfig'),
    newDeltaSyncConfig,

    -- ** DomainNameConfig
    DomainNameConfig (DomainNameConfig'),
    newDomainNameConfig,

    -- ** DynamodbDataSourceConfig
    DynamodbDataSourceConfig (DynamodbDataSourceConfig'),
    newDynamodbDataSourceConfig,

    -- ** ElasticsearchDataSourceConfig
    ElasticsearchDataSourceConfig (ElasticsearchDataSourceConfig'),
    newElasticsearchDataSourceConfig,

    -- ** ErrorDetail
    ErrorDetail (ErrorDetail'),
    newErrorDetail,

    -- ** EvaluateCodeErrorDetail
    EvaluateCodeErrorDetail (EvaluateCodeErrorDetail'),
    newEvaluateCodeErrorDetail,

    -- ** EventBridgeDataSourceConfig
    EventBridgeDataSourceConfig (EventBridgeDataSourceConfig'),
    newEventBridgeDataSourceConfig,

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

    -- ** SourceApiAssociation
    SourceApiAssociation (SourceApiAssociation'),
    newSourceApiAssociation,

    -- ** SourceApiAssociationConfig
    SourceApiAssociationConfig (SourceApiAssociationConfig'),
    newSourceApiAssociationConfig,

    -- ** SourceApiAssociationSummary
    SourceApiAssociationSummary (SourceApiAssociationSummary'),
    newSourceApiAssociationSummary,

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

import Amazonka.AppSync.AssociateApi
import Amazonka.AppSync.AssociateMergedGraphqlApi
import Amazonka.AppSync.AssociateSourceGraphqlApi
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
import Amazonka.AppSync.DisassociateMergedGraphqlApi
import Amazonka.AppSync.DisassociateSourceGraphqlApi
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
import Amazonka.AppSync.GetSourceApiAssociation
import Amazonka.AppSync.GetType
import Amazonka.AppSync.Lens
import Amazonka.AppSync.ListApiKeys
import Amazonka.AppSync.ListDataSources
import Amazonka.AppSync.ListDomainNames
import Amazonka.AppSync.ListFunctions
import Amazonka.AppSync.ListGraphqlApis
import Amazonka.AppSync.ListResolvers
import Amazonka.AppSync.ListResolversByFunction
import Amazonka.AppSync.ListSourceApiAssociations
import Amazonka.AppSync.ListTagsForResource
import Amazonka.AppSync.ListTypes
import Amazonka.AppSync.ListTypesByAssociation
import Amazonka.AppSync.StartSchemaCreation
import Amazonka.AppSync.StartSchemaMerge
import Amazonka.AppSync.TagResource
import Amazonka.AppSync.Types
import Amazonka.AppSync.UntagResource
import Amazonka.AppSync.UpdateApiCache
import Amazonka.AppSync.UpdateApiKey
import Amazonka.AppSync.UpdateDataSource
import Amazonka.AppSync.UpdateDomainName
import Amazonka.AppSync.UpdateFunction
import Amazonka.AppSync.UpdateGraphqlApi
import Amazonka.AppSync.UpdateResolver
import Amazonka.AppSync.UpdateSourceApiAssociation
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
