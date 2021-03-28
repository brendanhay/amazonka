-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _ApiKeyValidityOutOfBoundsException
    , _AccessDeniedException
    , _ApiKeyLimitExceededException
    , _ApiLimitExceededException
    , _NotFoundException
    , _GraphQLSchemaException
    , _ConcurrentModificationException
    , _InternalFailureException
    , _UnauthorizedException
    , _BadRequestException
    , _LimitExceededException

    -- * MappingTemplate
    , MappingTemplate (..)

    -- * ElasticsearchDataSourceConfig
    , ElasticsearchDataSourceConfig (..)
    , mkElasticsearchDataSourceConfig
    , edscEndpoint
    , edscAwsRegion

    -- * ApiCache
    , ApiCache (..)
    , mkApiCache
    , acApiCachingBehavior
    , acAtRestEncryptionEnabled
    , acStatus
    , acTransitEncryptionEnabled
    , acTtl
    , acType

    -- * PaginationToken
    , PaginationToken (..)

    -- * ApiKey
    , ApiKey (..)
    , mkApiKey
    , akDeletes
    , akDescription
    , akExpires
    , akId

    -- * OutputType
    , OutputType (..)

    -- * HttpDataSourceConfig
    , HttpDataSourceConfig (..)
    , mkHttpDataSourceConfig
    , hdscAuthorizationConfig
    , hdscEndpoint

    -- * DataSourceType
    , DataSourceType (..)

    -- * ApiCachingBehavior
    , ApiCachingBehavior (..)

    -- * ResourceName
    , ResourceName (..)

    -- * SchemaStatus
    , SchemaStatus (..)

    -- * OpenIDConnectConfig
    , OpenIDConnectConfig (..)
    , mkOpenIDConnectConfig
    , oidccIssuer
    , oidccAuthTTL
    , oidccClientId
    , oidccIatTTL

    -- * CognitoUserPoolConfig
    , CognitoUserPoolConfig (..)
    , mkCognitoUserPoolConfig
    , cupcUserPoolId
    , cupcAwsRegion
    , cupcAppIdClientRegex

    -- * LambdaDataSourceConfig
    , LambdaDataSourceConfig (..)
    , mkLambdaDataSourceConfig
    , ldscLambdaFunctionArn

    -- * FieldLogLevel
    , FieldLogLevel (..)

    -- * ApiCacheStatus
    , ApiCacheStatus (..)

    -- * CachingConfig
    , CachingConfig (..)
    , mkCachingConfig
    , ccCachingKeys
    , ccTtl

    -- * RelationalDatabaseSourceType
    , RelationalDatabaseSourceType (..)

    -- * TypeDefinitionFormat
    , TypeDefinitionFormat (..)

    -- * DynamodbDataSourceConfig
    , DynamodbDataSourceConfig (..)
    , mkDynamodbDataSourceConfig
    , ddscTableName
    , ddscAwsRegion
    , ddscDeltaSyncConfig
    , ddscUseCallerCredentials
    , ddscVersioned

    -- * AuthorizationConfig
    , AuthorizationConfig (..)
    , mkAuthorizationConfig
    , acAuthorizationType
    , acAwsIamConfig

    -- * TagValue
    , TagValue (..)

    -- * ApiCacheType
    , ApiCacheType (..)

    -- * RdsHttpEndpointConfig
    , RdsHttpEndpointConfig (..)
    , mkRdsHttpEndpointConfig
    , rhecAwsRegion
    , rhecAwsSecretStoreArn
    , rhecDatabaseName
    , rhecDbClusterIdentifier
    , rhecSchema

    -- * UserPoolConfig
    , UserPoolConfig (..)
    , mkUserPoolConfig
    , upcUserPoolId
    , upcAwsRegion
    , upcDefaultAction
    , upcAppIdClientRegex

    -- * ResourceArn
    , ResourceArn (..)

    -- * AwsIamConfig
    , AwsIamConfig (..)
    , mkAwsIamConfig
    , aicSigningRegion
    , aicSigningServiceName

    -- * FunctionConfiguration
    , FunctionConfiguration (..)
    , mkFunctionConfiguration
    , fcDataSourceName
    , fcDescription
    , fcFunctionArn
    , fcFunctionId
    , fcFunctionVersion
    , fcName
    , fcRequestMappingTemplate
    , fcResponseMappingTemplate

    -- * DefaultAction
    , DefaultAction (..)

    -- * DataSource
    , DataSource (..)
    , mkDataSource
    , dsDataSourceArn
    , dsDescription
    , dsDynamodbConfig
    , dsElasticsearchConfig
    , dsHttpConfig
    , dsLambdaConfig
    , dsName
    , dsRelationalDatabaseConfig
    , dsServiceRoleArn
    , dsType

    -- * SyncConfig
    , SyncConfig (..)
    , mkSyncConfig
    , scConflictDetection
    , scConflictHandler
    , scLambdaConflictHandlerConfig

    -- * DeltaSyncConfig
    , DeltaSyncConfig (..)
    , mkDeltaSyncConfig
    , dscBaseTableTTL
    , dscDeltaSyncTableName
    , dscDeltaSyncTableTTL

    -- * TagKey
    , TagKey (..)

    -- * AuthenticationType
    , AuthenticationType (..)

    -- * Type
    , Type (..)
    , mkType
    , tArn
    , tDefinition
    , tDescription
    , tFormat
    , tName

    -- * PipelineConfig
    , PipelineConfig (..)
    , mkPipelineConfig
    , pcFunctions

    -- * AdditionalAuthenticationProvider
    , AdditionalAuthenticationProvider (..)
    , mkAdditionalAuthenticationProvider
    , aapAuthenticationType
    , aapOpenIDConnectConfig
    , aapUserPoolConfig

    -- * AuthorizationType
    , AuthorizationType (..)

    -- * LambdaConflictHandlerConfig
    , LambdaConflictHandlerConfig (..)
    , mkLambdaConflictHandlerConfig
    , lchcLambdaConflictHandlerArn

    -- * Resolver
    , Resolver (..)
    , mkResolver
    , rCachingConfig
    , rDataSourceName
    , rFieldName
    , rKind
    , rPipelineConfig
    , rRequestMappingTemplate
    , rResolverArn
    , rResponseMappingTemplate
    , rSyncConfig
    , rTypeName

    -- * ResolverKind
    , ResolverKind (..)

    -- * ConflictHandlerType
    , ConflictHandlerType (..)

    -- * LogConfig
    , LogConfig (..)
    , mkLogConfig
    , lcFieldLogLevel
    , lcCloudWatchLogsRoleArn
    , lcExcludeVerboseContent

    -- * RelationalDatabaseDataSourceConfig
    , RelationalDatabaseDataSourceConfig (..)
    , mkRelationalDatabaseDataSourceConfig
    , rddscRdsHttpEndpointConfig
    , rddscRelationalDatabaseSourceType

    -- * ConflictDetectionType
    , ConflictDetectionType (..)

    -- * GraphqlApi
    , GraphqlApi (..)
    , mkGraphqlApi
    , gaAdditionalAuthenticationProviders
    , gaApiId
    , gaArn
    , gaAuthenticationType
    , gaLogConfig
    , gaName
    , gaOpenIDConnectConfig
    , gaTags
    , gaUris
    , gaUserPoolConfig
    , gaWafWebAclArn
    , gaXrayEnabled

    -- * Name
    , Name (..)

    -- * DataSourceName
    , DataSourceName (..)

    -- * FunctionId
    , FunctionId (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.AppSync.Types.MappingTemplate
  
import Network.AWS.AppSync.Types.ElasticsearchDataSourceConfig
  
import Network.AWS.AppSync.Types.ApiCache
  
  
  
import Network.AWS.AppSync.Types.PaginationToken
  
import Network.AWS.AppSync.Types.ApiKey
  
import Network.AWS.AppSync.Types.OutputType
  
import Network.AWS.AppSync.Types.HttpDataSourceConfig
  
import Network.AWS.AppSync.Types.DataSourceType
  
import Network.AWS.AppSync.Types.ApiCachingBehavior
  
import Network.AWS.AppSync.Types.ResourceName
  
import Network.AWS.AppSync.Types.SchemaStatus
  
import Network.AWS.AppSync.Types.OpenIDConnectConfig
  
import Network.AWS.AppSync.Types.CognitoUserPoolConfig
  
import Network.AWS.AppSync.Types.LambdaDataSourceConfig
  
import Network.AWS.AppSync.Types.FieldLogLevel
  
import Network.AWS.AppSync.Types.ApiCacheStatus
  
import Network.AWS.AppSync.Types.CachingConfig
  
import Network.AWS.AppSync.Types.RelationalDatabaseSourceType
  
  
import Network.AWS.AppSync.Types.TypeDefinitionFormat
  
import Network.AWS.AppSync.Types.DynamodbDataSourceConfig
  
import Network.AWS.AppSync.Types.AuthorizationConfig
  
  
import Network.AWS.AppSync.Types.TagValue
  
import Network.AWS.AppSync.Types.ApiCacheType
  
import Network.AWS.AppSync.Types.RdsHttpEndpointConfig
  
  
import Network.AWS.AppSync.Types.UserPoolConfig
  
import Network.AWS.AppSync.Types.ResourceArn
  
  
import Network.AWS.AppSync.Types.AwsIamConfig
  
  
import Network.AWS.AppSync.Types.FunctionConfiguration
  
import Network.AWS.AppSync.Types.DefaultAction
  
import Network.AWS.AppSync.Types.DataSource
  
import Network.AWS.AppSync.Types.SyncConfig
  
import Network.AWS.AppSync.Types.DeltaSyncConfig
  
import Network.AWS.AppSync.Types.TagKey
  
  
import Network.AWS.AppSync.Types.AuthenticationType
  
import Network.AWS.AppSync.Types.Type
  
import Network.AWS.AppSync.Types.PipelineConfig
  
import Network.AWS.AppSync.Types.AdditionalAuthenticationProvider
  
import Network.AWS.AppSync.Types.AuthorizationType
  
import Network.AWS.AppSync.Types.LambdaConflictHandlerConfig
  
  
import Network.AWS.AppSync.Types.Resolver
  
import Network.AWS.AppSync.Types.ResolverKind
  
import Network.AWS.AppSync.Types.ConflictHandlerType
  
import Network.AWS.AppSync.Types.LogConfig
  
import Network.AWS.AppSync.Types.RelationalDatabaseDataSourceConfig
  
import Network.AWS.AppSync.Types.ConflictDetectionType
  
  
import Network.AWS.AppSync.Types.GraphqlApi
  
  
import Network.AWS.AppSync.Types.Name
  
import Network.AWS.AppSync.Types.DataSourceName
  
import Network.AWS.AppSync.Types.FunctionId
  

-- | API version @2017-07-25@ of the Amazon AppSync SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "AppSync",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "appsync",
                 Core._svcVersion = "2017-07-25", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "AppSync",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The API key expiration must be set to a value between 1 and 365 days from creation (for @CreateApiKey@ ) or from update (for @UpdateApiKey@ ).
_ApiKeyValidityOutOfBoundsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ApiKeyValidityOutOfBoundsException
  = Core._MatchServiceError mkServiceConfig
      "ApiKeyValidityOutOfBoundsException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ApiKeyValidityOutOfBoundsException #-}
{-# DEPRECATED _ApiKeyValidityOutOfBoundsException "Use generic-lens or generic-optics instead"  #-}

-- | You do not have access to perform this operation on this resource.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException
  = Core._MatchServiceError mkServiceConfig "AccessDeniedException"
      Core.. Core.hasStatues 403
{-# INLINEABLE _AccessDeniedException #-}
{-# DEPRECATED _AccessDeniedException "Use generic-lens or generic-optics instead"  #-}

-- | The API key exceeded a limit. Try your request again.
_ApiKeyLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ApiKeyLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "ApiKeyLimitExceededException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ApiKeyLimitExceededException #-}
{-# DEPRECATED _ApiKeyLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The GraphQL API exceeded a limit. Try your request again.
_ApiLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ApiLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "ApiLimitExceededException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ApiLimitExceededException #-}
{-# DEPRECATED _ApiLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The resource specified in the request was not found. Check the resource, and then try again.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException
  = Core._MatchServiceError mkServiceConfig "NotFoundException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _NotFoundException #-}
{-# DEPRECATED _NotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The GraphQL schema is not valid.
_GraphQLSchemaException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_GraphQLSchemaException
  = Core._MatchServiceError mkServiceConfig "GraphQLSchemaException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _GraphQLSchemaException #-}
{-# DEPRECATED _GraphQLSchemaException "Use generic-lens or generic-optics instead"  #-}

-- | Another modification is in progress at this time and it must complete before you can make your change. 
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException
  = Core._MatchServiceError mkServiceConfig
      "ConcurrentModificationException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _ConcurrentModificationException #-}
{-# DEPRECATED _ConcurrentModificationException "Use generic-lens or generic-optics instead"  #-}

-- | An internal AWS AppSync error occurred. Try your request again.
_InternalFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalFailureException
  = Core._MatchServiceError mkServiceConfig
      "InternalFailureException"
      Core.. Core.hasStatues 500
{-# INLINEABLE _InternalFailureException #-}
{-# DEPRECATED _InternalFailureException "Use generic-lens or generic-optics instead"  #-}

-- | You are not authorized to perform this operation.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException
  = Core._MatchServiceError mkServiceConfig "UnauthorizedException"
      Core.. Core.hasStatues 401
{-# INLINEABLE _UnauthorizedException #-}
{-# DEPRECATED _UnauthorizedException "Use generic-lens or generic-optics instead"  #-}

-- | The request is not well formed. For example, a value is invalid or a required field is missing. Check the field values, and then try again. 
_BadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequestException
  = Core._MatchServiceError mkServiceConfig "BadRequestException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _BadRequestException #-}
{-# DEPRECATED _BadRequestException "Use generic-lens or generic-optics instead"  #-}

-- | The request exceeded a limit. Try your request again.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
      Core.. Core.hasStatues 429
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}
