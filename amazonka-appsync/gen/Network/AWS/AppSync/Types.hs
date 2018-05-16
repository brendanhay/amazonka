{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types
    (
    -- * Service Configuration
      appSync

    -- * Errors
    , _APIKeyValidityOutOfBoundsException
    , _APIKeyLimitExceededException
    , _APILimitExceededException
    , _NotFoundException
    , _GraphQLSchemaException
    , _ConcurrentModificationException
    , _InternalFailureException
    , _UnauthorizedException
    , _BadRequestException
    , _LimitExceededException

    -- * AuthenticationType
    , AuthenticationType (..)

    -- * DataSourceType
    , DataSourceType (..)

    -- * DefaultAction
    , DefaultAction (..)

    -- * FieldLogLevel
    , FieldLogLevel (..)

    -- * OutputType
    , OutputType (..)

    -- * SchemaStatus
    , SchemaStatus (..)

    -- * TypeDefinitionFormat
    , TypeDefinitionFormat (..)

    -- * APIKey
    , APIKey
    , apiKey
    , akExpires
    , akId
    , akDescription

    -- * DataSource
    , DataSource
    , dataSource
    , dsServiceRoleARN
    , dsDataSourceARN
    , dsDynamodbConfig
    , dsName
    , dsLambdaConfig
    , dsType
    , dsDescription
    , dsElasticsearchConfig

    -- * DynamodbDataSourceConfig
    , DynamodbDataSourceConfig
    , dynamodbDataSourceConfig
    , ddscUseCallerCredentials
    , ddscTableName
    , ddscAwsRegion

    -- * ElasticsearchDataSourceConfig
    , ElasticsearchDataSourceConfig
    , elasticsearchDataSourceConfig
    , edscEndpoint
    , edscAwsRegion

    -- * GraphqlAPI
    , GraphqlAPI
    , graphqlAPI
    , gaArn
    , gaApiId
    , gaUris
    , gaOpenIdConnectConfig
    , gaName
    , gaUserPoolConfig
    , gaAuthenticationType
    , gaLogConfig

    -- * LambdaDataSourceConfig
    , LambdaDataSourceConfig
    , lambdaDataSourceConfig
    , ldscLambdaFunctionARN

    -- * LogConfig
    , LogConfig
    , logConfig
    , lcFieldLogLevel
    , lcCloudWatchLogsRoleARN

    -- * OpenIdConnectConfig
    , OpenIdConnectConfig
    , openIdConnectConfig
    , oiccAuthTTL
    , oiccClientId
    , oiccIatTTL
    , oiccIssuer

    -- * Resolver
    , Resolver
    , resolver
    , rTypeName
    , rDataSourceName
    , rRequestMappingTemplate
    , rResolverARN
    , rResponseMappingTemplate
    , rFieldName

    -- * Type
    , Type
    , type'
    , tArn
    , tDefinition
    , tFormat
    , tName
    , tDescription

    -- * UserPoolConfig
    , UserPoolConfig
    , userPoolConfig
    , upcAppIdClientRegex
    , upcUserPoolId
    , upcAwsRegion
    , upcDefaultAction
    ) where

import Network.AWS.AppSync.Types.Product
import Network.AWS.AppSync.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-07-25@ of the Amazon AppSync SDK configuration.
appSync :: Service
appSync =
  Service
    { _svcAbbrev = "AppSync"
    , _svcSigner = v4
    , _svcPrefix = "appsync"
    , _svcVersion = "2017-07-25"
    , _svcEndpoint = defaultEndpoint appSync
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "AppSync"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The API key expiration must be set to a value between 1 and 365 days from creation (for @CreateApiKey@ ) or from update (for @UpdateApiKey@ ).
--
--
_APIKeyValidityOutOfBoundsException :: AsError a => Getting (First ServiceError) a ServiceError
_APIKeyValidityOutOfBoundsException =
  _MatchServiceError appSync "ApiKeyValidityOutOfBoundsException" .
  hasStatus 400


-- | The API key exceeded a limit. Try your request again.
--
--
_APIKeyLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_APIKeyLimitExceededException =
  _MatchServiceError appSync "ApiKeyLimitExceededException" . hasStatus 400


-- | The GraphQL API exceeded a limit. Try your request again.
--
--
_APILimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_APILimitExceededException =
  _MatchServiceError appSync "ApiLimitExceededException" . hasStatus 400


-- | The resource specified in the request was not found. Check the resource and try again.
--
--
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException =
  _MatchServiceError appSync "NotFoundException" . hasStatus 404


-- | The GraphQL schema is not valid.
--
--
_GraphQLSchemaException :: AsError a => Getting (First ServiceError) a ServiceError
_GraphQLSchemaException =
  _MatchServiceError appSync "GraphQLSchemaException" . hasStatus 400


-- | Another modification is being made. That modification must complete before you can make your change.
--
--
_ConcurrentModificationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentModificationException =
  _MatchServiceError appSync "ConcurrentModificationException" . hasStatus 409


-- | An internal AWS AppSync error occurred. Try your request again.
--
--
_InternalFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalFailureException =
  _MatchServiceError appSync "InternalFailureException" . hasStatus 500


-- | You are not authorized to perform this operation.
--
--
_UnauthorizedException :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthorizedException =
  _MatchServiceError appSync "UnauthorizedException" . hasStatus 401


-- | The request is not well formed. For example, a value is invalid or a required field is missing. Check the field values, and try again.
--
--
_BadRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_BadRequestException =
  _MatchServiceError appSync "BadRequestException" . hasStatus 400


-- | The request exceeded a limit. Try your request again.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError appSync "LimitExceededException" . hasStatus 429

