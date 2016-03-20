{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.APIGateway.Types
    (
    -- * Service Configuration
      aPIGateway

    -- * Errors
    , _ConflictException
    , _NotFoundException
    , _TooManyRequestsException
    , _ServiceUnavailableException
    , _UnauthorizedException
    , _BadRequestException
    , _LimitExceededException

    -- * AuthorizerType
    , AuthorizerType (..)

    -- * CacheClusterSize
    , CacheClusterSize (..)

    -- * CacheClusterStatus
    , CacheClusterStatus (..)

    -- * IntegrationType
    , IntegrationType (..)

    -- * Op
    , Op (..)

    -- * UnauthorizedCacheControlHeaderStrategy
    , UnauthorizedCacheControlHeaderStrategy (..)

    -- * APIKey
    , APIKey
    , apiKey
    , akEnabled
    , akCreatedDate
    , akName
    , akId
    , akStageKeys
    , akLastUpdatedDate
    , akDescription

    -- * Account
    , Account
    , account
    , aCloudwatchRoleARN
    , aThrottleSettings

    -- * Authorizer
    , Authorizer
    , authorizer
    , aAuthorizerURI
    , aIdentityValidationExpression
    , aName
    , aId
    , aAuthorizerResultTtlInSeconds
    , aType
    , aIdentitySource
    , aAuthorizerCredentials

    -- * BasePathMapping
    , BasePathMapping
    , basePathMapping
    , bpmStage
    , bpmBasePath
    , bpmRestAPIId

    -- * ClientCertificate
    , ClientCertificate
    , clientCertificate
    , ccPemEncodedCertificate
    , ccClientCertificateId
    , ccCreatedDate
    , ccExpirationDate
    , ccDescription

    -- * Deployment
    , Deployment
    , deployment
    , dApiSummary
    , dCreatedDate
    , dId
    , dDescription

    -- * DomainName
    , DomainName
    , domainName
    , dnCertificateName
    , dnDomainName
    , dnCertificateUploadDate
    , dnDistributionDomainName

    -- * Integration
    , Integration
    , integration
    , iHttpMethod
    , iRequestTemplates
    , iCredentials
    , iRequestParameters
    , iUri
    , iIntegrationResponses
    , iCacheNamespace
    , iType
    , iCacheKeyParameters

    -- * IntegrationResponse
    , IntegrationResponse
    , integrationResponse
    , iResponseTemplates
    , iSelectionPattern
    , iStatusCode
    , iResponseParameters

    -- * Method
    , Method
    , method
    , mMethodResponses
    , mHttpMethod
    , mRequestModels
    , mRequestParameters
    , mAuthorizerId
    , mAuthorizationType
    , mApiKeyRequired
    , mMethodIntegration

    -- * MethodResponse
    , MethodResponse
    , methodResponse
    , mResponseModels
    , mStatusCode
    , mResponseParameters

    -- * MethodSetting
    , MethodSetting
    , methodSetting
    , msCacheTtlInSeconds
    , msDataTraceEnabled
    , msThrottlingBurstLimit
    , msCacheDataEncrypted
    , msLoggingLevel
    , msRequireAuthorizationForCacheControl
    , msCachingEnabled
    , msMetricsEnabled
    , msThrottlingRateLimit
    , msUnauthorizedCacheControlHeaderStrategy

    -- * MethodSnapshot
    , MethodSnapshot
    , methodSnapshot
    , msAuthorizationType
    , msApiKeyRequired

    -- * Model
    , Model
    , model
    , mSchema
    , mName
    , mId
    , mDescription
    , mContentType

    -- * PatchOperation
    , PatchOperation
    , patchOperation
    , poOp
    , poPath
    , poValue
    , poFrom

    -- * Resource
    , Resource
    , resource
    , rPathPart
    , rPath
    , rId
    , rResourceMethods
    , rParentId

    -- * RestAPI
    , RestAPI
    , restAPI
    , raCreatedDate
    , raName
    , raId
    , raDescription

    -- * Stage
    , Stage
    , stage
    , sDeploymentId
    , sVariables
    , sClientCertificateId
    , sCreatedDate
    , sCacheClusterStatus
    , sMethodSettings
    , sLastUpdatedDate
    , sCacheClusterSize
    , sCacheClusterEnabled
    , sStageName
    , sDescription

    -- * StageKey
    , StageKey
    , stageKey
    , skRestAPIId
    , skStageName

    -- * ThrottleSettings
    , ThrottleSettings
    , throttleSettings
    , tsBurstLimit
    , tsRateLimit
    ) where

import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.APIGateway.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2015-07-09' of the Amazon API Gateway SDK configuration.
aPIGateway :: Service
aPIGateway =
    Service
    { _svcAbbrev = "APIGateway"
    , _svcSigner = v4
    , _svcPrefix = "apigateway"
    , _svcVersion = "2015-07-09"
    , _svcEndpoint = defaultEndpoint aPIGateway
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError
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
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | Prism for ConflictException' errors.
_ConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictException =
    _ServiceError . hasStatus 409 . hasCode "ConflictException"

-- | Prism for NotFoundException' errors.
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException =
    _ServiceError . hasStatus 404 . hasCode "NotFoundException"

-- | Prism for TooManyRequestsException' errors.
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException =
    _ServiceError . hasStatus 429 . hasCode "TooManyRequestsException"

-- | Prism for ServiceUnavailableException' errors.
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException =
    _ServiceError . hasStatus 503 . hasCode "ServiceUnavailableException"

-- | Prism for UnauthorizedException' errors.
_UnauthorizedException :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthorizedException =
    _ServiceError . hasStatus 401 . hasCode "UnauthorizedException"

-- | Prism for BadRequestException' errors.
_BadRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_BadRequestException =
    _ServiceError . hasStatus 400 . hasCode "BadRequestException"

-- | Prism for LimitExceededException' errors.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _ServiceError . hasStatus 429 . hasCode "LimitExceededException"
