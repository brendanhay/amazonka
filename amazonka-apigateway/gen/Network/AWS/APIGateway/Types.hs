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
      apiGateway

    -- * Errors
    , _ConflictException
    , _NotFoundException
    , _TooManyRequestsException
    , _ServiceUnavailableException
    , _UnauthorizedException
    , _BadRequestException
    , _LimitExceededException

    -- * APIKeysFormat
    , APIKeysFormat (..)

    -- * AuthorizerType
    , AuthorizerType (..)

    -- * CacheClusterSize
    , CacheClusterSize (..)

    -- * CacheClusterStatus
    , CacheClusterStatus (..)

    -- * ContentHandlingStrategy
    , ContentHandlingStrategy (..)

    -- * DocumentationPartType
    , DocumentationPartType (..)

    -- * GatewayResponseType
    , GatewayResponseType (..)

    -- * IntegrationType
    , IntegrationType (..)

    -- * Op
    , Op (..)

    -- * PutMode
    , PutMode (..)

    -- * QuotaPeriodType
    , QuotaPeriodType (..)

    -- * UnauthorizedCacheControlHeaderStrategy
    , UnauthorizedCacheControlHeaderStrategy (..)

    -- * APIKey
    , APIKey
    , apiKey
    , akEnabled
    , akValue
    , akCustomerId
    , akCreatedDate
    , akName
    , akId
    , akStageKeys
    , akLastUpdatedDate
    , akDescription

    -- * APIStage
    , APIStage
    , apiStage
    , asStage
    , asApiId

    -- * Account
    , Account
    , account
    , aApiKeyVersion
    , aCloudwatchRoleARN
    , aFeatures
    , aThrottleSettings

    -- * Authorizer
    , Authorizer
    , authorizer
    , aAuthorizerURI
    , aIdentityValidationExpression
    , aProviderARNs
    , aName
    , aId
    , aAuthorizerResultTtlInSeconds
    , aAuthType
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

    -- * DocumentationPart
    , DocumentationPart
    , documentationPart
    , dpLocation
    , dpId
    , dpProperties

    -- * DocumentationPartLocation
    , DocumentationPartLocation
    , documentationPartLocation
    , dplPath
    , dplName
    , dplMethod
    , dplStatusCode
    , dplType

    -- * DocumentationVersion
    , DocumentationVersion
    , documentationVersion
    , dvCreatedDate
    , dvVersion
    , dvDescription

    -- * DomainName
    , DomainName
    , domainName
    , dnCertificateName
    , dnCertificateARN
    , dnDomainName
    , dnCertificateUploadDate
    , dnDistributionDomainName

    -- * GatewayResponse
    , GatewayResponse
    , gatewayResponse
    , gDefaultResponse
    , gResponseTemplates
    , gResponseType
    , gStatusCode
    , gResponseParameters

    -- * Integration
    , Integration
    , integration
    , iHttpMethod
    , iRequestTemplates
    , iCredentials
    , iRequestParameters
    , iContentHandling
    , iPassthroughBehavior
    , iUri
    , iIntegrationResponses
    , iCacheNamespace
    , iType
    , iCacheKeyParameters

    -- * IntegrationResponse
    , IntegrationResponse
    , integrationResponse
    , intContentHandling
    , intResponseTemplates
    , intSelectionPattern
    , intStatusCode
    , intResponseParameters

    -- * Method
    , Method
    , method
    , mMethodResponses
    , mHttpMethod
    , mRequestValidatorId
    , mRequestModels
    , mRequestParameters
    , mAuthorizerId
    , mOperationName
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

    -- * QuotaSettings
    , QuotaSettings
    , quotaSettings
    , qsOffset
    , qsPeriod
    , qsLimit

    -- * RequestValidator
    , RequestValidator
    , requestValidator
    , rvValidateRequestParameters
    , rvName
    , rvValidateRequestBody
    , rvId

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
    , raBinaryMediaTypes
    , raWarnings
    , raCreatedDate
    , raName
    , raVersion
    , raId
    , raDescription

    -- * SDKConfigurationProperty
    , SDKConfigurationProperty
    , sdkConfigurationProperty
    , scpFriendlyName
    , scpRequired
    , scpName
    , scpDefaultValue
    , scpDescription

    -- * SDKType
    , SDKType
    , sdkType
    , stFriendlyName
    , stConfigurationProperties
    , stId
    , stDescription

    -- * Stage
    , Stage
    , stage
    , sDeploymentId
    , sVariables
    , sDocumentationVersion
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

    -- * Usage
    , Usage
    , usage
    , uUsagePlanId
    , uEndDate
    , uItems
    , uStartDate
    , uPosition

    -- * UsagePlan
    , UsagePlan
    , usagePlan
    , upApiStages
    , upName
    , upId
    , upThrottle
    , upQuota
    , upDescription
    , upProductCode

    -- * UsagePlanKey
    , UsagePlanKey
    , usagePlanKey
    , upkValue
    , upkName
    , upkId
    , upkType
    ) where

import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.APIGateway.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version @2015-07-09@ of the Amazon API Gateway SDK configuration.
apiGateway :: Service
apiGateway =
    Service
    { _svcAbbrev = "APIGateway"
    , _svcSigner = v4
    , _svcPrefix = "apigateway"
    , _svcVersion = "2015-07-09"
    , _svcEndpoint = defaultEndpoint apiGateway
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "APIGateway"
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
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | Prism for ConflictException' errors.
_ConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictException =
    _MatchServiceError apiGateway "ConflictException" . hasStatus 409

-- | Prism for NotFoundException' errors.
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException =
    _MatchServiceError apiGateway "NotFoundException" . hasStatus 404

-- | Prism for TooManyRequestsException' errors.
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException =
    _MatchServiceError apiGateway "TooManyRequestsException" . hasStatus 429

-- | Prism for ServiceUnavailableException' errors.
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException =
    _MatchServiceError apiGateway "ServiceUnavailableException" . hasStatus 503

-- | Prism for UnauthorizedException' errors.
_UnauthorizedException :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthorizedException =
    _MatchServiceError apiGateway "UnauthorizedException" . hasStatus 401

-- | Prism for BadRequestException' errors.
_BadRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_BadRequestException =
    _MatchServiceError apiGateway "BadRequestException" . hasStatus 400

-- | Prism for LimitExceededException' errors.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _MatchServiceError apiGateway "LimitExceededException" . hasStatus 429
