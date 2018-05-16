{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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

    -- * APIKeySourceType
    , APIKeySourceType (..)

    -- * APIKeysFormat
    , APIKeysFormat (..)

    -- * AuthorizerType
    , AuthorizerType (..)

    -- * CacheClusterSize
    , CacheClusterSize (..)

    -- * CacheClusterStatus
    , CacheClusterStatus (..)

    -- * ConnectionType
    , ConnectionType (..)

    -- * ContentHandlingStrategy
    , ContentHandlingStrategy (..)

    -- * DocumentationPartType
    , DocumentationPartType (..)

    -- * EndpointType
    , EndpointType (..)

    -- * GatewayResponseType
    , GatewayResponseType (..)

    -- * IntegrationType
    , IntegrationType (..)

    -- * LocationStatusType
    , LocationStatusType (..)

    -- * Op
    , Op (..)

    -- * PutMode
    , PutMode (..)

    -- * QuotaPeriodType
    , QuotaPeriodType (..)

    -- * UnauthorizedCacheControlHeaderStrategy
    , UnauthorizedCacheControlHeaderStrategy (..)

    -- * VPCLinkStatus
    , VPCLinkStatus (..)

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

    -- * AccessLogSettings
    , AccessLogSettings
    , accessLogSettings
    , alsFormat
    , alsDestinationARN

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

    -- * CanarySettings
    , CanarySettings
    , canarySettings
    , csDeploymentId
    , csStageVariableOverrides
    , csUseStageCache
    , csPercentTraffic

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

    -- * DeploymentCanarySettings
    , DeploymentCanarySettings
    , deploymentCanarySettings
    , dcsStageVariableOverrides
    , dcsUseStageCache
    , dcsPercentTraffic

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
    , dnRegionalHostedZoneId
    , dnCertificateName
    , dnRegionalCertificateARN
    , dnCertificateARN
    , dnDistributionHostedZoneId
    , dnDomainName
    , dnRegionalCertificateName
    , dnRegionalDomainName
    , dnCertificateUploadDate
    , dnDistributionDomainName
    , dnEndpointConfiguration

    -- * EndpointConfiguration
    , EndpointConfiguration
    , endpointConfiguration
    , ecTypes

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
    , iConnectionId
    , iRequestParameters
    , iContentHandling
    , iPassthroughBehavior
    , iUri
    , iIntegrationResponses
    , iCacheNamespace
    , iTimeoutInMillis
    , iType
    , iConnectionType
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
    , mAuthorizationScopes
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
    , raMinimumCompressionSize
    , raBinaryMediaTypes
    , raWarnings
    , raCreatedDate
    , raName
    , raVersion
    , raApiKeySource
    , raId
    , raPolicy
    , raEndpointConfiguration
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
    , sAccessLogSettings
    , sDocumentationVersion
    , sClientCertificateId
    , sCreatedDate
    , sCacheClusterStatus
    , sMethodSettings
    , sLastUpdatedDate
    , sCacheClusterSize
    , sCanarySettings
    , sCacheClusterEnabled
    , sStageName
    , sDescription
    , sTags

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

    -- * VPCLink
    , VPCLink
    , vpcLink
    , vlStatus
    , vlTargetARNs
    , vlName
    , vlStatusMessage
    , vlId
    , vlDescription
    ) where

import Network.AWS.APIGateway.Types.Product
import Network.AWS.APIGateway.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

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
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The request configuration has conflicts. For details, see the accompanying error message.
--
--
_ConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictException =
  _MatchServiceError apiGateway "ConflictException" . hasStatus 409


-- | The requested resource is not found. Make sure that the request URI is correct.
--
--
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException =
  _MatchServiceError apiGateway "NotFoundException" . hasStatus 404


-- | The request has reached its throttling limit. Retry after the specified time period.
--
--
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException =
  _MatchServiceError apiGateway "TooManyRequestsException" . hasStatus 429


-- | The requested service is not available. For details see the accompanying error message. Retry after the specified time period.
--
--
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException =
  _MatchServiceError apiGateway "ServiceUnavailableException" . hasStatus 503


-- | The request is denied because the caller has insufficient permissions.
--
--
_UnauthorizedException :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthorizedException =
  _MatchServiceError apiGateway "UnauthorizedException" . hasStatus 401


-- | The submitted request is not valid, for example, the input is incomplete or incorrect. See the accompanying error message for details.
--
--
_BadRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_BadRequestException =
  _MatchServiceError apiGateway "BadRequestException" . hasStatus 400


-- | The request exceeded the rate limit. Retry after the specified time period.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError apiGateway "LimitExceededException" . hasStatus 429

