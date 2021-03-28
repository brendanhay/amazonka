-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _ConflictException
    , _NotFoundException
    , _TooManyRequestsException
    , _ServiceUnavailableException
    , _UnauthorizedException
    , _BadRequestException
    , _LimitExceededException

    -- * ProviderARN
    , ProviderARN (..)

    -- * Stage
    , Stage (..)
    , mkStage
    , sAccessLogSettings
    , sCacheClusterEnabled
    , sCacheClusterSize
    , sCacheClusterStatus
    , sCanarySettings
    , sClientCertificateId
    , sCreatedDate
    , sDeploymentId
    , sDescription
    , sDocumentationVersion
    , sLastUpdatedDate
    , sMethodSettings
    , sStageName
    , sTags
    , sTracingEnabled
    , sVariables
    , sWebAclArn

    -- * ApiKey
    , ApiKey (..)
    , mkApiKey
    , akCreatedDate
    , akCustomerId
    , akDescription
    , akEnabled
    , akId
    , akLastUpdatedDate
    , akName
    , akStageKeys
    , akTags
    , akValue

    -- * Op
    , Op (..)

    -- * ApiKeysFormat
    , ApiKeysFormat (..)

    -- * ApiKeySourceType
    , ApiKeySourceType (..)

    -- * DeploymentCanarySettings
    , DeploymentCanarySettings (..)
    , mkDeploymentCanarySettings
    , dcsPercentTraffic
    , dcsStageVariableOverrides
    , dcsUseStageCache

    -- * DocumentationPartType
    , DocumentationPartType (..)

    -- * AccessLogSettings
    , AccessLogSettings (..)
    , mkAccessLogSettings
    , alsDestinationArn
    , alsFormat

    -- * RestApi
    , RestApi (..)
    , mkRestApi
    , raApiKeySource
    , raBinaryMediaTypes
    , raCreatedDate
    , raDescription
    , raDisableExecuteApiEndpoint
    , raEndpointConfiguration
    , raId
    , raMinimumCompressionSize
    , raName
    , raPolicy
    , raTags
    , raVersion
    , raWarnings

    -- * DocumentationVersion
    , DocumentationVersion (..)
    , mkDocumentationVersion
    , dvCreatedDate
    , dvDescription
    , dvVersion

    -- * QuotaSettings
    , QuotaSettings (..)
    , mkQuotaSettings
    , qsLimit
    , qsOffset
    , qsPeriod

    -- * GatewayResponse
    , GatewayResponse (..)
    , mkGatewayResponse
    , grDefaultResponse
    , grResponseParameters
    , grResponseTemplates
    , grResponseType
    , grStatusCode

    -- * EndpointType
    , EndpointType (..)

    -- * Integration
    , Integration (..)
    , mkIntegration
    , iCacheKeyParameters
    , iCacheNamespace
    , iConnectionId
    , iConnectionType
    , iContentHandling
    , iCredentials
    , iHttpMethod
    , iIntegrationResponses
    , iPassthroughBehavior
    , iRequestParameters
    , iRequestTemplates
    , iTimeoutInMillis
    , iTlsConfig
    , iType
    , iUri

    -- * UsagePlan
    , UsagePlan (..)
    , mkUsagePlan
    , upApiStages
    , upDescription
    , upId
    , upName
    , upProductCode
    , upQuota
    , upTags
    , upThrottle

    -- * SecurityPolicy
    , SecurityPolicy (..)

    -- * Authorizer
    , Authorizer (..)
    , mkAuthorizer
    , aAuthType
    , aAuthorizerCredentials
    , aAuthorizerResultTtlInSeconds
    , aAuthorizerUri
    , aId
    , aIdentitySource
    , aIdentityValidationExpression
    , aName
    , aProviderARNs
    , aType

    -- * DocumentationPartLocationStatusCode
    , DocumentationPartLocationStatusCode (..)

    -- * Account
    , Account (..)
    , mkAccount
    , aApiKeyVersion
    , aCloudwatchRoleArn
    , aFeatures
    , aThrottleSettings

    -- * UsagePlanKey
    , UsagePlanKey (..)
    , mkUsagePlanKey
    , upkId
    , upkName
    , upkType
    , upkValue

    -- * TlsConfig
    , TlsConfig (..)
    , mkTlsConfig
    , tcInsecureSkipVerification

    -- * SdkType
    , SdkType (..)
    , mkSdkType
    , stConfigurationProperties
    , stDescription
    , stFriendlyName
    , stId

    -- * ClientCertificate
    , ClientCertificate (..)
    , mkClientCertificate
    , ccClientCertificateId
    , ccCreatedDate
    , ccDescription
    , ccExpirationDate
    , ccPemEncodedCertificate
    , ccTags

    -- * MethodResponse
    , MethodResponse (..)
    , mkMethodResponse
    , mrResponseModels
    , mrResponseParameters
    , mrStatusCode

    -- * ApiStage
    , ApiStage (..)
    , mkApiStage
    , asApiId
    , asStage
    , asThrottle

    -- * BasePathMapping
    , BasePathMapping (..)
    , mkBasePathMapping
    , bpmBasePath
    , bpmRestApiId
    , bpmStage

    -- * RequestValidator
    , RequestValidator (..)
    , mkRequestValidator
    , rvId
    , rvName
    , rvValidateRequestBody
    , rvValidateRequestParameters

    -- * DomainName
    , DomainName (..)
    , mkDomainName
    , dnCertificateArn
    , dnCertificateName
    , dnCertificateUploadDate
    , dnDistributionDomainName
    , dnDistributionHostedZoneId
    , dnDomainName
    , dnDomainNameStatus
    , dnDomainNameStatusMessage
    , dnEndpointConfiguration
    , dnMutualTlsAuthentication
    , dnRegionalCertificateArn
    , dnRegionalCertificateName
    , dnRegionalDomainName
    , dnRegionalHostedZoneId
    , dnSecurityPolicy
    , dnTags

    -- * Method
    , Method (..)
    , mkMethod
    , mApiKeyRequired
    , mAuthorizationScopes
    , mAuthorizationType
    , mAuthorizerId
    , mHttpMethod
    , mMethodIntegration
    , mMethodResponses
    , mOperationName
    , mRequestModels
    , mRequestParameters
    , mRequestValidatorId

    -- * Model
    , Model (..)
    , mkModel
    , mContentType
    , mDescription
    , mId
    , mName
    , mSchema

    -- * MutualTlsAuthentication
    , MutualTlsAuthentication (..)
    , mkMutualTlsAuthentication
    , mtaTruststoreUri
    , mtaTruststoreVersion
    , mtaTruststoreWarnings

    -- * PutMode
    , PutMode (..)

    -- * Resource
    , Resource (..)
    , mkResource
    , rId
    , rParentId
    , rPath
    , rPathPart
    , rResourceMethods

    -- * CacheClusterStatus
    , CacheClusterStatus (..)

    -- * DocumentationPart
    , DocumentationPart (..)
    , mkDocumentationPart
    , dpId
    , dpLocation
    , dpProperties

    -- * IntegrationResponse
    , IntegrationResponse (..)
    , mkIntegrationResponse
    , irContentHandling
    , irResponseParameters
    , irResponseTemplates
    , irSelectionPattern
    , irStatusCode

    -- * Usage
    , Usage (..)
    , mkUsage
    , uEndDate
    , uItems
    , uPosition
    , uStartDate
    , uUsagePlanId

    -- * ThrottleSettings
    , ThrottleSettings (..)
    , mkThrottleSettings
    , tsBurstLimit
    , tsRateLimit

    -- * VpcLink
    , VpcLink (..)
    , mkVpcLink
    , vlDescription
    , vlId
    , vlName
    , vlStatus
    , vlStatusMessage
    , vlTags
    , vlTargetArns

    -- * ContentHandlingStrategy
    , ContentHandlingStrategy (..)

    -- * DocumentationPartLocation
    , DocumentationPartLocation (..)
    , mkDocumentationPartLocation
    , dplType
    , dplMethod
    , dplName
    , dplPath
    , dplStatusCode

    -- * CacheClusterSize
    , CacheClusterSize (..)

    -- * UnauthorizedCacheControlHeaderStrategy
    , UnauthorizedCacheControlHeaderStrategy (..)

    -- * StageKey
    , StageKey (..)
    , mkStageKey
    , skRestApiId
    , skStageName

    -- * DomainNameStatus
    , DomainNameStatus (..)

    -- * CanarySettings
    , CanarySettings (..)
    , mkCanarySettings
    , csDeploymentId
    , csPercentTraffic
    , csStageVariableOverrides
    , csUseStageCache

    -- * EndpointConfiguration
    , EndpointConfiguration (..)
    , mkEndpointConfiguration
    , ecTypes
    , ecVpcEndpointIds

    -- * SdkConfigurationProperty
    , SdkConfigurationProperty (..)
    , mkSdkConfigurationProperty
    , scpDefaultValue
    , scpDescription
    , scpFriendlyName
    , scpName
    , scpRequired

    -- * GatewayResponseType
    , GatewayResponseType (..)

    -- * QuotaPeriodType
    , QuotaPeriodType (..)

    -- * MethodSnapshot
    , MethodSnapshot (..)
    , mkMethodSnapshot
    , msApiKeyRequired
    , msAuthorizationType

    -- * PatchOperation
    , PatchOperation (..)
    , mkPatchOperation
    , poFrom
    , poOp
    , poPath
    , poValue

    -- * IntegrationType
    , IntegrationType (..)

    -- * ConnectionType
    , ConnectionType (..)

    -- * AuthorizerType
    , AuthorizerType (..)

    -- * LocationStatusType
    , LocationStatusType (..)

    -- * Deployment
    , Deployment (..)
    , mkDeployment
    , dApiSummary
    , dCreatedDate
    , dDescription
    , dId

    -- * MutualTlsAuthenticationInput
    , MutualTlsAuthenticationInput (..)
    , mkMutualTlsAuthenticationInput
    , mtaiTruststoreUri
    , mtaiTruststoreVersion

    -- * StatusCode
    , StatusCode (..)

    -- * VpcLinkStatus
    , VpcLinkStatus (..)

    -- * MethodSetting
    , MethodSetting (..)
    , mkMethodSetting
    , msCacheDataEncrypted
    , msCacheTtlInSeconds
    , msCachingEnabled
    , msDataTraceEnabled
    , msLoggingLevel
    , msMetricsEnabled
    , msRequireAuthorizationForCacheControl
    , msThrottlingBurstLimit
    , msThrottlingRateLimit
    , msUnauthorizedCacheControlHeaderStrategy
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.ApiGateway.Types.ProviderARN
  
import Network.AWS.ApiGateway.Types.Stage
  
import Network.AWS.ApiGateway.Types.ApiKey
  
import Network.AWS.ApiGateway.Types.Op
  
import Network.AWS.ApiGateway.Types.ApiKeysFormat
  
import Network.AWS.ApiGateway.Types.ApiKeySourceType
  
import Network.AWS.ApiGateway.Types.DeploymentCanarySettings
  
import Network.AWS.ApiGateway.Types.DocumentationPartType
  
import Network.AWS.ApiGateway.Types.AccessLogSettings
  
import Network.AWS.ApiGateway.Types.RestApi
  
import Network.AWS.ApiGateway.Types.DocumentationVersion
  
import Network.AWS.ApiGateway.Types.QuotaSettings
  
import Network.AWS.ApiGateway.Types.GatewayResponse
  
import Network.AWS.ApiGateway.Types.EndpointType
  
import Network.AWS.ApiGateway.Types.Integration
  
import Network.AWS.ApiGateway.Types.UsagePlan
  
import Network.AWS.ApiGateway.Types.SecurityPolicy
  
import Network.AWS.ApiGateway.Types.Authorizer
  
import Network.AWS.ApiGateway.Types.DocumentationPartLocationStatusCode
  
  
import Network.AWS.ApiGateway.Types.Account
  
import Network.AWS.ApiGateway.Types.UsagePlanKey
  
import Network.AWS.ApiGateway.Types.TlsConfig
  
import Network.AWS.ApiGateway.Types.SdkType
  
import Network.AWS.ApiGateway.Types.ClientCertificate
  
import Network.AWS.ApiGateway.Types.MethodResponse
  
  
import Network.AWS.ApiGateway.Types.ApiStage
  
import Network.AWS.ApiGateway.Types.BasePathMapping
  
import Network.AWS.ApiGateway.Types.RequestValidator
  
import Network.AWS.ApiGateway.Types.DomainName
  
import Network.AWS.ApiGateway.Types.Method
  
import Network.AWS.ApiGateway.Types.Model
  
import Network.AWS.ApiGateway.Types.MutualTlsAuthentication
  
import Network.AWS.ApiGateway.Types.PutMode
  
  
import Network.AWS.ApiGateway.Types.Resource
  
import Network.AWS.ApiGateway.Types.CacheClusterStatus
  
import Network.AWS.ApiGateway.Types.DocumentationPart
  
import Network.AWS.ApiGateway.Types.IntegrationResponse
  
import Network.AWS.ApiGateway.Types.Usage
  
import Network.AWS.ApiGateway.Types.ThrottleSettings
  
import Network.AWS.ApiGateway.Types.VpcLink
  
import Network.AWS.ApiGateway.Types.ContentHandlingStrategy
  
import Network.AWS.ApiGateway.Types.DocumentationPartLocation
  
  
import Network.AWS.ApiGateway.Types.CacheClusterSize
  
import Network.AWS.ApiGateway.Types.UnauthorizedCacheControlHeaderStrategy
  
import Network.AWS.ApiGateway.Types.StageKey
  
import Network.AWS.ApiGateway.Types.DomainNameStatus
  
import Network.AWS.ApiGateway.Types.CanarySettings
  
  
import Network.AWS.ApiGateway.Types.EndpointConfiguration
  
import Network.AWS.ApiGateway.Types.SdkConfigurationProperty
  
import Network.AWS.ApiGateway.Types.GatewayResponseType
  
import Network.AWS.ApiGateway.Types.QuotaPeriodType
  
import Network.AWS.ApiGateway.Types.MethodSnapshot
  
import Network.AWS.ApiGateway.Types.PatchOperation
  
import Network.AWS.ApiGateway.Types.IntegrationType
  
import Network.AWS.ApiGateway.Types.ConnectionType
  
import Network.AWS.ApiGateway.Types.AuthorizerType
  
import Network.AWS.ApiGateway.Types.LocationStatusType
  
  
import Network.AWS.ApiGateway.Types.Deployment
  
import Network.AWS.ApiGateway.Types.MutualTlsAuthenticationInput
  
import Network.AWS.ApiGateway.Types.StatusCode
  
import Network.AWS.ApiGateway.Types.VpcLinkStatus
  
import Network.AWS.ApiGateway.Types.MethodSetting
  
  

-- | API version @2015-07-09@ of the Amazon API Gateway SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "ApiGateway",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "apigateway",
                 Core._svcVersion = "2015-07-09", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "ApiGateway",
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

-- | The request configuration has conflicts. For details, see the accompanying error message.
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException
  = Core._MatchServiceError mkServiceConfig "ConflictException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _ConflictException #-}
{-# DEPRECATED _ConflictException "Use generic-lens or generic-optics instead"  #-}

-- | The requested resource is not found. Make sure that the request URI is correct.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException
  = Core._MatchServiceError mkServiceConfig "NotFoundException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _NotFoundException #-}
{-# DEPRECATED _NotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The request has reached its throttling limit. Retry after the specified time period.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException
  = Core._MatchServiceError mkServiceConfig
      "TooManyRequestsException"
      Core.. Core.hasStatues 429
{-# INLINEABLE _TooManyRequestsException #-}
{-# DEPRECATED _TooManyRequestsException "Use generic-lens or generic-optics instead"  #-}

-- | The requested service is not available. For details see the accompanying error message. Retry after the specified time period.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException
  = Core._MatchServiceError mkServiceConfig
      "ServiceUnavailableException"
      Core.. Core.hasStatues 503
{-# INLINEABLE _ServiceUnavailableException #-}
{-# DEPRECATED _ServiceUnavailableException "Use generic-lens or generic-optics instead"  #-}

-- | The request is denied because the caller has insufficient permissions.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException
  = Core._MatchServiceError mkServiceConfig "UnauthorizedException"
      Core.. Core.hasStatues 401
{-# INLINEABLE _UnauthorizedException #-}
{-# DEPRECATED _UnauthorizedException "Use generic-lens or generic-optics instead"  #-}

-- | The submitted request is not valid, for example, the input is incomplete or incorrect. See the accompanying error message for details.
_BadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequestException
  = Core._MatchServiceError mkServiceConfig "BadRequestException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _BadRequestException #-}
{-# DEPRECATED _BadRequestException "Use generic-lens or generic-optics instead"  #-}

-- | The request exceeded the rate limit. Retry after the specified time period.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
      Core.. Core.hasStatues 429
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}
