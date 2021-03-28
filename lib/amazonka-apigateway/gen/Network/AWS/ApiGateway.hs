{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon API Gateway__ 
--
-- Amazon API Gateway helps developers deliver robust, secure, and scalable mobile and web application back ends. API Gateway allows developers to securely connect mobile and web applications to APIs that run on AWS Lambda, Amazon EC2, or other publicly addressable web services that are hosted outside of AWS.
module Network.AWS.ApiGateway
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** ConflictException
    , _ConflictException

    -- ** NotFoundException
    , _NotFoundException

    -- ** TooManyRequestsException
    , _TooManyRequestsException

    -- ** ServiceUnavailableException
    , _ServiceUnavailableException

    -- ** UnauthorizedException
    , _UnauthorizedException

    -- ** BadRequestException
    , _BadRequestException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetResource 
    , module Network.AWS.ApiGateway.GetResource

    -- ** GetDeployments (Paginated)
    , module Network.AWS.ApiGateway.GetDeployments

    -- ** GetDeployment 
    , module Network.AWS.ApiGateway.GetDeployment

    -- ** GetTags 
    , module Network.AWS.ApiGateway.GetTags

    -- ** DeleteGatewayResponse 
    , module Network.AWS.ApiGateway.DeleteGatewayResponse

    -- ** UpdateGatewayResponse 
    , module Network.AWS.ApiGateway.UpdateGatewayResponse

    -- ** CreateUsagePlan 
    , module Network.AWS.ApiGateway.CreateUsagePlan

    -- ** GetDomainNames (Paginated)
    , module Network.AWS.ApiGateway.GetDomainNames

    -- ** GetClientCertificate 
    , module Network.AWS.ApiGateway.GetClientCertificate

    -- ** PutGatewayResponse 
    , module Network.AWS.ApiGateway.PutGatewayResponse

    -- ** GetSdkType 
    , module Network.AWS.ApiGateway.GetSdkType

    -- ** GetMethodResponse 
    , module Network.AWS.ApiGateway.GetMethodResponse

    -- ** GetModels (Paginated)
    , module Network.AWS.ApiGateway.GetModels

    -- ** GetBasePathMapping 
    , module Network.AWS.ApiGateway.GetBasePathMapping

    -- ** GetRequestValidators (Paginated)
    , module Network.AWS.ApiGateway.GetRequestValidators

    -- ** PutMethodResponse 
    , module Network.AWS.ApiGateway.PutMethodResponse

    -- ** ImportRestApi 
    , module Network.AWS.ApiGateway.ImportRestApi

    -- ** DeleteMethodResponse 
    , module Network.AWS.ApiGateway.DeleteMethodResponse

    -- ** UpdateMethodResponse 
    , module Network.AWS.ApiGateway.UpdateMethodResponse

    -- ** DeleteStage 
    , module Network.AWS.ApiGateway.DeleteStage

    -- ** UpdateStage 
    , module Network.AWS.ApiGateway.UpdateStage

    -- ** GetRestApis (Paginated)
    , module Network.AWS.ApiGateway.GetRestApis

    -- ** GetDocumentationVersions (Paginated)
    , module Network.AWS.ApiGateway.GetDocumentationVersions

    -- ** CreateDeployment 
    , module Network.AWS.ApiGateway.CreateDeployment

    -- ** GetVpcLinks (Paginated)
    , module Network.AWS.ApiGateway.GetVpcLinks

    -- ** CreateBasePathMapping 
    , module Network.AWS.ApiGateway.CreateBasePathMapping

    -- ** GetIntegration 
    , module Network.AWS.ApiGateway.GetIntegration

    -- ** GetDocumentationParts (Paginated)
    , module Network.AWS.ApiGateway.GetDocumentationParts

    -- ** UpdateAccount 
    , module Network.AWS.ApiGateway.UpdateAccount

    -- ** GetUsagePlan 
    , module Network.AWS.ApiGateway.GetUsagePlan

    -- ** DeleteDeployment 
    , module Network.AWS.ApiGateway.DeleteDeployment

    -- ** UpdateDeployment 
    , module Network.AWS.ApiGateway.UpdateDeployment

    -- ** GetDocumentationPart 
    , module Network.AWS.ApiGateway.GetDocumentationPart

    -- ** DeleteResource 
    , module Network.AWS.ApiGateway.DeleteResource

    -- ** UpdateResource 
    , module Network.AWS.ApiGateway.UpdateResource

    -- ** CreateRequestValidator 
    , module Network.AWS.ApiGateway.CreateRequestValidator

    -- ** ImportDocumentationParts 
    , module Network.AWS.ApiGateway.ImportDocumentationParts

    -- ** GetUsage (Paginated)
    , module Network.AWS.ApiGateway.GetUsage

    -- ** GetVpcLink 
    , module Network.AWS.ApiGateway.GetVpcLink

    -- ** CreateModel 
    , module Network.AWS.ApiGateway.CreateModel

    -- ** GetIntegrationResponse 
    , module Network.AWS.ApiGateway.GetIntegrationResponse

    -- ** CreateDomainName 
    , module Network.AWS.ApiGateway.CreateDomainName

    -- ** FlushStageAuthorizersCache 
    , module Network.AWS.ApiGateway.FlushStageAuthorizersCache

    -- ** GetGatewayResponses (Paginated)
    , module Network.AWS.ApiGateway.GetGatewayResponses

    -- ** DeleteModel 
    , module Network.AWS.ApiGateway.DeleteModel

    -- ** UpdateModel 
    , module Network.AWS.ApiGateway.UpdateModel

    -- ** GetDocumentationVersion 
    , module Network.AWS.ApiGateway.GetDocumentationVersion

    -- ** DeleteApiKey 
    , module Network.AWS.ApiGateway.DeleteApiKey

    -- ** UpdateApiKey 
    , module Network.AWS.ApiGateway.UpdateApiKey

    -- ** GetRestApi 
    , module Network.AWS.ApiGateway.GetRestApi

    -- ** GetStages 
    , module Network.AWS.ApiGateway.GetStages

    -- ** PutRestApi 
    , module Network.AWS.ApiGateway.PutRestApi

    -- ** GetMethod 
    , module Network.AWS.ApiGateway.GetMethod

    -- ** GetModel 
    , module Network.AWS.ApiGateway.GetModel

    -- ** UpdateRestApi 
    , module Network.AWS.ApiGateway.UpdateRestApi

    -- ** DeleteRestApi 
    , module Network.AWS.ApiGateway.DeleteRestApi

    -- ** ImportApiKeys 
    , module Network.AWS.ApiGateway.ImportApiKeys

    -- ** CreateDocumentationPart 
    , module Network.AWS.ApiGateway.CreateDocumentationPart

    -- ** TestInvokeMethod 
    , module Network.AWS.ApiGateway.TestInvokeMethod

    -- ** GetRequestValidator 
    , module Network.AWS.ApiGateway.GetRequestValidator

    -- ** GetDomainName 
    , module Network.AWS.ApiGateway.GetDomainName

    -- ** CreateVpcLink 
    , module Network.AWS.ApiGateway.CreateVpcLink

    -- ** DeleteDocumentationPart 
    , module Network.AWS.ApiGateway.DeleteDocumentationPart

    -- ** UpdateDocumentationPart 
    , module Network.AWS.ApiGateway.UpdateDocumentationPart

    -- ** GetAuthorizers (Paginated)
    , module Network.AWS.ApiGateway.GetAuthorizers

    -- ** CreateDocumentationVersion 
    , module Network.AWS.ApiGateway.CreateDocumentationVersion

    -- ** PutIntegrationResponse 
    , module Network.AWS.ApiGateway.PutIntegrationResponse

    -- ** GetUsagePlanKeys (Paginated)
    , module Network.AWS.ApiGateway.GetUsagePlanKeys

    -- ** DeleteVpcLink 
    , module Network.AWS.ApiGateway.DeleteVpcLink

    -- ** UpdateVpcLink 
    , module Network.AWS.ApiGateway.UpdateVpcLink

    -- ** FlushStageCache 
    , module Network.AWS.ApiGateway.FlushStageCache

    -- ** CreateRestApi 
    , module Network.AWS.ApiGateway.CreateRestApi

    -- ** DeleteIntegrationResponse 
    , module Network.AWS.ApiGateway.DeleteIntegrationResponse

    -- ** UpdateIntegrationResponse 
    , module Network.AWS.ApiGateway.UpdateIntegrationResponse

    -- ** UpdateUsage 
    , module Network.AWS.ApiGateway.UpdateUsage

    -- ** DeleteIntegration 
    , module Network.AWS.ApiGateway.DeleteIntegration

    -- ** UpdateIntegration 
    , module Network.AWS.ApiGateway.UpdateIntegration

    -- ** TestInvokeAuthorizer 
    , module Network.AWS.ApiGateway.TestInvokeAuthorizer

    -- ** GenerateClientCertificate 
    , module Network.AWS.ApiGateway.GenerateClientCertificate

    -- ** GetResources (Paginated)
    , module Network.AWS.ApiGateway.GetResources

    -- ** GetUsagePlanKey 
    , module Network.AWS.ApiGateway.GetUsagePlanKey

    -- ** GetAccount 
    , module Network.AWS.ApiGateway.GetAccount

    -- ** PutIntegration 
    , module Network.AWS.ApiGateway.PutIntegration

    -- ** GetAuthorizer 
    , module Network.AWS.ApiGateway.GetAuthorizer

    -- ** DeleteUsagePlan 
    , module Network.AWS.ApiGateway.DeleteUsagePlan

    -- ** UpdateUsagePlan 
    , module Network.AWS.ApiGateway.UpdateUsagePlan

    -- ** GetStage 
    , module Network.AWS.ApiGateway.GetStage

    -- ** GetExport 
    , module Network.AWS.ApiGateway.GetExport

    -- ** GetSdk 
    , module Network.AWS.ApiGateway.GetSdk

    -- ** GetApiKeys (Paginated)
    , module Network.AWS.ApiGateway.GetApiKeys

    -- ** DeleteBasePathMapping 
    , module Network.AWS.ApiGateway.DeleteBasePathMapping

    -- ** UpdateBasePathMapping 
    , module Network.AWS.ApiGateway.UpdateBasePathMapping

    -- ** DeleteClientCertificate 
    , module Network.AWS.ApiGateway.DeleteClientCertificate

    -- ** UpdateClientCertificate 
    , module Network.AWS.ApiGateway.UpdateClientCertificate

    -- ** GetGatewayResponse 
    , module Network.AWS.ApiGateway.GetGatewayResponse

    -- ** CreateUsagePlanKey 
    , module Network.AWS.ApiGateway.CreateUsagePlanKey

    -- ** CreateAuthorizer 
    , module Network.AWS.ApiGateway.CreateAuthorizer

    -- ** UpdateAuthorizer 
    , module Network.AWS.ApiGateway.UpdateAuthorizer

    -- ** DeleteAuthorizer 
    , module Network.AWS.ApiGateway.DeleteAuthorizer

    -- ** TagResource 
    , module Network.AWS.ApiGateway.TagResource

    -- ** CreateStage 
    , module Network.AWS.ApiGateway.CreateStage

    -- ** DeleteUsagePlanKey 
    , module Network.AWS.ApiGateway.DeleteUsagePlanKey

    -- ** UntagResource 
    , module Network.AWS.ApiGateway.UntagResource

    -- ** CreateApiKey 
    , module Network.AWS.ApiGateway.CreateApiKey

    -- ** GetUsagePlans (Paginated)
    , module Network.AWS.ApiGateway.GetUsagePlans

    -- ** PutMethod 
    , module Network.AWS.ApiGateway.PutMethod

    -- ** UpdateDomainName 
    , module Network.AWS.ApiGateway.UpdateDomainName

    -- ** DeleteDomainName 
    , module Network.AWS.ApiGateway.DeleteDomainName

    -- ** CreateResource 
    , module Network.AWS.ApiGateway.CreateResource

    -- ** DeleteMethod 
    , module Network.AWS.ApiGateway.DeleteMethod

    -- ** UpdateMethod 
    , module Network.AWS.ApiGateway.UpdateMethod

    -- ** UpdateRequestValidator 
    , module Network.AWS.ApiGateway.UpdateRequestValidator

    -- ** DeleteRequestValidator 
    , module Network.AWS.ApiGateway.DeleteRequestValidator

    -- ** GetSdkTypes (Paginated)
    , module Network.AWS.ApiGateway.GetSdkTypes

    -- ** GetClientCertificates (Paginated)
    , module Network.AWS.ApiGateway.GetClientCertificates

    -- ** GetModelTemplate 
    , module Network.AWS.ApiGateway.GetModelTemplate

    -- ** UpdateDocumentationVersion 
    , module Network.AWS.ApiGateway.UpdateDocumentationVersion

    -- ** DeleteDocumentationVersion 
    , module Network.AWS.ApiGateway.DeleteDocumentationVersion

    -- ** GetBasePathMappings (Paginated)
    , module Network.AWS.ApiGateway.GetBasePathMappings

    -- ** GetApiKey 
    , module Network.AWS.ApiGateway.GetApiKey

    -- * Types

    -- ** ProviderARN
    , ProviderARN (..)

    -- ** Stage
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

    -- ** ApiKey
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

    -- ** Op
    , Op (..)

    -- ** ApiKeysFormat
    , ApiKeysFormat (..)

    -- ** ApiKeySourceType
    , ApiKeySourceType (..)

    -- ** DeploymentCanarySettings
    , DeploymentCanarySettings (..)
    , mkDeploymentCanarySettings
    , dcsPercentTraffic
    , dcsStageVariableOverrides
    , dcsUseStageCache

    -- ** DocumentationPartType
    , DocumentationPartType (..)

    -- ** AccessLogSettings
    , AccessLogSettings (..)
    , mkAccessLogSettings
    , alsDestinationArn
    , alsFormat

    -- ** RestApi
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

    -- ** DocumentationVersion
    , DocumentationVersion (..)
    , mkDocumentationVersion
    , dvCreatedDate
    , dvDescription
    , dvVersion

    -- ** QuotaSettings
    , QuotaSettings (..)
    , mkQuotaSettings
    , qsLimit
    , qsOffset
    , qsPeriod

    -- ** GatewayResponse
    , GatewayResponse (..)
    , mkGatewayResponse
    , grDefaultResponse
    , grResponseParameters
    , grResponseTemplates
    , grResponseType
    , grStatusCode

    -- ** EndpointType
    , EndpointType (..)

    -- ** Integration
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

    -- ** UsagePlan
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

    -- ** SecurityPolicy
    , SecurityPolicy (..)

    -- ** Authorizer
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

    -- ** DocumentationPartLocationStatusCode
    , DocumentationPartLocationStatusCode (..)

    -- ** Account
    , Account (..)
    , mkAccount
    , aApiKeyVersion
    , aCloudwatchRoleArn
    , aFeatures
    , aThrottleSettings

    -- ** UsagePlanKey
    , UsagePlanKey (..)
    , mkUsagePlanKey
    , upkId
    , upkName
    , upkType
    , upkValue

    -- ** TlsConfig
    , TlsConfig (..)
    , mkTlsConfig
    , tcInsecureSkipVerification

    -- ** SdkType
    , SdkType (..)
    , mkSdkType
    , stConfigurationProperties
    , stDescription
    , stFriendlyName
    , stId

    -- ** ClientCertificate
    , ClientCertificate (..)
    , mkClientCertificate
    , ccClientCertificateId
    , ccCreatedDate
    , ccDescription
    , ccExpirationDate
    , ccPemEncodedCertificate
    , ccTags

    -- ** MethodResponse
    , MethodResponse (..)
    , mkMethodResponse
    , mrResponseModels
    , mrResponseParameters
    , mrStatusCode

    -- ** ApiStage
    , ApiStage (..)
    , mkApiStage
    , asApiId
    , asStage
    , asThrottle

    -- ** BasePathMapping
    , BasePathMapping (..)
    , mkBasePathMapping
    , bpmBasePath
    , bpmRestApiId
    , bpmStage

    -- ** RequestValidator
    , RequestValidator (..)
    , mkRequestValidator
    , rvId
    , rvName
    , rvValidateRequestBody
    , rvValidateRequestParameters

    -- ** DomainName
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

    -- ** Method
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

    -- ** Model
    , Model (..)
    , mkModel
    , mContentType
    , mDescription
    , mId
    , mName
    , mSchema

    -- ** MutualTlsAuthentication
    , MutualTlsAuthentication (..)
    , mkMutualTlsAuthentication
    , mtaTruststoreUri
    , mtaTruststoreVersion
    , mtaTruststoreWarnings

    -- ** PutMode
    , PutMode (..)

    -- ** Resource
    , Resource (..)
    , mkResource
    , rId
    , rParentId
    , rPath
    , rPathPart
    , rResourceMethods

    -- ** CacheClusterStatus
    , CacheClusterStatus (..)

    -- ** DocumentationPart
    , DocumentationPart (..)
    , mkDocumentationPart
    , dpId
    , dpLocation
    , dpProperties

    -- ** IntegrationResponse
    , IntegrationResponse (..)
    , mkIntegrationResponse
    , irContentHandling
    , irResponseParameters
    , irResponseTemplates
    , irSelectionPattern
    , irStatusCode

    -- ** Usage
    , Usage (..)
    , mkUsage
    , uEndDate
    , uItems
    , uPosition
    , uStartDate
    , uUsagePlanId

    -- ** ThrottleSettings
    , ThrottleSettings (..)
    , mkThrottleSettings
    , tsBurstLimit
    , tsRateLimit

    -- ** VpcLink
    , VpcLink (..)
    , mkVpcLink
    , vlDescription
    , vlId
    , vlName
    , vlStatus
    , vlStatusMessage
    , vlTags
    , vlTargetArns

    -- ** ContentHandlingStrategy
    , ContentHandlingStrategy (..)

    -- ** DocumentationPartLocation
    , DocumentationPartLocation (..)
    , mkDocumentationPartLocation
    , dplType
    , dplMethod
    , dplName
    , dplPath
    , dplStatusCode

    -- ** CacheClusterSize
    , CacheClusterSize (..)

    -- ** UnauthorizedCacheControlHeaderStrategy
    , UnauthorizedCacheControlHeaderStrategy (..)

    -- ** StageKey
    , StageKey (..)
    , mkStageKey
    , skRestApiId
    , skStageName

    -- ** DomainNameStatus
    , DomainNameStatus (..)

    -- ** CanarySettings
    , CanarySettings (..)
    , mkCanarySettings
    , csDeploymentId
    , csPercentTraffic
    , csStageVariableOverrides
    , csUseStageCache

    -- ** EndpointConfiguration
    , EndpointConfiguration (..)
    , mkEndpointConfiguration
    , ecTypes
    , ecVpcEndpointIds

    -- ** SdkConfigurationProperty
    , SdkConfigurationProperty (..)
    , mkSdkConfigurationProperty
    , scpDefaultValue
    , scpDescription
    , scpFriendlyName
    , scpName
    , scpRequired

    -- ** GatewayResponseType
    , GatewayResponseType (..)

    -- ** QuotaPeriodType
    , QuotaPeriodType (..)

    -- ** MethodSnapshot
    , MethodSnapshot (..)
    , mkMethodSnapshot
    , msApiKeyRequired
    , msAuthorizationType

    -- ** PatchOperation
    , PatchOperation (..)
    , mkPatchOperation
    , poFrom
    , poOp
    , poPath
    , poValue

    -- ** IntegrationType
    , IntegrationType (..)

    -- ** ConnectionType
    , ConnectionType (..)

    -- ** AuthorizerType
    , AuthorizerType (..)

    -- ** LocationStatusType
    , LocationStatusType (..)

    -- ** Deployment
    , Deployment (..)
    , mkDeployment
    , dApiSummary
    , dCreatedDate
    , dDescription
    , dId

    -- ** MutualTlsAuthenticationInput
    , MutualTlsAuthenticationInput (..)
    , mkMutualTlsAuthenticationInput
    , mtaiTruststoreUri
    , mtaiTruststoreVersion

    -- ** StatusCode
    , StatusCode (..)

    -- ** VpcLinkStatus
    , VpcLinkStatus (..)

    -- ** MethodSetting
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

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.ApiGateway.Types
import Network.AWS.ApiGateway.Waiters
import Network.AWS.ApiGateway.GetResource
import Network.AWS.ApiGateway.GetDeployments
import Network.AWS.ApiGateway.GetDeployment
import Network.AWS.ApiGateway.GetTags
import Network.AWS.ApiGateway.DeleteGatewayResponse
import Network.AWS.ApiGateway.UpdateGatewayResponse
import Network.AWS.ApiGateway.CreateUsagePlan
import Network.AWS.ApiGateway.GetDomainNames
import Network.AWS.ApiGateway.GetClientCertificate
import Network.AWS.ApiGateway.PutGatewayResponse
import Network.AWS.ApiGateway.GetSdkType
import Network.AWS.ApiGateway.GetMethodResponse
import Network.AWS.ApiGateway.GetModels
import Network.AWS.ApiGateway.GetBasePathMapping
import Network.AWS.ApiGateway.GetRequestValidators
import Network.AWS.ApiGateway.PutMethodResponse
import Network.AWS.ApiGateway.ImportRestApi
import Network.AWS.ApiGateway.DeleteMethodResponse
import Network.AWS.ApiGateway.UpdateMethodResponse
import Network.AWS.ApiGateway.DeleteStage
import Network.AWS.ApiGateway.UpdateStage
import Network.AWS.ApiGateway.GetRestApis
import Network.AWS.ApiGateway.GetDocumentationVersions
import Network.AWS.ApiGateway.CreateDeployment
import Network.AWS.ApiGateway.GetVpcLinks
import Network.AWS.ApiGateway.CreateBasePathMapping
import Network.AWS.ApiGateway.GetIntegration
import Network.AWS.ApiGateway.GetDocumentationParts
import Network.AWS.ApiGateway.UpdateAccount
import Network.AWS.ApiGateway.GetUsagePlan
import Network.AWS.ApiGateway.DeleteDeployment
import Network.AWS.ApiGateway.UpdateDeployment
import Network.AWS.ApiGateway.GetDocumentationPart
import Network.AWS.ApiGateway.DeleteResource
import Network.AWS.ApiGateway.UpdateResource
import Network.AWS.ApiGateway.CreateRequestValidator
import Network.AWS.ApiGateway.ImportDocumentationParts
import Network.AWS.ApiGateway.GetUsage
import Network.AWS.ApiGateway.GetVpcLink
import Network.AWS.ApiGateway.CreateModel
import Network.AWS.ApiGateway.GetIntegrationResponse
import Network.AWS.ApiGateway.CreateDomainName
import Network.AWS.ApiGateway.FlushStageAuthorizersCache
import Network.AWS.ApiGateway.GetGatewayResponses
import Network.AWS.ApiGateway.DeleteModel
import Network.AWS.ApiGateway.UpdateModel
import Network.AWS.ApiGateway.GetDocumentationVersion
import Network.AWS.ApiGateway.DeleteApiKey
import Network.AWS.ApiGateway.UpdateApiKey
import Network.AWS.ApiGateway.GetRestApi
import Network.AWS.ApiGateway.GetStages
import Network.AWS.ApiGateway.PutRestApi
import Network.AWS.ApiGateway.GetMethod
import Network.AWS.ApiGateway.GetModel
import Network.AWS.ApiGateway.UpdateRestApi
import Network.AWS.ApiGateway.DeleteRestApi
import Network.AWS.ApiGateway.ImportApiKeys
import Network.AWS.ApiGateway.CreateDocumentationPart
import Network.AWS.ApiGateway.TestInvokeMethod
import Network.AWS.ApiGateway.GetRequestValidator
import Network.AWS.ApiGateway.GetDomainName
import Network.AWS.ApiGateway.CreateVpcLink
import Network.AWS.ApiGateway.DeleteDocumentationPart
import Network.AWS.ApiGateway.UpdateDocumentationPart
import Network.AWS.ApiGateway.GetAuthorizers
import Network.AWS.ApiGateway.CreateDocumentationVersion
import Network.AWS.ApiGateway.PutIntegrationResponse
import Network.AWS.ApiGateway.GetUsagePlanKeys
import Network.AWS.ApiGateway.DeleteVpcLink
import Network.AWS.ApiGateway.UpdateVpcLink
import Network.AWS.ApiGateway.FlushStageCache
import Network.AWS.ApiGateway.CreateRestApi
import Network.AWS.ApiGateway.DeleteIntegrationResponse
import Network.AWS.ApiGateway.UpdateIntegrationResponse
import Network.AWS.ApiGateway.UpdateUsage
import Network.AWS.ApiGateway.DeleteIntegration
import Network.AWS.ApiGateway.UpdateIntegration
import Network.AWS.ApiGateway.TestInvokeAuthorizer
import Network.AWS.ApiGateway.GenerateClientCertificate
import Network.AWS.ApiGateway.GetResources
import Network.AWS.ApiGateway.GetUsagePlanKey
import Network.AWS.ApiGateway.GetAccount
import Network.AWS.ApiGateway.PutIntegration
import Network.AWS.ApiGateway.GetAuthorizer
import Network.AWS.ApiGateway.DeleteUsagePlan
import Network.AWS.ApiGateway.UpdateUsagePlan
import Network.AWS.ApiGateway.GetStage
import Network.AWS.ApiGateway.GetExport
import Network.AWS.ApiGateway.GetSdk
import Network.AWS.ApiGateway.GetApiKeys
import Network.AWS.ApiGateway.DeleteBasePathMapping
import Network.AWS.ApiGateway.UpdateBasePathMapping
import Network.AWS.ApiGateway.DeleteClientCertificate
import Network.AWS.ApiGateway.UpdateClientCertificate
import Network.AWS.ApiGateway.GetGatewayResponse
import Network.AWS.ApiGateway.CreateUsagePlanKey
import Network.AWS.ApiGateway.CreateAuthorizer
import Network.AWS.ApiGateway.UpdateAuthorizer
import Network.AWS.ApiGateway.DeleteAuthorizer
import Network.AWS.ApiGateway.TagResource
import Network.AWS.ApiGateway.CreateStage
import Network.AWS.ApiGateway.DeleteUsagePlanKey
import Network.AWS.ApiGateway.UntagResource
import Network.AWS.ApiGateway.CreateApiKey
import Network.AWS.ApiGateway.GetUsagePlans
import Network.AWS.ApiGateway.PutMethod
import Network.AWS.ApiGateway.UpdateDomainName
import Network.AWS.ApiGateway.DeleteDomainName
import Network.AWS.ApiGateway.CreateResource
import Network.AWS.ApiGateway.DeleteMethod
import Network.AWS.ApiGateway.UpdateMethod
import Network.AWS.ApiGateway.UpdateRequestValidator
import Network.AWS.ApiGateway.DeleteRequestValidator
import Network.AWS.ApiGateway.GetSdkTypes
import Network.AWS.ApiGateway.GetClientCertificates
import Network.AWS.ApiGateway.GetModelTemplate
import Network.AWS.ApiGateway.UpdateDocumentationVersion
import Network.AWS.ApiGateway.DeleteDocumentationVersion
import Network.AWS.ApiGateway.GetBasePathMappings
import Network.AWS.ApiGateway.GetApiKey
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'ApiGateway'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
