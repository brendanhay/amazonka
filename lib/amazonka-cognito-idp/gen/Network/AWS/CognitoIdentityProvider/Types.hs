-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _UnsupportedUserStateException
    , _PasswordResetRequiredException
    , _InvalidParameterException
    , _InvalidLambdaResponseException
    , _InvalidEmailRoleAccessPolicyException
    , _UnsupportedIdentityProviderException
    , _UserNotFoundException
    , _UnexpectedLambdaException
    , _NotAuthorizedException
    , _InternalErrorException
    , _InvalidUserPoolConfigurationException
    , _InvalidSmsRoleAccessPolicyException
    , _InvalidOAuthFlowException
    , _CodeMismatchException
    , _UserImportInProgressException
    , _InvalidSmsRoleTrustRelationshipException
    , _UserPoolTaggingException
    , _SoftwareTokenMFANotFoundException
    , _TooManyRequestsException
    , _ConcurrentModificationException
    , _UserPoolAddOnNotEnabledException
    , _UserLambdaValidationException
    , _PreconditionNotMetException
    , _ExpiredCodeException
    , _TooManyFailedAttemptsException
    , _EnableSoftwareTokenMFAException
    , _UserNotConfirmedException
    , _GroupExistsException
    , _CodeDeliveryFailureException
    , _ScopeDoesNotExistException
    , _ResourceNotFoundException
    , _MFAMethodNotFoundException
    , _AliasExistsException
    , _DuplicateProviderException
    , _LimitExceededException
    , _InvalidPasswordException
    , _UsernameExistsException

    -- * ImageUrlType
    , ImageUrlType (..)

    -- * UserImportJobNameType
    , UserImportJobNameType (..)

    -- * SecretHashType
    , SecretHashType (..)

    -- * EventFilterType
    , EventFilterType (..)

    -- * EmailConfigurationType
    , EmailConfigurationType (..)
    , mkEmailConfigurationType
    , ectConfigurationSet
    , ectEmailSendingAccount
    , ectFrom
    , ectReplyToEmailAddress
    , ectSourceArn

    -- * ChallengeName
    , ChallengeName (..)

    -- * TokenValidityUnitsType
    , TokenValidityUnitsType (..)
    , mkTokenValidityUnitsType
    , tvutAccessToken
    , tvutIdToken
    , tvutRefreshToken

    -- * AccountRecoverySettingType
    , AccountRecoverySettingType (..)
    , mkAccountRecoverySettingType
    , arstRecoveryMechanisms

    -- * CompletionMessageType
    , CompletionMessageType (..)

    -- * IdentityProviderType
    , IdentityProviderType (..)
    , mkIdentityProviderType
    , iptAttributeMapping
    , iptCreationDate
    , iptIdpIdentifiers
    , iptLastModifiedDate
    , iptProviderDetails
    , iptProviderName
    , iptProviderType
    , iptUserPoolId

    -- * DeviceKeyType
    , DeviceKeyType (..)

    -- * HttpHeader
    , HttpHeader (..)
    , mkHttpHeader
    , hhHeaderName
    , hhHeaderValue

    -- * StringAttributeConstraintsType
    , StringAttributeConstraintsType (..)
    , mkStringAttributeConstraintsType
    , sactMaxLength
    , sactMinLength

    -- * DeviceNameType
    , DeviceNameType (..)

    -- * PasswordType
    , PasswordType (..)

    -- * ProviderDescription
    , ProviderDescription (..)
    , mkProviderDescription
    , pdCreationDate
    , pdLastModifiedDate
    , pdProviderName
    , pdProviderType

    -- * SmsVerificationMessageType
    , SmsVerificationMessageType (..)

    -- * UserPoolClientDescription
    , UserPoolClientDescription (..)
    , mkUserPoolClientDescription
    , upcdClientId
    , upcdClientName
    , upcdUserPoolId

    -- * ResourceServerScopeNameType
    , ResourceServerScopeNameType (..)

    -- * PreventUserExistenceErrorTypes
    , PreventUserExistenceErrorTypes (..)

    -- * ScopeType
    , ScopeType (..)

    -- * AliasAttributeType
    , AliasAttributeType (..)

    -- * AccountTakeoverActionType
    , AccountTakeoverActionType (..)
    , mkAccountTakeoverActionType
    , atatNotify
    , atatEventAction

    -- * DomainVersionType
    , DomainVersionType (..)

    -- * SoftwareTokenMFAUserCodeType
    , SoftwareTokenMFAUserCodeType (..)

    -- * UsernameAttributeType
    , UsernameAttributeType (..)

    -- * VerifiedAttributeType
    , VerifiedAttributeType (..)

    -- * SchemaAttributeType
    , SchemaAttributeType (..)
    , mkSchemaAttributeType
    , satAttributeDataType
    , satDeveloperOnlyAttribute
    , satMutable
    , satName
    , satNumberAttributeConstraints
    , satRequired
    , satStringAttributeConstraints

    -- * EmailVerificationMessageByLinkType
    , EmailVerificationMessageByLinkType (..)

    -- * SmsConfigurationType
    , SmsConfigurationType (..)
    , mkSmsConfigurationType
    , sctSnsCallerArn
    , sctExternalId

    -- * EventFeedbackType
    , EventFeedbackType (..)
    , mkEventFeedbackType
    , eftFeedbackValue
    , eftProvider
    , eftFeedbackDate

    -- * LambdaConfigType
    , LambdaConfigType (..)
    , mkLambdaConfigType
    , lctCreateAuthChallenge
    , lctCustomEmailSender
    , lctCustomMessage
    , lctCustomSMSSender
    , lctDefineAuthChallenge
    , lctKMSKeyID
    , lctPostAuthentication
    , lctPostConfirmation
    , lctPreAuthentication
    , lctPreSignUp
    , lctPreTokenGeneration
    , lctUserMigration
    , lctVerifyAuthChallengeResponse

    -- * UserPoolType
    , UserPoolType (..)
    , mkUserPoolType
    , uptAccountRecoverySetting
    , uptAdminCreateUserConfig
    , uptAliasAttributes
    , uptArn
    , uptAutoVerifiedAttributes
    , uptCreationDate
    , uptCustomDomain
    , uptDeviceConfiguration
    , uptDomain
    , uptEmailConfiguration
    , uptEmailConfigurationFailure
    , uptEmailVerificationMessage
    , uptEmailVerificationSubject
    , uptEstimatedNumberOfUsers
    , uptId
    , uptLambdaConfig
    , uptLastModifiedDate
    , uptMfaConfiguration
    , uptName
    , uptPolicies
    , uptSchemaAttributes
    , uptSmsAuthenticationMessage
    , uptSmsConfiguration
    , uptSmsConfigurationFailure
    , uptSmsVerificationMessage
    , uptStatus
    , uptUserPoolAddOns
    , uptUserPoolTags
    , uptUsernameAttributes
    , uptUsernameConfiguration
    , uptVerificationMessageTemplate

    -- * RiskLevelType
    , RiskLevelType (..)

    -- * OAuthFlowType
    , OAuthFlowType (..)

    -- * EmailNotificationSubjectType
    , EmailNotificationSubjectType (..)

    -- * ResourceServerNameType
    , ResourceServerNameType (..)

    -- * ChallengeResponseType
    , ChallengeResponseType (..)
    , mkChallengeResponseType
    , crtChallengeName
    , crtChallengeResponse

    -- * RecoveryOptionType
    , RecoveryOptionType (..)
    , mkRecoveryOptionType
    , rotPriority
    , rotName

    -- * AdminCreateUserConfigType
    , AdminCreateUserConfigType (..)
    , mkAdminCreateUserConfigType
    , acuctAllowAdminCreateUserOnly
    , acuctInviteMessageTemplate
    , acuctUnusedAccountValidityDays

    -- * CustomEmailLambdaVersionConfigType
    , CustomEmailLambdaVersionConfigType (..)
    , mkCustomEmailLambdaVersionConfigType
    , celvctLambdaVersion
    , celvctLambdaArn

    -- * VerifySoftwareTokenResponseType
    , VerifySoftwareTokenResponseType (..)

    -- * GroupNameType
    , GroupNameType (..)

    -- * DeviceConfigurationType
    , DeviceConfigurationType (..)
    , mkDeviceConfigurationType
    , dctChallengeRequiredOnNewDevice
    , dctDeviceOnlyRememberedOnUserPrompt

    -- * CompromisedCredentialsEventActionType
    , CompromisedCredentialsEventActionType (..)

    -- * PaginationKeyType
    , PaginationKeyType (..)

    -- * RiskDecisionType
    , RiskDecisionType (..)

    -- * ProviderUserIdentifierType
    , ProviderUserIdentifierType (..)
    , mkProviderUserIdentifierType
    , puitProviderAttributeName
    , puitProviderAttributeValue
    , puitProviderName

    -- * NotifyConfigurationType
    , NotifyConfigurationType (..)
    , mkNotifyConfigurationType
    , nctSourceArn
    , nctBlockEmail
    , nctFrom
    , nctMfaEmail
    , nctNoActionEmail
    , nctReplyTo

    -- * CodeDeliveryDetailsType
    , CodeDeliveryDetailsType (..)
    , mkCodeDeliveryDetailsType
    , cddtAttributeName
    , cddtDeliveryMedium
    , cddtDestination

    -- * AdvancedSecurityModeType
    , AdvancedSecurityModeType (..)

    -- * AttributeNameType
    , AttributeNameType (..)

    -- * ResourceServerScopeDescriptionType
    , ResourceServerScopeDescriptionType (..)

    -- * DescriptionType
    , DescriptionType (..)

    -- * ClientNameType
    , ClientNameType (..)

    -- * IdpIdentifierType
    , IdpIdentifierType (..)

    -- * AnalyticsConfigurationType
    , AnalyticsConfigurationType (..)
    , mkAnalyticsConfigurationType
    , actApplicationArn
    , actApplicationId
    , actExternalId
    , actRoleArn
    , actUserDataShared

    -- * S3BucketType
    , S3BucketType (..)

    -- * DomainDescriptionType
    , DomainDescriptionType (..)
    , mkDomainDescriptionType
    , ddtAWSAccountId
    , ddtCloudFrontDistribution
    , ddtCustomDomainConfig
    , ddtDomain
    , ddtS3Bucket
    , ddtStatus
    , ddtUserPoolId
    , ddtVersion

    -- * ProviderNameType
    , ProviderNameType (..)

    -- * SessionType
    , SessionType (..)

    -- * UsernameConfigurationType
    , UsernameConfigurationType (..)
    , mkUsernameConfigurationType
    , uctCaseSensitive

    -- * RedirectUrlType
    , RedirectUrlType (..)

    -- * UserFilterType
    , UserFilterType (..)

    -- * EmailSendingAccountType
    , EmailSendingAccountType (..)

    -- * CustomSMSLambdaVersionConfigType
    , CustomSMSLambdaVersionConfigType (..)
    , mkCustomSMSLambdaVersionConfigType
    , csmslvctLambdaVersion
    , csmslvctLambdaArn

    -- * MFAOptionType
    , MFAOptionType (..)
    , mkMFAOptionType
    , mfaotAttributeName
    , mfaotDeliveryMedium

    -- * ResourceServerIdentifierType
    , ResourceServerIdentifierType (..)

    -- * ChallengeNameType
    , ChallengeNameType (..)

    -- * UserPoolClientType
    , UserPoolClientType (..)
    , mkUserPoolClientType
    , upctAccessTokenValidity
    , upctAllowedOAuthFlows
    , upctAllowedOAuthFlowsUserPoolClient
    , upctAllowedOAuthScopes
    , upctAnalyticsConfiguration
    , upctCallbackURLs
    , upctClientId
    , upctClientName
    , upctClientSecret
    , upctCreationDate
    , upctDefaultRedirectURI
    , upctExplicitAuthFlows
    , upctIdTokenValidity
    , upctLastModifiedDate
    , upctLogoutURLs
    , upctPreventUserExistenceErrors
    , upctReadAttributes
    , upctRefreshTokenValidity
    , upctSupportedIdentityProviders
    , upctTokenValidityUnits
    , upctUserPoolId
    , upctWriteAttributes

    -- * AuthEventType
    , AuthEventType (..)
    , mkAuthEventType
    , aetChallengeResponses
    , aetCreationDate
    , aetEventContextData
    , aetEventFeedback
    , aetEventId
    , aetEventResponse
    , aetEventRisk
    , aetEventType

    -- * DomainStatusType
    , DomainStatusType (..)

    -- * TokenModelType
    , TokenModelType (..)

    -- * SoftwareTokenMfaConfigType
    , SoftwareTokenMfaConfigType (..)
    , mkSoftwareTokenMfaConfigType
    , stmctEnabled

    -- * DeviceSecretVerifierConfigType
    , DeviceSecretVerifierConfigType (..)
    , mkDeviceSecretVerifierConfigType
    , dsvctPasswordVerifier
    , dsvctSalt

    -- * AttributeDataType
    , AttributeDataType (..)

    -- * ClientIdType
    , ClientIdType (..)

    -- * EventType
    , EventType (..)

    -- * DefaultEmailOptionType
    , DefaultEmailOptionType (..)

    -- * AttributeType
    , AttributeType (..)
    , mkAttributeType
    , atName
    , atValue

    -- * RiskExceptionConfigurationType
    , RiskExceptionConfigurationType (..)
    , mkRiskExceptionConfigurationType
    , rectBlockedIPRangeList
    , rectSkippedIPRangeList

    -- * StatusType
    , StatusType (..)

    -- * ContextDataType
    , ContextDataType (..)
    , mkContextDataType
    , cdtIpAddress
    , cdtServerName
    , cdtServerPath
    , cdtHttpHeaders
    , cdtEncodedData

    -- * EventRiskType
    , EventRiskType (..)
    , mkEventRiskType
    , ertCompromisedCredentialsDetected
    , ertRiskDecision
    , ertRiskLevel

    -- * CompromisedCredentialsActionsType
    , CompromisedCredentialsActionsType (..)
    , mkCompromisedCredentialsActionsType
    , ccatEventAction

    -- * EventResponseType
    , EventResponseType (..)

    -- * ResourceServerType
    , ResourceServerType (..)
    , mkResourceServerType
    , rstIdentifier
    , rstName
    , rstScopes
    , rstUserPoolId

    -- * UserPoolMfaType
    , UserPoolMfaType (..)

    -- * IdentityProviderTypeType
    , IdentityProviderTypeType (..)

    -- * ExplicitAuthFlowsType
    , ExplicitAuthFlowsType (..)

    -- * AccountTakeoverRiskConfigurationType
    , AccountTakeoverRiskConfigurationType (..)
    , mkAccountTakeoverRiskConfigurationType
    , atrctActions
    , atrctNotifyConfiguration

    -- * UserImportJobStatusType
    , UserImportJobStatusType (..)

    -- * NotifyEmailType
    , NotifyEmailType (..)
    , mkNotifyEmailType
    , netSubject
    , netHtmlBody
    , netTextBody

    -- * PasswordPolicyType
    , PasswordPolicyType (..)
    , mkPasswordPolicyType
    , pptMinimumLength
    , pptRequireLowercase
    , pptRequireNumbers
    , pptRequireSymbols
    , pptRequireUppercase
    , pptTemporaryPasswordValidityDays

    -- * GroupType
    , GroupType (..)
    , mkGroupType
    , gtCreationDate
    , gtDescription
    , gtGroupName
    , gtLastModifiedDate
    , gtPrecedence
    , gtRoleArn
    , gtUserPoolId

    -- * RecoveryOptionNameType
    , RecoveryOptionNameType (..)

    -- * AttributeMappingKeyType
    , AttributeMappingKeyType (..)

    -- * ConfirmationCodeType
    , ConfirmationCodeType (..)

    -- * EmailVerificationSubjectByLinkType
    , EmailVerificationSubjectByLinkType (..)

    -- * AuthFlowType
    , AuthFlowType (..)

    -- * DeviceRememberedStatusType
    , DeviceRememberedStatusType (..)

    -- * FeedbackValueType
    , FeedbackValueType (..)

    -- * VerificationMessageTemplateType
    , VerificationMessageTemplateType (..)
    , mkVerificationMessageTemplateType
    , vmttDefaultEmailOption
    , vmttEmailMessage
    , vmttEmailMessageByLink
    , vmttEmailSubject
    , vmttEmailSubjectByLink
    , vmttSmsMessage

    -- * ClientPermissionType
    , ClientPermissionType (..)

    -- * UserPoolNameType
    , UserPoolNameType (..)

    -- * SMSMfaSettingsType
    , SMSMfaSettingsType (..)
    , mkSMSMfaSettingsType
    , smsmstEnabled
    , smsmstPreferredMfa

    -- * UserPoolPolicyType
    , UserPoolPolicyType (..)
    , mkUserPoolPolicyType
    , upptPasswordPolicy

    -- * ResourceServerScopeType
    , ResourceServerScopeType (..)
    , mkResourceServerScopeType
    , rsstScopeName
    , rsstScopeDescription

    -- * SmsMfaConfigType
    , SmsMfaConfigType (..)
    , mkSmsMfaConfigType
    , smctSmsAuthenticationMessage
    , smctSmsConfiguration

    -- * StringType
    , StringType (..)

    -- * MessageActionType
    , MessageActionType (..)

    -- * ArnType
    , ArnType (..)

    -- * UserStatusType
    , UserStatusType (..)

    -- * DeviceType
    , DeviceType (..)
    , mkDeviceType
    , dtDeviceAttributes
    , dtDeviceCreateDate
    , dtDeviceKey
    , dtDeviceLastAuthenticatedDate
    , dtDeviceLastModifiedDate

    -- * DomainType
    , DomainType (..)

    -- * UserPoolIdType
    , UserPoolIdType (..)

    -- * CustomSMSSenderLambdaVersionType
    , CustomSMSSenderLambdaVersionType (..)

    -- * TimeUnitsType
    , TimeUnitsType (..)

    -- * ChallengeResponse
    , ChallengeResponse (..)

    -- * UserPoolDescriptionType
    , UserPoolDescriptionType (..)
    , mkUserPoolDescriptionType
    , updtCreationDate
    , updtId
    , updtLambdaConfig
    , updtLastModifiedDate
    , updtName
    , updtStatus

    -- * MessageTemplateType
    , MessageTemplateType (..)
    , mkMessageTemplateType
    , mttEmailMessage
    , mttEmailSubject
    , mttSMSMessage

    -- * SoftwareTokenMfaSettingsType
    , SoftwareTokenMfaSettingsType (..)
    , mkSoftwareTokenMfaSettingsType
    , stmstEnabled
    , stmstPreferredMfa

    -- * EventContextDataType
    , EventContextDataType (..)
    , mkEventContextDataType
    , ecdtCity
    , ecdtCountry
    , ecdtDeviceName
    , ecdtIpAddress
    , ecdtTimezone

    -- * RiskConfigurationType
    , RiskConfigurationType (..)
    , mkRiskConfigurationType
    , rctAccountTakeoverRiskConfiguration
    , rctClientId
    , rctCompromisedCredentialsRiskConfiguration
    , rctLastModifiedDate
    , rctRiskExceptionConfiguration
    , rctUserPoolId

    -- * AnalyticsMetadataType
    , AnalyticsMetadataType (..)
    , mkAnalyticsMetadataType
    , amtAnalyticsEndpointId

    -- * CSSType
    , CSSType (..)

    -- * UserImportJobType
    , UserImportJobType (..)
    , mkUserImportJobType
    , uijtCloudWatchLogsRoleArn
    , uijtCompletionDate
    , uijtCompletionMessage
    , uijtCreationDate
    , uijtFailedUsers
    , uijtImportedUsers
    , uijtJobId
    , uijtJobName
    , uijtPreSignedUrl
    , uijtSkippedUsers
    , uijtStartDate
    , uijtStatus
    , uijtUserPoolId

    -- * PaginationKey
    , PaginationKey (..)

    -- * TagKeysType
    , TagKeysType (..)

    -- * UserContextDataType
    , UserContextDataType (..)
    , mkUserContextDataType
    , ucdtEncodedData

    -- * UsernameType
    , UsernameType (..)

    -- * UserPoolAddOnsType
    , UserPoolAddOnsType (..)
    , mkUserPoolAddOnsType
    , upaotAdvancedSecurityMode

    -- * AccountTakeoverEventActionType
    , AccountTakeoverEventActionType (..)

    -- * UICustomizationType
    , UICustomizationType (..)
    , mkUICustomizationType
    , uictCSS
    , uictCSSVersion
    , uictClientId
    , uictCreationDate
    , uictImageUrl
    , uictLastModifiedDate
    , uictUserPoolId

    -- * CustomEmailSenderLambdaVersionType
    , CustomEmailSenderLambdaVersionType (..)

    -- * SearchPaginationTokenType
    , SearchPaginationTokenType (..)

    -- * DeliveryMediumType
    , DeliveryMediumType (..)

    -- * AccountTakeoverActionsType
    , AccountTakeoverActionsType (..)
    , mkAccountTakeoverActionsType
    , atatHighAction
    , atatLowAction
    , atatMediumAction

    -- * AuthenticationResultType
    , AuthenticationResultType (..)
    , mkAuthenticationResultType
    , artAccessToken
    , artExpiresIn
    , artIdToken
    , artNewDeviceMetadata
    , artRefreshToken
    , artTokenType

    -- * CustomDomainConfigType
    , CustomDomainConfigType (..)
    , mkCustomDomainConfigType
    , cdctCertificateArn

    -- * TagValueType
    , TagValueType (..)

    -- * UserType
    , UserType (..)
    , mkUserType
    , utAttributes
    , utEnabled
    , utMFAOptions
    , utUserCreateDate
    , utUserLastModifiedDate
    , utUserStatus
    , utUsername

    -- * NumberAttributeConstraintsType
    , NumberAttributeConstraintsType (..)
    , mkNumberAttributeConstraintsType
    , nactMaxValue
    , nactMinValue

    -- * NewDeviceMetadataType
    , NewDeviceMetadataType (..)
    , mkNewDeviceMetadataType
    , ndmtDeviceGroupKey
    , ndmtDeviceKey

    -- * CompromisedCredentialsRiskConfigurationType
    , CompromisedCredentialsRiskConfigurationType (..)
    , mkCompromisedCredentialsRiskConfigurationType
    , ccrctActions
    , ccrctEventFilter

    -- * ConfigurationSet
    , ConfigurationSet (..)

    -- * From
    , From (..)

    -- * ReplyToEmailAddress
    , ReplyToEmailAddress (..)

    -- * SourceArn
    , SourceArn (..)

    -- * ProviderName
    , ProviderName (..)

    -- * UserPoolId
    , UserPoolId (..)

    -- * Identifier
    , Identifier (..)

    -- * Name
    , Name (..)

    -- * Session
    , Session (..)

    -- * AccessToken
    , AccessToken (..)

    -- * CloudFrontDomain
    , CloudFrontDomain (..)

    -- * HeaderName
    , HeaderName (..)

    -- * HeaderValue
    , HeaderValue (..)

    -- * MaxLength
    , MaxLength (..)

    -- * MinLength
    , MinLength (..)

    -- * NextToken
    , NextToken (..)

    -- * ClientId
    , ClientId (..)

    -- * Username
    , Username (..)

    -- * ClientName
    , ClientName (..)

    -- * EventId
    , EventId (..)

    -- * JobId
    , JobId (..)

    -- * CSS
    , CSS (..)

    -- * ConfirmationCode
    , ConfirmationCode (..)

    -- * SnsCallerArn
    , SnsCallerArn (..)

    -- * ExternalId
    , ExternalId (..)

    -- * Provider
    , Provider (..)

    -- * CreateAuthChallenge
    , CreateAuthChallenge (..)

    -- * CustomMessage
    , CustomMessage (..)

    -- * DefineAuthChallenge
    , DefineAuthChallenge (..)

    -- * KMSKeyID
    , KMSKeyID (..)

    -- * PostAuthentication
    , PostAuthentication (..)

    -- * PostConfirmation
    , PostConfirmation (..)

    -- * PreAuthentication
    , PreAuthentication (..)

    -- * PreSignUp
    , PreSignUp (..)

    -- * PreTokenGeneration
    , PreTokenGeneration (..)

    -- * UserMigration
    , UserMigration (..)

    -- * VerifyAuthChallengeResponse
    , VerifyAuthChallengeResponse (..)

    -- * Arn
    , Arn (..)

    -- * CustomDomain
    , CustomDomain (..)

    -- * Domain
    , Domain (..)

    -- * EmailConfigurationFailure
    , EmailConfigurationFailure (..)

    -- * EmailVerificationMessage
    , EmailVerificationMessage (..)

    -- * EmailVerificationSubject
    , EmailVerificationSubject (..)

    -- * Id
    , Id (..)

    -- * SmsConfigurationFailure
    , SmsConfigurationFailure (..)

    -- * FeedbackToken
    , FeedbackToken (..)

    -- * PaginationToken
    , PaginationToken (..)

    -- * PoolName
    , PoolName (..)

    -- * IdpIdentifier
    , IdpIdentifier (..)

    -- * LambdaArn
    , LambdaArn (..)

    -- * PreferredMfaSetting
    , PreferredMfaSetting (..)

    -- * ProviderAttributeName
    , ProviderAttributeName (..)

    -- * ProviderAttributeValue
    , ProviderAttributeValue (..)

    -- * ReplyTo
    , ReplyTo (..)

    -- * AttributeName
    , AttributeName (..)

    -- * Destination
    , Destination (..)

    -- * SecretCode
    , SecretCode (..)

    -- * CloudWatchLogsRoleArn
    , CloudWatchLogsRoleArn (..)

    -- * ApplicationArn
    , ApplicationArn (..)

    -- * ApplicationId
    , ApplicationId (..)

    -- * RoleArn
    , RoleArn (..)

    -- * AWSAccountId
    , AWSAccountId (..)

    -- * CloudFrontDistribution
    , CloudFrontDistribution (..)

    -- * ResourceArn
    , ResourceArn (..)

    -- * ClientSecret
    , ClientSecret (..)

    -- * PasswordVerifier
    , PasswordVerifier (..)

    -- * Salt
    , Salt (..)

    -- * Value
    , Value (..)

    -- * IpAddress
    , IpAddress (..)

    -- * ServerName
    , ServerName (..)

    -- * ServerPath
    , ServerPath (..)

    -- * EncodedData
    , EncodedData (..)

    -- * HtmlBody
    , HtmlBody (..)

    -- * TextBody
    , TextBody (..)

    -- * FriendlyDeviceName
    , FriendlyDeviceName (..)

    -- * EmailMessage
    , EmailMessage (..)

    -- * EmailSubject
    , EmailSubject (..)

    -- * PreSignedUrl
    , PreSignedUrl (..)

    -- * CSSVersion
    , CSSVersion (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.CognitoIdentityProvider.Types.ImageUrlType
  
import Network.AWS.CognitoIdentityProvider.Types.UserImportJobNameType
  
import Network.AWS.CognitoIdentityProvider.Types.SecretHashType
  
import Network.AWS.CognitoIdentityProvider.Types.EventFilterType
  
import Network.AWS.CognitoIdentityProvider.Types.EmailConfigurationType
  
import Network.AWS.CognitoIdentityProvider.Types.ChallengeName
  
  
  
import Network.AWS.CognitoIdentityProvider.Types.TokenValidityUnitsType
  
import Network.AWS.CognitoIdentityProvider.Types.AccountRecoverySettingType
  
import Network.AWS.CognitoIdentityProvider.Types.CompletionMessageType
  
import Network.AWS.CognitoIdentityProvider.Types.IdentityProviderType
  
import Network.AWS.CognitoIdentityProvider.Types.DeviceKeyType
  
import Network.AWS.CognitoIdentityProvider.Types.HttpHeader
  
  
import Network.AWS.CognitoIdentityProvider.Types.StringAttributeConstraintsType
  
import Network.AWS.CognitoIdentityProvider.Types.DeviceNameType
  
import Network.AWS.CognitoIdentityProvider.Types.PasswordType
  
import Network.AWS.CognitoIdentityProvider.Types.ProviderDescription
  
import Network.AWS.CognitoIdentityProvider.Types.SmsVerificationMessageType
  
import Network.AWS.CognitoIdentityProvider.Types.UserPoolClientDescription
  
  
import Network.AWS.CognitoIdentityProvider.Types.ResourceServerScopeNameType
  
import Network.AWS.CognitoIdentityProvider.Types.PreventUserExistenceErrorTypes
  
import Network.AWS.CognitoIdentityProvider.Types.ScopeType
  
import Network.AWS.CognitoIdentityProvider.Types.AliasAttributeType
  
import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionType
  
import Network.AWS.CognitoIdentityProvider.Types.DomainVersionType
  
import Network.AWS.CognitoIdentityProvider.Types.SoftwareTokenMFAUserCodeType
  
  
import Network.AWS.CognitoIdentityProvider.Types.UsernameAttributeType
  
import Network.AWS.CognitoIdentityProvider.Types.VerifiedAttributeType
  
  
import Network.AWS.CognitoIdentityProvider.Types.SchemaAttributeType
  
import Network.AWS.CognitoIdentityProvider.Types.EmailVerificationMessageByLinkType
  
import Network.AWS.CognitoIdentityProvider.Types.SmsConfigurationType
  
  
import Network.AWS.CognitoIdentityProvider.Types.EventFeedbackType
  
import Network.AWS.CognitoIdentityProvider.Types.LambdaConfigType
  
import Network.AWS.CognitoIdentityProvider.Types.UserPoolType
  
import Network.AWS.CognitoIdentityProvider.Types.RiskLevelType
  
  
  
import Network.AWS.CognitoIdentityProvider.Types.OAuthFlowType
  
import Network.AWS.CognitoIdentityProvider.Types.EmailNotificationSubjectType
  
import Network.AWS.CognitoIdentityProvider.Types.ResourceServerNameType
  
import Network.AWS.CognitoIdentityProvider.Types.ChallengeResponseType
  
import Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionType
  
import Network.AWS.CognitoIdentityProvider.Types.AdminCreateUserConfigType
  
import Network.AWS.CognitoIdentityProvider.Types.CustomEmailLambdaVersionConfigType
  
import Network.AWS.CognitoIdentityProvider.Types.VerifySoftwareTokenResponseType
  
import Network.AWS.CognitoIdentityProvider.Types.GroupNameType
  
import Network.AWS.CognitoIdentityProvider.Types.DeviceConfigurationType
  
  
import Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsEventActionType
  
  
import Network.AWS.CognitoIdentityProvider.Types.PaginationKeyType
  
  
import Network.AWS.CognitoIdentityProvider.Types.RiskDecisionType
  
import Network.AWS.CognitoIdentityProvider.Types.ProviderUserIdentifierType
  
import Network.AWS.CognitoIdentityProvider.Types.NotifyConfigurationType
  
import Network.AWS.CognitoIdentityProvider.Types.CodeDeliveryDetailsType
  
import Network.AWS.CognitoIdentityProvider.Types.AdvancedSecurityModeType
  
import Network.AWS.CognitoIdentityProvider.Types.AttributeNameType
  
import Network.AWS.CognitoIdentityProvider.Types.ResourceServerScopeDescriptionType
  
  
import Network.AWS.CognitoIdentityProvider.Types.DescriptionType
  
import Network.AWS.CognitoIdentityProvider.Types.ClientNameType
  
import Network.AWS.CognitoIdentityProvider.Types.IdpIdentifierType
  
import Network.AWS.CognitoIdentityProvider.Types.AnalyticsConfigurationType
  
import Network.AWS.CognitoIdentityProvider.Types.S3BucketType
  
import Network.AWS.CognitoIdentityProvider.Types.DomainDescriptionType
  
import Network.AWS.CognitoIdentityProvider.Types.ProviderNameType
  
import Network.AWS.CognitoIdentityProvider.Types.SessionType
  
import Network.AWS.CognitoIdentityProvider.Types.UsernameConfigurationType
  
  
  
import Network.AWS.CognitoIdentityProvider.Types.RedirectUrlType
  
import Network.AWS.CognitoIdentityProvider.Types.UserFilterType
  
import Network.AWS.CognitoIdentityProvider.Types.EmailSendingAccountType
  
import Network.AWS.CognitoIdentityProvider.Types.CustomSMSLambdaVersionConfigType
  
import Network.AWS.CognitoIdentityProvider.Types.MFAOptionType
  
import Network.AWS.CognitoIdentityProvider.Types.ResourceServerIdentifierType
  
import Network.AWS.CognitoIdentityProvider.Types.ChallengeNameType
  
import Network.AWS.CognitoIdentityProvider.Types.UserPoolClientType
  
import Network.AWS.CognitoIdentityProvider.Types.AuthEventType
  
import Network.AWS.CognitoIdentityProvider.Types.DomainStatusType
  
import Network.AWS.CognitoIdentityProvider.Types.TokenModelType
  
import Network.AWS.CognitoIdentityProvider.Types.SoftwareTokenMfaConfigType
  
import Network.AWS.CognitoIdentityProvider.Types.DeviceSecretVerifierConfigType
  
import Network.AWS.CognitoIdentityProvider.Types.AttributeDataType
  
import Network.AWS.CognitoIdentityProvider.Types.ClientIdType
  
import Network.AWS.CognitoIdentityProvider.Types.EventType
  
import Network.AWS.CognitoIdentityProvider.Types.DefaultEmailOptionType
  
import Network.AWS.CognitoIdentityProvider.Types.AttributeType
  
import Network.AWS.CognitoIdentityProvider.Types.RiskExceptionConfigurationType
  
import Network.AWS.CognitoIdentityProvider.Types.StatusType
  
import Network.AWS.CognitoIdentityProvider.Types.ContextDataType
  
  
import Network.AWS.CognitoIdentityProvider.Types.EventRiskType
  
import Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsActionsType
  
import Network.AWS.CognitoIdentityProvider.Types.EventResponseType
  
import Network.AWS.CognitoIdentityProvider.Types.ResourceServerType
  
  
import Network.AWS.CognitoIdentityProvider.Types.UserPoolMfaType
  
import Network.AWS.CognitoIdentityProvider.Types.IdentityProviderTypeType
  
import Network.AWS.CognitoIdentityProvider.Types.ExplicitAuthFlowsType
  
import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverRiskConfigurationType
  
import Network.AWS.CognitoIdentityProvider.Types.UserImportJobStatusType
  
import Network.AWS.CognitoIdentityProvider.Types.NotifyEmailType
  
import Network.AWS.CognitoIdentityProvider.Types.PasswordPolicyType
  
import Network.AWS.CognitoIdentityProvider.Types.GroupType
  
import Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionNameType
  
import Network.AWS.CognitoIdentityProvider.Types.AttributeMappingKeyType
  
import Network.AWS.CognitoIdentityProvider.Types.ConfirmationCodeType
  
import Network.AWS.CognitoIdentityProvider.Types.EmailVerificationSubjectByLinkType
  
import Network.AWS.CognitoIdentityProvider.Types.AuthFlowType
  
  
  
  
  
  
import Network.AWS.CognitoIdentityProvider.Types.DeviceRememberedStatusType
  
import Network.AWS.CognitoIdentityProvider.Types.FeedbackValueType
  
  
import Network.AWS.CognitoIdentityProvider.Types.VerificationMessageTemplateType
  
import Network.AWS.CognitoIdentityProvider.Types.ClientPermissionType
  
import Network.AWS.CognitoIdentityProvider.Types.UserPoolNameType
  
import Network.AWS.CognitoIdentityProvider.Types.SMSMfaSettingsType
  
import Network.AWS.CognitoIdentityProvider.Types.UserPoolPolicyType
  
import Network.AWS.CognitoIdentityProvider.Types.ResourceServerScopeType
  
import Network.AWS.CognitoIdentityProvider.Types.SmsMfaConfigType
  
import Network.AWS.CognitoIdentityProvider.Types.StringType
  
  
import Network.AWS.CognitoIdentityProvider.Types.MessageActionType
  
import Network.AWS.CognitoIdentityProvider.Types.ArnType
  
import Network.AWS.CognitoIdentityProvider.Types.UserStatusType
  
  
import Network.AWS.CognitoIdentityProvider.Types.DeviceType
  
import Network.AWS.CognitoIdentityProvider.Types.DomainType
  
import Network.AWS.CognitoIdentityProvider.Types.UserPoolIdType
  
import Network.AWS.CognitoIdentityProvider.Types.CustomSMSSenderLambdaVersionType
  
  
import Network.AWS.CognitoIdentityProvider.Types.TimeUnitsType
  
  
  
import Network.AWS.CognitoIdentityProvider.Types.ChallengeResponse
  
import Network.AWS.CognitoIdentityProvider.Types.UserPoolDescriptionType
  
import Network.AWS.CognitoIdentityProvider.Types.MessageTemplateType
  
import Network.AWS.CognitoIdentityProvider.Types.SoftwareTokenMfaSettingsType
  
import Network.AWS.CognitoIdentityProvider.Types.EventContextDataType
  
import Network.AWS.CognitoIdentityProvider.Types.RiskConfigurationType
  
import Network.AWS.CognitoIdentityProvider.Types.AnalyticsMetadataType
  
import Network.AWS.CognitoIdentityProvider.Types.CSSType
  
import Network.AWS.CognitoIdentityProvider.Types.UserImportJobType
  
import Network.AWS.CognitoIdentityProvider.Types.PaginationKey
  
import Network.AWS.CognitoIdentityProvider.Types.TagKeysType
  
import Network.AWS.CognitoIdentityProvider.Types.UserContextDataType
  
  
  
import Network.AWS.CognitoIdentityProvider.Types.UsernameType
  
import Network.AWS.CognitoIdentityProvider.Types.UserPoolAddOnsType
  
import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverEventActionType
  
  
import Network.AWS.CognitoIdentityProvider.Types.UICustomizationType
  
import Network.AWS.CognitoIdentityProvider.Types.CustomEmailSenderLambdaVersionType
  
  
import Network.AWS.CognitoIdentityProvider.Types.SearchPaginationTokenType
  
import Network.AWS.CognitoIdentityProvider.Types.DeliveryMediumType
  
import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionsType
  
import Network.AWS.CognitoIdentityProvider.Types.AuthenticationResultType
  
import Network.AWS.CognitoIdentityProvider.Types.CustomDomainConfigType
  
import Network.AWS.CognitoIdentityProvider.Types.TagValueType
  
  
  
import Network.AWS.CognitoIdentityProvider.Types.UserType
  
import Network.AWS.CognitoIdentityProvider.Types.NumberAttributeConstraintsType
  
  
import Network.AWS.CognitoIdentityProvider.Types.NewDeviceMetadataType
  
  
import Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsRiskConfigurationType
  
  
import Network.AWS.CognitoIdentityProvider.Types.ConfigurationSet
  
import Network.AWS.CognitoIdentityProvider.Types.From
  
import Network.AWS.CognitoIdentityProvider.Types.ReplyToEmailAddress
  
import Network.AWS.CognitoIdentityProvider.Types.SourceArn
  
import Network.AWS.CognitoIdentityProvider.Types.ProviderName
  
import Network.AWS.CognitoIdentityProvider.Types.UserPoolId
  
import Network.AWS.CognitoIdentityProvider.Types.Identifier
  
import Network.AWS.CognitoIdentityProvider.Types.Name
  
import Network.AWS.CognitoIdentityProvider.Types.Session
  
import Network.AWS.CognitoIdentityProvider.Types.AccessToken
  
import Network.AWS.CognitoIdentityProvider.Types.CloudFrontDomain
  
import Network.AWS.CognitoIdentityProvider.Types.HeaderName
  
import Network.AWS.CognitoIdentityProvider.Types.HeaderValue
  
import Network.AWS.CognitoIdentityProvider.Types.MaxLength
  
import Network.AWS.CognitoIdentityProvider.Types.MinLength
  
import Network.AWS.CognitoIdentityProvider.Types.NextToken
  
import Network.AWS.CognitoIdentityProvider.Types.ClientId
  
import Network.AWS.CognitoIdentityProvider.Types.Username
  
import Network.AWS.CognitoIdentityProvider.Types.ClientName
  
import Network.AWS.CognitoIdentityProvider.Types.EventId
  
import Network.AWS.CognitoIdentityProvider.Types.JobId
  
import Network.AWS.CognitoIdentityProvider.Types.CSS
  
import Network.AWS.CognitoIdentityProvider.Types.ConfirmationCode
  
import Network.AWS.CognitoIdentityProvider.Types.SnsCallerArn
  
import Network.AWS.CognitoIdentityProvider.Types.ExternalId
  
import Network.AWS.CognitoIdentityProvider.Types.Provider
  
import Network.AWS.CognitoIdentityProvider.Types.CreateAuthChallenge
  
import Network.AWS.CognitoIdentityProvider.Types.CustomMessage
  
import Network.AWS.CognitoIdentityProvider.Types.DefineAuthChallenge
  
import Network.AWS.CognitoIdentityProvider.Types.KMSKeyID
  
import Network.AWS.CognitoIdentityProvider.Types.PostAuthentication
  
import Network.AWS.CognitoIdentityProvider.Types.PostConfirmation
  
import Network.AWS.CognitoIdentityProvider.Types.PreAuthentication
  
import Network.AWS.CognitoIdentityProvider.Types.PreSignUp
  
import Network.AWS.CognitoIdentityProvider.Types.PreTokenGeneration
  
import Network.AWS.CognitoIdentityProvider.Types.UserMigration
  
import Network.AWS.CognitoIdentityProvider.Types.VerifyAuthChallengeResponse
  
import Network.AWS.CognitoIdentityProvider.Types.Arn
  
import Network.AWS.CognitoIdentityProvider.Types.CustomDomain
  
import Network.AWS.CognitoIdentityProvider.Types.Domain
  
import Network.AWS.CognitoIdentityProvider.Types.EmailConfigurationFailure
  
import Network.AWS.CognitoIdentityProvider.Types.EmailVerificationMessage
  
import Network.AWS.CognitoIdentityProvider.Types.EmailVerificationSubject
  
import Network.AWS.CognitoIdentityProvider.Types.Id
  
import Network.AWS.CognitoIdentityProvider.Types.SmsConfigurationFailure
  
import Network.AWS.CognitoIdentityProvider.Types.FeedbackToken
  
import Network.AWS.CognitoIdentityProvider.Types.PaginationToken
  
import Network.AWS.CognitoIdentityProvider.Types.PoolName
  
import Network.AWS.CognitoIdentityProvider.Types.IdpIdentifier
  
import Network.AWS.CognitoIdentityProvider.Types.LambdaArn
  
import Network.AWS.CognitoIdentityProvider.Types.PreferredMfaSetting
  
import Network.AWS.CognitoIdentityProvider.Types.ProviderAttributeName
  
import Network.AWS.CognitoIdentityProvider.Types.ProviderAttributeValue
  
import Network.AWS.CognitoIdentityProvider.Types.ReplyTo
  
import Network.AWS.CognitoIdentityProvider.Types.AttributeName
  
import Network.AWS.CognitoIdentityProvider.Types.Destination
  
import Network.AWS.CognitoIdentityProvider.Types.SecretCode
  
import Network.AWS.CognitoIdentityProvider.Types.CloudWatchLogsRoleArn
  
import Network.AWS.CognitoIdentityProvider.Types.ApplicationArn
  
import Network.AWS.CognitoIdentityProvider.Types.ApplicationId
  
import Network.AWS.CognitoIdentityProvider.Types.RoleArn
  
import Network.AWS.CognitoIdentityProvider.Types.AWSAccountId
  
import Network.AWS.CognitoIdentityProvider.Types.CloudFrontDistribution
  
import Network.AWS.CognitoIdentityProvider.Types.ResourceArn
  
import Network.AWS.CognitoIdentityProvider.Types.ClientSecret
  
import Network.AWS.CognitoIdentityProvider.Types.PasswordVerifier
  
import Network.AWS.CognitoIdentityProvider.Types.Salt
  
import Network.AWS.CognitoIdentityProvider.Types.Value
  
import Network.AWS.CognitoIdentityProvider.Types.IpAddress
  
import Network.AWS.CognitoIdentityProvider.Types.ServerName
  
import Network.AWS.CognitoIdentityProvider.Types.ServerPath
  
import Network.AWS.CognitoIdentityProvider.Types.EncodedData
  
import Network.AWS.CognitoIdentityProvider.Types.HtmlBody
  
import Network.AWS.CognitoIdentityProvider.Types.TextBody
  
import Network.AWS.CognitoIdentityProvider.Types.FriendlyDeviceName
  
import Network.AWS.CognitoIdentityProvider.Types.EmailMessage
  
import Network.AWS.CognitoIdentityProvider.Types.EmailSubject
  
import Network.AWS.CognitoIdentityProvider.Types.PreSignedUrl
  
import Network.AWS.CognitoIdentityProvider.Types.CSSVersion
  

-- | API version @2016-04-18@ of the Amazon Cognito Identity Provider SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "CognitoIdentityProvider",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "cognito-idp",
                 Core._svcVersion = "2016-04-18", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "CognitoIdentityProvider",
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

-- | The request failed because the user is in an unsupported state.
_UnsupportedUserStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedUserStateException
  = Core._MatchServiceError mkServiceConfig
      "UnsupportedUserStateException"
{-# INLINEABLE _UnsupportedUserStateException #-}
{-# DEPRECATED _UnsupportedUserStateException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when a password reset is required.
_PasswordResetRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PasswordResetRequiredException
  = Core._MatchServiceError mkServiceConfig
      "PasswordResetRequiredException"
{-# INLINEABLE _PasswordResetRequiredException #-}
{-# DEPRECATED _PasswordResetRequiredException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when the Amazon Cognito service encounters an invalid parameter.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException
  = Core._MatchServiceError mkServiceConfig
      "InvalidParameterException"
{-# INLINEABLE _InvalidParameterException #-}
{-# DEPRECATED _InvalidParameterException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when the Amazon Cognito service encounters an invalid AWS Lambda response.
_InvalidLambdaResponseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidLambdaResponseException
  = Core._MatchServiceError mkServiceConfig
      "InvalidLambdaResponseException"
{-# INLINEABLE _InvalidLambdaResponseException #-}
{-# DEPRECATED _InvalidLambdaResponseException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when Amazon Cognito is not allowed to use your email identity. HTTP status code: 400.
_InvalidEmailRoleAccessPolicyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidEmailRoleAccessPolicyException
  = Core._MatchServiceError mkServiceConfig
      "InvalidEmailRoleAccessPolicyException"
{-# INLINEABLE _InvalidEmailRoleAccessPolicyException #-}
{-# DEPRECATED _InvalidEmailRoleAccessPolicyException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when the specified identifier is not supported.
_UnsupportedIdentityProviderException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedIdentityProviderException
  = Core._MatchServiceError mkServiceConfig
      "UnsupportedIdentityProviderException"
{-# INLINEABLE _UnsupportedIdentityProviderException #-}
{-# DEPRECATED _UnsupportedIdentityProviderException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when a user is not found.
_UserNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UserNotFoundException
  = Core._MatchServiceError mkServiceConfig "UserNotFoundException"
{-# INLINEABLE _UserNotFoundException #-}
{-# DEPRECATED _UserNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when the Amazon Cognito service encounters an unexpected exception with the AWS Lambda service.
_UnexpectedLambdaException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnexpectedLambdaException
  = Core._MatchServiceError mkServiceConfig
      "UnexpectedLambdaException"
{-# INLINEABLE _UnexpectedLambdaException #-}
{-# DEPRECATED _UnexpectedLambdaException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when a user is not authorized.
_NotAuthorizedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotAuthorizedException
  = Core._MatchServiceError mkServiceConfig "NotAuthorizedException"
{-# INLINEABLE _NotAuthorizedException #-}
{-# DEPRECATED _NotAuthorizedException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when Amazon Cognito encounters an internal error.
_InternalErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalErrorException
  = Core._MatchServiceError mkServiceConfig "InternalErrorException"
{-# INLINEABLE _InternalErrorException #-}
{-# DEPRECATED _InternalErrorException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when the user pool configuration is invalid.
_InvalidUserPoolConfigurationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidUserPoolConfigurationException
  = Core._MatchServiceError mkServiceConfig
      "InvalidUserPoolConfigurationException"
{-# INLINEABLE _InvalidUserPoolConfigurationException #-}
{-# DEPRECATED _InvalidUserPoolConfigurationException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is returned when the role provided for SMS configuration does not have permission to publish using Amazon SNS.
_InvalidSmsRoleAccessPolicyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSmsRoleAccessPolicyException
  = Core._MatchServiceError mkServiceConfig
      "InvalidSmsRoleAccessPolicyException"
{-# INLINEABLE _InvalidSmsRoleAccessPolicyException #-}
{-# DEPRECATED _InvalidSmsRoleAccessPolicyException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when the specified OAuth flow is invalid.
_InvalidOAuthFlowException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOAuthFlowException
  = Core._MatchServiceError mkServiceConfig
      "InvalidOAuthFlowException"
{-# INLINEABLE _InvalidOAuthFlowException #-}
{-# DEPRECATED _InvalidOAuthFlowException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown if the provided code does not match what the server was expecting.
_CodeMismatchException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CodeMismatchException
  = Core._MatchServiceError mkServiceConfig "CodeMismatchException"
{-# INLINEABLE _CodeMismatchException #-}
{-# DEPRECATED _CodeMismatchException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when you are trying to modify a user pool while a user import job is in progress for that pool.
_UserImportInProgressException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UserImportInProgressException
  = Core._MatchServiceError mkServiceConfig
      "UserImportInProgressException"
{-# INLINEABLE _UserImportInProgressException #-}
{-# DEPRECATED _UserImportInProgressException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when the trust relationship is invalid for the role provided for SMS configuration. This can happen if you do not trust __cognito-idp.amazonaws.com__ or the external ID provided in the role does not match what is provided in the SMS configuration for the user pool.
_InvalidSmsRoleTrustRelationshipException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSmsRoleTrustRelationshipException
  = Core._MatchServiceError mkServiceConfig
      "InvalidSmsRoleTrustRelationshipException"
{-# INLINEABLE _InvalidSmsRoleTrustRelationshipException #-}
{-# DEPRECATED _InvalidSmsRoleTrustRelationshipException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when a user pool tag cannot be set or updated.
_UserPoolTaggingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UserPoolTaggingException
  = Core._MatchServiceError mkServiceConfig
      "UserPoolTaggingException"
{-# INLINEABLE _UserPoolTaggingException #-}
{-# DEPRECATED _UserPoolTaggingException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when the software token TOTP multi-factor authentication (MFA) is not enabled for the user pool.
_SoftwareTokenMFANotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SoftwareTokenMFANotFoundException
  = Core._MatchServiceError mkServiceConfig
      "SoftwareTokenMFANotFoundException"
{-# INLINEABLE _SoftwareTokenMFANotFoundException #-}
{-# DEPRECATED _SoftwareTokenMFANotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when the user has made too many requests for a given operation.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException
  = Core._MatchServiceError mkServiceConfig
      "TooManyRequestsException"
{-# INLINEABLE _TooManyRequestsException #-}
{-# DEPRECATED _TooManyRequestsException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown if two or more modifications are happening concurrently.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException
  = Core._MatchServiceError mkServiceConfig
      "ConcurrentModificationException"
{-# INLINEABLE _ConcurrentModificationException #-}
{-# DEPRECATED _ConcurrentModificationException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when user pool add-ons are not enabled.
_UserPoolAddOnNotEnabledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UserPoolAddOnNotEnabledException
  = Core._MatchServiceError mkServiceConfig
      "UserPoolAddOnNotEnabledException"
{-# INLINEABLE _UserPoolAddOnNotEnabledException #-}
{-# DEPRECATED _UserPoolAddOnNotEnabledException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when the Amazon Cognito service encounters a user validation exception with the AWS Lambda service.
_UserLambdaValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UserLambdaValidationException
  = Core._MatchServiceError mkServiceConfig
      "UserLambdaValidationException"
{-# INLINEABLE _UserLambdaValidationException #-}
{-# DEPRECATED _UserLambdaValidationException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when a precondition is not met.
_PreconditionNotMetException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PreconditionNotMetException
  = Core._MatchServiceError mkServiceConfig
      "PreconditionNotMetException"
{-# INLINEABLE _PreconditionNotMetException #-}
{-# DEPRECATED _PreconditionNotMetException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown if a code has expired.
_ExpiredCodeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ExpiredCodeException
  = Core._MatchServiceError mkServiceConfig "ExpiredCodeException"
{-# INLINEABLE _ExpiredCodeException #-}
{-# DEPRECATED _ExpiredCodeException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when the user has made too many failed attempts for a given action (e.g., sign in).
_TooManyFailedAttemptsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyFailedAttemptsException
  = Core._MatchServiceError mkServiceConfig
      "TooManyFailedAttemptsException"
{-# INLINEABLE _TooManyFailedAttemptsException #-}
{-# DEPRECATED _TooManyFailedAttemptsException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when there is a code mismatch and the service fails to configure the software token TOTP multi-factor authentication (MFA).
_EnableSoftwareTokenMFAException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EnableSoftwareTokenMFAException
  = Core._MatchServiceError mkServiceConfig
      "EnableSoftwareTokenMFAException"
{-# INLINEABLE _EnableSoftwareTokenMFAException #-}
{-# DEPRECATED _EnableSoftwareTokenMFAException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when a user is not confirmed successfully.
_UserNotConfirmedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UserNotConfirmedException
  = Core._MatchServiceError mkServiceConfig
      "UserNotConfirmedException"
{-# INLINEABLE _UserNotConfirmedException #-}
{-# DEPRECATED _UserNotConfirmedException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when Amazon Cognito encounters a group that already exists in the user pool.
_GroupExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_GroupExistsException
  = Core._MatchServiceError mkServiceConfig "GroupExistsException"
{-# INLINEABLE _GroupExistsException #-}
{-# DEPRECATED _GroupExistsException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when a verification code fails to deliver successfully.
_CodeDeliveryFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CodeDeliveryFailureException
  = Core._MatchServiceError mkServiceConfig
      "CodeDeliveryFailureException"
{-# INLINEABLE _CodeDeliveryFailureException #-}
{-# DEPRECATED _CodeDeliveryFailureException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when the specified scope does not exist.
_ScopeDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ScopeDoesNotExistException
  = Core._MatchServiceError mkServiceConfig
      "ScopeDoesNotExistException"
{-# INLINEABLE _ScopeDoesNotExistException #-}
{-# DEPRECATED _ScopeDoesNotExistException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when the Amazon Cognito service cannot find the requested resource.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when Amazon Cognito cannot find a multi-factor authentication (MFA) method.
_MFAMethodNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MFAMethodNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "MFAMethodNotFoundException"
{-# INLINEABLE _MFAMethodNotFoundException #-}
{-# DEPRECATED _MFAMethodNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when a user tries to confirm the account with an email or phone number that has already been supplied as an alias from a different account. This exception tells user that an account with this email or phone already exists.
_AliasExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AliasExistsException
  = Core._MatchServiceError mkServiceConfig "AliasExistsException"
{-# INLINEABLE _AliasExistsException #-}
{-# DEPRECATED _AliasExistsException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when the provider is already supported by the user pool.
_DuplicateProviderException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateProviderException
  = Core._MatchServiceError mkServiceConfig
      "DuplicateProviderException"
{-# INLINEABLE _DuplicateProviderException #-}
{-# DEPRECATED _DuplicateProviderException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when a user exceeds the limit for a requested AWS resource.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when the Amazon Cognito service encounters an invalid password.
_InvalidPasswordException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPasswordException
  = Core._MatchServiceError mkServiceConfig
      "InvalidPasswordException"
{-# INLINEABLE _InvalidPasswordException #-}
{-# DEPRECATED _InvalidPasswordException "Use generic-lens or generic-optics instead"  #-}

-- | This exception is thrown when Amazon Cognito encounters a user name that already exists in the user pool.
_UsernameExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UsernameExistsException
  = Core._MatchServiceError mkServiceConfig "UsernameExistsException"
{-# INLINEABLE _UsernameExistsException #-}
{-# DEPRECATED _UsernameExistsException "Use generic-lens or generic-optics instead"  #-}
