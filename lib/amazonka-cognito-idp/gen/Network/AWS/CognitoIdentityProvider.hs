{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Using the Amazon Cognito User Pools API, you can create a user pool to manage directories and users. You can authenticate a user to obtain tokens related to user identity and access policies.
--
-- This API reference provides information about user pools in Amazon Cognito User Pools.
-- For more information, see the Amazon Cognito Documentation.
module Network.AWS.CognitoIdentityProvider
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** UnsupportedUserStateException
    , _UnsupportedUserStateException

    -- ** PasswordResetRequiredException
    , _PasswordResetRequiredException

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** InvalidLambdaResponseException
    , _InvalidLambdaResponseException

    -- ** InvalidEmailRoleAccessPolicyException
    , _InvalidEmailRoleAccessPolicyException

    -- ** UnsupportedIdentityProviderException
    , _UnsupportedIdentityProviderException

    -- ** UserNotFoundException
    , _UserNotFoundException

    -- ** UnexpectedLambdaException
    , _UnexpectedLambdaException

    -- ** NotAuthorizedException
    , _NotAuthorizedException

    -- ** InternalErrorException
    , _InternalErrorException

    -- ** InvalidUserPoolConfigurationException
    , _InvalidUserPoolConfigurationException

    -- ** InvalidSmsRoleAccessPolicyException
    , _InvalidSmsRoleAccessPolicyException

    -- ** InvalidOAuthFlowException
    , _InvalidOAuthFlowException

    -- ** CodeMismatchException
    , _CodeMismatchException

    -- ** UserImportInProgressException
    , _UserImportInProgressException

    -- ** InvalidSmsRoleTrustRelationshipException
    , _InvalidSmsRoleTrustRelationshipException

    -- ** UserPoolTaggingException
    , _UserPoolTaggingException

    -- ** SoftwareTokenMFANotFoundException
    , _SoftwareTokenMFANotFoundException

    -- ** TooManyRequestsException
    , _TooManyRequestsException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** UserPoolAddOnNotEnabledException
    , _UserPoolAddOnNotEnabledException

    -- ** UserLambdaValidationException
    , _UserLambdaValidationException

    -- ** PreconditionNotMetException
    , _PreconditionNotMetException

    -- ** ExpiredCodeException
    , _ExpiredCodeException

    -- ** TooManyFailedAttemptsException
    , _TooManyFailedAttemptsException

    -- ** EnableSoftwareTokenMFAException
    , _EnableSoftwareTokenMFAException

    -- ** UserNotConfirmedException
    , _UserNotConfirmedException

    -- ** GroupExistsException
    , _GroupExistsException

    -- ** CodeDeliveryFailureException
    , _CodeDeliveryFailureException

    -- ** ScopeDoesNotExistException
    , _ScopeDoesNotExistException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** MFAMethodNotFoundException
    , _MFAMethodNotFoundException

    -- ** AliasExistsException
    , _AliasExistsException

    -- ** DuplicateProviderException
    , _DuplicateProviderException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** InvalidPasswordException
    , _InvalidPasswordException

    -- ** UsernameExistsException
    , _UsernameExistsException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteUserPool 
    , module Network.AWS.CognitoIdentityProvider.DeleteUserPool

    -- ** UpdateUserPool 
    , module Network.AWS.CognitoIdentityProvider.UpdateUserPool

    -- ** UpdateUserPoolDomain 
    , module Network.AWS.CognitoIdentityProvider.UpdateUserPoolDomain

    -- ** DeleteUserPoolDomain 
    , module Network.AWS.CognitoIdentityProvider.DeleteUserPoolDomain

    -- ** AdminInitiateAuth 
    , module Network.AWS.CognitoIdentityProvider.AdminInitiateAuth

    -- ** AdminLinkProviderForUser 
    , module Network.AWS.CognitoIdentityProvider.AdminLinkProviderForUser

    -- ** AdminEnableUser 
    , module Network.AWS.CognitoIdentityProvider.AdminEnableUser

    -- ** GetUserAttributeVerificationCode 
    , module Network.AWS.CognitoIdentityProvider.GetUserAttributeVerificationCode

    -- ** SetUserPoolMfaConfig 
    , module Network.AWS.CognitoIdentityProvider.SetUserPoolMfaConfig

    -- ** UpdateUserAttributes 
    , module Network.AWS.CognitoIdentityProvider.UpdateUserAttributes

    -- ** DeleteUserAttributes 
    , module Network.AWS.CognitoIdentityProvider.DeleteUserAttributes

    -- ** VerifyUserAttribute 
    , module Network.AWS.CognitoIdentityProvider.VerifyUserAttribute

    -- ** AdminDisableUser 
    , module Network.AWS.CognitoIdentityProvider.AdminDisableUser

    -- ** ConfirmDevice 
    , module Network.AWS.CognitoIdentityProvider.ConfirmDevice

    -- ** ConfirmForgotPassword 
    , module Network.AWS.CognitoIdentityProvider.ConfirmForgotPassword

    -- ** ListUserImportJobs 
    , module Network.AWS.CognitoIdentityProvider.ListUserImportJobs

    -- ** ListTagsForResource 
    , module Network.AWS.CognitoIdentityProvider.ListTagsForResource

    -- ** DescribeIdentityProvider 
    , module Network.AWS.CognitoIdentityProvider.DescribeIdentityProvider

    -- ** ListUsers (Paginated)
    , module Network.AWS.CognitoIdentityProvider.ListUsers

    -- ** AdminDeleteUserAttributes 
    , module Network.AWS.CognitoIdentityProvider.AdminDeleteUserAttributes

    -- ** DescribeUserPoolDomain 
    , module Network.AWS.CognitoIdentityProvider.DescribeUserPoolDomain

    -- ** AdminUpdateUserAttributes 
    , module Network.AWS.CognitoIdentityProvider.AdminUpdateUserAttributes

    -- ** AdminGetUser 
    , module Network.AWS.CognitoIdentityProvider.AdminGetUser

    -- ** AdminUserGlobalSignOut 
    , module Network.AWS.CognitoIdentityProvider.AdminUserGlobalSignOut

    -- ** ListUsersInGroup (Paginated)
    , module Network.AWS.CognitoIdentityProvider.ListUsersInGroup

    -- ** AssociateSoftwareToken 
    , module Network.AWS.CognitoIdentityProvider.AssociateSoftwareToken

    -- ** AdminDisableProviderForUser 
    , module Network.AWS.CognitoIdentityProvider.AdminDisableProviderForUser

    -- ** ForgotPassword 
    , module Network.AWS.CognitoIdentityProvider.ForgotPassword

    -- ** DescribeUserPool 
    , module Network.AWS.CognitoIdentityProvider.DescribeUserPool

    -- ** InitiateAuth 
    , module Network.AWS.CognitoIdentityProvider.InitiateAuth

    -- ** AdminListGroupsForUser (Paginated)
    , module Network.AWS.CognitoIdentityProvider.AdminListGroupsForUser

    -- ** AdminConfirmSignUp 
    , module Network.AWS.CognitoIdentityProvider.AdminConfirmSignUp

    -- ** AdminUpdateAuthEventFeedback 
    , module Network.AWS.CognitoIdentityProvider.AdminUpdateAuthEventFeedback

    -- ** AdminSetUserPassword 
    , module Network.AWS.CognitoIdentityProvider.AdminSetUserPassword

    -- ** StartUserImportJob 
    , module Network.AWS.CognitoIdentityProvider.StartUserImportJob

    -- ** CreateIdentityProvider 
    , module Network.AWS.CognitoIdentityProvider.CreateIdentityProvider

    -- ** SetUICustomization 
    , module Network.AWS.CognitoIdentityProvider.SetUICustomization

    -- ** ListIdentityProviders (Paginated)
    , module Network.AWS.CognitoIdentityProvider.ListIdentityProviders

    -- ** GetDevice 
    , module Network.AWS.CognitoIdentityProvider.GetDevice

    -- ** SignUp 
    , module Network.AWS.CognitoIdentityProvider.SignUp

    -- ** DeleteResourceServer 
    , module Network.AWS.CognitoIdentityProvider.DeleteResourceServer

    -- ** UpdateResourceServer 
    , module Network.AWS.CognitoIdentityProvider.UpdateResourceServer

    -- ** ChangePassword 
    , module Network.AWS.CognitoIdentityProvider.ChangePassword

    -- ** CreateUserPoolDomain 
    , module Network.AWS.CognitoIdentityProvider.CreateUserPoolDomain

    -- ** RespondToAuthChallenge 
    , module Network.AWS.CognitoIdentityProvider.RespondToAuthChallenge

    -- ** CreateUserPool 
    , module Network.AWS.CognitoIdentityProvider.CreateUserPool

    -- ** AdminGetDevice 
    , module Network.AWS.CognitoIdentityProvider.AdminGetDevice

    -- ** GetIdentityProviderByIdentifier 
    , module Network.AWS.CognitoIdentityProvider.GetIdentityProviderByIdentifier

    -- ** AdminRemoveUserFromGroup 
    , module Network.AWS.CognitoIdentityProvider.AdminRemoveUserFromGroup

    -- ** SetRiskConfiguration 
    , module Network.AWS.CognitoIdentityProvider.SetRiskConfiguration

    -- ** ConfirmSignUp 
    , module Network.AWS.CognitoIdentityProvider.ConfirmSignUp

    -- ** ListUserPools (Paginated)
    , module Network.AWS.CognitoIdentityProvider.ListUserPools

    -- ** AdminResetUserPassword 
    , module Network.AWS.CognitoIdentityProvider.AdminResetUserPassword

    -- ** UpdateAuthEventFeedback 
    , module Network.AWS.CognitoIdentityProvider.UpdateAuthEventFeedback

    -- ** CreateUserImportJob 
    , module Network.AWS.CognitoIdentityProvider.CreateUserImportJob

    -- ** GetUser 
    , module Network.AWS.CognitoIdentityProvider.GetUser

    -- ** GetUICustomization 
    , module Network.AWS.CognitoIdentityProvider.GetUICustomization

    -- ** GetCSVHeader 
    , module Network.AWS.CognitoIdentityProvider.GetCSVHeader

    -- ** AdminDeleteUser 
    , module Network.AWS.CognitoIdentityProvider.AdminDeleteUser

    -- ** AdminForgetDevice 
    , module Network.AWS.CognitoIdentityProvider.AdminForgetDevice

    -- ** DescribeResourceServer 
    , module Network.AWS.CognitoIdentityProvider.DescribeResourceServer

    -- ** SetUserMFAPreference 
    , module Network.AWS.CognitoIdentityProvider.SetUserMFAPreference

    -- ** AdminUpdateDeviceStatus 
    , module Network.AWS.CognitoIdentityProvider.AdminUpdateDeviceStatus

    -- ** AdminCreateUser 
    , module Network.AWS.CognitoIdentityProvider.AdminCreateUser

    -- ** AddCustomAttributes 
    , module Network.AWS.CognitoIdentityProvider.AddCustomAttributes

    -- ** ListUserPoolClients (Paginated)
    , module Network.AWS.CognitoIdentityProvider.ListUserPoolClients

    -- ** AdminSetUserMFAPreference 
    , module Network.AWS.CognitoIdentityProvider.AdminSetUserMFAPreference

    -- ** UpdateUserPoolClient 
    , module Network.AWS.CognitoIdentityProvider.UpdateUserPoolClient

    -- ** DeleteUserPoolClient 
    , module Network.AWS.CognitoIdentityProvider.DeleteUserPoolClient

    -- ** UpdateDeviceStatus 
    , module Network.AWS.CognitoIdentityProvider.UpdateDeviceStatus

    -- ** ForgetDevice 
    , module Network.AWS.CognitoIdentityProvider.ForgetDevice

    -- ** GetSigningCertificate 
    , module Network.AWS.CognitoIdentityProvider.GetSigningCertificate

    -- ** DeleteUser 
    , module Network.AWS.CognitoIdentityProvider.DeleteUser

    -- ** TagResource 
    , module Network.AWS.CognitoIdentityProvider.TagResource

    -- ** CreateUserPoolClient 
    , module Network.AWS.CognitoIdentityProvider.CreateUserPoolClient

    -- ** GetUserPoolMfaConfig 
    , module Network.AWS.CognitoIdentityProvider.GetUserPoolMfaConfig

    -- ** CreateResourceServer 
    , module Network.AWS.CognitoIdentityProvider.CreateResourceServer

    -- ** AdminListUserAuthEvents (Paginated)
    , module Network.AWS.CognitoIdentityProvider.AdminListUserAuthEvents

    -- ** CreateGroup 
    , module Network.AWS.CognitoIdentityProvider.CreateGroup

    -- ** AdminAddUserToGroup 
    , module Network.AWS.CognitoIdentityProvider.AdminAddUserToGroup

    -- ** VerifySoftwareToken 
    , module Network.AWS.CognitoIdentityProvider.VerifySoftwareToken

    -- ** UntagResource 
    , module Network.AWS.CognitoIdentityProvider.UntagResource

    -- ** StopUserImportJob 
    , module Network.AWS.CognitoIdentityProvider.StopUserImportJob

    -- ** DescribeUserImportJob 
    , module Network.AWS.CognitoIdentityProvider.DescribeUserImportJob

    -- ** DescribeRiskConfiguration 
    , module Network.AWS.CognitoIdentityProvider.DescribeRiskConfiguration

    -- ** DeleteGroup 
    , module Network.AWS.CognitoIdentityProvider.DeleteGroup

    -- ** UpdateGroup 
    , module Network.AWS.CognitoIdentityProvider.UpdateGroup

    -- ** GlobalSignOut 
    , module Network.AWS.CognitoIdentityProvider.GlobalSignOut

    -- ** ListGroups (Paginated)
    , module Network.AWS.CognitoIdentityProvider.ListGroups

    -- ** UpdateIdentityProvider 
    , module Network.AWS.CognitoIdentityProvider.UpdateIdentityProvider

    -- ** DeleteIdentityProvider 
    , module Network.AWS.CognitoIdentityProvider.DeleteIdentityProvider

    -- ** ListResourceServers (Paginated)
    , module Network.AWS.CognitoIdentityProvider.ListResourceServers

    -- ** AdminRespondToAuthChallenge 
    , module Network.AWS.CognitoIdentityProvider.AdminRespondToAuthChallenge

    -- ** SetUserSettings 
    , module Network.AWS.CognitoIdentityProvider.SetUserSettings

    -- ** AdminListDevices 
    , module Network.AWS.CognitoIdentityProvider.AdminListDevices

    -- ** DescribeUserPoolClient 
    , module Network.AWS.CognitoIdentityProvider.DescribeUserPoolClient

    -- ** ResendConfirmationCode 
    , module Network.AWS.CognitoIdentityProvider.ResendConfirmationCode

    -- ** GetGroup 
    , module Network.AWS.CognitoIdentityProvider.GetGroup

    -- ** AdminSetUserSettings 
    , module Network.AWS.CognitoIdentityProvider.AdminSetUserSettings

    -- ** ListDevices 
    , module Network.AWS.CognitoIdentityProvider.ListDevices

    -- * Types

    -- ** ImageUrlType
    , ImageUrlType (..)

    -- ** UserImportJobNameType
    , UserImportJobNameType (..)

    -- ** SecretHashType
    , SecretHashType (..)

    -- ** EventFilterType
    , EventFilterType (..)

    -- ** EmailConfigurationType
    , EmailConfigurationType (..)
    , mkEmailConfigurationType
    , ectConfigurationSet
    , ectEmailSendingAccount
    , ectFrom
    , ectReplyToEmailAddress
    , ectSourceArn

    -- ** ChallengeName
    , ChallengeName (..)

    -- ** TokenValidityUnitsType
    , TokenValidityUnitsType (..)
    , mkTokenValidityUnitsType
    , tvutAccessToken
    , tvutIdToken
    , tvutRefreshToken

    -- ** AccountRecoverySettingType
    , AccountRecoverySettingType (..)
    , mkAccountRecoverySettingType
    , arstRecoveryMechanisms

    -- ** CompletionMessageType
    , CompletionMessageType (..)

    -- ** IdentityProviderType
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

    -- ** DeviceKeyType
    , DeviceKeyType (..)

    -- ** HttpHeader
    , HttpHeader (..)
    , mkHttpHeader
    , hhHeaderName
    , hhHeaderValue

    -- ** StringAttributeConstraintsType
    , StringAttributeConstraintsType (..)
    , mkStringAttributeConstraintsType
    , sactMaxLength
    , sactMinLength

    -- ** DeviceNameType
    , DeviceNameType (..)

    -- ** PasswordType
    , PasswordType (..)

    -- ** ProviderDescription
    , ProviderDescription (..)
    , mkProviderDescription
    , pdCreationDate
    , pdLastModifiedDate
    , pdProviderName
    , pdProviderType

    -- ** SmsVerificationMessageType
    , SmsVerificationMessageType (..)

    -- ** UserPoolClientDescription
    , UserPoolClientDescription (..)
    , mkUserPoolClientDescription
    , upcdClientId
    , upcdClientName
    , upcdUserPoolId

    -- ** ResourceServerScopeNameType
    , ResourceServerScopeNameType (..)

    -- ** PreventUserExistenceErrorTypes
    , PreventUserExistenceErrorTypes (..)

    -- ** ScopeType
    , ScopeType (..)

    -- ** AliasAttributeType
    , AliasAttributeType (..)

    -- ** AccountTakeoverActionType
    , AccountTakeoverActionType (..)
    , mkAccountTakeoverActionType
    , atatNotify
    , atatEventAction

    -- ** DomainVersionType
    , DomainVersionType (..)

    -- ** SoftwareTokenMFAUserCodeType
    , SoftwareTokenMFAUserCodeType (..)

    -- ** UsernameAttributeType
    , UsernameAttributeType (..)

    -- ** VerifiedAttributeType
    , VerifiedAttributeType (..)

    -- ** SchemaAttributeType
    , SchemaAttributeType (..)
    , mkSchemaAttributeType
    , satAttributeDataType
    , satDeveloperOnlyAttribute
    , satMutable
    , satName
    , satNumberAttributeConstraints
    , satRequired
    , satStringAttributeConstraints

    -- ** EmailVerificationMessageByLinkType
    , EmailVerificationMessageByLinkType (..)

    -- ** SmsConfigurationType
    , SmsConfigurationType (..)
    , mkSmsConfigurationType
    , sctSnsCallerArn
    , sctExternalId

    -- ** EventFeedbackType
    , EventFeedbackType (..)
    , mkEventFeedbackType
    , eftFeedbackValue
    , eftProvider
    , eftFeedbackDate

    -- ** LambdaConfigType
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

    -- ** UserPoolType
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

    -- ** RiskLevelType
    , RiskLevelType (..)

    -- ** OAuthFlowType
    , OAuthFlowType (..)

    -- ** EmailNotificationSubjectType
    , EmailNotificationSubjectType (..)

    -- ** ResourceServerNameType
    , ResourceServerNameType (..)

    -- ** ChallengeResponseType
    , ChallengeResponseType (..)
    , mkChallengeResponseType
    , crtChallengeName
    , crtChallengeResponse

    -- ** RecoveryOptionType
    , RecoveryOptionType (..)
    , mkRecoveryOptionType
    , rotPriority
    , rotName

    -- ** AdminCreateUserConfigType
    , AdminCreateUserConfigType (..)
    , mkAdminCreateUserConfigType
    , acuctAllowAdminCreateUserOnly
    , acuctInviteMessageTemplate
    , acuctUnusedAccountValidityDays

    -- ** CustomEmailLambdaVersionConfigType
    , CustomEmailLambdaVersionConfigType (..)
    , mkCustomEmailLambdaVersionConfigType
    , celvctLambdaVersion
    , celvctLambdaArn

    -- ** VerifySoftwareTokenResponseType
    , VerifySoftwareTokenResponseType (..)

    -- ** GroupNameType
    , GroupNameType (..)

    -- ** DeviceConfigurationType
    , DeviceConfigurationType (..)
    , mkDeviceConfigurationType
    , dctChallengeRequiredOnNewDevice
    , dctDeviceOnlyRememberedOnUserPrompt

    -- ** CompromisedCredentialsEventActionType
    , CompromisedCredentialsEventActionType (..)

    -- ** PaginationKeyType
    , PaginationKeyType (..)

    -- ** RiskDecisionType
    , RiskDecisionType (..)

    -- ** ProviderUserIdentifierType
    , ProviderUserIdentifierType (..)
    , mkProviderUserIdentifierType
    , puitProviderAttributeName
    , puitProviderAttributeValue
    , puitProviderName

    -- ** NotifyConfigurationType
    , NotifyConfigurationType (..)
    , mkNotifyConfigurationType
    , nctSourceArn
    , nctBlockEmail
    , nctFrom
    , nctMfaEmail
    , nctNoActionEmail
    , nctReplyTo

    -- ** CodeDeliveryDetailsType
    , CodeDeliveryDetailsType (..)
    , mkCodeDeliveryDetailsType
    , cddtAttributeName
    , cddtDeliveryMedium
    , cddtDestination

    -- ** AdvancedSecurityModeType
    , AdvancedSecurityModeType (..)

    -- ** AttributeNameType
    , AttributeNameType (..)

    -- ** ResourceServerScopeDescriptionType
    , ResourceServerScopeDescriptionType (..)

    -- ** DescriptionType
    , DescriptionType (..)

    -- ** ClientNameType
    , ClientNameType (..)

    -- ** IdpIdentifierType
    , IdpIdentifierType (..)

    -- ** AnalyticsConfigurationType
    , AnalyticsConfigurationType (..)
    , mkAnalyticsConfigurationType
    , actApplicationArn
    , actApplicationId
    , actExternalId
    , actRoleArn
    , actUserDataShared

    -- ** S3BucketType
    , S3BucketType (..)

    -- ** DomainDescriptionType
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

    -- ** ProviderNameType
    , ProviderNameType (..)

    -- ** SessionType
    , SessionType (..)

    -- ** UsernameConfigurationType
    , UsernameConfigurationType (..)
    , mkUsernameConfigurationType
    , uctCaseSensitive

    -- ** RedirectUrlType
    , RedirectUrlType (..)

    -- ** UserFilterType
    , UserFilterType (..)

    -- ** EmailSendingAccountType
    , EmailSendingAccountType (..)

    -- ** CustomSMSLambdaVersionConfigType
    , CustomSMSLambdaVersionConfigType (..)
    , mkCustomSMSLambdaVersionConfigType
    , csmslvctLambdaVersion
    , csmslvctLambdaArn

    -- ** MFAOptionType
    , MFAOptionType (..)
    , mkMFAOptionType
    , mfaotAttributeName
    , mfaotDeliveryMedium

    -- ** ResourceServerIdentifierType
    , ResourceServerIdentifierType (..)

    -- ** ChallengeNameType
    , ChallengeNameType (..)

    -- ** UserPoolClientType
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

    -- ** AuthEventType
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

    -- ** DomainStatusType
    , DomainStatusType (..)

    -- ** TokenModelType
    , TokenModelType (..)

    -- ** SoftwareTokenMfaConfigType
    , SoftwareTokenMfaConfigType (..)
    , mkSoftwareTokenMfaConfigType
    , stmctEnabled

    -- ** DeviceSecretVerifierConfigType
    , DeviceSecretVerifierConfigType (..)
    , mkDeviceSecretVerifierConfigType
    , dsvctPasswordVerifier
    , dsvctSalt

    -- ** AttributeDataType
    , AttributeDataType (..)

    -- ** ClientIdType
    , ClientIdType (..)

    -- ** EventType
    , EventType (..)

    -- ** DefaultEmailOptionType
    , DefaultEmailOptionType (..)

    -- ** AttributeType
    , AttributeType (..)
    , mkAttributeType
    , atName
    , atValue

    -- ** RiskExceptionConfigurationType
    , RiskExceptionConfigurationType (..)
    , mkRiskExceptionConfigurationType
    , rectBlockedIPRangeList
    , rectSkippedIPRangeList

    -- ** StatusType
    , StatusType (..)

    -- ** ContextDataType
    , ContextDataType (..)
    , mkContextDataType
    , cdtIpAddress
    , cdtServerName
    , cdtServerPath
    , cdtHttpHeaders
    , cdtEncodedData

    -- ** EventRiskType
    , EventRiskType (..)
    , mkEventRiskType
    , ertCompromisedCredentialsDetected
    , ertRiskDecision
    , ertRiskLevel

    -- ** CompromisedCredentialsActionsType
    , CompromisedCredentialsActionsType (..)
    , mkCompromisedCredentialsActionsType
    , ccatEventAction

    -- ** EventResponseType
    , EventResponseType (..)

    -- ** ResourceServerType
    , ResourceServerType (..)
    , mkResourceServerType
    , rstIdentifier
    , rstName
    , rstScopes
    , rstUserPoolId

    -- ** UserPoolMfaType
    , UserPoolMfaType (..)

    -- ** IdentityProviderTypeType
    , IdentityProviderTypeType (..)

    -- ** ExplicitAuthFlowsType
    , ExplicitAuthFlowsType (..)

    -- ** AccountTakeoverRiskConfigurationType
    , AccountTakeoverRiskConfigurationType (..)
    , mkAccountTakeoverRiskConfigurationType
    , atrctActions
    , atrctNotifyConfiguration

    -- ** UserImportJobStatusType
    , UserImportJobStatusType (..)

    -- ** NotifyEmailType
    , NotifyEmailType (..)
    , mkNotifyEmailType
    , netSubject
    , netHtmlBody
    , netTextBody

    -- ** PasswordPolicyType
    , PasswordPolicyType (..)
    , mkPasswordPolicyType
    , pptMinimumLength
    , pptRequireLowercase
    , pptRequireNumbers
    , pptRequireSymbols
    , pptRequireUppercase
    , pptTemporaryPasswordValidityDays

    -- ** GroupType
    , GroupType (..)
    , mkGroupType
    , gtCreationDate
    , gtDescription
    , gtGroupName
    , gtLastModifiedDate
    , gtPrecedence
    , gtRoleArn
    , gtUserPoolId

    -- ** RecoveryOptionNameType
    , RecoveryOptionNameType (..)

    -- ** AttributeMappingKeyType
    , AttributeMappingKeyType (..)

    -- ** ConfirmationCodeType
    , ConfirmationCodeType (..)

    -- ** EmailVerificationSubjectByLinkType
    , EmailVerificationSubjectByLinkType (..)

    -- ** AuthFlowType
    , AuthFlowType (..)

    -- ** DeviceRememberedStatusType
    , DeviceRememberedStatusType (..)

    -- ** FeedbackValueType
    , FeedbackValueType (..)

    -- ** VerificationMessageTemplateType
    , VerificationMessageTemplateType (..)
    , mkVerificationMessageTemplateType
    , vmttDefaultEmailOption
    , vmttEmailMessage
    , vmttEmailMessageByLink
    , vmttEmailSubject
    , vmttEmailSubjectByLink
    , vmttSmsMessage

    -- ** ClientPermissionType
    , ClientPermissionType (..)

    -- ** UserPoolNameType
    , UserPoolNameType (..)

    -- ** SMSMfaSettingsType
    , SMSMfaSettingsType (..)
    , mkSMSMfaSettingsType
    , smsmstEnabled
    , smsmstPreferredMfa

    -- ** UserPoolPolicyType
    , UserPoolPolicyType (..)
    , mkUserPoolPolicyType
    , upptPasswordPolicy

    -- ** ResourceServerScopeType
    , ResourceServerScopeType (..)
    , mkResourceServerScopeType
    , rsstScopeName
    , rsstScopeDescription

    -- ** SmsMfaConfigType
    , SmsMfaConfigType (..)
    , mkSmsMfaConfigType
    , smctSmsAuthenticationMessage
    , smctSmsConfiguration

    -- ** StringType
    , StringType (..)

    -- ** MessageActionType
    , MessageActionType (..)

    -- ** ArnType
    , ArnType (..)

    -- ** UserStatusType
    , UserStatusType (..)

    -- ** DeviceType
    , DeviceType (..)
    , mkDeviceType
    , dtDeviceAttributes
    , dtDeviceCreateDate
    , dtDeviceKey
    , dtDeviceLastAuthenticatedDate
    , dtDeviceLastModifiedDate

    -- ** DomainType
    , DomainType (..)

    -- ** UserPoolIdType
    , UserPoolIdType (..)

    -- ** CustomSMSSenderLambdaVersionType
    , CustomSMSSenderLambdaVersionType (..)

    -- ** TimeUnitsType
    , TimeUnitsType (..)

    -- ** ChallengeResponse
    , ChallengeResponse (..)

    -- ** UserPoolDescriptionType
    , UserPoolDescriptionType (..)
    , mkUserPoolDescriptionType
    , updtCreationDate
    , updtId
    , updtLambdaConfig
    , updtLastModifiedDate
    , updtName
    , updtStatus

    -- ** MessageTemplateType
    , MessageTemplateType (..)
    , mkMessageTemplateType
    , mttEmailMessage
    , mttEmailSubject
    , mttSMSMessage

    -- ** SoftwareTokenMfaSettingsType
    , SoftwareTokenMfaSettingsType (..)
    , mkSoftwareTokenMfaSettingsType
    , stmstEnabled
    , stmstPreferredMfa

    -- ** EventContextDataType
    , EventContextDataType (..)
    , mkEventContextDataType
    , ecdtCity
    , ecdtCountry
    , ecdtDeviceName
    , ecdtIpAddress
    , ecdtTimezone

    -- ** RiskConfigurationType
    , RiskConfigurationType (..)
    , mkRiskConfigurationType
    , rctAccountTakeoverRiskConfiguration
    , rctClientId
    , rctCompromisedCredentialsRiskConfiguration
    , rctLastModifiedDate
    , rctRiskExceptionConfiguration
    , rctUserPoolId

    -- ** AnalyticsMetadataType
    , AnalyticsMetadataType (..)
    , mkAnalyticsMetadataType
    , amtAnalyticsEndpointId

    -- ** CSSType
    , CSSType (..)

    -- ** UserImportJobType
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

    -- ** PaginationKey
    , PaginationKey (..)

    -- ** TagKeysType
    , TagKeysType (..)

    -- ** UserContextDataType
    , UserContextDataType (..)
    , mkUserContextDataType
    , ucdtEncodedData

    -- ** UsernameType
    , UsernameType (..)

    -- ** UserPoolAddOnsType
    , UserPoolAddOnsType (..)
    , mkUserPoolAddOnsType
    , upaotAdvancedSecurityMode

    -- ** AccountTakeoverEventActionType
    , AccountTakeoverEventActionType (..)

    -- ** UICustomizationType
    , UICustomizationType (..)
    , mkUICustomizationType
    , uictCSS
    , uictCSSVersion
    , uictClientId
    , uictCreationDate
    , uictImageUrl
    , uictLastModifiedDate
    , uictUserPoolId

    -- ** CustomEmailSenderLambdaVersionType
    , CustomEmailSenderLambdaVersionType (..)

    -- ** SearchPaginationTokenType
    , SearchPaginationTokenType (..)

    -- ** DeliveryMediumType
    , DeliveryMediumType (..)

    -- ** AccountTakeoverActionsType
    , AccountTakeoverActionsType (..)
    , mkAccountTakeoverActionsType
    , atatHighAction
    , atatLowAction
    , atatMediumAction

    -- ** AuthenticationResultType
    , AuthenticationResultType (..)
    , mkAuthenticationResultType
    , artAccessToken
    , artExpiresIn
    , artIdToken
    , artNewDeviceMetadata
    , artRefreshToken
    , artTokenType

    -- ** CustomDomainConfigType
    , CustomDomainConfigType (..)
    , mkCustomDomainConfigType
    , cdctCertificateArn

    -- ** TagValueType
    , TagValueType (..)

    -- ** UserType
    , UserType (..)
    , mkUserType
    , utAttributes
    , utEnabled
    , utMFAOptions
    , utUserCreateDate
    , utUserLastModifiedDate
    , utUserStatus
    , utUsername

    -- ** NumberAttributeConstraintsType
    , NumberAttributeConstraintsType (..)
    , mkNumberAttributeConstraintsType
    , nactMaxValue
    , nactMinValue

    -- ** NewDeviceMetadataType
    , NewDeviceMetadataType (..)
    , mkNewDeviceMetadataType
    , ndmtDeviceGroupKey
    , ndmtDeviceKey

    -- ** CompromisedCredentialsRiskConfigurationType
    , CompromisedCredentialsRiskConfigurationType (..)
    , mkCompromisedCredentialsRiskConfigurationType
    , ccrctActions
    , ccrctEventFilter

    -- ** ConfigurationSet
    , ConfigurationSet (..)

    -- ** From
    , From (..)

    -- ** ReplyToEmailAddress
    , ReplyToEmailAddress (..)

    -- ** SourceArn
    , SourceArn (..)

    -- ** ProviderName
    , ProviderName (..)

    -- ** UserPoolId
    , UserPoolId (..)

    -- ** Identifier
    , Identifier (..)

    -- ** Name
    , Name (..)

    -- ** Session
    , Session (..)

    -- ** AccessToken
    , AccessToken (..)

    -- ** CloudFrontDomain
    , CloudFrontDomain (..)

    -- ** HeaderName
    , HeaderName (..)

    -- ** HeaderValue
    , HeaderValue (..)

    -- ** MaxLength
    , MaxLength (..)

    -- ** MinLength
    , MinLength (..)

    -- ** NextToken
    , NextToken (..)

    -- ** ClientId
    , ClientId (..)

    -- ** Username
    , Username (..)

    -- ** ClientName
    , ClientName (..)

    -- ** EventId
    , EventId (..)

    -- ** JobId
    , JobId (..)

    -- ** CSS
    , CSS (..)

    -- ** ConfirmationCode
    , ConfirmationCode (..)

    -- ** SnsCallerArn
    , SnsCallerArn (..)

    -- ** ExternalId
    , ExternalId (..)

    -- ** Provider
    , Provider (..)

    -- ** CreateAuthChallenge
    , CreateAuthChallenge (..)

    -- ** CustomMessage
    , CustomMessage (..)

    -- ** DefineAuthChallenge
    , DefineAuthChallenge (..)

    -- ** KMSKeyID
    , KMSKeyID (..)

    -- ** PostAuthentication
    , PostAuthentication (..)

    -- ** PostConfirmation
    , PostConfirmation (..)

    -- ** PreAuthentication
    , PreAuthentication (..)

    -- ** PreSignUp
    , PreSignUp (..)

    -- ** PreTokenGeneration
    , PreTokenGeneration (..)

    -- ** UserMigration
    , UserMigration (..)

    -- ** VerifyAuthChallengeResponse
    , VerifyAuthChallengeResponse (..)

    -- ** Arn
    , Arn (..)

    -- ** CustomDomain
    , CustomDomain (..)

    -- ** Domain
    , Domain (..)

    -- ** EmailConfigurationFailure
    , EmailConfigurationFailure (..)

    -- ** EmailVerificationMessage
    , EmailVerificationMessage (..)

    -- ** EmailVerificationSubject
    , EmailVerificationSubject (..)

    -- ** Id
    , Id (..)

    -- ** SmsConfigurationFailure
    , SmsConfigurationFailure (..)

    -- ** FeedbackToken
    , FeedbackToken (..)

    -- ** PaginationToken
    , PaginationToken (..)

    -- ** PoolName
    , PoolName (..)

    -- ** IdpIdentifier
    , IdpIdentifier (..)

    -- ** LambdaArn
    , LambdaArn (..)

    -- ** PreferredMfaSetting
    , PreferredMfaSetting (..)

    -- ** ProviderAttributeName
    , ProviderAttributeName (..)

    -- ** ProviderAttributeValue
    , ProviderAttributeValue (..)

    -- ** ReplyTo
    , ReplyTo (..)

    -- ** AttributeName
    , AttributeName (..)

    -- ** Destination
    , Destination (..)

    -- ** SecretCode
    , SecretCode (..)

    -- ** CloudWatchLogsRoleArn
    , CloudWatchLogsRoleArn (..)

    -- ** ApplicationArn
    , ApplicationArn (..)

    -- ** ApplicationId
    , ApplicationId (..)

    -- ** RoleArn
    , RoleArn (..)

    -- ** AWSAccountId
    , AWSAccountId (..)

    -- ** CloudFrontDistribution
    , CloudFrontDistribution (..)

    -- ** ResourceArn
    , ResourceArn (..)

    -- ** ClientSecret
    , ClientSecret (..)

    -- ** PasswordVerifier
    , PasswordVerifier (..)

    -- ** Salt
    , Salt (..)

    -- ** Value
    , Value (..)

    -- ** IpAddress
    , IpAddress (..)

    -- ** ServerName
    , ServerName (..)

    -- ** ServerPath
    , ServerPath (..)

    -- ** EncodedData
    , EncodedData (..)

    -- ** HtmlBody
    , HtmlBody (..)

    -- ** TextBody
    , TextBody (..)

    -- ** FriendlyDeviceName
    , FriendlyDeviceName (..)

    -- ** EmailMessage
    , EmailMessage (..)

    -- ** EmailSubject
    , EmailSubject (..)

    -- ** PreSignedUrl
    , PreSignedUrl (..)

    -- ** CSSVersion
    , CSSVersion (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Waiters
import Network.AWS.CognitoIdentityProvider.DeleteUserPool
import Network.AWS.CognitoIdentityProvider.UpdateUserPool
import Network.AWS.CognitoIdentityProvider.UpdateUserPoolDomain
import Network.AWS.CognitoIdentityProvider.DeleteUserPoolDomain
import Network.AWS.CognitoIdentityProvider.AdminInitiateAuth
import Network.AWS.CognitoIdentityProvider.AdminLinkProviderForUser
import Network.AWS.CognitoIdentityProvider.AdminEnableUser
import Network.AWS.CognitoIdentityProvider.GetUserAttributeVerificationCode
import Network.AWS.CognitoIdentityProvider.SetUserPoolMfaConfig
import Network.AWS.CognitoIdentityProvider.UpdateUserAttributes
import Network.AWS.CognitoIdentityProvider.DeleteUserAttributes
import Network.AWS.CognitoIdentityProvider.VerifyUserAttribute
import Network.AWS.CognitoIdentityProvider.AdminDisableUser
import Network.AWS.CognitoIdentityProvider.ConfirmDevice
import Network.AWS.CognitoIdentityProvider.ConfirmForgotPassword
import Network.AWS.CognitoIdentityProvider.ListUserImportJobs
import Network.AWS.CognitoIdentityProvider.ListTagsForResource
import Network.AWS.CognitoIdentityProvider.DescribeIdentityProvider
import Network.AWS.CognitoIdentityProvider.ListUsers
import Network.AWS.CognitoIdentityProvider.AdminDeleteUserAttributes
import Network.AWS.CognitoIdentityProvider.DescribeUserPoolDomain
import Network.AWS.CognitoIdentityProvider.AdminUpdateUserAttributes
import Network.AWS.CognitoIdentityProvider.AdminGetUser
import Network.AWS.CognitoIdentityProvider.AdminUserGlobalSignOut
import Network.AWS.CognitoIdentityProvider.ListUsersInGroup
import Network.AWS.CognitoIdentityProvider.AssociateSoftwareToken
import Network.AWS.CognitoIdentityProvider.AdminDisableProviderForUser
import Network.AWS.CognitoIdentityProvider.ForgotPassword
import Network.AWS.CognitoIdentityProvider.DescribeUserPool
import Network.AWS.CognitoIdentityProvider.InitiateAuth
import Network.AWS.CognitoIdentityProvider.AdminListGroupsForUser
import Network.AWS.CognitoIdentityProvider.AdminConfirmSignUp
import Network.AWS.CognitoIdentityProvider.AdminUpdateAuthEventFeedback
import Network.AWS.CognitoIdentityProvider.AdminSetUserPassword
import Network.AWS.CognitoIdentityProvider.StartUserImportJob
import Network.AWS.CognitoIdentityProvider.CreateIdentityProvider
import Network.AWS.CognitoIdentityProvider.SetUICustomization
import Network.AWS.CognitoIdentityProvider.ListIdentityProviders
import Network.AWS.CognitoIdentityProvider.GetDevice
import Network.AWS.CognitoIdentityProvider.SignUp
import Network.AWS.CognitoIdentityProvider.DeleteResourceServer
import Network.AWS.CognitoIdentityProvider.UpdateResourceServer
import Network.AWS.CognitoIdentityProvider.ChangePassword
import Network.AWS.CognitoIdentityProvider.CreateUserPoolDomain
import Network.AWS.CognitoIdentityProvider.RespondToAuthChallenge
import Network.AWS.CognitoIdentityProvider.CreateUserPool
import Network.AWS.CognitoIdentityProvider.AdminGetDevice
import Network.AWS.CognitoIdentityProvider.GetIdentityProviderByIdentifier
import Network.AWS.CognitoIdentityProvider.AdminRemoveUserFromGroup
import Network.AWS.CognitoIdentityProvider.SetRiskConfiguration
import Network.AWS.CognitoIdentityProvider.ConfirmSignUp
import Network.AWS.CognitoIdentityProvider.ListUserPools
import Network.AWS.CognitoIdentityProvider.AdminResetUserPassword
import Network.AWS.CognitoIdentityProvider.UpdateAuthEventFeedback
import Network.AWS.CognitoIdentityProvider.CreateUserImportJob
import Network.AWS.CognitoIdentityProvider.GetUser
import Network.AWS.CognitoIdentityProvider.GetUICustomization
import Network.AWS.CognitoIdentityProvider.GetCSVHeader
import Network.AWS.CognitoIdentityProvider.AdminDeleteUser
import Network.AWS.CognitoIdentityProvider.AdminForgetDevice
import Network.AWS.CognitoIdentityProvider.DescribeResourceServer
import Network.AWS.CognitoIdentityProvider.SetUserMFAPreference
import Network.AWS.CognitoIdentityProvider.AdminUpdateDeviceStatus
import Network.AWS.CognitoIdentityProvider.AdminCreateUser
import Network.AWS.CognitoIdentityProvider.AddCustomAttributes
import Network.AWS.CognitoIdentityProvider.ListUserPoolClients
import Network.AWS.CognitoIdentityProvider.AdminSetUserMFAPreference
import Network.AWS.CognitoIdentityProvider.UpdateUserPoolClient
import Network.AWS.CognitoIdentityProvider.DeleteUserPoolClient
import Network.AWS.CognitoIdentityProvider.UpdateDeviceStatus
import Network.AWS.CognitoIdentityProvider.ForgetDevice
import Network.AWS.CognitoIdentityProvider.GetSigningCertificate
import Network.AWS.CognitoIdentityProvider.DeleteUser
import Network.AWS.CognitoIdentityProvider.TagResource
import Network.AWS.CognitoIdentityProvider.CreateUserPoolClient
import Network.AWS.CognitoIdentityProvider.GetUserPoolMfaConfig
import Network.AWS.CognitoIdentityProvider.CreateResourceServer
import Network.AWS.CognitoIdentityProvider.AdminListUserAuthEvents
import Network.AWS.CognitoIdentityProvider.CreateGroup
import Network.AWS.CognitoIdentityProvider.AdminAddUserToGroup
import Network.AWS.CognitoIdentityProvider.VerifySoftwareToken
import Network.AWS.CognitoIdentityProvider.UntagResource
import Network.AWS.CognitoIdentityProvider.StopUserImportJob
import Network.AWS.CognitoIdentityProvider.DescribeUserImportJob
import Network.AWS.CognitoIdentityProvider.DescribeRiskConfiguration
import Network.AWS.CognitoIdentityProvider.DeleteGroup
import Network.AWS.CognitoIdentityProvider.UpdateGroup
import Network.AWS.CognitoIdentityProvider.GlobalSignOut
import Network.AWS.CognitoIdentityProvider.ListGroups
import Network.AWS.CognitoIdentityProvider.UpdateIdentityProvider
import Network.AWS.CognitoIdentityProvider.DeleteIdentityProvider
import Network.AWS.CognitoIdentityProvider.ListResourceServers
import Network.AWS.CognitoIdentityProvider.AdminRespondToAuthChallenge
import Network.AWS.CognitoIdentityProvider.SetUserSettings
import Network.AWS.CognitoIdentityProvider.AdminListDevices
import Network.AWS.CognitoIdentityProvider.DescribeUserPoolClient
import Network.AWS.CognitoIdentityProvider.ResendConfirmationCode
import Network.AWS.CognitoIdentityProvider.GetGroup
import Network.AWS.CognitoIdentityProvider.AdminSetUserSettings
import Network.AWS.CognitoIdentityProvider.ListDevices
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'CognitoIdentityProvider'.
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
