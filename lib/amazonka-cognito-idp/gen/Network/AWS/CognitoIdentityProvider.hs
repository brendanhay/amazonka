{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Using the Amazon Cognito User Pools API, you can create a user pool to manage directories and users. You can authenticate a user to obtain tokens related to user identity and access policies.
--
--
-- This API reference provides information about user pools in Amazon Cognito User Pools.
--
-- For more information, see the Amazon Cognito Documentation.
--
module Network.AWS.CognitoIdentityProvider
    (
    -- * Service Configuration
      cognitoIdentityProvider

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

    -- ** SetUserPoolMFAConfig
    , module Network.AWS.CognitoIdentityProvider.SetUserPoolMFAConfig

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

    -- ** DescribeIdentityProvider
    , module Network.AWS.CognitoIdentityProvider.DescribeIdentityProvider

    -- ** ListUsers
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

    -- ** ListUsersInGroup
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

    -- ** AdminListGroupsForUser
    , module Network.AWS.CognitoIdentityProvider.AdminListGroupsForUser

    -- ** AdminConfirmSignUp
    , module Network.AWS.CognitoIdentityProvider.AdminConfirmSignUp

    -- ** AdminUpdateAuthEventFeedback
    , module Network.AWS.CognitoIdentityProvider.AdminUpdateAuthEventFeedback

    -- ** StartUserImportJob
    , module Network.AWS.CognitoIdentityProvider.StartUserImportJob

    -- ** CreateIdentityProvider
    , module Network.AWS.CognitoIdentityProvider.CreateIdentityProvider

    -- ** SetUICustomization
    , module Network.AWS.CognitoIdentityProvider.SetUICustomization

    -- ** ListIdentityProviders
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

    -- ** ListUserPools
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

    -- ** ListUserPoolClients
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

    -- ** CreateUserPoolClient
    , module Network.AWS.CognitoIdentityProvider.CreateUserPoolClient

    -- ** GetUserPoolMFAConfig
    , module Network.AWS.CognitoIdentityProvider.GetUserPoolMFAConfig

    -- ** CreateResourceServer
    , module Network.AWS.CognitoIdentityProvider.CreateResourceServer

    -- ** AdminListUserAuthEvents
    , module Network.AWS.CognitoIdentityProvider.AdminListUserAuthEvents

    -- ** CreateGroup
    , module Network.AWS.CognitoIdentityProvider.CreateGroup

    -- ** AdminAddUserToGroup
    , module Network.AWS.CognitoIdentityProvider.AdminAddUserToGroup

    -- ** VerifySoftwareToken
    , module Network.AWS.CognitoIdentityProvider.VerifySoftwareToken

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

    -- ** ListGroups
    , module Network.AWS.CognitoIdentityProvider.ListGroups

    -- ** UpdateIdentityProvider
    , module Network.AWS.CognitoIdentityProvider.UpdateIdentityProvider

    -- ** DeleteIdentityProvider
    , module Network.AWS.CognitoIdentityProvider.DeleteIdentityProvider

    -- ** ListResourceServers
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

    -- ** AccountTakeoverEventActionType
    , AccountTakeoverEventActionType (..)

    -- ** AdvancedSecurityModeType
    , AdvancedSecurityModeType (..)

    -- ** AliasAttributeType
    , AliasAttributeType (..)

    -- ** AttributeDataType
    , AttributeDataType (..)

    -- ** AuthFlowType
    , AuthFlowType (..)

    -- ** ChallengeName
    , ChallengeName (..)

    -- ** ChallengeNameType
    , ChallengeNameType (..)

    -- ** ChallengeResponse
    , ChallengeResponse (..)

    -- ** CompromisedCredentialsEventActionType
    , CompromisedCredentialsEventActionType (..)

    -- ** DefaultEmailOptionType
    , DefaultEmailOptionType (..)

    -- ** DeliveryMediumType
    , DeliveryMediumType (..)

    -- ** DeviceRememberedStatusType
    , DeviceRememberedStatusType (..)

    -- ** DomainStatusType
    , DomainStatusType (..)

    -- ** EventFilterType
    , EventFilterType (..)

    -- ** EventResponseType
    , EventResponseType (..)

    -- ** EventType
    , EventType (..)

    -- ** ExplicitAuthFlowsType
    , ExplicitAuthFlowsType (..)

    -- ** FeedbackValueType
    , FeedbackValueType (..)

    -- ** IdentityProviderTypeType
    , IdentityProviderTypeType (..)

    -- ** MessageActionType
    , MessageActionType (..)

    -- ** OAuthFlowType
    , OAuthFlowType (..)

    -- ** RiskDecisionType
    , RiskDecisionType (..)

    -- ** RiskLevelType
    , RiskLevelType (..)

    -- ** StatusType
    , StatusType (..)

    -- ** UserImportJobStatusType
    , UserImportJobStatusType (..)

    -- ** UserPoolMFAType
    , UserPoolMFAType (..)

    -- ** UserStatusType
    , UserStatusType (..)

    -- ** UsernameAttributeType
    , UsernameAttributeType (..)

    -- ** VerifiedAttributeType
    , VerifiedAttributeType (..)

    -- ** VerifySoftwareTokenResponseType
    , VerifySoftwareTokenResponseType (..)

    -- ** AccountTakeoverActionType
    , AccountTakeoverActionType
    , accountTakeoverActionType
    , atatNotify
    , atatEventAction

    -- ** AccountTakeoverActionsType
    , AccountTakeoverActionsType
    , accountTakeoverActionsType
    , atatLowAction
    , atatHighAction
    , atatMediumAction

    -- ** AccountTakeoverRiskConfigurationType
    , AccountTakeoverRiskConfigurationType
    , accountTakeoverRiskConfigurationType
    , atrctNotifyConfiguration
    , atrctActions

    -- ** AdminCreateUserConfigType
    , AdminCreateUserConfigType
    , adminCreateUserConfigType
    , acuctAllowAdminCreateUserOnly
    , acuctUnusedAccountValidityDays
    , acuctInviteMessageTemplate

    -- ** AnalyticsConfigurationType
    , AnalyticsConfigurationType
    , analyticsConfigurationType
    , actUserDataShared
    , actApplicationId
    , actRoleARN
    , actExternalId

    -- ** AnalyticsMetadataType
    , AnalyticsMetadataType
    , analyticsMetadataType
    , amtAnalyticsEndpointId

    -- ** AttributeType
    , AttributeType
    , attributeType
    , atValue
    , atName

    -- ** AuthEventType
    , AuthEventType
    , authEventType
    , aetEventRisk
    , aetEventResponse
    , aetEventContextData
    , aetChallengeResponses
    , aetEventType
    , aetCreationDate
    , aetEventFeedback
    , aetEventId

    -- ** AuthenticationResultType
    , AuthenticationResultType
    , authenticationResultType
    , artAccessToken
    , artRefreshToken
    , artNewDeviceMetadata
    , artExpiresIn
    , artTokenType
    , artIdToken

    -- ** ChallengeResponseType
    , ChallengeResponseType
    , challengeResponseType
    , crtChallengeName
    , crtChallengeResponse

    -- ** CodeDeliveryDetailsType
    , CodeDeliveryDetailsType
    , codeDeliveryDetailsType
    , cddtDestination
    , cddtDeliveryMedium
    , cddtAttributeName

    -- ** CompromisedCredentialsActionsType
    , CompromisedCredentialsActionsType
    , compromisedCredentialsActionsType
    , ccatEventAction

    -- ** CompromisedCredentialsRiskConfigurationType
    , CompromisedCredentialsRiskConfigurationType
    , compromisedCredentialsRiskConfigurationType
    , ccrctEventFilter
    , ccrctActions

    -- ** ContextDataType
    , ContextDataType
    , contextDataType
    , cdtEncodedData
    , cdtIPAddress
    , cdtServerName
    , cdtServerPath
    , cdtHTTPHeaders

    -- ** DeviceConfigurationType
    , DeviceConfigurationType
    , deviceConfigurationType
    , dctChallengeRequiredOnNewDevice
    , dctDeviceOnlyRememberedOnUserPrompt

    -- ** DeviceSecretVerifierConfigType
    , DeviceSecretVerifierConfigType
    , deviceSecretVerifierConfigType
    , dsvctPasswordVerifier
    , dsvctSalt

    -- ** DeviceType
    , DeviceType
    , deviceType
    , dtDeviceLastModifiedDate
    , dtDeviceCreateDate
    , dtDeviceAttributes
    , dtDeviceKey
    , dtDeviceLastAuthenticatedDate

    -- ** DomainDescriptionType
    , DomainDescriptionType
    , domainDescriptionType
    , ddtStatus
    , ddtCloudFrontDistribution
    , ddtUserPoolId
    , ddtDomain
    , ddtAWSAccountId
    , ddtVersion
    , ddtS3Bucket

    -- ** EmailConfigurationType
    , EmailConfigurationType
    , emailConfigurationType
    , ectSourceARN
    , ectReplyToEmailAddress

    -- ** EventContextDataType
    , EventContextDataType
    , eventContextDataType
    , ecdtIPAddress
    , ecdtCountry
    , ecdtCity
    , ecdtDeviceName
    , ecdtTimezone

    -- ** EventFeedbackType
    , EventFeedbackType
    , eventFeedbackType
    , eftFeedbackDate
    , eftFeedbackValue
    , eftProvider

    -- ** EventRiskType
    , EventRiskType
    , eventRiskType
    , ertRiskLevel
    , ertRiskDecision

    -- ** GroupType
    , GroupType
    , groupType
    , gtLastModifiedDate
    , gtUserPoolId
    , gtCreationDate
    , gtPrecedence
    , gtGroupName
    , gtDescription
    , gtRoleARN

    -- ** HTTPHeader
    , HTTPHeader
    , hTTPHeader
    , httphHeaderValue
    , httphHeaderName

    -- ** IdentityProviderType
    , IdentityProviderType
    , identityProviderType
    , iptLastModifiedDate
    , iptUserPoolId
    , iptProviderType
    , iptCreationDate
    , iptIdpIdentifiers
    , iptAttributeMapping
    , iptProviderDetails
    , iptProviderName

    -- ** LambdaConfigType
    , LambdaConfigType
    , lambdaConfigType
    , lctPreAuthentication
    , lctCreateAuthChallenge
    , lctVerifyAuthChallengeResponse
    , lctPostAuthentication
    , lctCustomMessage
    , lctDefineAuthChallenge
    , lctPostConfirmation
    , lctPreTokenGeneration
    , lctUserMigration
    , lctPreSignUp

    -- ** MFAOptionType
    , MFAOptionType
    , mfaOptionType
    , motDeliveryMedium
    , motAttributeName

    -- ** MessageTemplateType
    , MessageTemplateType
    , messageTemplateType
    , mttEmailSubject
    , mttSMSMessage
    , mttEmailMessage

    -- ** NewDeviceMetadataType
    , NewDeviceMetadataType
    , newDeviceMetadataType
    , ndmtDeviceGroupKey
    , ndmtDeviceKey

    -- ** NotifyConfigurationType
    , NotifyConfigurationType
    , notifyConfigurationType
    , nctNoActionEmail
    , nctFrom
    , nctReplyTo
    , nctBlockEmail
    , nctMFAEmail
    , nctSourceARN

    -- ** NotifyEmailType
    , NotifyEmailType
    , notifyEmailType
    , netTextBody
    , netHTMLBody
    , netSubject

    -- ** NumberAttributeConstraintsType
    , NumberAttributeConstraintsType
    , numberAttributeConstraintsType
    , nactMaxValue
    , nactMinValue

    -- ** PasswordPolicyType
    , PasswordPolicyType
    , passwordPolicyType
    , pptRequireNumbers
    , pptRequireUppercase
    , pptRequireLowercase
    , pptMinimumLength
    , pptRequireSymbols

    -- ** ProviderDescription
    , ProviderDescription
    , providerDescription
    , pdLastModifiedDate
    , pdProviderType
    , pdCreationDate
    , pdProviderName

    -- ** ProviderUserIdentifierType
    , ProviderUserIdentifierType
    , providerUserIdentifierType
    , puitProviderAttributeValue
    , puitProviderAttributeName
    , puitProviderName

    -- ** ResourceServerScopeType
    , ResourceServerScopeType
    , resourceServerScopeType
    , rsstScopeName
    , rsstScopeDescription

    -- ** ResourceServerType
    , ResourceServerType
    , resourceServerType
    , rstUserPoolId
    , rstIdentifier
    , rstScopes
    , rstName

    -- ** RiskConfigurationType
    , RiskConfigurationType
    , riskConfigurationType
    , rctRiskExceptionConfiguration
    , rctClientId
    , rctAccountTakeoverRiskConfiguration
    , rctLastModifiedDate
    , rctUserPoolId
    , rctCompromisedCredentialsRiskConfiguration

    -- ** RiskExceptionConfigurationType
    , RiskExceptionConfigurationType
    , riskExceptionConfigurationType
    , rectSkippedIPRangeList
    , rectBlockedIPRangeList

    -- ** SMSMFASettingsType
    , SMSMFASettingsType
    , sMSMFASettingsType
    , smsmstEnabled
    , smsmstPreferredMFA

    -- ** SchemaAttributeType
    , SchemaAttributeType
    , schemaAttributeType
    , satNumberAttributeConstraints
    , satRequired
    , satAttributeDataType
    , satStringAttributeConstraints
    , satName
    , satDeveloperOnlyAttribute
    , satMutable

    -- ** SmsConfigurationType
    , SmsConfigurationType
    , smsConfigurationType
    , sctExternalId
    , sctSNSCallerARN

    -- ** SmsMFAConfigType
    , SmsMFAConfigType
    , smsMFAConfigType
    , smctSmsAuthenticationMessage
    , smctSmsConfiguration

    -- ** SoftwareTokenMFAConfigType
    , SoftwareTokenMFAConfigType
    , softwareTokenMFAConfigType
    , stmctEnabled

    -- ** SoftwareTokenMFASettingsType
    , SoftwareTokenMFASettingsType
    , softwareTokenMFASettingsType
    , stmstEnabled
    , stmstPreferredMFA

    -- ** StringAttributeConstraintsType
    , StringAttributeConstraintsType
    , stringAttributeConstraintsType
    , sactMaxLength
    , sactMinLength

    -- ** UICustomizationType
    , UICustomizationType
    , uICustomizationType
    , uictClientId
    , uictLastModifiedDate
    , uictUserPoolId
    , uictCSS
    , uictCSSVersion
    , uictImageURL
    , uictCreationDate

    -- ** UserContextDataType
    , UserContextDataType
    , userContextDataType
    , ucdtEncodedData

    -- ** UserImportJobType
    , UserImportJobType
    , userImportJobType
    , uijtStatus
    , uijtSkippedUsers
    , uijtJobId
    , uijtUserPoolId
    , uijtJobName
    , uijtPreSignedURL
    , uijtFailedUsers
    , uijtStartDate
    , uijtCompletionMessage
    , uijtCreationDate
    , uijtCompletionDate
    , uijtCloudWatchLogsRoleARN
    , uijtImportedUsers

    -- ** UserPoolAddOnsType
    , UserPoolAddOnsType
    , userPoolAddOnsType
    , upaotAdvancedSecurityMode

    -- ** UserPoolClientDescription
    , UserPoolClientDescription
    , userPoolClientDescription
    , upcdClientId
    , upcdUserPoolId
    , upcdClientName

    -- ** UserPoolClientType
    , UserPoolClientType
    , userPoolClientType
    , upctRefreshTokenValidity
    , upctClientId
    , upctExplicitAuthFlows
    , upctClientSecret
    , upctLastModifiedDate
    , upctSupportedIdentityProviders
    , upctLogoutURLs
    , upctAllowedOAuthFlowsUserPoolClient
    , upctUserPoolId
    , upctDefaultRedirectURI
    , upctWriteAttributes
    , upctCreationDate
    , upctReadAttributes
    , upctAllowedOAuthScopes
    , upctAllowedOAuthFlows
    , upctAnalyticsConfiguration
    , upctClientName
    , upctCallbackURLs

    -- ** UserPoolDescriptionType
    , UserPoolDescriptionType
    , userPoolDescriptionType
    , updtStatus
    , updtLastModifiedDate
    , updtName
    , updtId
    , updtCreationDate
    , updtLambdaConfig

    -- ** UserPoolPolicyType
    , UserPoolPolicyType
    , userPoolPolicyType
    , upptPasswordPolicy

    -- ** UserPoolType
    , UserPoolType
    , userPoolType
    , uptStatus
    , uptUserPoolTags
    , uptEmailConfigurationFailure
    , uptLastModifiedDate
    , uptVerificationMessageTemplate
    , uptEstimatedNumberOfUsers
    , uptDomain
    , uptEmailVerificationMessage
    , uptSmsAuthenticationMessage
    , uptUserPoolAddOns
    , uptSchemaAttributes
    , uptEmailVerificationSubject
    , uptUsernameAttributes
    , uptAliasAttributes
    , uptEmailConfiguration
    , uptSmsVerificationMessage
    , uptName
    , uptMFAConfiguration
    , uptId
    , uptSmsConfigurationFailure
    , uptCreationDate
    , uptLambdaConfig
    , uptSmsConfiguration
    , uptAdminCreateUserConfig
    , uptDeviceConfiguration
    , uptAutoVerifiedAttributes
    , uptPolicies

    -- ** UserType
    , UserType
    , userType
    , utEnabled
    , utUserStatus
    , utUsername
    , utUserCreateDate
    , utAttributes
    , utMFAOptions
    , utUserLastModifiedDate

    -- ** VerificationMessageTemplateType
    , VerificationMessageTemplateType
    , verificationMessageTemplateType
    , vmttDefaultEmailOption
    , vmttEmailSubject
    , vmttEmailSubjectByLink
    , vmttSmsMessage
    , vmttEmailMessageByLink
    , vmttEmailMessage
    ) where

import Network.AWS.CognitoIdentityProvider.AddCustomAttributes
import Network.AWS.CognitoIdentityProvider.AdminAddUserToGroup
import Network.AWS.CognitoIdentityProvider.AdminConfirmSignUp
import Network.AWS.CognitoIdentityProvider.AdminCreateUser
import Network.AWS.CognitoIdentityProvider.AdminDeleteUser
import Network.AWS.CognitoIdentityProvider.AdminDeleteUserAttributes
import Network.AWS.CognitoIdentityProvider.AdminDisableProviderForUser
import Network.AWS.CognitoIdentityProvider.AdminDisableUser
import Network.AWS.CognitoIdentityProvider.AdminEnableUser
import Network.AWS.CognitoIdentityProvider.AdminForgetDevice
import Network.AWS.CognitoIdentityProvider.AdminGetDevice
import Network.AWS.CognitoIdentityProvider.AdminGetUser
import Network.AWS.CognitoIdentityProvider.AdminInitiateAuth
import Network.AWS.CognitoIdentityProvider.AdminLinkProviderForUser
import Network.AWS.CognitoIdentityProvider.AdminListDevices
import Network.AWS.CognitoIdentityProvider.AdminListGroupsForUser
import Network.AWS.CognitoIdentityProvider.AdminListUserAuthEvents
import Network.AWS.CognitoIdentityProvider.AdminRemoveUserFromGroup
import Network.AWS.CognitoIdentityProvider.AdminResetUserPassword
import Network.AWS.CognitoIdentityProvider.AdminRespondToAuthChallenge
import Network.AWS.CognitoIdentityProvider.AdminSetUserMFAPreference
import Network.AWS.CognitoIdentityProvider.AdminSetUserSettings
import Network.AWS.CognitoIdentityProvider.AdminUpdateAuthEventFeedback
import Network.AWS.CognitoIdentityProvider.AdminUpdateDeviceStatus
import Network.AWS.CognitoIdentityProvider.AdminUpdateUserAttributes
import Network.AWS.CognitoIdentityProvider.AdminUserGlobalSignOut
import Network.AWS.CognitoIdentityProvider.AssociateSoftwareToken
import Network.AWS.CognitoIdentityProvider.ChangePassword
import Network.AWS.CognitoIdentityProvider.ConfirmDevice
import Network.AWS.CognitoIdentityProvider.ConfirmForgotPassword
import Network.AWS.CognitoIdentityProvider.ConfirmSignUp
import Network.AWS.CognitoIdentityProvider.CreateGroup
import Network.AWS.CognitoIdentityProvider.CreateIdentityProvider
import Network.AWS.CognitoIdentityProvider.CreateResourceServer
import Network.AWS.CognitoIdentityProvider.CreateUserImportJob
import Network.AWS.CognitoIdentityProvider.CreateUserPool
import Network.AWS.CognitoIdentityProvider.CreateUserPoolClient
import Network.AWS.CognitoIdentityProvider.CreateUserPoolDomain
import Network.AWS.CognitoIdentityProvider.DeleteGroup
import Network.AWS.CognitoIdentityProvider.DeleteIdentityProvider
import Network.AWS.CognitoIdentityProvider.DeleteResourceServer
import Network.AWS.CognitoIdentityProvider.DeleteUser
import Network.AWS.CognitoIdentityProvider.DeleteUserAttributes
import Network.AWS.CognitoIdentityProvider.DeleteUserPool
import Network.AWS.CognitoIdentityProvider.DeleteUserPoolClient
import Network.AWS.CognitoIdentityProvider.DeleteUserPoolDomain
import Network.AWS.CognitoIdentityProvider.DescribeIdentityProvider
import Network.AWS.CognitoIdentityProvider.DescribeResourceServer
import Network.AWS.CognitoIdentityProvider.DescribeRiskConfiguration
import Network.AWS.CognitoIdentityProvider.DescribeUserImportJob
import Network.AWS.CognitoIdentityProvider.DescribeUserPool
import Network.AWS.CognitoIdentityProvider.DescribeUserPoolClient
import Network.AWS.CognitoIdentityProvider.DescribeUserPoolDomain
import Network.AWS.CognitoIdentityProvider.ForgetDevice
import Network.AWS.CognitoIdentityProvider.ForgotPassword
import Network.AWS.CognitoIdentityProvider.GetCSVHeader
import Network.AWS.CognitoIdentityProvider.GetDevice
import Network.AWS.CognitoIdentityProvider.GetGroup
import Network.AWS.CognitoIdentityProvider.GetIdentityProviderByIdentifier
import Network.AWS.CognitoIdentityProvider.GetSigningCertificate
import Network.AWS.CognitoIdentityProvider.GetUICustomization
import Network.AWS.CognitoIdentityProvider.GetUser
import Network.AWS.CognitoIdentityProvider.GetUserAttributeVerificationCode
import Network.AWS.CognitoIdentityProvider.GetUserPoolMFAConfig
import Network.AWS.CognitoIdentityProvider.GlobalSignOut
import Network.AWS.CognitoIdentityProvider.InitiateAuth
import Network.AWS.CognitoIdentityProvider.ListDevices
import Network.AWS.CognitoIdentityProvider.ListGroups
import Network.AWS.CognitoIdentityProvider.ListIdentityProviders
import Network.AWS.CognitoIdentityProvider.ListResourceServers
import Network.AWS.CognitoIdentityProvider.ListUserImportJobs
import Network.AWS.CognitoIdentityProvider.ListUserPoolClients
import Network.AWS.CognitoIdentityProvider.ListUserPools
import Network.AWS.CognitoIdentityProvider.ListUsers
import Network.AWS.CognitoIdentityProvider.ListUsersInGroup
import Network.AWS.CognitoIdentityProvider.ResendConfirmationCode
import Network.AWS.CognitoIdentityProvider.RespondToAuthChallenge
import Network.AWS.CognitoIdentityProvider.SetRiskConfiguration
import Network.AWS.CognitoIdentityProvider.SetUICustomization
import Network.AWS.CognitoIdentityProvider.SetUserMFAPreference
import Network.AWS.CognitoIdentityProvider.SetUserPoolMFAConfig
import Network.AWS.CognitoIdentityProvider.SetUserSettings
import Network.AWS.CognitoIdentityProvider.SignUp
import Network.AWS.CognitoIdentityProvider.StartUserImportJob
import Network.AWS.CognitoIdentityProvider.StopUserImportJob
import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.UpdateAuthEventFeedback
import Network.AWS.CognitoIdentityProvider.UpdateDeviceStatus
import Network.AWS.CognitoIdentityProvider.UpdateGroup
import Network.AWS.CognitoIdentityProvider.UpdateIdentityProvider
import Network.AWS.CognitoIdentityProvider.UpdateResourceServer
import Network.AWS.CognitoIdentityProvider.UpdateUserAttributes
import Network.AWS.CognitoIdentityProvider.UpdateUserPool
import Network.AWS.CognitoIdentityProvider.UpdateUserPoolClient
import Network.AWS.CognitoIdentityProvider.VerifySoftwareToken
import Network.AWS.CognitoIdentityProvider.VerifyUserAttribute
import Network.AWS.CognitoIdentityProvider.Waiters

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
