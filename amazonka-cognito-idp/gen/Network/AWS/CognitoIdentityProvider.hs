{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can create a user pool in Amazon Cognito Identity to manage directories and users. You can authenticate a user to obtain tokens related to user identity and access policies.
--
-- This API reference provides information about user pools in Amazon Cognito Identity.
--
-- For more information, see <https://aws.amazon.com/cognito/ Amazon Cognito>.
module Network.AWS.CognitoIdentityProvider
    (
    -- * Service Configuration
      cognitoIdentityProvider

    -- * Errors
    -- $errors

    -- ** PasswordResetRequiredException
    , _PasswordResetRequiredException

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** InvalidLambdaResponseException
    , _InvalidLambdaResponseException

    -- ** InvalidEmailRoleAccessPolicyException
    , _InvalidEmailRoleAccessPolicyException

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

    -- ** CodeMismatchException
    , _CodeMismatchException

    -- ** UserImportInProgressException
    , _UserImportInProgressException

    -- ** InvalidSmsRoleTrustRelationshipException
    , _InvalidSmsRoleTrustRelationshipException

    -- ** TooManyRequestsException
    , _TooManyRequestsException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** UserLambdaValidationException
    , _UserLambdaValidationException

    -- ** PreconditionNotMetException
    , _PreconditionNotMetException

    -- ** ExpiredCodeException
    , _ExpiredCodeException

    -- ** TooManyFailedAttemptsException
    , _TooManyFailedAttemptsException

    -- ** UserNotConfirmedException
    , _UserNotConfirmedException

    -- ** CodeDeliveryFailureException
    , _CodeDeliveryFailureException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** MFAMethodNotFoundException
    , _MFAMethodNotFoundException

    -- ** AliasExistsException
    , _AliasExistsException

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

    -- ** AdminInitiateAuth
    , module Network.AWS.CognitoIdentityProvider.AdminInitiateAuth

    -- ** AdminEnableUser
    , module Network.AWS.CognitoIdentityProvider.AdminEnableUser

    -- ** GetUserAttributeVerificationCode
    , module Network.AWS.CognitoIdentityProvider.GetUserAttributeVerificationCode

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

    -- ** ListUsers
    , module Network.AWS.CognitoIdentityProvider.ListUsers

    -- ** AdminDeleteUserAttributes
    , module Network.AWS.CognitoIdentityProvider.AdminDeleteUserAttributes

    -- ** AdminUpdateUserAttributes
    , module Network.AWS.CognitoIdentityProvider.AdminUpdateUserAttributes

    -- ** AdminGetUser
    , module Network.AWS.CognitoIdentityProvider.AdminGetUser

    -- ** AdminUserGlobalSignOut
    , module Network.AWS.CognitoIdentityProvider.AdminUserGlobalSignOut

    -- ** ForgotPassword
    , module Network.AWS.CognitoIdentityProvider.ForgotPassword

    -- ** DescribeUserPool
    , module Network.AWS.CognitoIdentityProvider.DescribeUserPool

    -- ** InitiateAuth
    , module Network.AWS.CognitoIdentityProvider.InitiateAuth

    -- ** AdminConfirmSignUp
    , module Network.AWS.CognitoIdentityProvider.AdminConfirmSignUp

    -- ** StartUserImportJob
    , module Network.AWS.CognitoIdentityProvider.StartUserImportJob

    -- ** GetDevice
    , module Network.AWS.CognitoIdentityProvider.GetDevice

    -- ** SignUp
    , module Network.AWS.CognitoIdentityProvider.SignUp

    -- ** ChangePassword
    , module Network.AWS.CognitoIdentityProvider.ChangePassword

    -- ** RespondToAuthChallenge
    , module Network.AWS.CognitoIdentityProvider.RespondToAuthChallenge

    -- ** CreateUserPool
    , module Network.AWS.CognitoIdentityProvider.CreateUserPool

    -- ** AdminGetDevice
    , module Network.AWS.CognitoIdentityProvider.AdminGetDevice

    -- ** ConfirmSignUp
    , module Network.AWS.CognitoIdentityProvider.ConfirmSignUp

    -- ** ListUserPools
    , module Network.AWS.CognitoIdentityProvider.ListUserPools

    -- ** AdminResetUserPassword
    , module Network.AWS.CognitoIdentityProvider.AdminResetUserPassword

    -- ** CreateUserImportJob
    , module Network.AWS.CognitoIdentityProvider.CreateUserImportJob

    -- ** GetUser
    , module Network.AWS.CognitoIdentityProvider.GetUser

    -- ** GetCSVHeader
    , module Network.AWS.CognitoIdentityProvider.GetCSVHeader

    -- ** AdminDeleteUser
    , module Network.AWS.CognitoIdentityProvider.AdminDeleteUser

    -- ** AdminForgetDevice
    , module Network.AWS.CognitoIdentityProvider.AdminForgetDevice

    -- ** AdminUpdateDeviceStatus
    , module Network.AWS.CognitoIdentityProvider.AdminUpdateDeviceStatus

    -- ** AddCustomAttributes
    , module Network.AWS.CognitoIdentityProvider.AddCustomAttributes

    -- ** ListUserPoolClients
    , module Network.AWS.CognitoIdentityProvider.ListUserPoolClients

    -- ** UpdateUserPoolClient
    , module Network.AWS.CognitoIdentityProvider.UpdateUserPoolClient

    -- ** DeleteUserPoolClient
    , module Network.AWS.CognitoIdentityProvider.DeleteUserPoolClient

    -- ** UpdateDeviceStatus
    , module Network.AWS.CognitoIdentityProvider.UpdateDeviceStatus

    -- ** ForgetDevice
    , module Network.AWS.CognitoIdentityProvider.ForgetDevice

    -- ** DeleteUser
    , module Network.AWS.CognitoIdentityProvider.DeleteUser

    -- ** CreateUserPoolClient
    , module Network.AWS.CognitoIdentityProvider.CreateUserPoolClient

    -- ** StopUserImportJob
    , module Network.AWS.CognitoIdentityProvider.StopUserImportJob

    -- ** DescribeUserImportJob
    , module Network.AWS.CognitoIdentityProvider.DescribeUserImportJob

    -- ** GlobalSignOut
    , module Network.AWS.CognitoIdentityProvider.GlobalSignOut

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

    -- ** AdminSetUserSettings
    , module Network.AWS.CognitoIdentityProvider.AdminSetUserSettings

    -- ** ListDevices
    , module Network.AWS.CognitoIdentityProvider.ListDevices

    -- * Types

    -- ** AliasAttributeType
    , AliasAttributeType (..)

    -- ** AttributeDataType
    , AttributeDataType (..)

    -- ** AuthFlowType
    , AuthFlowType (..)

    -- ** ChallengeNameType
    , ChallengeNameType (..)

    -- ** DeliveryMediumType
    , DeliveryMediumType (..)

    -- ** DeviceRememberedStatusType
    , DeviceRememberedStatusType (..)

    -- ** ExplicitAuthFlowsType
    , ExplicitAuthFlowsType (..)

    -- ** StatusType
    , StatusType (..)

    -- ** UserImportJobStatusType
    , UserImportJobStatusType (..)

    -- ** UserPoolMFAType
    , UserPoolMFAType (..)

    -- ** UserStatusType
    , UserStatusType (..)

    -- ** VerifiedAttributeType
    , VerifiedAttributeType (..)

    -- ** AttributeType
    , AttributeType
    , attributeType
    , atValue
    , atName

    -- ** AuthenticationResultType
    , AuthenticationResultType
    , authenticationResultType
    , artAccessToken
    , artRefreshToken
    , artNewDeviceMetadata
    , artExpiresIn
    , artTokenType
    , artIdToken

    -- ** CodeDeliveryDetailsType
    , CodeDeliveryDetailsType
    , codeDeliveryDetailsType
    , cddtDestination
    , cddtDeliveryMedium
    , cddtAttributeName

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

    -- ** EmailConfigurationType
    , EmailConfigurationType
    , emailConfigurationType
    , ectSourceARN
    , ectReplyToEmailAddress

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
    , lctPreSignUp

    -- ** MFAOptionType
    , MFAOptionType
    , mfaOptionType
    , motDeliveryMedium
    , motAttributeName

    -- ** NewDeviceMetadataType
    , NewDeviceMetadataType
    , newDeviceMetadataType
    , ndmtDeviceGroupKey
    , ndmtDeviceKey

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
    , sctSNSCallerARN
    , sctExternalId

    -- ** StringAttributeConstraintsType
    , StringAttributeConstraintsType
    , stringAttributeConstraintsType
    , sactMaxLength
    , sactMinLength

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
    , upctUserPoolId
    , upctWriteAttributes
    , upctCreationDate
    , upctReadAttributes
    , upctClientName

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
    , uptEmailConfigurationFailure
    , uptLastModifiedDate
    , uptEstimatedNumberOfUsers
    , uptEmailVerificationMessage
    , uptSmsAuthenticationMessage
    , uptSchemaAttributes
    , uptEmailVerificationSubject
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
    , utUserLastModifiedDate
    ) where

import           Network.AWS.CognitoIdentityProvider.AddCustomAttributes
import           Network.AWS.CognitoIdentityProvider.AdminConfirmSignUp
import           Network.AWS.CognitoIdentityProvider.AdminDeleteUser
import           Network.AWS.CognitoIdentityProvider.AdminDeleteUserAttributes
import           Network.AWS.CognitoIdentityProvider.AdminDisableUser
import           Network.AWS.CognitoIdentityProvider.AdminEnableUser
import           Network.AWS.CognitoIdentityProvider.AdminForgetDevice
import           Network.AWS.CognitoIdentityProvider.AdminGetDevice
import           Network.AWS.CognitoIdentityProvider.AdminGetUser
import           Network.AWS.CognitoIdentityProvider.AdminInitiateAuth
import           Network.AWS.CognitoIdentityProvider.AdminListDevices
import           Network.AWS.CognitoIdentityProvider.AdminResetUserPassword
import           Network.AWS.CognitoIdentityProvider.AdminRespondToAuthChallenge
import           Network.AWS.CognitoIdentityProvider.AdminSetUserSettings
import           Network.AWS.CognitoIdentityProvider.AdminUpdateDeviceStatus
import           Network.AWS.CognitoIdentityProvider.AdminUpdateUserAttributes
import           Network.AWS.CognitoIdentityProvider.AdminUserGlobalSignOut
import           Network.AWS.CognitoIdentityProvider.ChangePassword
import           Network.AWS.CognitoIdentityProvider.ConfirmDevice
import           Network.AWS.CognitoIdentityProvider.ConfirmForgotPassword
import           Network.AWS.CognitoIdentityProvider.ConfirmSignUp
import           Network.AWS.CognitoIdentityProvider.CreateUserImportJob
import           Network.AWS.CognitoIdentityProvider.CreateUserPool
import           Network.AWS.CognitoIdentityProvider.CreateUserPoolClient
import           Network.AWS.CognitoIdentityProvider.DeleteUser
import           Network.AWS.CognitoIdentityProvider.DeleteUserAttributes
import           Network.AWS.CognitoIdentityProvider.DeleteUserPool
import           Network.AWS.CognitoIdentityProvider.DeleteUserPoolClient
import           Network.AWS.CognitoIdentityProvider.DescribeUserImportJob
import           Network.AWS.CognitoIdentityProvider.DescribeUserPool
import           Network.AWS.CognitoIdentityProvider.DescribeUserPoolClient
import           Network.AWS.CognitoIdentityProvider.ForgetDevice
import           Network.AWS.CognitoIdentityProvider.ForgotPassword
import           Network.AWS.CognitoIdentityProvider.GetCSVHeader
import           Network.AWS.CognitoIdentityProvider.GetDevice
import           Network.AWS.CognitoIdentityProvider.GetUser
import           Network.AWS.CognitoIdentityProvider.GetUserAttributeVerificationCode
import           Network.AWS.CognitoIdentityProvider.GlobalSignOut
import           Network.AWS.CognitoIdentityProvider.InitiateAuth
import           Network.AWS.CognitoIdentityProvider.ListDevices
import           Network.AWS.CognitoIdentityProvider.ListUserImportJobs
import           Network.AWS.CognitoIdentityProvider.ListUserPoolClients
import           Network.AWS.CognitoIdentityProvider.ListUserPools
import           Network.AWS.CognitoIdentityProvider.ListUsers
import           Network.AWS.CognitoIdentityProvider.ResendConfirmationCode
import           Network.AWS.CognitoIdentityProvider.RespondToAuthChallenge
import           Network.AWS.CognitoIdentityProvider.SetUserSettings
import           Network.AWS.CognitoIdentityProvider.SignUp
import           Network.AWS.CognitoIdentityProvider.StartUserImportJob
import           Network.AWS.CognitoIdentityProvider.StopUserImportJob
import           Network.AWS.CognitoIdentityProvider.Types
import           Network.AWS.CognitoIdentityProvider.UpdateDeviceStatus
import           Network.AWS.CognitoIdentityProvider.UpdateUserAttributes
import           Network.AWS.CognitoIdentityProvider.UpdateUserPool
import           Network.AWS.CognitoIdentityProvider.UpdateUserPoolClient
import           Network.AWS.CognitoIdentityProvider.VerifyUserAttribute
import           Network.AWS.CognitoIdentityProvider.Waiters

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
