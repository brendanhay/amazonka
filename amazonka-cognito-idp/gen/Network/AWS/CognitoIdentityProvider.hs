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
-- You can create a user pool in Amazon Cognito Identity to manage
-- directories and users. You can authenticate a user to obtain tokens
-- related to user identity and access policies.
--
-- This API reference provides information about user pools in Amazon
-- Cognito Identity, which is a new capability that is available as a beta.
module Network.AWS.CognitoIdentityProvider
    (
    -- * Service Configuration
      cognitoIdentityProvider

    -- * Errors
    -- $errors

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** InvalidLambdaResponseException
    , _InvalidLambdaResponseException

    -- ** UnexpectedLambdaException
    , _UnexpectedLambdaException

    -- ** NotAuthorizedException
    , _NotAuthorizedException

    -- ** InternalErrorException
    , _InternalErrorException

    -- ** CodeMismatchException
    , _CodeMismatchException

    -- ** TooManyRequestsException
    , _TooManyRequestsException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** UserLambdaValidationException
    , _UserLambdaValidationException

    -- ** ExpiredCodeException
    , _ExpiredCodeException

    -- ** TooManyFailedAttemptsException
    , _TooManyFailedAttemptsException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

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

    -- ** ConfirmForgotPassword
    , module Network.AWS.CognitoIdentityProvider.ConfirmForgotPassword

    -- ** ListUsers
    , module Network.AWS.CognitoIdentityProvider.ListUsers

    -- ** AdminDeleteUserAttributes
    , module Network.AWS.CognitoIdentityProvider.AdminDeleteUserAttributes

    -- ** AdminUpdateUserAttributes
    , module Network.AWS.CognitoIdentityProvider.AdminUpdateUserAttributes

    -- ** AdminGetUser
    , module Network.AWS.CognitoIdentityProvider.AdminGetUser

    -- ** ForgotPassword
    , module Network.AWS.CognitoIdentityProvider.ForgotPassword

    -- ** DescribeUserPool
    , module Network.AWS.CognitoIdentityProvider.DescribeUserPool

    -- ** AdminConfirmSignUp
    , module Network.AWS.CognitoIdentityProvider.AdminConfirmSignUp

    -- ** SignUp
    , module Network.AWS.CognitoIdentityProvider.SignUp

    -- ** ChangePassword
    , module Network.AWS.CognitoIdentityProvider.ChangePassword

    -- ** CreateUserPool
    , module Network.AWS.CognitoIdentityProvider.CreateUserPool

    -- ** ConfirmSignUp
    , module Network.AWS.CognitoIdentityProvider.ConfirmSignUp

    -- ** ListUserPools
    , module Network.AWS.CognitoIdentityProvider.ListUserPools

    -- ** AdminResetUserPassword
    , module Network.AWS.CognitoIdentityProvider.AdminResetUserPassword

    -- ** GetUser
    , module Network.AWS.CognitoIdentityProvider.GetUser

    -- ** AdminDeleteUser
    , module Network.AWS.CognitoIdentityProvider.AdminDeleteUser

    -- ** AddCustomAttributes
    , module Network.AWS.CognitoIdentityProvider.AddCustomAttributes

    -- ** ListUserPoolClients
    , module Network.AWS.CognitoIdentityProvider.ListUserPoolClients

    -- ** UpdateUserPoolClient
    , module Network.AWS.CognitoIdentityProvider.UpdateUserPoolClient

    -- ** DeleteUserPoolClient
    , module Network.AWS.CognitoIdentityProvider.DeleteUserPoolClient

    -- ** DeleteUser
    , module Network.AWS.CognitoIdentityProvider.DeleteUser

    -- ** CreateUserPoolClient
    , module Network.AWS.CognitoIdentityProvider.CreateUserPoolClient

    -- ** SetUserSettings
    , module Network.AWS.CognitoIdentityProvider.SetUserSettings

    -- ** DescribeUserPoolClient
    , module Network.AWS.CognitoIdentityProvider.DescribeUserPoolClient

    -- ** ResendConfirmationCode
    , module Network.AWS.CognitoIdentityProvider.ResendConfirmationCode

    -- ** AdminSetUserSettings
    , module Network.AWS.CognitoIdentityProvider.AdminSetUserSettings

    -- * Types

    -- ** AliasAttributeType
    , AliasAttributeType (..)

    -- ** AttributeDataType
    , AttributeDataType (..)

    -- ** DeliveryMediumType
    , DeliveryMediumType (..)

    -- ** StatusType
    , StatusType (..)

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

    -- ** CodeDeliveryDetailsType
    , CodeDeliveryDetailsType
    , codeDeliveryDetailsType
    , cddtDestination
    , cddtDeliveryMedium
    , cddtAttributeName

    -- ** LambdaConfigType
    , LambdaConfigType
    , lambdaConfigType
    , lctPreAuthentication
    , lctPostAuthentication
    , lctCustomMessage
    , lctPostConfirmation
    , lctPreSignUp

    -- ** MFAOptionType
    , MFAOptionType
    , mfaOptionType
    , motDeliveryMedium
    , motAttributeName

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

    -- ** StringAttributeConstraintsType
    , StringAttributeConstraintsType
    , stringAttributeConstraintsType
    , sactMaxLength
    , sactMinLength

    -- ** UserPoolClientDescription
    , UserPoolClientDescription
    , userPoolClientDescription
    , upcdClientId
    , upcdUserPoolId
    , upcdClientName

    -- ** UserPoolClientType
    , UserPoolClientType
    , userPoolClientType
    , upctClientId
    , upctClientSecret
    , upctLastModifiedDate
    , upctUserPoolId
    , upctCreationDate
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
    , uptLastModifiedDate
    , uptEstimatedNumberOfUsers
    , uptEmailVerificationMessage
    , uptSmsAuthenticationMessage
    , uptSchemaAttributes
    , uptEmailVerificationSubject
    , uptAliasAttributes
    , uptSmsVerificationMessage
    , uptName
    , uptMFAConfiguration
    , uptId
    , uptCreationDate
    , uptLambdaConfig
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
import           Network.AWS.CognitoIdentityProvider.AdminGetUser
import           Network.AWS.CognitoIdentityProvider.AdminResetUserPassword
import           Network.AWS.CognitoIdentityProvider.AdminSetUserSettings
import           Network.AWS.CognitoIdentityProvider.AdminUpdateUserAttributes
import           Network.AWS.CognitoIdentityProvider.ChangePassword
import           Network.AWS.CognitoIdentityProvider.ConfirmForgotPassword
import           Network.AWS.CognitoIdentityProvider.ConfirmSignUp
import           Network.AWS.CognitoIdentityProvider.CreateUserPool
import           Network.AWS.CognitoIdentityProvider.CreateUserPoolClient
import           Network.AWS.CognitoIdentityProvider.DeleteUser
import           Network.AWS.CognitoIdentityProvider.DeleteUserAttributes
import           Network.AWS.CognitoIdentityProvider.DeleteUserPool
import           Network.AWS.CognitoIdentityProvider.DeleteUserPoolClient
import           Network.AWS.CognitoIdentityProvider.DescribeUserPool
import           Network.AWS.CognitoIdentityProvider.DescribeUserPoolClient
import           Network.AWS.CognitoIdentityProvider.ForgotPassword
import           Network.AWS.CognitoIdentityProvider.GetUser
import           Network.AWS.CognitoIdentityProvider.GetUserAttributeVerificationCode
import           Network.AWS.CognitoIdentityProvider.ListUserPoolClients
import           Network.AWS.CognitoIdentityProvider.ListUserPools
import           Network.AWS.CognitoIdentityProvider.ListUsers
import           Network.AWS.CognitoIdentityProvider.ResendConfirmationCode
import           Network.AWS.CognitoIdentityProvider.SetUserSettings
import           Network.AWS.CognitoIdentityProvider.SignUp
import           Network.AWS.CognitoIdentityProvider.Types
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
