{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types
    (
    -- * Service Configuration
      cognitoIdentityProvider

    -- * Errors
    , _InvalidParameterException
    , _InvalidLambdaResponseException
    , _UnexpectedLambdaException
    , _NotAuthorizedException
    , _InternalErrorException
    , _CodeMismatchException
    , _TooManyRequestsException
    , _ConcurrentModificationException
    , _UserLambdaValidationException
    , _ExpiredCodeException
    , _TooManyFailedAttemptsException
    , _ResourceNotFoundException
    , _AliasExistsException
    , _LimitExceededException
    , _InvalidPasswordException
    , _UsernameExistsException

    -- * AliasAttributeType
    , AliasAttributeType (..)

    -- * AttributeDataType
    , AttributeDataType (..)

    -- * DeliveryMediumType
    , DeliveryMediumType (..)

    -- * StatusType
    , StatusType (..)

    -- * UserPoolMFAType
    , UserPoolMFAType (..)

    -- * UserStatusType
    , UserStatusType (..)

    -- * VerifiedAttributeType
    , VerifiedAttributeType (..)

    -- * AttributeType
    , AttributeType
    , attributeType
    , atValue
    , atName

    -- * CodeDeliveryDetailsType
    , CodeDeliveryDetailsType
    , codeDeliveryDetailsType
    , cddtDestination
    , cddtDeliveryMedium
    , cddtAttributeName

    -- * LambdaConfigType
    , LambdaConfigType
    , lambdaConfigType
    , lctPreAuthentication
    , lctPostAuthentication
    , lctCustomMessage
    , lctPostConfirmation
    , lctPreSignUp

    -- * MFAOptionType
    , MFAOptionType
    , mfaOptionType
    , motDeliveryMedium
    , motAttributeName

    -- * NumberAttributeConstraintsType
    , NumberAttributeConstraintsType
    , numberAttributeConstraintsType
    , nactMaxValue
    , nactMinValue

    -- * PasswordPolicyType
    , PasswordPolicyType
    , passwordPolicyType
    , pptRequireNumbers
    , pptRequireUppercase
    , pptRequireLowercase
    , pptMinimumLength
    , pptRequireSymbols

    -- * SchemaAttributeType
    , SchemaAttributeType
    , schemaAttributeType
    , satNumberAttributeConstraints
    , satRequired
    , satAttributeDataType
    , satStringAttributeConstraints
    , satName
    , satDeveloperOnlyAttribute
    , satMutable

    -- * StringAttributeConstraintsType
    , StringAttributeConstraintsType
    , stringAttributeConstraintsType
    , sactMaxLength
    , sactMinLength

    -- * UserPoolClientDescription
    , UserPoolClientDescription
    , userPoolClientDescription
    , upcdClientId
    , upcdUserPoolId
    , upcdClientName

    -- * UserPoolClientType
    , UserPoolClientType
    , userPoolClientType
    , upctClientId
    , upctClientSecret
    , upctLastModifiedDate
    , upctUserPoolId
    , upctCreationDate
    , upctClientName

    -- * UserPoolDescriptionType
    , UserPoolDescriptionType
    , userPoolDescriptionType
    , updtStatus
    , updtLastModifiedDate
    , updtName
    , updtId
    , updtCreationDate
    , updtLambdaConfig

    -- * UserPoolPolicyType
    , UserPoolPolicyType
    , userPoolPolicyType
    , upptPasswordPolicy

    -- * UserPoolType
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

    -- * UserType
    , UserType
    , userType
    , utEnabled
    , utUserStatus
    , utUsername
    , utUserCreateDate
    , utAttributes
    , utUserLastModifiedDate
    ) where

import           Network.AWS.CognitoIdentityProvider.Types.Product
import           Network.AWS.CognitoIdentityProvider.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2016-04-18' of the Amazon Cognito Identity Provider SDK configuration.
cognitoIdentityProvider :: Service
cognitoIdentityProvider =
    Service
    { _svcAbbrev = "CognitoIdentityProvider"
    , _svcSigner = v4
    , _svcPrefix = "cognito-idp"
    , _svcVersion = "2016-04-18"
    , _svcEndpoint = defaultEndpoint cognitoIdentityProvider
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "CognitoIdentityProvider"
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
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | This exception is thrown when the Amazon Cognito service encounters an invalid parameter.
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
    _ServiceError . hasCode "InvalidParameterException"

-- | This exception is thrown when the Amazon Cognito service encounters an invalid AWS Lambda response.
_InvalidLambdaResponseException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidLambdaResponseException =
    _ServiceError . hasCode "InvalidLambdaResponseException"

-- | This exception gets thrown when the Amazon Cognito service encounters an unexpected exception with the AWS Lambda service.
_UnexpectedLambdaException :: AsError a => Getting (First ServiceError) a ServiceError
_UnexpectedLambdaException =
    _ServiceError . hasCode "UnexpectedLambdaException"

-- | This exception gets thrown when a user is not authorized.
_NotAuthorizedException :: AsError a => Getting (First ServiceError) a ServiceError
_NotAuthorizedException = _ServiceError . hasCode "NotAuthorizedException"

-- | This exception is thrown when Amazon Cognito encounters an internal error.
_InternalErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalErrorException = _ServiceError . hasCode "InternalErrorException"

-- | This exception is thrown if the provided code does not match what the server was expecting.
_CodeMismatchException :: AsError a => Getting (First ServiceError) a ServiceError
_CodeMismatchException = _ServiceError . hasCode "CodeMismatchException"

-- | This exception gets thrown when the user has made too many requests for a given operation.
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException = _ServiceError . hasCode "TooManyRequestsException"

-- | This exception is thrown if two or more modifications are happening concurrently.
_ConcurrentModificationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentModificationException =
    _ServiceError . hasCode "ConcurrentModificationException"

-- | This exception gets thrown when the Amazon Cognito service encounters a user validation exception with the AWS Lambda service.
_UserLambdaValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_UserLambdaValidationException =
    _ServiceError . hasCode "UserLambdaValidationException"

-- | This exception is thrown if a code has expired.
_ExpiredCodeException :: AsError a => Getting (First ServiceError) a ServiceError
_ExpiredCodeException = _ServiceError . hasCode "ExpiredCodeException"

-- | This exception gets thrown when the user has made too many failed attempts for a given action (e.g., sign in).
_TooManyFailedAttemptsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyFailedAttemptsException =
    _ServiceError . hasCode "TooManyFailedAttemptsException"

-- | This exception is thrown when the Amazon Cognito service cannot find the requested resource.
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasCode "ResourceNotFoundException"

-- | This exception is thrown when a user tries to confirm the account with an email or phone number that has already been supplied as an alias from a different account. This exception tells user that an account with this email or phone already exists.
_AliasExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_AliasExistsException = _ServiceError . hasCode "AliasExistsException"

-- | This exception is thrown when a user exceeds the limit for a requested AWS resource.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _ServiceError . hasCode "LimitExceededException"

-- | This exception is thrown when the Amazon Cognito service encounters an invalid password.
_InvalidPasswordException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidPasswordException = _ServiceError . hasCode "InvalidPasswordException"

-- | This exception is thrown when Amazon Cognito encounters a user name that already exists in the user pool.
_UsernameExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_UsernameExistsException = _ServiceError . hasCode "UsernameExistsException"
