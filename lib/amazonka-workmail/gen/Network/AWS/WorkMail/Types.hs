{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkMail.Types
    (
    -- * Service Configuration
      workMail

    -- * Errors
    , _DirectoryUnavailableException
    , _InvalidParameterException
    , _UnsupportedOperationException
    , _DirectoryServiceAuthenticationFailedException
    , _OrganizationStateException
    , _EntityStateException
    , _InvalidConfigurationException
    , _MailDomainStateException
    , _ReservedNameException
    , _OrganizationNotFoundException
    , _EntityNotFoundException
    , _EntityAlreadyRegisteredException
    , _MailDomainNotFoundException
    , _EmailAddressInUseException
    , _NameAvailabilityException
    , _InvalidPasswordException

    -- * EntityState
    , EntityState (..)

    -- * MemberType
    , MemberType (..)

    -- * PermissionType
    , PermissionType (..)

    -- * ResourceType
    , ResourceType (..)

    -- * UserRole
    , UserRole (..)

    -- * BookingOptions
    , BookingOptions
    , bookingOptions
    , boAutoDeclineConflictingRequests
    , boAutoDeclineRecurringRequests
    , boAutoAcceptRequests

    -- * Delegate
    , Delegate
    , delegate
    , dId
    , dType

    -- * Group
    , Group
    , group'
    , gEmail
    , gState
    , gDisabledDate
    , gName
    , gId
    , gEnabledDate

    -- * Member
    , Member
    , member
    , mState
    , mDisabledDate
    , mName
    , mId
    , mType
    , mEnabledDate

    -- * OrganizationSummary
    , OrganizationSummary
    , organizationSummary
    , osState
    , osAlias
    , osErrorMessage
    , osOrganizationId

    -- * Permission
    , Permission
    , permission
    , pGranteeId
    , pGranteeType
    , pPermissionValues

    -- * Resource
    , Resource
    , resource
    , rEmail
    , rState
    , rDisabledDate
    , rName
    , rId
    , rType
    , rEnabledDate

    -- * User
    , User
    , user
    , uEmail
    , uState
    , uDisabledDate
    , uName
    , uId
    , uDisplayName
    , uUserRole
    , uEnabledDate
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.WorkMail.Types.Product
import Network.AWS.WorkMail.Types.Sum

-- | API version @2017-10-01@ of the Amazon WorkMail SDK configuration.
workMail :: Service
workMail =
  Service
    { _svcAbbrev = "WorkMail"
    , _svcSigner = v4
    , _svcPrefix = "workmail"
    , _svcVersion = "2017-10-01"
    , _svcEndpoint = defaultEndpoint workMail
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "WorkMail"
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


-- | The directory that you are trying to perform operations on isn't available.
--
--
_DirectoryUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_DirectoryUnavailableException =
  _MatchServiceError workMail "DirectoryUnavailableException"


-- | One or more of the input parameters don't match the service's restrictions.
--
--
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
  _MatchServiceError workMail "InvalidParameterException"


-- | You can't perform a write operation against a read-only directory.
--
--
_UnsupportedOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedOperationException =
  _MatchServiceError workMail "UnsupportedOperationException"


-- | The Directory Service doesn't recognize the credentials supplied by the Amazon WorkMail service.
--
--
_DirectoryServiceAuthenticationFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_DirectoryServiceAuthenticationFailedException =
  _MatchServiceError workMail "DirectoryServiceAuthenticationFailedException"


-- | The organization must have a valid state (Active or Synchronizing) to perform certain operations on the organization or its entities.
--
--
_OrganizationStateException :: AsError a => Getting (First ServiceError) a ServiceError
_OrganizationStateException =
  _MatchServiceError workMail "OrganizationStateException"


-- | You are performing an operation on an entity that isn't in the expected state, such as trying to update a deleted user.
--
--
_EntityStateException :: AsError a => Getting (First ServiceError) a ServiceError
_EntityStateException = _MatchServiceError workMail "EntityStateException"


-- | The configuration for a resource isn't valid. A resource must either be able to auto-respond to requests or have at least one delegate associated that can do it on its behalf.
--
--
_InvalidConfigurationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidConfigurationException =
  _MatchServiceError workMail "InvalidConfigurationException"


-- | After a domain has been added to the organization, it must be verified. The domain is not yet verified.
--
--
_MailDomainStateException :: AsError a => Getting (First ServiceError) a ServiceError
_MailDomainStateException =
  _MatchServiceError workMail "MailDomainStateException"


-- | This entity name is not allowed in Amazon WorkMail.
--
--
_ReservedNameException :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedNameException = _MatchServiceError workMail "ReservedNameException"


-- | An operation received a valid organization identifier that either doesn't belong or exist in the system.
--
--
_OrganizationNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_OrganizationNotFoundException =
  _MatchServiceError workMail "OrganizationNotFoundException"


-- | The identifier supplied for the entity is valid, but it does not exist in your organization.
--
--
_EntityNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_EntityNotFoundException = _MatchServiceError workMail "EntityNotFoundException"


-- | The user, group, or resource that you're trying to register is already registered.
--
--
_EntityAlreadyRegisteredException :: AsError a => Getting (First ServiceError) a ServiceError
_EntityAlreadyRegisteredException =
  _MatchServiceError workMail "EntityAlreadyRegisteredException"


-- | For an email or alias to be created in Amazon WorkMail, the included domain must be defined in the organization.
--
--
_MailDomainNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_MailDomainNotFoundException =
  _MatchServiceError workMail "MailDomainNotFoundException"


-- | The email address that you're trying to assign is already created for a different user, group, or resource.
--
--
_EmailAddressInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_EmailAddressInUseException =
  _MatchServiceError workMail "EmailAddressInUseException"


-- | The entity (user, group, or user) name isn't unique in Amazon WorkMail.
--
--
_NameAvailabilityException :: AsError a => Getting (First ServiceError) a ServiceError
_NameAvailabilityException =
  _MatchServiceError workMail "NameAvailabilityException"


-- | The supplied password doesn't match the minimum security constraints, such as length or use of special characters.
--
--
_InvalidPasswordException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidPasswordException =
  _MatchServiceError workMail "InvalidPasswordException"

