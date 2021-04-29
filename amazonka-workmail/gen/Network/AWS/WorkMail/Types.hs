{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _EntityNotFoundException,
    _OrganizationNotFoundException,
    _ReservedNameException,
    _MailDomainStateException,
    _TooManyTagsException,
    _NameAvailabilityException,
    _InvalidConfigurationException,
    _OrganizationStateException,
    _EntityStateException,
    _DirectoryInUseException,
    _UnsupportedOperationException,
    _InvalidParameterException,
    _DirectoryUnavailableException,
    _LimitExceededException,
    _InvalidPasswordException,
    _EmailAddressInUseException,
    _ResourceNotFoundException,
    _DirectoryServiceAuthenticationFailedException,
    _MailDomainNotFoundException,
    _EntityAlreadyRegisteredException,

    -- * AccessControlRuleEffect
    AccessControlRuleEffect (..),

    -- * EntityState
    EntityState (..),

    -- * FolderName
    FolderName (..),

    -- * MailboxExportJobState
    MailboxExportJobState (..),

    -- * MemberType
    MemberType (..),

    -- * PermissionType
    PermissionType (..),

    -- * ResourceType
    ResourceType (..),

    -- * RetentionAction
    RetentionAction (..),

    -- * UserRole
    UserRole (..),

    -- * AccessControlRule
    AccessControlRule (..),
    newAccessControlRule,
    accessControlRule_effect,
    accessControlRule_dateCreated,
    accessControlRule_notIpRanges,
    accessControlRule_ipRanges,
    accessControlRule_dateModified,
    accessControlRule_actions,
    accessControlRule_userIds,
    accessControlRule_name,
    accessControlRule_description,
    accessControlRule_notActions,
    accessControlRule_notUserIds,

    -- * BookingOptions
    BookingOptions (..),
    newBookingOptions,
    bookingOptions_autoDeclineConflictingRequests,
    bookingOptions_autoDeclineRecurringRequests,
    bookingOptions_autoAcceptRequests,

    -- * Delegate
    Delegate (..),
    newDelegate,
    delegate_id,
    delegate_type,

    -- * Domain
    Domain (..),
    newDomain,
    domain_hostedZoneId,
    domain_domainName,

    -- * FolderConfiguration
    FolderConfiguration (..),
    newFolderConfiguration,
    folderConfiguration_period,
    folderConfiguration_name,
    folderConfiguration_action,

    -- * Group
    Group (..),
    newGroup,
    group_enabledDate,
    group_id,
    group_state,
    group_name,
    group_email,
    group_disabledDate,

    -- * MailboxExportJob
    MailboxExportJob (..),
    newMailboxExportJob,
    mailboxExportJob_estimatedProgress,
    mailboxExportJob_entityId,
    mailboxExportJob_startTime,
    mailboxExportJob_s3Path,
    mailboxExportJob_endTime,
    mailboxExportJob_state,
    mailboxExportJob_s3BucketName,
    mailboxExportJob_description,
    mailboxExportJob_jobId,

    -- * Member
    Member (..),
    newMember,
    member_enabledDate,
    member_id,
    member_state,
    member_name,
    member_disabledDate,
    member_type,

    -- * OrganizationSummary
    OrganizationSummary (..),
    newOrganizationSummary,
    organizationSummary_organizationId,
    organizationSummary_alias,
    organizationSummary_defaultMailDomain,
    organizationSummary_state,
    organizationSummary_errorMessage,

    -- * Permission
    Permission (..),
    newPermission,
    permission_granteeId,
    permission_granteeType,
    permission_permissionValues,

    -- * Resource
    Resource (..),
    newResource,
    resource_enabledDate,
    resource_id,
    resource_state,
    resource_name,
    resource_email,
    resource_disabledDate,
    resource_type,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * User
    User (..),
    newUser,
    user_enabledDate,
    user_id,
    user_userRole,
    user_state,
    user_name,
    user_email,
    user_disabledDate,
    user_displayName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.WorkMail.Types.AccessControlRule
import Network.AWS.WorkMail.Types.AccessControlRuleEffect
import Network.AWS.WorkMail.Types.BookingOptions
import Network.AWS.WorkMail.Types.Delegate
import Network.AWS.WorkMail.Types.Domain
import Network.AWS.WorkMail.Types.EntityState
import Network.AWS.WorkMail.Types.FolderConfiguration
import Network.AWS.WorkMail.Types.FolderName
import Network.AWS.WorkMail.Types.Group
import Network.AWS.WorkMail.Types.MailboxExportJob
import Network.AWS.WorkMail.Types.MailboxExportJobState
import Network.AWS.WorkMail.Types.Member
import Network.AWS.WorkMail.Types.MemberType
import Network.AWS.WorkMail.Types.OrganizationSummary
import Network.AWS.WorkMail.Types.Permission
import Network.AWS.WorkMail.Types.PermissionType
import Network.AWS.WorkMail.Types.Resource
import Network.AWS.WorkMail.Types.ResourceType
import Network.AWS.WorkMail.Types.RetentionAction
import Network.AWS.WorkMail.Types.Tag
import Network.AWS.WorkMail.Types.User
import Network.AWS.WorkMail.Types.UserRole

-- | API version @2017-10-01@ of the Amazon WorkMail SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "WorkMail",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "workmail",
      Prelude._svcVersion = "2017-10-01",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "WorkMail",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The identifier supplied for the user, group, or resource does not exist
-- in your organization.
_EntityNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_EntityNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "EntityNotFoundException"

-- | An operation received a valid organization identifier that either
-- doesn\'t belong or exist in the system.
_OrganizationNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_OrganizationNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "OrganizationNotFoundException"

-- | This user, group, or resource name is not allowed in Amazon WorkMail.
_ReservedNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ReservedNameException =
  Prelude._MatchServiceError
    defaultService
    "ReservedNameException"

-- | After a domain has been added to the organization, it must be verified.
-- The domain is not yet verified.
_MailDomainStateException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MailDomainStateException =
  Prelude._MatchServiceError
    defaultService
    "MailDomainStateException"

-- | The resource can have up to 50 user-applied tags.
_TooManyTagsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyTagsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The user, group, or resource name isn\'t unique in Amazon WorkMail.
_NameAvailabilityException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NameAvailabilityException =
  Prelude._MatchServiceError
    defaultService
    "NameAvailabilityException"

-- | The configuration for a resource isn\'t valid. A resource must either be
-- able to auto-respond to requests or have at least one delegate
-- associated that can do so on its behalf.
_InvalidConfigurationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidConfigurationException =
  Prelude._MatchServiceError
    defaultService
    "InvalidConfigurationException"

-- | The organization must have a valid state to perform certain operations
-- on the organization or its members.
_OrganizationStateException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_OrganizationStateException =
  Prelude._MatchServiceError
    defaultService
    "OrganizationStateException"

-- | You are performing an operation on a user, group, or resource that
-- isn\'t in the expected state, such as trying to delete an active user.
_EntityStateException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_EntityStateException =
  Prelude._MatchServiceError
    defaultService
    "EntityStateException"

-- | The directory is already in use by another WorkMail organization in the
-- same account and Region.
_DirectoryInUseException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DirectoryInUseException =
  Prelude._MatchServiceError
    defaultService
    "DirectoryInUseException"

-- | You can\'t perform a write operation against a read-only directory.
_UnsupportedOperationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnsupportedOperationException =
  Prelude._MatchServiceError
    defaultService
    "UnsupportedOperationException"

-- | One or more of the input parameters don\'t match the service\'s
-- restrictions.
_InvalidParameterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParameterException =
  Prelude._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | The directory is unavailable. It might be located in another Region or
-- deleted.
_DirectoryUnavailableException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DirectoryUnavailableException =
  Prelude._MatchServiceError
    defaultService
    "DirectoryUnavailableException"

-- | The request exceeds the limit of the resource.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The supplied password doesn\'t match the minimum security constraints,
-- such as length or use of special characters.
_InvalidPasswordException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidPasswordException =
  Prelude._MatchServiceError
    defaultService
    "InvalidPasswordException"

-- | The email address that you\'re trying to assign is already created for a
-- different user, group, or resource.
_EmailAddressInUseException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_EmailAddressInUseException =
  Prelude._MatchServiceError
    defaultService
    "EmailAddressInUseException"

-- | The resource cannot be found.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The directory service doesn\'t recognize the credentials supplied by
-- WorkMail.
_DirectoryServiceAuthenticationFailedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DirectoryServiceAuthenticationFailedException =
  Prelude._MatchServiceError
    defaultService
    "DirectoryServiceAuthenticationFailedException"

-- | For an email or alias to be created in Amazon WorkMail, the included
-- domain must be defined in the organization.
_MailDomainNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MailDomainNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "MailDomainNotFoundException"

-- | The user, group, or resource that you\'re trying to register is already
-- registered.
_EntityAlreadyRegisteredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_EntityAlreadyRegisteredException =
  Prelude._MatchServiceError
    defaultService
    "EntityAlreadyRegisteredException"
