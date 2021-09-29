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
    _OrganizationNotFoundException,
    _EntityNotFoundException,
    _ReservedNameException,
    _NameAvailabilityException,
    _TooManyTagsException,
    _MailDomainStateException,
    _InvalidConfigurationException,
    _EntityStateException,
    _OrganizationStateException,
    _DirectoryInUseException,
    _UnsupportedOperationException,
    _InvalidParameterException,
    _DirectoryUnavailableException,
    _InvalidPasswordException,
    _EmailAddressInUseException,
    _LimitExceededException,
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

    -- * MobileDeviceAccessRuleEffect
    MobileDeviceAccessRuleEffect (..),

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
    accessControlRule_actions,
    accessControlRule_ipRanges,
    accessControlRule_dateModified,
    accessControlRule_name,
    accessControlRule_userIds,
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
    group_name,
    group_state,
    group_disabledDate,
    group_email,

    -- * MailboxExportJob
    MailboxExportJob (..),
    newMailboxExportJob,
    mailboxExportJob_estimatedProgress,
    mailboxExportJob_entityId,
    mailboxExportJob_s3Path,
    mailboxExportJob_startTime,
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
    member_name,
    member_state,
    member_disabledDate,
    member_type,

    -- * MobileDeviceAccessMatchedRule
    MobileDeviceAccessMatchedRule (..),
    newMobileDeviceAccessMatchedRule,
    mobileDeviceAccessMatchedRule_name,
    mobileDeviceAccessMatchedRule_mobileDeviceAccessRuleId,

    -- * MobileDeviceAccessRule
    MobileDeviceAccessRule (..),
    newMobileDeviceAccessRule,
    mobileDeviceAccessRule_effect,
    mobileDeviceAccessRule_dateCreated,
    mobileDeviceAccessRule_notDeviceModels,
    mobileDeviceAccessRule_notDeviceOperatingSystems,
    mobileDeviceAccessRule_deviceUserAgents,
    mobileDeviceAccessRule_dateModified,
    mobileDeviceAccessRule_name,
    mobileDeviceAccessRule_mobileDeviceAccessRuleId,
    mobileDeviceAccessRule_deviceModels,
    mobileDeviceAccessRule_notDeviceTypes,
    mobileDeviceAccessRule_description,
    mobileDeviceAccessRule_notDeviceUserAgents,
    mobileDeviceAccessRule_deviceTypes,
    mobileDeviceAccessRule_deviceOperatingSystems,

    -- * OrganizationSummary
    OrganizationSummary (..),
    newOrganizationSummary,
    organizationSummary_alias,
    organizationSummary_organizationId,
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
    resource_name,
    resource_state,
    resource_disabledDate,
    resource_email,
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
    user_name,
    user_state,
    user_disabledDate,
    user_email,
    user_displayName,
  )
where

import qualified Network.AWS.Core as Core
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
import Network.AWS.WorkMail.Types.MobileDeviceAccessMatchedRule
import Network.AWS.WorkMail.Types.MobileDeviceAccessRule
import Network.AWS.WorkMail.Types.MobileDeviceAccessRuleEffect
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
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "WorkMail",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "workmail",
      Core._serviceSigningName = "workmail",
      Core._serviceVersion = "2017-10-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "WorkMail",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | An operation received a valid organization identifier that either
-- doesn\'t belong or exist in the system.
_OrganizationNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OrganizationNotFoundException =
  Core._MatchServiceError
    defaultService
    "OrganizationNotFoundException"

-- | The identifier supplied for the user, group, or resource does not exist
-- in your organization.
_EntityNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EntityNotFoundException =
  Core._MatchServiceError
    defaultService
    "EntityNotFoundException"

-- | This user, group, or resource name is not allowed in Amazon WorkMail.
_ReservedNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReservedNameException =
  Core._MatchServiceError
    defaultService
    "ReservedNameException"

-- | The user, group, or resource name isn\'t unique in Amazon WorkMail.
_NameAvailabilityException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NameAvailabilityException =
  Core._MatchServiceError
    defaultService
    "NameAvailabilityException"

-- | The resource can have up to 50 user-applied tags.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | After a domain has been added to the organization, it must be verified.
-- The domain is not yet verified.
_MailDomainStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MailDomainStateException =
  Core._MatchServiceError
    defaultService
    "MailDomainStateException"

-- | The configuration for a resource isn\'t valid. A resource must either be
-- able to auto-respond to requests or have at least one delegate
-- associated that can do so on its behalf.
_InvalidConfigurationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidConfigurationException =
  Core._MatchServiceError
    defaultService
    "InvalidConfigurationException"

-- | You are performing an operation on a user, group, or resource that
-- isn\'t in the expected state, such as trying to delete an active user.
_EntityStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EntityStateException =
  Core._MatchServiceError
    defaultService
    "EntityStateException"

-- | The organization must have a valid state to perform certain operations
-- on the organization or its members.
_OrganizationStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OrganizationStateException =
  Core._MatchServiceError
    defaultService
    "OrganizationStateException"

-- | The directory is already in use by another WorkMail organization in the
-- same account and Region.
_DirectoryInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryInUseException =
  Core._MatchServiceError
    defaultService
    "DirectoryInUseException"

-- | You can\'t perform a write operation against a read-only directory.
_UnsupportedOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperationException =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperationException"

-- | One or more of the input parameters don\'t match the service\'s
-- restrictions.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | The directory is unavailable. It might be located in another Region or
-- deleted.
_DirectoryUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryUnavailableException =
  Core._MatchServiceError
    defaultService
    "DirectoryUnavailableException"

-- | The supplied password doesn\'t match the minimum security constraints,
-- such as length or use of special characters.
_InvalidPasswordException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPasswordException =
  Core._MatchServiceError
    defaultService
    "InvalidPasswordException"

-- | The email address that you\'re trying to assign is already created for a
-- different user, group, or resource.
_EmailAddressInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EmailAddressInUseException =
  Core._MatchServiceError
    defaultService
    "EmailAddressInUseException"

-- | The request exceeds the limit of the resource.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The resource cannot be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The directory service doesn\'t recognize the credentials supplied by
-- WorkMail.
_DirectoryServiceAuthenticationFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryServiceAuthenticationFailedException =
  Core._MatchServiceError
    defaultService
    "DirectoryServiceAuthenticationFailedException"

-- | For an email or alias to be created in Amazon WorkMail, the included
-- domain must be defined in the organization.
_MailDomainNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MailDomainNotFoundException =
  Core._MatchServiceError
    defaultService
    "MailDomainNotFoundException"

-- | The user, group, or resource that you\'re trying to register is already
-- registered.
_EntityAlreadyRegisteredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EntityAlreadyRegisteredException =
  Core._MatchServiceError
    defaultService
    "EntityAlreadyRegisteredException"
