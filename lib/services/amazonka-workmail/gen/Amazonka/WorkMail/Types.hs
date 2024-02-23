{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WorkMail.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _DirectoryInUseException,
    _DirectoryServiceAuthenticationFailedException,
    _DirectoryUnavailableException,
    _EmailAddressInUseException,
    _EntityAlreadyRegisteredException,
    _EntityNotFoundException,
    _EntityStateException,
    _InvalidConfigurationException,
    _InvalidCustomSesConfigurationException,
    _InvalidParameterException,
    _InvalidPasswordException,
    _LimitExceededException,
    _MailDomainInUseException,
    _MailDomainNotFoundException,
    _MailDomainStateException,
    _NameAvailabilityException,
    _OrganizationNotFoundException,
    _OrganizationStateException,
    _ReservedNameException,
    _ResourceNotFoundException,
    _TooManyTagsException,
    _UnsupportedOperationException,

    -- * AccessControlRuleEffect
    AccessControlRuleEffect (..),

    -- * AccessEffect
    AccessEffect (..),

    -- * AvailabilityProviderType
    AvailabilityProviderType (..),

    -- * DnsRecordVerificationStatus
    DnsRecordVerificationStatus (..),

    -- * EntityState
    EntityState (..),

    -- * FolderName
    FolderName (..),

    -- * ImpersonationRoleType
    ImpersonationRoleType (..),

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
    accessControlRule_actions,
    accessControlRule_dateCreated,
    accessControlRule_dateModified,
    accessControlRule_description,
    accessControlRule_effect,
    accessControlRule_impersonationRoleIds,
    accessControlRule_ipRanges,
    accessControlRule_name,
    accessControlRule_notActions,
    accessControlRule_notImpersonationRoleIds,
    accessControlRule_notIpRanges,
    accessControlRule_notUserIds,
    accessControlRule_userIds,

    -- * AvailabilityConfiguration
    AvailabilityConfiguration (..),
    newAvailabilityConfiguration,
    availabilityConfiguration_dateCreated,
    availabilityConfiguration_dateModified,
    availabilityConfiguration_domainName,
    availabilityConfiguration_ewsProvider,
    availabilityConfiguration_lambdaProvider,
    availabilityConfiguration_providerType,

    -- * BookingOptions
    BookingOptions (..),
    newBookingOptions,
    bookingOptions_autoAcceptRequests,
    bookingOptions_autoDeclineConflictingRequests,
    bookingOptions_autoDeclineRecurringRequests,

    -- * Delegate
    Delegate (..),
    newDelegate,
    delegate_id,
    delegate_type,

    -- * DnsRecord
    DnsRecord (..),
    newDnsRecord,
    dnsRecord_hostname,
    dnsRecord_type,
    dnsRecord_value,

    -- * Domain
    Domain (..),
    newDomain,
    domain_domainName,
    domain_hostedZoneId,

    -- * EwsAvailabilityProvider
    EwsAvailabilityProvider (..),
    newEwsAvailabilityProvider,
    ewsAvailabilityProvider_ewsEndpoint,
    ewsAvailabilityProvider_ewsUsername,
    ewsAvailabilityProvider_ewsPassword,

    -- * FolderConfiguration
    FolderConfiguration (..),
    newFolderConfiguration,
    folderConfiguration_period,
    folderConfiguration_name,
    folderConfiguration_action,

    -- * Group
    Group (..),
    newGroup,
    group_disabledDate,
    group_email,
    group_enabledDate,
    group_id,
    group_name,
    group_state,

    -- * ImpersonationMatchedRule
    ImpersonationMatchedRule (..),
    newImpersonationMatchedRule,
    impersonationMatchedRule_impersonationRuleId,
    impersonationMatchedRule_name,

    -- * ImpersonationRole
    ImpersonationRole (..),
    newImpersonationRole,
    impersonationRole_dateCreated,
    impersonationRole_dateModified,
    impersonationRole_impersonationRoleId,
    impersonationRole_name,
    impersonationRole_type,

    -- * ImpersonationRule
    ImpersonationRule (..),
    newImpersonationRule,
    impersonationRule_description,
    impersonationRule_name,
    impersonationRule_notTargetUsers,
    impersonationRule_targetUsers,
    impersonationRule_impersonationRuleId,
    impersonationRule_effect,

    -- * LambdaAvailabilityProvider
    LambdaAvailabilityProvider (..),
    newLambdaAvailabilityProvider,
    lambdaAvailabilityProvider_lambdaArn,

    -- * MailDomainSummary
    MailDomainSummary (..),
    newMailDomainSummary,
    mailDomainSummary_defaultDomain,
    mailDomainSummary_domainName,

    -- * MailboxExportJob
    MailboxExportJob (..),
    newMailboxExportJob,
    mailboxExportJob_description,
    mailboxExportJob_endTime,
    mailboxExportJob_entityId,
    mailboxExportJob_estimatedProgress,
    mailboxExportJob_jobId,
    mailboxExportJob_s3BucketName,
    mailboxExportJob_s3Path,
    mailboxExportJob_startTime,
    mailboxExportJob_state,

    -- * Member
    Member (..),
    newMember,
    member_disabledDate,
    member_enabledDate,
    member_id,
    member_name,
    member_state,
    member_type,

    -- * MobileDeviceAccessMatchedRule
    MobileDeviceAccessMatchedRule (..),
    newMobileDeviceAccessMatchedRule,
    mobileDeviceAccessMatchedRule_mobileDeviceAccessRuleId,
    mobileDeviceAccessMatchedRule_name,

    -- * MobileDeviceAccessOverride
    MobileDeviceAccessOverride (..),
    newMobileDeviceAccessOverride,
    mobileDeviceAccessOverride_dateCreated,
    mobileDeviceAccessOverride_dateModified,
    mobileDeviceAccessOverride_description,
    mobileDeviceAccessOverride_deviceId,
    mobileDeviceAccessOverride_effect,
    mobileDeviceAccessOverride_userId,

    -- * MobileDeviceAccessRule
    MobileDeviceAccessRule (..),
    newMobileDeviceAccessRule,
    mobileDeviceAccessRule_dateCreated,
    mobileDeviceAccessRule_dateModified,
    mobileDeviceAccessRule_description,
    mobileDeviceAccessRule_deviceModels,
    mobileDeviceAccessRule_deviceOperatingSystems,
    mobileDeviceAccessRule_deviceTypes,
    mobileDeviceAccessRule_deviceUserAgents,
    mobileDeviceAccessRule_effect,
    mobileDeviceAccessRule_mobileDeviceAccessRuleId,
    mobileDeviceAccessRule_name,
    mobileDeviceAccessRule_notDeviceModels,
    mobileDeviceAccessRule_notDeviceOperatingSystems,
    mobileDeviceAccessRule_notDeviceTypes,
    mobileDeviceAccessRule_notDeviceUserAgents,

    -- * OrganizationSummary
    OrganizationSummary (..),
    newOrganizationSummary,
    organizationSummary_alias,
    organizationSummary_defaultMailDomain,
    organizationSummary_errorMessage,
    organizationSummary_organizationId,
    organizationSummary_state,

    -- * Permission
    Permission (..),
    newPermission,
    permission_granteeId,
    permission_granteeType,
    permission_permissionValues,

    -- * RedactedEwsAvailabilityProvider
    RedactedEwsAvailabilityProvider (..),
    newRedactedEwsAvailabilityProvider,
    redactedEwsAvailabilityProvider_ewsEndpoint,
    redactedEwsAvailabilityProvider_ewsUsername,

    -- * Resource
    Resource (..),
    newResource,
    resource_disabledDate,
    resource_email,
    resource_enabledDate,
    resource_id,
    resource_name,
    resource_state,
    resource_type,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * User
    User (..),
    newUser,
    user_disabledDate,
    user_displayName,
    user_email,
    user_enabledDate,
    user_id,
    user_name,
    user_state,
    user_userRole,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.WorkMail.Types.AccessControlRule
import Amazonka.WorkMail.Types.AccessControlRuleEffect
import Amazonka.WorkMail.Types.AccessEffect
import Amazonka.WorkMail.Types.AvailabilityConfiguration
import Amazonka.WorkMail.Types.AvailabilityProviderType
import Amazonka.WorkMail.Types.BookingOptions
import Amazonka.WorkMail.Types.Delegate
import Amazonka.WorkMail.Types.DnsRecord
import Amazonka.WorkMail.Types.DnsRecordVerificationStatus
import Amazonka.WorkMail.Types.Domain
import Amazonka.WorkMail.Types.EntityState
import Amazonka.WorkMail.Types.EwsAvailabilityProvider
import Amazonka.WorkMail.Types.FolderConfiguration
import Amazonka.WorkMail.Types.FolderName
import Amazonka.WorkMail.Types.Group
import Amazonka.WorkMail.Types.ImpersonationMatchedRule
import Amazonka.WorkMail.Types.ImpersonationRole
import Amazonka.WorkMail.Types.ImpersonationRoleType
import Amazonka.WorkMail.Types.ImpersonationRule
import Amazonka.WorkMail.Types.LambdaAvailabilityProvider
import Amazonka.WorkMail.Types.MailDomainSummary
import Amazonka.WorkMail.Types.MailboxExportJob
import Amazonka.WorkMail.Types.MailboxExportJobState
import Amazonka.WorkMail.Types.Member
import Amazonka.WorkMail.Types.MemberType
import Amazonka.WorkMail.Types.MobileDeviceAccessMatchedRule
import Amazonka.WorkMail.Types.MobileDeviceAccessOverride
import Amazonka.WorkMail.Types.MobileDeviceAccessRule
import Amazonka.WorkMail.Types.MobileDeviceAccessRuleEffect
import Amazonka.WorkMail.Types.OrganizationSummary
import Amazonka.WorkMail.Types.Permission
import Amazonka.WorkMail.Types.PermissionType
import Amazonka.WorkMail.Types.RedactedEwsAvailabilityProvider
import Amazonka.WorkMail.Types.Resource
import Amazonka.WorkMail.Types.ResourceType
import Amazonka.WorkMail.Types.RetentionAction
import Amazonka.WorkMail.Types.Tag
import Amazonka.WorkMail.Types.User
import Amazonka.WorkMail.Types.UserRole

-- | API version @2017-10-01@ of the Amazon WorkMail SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "WorkMail",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "workmail",
      Core.signingName = "workmail",
      Core.version = "2017-10-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "WorkMail",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The directory is already in use by another WorkMail organization in the
-- same account and Region.
_DirectoryInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DirectoryInUseException =
  Core._MatchServiceError
    defaultService
    "DirectoryInUseException"

-- | The directory service doesn\'t recognize the credentials supplied by
-- WorkMail.
_DirectoryServiceAuthenticationFailedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DirectoryServiceAuthenticationFailedException =
  Core._MatchServiceError
    defaultService
    "DirectoryServiceAuthenticationFailedException"

-- | The directory is unavailable. It might be located in another Region or
-- deleted.
_DirectoryUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DirectoryUnavailableException =
  Core._MatchServiceError
    defaultService
    "DirectoryUnavailableException"

-- | The email address that you\'re trying to assign is already created for a
-- different user, group, or resource.
_EmailAddressInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EmailAddressInUseException =
  Core._MatchServiceError
    defaultService
    "EmailAddressInUseException"

-- | The user, group, or resource that you\'re trying to register is already
-- registered.
_EntityAlreadyRegisteredException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EntityAlreadyRegisteredException =
  Core._MatchServiceError
    defaultService
    "EntityAlreadyRegisteredException"

-- | The identifier supplied for the user, group, or resource does not exist
-- in your organization.
_EntityNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EntityNotFoundException =
  Core._MatchServiceError
    defaultService
    "EntityNotFoundException"

-- | You are performing an operation on a user, group, or resource that
-- isn\'t in the expected state, such as trying to delete an active user.
_EntityStateException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EntityStateException =
  Core._MatchServiceError
    defaultService
    "EntityStateException"

-- | The configuration for a resource isn\'t valid. A resource must either be
-- able to auto-respond to requests or have at least one delegate
-- associated that can do so on its behalf.
_InvalidConfigurationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidConfigurationException =
  Core._MatchServiceError
    defaultService
    "InvalidConfigurationException"

-- | You SES configuration has customizations that WorkMail cannot save. The
-- error message lists the invalid setting. For examples of invalid
-- settings, refer to
-- <https://docs.aws.amazon.com/ses/latest/APIReference/API_CreateReceiptRule.html CreateReceiptRule>.
_InvalidCustomSesConfigurationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidCustomSesConfigurationException =
  Core._MatchServiceError
    defaultService
    "InvalidCustomSesConfigurationException"

-- | One or more of the input parameters don\'t match the service\'s
-- restrictions.
_InvalidParameterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | The supplied password doesn\'t match the minimum security constraints,
-- such as length or use of special characters.
_InvalidPasswordException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidPasswordException =
  Core._MatchServiceError
    defaultService
    "InvalidPasswordException"

-- | The request exceeds the limit of the resource.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The domain you\'re trying to change is in use by another user or
-- organization in your account. See the error message for details.
_MailDomainInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MailDomainInUseException =
  Core._MatchServiceError
    defaultService
    "MailDomainInUseException"

-- | The domain specified is not found in your organization.
_MailDomainNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MailDomainNotFoundException =
  Core._MatchServiceError
    defaultService
    "MailDomainNotFoundException"

-- | After a domain has been added to the organization, it must be verified.
-- The domain is not yet verified.
_MailDomainStateException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MailDomainStateException =
  Core._MatchServiceError
    defaultService
    "MailDomainStateException"

-- | The user, group, or resource name isn\'t unique in WorkMail.
_NameAvailabilityException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NameAvailabilityException =
  Core._MatchServiceError
    defaultService
    "NameAvailabilityException"

-- | An operation received a valid organization identifier that either
-- doesn\'t belong or exist in the system.
_OrganizationNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OrganizationNotFoundException =
  Core._MatchServiceError
    defaultService
    "OrganizationNotFoundException"

-- | The organization must have a valid state to perform certain operations
-- on the organization or its members.
_OrganizationStateException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OrganizationStateException =
  Core._MatchServiceError
    defaultService
    "OrganizationStateException"

-- | This user, group, or resource name is not allowed in WorkMail.
_ReservedNameException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReservedNameException =
  Core._MatchServiceError
    defaultService
    "ReservedNameException"

-- | The resource cannot be found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The resource can have up to 50 user-applied tags.
_TooManyTagsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | You can\'t perform a write operation against a read-only directory.
_UnsupportedOperationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnsupportedOperationException =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperationException"
