{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WorkMail.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NameAvailabilityException,
    _InvalidCustomSesConfigurationException,
    _UnsupportedOperationException,
    _MailDomainStateException,
    _OrganizationStateException,
    _TooManyTagsException,
    _MailDomainNotFoundException,
    _OrganizationNotFoundException,
    _InvalidPasswordException,
    _ResourceNotFoundException,
    _MailDomainInUseException,
    _DirectoryInUseException,
    _LimitExceededException,
    _DirectoryServiceAuthenticationFailedException,
    _EntityNotFoundException,
    _EntityAlreadyRegisteredException,
    _EntityStateException,
    _InvalidConfigurationException,
    _EmailAddressInUseException,
    _DirectoryUnavailableException,
    _ReservedNameException,
    _InvalidParameterException,

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
    accessControlRule_name,
    accessControlRule_notIpRanges,
    accessControlRule_effect,
    accessControlRule_notActions,
    accessControlRule_description,
    accessControlRule_dateCreated,
    accessControlRule_ipRanges,
    accessControlRule_notImpersonationRoleIds,
    accessControlRule_dateModified,
    accessControlRule_userIds,
    accessControlRule_impersonationRoleIds,
    accessControlRule_notUserIds,
    accessControlRule_actions,

    -- * AvailabilityConfiguration
    AvailabilityConfiguration (..),
    newAvailabilityConfiguration,
    availabilityConfiguration_ewsProvider,
    availabilityConfiguration_domainName,
    availabilityConfiguration_providerType,
    availabilityConfiguration_lambdaProvider,
    availabilityConfiguration_dateCreated,
    availabilityConfiguration_dateModified,

    -- * BookingOptions
    BookingOptions (..),
    newBookingOptions,
    bookingOptions_autoDeclineRecurringRequests,
    bookingOptions_autoDeclineConflictingRequests,
    bookingOptions_autoAcceptRequests,

    -- * Delegate
    Delegate (..),
    newDelegate,
    delegate_id,
    delegate_type,

    -- * DnsRecord
    DnsRecord (..),
    newDnsRecord,
    dnsRecord_type,
    dnsRecord_hostname,
    dnsRecord_value,

    -- * Domain
    Domain (..),
    newDomain,
    domain_hostedZoneId,
    domain_domainName,

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
    group_name,
    group_email,
    group_state,
    group_id,
    group_enabledDate,
    group_disabledDate,

    -- * ImpersonationMatchedRule
    ImpersonationMatchedRule (..),
    newImpersonationMatchedRule,
    impersonationMatchedRule_name,
    impersonationMatchedRule_impersonationRuleId,

    -- * ImpersonationRole
    ImpersonationRole (..),
    newImpersonationRole,
    impersonationRole_name,
    impersonationRole_type,
    impersonationRole_dateCreated,
    impersonationRole_impersonationRoleId,
    impersonationRole_dateModified,

    -- * ImpersonationRule
    ImpersonationRule (..),
    newImpersonationRule,
    impersonationRule_name,
    impersonationRule_targetUsers,
    impersonationRule_description,
    impersonationRule_notTargetUsers,
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
    mailboxExportJob_entityId,
    mailboxExportJob_s3BucketName,
    mailboxExportJob_state,
    mailboxExportJob_jobId,
    mailboxExportJob_endTime,
    mailboxExportJob_description,
    mailboxExportJob_s3Path,
    mailboxExportJob_startTime,
    mailboxExportJob_estimatedProgress,

    -- * Member
    Member (..),
    newMember,
    member_name,
    member_type,
    member_state,
    member_id,
    member_enabledDate,
    member_disabledDate,

    -- * MobileDeviceAccessMatchedRule
    MobileDeviceAccessMatchedRule (..),
    newMobileDeviceAccessMatchedRule,
    mobileDeviceAccessMatchedRule_name,
    mobileDeviceAccessMatchedRule_mobileDeviceAccessRuleId,

    -- * MobileDeviceAccessOverride
    MobileDeviceAccessOverride (..),
    newMobileDeviceAccessOverride,
    mobileDeviceAccessOverride_deviceId,
    mobileDeviceAccessOverride_effect,
    mobileDeviceAccessOverride_description,
    mobileDeviceAccessOverride_userId,
    mobileDeviceAccessOverride_dateCreated,
    mobileDeviceAccessOverride_dateModified,

    -- * MobileDeviceAccessRule
    MobileDeviceAccessRule (..),
    newMobileDeviceAccessRule,
    mobileDeviceAccessRule_name,
    mobileDeviceAccessRule_deviceTypes,
    mobileDeviceAccessRule_mobileDeviceAccessRuleId,
    mobileDeviceAccessRule_notDeviceUserAgents,
    mobileDeviceAccessRule_deviceUserAgents,
    mobileDeviceAccessRule_notDeviceModels,
    mobileDeviceAccessRule_effect,
    mobileDeviceAccessRule_description,
    mobileDeviceAccessRule_deviceModels,
    mobileDeviceAccessRule_notDeviceTypes,
    mobileDeviceAccessRule_deviceOperatingSystems,
    mobileDeviceAccessRule_notDeviceOperatingSystems,
    mobileDeviceAccessRule_dateCreated,
    mobileDeviceAccessRule_dateModified,

    -- * OrganizationSummary
    OrganizationSummary (..),
    newOrganizationSummary,
    organizationSummary_alias,
    organizationSummary_errorMessage,
    organizationSummary_defaultMailDomain,
    organizationSummary_state,
    organizationSummary_organizationId,

    -- * Permission
    Permission (..),
    newPermission,
    permission_granteeId,
    permission_granteeType,
    permission_permissionValues,

    -- * RedactedEwsAvailabilityProvider
    RedactedEwsAvailabilityProvider (..),
    newRedactedEwsAvailabilityProvider,
    redactedEwsAvailabilityProvider_ewsUsername,
    redactedEwsAvailabilityProvider_ewsEndpoint,

    -- * Resource
    Resource (..),
    newResource,
    resource_name,
    resource_type,
    resource_email,
    resource_state,
    resource_id,
    resource_enabledDate,
    resource_disabledDate,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * User
    User (..),
    newUser,
    user_name,
    user_email,
    user_displayName,
    user_state,
    user_id,
    user_userRole,
    user_enabledDate,
    user_disabledDate,
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The user, group, or resource name isn\'t unique in WorkMail.
_NameAvailabilityException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NameAvailabilityException =
  Core._MatchServiceError
    defaultService
    "NameAvailabilityException"

-- | You SES configuration has customizations that WorkMail cannot save. The
-- error message lists the invalid setting. For examples of invalid
-- settings, refer to
-- <https://docs.aws.amazon.com/ses/latest/APIReference/API_CreateReceiptRule.html CreateReceiptRule>.
_InvalidCustomSesConfigurationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCustomSesConfigurationException =
  Core._MatchServiceError
    defaultService
    "InvalidCustomSesConfigurationException"

-- | You can\'t perform a write operation against a read-only directory.
_UnsupportedOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperationException =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperationException"

-- | After a domain has been added to the organization, it must be verified.
-- The domain is not yet verified.
_MailDomainStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MailDomainStateException =
  Core._MatchServiceError
    defaultService
    "MailDomainStateException"

-- | The organization must have a valid state to perform certain operations
-- on the organization or its members.
_OrganizationStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OrganizationStateException =
  Core._MatchServiceError
    defaultService
    "OrganizationStateException"

-- | The resource can have up to 50 user-applied tags.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The domain specified is not found in your organization.
_MailDomainNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MailDomainNotFoundException =
  Core._MatchServiceError
    defaultService
    "MailDomainNotFoundException"

-- | An operation received a valid organization identifier that either
-- doesn\'t belong or exist in the system.
_OrganizationNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OrganizationNotFoundException =
  Core._MatchServiceError
    defaultService
    "OrganizationNotFoundException"

-- | The supplied password doesn\'t match the minimum security constraints,
-- such as length or use of special characters.
_InvalidPasswordException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPasswordException =
  Core._MatchServiceError
    defaultService
    "InvalidPasswordException"

-- | The resource cannot be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The domain you\'re trying to change is in use by another user or
-- organization in your account. See the error message for details.
_MailDomainInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MailDomainInUseException =
  Core._MatchServiceError
    defaultService
    "MailDomainInUseException"

-- | The directory is already in use by another WorkMail organization in the
-- same account and Region.
_DirectoryInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryInUseException =
  Core._MatchServiceError
    defaultService
    "DirectoryInUseException"

-- | The request exceeds the limit of the resource.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The directory service doesn\'t recognize the credentials supplied by
-- WorkMail.
_DirectoryServiceAuthenticationFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryServiceAuthenticationFailedException =
  Core._MatchServiceError
    defaultService
    "DirectoryServiceAuthenticationFailedException"

-- | The identifier supplied for the user, group, or resource does not exist
-- in your organization.
_EntityNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EntityNotFoundException =
  Core._MatchServiceError
    defaultService
    "EntityNotFoundException"

-- | The user, group, or resource that you\'re trying to register is already
-- registered.
_EntityAlreadyRegisteredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EntityAlreadyRegisteredException =
  Core._MatchServiceError
    defaultService
    "EntityAlreadyRegisteredException"

-- | You are performing an operation on a user, group, or resource that
-- isn\'t in the expected state, such as trying to delete an active user.
_EntityStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EntityStateException =
  Core._MatchServiceError
    defaultService
    "EntityStateException"

-- | The configuration for a resource isn\'t valid. A resource must either be
-- able to auto-respond to requests or have at least one delegate
-- associated that can do so on its behalf.
_InvalidConfigurationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidConfigurationException =
  Core._MatchServiceError
    defaultService
    "InvalidConfigurationException"

-- | The email address that you\'re trying to assign is already created for a
-- different user, group, or resource.
_EmailAddressInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EmailAddressInUseException =
  Core._MatchServiceError
    defaultService
    "EmailAddressInUseException"

-- | The directory is unavailable. It might be located in another Region or
-- deleted.
_DirectoryUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryUnavailableException =
  Core._MatchServiceError
    defaultService
    "DirectoryUnavailableException"

-- | This user, group, or resource name is not allowed in WorkMail.
_ReservedNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReservedNameException =
  Core._MatchServiceError
    defaultService
    "ReservedNameException"

-- | One or more of the input parameters don\'t match the service\'s
-- restrictions.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
