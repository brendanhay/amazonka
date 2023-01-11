{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.WorkMail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-10-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- WorkMail is a secure, managed business email and calendaring service
-- with support for existing desktop and mobile email clients. You can
-- access your email, contacts, and calendars using Microsoft Outlook, your
-- browser, or other native iOS and Android email applications. You can
-- integrate WorkMail with your existing corporate directory and control
-- both the keys that encrypt your data and the location in which your data
-- is stored.
--
-- The WorkMail API is designed for the following scenarios:
--
-- -   Listing and describing organizations
--
-- -   Managing users
--
-- -   Managing groups
--
-- -   Managing resources
--
-- All WorkMail API operations are Amazon-authenticated and
-- certificate-signed. They not only require the use of the AWS SDK, but
-- also allow for the exclusive use of AWS Identity and Access Management
-- users and roles to help facilitate access, trust, and permission
-- policies. By creating a role and allowing an IAM user to access the
-- WorkMail site, the IAM user gains full administrative visibility into
-- the entire WorkMail organization (or as set in the IAM policy). This
-- includes, but is not limited to, the ability to create, update, and
-- delete users, groups, and resources. This allows developers to perform
-- the scenarios listed above, as well as give users the ability to grant
-- access on a selective basis using the IAM model.
module Amazonka.WorkMail
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** DirectoryInUseException
    _DirectoryInUseException,

    -- ** DirectoryServiceAuthenticationFailedException
    _DirectoryServiceAuthenticationFailedException,

    -- ** DirectoryUnavailableException
    _DirectoryUnavailableException,

    -- ** EmailAddressInUseException
    _EmailAddressInUseException,

    -- ** EntityAlreadyRegisteredException
    _EntityAlreadyRegisteredException,

    -- ** EntityNotFoundException
    _EntityNotFoundException,

    -- ** EntityStateException
    _EntityStateException,

    -- ** InvalidConfigurationException
    _InvalidConfigurationException,

    -- ** InvalidCustomSesConfigurationException
    _InvalidCustomSesConfigurationException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidPasswordException
    _InvalidPasswordException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** MailDomainInUseException
    _MailDomainInUseException,

    -- ** MailDomainNotFoundException
    _MailDomainNotFoundException,

    -- ** MailDomainStateException
    _MailDomainStateException,

    -- ** NameAvailabilityException
    _NameAvailabilityException,

    -- ** OrganizationNotFoundException
    _OrganizationNotFoundException,

    -- ** OrganizationStateException
    _OrganizationStateException,

    -- ** ReservedNameException
    _ReservedNameException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** UnsupportedOperationException
    _UnsupportedOperationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateDelegateToResource
    AssociateDelegateToResource (AssociateDelegateToResource'),
    newAssociateDelegateToResource,
    AssociateDelegateToResourceResponse (AssociateDelegateToResourceResponse'),
    newAssociateDelegateToResourceResponse,

    -- ** AssociateMemberToGroup
    AssociateMemberToGroup (AssociateMemberToGroup'),
    newAssociateMemberToGroup,
    AssociateMemberToGroupResponse (AssociateMemberToGroupResponse'),
    newAssociateMemberToGroupResponse,

    -- ** AssumeImpersonationRole
    AssumeImpersonationRole (AssumeImpersonationRole'),
    newAssumeImpersonationRole,
    AssumeImpersonationRoleResponse (AssumeImpersonationRoleResponse'),
    newAssumeImpersonationRoleResponse,

    -- ** CancelMailboxExportJob
    CancelMailboxExportJob (CancelMailboxExportJob'),
    newCancelMailboxExportJob,
    CancelMailboxExportJobResponse (CancelMailboxExportJobResponse'),
    newCancelMailboxExportJobResponse,

    -- ** CreateAlias
    CreateAlias (CreateAlias'),
    newCreateAlias,
    CreateAliasResponse (CreateAliasResponse'),
    newCreateAliasResponse,

    -- ** CreateAvailabilityConfiguration
    CreateAvailabilityConfiguration (CreateAvailabilityConfiguration'),
    newCreateAvailabilityConfiguration,
    CreateAvailabilityConfigurationResponse (CreateAvailabilityConfigurationResponse'),
    newCreateAvailabilityConfigurationResponse,

    -- ** CreateGroup
    CreateGroup (CreateGroup'),
    newCreateGroup,
    CreateGroupResponse (CreateGroupResponse'),
    newCreateGroupResponse,

    -- ** CreateImpersonationRole
    CreateImpersonationRole (CreateImpersonationRole'),
    newCreateImpersonationRole,
    CreateImpersonationRoleResponse (CreateImpersonationRoleResponse'),
    newCreateImpersonationRoleResponse,

    -- ** CreateMobileDeviceAccessRule
    CreateMobileDeviceAccessRule (CreateMobileDeviceAccessRule'),
    newCreateMobileDeviceAccessRule,
    CreateMobileDeviceAccessRuleResponse (CreateMobileDeviceAccessRuleResponse'),
    newCreateMobileDeviceAccessRuleResponse,

    -- ** CreateOrganization
    CreateOrganization (CreateOrganization'),
    newCreateOrganization,
    CreateOrganizationResponse (CreateOrganizationResponse'),
    newCreateOrganizationResponse,

    -- ** CreateResource
    CreateResource (CreateResource'),
    newCreateResource,
    CreateResourceResponse (CreateResourceResponse'),
    newCreateResourceResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    CreateUserResponse (CreateUserResponse'),
    newCreateUserResponse,

    -- ** DeleteAccessControlRule
    DeleteAccessControlRule (DeleteAccessControlRule'),
    newDeleteAccessControlRule,
    DeleteAccessControlRuleResponse (DeleteAccessControlRuleResponse'),
    newDeleteAccessControlRuleResponse,

    -- ** DeleteAlias
    DeleteAlias (DeleteAlias'),
    newDeleteAlias,
    DeleteAliasResponse (DeleteAliasResponse'),
    newDeleteAliasResponse,

    -- ** DeleteAvailabilityConfiguration
    DeleteAvailabilityConfiguration (DeleteAvailabilityConfiguration'),
    newDeleteAvailabilityConfiguration,
    DeleteAvailabilityConfigurationResponse (DeleteAvailabilityConfigurationResponse'),
    newDeleteAvailabilityConfigurationResponse,

    -- ** DeleteEmailMonitoringConfiguration
    DeleteEmailMonitoringConfiguration (DeleteEmailMonitoringConfiguration'),
    newDeleteEmailMonitoringConfiguration,
    DeleteEmailMonitoringConfigurationResponse (DeleteEmailMonitoringConfigurationResponse'),
    newDeleteEmailMonitoringConfigurationResponse,

    -- ** DeleteGroup
    DeleteGroup (DeleteGroup'),
    newDeleteGroup,
    DeleteGroupResponse (DeleteGroupResponse'),
    newDeleteGroupResponse,

    -- ** DeleteImpersonationRole
    DeleteImpersonationRole (DeleteImpersonationRole'),
    newDeleteImpersonationRole,
    DeleteImpersonationRoleResponse (DeleteImpersonationRoleResponse'),
    newDeleteImpersonationRoleResponse,

    -- ** DeleteMailboxPermissions
    DeleteMailboxPermissions (DeleteMailboxPermissions'),
    newDeleteMailboxPermissions,
    DeleteMailboxPermissionsResponse (DeleteMailboxPermissionsResponse'),
    newDeleteMailboxPermissionsResponse,

    -- ** DeleteMobileDeviceAccessOverride
    DeleteMobileDeviceAccessOverride (DeleteMobileDeviceAccessOverride'),
    newDeleteMobileDeviceAccessOverride,
    DeleteMobileDeviceAccessOverrideResponse (DeleteMobileDeviceAccessOverrideResponse'),
    newDeleteMobileDeviceAccessOverrideResponse,

    -- ** DeleteMobileDeviceAccessRule
    DeleteMobileDeviceAccessRule (DeleteMobileDeviceAccessRule'),
    newDeleteMobileDeviceAccessRule,
    DeleteMobileDeviceAccessRuleResponse (DeleteMobileDeviceAccessRuleResponse'),
    newDeleteMobileDeviceAccessRuleResponse,

    -- ** DeleteOrganization
    DeleteOrganization (DeleteOrganization'),
    newDeleteOrganization,
    DeleteOrganizationResponse (DeleteOrganizationResponse'),
    newDeleteOrganizationResponse,

    -- ** DeleteResource
    DeleteResource (DeleteResource'),
    newDeleteResource,
    DeleteResourceResponse (DeleteResourceResponse'),
    newDeleteResourceResponse,

    -- ** DeleteRetentionPolicy
    DeleteRetentionPolicy (DeleteRetentionPolicy'),
    newDeleteRetentionPolicy,
    DeleteRetentionPolicyResponse (DeleteRetentionPolicyResponse'),
    newDeleteRetentionPolicyResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    DeleteUserResponse (DeleteUserResponse'),
    newDeleteUserResponse,

    -- ** DeregisterFromWorkMail
    DeregisterFromWorkMail (DeregisterFromWorkMail'),
    newDeregisterFromWorkMail,
    DeregisterFromWorkMailResponse (DeregisterFromWorkMailResponse'),
    newDeregisterFromWorkMailResponse,

    -- ** DeregisterMailDomain
    DeregisterMailDomain (DeregisterMailDomain'),
    newDeregisterMailDomain,
    DeregisterMailDomainResponse (DeregisterMailDomainResponse'),
    newDeregisterMailDomainResponse,

    -- ** DescribeEmailMonitoringConfiguration
    DescribeEmailMonitoringConfiguration (DescribeEmailMonitoringConfiguration'),
    newDescribeEmailMonitoringConfiguration,
    DescribeEmailMonitoringConfigurationResponse (DescribeEmailMonitoringConfigurationResponse'),
    newDescribeEmailMonitoringConfigurationResponse,

    -- ** DescribeGroup
    DescribeGroup (DescribeGroup'),
    newDescribeGroup,
    DescribeGroupResponse (DescribeGroupResponse'),
    newDescribeGroupResponse,

    -- ** DescribeInboundDmarcSettings
    DescribeInboundDmarcSettings (DescribeInboundDmarcSettings'),
    newDescribeInboundDmarcSettings,
    DescribeInboundDmarcSettingsResponse (DescribeInboundDmarcSettingsResponse'),
    newDescribeInboundDmarcSettingsResponse,

    -- ** DescribeMailboxExportJob
    DescribeMailboxExportJob (DescribeMailboxExportJob'),
    newDescribeMailboxExportJob,
    DescribeMailboxExportJobResponse (DescribeMailboxExportJobResponse'),
    newDescribeMailboxExportJobResponse,

    -- ** DescribeOrganization
    DescribeOrganization (DescribeOrganization'),
    newDescribeOrganization,
    DescribeOrganizationResponse (DescribeOrganizationResponse'),
    newDescribeOrganizationResponse,

    -- ** DescribeResource
    DescribeResource (DescribeResource'),
    newDescribeResource,
    DescribeResourceResponse (DescribeResourceResponse'),
    newDescribeResourceResponse,

    -- ** DescribeUser
    DescribeUser (DescribeUser'),
    newDescribeUser,
    DescribeUserResponse (DescribeUserResponse'),
    newDescribeUserResponse,

    -- ** DisassociateDelegateFromResource
    DisassociateDelegateFromResource (DisassociateDelegateFromResource'),
    newDisassociateDelegateFromResource,
    DisassociateDelegateFromResourceResponse (DisassociateDelegateFromResourceResponse'),
    newDisassociateDelegateFromResourceResponse,

    -- ** DisassociateMemberFromGroup
    DisassociateMemberFromGroup (DisassociateMemberFromGroup'),
    newDisassociateMemberFromGroup,
    DisassociateMemberFromGroupResponse (DisassociateMemberFromGroupResponse'),
    newDisassociateMemberFromGroupResponse,

    -- ** GetAccessControlEffect
    GetAccessControlEffect (GetAccessControlEffect'),
    newGetAccessControlEffect,
    GetAccessControlEffectResponse (GetAccessControlEffectResponse'),
    newGetAccessControlEffectResponse,

    -- ** GetDefaultRetentionPolicy
    GetDefaultRetentionPolicy (GetDefaultRetentionPolicy'),
    newGetDefaultRetentionPolicy,
    GetDefaultRetentionPolicyResponse (GetDefaultRetentionPolicyResponse'),
    newGetDefaultRetentionPolicyResponse,

    -- ** GetImpersonationRole
    GetImpersonationRole (GetImpersonationRole'),
    newGetImpersonationRole,
    GetImpersonationRoleResponse (GetImpersonationRoleResponse'),
    newGetImpersonationRoleResponse,

    -- ** GetImpersonationRoleEffect
    GetImpersonationRoleEffect (GetImpersonationRoleEffect'),
    newGetImpersonationRoleEffect,
    GetImpersonationRoleEffectResponse (GetImpersonationRoleEffectResponse'),
    newGetImpersonationRoleEffectResponse,

    -- ** GetMailDomain
    GetMailDomain (GetMailDomain'),
    newGetMailDomain,
    GetMailDomainResponse (GetMailDomainResponse'),
    newGetMailDomainResponse,

    -- ** GetMailboxDetails
    GetMailboxDetails (GetMailboxDetails'),
    newGetMailboxDetails,
    GetMailboxDetailsResponse (GetMailboxDetailsResponse'),
    newGetMailboxDetailsResponse,

    -- ** GetMobileDeviceAccessEffect
    GetMobileDeviceAccessEffect (GetMobileDeviceAccessEffect'),
    newGetMobileDeviceAccessEffect,
    GetMobileDeviceAccessEffectResponse (GetMobileDeviceAccessEffectResponse'),
    newGetMobileDeviceAccessEffectResponse,

    -- ** GetMobileDeviceAccessOverride
    GetMobileDeviceAccessOverride (GetMobileDeviceAccessOverride'),
    newGetMobileDeviceAccessOverride,
    GetMobileDeviceAccessOverrideResponse (GetMobileDeviceAccessOverrideResponse'),
    newGetMobileDeviceAccessOverrideResponse,

    -- ** ListAccessControlRules
    ListAccessControlRules (ListAccessControlRules'),
    newListAccessControlRules,
    ListAccessControlRulesResponse (ListAccessControlRulesResponse'),
    newListAccessControlRulesResponse,

    -- ** ListAliases (Paginated)
    ListAliases (ListAliases'),
    newListAliases,
    ListAliasesResponse (ListAliasesResponse'),
    newListAliasesResponse,

    -- ** ListAvailabilityConfigurations (Paginated)
    ListAvailabilityConfigurations (ListAvailabilityConfigurations'),
    newListAvailabilityConfigurations,
    ListAvailabilityConfigurationsResponse (ListAvailabilityConfigurationsResponse'),
    newListAvailabilityConfigurationsResponse,

    -- ** ListGroupMembers (Paginated)
    ListGroupMembers (ListGroupMembers'),
    newListGroupMembers,
    ListGroupMembersResponse (ListGroupMembersResponse'),
    newListGroupMembersResponse,

    -- ** ListGroups (Paginated)
    ListGroups (ListGroups'),
    newListGroups,
    ListGroupsResponse (ListGroupsResponse'),
    newListGroupsResponse,

    -- ** ListImpersonationRoles
    ListImpersonationRoles (ListImpersonationRoles'),
    newListImpersonationRoles,
    ListImpersonationRolesResponse (ListImpersonationRolesResponse'),
    newListImpersonationRolesResponse,

    -- ** ListMailDomains
    ListMailDomains (ListMailDomains'),
    newListMailDomains,
    ListMailDomainsResponse (ListMailDomainsResponse'),
    newListMailDomainsResponse,

    -- ** ListMailboxExportJobs
    ListMailboxExportJobs (ListMailboxExportJobs'),
    newListMailboxExportJobs,
    ListMailboxExportJobsResponse (ListMailboxExportJobsResponse'),
    newListMailboxExportJobsResponse,

    -- ** ListMailboxPermissions (Paginated)
    ListMailboxPermissions (ListMailboxPermissions'),
    newListMailboxPermissions,
    ListMailboxPermissionsResponse (ListMailboxPermissionsResponse'),
    newListMailboxPermissionsResponse,

    -- ** ListMobileDeviceAccessOverrides
    ListMobileDeviceAccessOverrides (ListMobileDeviceAccessOverrides'),
    newListMobileDeviceAccessOverrides,
    ListMobileDeviceAccessOverridesResponse (ListMobileDeviceAccessOverridesResponse'),
    newListMobileDeviceAccessOverridesResponse,

    -- ** ListMobileDeviceAccessRules
    ListMobileDeviceAccessRules (ListMobileDeviceAccessRules'),
    newListMobileDeviceAccessRules,
    ListMobileDeviceAccessRulesResponse (ListMobileDeviceAccessRulesResponse'),
    newListMobileDeviceAccessRulesResponse,

    -- ** ListOrganizations (Paginated)
    ListOrganizations (ListOrganizations'),
    newListOrganizations,
    ListOrganizationsResponse (ListOrganizationsResponse'),
    newListOrganizationsResponse,

    -- ** ListResourceDelegates (Paginated)
    ListResourceDelegates (ListResourceDelegates'),
    newListResourceDelegates,
    ListResourceDelegatesResponse (ListResourceDelegatesResponse'),
    newListResourceDelegatesResponse,

    -- ** ListResources (Paginated)
    ListResources (ListResources'),
    newListResources,
    ListResourcesResponse (ListResourcesResponse'),
    newListResourcesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListUsers (Paginated)
    ListUsers (ListUsers'),
    newListUsers,
    ListUsersResponse (ListUsersResponse'),
    newListUsersResponse,

    -- ** PutAccessControlRule
    PutAccessControlRule (PutAccessControlRule'),
    newPutAccessControlRule,
    PutAccessControlRuleResponse (PutAccessControlRuleResponse'),
    newPutAccessControlRuleResponse,

    -- ** PutEmailMonitoringConfiguration
    PutEmailMonitoringConfiguration (PutEmailMonitoringConfiguration'),
    newPutEmailMonitoringConfiguration,
    PutEmailMonitoringConfigurationResponse (PutEmailMonitoringConfigurationResponse'),
    newPutEmailMonitoringConfigurationResponse,

    -- ** PutInboundDmarcSettings
    PutInboundDmarcSettings (PutInboundDmarcSettings'),
    newPutInboundDmarcSettings,
    PutInboundDmarcSettingsResponse (PutInboundDmarcSettingsResponse'),
    newPutInboundDmarcSettingsResponse,

    -- ** PutMailboxPermissions
    PutMailboxPermissions (PutMailboxPermissions'),
    newPutMailboxPermissions,
    PutMailboxPermissionsResponse (PutMailboxPermissionsResponse'),
    newPutMailboxPermissionsResponse,

    -- ** PutMobileDeviceAccessOverride
    PutMobileDeviceAccessOverride (PutMobileDeviceAccessOverride'),
    newPutMobileDeviceAccessOverride,
    PutMobileDeviceAccessOverrideResponse (PutMobileDeviceAccessOverrideResponse'),
    newPutMobileDeviceAccessOverrideResponse,

    -- ** PutRetentionPolicy
    PutRetentionPolicy (PutRetentionPolicy'),
    newPutRetentionPolicy,
    PutRetentionPolicyResponse (PutRetentionPolicyResponse'),
    newPutRetentionPolicyResponse,

    -- ** RegisterMailDomain
    RegisterMailDomain (RegisterMailDomain'),
    newRegisterMailDomain,
    RegisterMailDomainResponse (RegisterMailDomainResponse'),
    newRegisterMailDomainResponse,

    -- ** RegisterToWorkMail
    RegisterToWorkMail (RegisterToWorkMail'),
    newRegisterToWorkMail,
    RegisterToWorkMailResponse (RegisterToWorkMailResponse'),
    newRegisterToWorkMailResponse,

    -- ** ResetPassword
    ResetPassword (ResetPassword'),
    newResetPassword,
    ResetPasswordResponse (ResetPasswordResponse'),
    newResetPasswordResponse,

    -- ** StartMailboxExportJob
    StartMailboxExportJob (StartMailboxExportJob'),
    newStartMailboxExportJob,
    StartMailboxExportJobResponse (StartMailboxExportJobResponse'),
    newStartMailboxExportJobResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** TestAvailabilityConfiguration
    TestAvailabilityConfiguration (TestAvailabilityConfiguration'),
    newTestAvailabilityConfiguration,
    TestAvailabilityConfigurationResponse (TestAvailabilityConfigurationResponse'),
    newTestAvailabilityConfigurationResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateAvailabilityConfiguration
    UpdateAvailabilityConfiguration (UpdateAvailabilityConfiguration'),
    newUpdateAvailabilityConfiguration,
    UpdateAvailabilityConfigurationResponse (UpdateAvailabilityConfigurationResponse'),
    newUpdateAvailabilityConfigurationResponse,

    -- ** UpdateDefaultMailDomain
    UpdateDefaultMailDomain (UpdateDefaultMailDomain'),
    newUpdateDefaultMailDomain,
    UpdateDefaultMailDomainResponse (UpdateDefaultMailDomainResponse'),
    newUpdateDefaultMailDomainResponse,

    -- ** UpdateImpersonationRole
    UpdateImpersonationRole (UpdateImpersonationRole'),
    newUpdateImpersonationRole,
    UpdateImpersonationRoleResponse (UpdateImpersonationRoleResponse'),
    newUpdateImpersonationRoleResponse,

    -- ** UpdateMailboxQuota
    UpdateMailboxQuota (UpdateMailboxQuota'),
    newUpdateMailboxQuota,
    UpdateMailboxQuotaResponse (UpdateMailboxQuotaResponse'),
    newUpdateMailboxQuotaResponse,

    -- ** UpdateMobileDeviceAccessRule
    UpdateMobileDeviceAccessRule (UpdateMobileDeviceAccessRule'),
    newUpdateMobileDeviceAccessRule,
    UpdateMobileDeviceAccessRuleResponse (UpdateMobileDeviceAccessRuleResponse'),
    newUpdateMobileDeviceAccessRuleResponse,

    -- ** UpdatePrimaryEmailAddress
    UpdatePrimaryEmailAddress (UpdatePrimaryEmailAddress'),
    newUpdatePrimaryEmailAddress,
    UpdatePrimaryEmailAddressResponse (UpdatePrimaryEmailAddressResponse'),
    newUpdatePrimaryEmailAddressResponse,

    -- ** UpdateResource
    UpdateResource (UpdateResource'),
    newUpdateResource,
    UpdateResourceResponse (UpdateResourceResponse'),
    newUpdateResourceResponse,

    -- * Types

    -- ** AccessControlRuleEffect
    AccessControlRuleEffect (..),

    -- ** AccessEffect
    AccessEffect (..),

    -- ** AvailabilityProviderType
    AvailabilityProviderType (..),

    -- ** DnsRecordVerificationStatus
    DnsRecordVerificationStatus (..),

    -- ** EntityState
    EntityState (..),

    -- ** FolderName
    FolderName (..),

    -- ** ImpersonationRoleType
    ImpersonationRoleType (..),

    -- ** MailboxExportJobState
    MailboxExportJobState (..),

    -- ** MemberType
    MemberType (..),

    -- ** MobileDeviceAccessRuleEffect
    MobileDeviceAccessRuleEffect (..),

    -- ** PermissionType
    PermissionType (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** RetentionAction
    RetentionAction (..),

    -- ** UserRole
    UserRole (..),

    -- ** AccessControlRule
    AccessControlRule (AccessControlRule'),
    newAccessControlRule,

    -- ** AvailabilityConfiguration
    AvailabilityConfiguration (AvailabilityConfiguration'),
    newAvailabilityConfiguration,

    -- ** BookingOptions
    BookingOptions (BookingOptions'),
    newBookingOptions,

    -- ** Delegate
    Delegate (Delegate'),
    newDelegate,

    -- ** DnsRecord
    DnsRecord (DnsRecord'),
    newDnsRecord,

    -- ** Domain
    Domain (Domain'),
    newDomain,

    -- ** EwsAvailabilityProvider
    EwsAvailabilityProvider (EwsAvailabilityProvider'),
    newEwsAvailabilityProvider,

    -- ** FolderConfiguration
    FolderConfiguration (FolderConfiguration'),
    newFolderConfiguration,

    -- ** Group
    Group (Group'),
    newGroup,

    -- ** ImpersonationMatchedRule
    ImpersonationMatchedRule (ImpersonationMatchedRule'),
    newImpersonationMatchedRule,

    -- ** ImpersonationRole
    ImpersonationRole (ImpersonationRole'),
    newImpersonationRole,

    -- ** ImpersonationRule
    ImpersonationRule (ImpersonationRule'),
    newImpersonationRule,

    -- ** LambdaAvailabilityProvider
    LambdaAvailabilityProvider (LambdaAvailabilityProvider'),
    newLambdaAvailabilityProvider,

    -- ** MailDomainSummary
    MailDomainSummary (MailDomainSummary'),
    newMailDomainSummary,

    -- ** MailboxExportJob
    MailboxExportJob (MailboxExportJob'),
    newMailboxExportJob,

    -- ** Member
    Member (Member'),
    newMember,

    -- ** MobileDeviceAccessMatchedRule
    MobileDeviceAccessMatchedRule (MobileDeviceAccessMatchedRule'),
    newMobileDeviceAccessMatchedRule,

    -- ** MobileDeviceAccessOverride
    MobileDeviceAccessOverride (MobileDeviceAccessOverride'),
    newMobileDeviceAccessOverride,

    -- ** MobileDeviceAccessRule
    MobileDeviceAccessRule (MobileDeviceAccessRule'),
    newMobileDeviceAccessRule,

    -- ** OrganizationSummary
    OrganizationSummary (OrganizationSummary'),
    newOrganizationSummary,

    -- ** Permission
    Permission (Permission'),
    newPermission,

    -- ** RedactedEwsAvailabilityProvider
    RedactedEwsAvailabilityProvider (RedactedEwsAvailabilityProvider'),
    newRedactedEwsAvailabilityProvider,

    -- ** Resource
    Resource (Resource'),
    newResource,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** User
    User (User'),
    newUser,
  )
where

import Amazonka.WorkMail.AssociateDelegateToResource
import Amazonka.WorkMail.AssociateMemberToGroup
import Amazonka.WorkMail.AssumeImpersonationRole
import Amazonka.WorkMail.CancelMailboxExportJob
import Amazonka.WorkMail.CreateAlias
import Amazonka.WorkMail.CreateAvailabilityConfiguration
import Amazonka.WorkMail.CreateGroup
import Amazonka.WorkMail.CreateImpersonationRole
import Amazonka.WorkMail.CreateMobileDeviceAccessRule
import Amazonka.WorkMail.CreateOrganization
import Amazonka.WorkMail.CreateResource
import Amazonka.WorkMail.CreateUser
import Amazonka.WorkMail.DeleteAccessControlRule
import Amazonka.WorkMail.DeleteAlias
import Amazonka.WorkMail.DeleteAvailabilityConfiguration
import Amazonka.WorkMail.DeleteEmailMonitoringConfiguration
import Amazonka.WorkMail.DeleteGroup
import Amazonka.WorkMail.DeleteImpersonationRole
import Amazonka.WorkMail.DeleteMailboxPermissions
import Amazonka.WorkMail.DeleteMobileDeviceAccessOverride
import Amazonka.WorkMail.DeleteMobileDeviceAccessRule
import Amazonka.WorkMail.DeleteOrganization
import Amazonka.WorkMail.DeleteResource
import Amazonka.WorkMail.DeleteRetentionPolicy
import Amazonka.WorkMail.DeleteUser
import Amazonka.WorkMail.DeregisterFromWorkMail
import Amazonka.WorkMail.DeregisterMailDomain
import Amazonka.WorkMail.DescribeEmailMonitoringConfiguration
import Amazonka.WorkMail.DescribeGroup
import Amazonka.WorkMail.DescribeInboundDmarcSettings
import Amazonka.WorkMail.DescribeMailboxExportJob
import Amazonka.WorkMail.DescribeOrganization
import Amazonka.WorkMail.DescribeResource
import Amazonka.WorkMail.DescribeUser
import Amazonka.WorkMail.DisassociateDelegateFromResource
import Amazonka.WorkMail.DisassociateMemberFromGroup
import Amazonka.WorkMail.GetAccessControlEffect
import Amazonka.WorkMail.GetDefaultRetentionPolicy
import Amazonka.WorkMail.GetImpersonationRole
import Amazonka.WorkMail.GetImpersonationRoleEffect
import Amazonka.WorkMail.GetMailDomain
import Amazonka.WorkMail.GetMailboxDetails
import Amazonka.WorkMail.GetMobileDeviceAccessEffect
import Amazonka.WorkMail.GetMobileDeviceAccessOverride
import Amazonka.WorkMail.Lens
import Amazonka.WorkMail.ListAccessControlRules
import Amazonka.WorkMail.ListAliases
import Amazonka.WorkMail.ListAvailabilityConfigurations
import Amazonka.WorkMail.ListGroupMembers
import Amazonka.WorkMail.ListGroups
import Amazonka.WorkMail.ListImpersonationRoles
import Amazonka.WorkMail.ListMailDomains
import Amazonka.WorkMail.ListMailboxExportJobs
import Amazonka.WorkMail.ListMailboxPermissions
import Amazonka.WorkMail.ListMobileDeviceAccessOverrides
import Amazonka.WorkMail.ListMobileDeviceAccessRules
import Amazonka.WorkMail.ListOrganizations
import Amazonka.WorkMail.ListResourceDelegates
import Amazonka.WorkMail.ListResources
import Amazonka.WorkMail.ListTagsForResource
import Amazonka.WorkMail.ListUsers
import Amazonka.WorkMail.PutAccessControlRule
import Amazonka.WorkMail.PutEmailMonitoringConfiguration
import Amazonka.WorkMail.PutInboundDmarcSettings
import Amazonka.WorkMail.PutMailboxPermissions
import Amazonka.WorkMail.PutMobileDeviceAccessOverride
import Amazonka.WorkMail.PutRetentionPolicy
import Amazonka.WorkMail.RegisterMailDomain
import Amazonka.WorkMail.RegisterToWorkMail
import Amazonka.WorkMail.ResetPassword
import Amazonka.WorkMail.StartMailboxExportJob
import Amazonka.WorkMail.TagResource
import Amazonka.WorkMail.TestAvailabilityConfiguration
import Amazonka.WorkMail.Types
import Amazonka.WorkMail.UntagResource
import Amazonka.WorkMail.UpdateAvailabilityConfiguration
import Amazonka.WorkMail.UpdateDefaultMailDomain
import Amazonka.WorkMail.UpdateImpersonationRole
import Amazonka.WorkMail.UpdateMailboxQuota
import Amazonka.WorkMail.UpdateMobileDeviceAccessRule
import Amazonka.WorkMail.UpdatePrimaryEmailAddress
import Amazonka.WorkMail.UpdateResource
import Amazonka.WorkMail.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'WorkMail'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
