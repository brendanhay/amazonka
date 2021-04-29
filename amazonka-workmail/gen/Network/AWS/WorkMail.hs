{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon WorkMail is a secure, managed business email and calendaring
-- service with support for existing desktop and mobile email clients. You
-- can access your email, contacts, and calendars using Microsoft Outlook,
-- your browser, or other native iOS and Android email applications. You
-- can integrate WorkMail with your existing corporate directory and
-- control both the keys that encrypt your data and the location in which
-- your data is stored.
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
module Network.AWS.WorkMail
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** EntityNotFoundException
    _EntityNotFoundException,

    -- ** OrganizationNotFoundException
    _OrganizationNotFoundException,

    -- ** ReservedNameException
    _ReservedNameException,

    -- ** MailDomainStateException
    _MailDomainStateException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** NameAvailabilityException
    _NameAvailabilityException,

    -- ** InvalidConfigurationException
    _InvalidConfigurationException,

    -- ** OrganizationStateException
    _OrganizationStateException,

    -- ** EntityStateException
    _EntityStateException,

    -- ** DirectoryInUseException
    _DirectoryInUseException,

    -- ** UnsupportedOperationException
    _UnsupportedOperationException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** DirectoryUnavailableException
    _DirectoryUnavailableException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** InvalidPasswordException
    _InvalidPasswordException,

    -- ** EmailAddressInUseException
    _EmailAddressInUseException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** DirectoryServiceAuthenticationFailedException
    _DirectoryServiceAuthenticationFailedException,

    -- ** MailDomainNotFoundException
    _MailDomainNotFoundException,

    -- ** EntityAlreadyRegisteredException
    _EntityAlreadyRegisteredException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateOrganization
    CreateOrganization (CreateOrganization'),
    newCreateOrganization,
    CreateOrganizationResponse (CreateOrganizationResponse'),
    newCreateOrganizationResponse,

    -- ** DeleteAlias
    DeleteAlias (DeleteAlias'),
    newDeleteAlias,
    DeleteAliasResponse (DeleteAliasResponse'),
    newDeleteAliasResponse,

    -- ** StartMailboxExportJob
    StartMailboxExportJob (StartMailboxExportJob'),
    newStartMailboxExportJob,
    StartMailboxExportJobResponse (StartMailboxExportJobResponse'),
    newStartMailboxExportJobResponse,

    -- ** DescribeResource
    DescribeResource (DescribeResource'),
    newDescribeResource,
    DescribeResourceResponse (DescribeResourceResponse'),
    newDescribeResourceResponse,

    -- ** ListResourceDelegates (Paginated)
    ListResourceDelegates (ListResourceDelegates'),
    newListResourceDelegates,
    ListResourceDelegatesResponse (ListResourceDelegatesResponse'),
    newListResourceDelegatesResponse,

    -- ** DeleteAccessControlRule
    DeleteAccessControlRule (DeleteAccessControlRule'),
    newDeleteAccessControlRule,
    DeleteAccessControlRuleResponse (DeleteAccessControlRuleResponse'),
    newDeleteAccessControlRuleResponse,

    -- ** DisassociateDelegateFromResource
    DisassociateDelegateFromResource (DisassociateDelegateFromResource'),
    newDisassociateDelegateFromResource,
    DisassociateDelegateFromResourceResponse (DisassociateDelegateFromResourceResponse'),
    newDisassociateDelegateFromResourceResponse,

    -- ** GetDefaultRetentionPolicy
    GetDefaultRetentionPolicy (GetDefaultRetentionPolicy'),
    newGetDefaultRetentionPolicy,
    GetDefaultRetentionPolicyResponse (GetDefaultRetentionPolicyResponse'),
    newGetDefaultRetentionPolicyResponse,

    -- ** ListGroups (Paginated)
    ListGroups (ListGroups'),
    newListGroups,
    ListGroupsResponse (ListGroupsResponse'),
    newListGroupsResponse,

    -- ** ListMailboxExportJobs
    ListMailboxExportJobs (ListMailboxExportJobs'),
    newListMailboxExportJobs,
    ListMailboxExportJobsResponse (ListMailboxExportJobsResponse'),
    newListMailboxExportJobsResponse,

    -- ** DescribeOrganization
    DescribeOrganization (DescribeOrganization'),
    newDescribeOrganization,
    DescribeOrganizationResponse (DescribeOrganizationResponse'),
    newDescribeOrganizationResponse,

    -- ** CreateResource
    CreateResource (CreateResource'),
    newCreateResource,
    CreateResourceResponse (CreateResourceResponse'),
    newCreateResourceResponse,

    -- ** UpdateResource
    UpdateResource (UpdateResource'),
    newUpdateResource,
    UpdateResourceResponse (UpdateResourceResponse'),
    newUpdateResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DeleteResource
    DeleteResource (DeleteResource'),
    newDeleteResource,
    DeleteResourceResponse (DeleteResourceResponse'),
    newDeleteResourceResponse,

    -- ** CreateGroup
    CreateGroup (CreateGroup'),
    newCreateGroup,
    CreateGroupResponse (CreateGroupResponse'),
    newCreateGroupResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** AssociateMemberToGroup
    AssociateMemberToGroup (AssociateMemberToGroup'),
    newAssociateMemberToGroup,
    AssociateMemberToGroupResponse (AssociateMemberToGroupResponse'),
    newAssociateMemberToGroupResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    CreateUserResponse (CreateUserResponse'),
    newCreateUserResponse,

    -- ** PutRetentionPolicy
    PutRetentionPolicy (PutRetentionPolicy'),
    newPutRetentionPolicy,
    PutRetentionPolicyResponse (PutRetentionPolicyResponse'),
    newPutRetentionPolicyResponse,

    -- ** PutMailboxPermissions
    PutMailboxPermissions (PutMailboxPermissions'),
    newPutMailboxPermissions,
    PutMailboxPermissionsResponse (PutMailboxPermissionsResponse'),
    newPutMailboxPermissionsResponse,

    -- ** AssociateDelegateToResource
    AssociateDelegateToResource (AssociateDelegateToResource'),
    newAssociateDelegateToResource,
    AssociateDelegateToResourceResponse (AssociateDelegateToResourceResponse'),
    newAssociateDelegateToResourceResponse,

    -- ** RegisterToWorkMail
    RegisterToWorkMail (RegisterToWorkMail'),
    newRegisterToWorkMail,
    RegisterToWorkMailResponse (RegisterToWorkMailResponse'),
    newRegisterToWorkMailResponse,

    -- ** DeleteOrganization
    DeleteOrganization (DeleteOrganization'),
    newDeleteOrganization,
    DeleteOrganizationResponse (DeleteOrganizationResponse'),
    newDeleteOrganizationResponse,

    -- ** DescribeMailboxExportJob
    DescribeMailboxExportJob (DescribeMailboxExportJob'),
    newDescribeMailboxExportJob,
    DescribeMailboxExportJobResponse (DescribeMailboxExportJobResponse'),
    newDescribeMailboxExportJobResponse,

    -- ** ListOrganizations (Paginated)
    ListOrganizations (ListOrganizations'),
    newListOrganizations,
    ListOrganizationsResponse (ListOrganizationsResponse'),
    newListOrganizationsResponse,

    -- ** UpdatePrimaryEmailAddress
    UpdatePrimaryEmailAddress (UpdatePrimaryEmailAddress'),
    newUpdatePrimaryEmailAddress,
    UpdatePrimaryEmailAddressResponse (UpdatePrimaryEmailAddressResponse'),
    newUpdatePrimaryEmailAddressResponse,

    -- ** ListAccessControlRules
    ListAccessControlRules (ListAccessControlRules'),
    newListAccessControlRules,
    ListAccessControlRulesResponse (ListAccessControlRulesResponse'),
    newListAccessControlRulesResponse,

    -- ** PutAccessControlRule
    PutAccessControlRule (PutAccessControlRule'),
    newPutAccessControlRule,
    PutAccessControlRuleResponse (PutAccessControlRuleResponse'),
    newPutAccessControlRuleResponse,

    -- ** DescribeUser
    DescribeUser (DescribeUser'),
    newDescribeUser,
    DescribeUserResponse (DescribeUserResponse'),
    newDescribeUserResponse,

    -- ** CancelMailboxExportJob
    CancelMailboxExportJob (CancelMailboxExportJob'),
    newCancelMailboxExportJob,
    CancelMailboxExportJobResponse (CancelMailboxExportJobResponse'),
    newCancelMailboxExportJobResponse,

    -- ** DeregisterFromWorkMail
    DeregisterFromWorkMail (DeregisterFromWorkMail'),
    newDeregisterFromWorkMail,
    DeregisterFromWorkMailResponse (DeregisterFromWorkMailResponse'),
    newDeregisterFromWorkMailResponse,

    -- ** ListGroupMembers (Paginated)
    ListGroupMembers (ListGroupMembers'),
    newListGroupMembers,
    ListGroupMembersResponse (ListGroupMembersResponse'),
    newListGroupMembersResponse,

    -- ** DeleteGroup
    DeleteGroup (DeleteGroup'),
    newDeleteGroup,
    DeleteGroupResponse (DeleteGroupResponse'),
    newDeleteGroupResponse,

    -- ** ListMailboxPermissions (Paginated)
    ListMailboxPermissions (ListMailboxPermissions'),
    newListMailboxPermissions,
    ListMailboxPermissionsResponse (ListMailboxPermissionsResponse'),
    newListMailboxPermissionsResponse,

    -- ** DisassociateMemberFromGroup
    DisassociateMemberFromGroup (DisassociateMemberFromGroup'),
    newDisassociateMemberFromGroup,
    DisassociateMemberFromGroupResponse (DisassociateMemberFromGroupResponse'),
    newDisassociateMemberFromGroupResponse,

    -- ** UpdateMailboxQuota
    UpdateMailboxQuota (UpdateMailboxQuota'),
    newUpdateMailboxQuota,
    UpdateMailboxQuotaResponse (UpdateMailboxQuotaResponse'),
    newUpdateMailboxQuotaResponse,

    -- ** ListResources (Paginated)
    ListResources (ListResources'),
    newListResources,
    ListResourcesResponse (ListResourcesResponse'),
    newListResourcesResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    DeleteUserResponse (DeleteUserResponse'),
    newDeleteUserResponse,

    -- ** ListUsers (Paginated)
    ListUsers (ListUsers'),
    newListUsers,
    ListUsersResponse (ListUsersResponse'),
    newListUsersResponse,

    -- ** GetMailboxDetails
    GetMailboxDetails (GetMailboxDetails'),
    newGetMailboxDetails,
    GetMailboxDetailsResponse (GetMailboxDetailsResponse'),
    newGetMailboxDetailsResponse,

    -- ** DeleteMailboxPermissions
    DeleteMailboxPermissions (DeleteMailboxPermissions'),
    newDeleteMailboxPermissions,
    DeleteMailboxPermissionsResponse (DeleteMailboxPermissionsResponse'),
    newDeleteMailboxPermissionsResponse,

    -- ** DeleteRetentionPolicy
    DeleteRetentionPolicy (DeleteRetentionPolicy'),
    newDeleteRetentionPolicy,
    DeleteRetentionPolicyResponse (DeleteRetentionPolicyResponse'),
    newDeleteRetentionPolicyResponse,

    -- ** DescribeGroup
    DescribeGroup (DescribeGroup'),
    newDescribeGroup,
    DescribeGroupResponse (DescribeGroupResponse'),
    newDescribeGroupResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** CreateAlias
    CreateAlias (CreateAlias'),
    newCreateAlias,
    CreateAliasResponse (CreateAliasResponse'),
    newCreateAliasResponse,

    -- ** GetAccessControlEffect
    GetAccessControlEffect (GetAccessControlEffect'),
    newGetAccessControlEffect,
    GetAccessControlEffectResponse (GetAccessControlEffectResponse'),
    newGetAccessControlEffectResponse,

    -- ** ListAliases (Paginated)
    ListAliases (ListAliases'),
    newListAliases,
    ListAliasesResponse (ListAliasesResponse'),
    newListAliasesResponse,

    -- ** ResetPassword
    ResetPassword (ResetPassword'),
    newResetPassword,
    ResetPasswordResponse (ResetPasswordResponse'),
    newResetPasswordResponse,

    -- * Types

    -- ** AccessControlRuleEffect
    AccessControlRuleEffect (..),

    -- ** EntityState
    EntityState (..),

    -- ** FolderName
    FolderName (..),

    -- ** MailboxExportJobState
    MailboxExportJobState (..),

    -- ** MemberType
    MemberType (..),

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

    -- ** BookingOptions
    BookingOptions (BookingOptions'),
    newBookingOptions,

    -- ** Delegate
    Delegate (Delegate'),
    newDelegate,

    -- ** Domain
    Domain (Domain'),
    newDomain,

    -- ** FolderConfiguration
    FolderConfiguration (FolderConfiguration'),
    newFolderConfiguration,

    -- ** Group
    Group (Group'),
    newGroup,

    -- ** MailboxExportJob
    MailboxExportJob (MailboxExportJob'),
    newMailboxExportJob,

    -- ** Member
    Member (Member'),
    newMember,

    -- ** OrganizationSummary
    OrganizationSummary (OrganizationSummary'),
    newOrganizationSummary,

    -- ** Permission
    Permission (Permission'),
    newPermission,

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

import Network.AWS.WorkMail.AssociateDelegateToResource
import Network.AWS.WorkMail.AssociateMemberToGroup
import Network.AWS.WorkMail.CancelMailboxExportJob
import Network.AWS.WorkMail.CreateAlias
import Network.AWS.WorkMail.CreateGroup
import Network.AWS.WorkMail.CreateOrganization
import Network.AWS.WorkMail.CreateResource
import Network.AWS.WorkMail.CreateUser
import Network.AWS.WorkMail.DeleteAccessControlRule
import Network.AWS.WorkMail.DeleteAlias
import Network.AWS.WorkMail.DeleteGroup
import Network.AWS.WorkMail.DeleteMailboxPermissions
import Network.AWS.WorkMail.DeleteOrganization
import Network.AWS.WorkMail.DeleteResource
import Network.AWS.WorkMail.DeleteRetentionPolicy
import Network.AWS.WorkMail.DeleteUser
import Network.AWS.WorkMail.DeregisterFromWorkMail
import Network.AWS.WorkMail.DescribeGroup
import Network.AWS.WorkMail.DescribeMailboxExportJob
import Network.AWS.WorkMail.DescribeOrganization
import Network.AWS.WorkMail.DescribeResource
import Network.AWS.WorkMail.DescribeUser
import Network.AWS.WorkMail.DisassociateDelegateFromResource
import Network.AWS.WorkMail.DisassociateMemberFromGroup
import Network.AWS.WorkMail.GetAccessControlEffect
import Network.AWS.WorkMail.GetDefaultRetentionPolicy
import Network.AWS.WorkMail.GetMailboxDetails
import Network.AWS.WorkMail.Lens
import Network.AWS.WorkMail.ListAccessControlRules
import Network.AWS.WorkMail.ListAliases
import Network.AWS.WorkMail.ListGroupMembers
import Network.AWS.WorkMail.ListGroups
import Network.AWS.WorkMail.ListMailboxExportJobs
import Network.AWS.WorkMail.ListMailboxPermissions
import Network.AWS.WorkMail.ListOrganizations
import Network.AWS.WorkMail.ListResourceDelegates
import Network.AWS.WorkMail.ListResources
import Network.AWS.WorkMail.ListTagsForResource
import Network.AWS.WorkMail.ListUsers
import Network.AWS.WorkMail.PutAccessControlRule
import Network.AWS.WorkMail.PutMailboxPermissions
import Network.AWS.WorkMail.PutRetentionPolicy
import Network.AWS.WorkMail.RegisterToWorkMail
import Network.AWS.WorkMail.ResetPassword
import Network.AWS.WorkMail.StartMailboxExportJob
import Network.AWS.WorkMail.TagResource
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.UntagResource
import Network.AWS.WorkMail.UpdateMailboxQuota
import Network.AWS.WorkMail.UpdatePrimaryEmailAddress
import Network.AWS.WorkMail.UpdateResource
import Network.AWS.WorkMail.Waiters

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
