{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon WorkMail is a secure, managed business email and calendaring service with support for existing desktop and mobile email clients. You can access your email, contacts, and calendars using Microsoft Outlook, your browser, or other native iOS and Android email applications. You can integrate WorkMail with your existing corporate directory and control both the keys that encrypt your data and the location in which your data is stored.
--
--
-- The WorkMail API is designed for the following scenarios:
--
--     * Listing and describing organizations
--
--
--
--     * Managing users
--
--
--
--     * Managing groups
--
--
--
--     * Managing resources
--
--
--
-- All WorkMail API operations are Amazon-authenticated and certificate-signed. They not only require the use of the AWS SDK, but also allow for the exclusive use of AWS Identity and Access Management users and roles to help facilitate access, trust, and permission policies. By creating a role and allowing an IAM user to access the WorkMail site, the IAM user gains full administrative visibility into the entire WorkMail organization (or as set in the IAM policy). This includes, but is not limited to, the ability to create, update, and delete users, groups, and resources. This allows developers to perform the scenarios listed above, as well as give users the ability to grant access on a selective basis using the IAM model.
module Network.AWS.WorkMail
  ( -- * Service Configuration
    workMail,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** UpdatePrimaryEmailAddress
    module Network.AWS.WorkMail.UpdatePrimaryEmailAddress,

    -- ** DescribeResource
    module Network.AWS.WorkMail.DescribeResource,

    -- ** CreateOrganization
    module Network.AWS.WorkMail.CreateOrganization,

    -- ** CreateAlias
    module Network.AWS.WorkMail.CreateAlias,

    -- ** DeleteOrganization
    module Network.AWS.WorkMail.DeleteOrganization,

    -- ** ResetPassword
    module Network.AWS.WorkMail.ResetPassword,

    -- ** DescribeGroup
    module Network.AWS.WorkMail.DescribeGroup,

    -- ** DescribeMailboxExportJob
    module Network.AWS.WorkMail.DescribeMailboxExportJob,

    -- ** ListTagsForResource
    module Network.AWS.WorkMail.ListTagsForResource,

    -- ** RegisterToWorkMail
    module Network.AWS.WorkMail.RegisterToWorkMail,

    -- ** ListAliases (Paginated)
    module Network.AWS.WorkMail.ListAliases,

    -- ** PutMailboxPermissions
    module Network.AWS.WorkMail.PutMailboxPermissions,

    -- ** DeleteMailboxPermissions
    module Network.AWS.WorkMail.DeleteMailboxPermissions,

    -- ** ListUsers (Paginated)
    module Network.AWS.WorkMail.ListUsers,

    -- ** GetMailboxDetails
    module Network.AWS.WorkMail.GetMailboxDetails,

    -- ** AssociateMemberToGroup
    module Network.AWS.WorkMail.AssociateMemberToGroup,

    -- ** DeleteResource
    module Network.AWS.WorkMail.DeleteResource,

    -- ** UpdateResource
    module Network.AWS.WorkMail.UpdateResource,

    -- ** DisassociateMemberFromGroup
    module Network.AWS.WorkMail.DisassociateMemberFromGroup,

    -- ** ListResources (Paginated)
    module Network.AWS.WorkMail.ListResources,

    -- ** DeregisterFromWorkMail
    module Network.AWS.WorkMail.DeregisterFromWorkMail,

    -- ** ListMailboxExportJobs
    module Network.AWS.WorkMail.ListMailboxExportJobs,

    -- ** ListMailboxPermissions (Paginated)
    module Network.AWS.WorkMail.ListMailboxPermissions,

    -- ** ListGroupMembers (Paginated)
    module Network.AWS.WorkMail.ListGroupMembers,

    -- ** DisassociateDelegateFromResource
    module Network.AWS.WorkMail.DisassociateDelegateFromResource,

    -- ** DeleteAccessControlRule
    module Network.AWS.WorkMail.DeleteAccessControlRule,

    -- ** ListResourceDelegates (Paginated)
    module Network.AWS.WorkMail.ListResourceDelegates,

    -- ** ListAccessControlRules
    module Network.AWS.WorkMail.ListAccessControlRules,

    -- ** DescribeUser
    module Network.AWS.WorkMail.DescribeUser,

    -- ** PutAccessControlRule
    module Network.AWS.WorkMail.PutAccessControlRule,

    -- ** StartMailboxExportJob
    module Network.AWS.WorkMail.StartMailboxExportJob,

    -- ** DeleteAlias
    module Network.AWS.WorkMail.DeleteAlias,

    -- ** ListOrganizations (Paginated)
    module Network.AWS.WorkMail.ListOrganizations,

    -- ** AssociateDelegateToResource
    module Network.AWS.WorkMail.AssociateDelegateToResource,

    -- ** GetAccessControlEffect
    module Network.AWS.WorkMail.GetAccessControlEffect,

    -- ** DeleteRetentionPolicy
    module Network.AWS.WorkMail.DeleteRetentionPolicy,

    -- ** CreateUser
    module Network.AWS.WorkMail.CreateUser,

    -- ** PutRetentionPolicy
    module Network.AWS.WorkMail.PutRetentionPolicy,

    -- ** DeleteUser
    module Network.AWS.WorkMail.DeleteUser,

    -- ** TagResource
    module Network.AWS.WorkMail.TagResource,

    -- ** CreateGroup
    module Network.AWS.WorkMail.CreateGroup,

    -- ** UpdateMailboxQuota
    module Network.AWS.WorkMail.UpdateMailboxQuota,

    -- ** UntagResource
    module Network.AWS.WorkMail.UntagResource,

    -- ** DeleteGroup
    module Network.AWS.WorkMail.DeleteGroup,

    -- ** ListGroups (Paginated)
    module Network.AWS.WorkMail.ListGroups,

    -- ** DescribeOrganization
    module Network.AWS.WorkMail.DescribeOrganization,

    -- ** CreateResource
    module Network.AWS.WorkMail.CreateResource,

    -- ** GetDefaultRetentionPolicy
    module Network.AWS.WorkMail.GetDefaultRetentionPolicy,

    -- ** CancelMailboxExportJob
    module Network.AWS.WorkMail.CancelMailboxExportJob,

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
    AccessControlRule,
    accessControlRule,
    acrEffect,
    acrUserIds,
    acrActions,
    acrDateCreated,
    acrName,
    acrNotUserIds,
    acrDateModified,
    acrIPRanges,
    acrNotIPRanges,
    acrNotActions,
    acrDescription,

    -- ** BookingOptions
    BookingOptions,
    bookingOptions,
    boAutoDeclineConflictingRequests,
    boAutoDeclineRecurringRequests,
    boAutoAcceptRequests,

    -- ** Delegate
    Delegate,
    delegate,
    dId,
    dType,

    -- ** Domain
    Domain,
    domain,
    dHostedZoneId,
    dDomainName,

    -- ** FolderConfiguration
    FolderConfiguration,
    folderConfiguration,
    fcPeriod,
    fcName,
    fcAction,

    -- ** Group
    Group,
    group',
    gEmail,
    gState,
    gDisabledDate,
    gName,
    gId,
    gEnabledDate,

    -- ** MailboxExportJob
    MailboxExportJob,
    mailboxExportJob,
    mejState,
    mejJobId,
    mejStartTime,
    mejEstimatedProgress,
    mejEndTime,
    mejS3Path,
    mejEntityId,
    mejDescription,
    mejS3BucketName,

    -- ** Member
    Member,
    member,
    mState,
    mDisabledDate,
    mName,
    mId,
    mType,
    mEnabledDate,

    -- ** OrganizationSummary
    OrganizationSummary,
    organizationSummary,
    osState,
    osAlias,
    osDefaultMailDomain,
    osErrorMessage,
    osOrganizationId,

    -- ** Permission
    Permission,
    permission,
    pGranteeId,
    pGranteeType,
    pPermissionValues,

    -- ** Resource
    Resource,
    resource,
    rEmail,
    rState,
    rDisabledDate,
    rName,
    rId,
    rType,
    rEnabledDate,

    -- ** Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- ** User
    User,
    user,
    uEmail,
    uState,
    uDisabledDate,
    uName,
    uId,
    uDisplayName,
    uUserRole,
    uEnabledDate,
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
