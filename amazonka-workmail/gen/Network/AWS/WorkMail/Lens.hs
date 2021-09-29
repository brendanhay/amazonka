{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Lens
  ( -- * Operations

    -- ** DescribeResource
    describeResource_organizationId,
    describeResource_resourceId,
    describeResourceResponse_resourceId,
    describeResourceResponse_enabledDate,
    describeResourceResponse_name,
    describeResourceResponse_state,
    describeResourceResponse_disabledDate,
    describeResourceResponse_email,
    describeResourceResponse_bookingOptions,
    describeResourceResponse_type,
    describeResourceResponse_httpStatus,

    -- ** DeleteAlias
    deleteAlias_organizationId,
    deleteAlias_entityId,
    deleteAlias_alias,
    deleteAliasResponse_httpStatus,

    -- ** CreateOrganization
    createOrganization_enableInteroperability,
    createOrganization_domains,
    createOrganization_kmsKeyArn,
    createOrganization_directoryId,
    createOrganization_clientToken,
    createOrganization_alias,
    createOrganizationResponse_organizationId,
    createOrganizationResponse_httpStatus,

    -- ** StartMailboxExportJob
    startMailboxExportJob_description,
    startMailboxExportJob_clientToken,
    startMailboxExportJob_organizationId,
    startMailboxExportJob_entityId,
    startMailboxExportJob_roleArn,
    startMailboxExportJob_kmsKeyArn,
    startMailboxExportJob_s3BucketName,
    startMailboxExportJob_s3Prefix,
    startMailboxExportJobResponse_jobId,
    startMailboxExportJobResponse_httpStatus,

    -- ** DeleteAccessControlRule
    deleteAccessControlRule_organizationId,
    deleteAccessControlRule_name,
    deleteAccessControlRuleResponse_httpStatus,

    -- ** ListResourceDelegates
    listResourceDelegates_nextToken,
    listResourceDelegates_maxResults,
    listResourceDelegates_organizationId,
    listResourceDelegates_resourceId,
    listResourceDelegatesResponse_nextToken,
    listResourceDelegatesResponse_delegates,
    listResourceDelegatesResponse_httpStatus,

    -- ** DisassociateDelegateFromResource
    disassociateDelegateFromResource_organizationId,
    disassociateDelegateFromResource_resourceId,
    disassociateDelegateFromResource_entityId,
    disassociateDelegateFromResourceResponse_httpStatus,

    -- ** GetDefaultRetentionPolicy
    getDefaultRetentionPolicy_organizationId,
    getDefaultRetentionPolicyResponse_id,
    getDefaultRetentionPolicyResponse_folderConfigurations,
    getDefaultRetentionPolicyResponse_name,
    getDefaultRetentionPolicyResponse_description,
    getDefaultRetentionPolicyResponse_httpStatus,

    -- ** ListGroups
    listGroups_nextToken,
    listGroups_maxResults,
    listGroups_organizationId,
    listGroupsResponse_groups,
    listGroupsResponse_nextToken,
    listGroupsResponse_httpStatus,

    -- ** CreateResource
    createResource_organizationId,
    createResource_name,
    createResource_type,
    createResourceResponse_resourceId,
    createResourceResponse_httpStatus,

    -- ** ListMailboxExportJobs
    listMailboxExportJobs_nextToken,
    listMailboxExportJobs_maxResults,
    listMailboxExportJobs_organizationId,
    listMailboxExportJobsResponse_nextToken,
    listMailboxExportJobsResponse_jobs,
    listMailboxExportJobsResponse_httpStatus,

    -- ** DescribeOrganization
    describeOrganization_organizationId,
    describeOrganizationResponse_alias,
    describeOrganizationResponse_organizationId,
    describeOrganizationResponse_arn,
    describeOrganizationResponse_defaultMailDomain,
    describeOrganizationResponse_state,
    describeOrganizationResponse_directoryId,
    describeOrganizationResponse_directoryType,
    describeOrganizationResponse_completedDate,
    describeOrganizationResponse_errorMessage,
    describeOrganizationResponse_httpStatus,

    -- ** UpdateResource
    updateResource_name,
    updateResource_bookingOptions,
    updateResource_organizationId,
    updateResource_resourceId,
    updateResourceResponse_httpStatus,

    -- ** DeleteResource
    deleteResource_organizationId,
    deleteResource_resourceId,
    deleteResourceResponse_httpStatus,

    -- ** ListMobileDeviceAccessRules
    listMobileDeviceAccessRules_organizationId,
    listMobileDeviceAccessRulesResponse_rules,
    listMobileDeviceAccessRulesResponse_httpStatus,

    -- ** CreateGroup
    createGroup_organizationId,
    createGroup_name,
    createGroupResponse_groupId,
    createGroupResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** AssociateMemberToGroup
    associateMemberToGroup_organizationId,
    associateMemberToGroup_groupId,
    associateMemberToGroup_memberId,
    associateMemberToGroupResponse_httpStatus,

    -- ** PutRetentionPolicy
    putRetentionPolicy_id,
    putRetentionPolicy_description,
    putRetentionPolicy_organizationId,
    putRetentionPolicy_name,
    putRetentionPolicy_folderConfigurations,
    putRetentionPolicyResponse_httpStatus,

    -- ** CreateUser
    createUser_organizationId,
    createUser_name,
    createUser_displayName,
    createUser_password,
    createUserResponse_userId,
    createUserResponse_httpStatus,

    -- ** PutMailboxPermissions
    putMailboxPermissions_organizationId,
    putMailboxPermissions_entityId,
    putMailboxPermissions_granteeId,
    putMailboxPermissions_permissionValues,
    putMailboxPermissionsResponse_httpStatus,

    -- ** GetMobileDeviceAccessEffect
    getMobileDeviceAccessEffect_deviceModel,
    getMobileDeviceAccessEffect_deviceUserAgent,
    getMobileDeviceAccessEffect_deviceOperatingSystem,
    getMobileDeviceAccessEffect_deviceType,
    getMobileDeviceAccessEffect_organizationId,
    getMobileDeviceAccessEffectResponse_matchedRules,
    getMobileDeviceAccessEffectResponse_effect,
    getMobileDeviceAccessEffectResponse_httpStatus,

    -- ** RegisterToWorkMail
    registerToWorkMail_organizationId,
    registerToWorkMail_entityId,
    registerToWorkMail_email,
    registerToWorkMailResponse_httpStatus,

    -- ** DeleteOrganization
    deleteOrganization_clientToken,
    deleteOrganization_organizationId,
    deleteOrganization_deleteDirectory,
    deleteOrganizationResponse_organizationId,
    deleteOrganizationResponse_state,
    deleteOrganizationResponse_httpStatus,

    -- ** DescribeMailboxExportJob
    describeMailboxExportJob_jobId,
    describeMailboxExportJob_organizationId,
    describeMailboxExportJobResponse_estimatedProgress,
    describeMailboxExportJobResponse_roleArn,
    describeMailboxExportJobResponse_entityId,
    describeMailboxExportJobResponse_s3Path,
    describeMailboxExportJobResponse_startTime,
    describeMailboxExportJobResponse_endTime,
    describeMailboxExportJobResponse_kmsKeyArn,
    describeMailboxExportJobResponse_state,
    describeMailboxExportJobResponse_s3BucketName,
    describeMailboxExportJobResponse_description,
    describeMailboxExportJobResponse_errorInfo,
    describeMailboxExportJobResponse_s3Prefix,
    describeMailboxExportJobResponse_httpStatus,

    -- ** AssociateDelegateToResource
    associateDelegateToResource_organizationId,
    associateDelegateToResource_resourceId,
    associateDelegateToResource_entityId,
    associateDelegateToResourceResponse_httpStatus,

    -- ** ListOrganizations
    listOrganizations_nextToken,
    listOrganizations_maxResults,
    listOrganizationsResponse_nextToken,
    listOrganizationsResponse_organizationSummaries,
    listOrganizationsResponse_httpStatus,

    -- ** UpdatePrimaryEmailAddress
    updatePrimaryEmailAddress_organizationId,
    updatePrimaryEmailAddress_entityId,
    updatePrimaryEmailAddress_email,
    updatePrimaryEmailAddressResponse_httpStatus,

    -- ** ListAccessControlRules
    listAccessControlRules_organizationId,
    listAccessControlRulesResponse_rules,
    listAccessControlRulesResponse_httpStatus,

    -- ** PutAccessControlRule
    putAccessControlRule_notIpRanges,
    putAccessControlRule_actions,
    putAccessControlRule_ipRanges,
    putAccessControlRule_userIds,
    putAccessControlRule_notActions,
    putAccessControlRule_notUserIds,
    putAccessControlRule_name,
    putAccessControlRule_effect,
    putAccessControlRule_description,
    putAccessControlRule_organizationId,
    putAccessControlRuleResponse_httpStatus,

    -- ** DescribeUser
    describeUser_organizationId,
    describeUser_userId,
    describeUserResponse_enabledDate,
    describeUserResponse_userRole,
    describeUserResponse_name,
    describeUserResponse_state,
    describeUserResponse_disabledDate,
    describeUserResponse_email,
    describeUserResponse_userId,
    describeUserResponse_displayName,
    describeUserResponse_httpStatus,

    -- ** CancelMailboxExportJob
    cancelMailboxExportJob_clientToken,
    cancelMailboxExportJob_jobId,
    cancelMailboxExportJob_organizationId,
    cancelMailboxExportJobResponse_httpStatus,

    -- ** CreateMobileDeviceAccessRule
    createMobileDeviceAccessRule_notDeviceModels,
    createMobileDeviceAccessRule_notDeviceOperatingSystems,
    createMobileDeviceAccessRule_deviceUserAgents,
    createMobileDeviceAccessRule_deviceModels,
    createMobileDeviceAccessRule_notDeviceTypes,
    createMobileDeviceAccessRule_description,
    createMobileDeviceAccessRule_notDeviceUserAgents,
    createMobileDeviceAccessRule_deviceTypes,
    createMobileDeviceAccessRule_deviceOperatingSystems,
    createMobileDeviceAccessRule_clientToken,
    createMobileDeviceAccessRule_organizationId,
    createMobileDeviceAccessRule_name,
    createMobileDeviceAccessRule_effect,
    createMobileDeviceAccessRuleResponse_mobileDeviceAccessRuleId,
    createMobileDeviceAccessRuleResponse_httpStatus,

    -- ** DeleteGroup
    deleteGroup_organizationId,
    deleteGroup_groupId,
    deleteGroupResponse_httpStatus,

    -- ** ListGroupMembers
    listGroupMembers_nextToken,
    listGroupMembers_maxResults,
    listGroupMembers_organizationId,
    listGroupMembers_groupId,
    listGroupMembersResponse_nextToken,
    listGroupMembersResponse_members,
    listGroupMembersResponse_httpStatus,

    -- ** ListMailboxPermissions
    listMailboxPermissions_nextToken,
    listMailboxPermissions_maxResults,
    listMailboxPermissions_organizationId,
    listMailboxPermissions_entityId,
    listMailboxPermissionsResponse_nextToken,
    listMailboxPermissionsResponse_permissions,
    listMailboxPermissionsResponse_httpStatus,

    -- ** DeregisterFromWorkMail
    deregisterFromWorkMail_organizationId,
    deregisterFromWorkMail_entityId,
    deregisterFromWorkMailResponse_httpStatus,

    -- ** UpdateMailboxQuota
    updateMailboxQuota_organizationId,
    updateMailboxQuota_userId,
    updateMailboxQuota_mailboxQuota,
    updateMailboxQuotaResponse_httpStatus,

    -- ** UpdateMobileDeviceAccessRule
    updateMobileDeviceAccessRule_notDeviceModels,
    updateMobileDeviceAccessRule_notDeviceOperatingSystems,
    updateMobileDeviceAccessRule_deviceUserAgents,
    updateMobileDeviceAccessRule_deviceModels,
    updateMobileDeviceAccessRule_notDeviceTypes,
    updateMobileDeviceAccessRule_description,
    updateMobileDeviceAccessRule_notDeviceUserAgents,
    updateMobileDeviceAccessRule_deviceTypes,
    updateMobileDeviceAccessRule_deviceOperatingSystems,
    updateMobileDeviceAccessRule_organizationId,
    updateMobileDeviceAccessRule_mobileDeviceAccessRuleId,
    updateMobileDeviceAccessRule_name,
    updateMobileDeviceAccessRule_effect,
    updateMobileDeviceAccessRuleResponse_httpStatus,

    -- ** DeleteMobileDeviceAccessRule
    deleteMobileDeviceAccessRule_organizationId,
    deleteMobileDeviceAccessRule_mobileDeviceAccessRuleId,
    deleteMobileDeviceAccessRuleResponse_httpStatus,

    -- ** DisassociateMemberFromGroup
    disassociateMemberFromGroup_organizationId,
    disassociateMemberFromGroup_groupId,
    disassociateMemberFromGroup_memberId,
    disassociateMemberFromGroupResponse_httpStatus,

    -- ** ListResources
    listResources_nextToken,
    listResources_maxResults,
    listResources_organizationId,
    listResourcesResponse_nextToken,
    listResourcesResponse_resources,
    listResourcesResponse_httpStatus,

    -- ** GetMailboxDetails
    getMailboxDetails_organizationId,
    getMailboxDetails_userId,
    getMailboxDetailsResponse_mailboxQuota,
    getMailboxDetailsResponse_mailboxSize,
    getMailboxDetailsResponse_httpStatus,

    -- ** ListUsers
    listUsers_nextToken,
    listUsers_maxResults,
    listUsers_organizationId,
    listUsersResponse_nextToken,
    listUsersResponse_users,
    listUsersResponse_httpStatus,

    -- ** DeleteUser
    deleteUser_organizationId,
    deleteUser_userId,
    deleteUserResponse_httpStatus,

    -- ** DeleteRetentionPolicy
    deleteRetentionPolicy_organizationId,
    deleteRetentionPolicy_id,
    deleteRetentionPolicyResponse_httpStatus,

    -- ** DeleteMailboxPermissions
    deleteMailboxPermissions_organizationId,
    deleteMailboxPermissions_entityId,
    deleteMailboxPermissions_granteeId,
    deleteMailboxPermissionsResponse_httpStatus,

    -- ** ResetPassword
    resetPassword_organizationId,
    resetPassword_userId,
    resetPassword_password,
    resetPasswordResponse_httpStatus,

    -- ** ListAliases
    listAliases_nextToken,
    listAliases_maxResults,
    listAliases_organizationId,
    listAliases_entityId,
    listAliasesResponse_nextToken,
    listAliasesResponse_aliases,
    listAliasesResponse_httpStatus,

    -- ** DescribeGroup
    describeGroup_organizationId,
    describeGroup_groupId,
    describeGroupResponse_enabledDate,
    describeGroupResponse_groupId,
    describeGroupResponse_name,
    describeGroupResponse_state,
    describeGroupResponse_disabledDate,
    describeGroupResponse_email,
    describeGroupResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** GetAccessControlEffect
    getAccessControlEffect_organizationId,
    getAccessControlEffect_ipAddress,
    getAccessControlEffect_action,
    getAccessControlEffect_userId,
    getAccessControlEffectResponse_matchedRules,
    getAccessControlEffectResponse_effect,
    getAccessControlEffectResponse_httpStatus,

    -- ** CreateAlias
    createAlias_organizationId,
    createAlias_entityId,
    createAlias_alias,
    createAliasResponse_httpStatus,

    -- * Types

    -- ** AccessControlRule
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

    -- ** BookingOptions
    bookingOptions_autoDeclineConflictingRequests,
    bookingOptions_autoDeclineRecurringRequests,
    bookingOptions_autoAcceptRequests,

    -- ** Delegate
    delegate_id,
    delegate_type,

    -- ** Domain
    domain_hostedZoneId,
    domain_domainName,

    -- ** FolderConfiguration
    folderConfiguration_period,
    folderConfiguration_name,
    folderConfiguration_action,

    -- ** Group
    group_enabledDate,
    group_id,
    group_name,
    group_state,
    group_disabledDate,
    group_email,

    -- ** MailboxExportJob
    mailboxExportJob_estimatedProgress,
    mailboxExportJob_entityId,
    mailboxExportJob_s3Path,
    mailboxExportJob_startTime,
    mailboxExportJob_endTime,
    mailboxExportJob_state,
    mailboxExportJob_s3BucketName,
    mailboxExportJob_description,
    mailboxExportJob_jobId,

    -- ** Member
    member_enabledDate,
    member_id,
    member_name,
    member_state,
    member_disabledDate,
    member_type,

    -- ** MobileDeviceAccessMatchedRule
    mobileDeviceAccessMatchedRule_name,
    mobileDeviceAccessMatchedRule_mobileDeviceAccessRuleId,

    -- ** MobileDeviceAccessRule
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

    -- ** OrganizationSummary
    organizationSummary_alias,
    organizationSummary_organizationId,
    organizationSummary_defaultMailDomain,
    organizationSummary_state,
    organizationSummary_errorMessage,

    -- ** Permission
    permission_granteeId,
    permission_granteeType,
    permission_permissionValues,

    -- ** Resource
    resource_enabledDate,
    resource_id,
    resource_name,
    resource_state,
    resource_disabledDate,
    resource_email,
    resource_type,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** User
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

import Network.AWS.WorkMail.AssociateDelegateToResource
import Network.AWS.WorkMail.AssociateMemberToGroup
import Network.AWS.WorkMail.CancelMailboxExportJob
import Network.AWS.WorkMail.CreateAlias
import Network.AWS.WorkMail.CreateGroup
import Network.AWS.WorkMail.CreateMobileDeviceAccessRule
import Network.AWS.WorkMail.CreateOrganization
import Network.AWS.WorkMail.CreateResource
import Network.AWS.WorkMail.CreateUser
import Network.AWS.WorkMail.DeleteAccessControlRule
import Network.AWS.WorkMail.DeleteAlias
import Network.AWS.WorkMail.DeleteGroup
import Network.AWS.WorkMail.DeleteMailboxPermissions
import Network.AWS.WorkMail.DeleteMobileDeviceAccessRule
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
import Network.AWS.WorkMail.GetMobileDeviceAccessEffect
import Network.AWS.WorkMail.ListAccessControlRules
import Network.AWS.WorkMail.ListAliases
import Network.AWS.WorkMail.ListGroupMembers
import Network.AWS.WorkMail.ListGroups
import Network.AWS.WorkMail.ListMailboxExportJobs
import Network.AWS.WorkMail.ListMailboxPermissions
import Network.AWS.WorkMail.ListMobileDeviceAccessRules
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
import Network.AWS.WorkMail.Types.AccessControlRule
import Network.AWS.WorkMail.Types.BookingOptions
import Network.AWS.WorkMail.Types.Delegate
import Network.AWS.WorkMail.Types.Domain
import Network.AWS.WorkMail.Types.FolderConfiguration
import Network.AWS.WorkMail.Types.Group
import Network.AWS.WorkMail.Types.MailboxExportJob
import Network.AWS.WorkMail.Types.Member
import Network.AWS.WorkMail.Types.MobileDeviceAccessMatchedRule
import Network.AWS.WorkMail.Types.MobileDeviceAccessRule
import Network.AWS.WorkMail.Types.OrganizationSummary
import Network.AWS.WorkMail.Types.Permission
import Network.AWS.WorkMail.Types.Resource
import Network.AWS.WorkMail.Types.Tag
import Network.AWS.WorkMail.Types.User
import Network.AWS.WorkMail.UntagResource
import Network.AWS.WorkMail.UpdateMailboxQuota
import Network.AWS.WorkMail.UpdateMobileDeviceAccessRule
import Network.AWS.WorkMail.UpdatePrimaryEmailAddress
import Network.AWS.WorkMail.UpdateResource
