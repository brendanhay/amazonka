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

    -- ** DescribeInboundDmarcSettings
    describeInboundDmarcSettings_organizationId,
    describeInboundDmarcSettingsResponse_enforced,
    describeInboundDmarcSettingsResponse_httpStatus,

    -- ** GetMailDomain
    getMailDomain_organizationId,
    getMailDomain_domainName,
    getMailDomainResponse_isTestDomain,
    getMailDomainResponse_records,
    getMailDomainResponse_ownershipVerificationStatus,
    getMailDomainResponse_dkimVerificationStatus,
    getMailDomainResponse_isDefault,
    getMailDomainResponse_httpStatus,

    -- ** UpdatePrimaryEmailAddress
    updatePrimaryEmailAddress_organizationId,
    updatePrimaryEmailAddress_entityId,
    updatePrimaryEmailAddress_email,
    updatePrimaryEmailAddressResponse_httpStatus,

    -- ** DescribeResource
    describeResource_organizationId,
    describeResource_resourceId,
    describeResourceResponse_email,
    describeResourceResponse_state,
    describeResourceResponse_resourceId,
    describeResourceResponse_disabledDate,
    describeResourceResponse_name,
    describeResourceResponse_type,
    describeResourceResponse_enabledDate,
    describeResourceResponse_bookingOptions,
    describeResourceResponse_httpStatus,

    -- ** CreateOrganization
    createOrganization_directoryId,
    createOrganization_enableInteroperability,
    createOrganization_kmsKeyArn,
    createOrganization_clientToken,
    createOrganization_domains,
    createOrganization_alias,
    createOrganizationResponse_organizationId,
    createOrganizationResponse_httpStatus,

    -- ** CreateAlias
    createAlias_organizationId,
    createAlias_entityId,
    createAlias_alias,
    createAliasResponse_httpStatus,

    -- ** DeleteOrganization
    deleteOrganization_clientToken,
    deleteOrganization_organizationId,
    deleteOrganization_deleteDirectory,
    deleteOrganizationResponse_state,
    deleteOrganizationResponse_organizationId,
    deleteOrganizationResponse_httpStatus,

    -- ** ResetPassword
    resetPassword_organizationId,
    resetPassword_userId,
    resetPassword_password,
    resetPasswordResponse_httpStatus,

    -- ** DescribeGroup
    describeGroup_organizationId,
    describeGroup_groupId,
    describeGroupResponse_email,
    describeGroupResponse_state,
    describeGroupResponse_disabledDate,
    describeGroupResponse_name,
    describeGroupResponse_groupId,
    describeGroupResponse_enabledDate,
    describeGroupResponse_httpStatus,

    -- ** DescribeMailboxExportJob
    describeMailboxExportJob_jobId,
    describeMailboxExportJob_organizationId,
    describeMailboxExportJobResponse_state,
    describeMailboxExportJobResponse_kmsKeyArn,
    describeMailboxExportJobResponse_startTime,
    describeMailboxExportJobResponse_estimatedProgress,
    describeMailboxExportJobResponse_endTime,
    describeMailboxExportJobResponse_s3Path,
    describeMailboxExportJobResponse_s3Prefix,
    describeMailboxExportJobResponse_entityId,
    describeMailboxExportJobResponse_description,
    describeMailboxExportJobResponse_errorInfo,
    describeMailboxExportJobResponse_s3BucketName,
    describeMailboxExportJobResponse_roleArn,
    describeMailboxExportJobResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** RegisterToWorkMail
    registerToWorkMail_organizationId,
    registerToWorkMail_entityId,
    registerToWorkMail_email,
    registerToWorkMailResponse_httpStatus,

    -- ** ListAliases
    listAliases_nextToken,
    listAliases_maxResults,
    listAliases_organizationId,
    listAliases_entityId,
    listAliasesResponse_aliases,
    listAliasesResponse_nextToken,
    listAliasesResponse_httpStatus,

    -- ** PutMailboxPermissions
    putMailboxPermissions_organizationId,
    putMailboxPermissions_entityId,
    putMailboxPermissions_granteeId,
    putMailboxPermissions_permissionValues,
    putMailboxPermissionsResponse_httpStatus,

    -- ** GetMobileDeviceAccessEffect
    getMobileDeviceAccessEffect_deviceOperatingSystem,
    getMobileDeviceAccessEffect_deviceModel,
    getMobileDeviceAccessEffect_deviceType,
    getMobileDeviceAccessEffect_deviceUserAgent,
    getMobileDeviceAccessEffect_organizationId,
    getMobileDeviceAccessEffectResponse_effect,
    getMobileDeviceAccessEffectResponse_matchedRules,
    getMobileDeviceAccessEffectResponse_httpStatus,

    -- ** DeleteMailboxPermissions
    deleteMailboxPermissions_organizationId,
    deleteMailboxPermissions_entityId,
    deleteMailboxPermissions_granteeId,
    deleteMailboxPermissionsResponse_httpStatus,

    -- ** ListUsers
    listUsers_nextToken,
    listUsers_maxResults,
    listUsers_organizationId,
    listUsersResponse_users,
    listUsersResponse_nextToken,
    listUsersResponse_httpStatus,

    -- ** PutInboundDmarcSettings
    putInboundDmarcSettings_organizationId,
    putInboundDmarcSettings_enforced,
    putInboundDmarcSettingsResponse_httpStatus,

    -- ** GetMailboxDetails
    getMailboxDetails_organizationId,
    getMailboxDetails_userId,
    getMailboxDetailsResponse_mailboxQuota,
    getMailboxDetailsResponse_mailboxSize,
    getMailboxDetailsResponse_httpStatus,

    -- ** AssociateMemberToGroup
    associateMemberToGroup_organizationId,
    associateMemberToGroup_groupId,
    associateMemberToGroup_memberId,
    associateMemberToGroupResponse_httpStatus,

    -- ** DeleteResource
    deleteResource_organizationId,
    deleteResource_resourceId,
    deleteResourceResponse_httpStatus,

    -- ** UpdateResource
    updateResource_name,
    updateResource_bookingOptions,
    updateResource_organizationId,
    updateResource_resourceId,
    updateResourceResponse_httpStatus,

    -- ** DisassociateMemberFromGroup
    disassociateMemberFromGroup_organizationId,
    disassociateMemberFromGroup_groupId,
    disassociateMemberFromGroup_memberId,
    disassociateMemberFromGroupResponse_httpStatus,

    -- ** ListResources
    listResources_nextToken,
    listResources_maxResults,
    listResources_organizationId,
    listResourcesResponse_resources,
    listResourcesResponse_nextToken,
    listResourcesResponse_httpStatus,

    -- ** DeregisterFromWorkMail
    deregisterFromWorkMail_organizationId,
    deregisterFromWorkMail_entityId,
    deregisterFromWorkMailResponse_httpStatus,

    -- ** ListMailboxExportJobs
    listMailboxExportJobs_nextToken,
    listMailboxExportJobs_maxResults,
    listMailboxExportJobs_organizationId,
    listMailboxExportJobsResponse_nextToken,
    listMailboxExportJobsResponse_jobs,
    listMailboxExportJobsResponse_httpStatus,

    -- ** CreateMobileDeviceAccessRule
    createMobileDeviceAccessRule_clientToken,
    createMobileDeviceAccessRule_deviceUserAgents,
    createMobileDeviceAccessRule_deviceTypes,
    createMobileDeviceAccessRule_notDeviceTypes,
    createMobileDeviceAccessRule_notDeviceOperatingSystems,
    createMobileDeviceAccessRule_deviceModels,
    createMobileDeviceAccessRule_deviceOperatingSystems,
    createMobileDeviceAccessRule_description,
    createMobileDeviceAccessRule_notDeviceUserAgents,
    createMobileDeviceAccessRule_notDeviceModels,
    createMobileDeviceAccessRule_organizationId,
    createMobileDeviceAccessRule_name,
    createMobileDeviceAccessRule_effect,
    createMobileDeviceAccessRuleResponse_mobileDeviceAccessRuleId,
    createMobileDeviceAccessRuleResponse_httpStatus,

    -- ** ListMailboxPermissions
    listMailboxPermissions_nextToken,
    listMailboxPermissions_maxResults,
    listMailboxPermissions_organizationId,
    listMailboxPermissions_entityId,
    listMailboxPermissionsResponse_nextToken,
    listMailboxPermissionsResponse_permissions,
    listMailboxPermissionsResponse_httpStatus,

    -- ** GetMobileDeviceAccessOverride
    getMobileDeviceAccessOverride_organizationId,
    getMobileDeviceAccessOverride_userId,
    getMobileDeviceAccessOverride_deviceId,
    getMobileDeviceAccessOverrideResponse_effect,
    getMobileDeviceAccessOverrideResponse_userId,
    getMobileDeviceAccessOverrideResponse_dateCreated,
    getMobileDeviceAccessOverrideResponse_dateModified,
    getMobileDeviceAccessOverrideResponse_deviceId,
    getMobileDeviceAccessOverrideResponse_description,
    getMobileDeviceAccessOverrideResponse_httpStatus,

    -- ** ListGroupMembers
    listGroupMembers_nextToken,
    listGroupMembers_maxResults,
    listGroupMembers_organizationId,
    listGroupMembers_groupId,
    listGroupMembersResponse_members,
    listGroupMembersResponse_nextToken,
    listGroupMembersResponse_httpStatus,

    -- ** DisassociateDelegateFromResource
    disassociateDelegateFromResource_organizationId,
    disassociateDelegateFromResource_resourceId,
    disassociateDelegateFromResource_entityId,
    disassociateDelegateFromResourceResponse_httpStatus,

    -- ** DeleteAccessControlRule
    deleteAccessControlRule_organizationId,
    deleteAccessControlRule_name,
    deleteAccessControlRuleResponse_httpStatus,

    -- ** ListResourceDelegates
    listResourceDelegates_nextToken,
    listResourceDelegates_maxResults,
    listResourceDelegates_organizationId,
    listResourceDelegates_resourceId,
    listResourceDelegatesResponse_delegates,
    listResourceDelegatesResponse_nextToken,
    listResourceDelegatesResponse_httpStatus,

    -- ** ListAccessControlRules
    listAccessControlRules_organizationId,
    listAccessControlRulesResponse_rules,
    listAccessControlRulesResponse_httpStatus,

    -- ** DescribeUser
    describeUser_organizationId,
    describeUser_userId,
    describeUserResponse_email,
    describeUserResponse_state,
    describeUserResponse_userId,
    describeUserResponse_disabledDate,
    describeUserResponse_name,
    describeUserResponse_displayName,
    describeUserResponse_userRole,
    describeUserResponse_enabledDate,
    describeUserResponse_httpStatus,

    -- ** PutAccessControlRule
    putAccessControlRule_userIds,
    putAccessControlRule_actions,
    putAccessControlRule_notUserIds,
    putAccessControlRule_ipRanges,
    putAccessControlRule_notIpRanges,
    putAccessControlRule_notActions,
    putAccessControlRule_name,
    putAccessControlRule_effect,
    putAccessControlRule_description,
    putAccessControlRule_organizationId,
    putAccessControlRuleResponse_httpStatus,

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

    -- ** DeleteAlias
    deleteAlias_organizationId,
    deleteAlias_entityId,
    deleteAlias_alias,
    deleteAliasResponse_httpStatus,

    -- ** ListOrganizations
    listOrganizations_nextToken,
    listOrganizations_maxResults,
    listOrganizationsResponse_nextToken,
    listOrganizationsResponse_organizationSummaries,
    listOrganizationsResponse_httpStatus,

    -- ** AssociateDelegateToResource
    associateDelegateToResource_organizationId,
    associateDelegateToResource_resourceId,
    associateDelegateToResource_entityId,
    associateDelegateToResourceResponse_httpStatus,

    -- ** GetAccessControlEffect
    getAccessControlEffect_organizationId,
    getAccessControlEffect_ipAddress,
    getAccessControlEffect_action,
    getAccessControlEffect_userId,
    getAccessControlEffectResponse_effect,
    getAccessControlEffectResponse_matchedRules,
    getAccessControlEffectResponse_httpStatus,

    -- ** DeleteRetentionPolicy
    deleteRetentionPolicy_organizationId,
    deleteRetentionPolicy_id,
    deleteRetentionPolicyResponse_httpStatus,

    -- ** CreateUser
    createUser_organizationId,
    createUser_name,
    createUser_displayName,
    createUser_password,
    createUserResponse_userId,
    createUserResponse_httpStatus,

    -- ** PutRetentionPolicy
    putRetentionPolicy_id,
    putRetentionPolicy_description,
    putRetentionPolicy_organizationId,
    putRetentionPolicy_name,
    putRetentionPolicy_folderConfigurations,
    putRetentionPolicyResponse_httpStatus,

    -- ** ListMailDomains
    listMailDomains_nextToken,
    listMailDomains_maxResults,
    listMailDomains_organizationId,
    listMailDomainsResponse_nextToken,
    listMailDomainsResponse_mailDomains,
    listMailDomainsResponse_httpStatus,

    -- ** DeleteUser
    deleteUser_organizationId,
    deleteUser_userId,
    deleteUserResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** RegisterMailDomain
    registerMailDomain_clientToken,
    registerMailDomain_organizationId,
    registerMailDomain_domainName,
    registerMailDomainResponse_httpStatus,

    -- ** UpdateDefaultMailDomain
    updateDefaultMailDomain_organizationId,
    updateDefaultMailDomain_domainName,
    updateDefaultMailDomainResponse_httpStatus,

    -- ** UpdateMobileDeviceAccessRule
    updateMobileDeviceAccessRule_deviceUserAgents,
    updateMobileDeviceAccessRule_deviceTypes,
    updateMobileDeviceAccessRule_notDeviceTypes,
    updateMobileDeviceAccessRule_notDeviceOperatingSystems,
    updateMobileDeviceAccessRule_deviceModels,
    updateMobileDeviceAccessRule_deviceOperatingSystems,
    updateMobileDeviceAccessRule_description,
    updateMobileDeviceAccessRule_notDeviceUserAgents,
    updateMobileDeviceAccessRule_notDeviceModels,
    updateMobileDeviceAccessRule_organizationId,
    updateMobileDeviceAccessRule_mobileDeviceAccessRuleId,
    updateMobileDeviceAccessRule_name,
    updateMobileDeviceAccessRule_effect,
    updateMobileDeviceAccessRuleResponse_httpStatus,

    -- ** DeleteMobileDeviceAccessRule
    deleteMobileDeviceAccessRule_organizationId,
    deleteMobileDeviceAccessRule_mobileDeviceAccessRuleId,
    deleteMobileDeviceAccessRuleResponse_httpStatus,

    -- ** CreateGroup
    createGroup_organizationId,
    createGroup_name,
    createGroupResponse_groupId,
    createGroupResponse_httpStatus,

    -- ** UpdateMailboxQuota
    updateMailboxQuota_organizationId,
    updateMailboxQuota_userId,
    updateMailboxQuota_mailboxQuota,
    updateMailboxQuotaResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** ListMobileDeviceAccessRules
    listMobileDeviceAccessRules_organizationId,
    listMobileDeviceAccessRulesResponse_rules,
    listMobileDeviceAccessRulesResponse_httpStatus,

    -- ** DeleteGroup
    deleteGroup_organizationId,
    deleteGroup_groupId,
    deleteGroupResponse_httpStatus,

    -- ** ListGroups
    listGroups_nextToken,
    listGroups_maxResults,
    listGroups_organizationId,
    listGroupsResponse_groups,
    listGroupsResponse_nextToken,
    listGroupsResponse_httpStatus,

    -- ** DescribeOrganization
    describeOrganization_organizationId,
    describeOrganizationResponse_directoryId,
    describeOrganizationResponse_state,
    describeOrganizationResponse_arn,
    describeOrganizationResponse_alias,
    describeOrganizationResponse_completedDate,
    describeOrganizationResponse_directoryType,
    describeOrganizationResponse_defaultMailDomain,
    describeOrganizationResponse_errorMessage,
    describeOrganizationResponse_organizationId,
    describeOrganizationResponse_httpStatus,

    -- ** CreateResource
    createResource_organizationId,
    createResource_name,
    createResource_type,
    createResourceResponse_resourceId,
    createResourceResponse_httpStatus,

    -- ** GetDefaultRetentionPolicy
    getDefaultRetentionPolicy_organizationId,
    getDefaultRetentionPolicyResponse_name,
    getDefaultRetentionPolicyResponse_id,
    getDefaultRetentionPolicyResponse_folderConfigurations,
    getDefaultRetentionPolicyResponse_description,
    getDefaultRetentionPolicyResponse_httpStatus,

    -- ** DeregisterMailDomain
    deregisterMailDomain_organizationId,
    deregisterMailDomain_domainName,
    deregisterMailDomainResponse_httpStatus,

    -- ** CancelMailboxExportJob
    cancelMailboxExportJob_clientToken,
    cancelMailboxExportJob_jobId,
    cancelMailboxExportJob_organizationId,
    cancelMailboxExportJobResponse_httpStatus,

    -- ** ListMobileDeviceAccessOverrides
    listMobileDeviceAccessOverrides_userId,
    listMobileDeviceAccessOverrides_nextToken,
    listMobileDeviceAccessOverrides_deviceId,
    listMobileDeviceAccessOverrides_maxResults,
    listMobileDeviceAccessOverrides_organizationId,
    listMobileDeviceAccessOverridesResponse_overrides,
    listMobileDeviceAccessOverridesResponse_nextToken,
    listMobileDeviceAccessOverridesResponse_httpStatus,

    -- ** DeleteMobileDeviceAccessOverride
    deleteMobileDeviceAccessOverride_organizationId,
    deleteMobileDeviceAccessOverride_userId,
    deleteMobileDeviceAccessOverride_deviceId,
    deleteMobileDeviceAccessOverrideResponse_httpStatus,

    -- ** PutMobileDeviceAccessOverride
    putMobileDeviceAccessOverride_description,
    putMobileDeviceAccessOverride_organizationId,
    putMobileDeviceAccessOverride_userId,
    putMobileDeviceAccessOverride_deviceId,
    putMobileDeviceAccessOverride_effect,
    putMobileDeviceAccessOverrideResponse_httpStatus,

    -- * Types

    -- ** AccessControlRule
    accessControlRule_effect,
    accessControlRule_userIds,
    accessControlRule_actions,
    accessControlRule_dateCreated,
    accessControlRule_name,
    accessControlRule_notUserIds,
    accessControlRule_dateModified,
    accessControlRule_ipRanges,
    accessControlRule_notIpRanges,
    accessControlRule_notActions,
    accessControlRule_description,

    -- ** BookingOptions
    bookingOptions_autoDeclineConflictingRequests,
    bookingOptions_autoDeclineRecurringRequests,
    bookingOptions_autoAcceptRequests,

    -- ** Delegate
    delegate_id,
    delegate_type,

    -- ** DnsRecord
    dnsRecord_hostname,
    dnsRecord_value,
    dnsRecord_type,

    -- ** Domain
    domain_hostedZoneId,
    domain_domainName,

    -- ** FolderConfiguration
    folderConfiguration_period,
    folderConfiguration_name,
    folderConfiguration_action,

    -- ** Group
    group_email,
    group_state,
    group_disabledDate,
    group_name,
    group_id,
    group_enabledDate,

    -- ** MailDomainSummary
    mailDomainSummary_defaultDomain,
    mailDomainSummary_domainName,

    -- ** MailboxExportJob
    mailboxExportJob_state,
    mailboxExportJob_jobId,
    mailboxExportJob_startTime,
    mailboxExportJob_estimatedProgress,
    mailboxExportJob_endTime,
    mailboxExportJob_s3Path,
    mailboxExportJob_entityId,
    mailboxExportJob_description,
    mailboxExportJob_s3BucketName,

    -- ** Member
    member_state,
    member_disabledDate,
    member_name,
    member_id,
    member_type,
    member_enabledDate,

    -- ** MobileDeviceAccessMatchedRule
    mobileDeviceAccessMatchedRule_mobileDeviceAccessRuleId,
    mobileDeviceAccessMatchedRule_name,

    -- ** MobileDeviceAccessOverride
    mobileDeviceAccessOverride_effect,
    mobileDeviceAccessOverride_userId,
    mobileDeviceAccessOverride_dateCreated,
    mobileDeviceAccessOverride_dateModified,
    mobileDeviceAccessOverride_deviceId,
    mobileDeviceAccessOverride_description,

    -- ** MobileDeviceAccessRule
    mobileDeviceAccessRule_effect,
    mobileDeviceAccessRule_deviceUserAgents,
    mobileDeviceAccessRule_deviceTypes,
    mobileDeviceAccessRule_notDeviceTypes,
    mobileDeviceAccessRule_notDeviceOperatingSystems,
    mobileDeviceAccessRule_dateCreated,
    mobileDeviceAccessRule_deviceModels,
    mobileDeviceAccessRule_mobileDeviceAccessRuleId,
    mobileDeviceAccessRule_name,
    mobileDeviceAccessRule_dateModified,
    mobileDeviceAccessRule_deviceOperatingSystems,
    mobileDeviceAccessRule_description,
    mobileDeviceAccessRule_notDeviceUserAgents,
    mobileDeviceAccessRule_notDeviceModels,

    -- ** OrganizationSummary
    organizationSummary_state,
    organizationSummary_alias,
    organizationSummary_defaultMailDomain,
    organizationSummary_errorMessage,
    organizationSummary_organizationId,

    -- ** Permission
    permission_granteeId,
    permission_granteeType,
    permission_permissionValues,

    -- ** Resource
    resource_email,
    resource_state,
    resource_disabledDate,
    resource_name,
    resource_id,
    resource_type,
    resource_enabledDate,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** User
    user_email,
    user_state,
    user_disabledDate,
    user_name,
    user_id,
    user_displayName,
    user_userRole,
    user_enabledDate,
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
import Network.AWS.WorkMail.DeleteMobileDeviceAccessOverride
import Network.AWS.WorkMail.DeleteMobileDeviceAccessRule
import Network.AWS.WorkMail.DeleteOrganization
import Network.AWS.WorkMail.DeleteResource
import Network.AWS.WorkMail.DeleteRetentionPolicy
import Network.AWS.WorkMail.DeleteUser
import Network.AWS.WorkMail.DeregisterFromWorkMail
import Network.AWS.WorkMail.DeregisterMailDomain
import Network.AWS.WorkMail.DescribeGroup
import Network.AWS.WorkMail.DescribeInboundDmarcSettings
import Network.AWS.WorkMail.DescribeMailboxExportJob
import Network.AWS.WorkMail.DescribeOrganization
import Network.AWS.WorkMail.DescribeResource
import Network.AWS.WorkMail.DescribeUser
import Network.AWS.WorkMail.DisassociateDelegateFromResource
import Network.AWS.WorkMail.DisassociateMemberFromGroup
import Network.AWS.WorkMail.GetAccessControlEffect
import Network.AWS.WorkMail.GetDefaultRetentionPolicy
import Network.AWS.WorkMail.GetMailDomain
import Network.AWS.WorkMail.GetMailboxDetails
import Network.AWS.WorkMail.GetMobileDeviceAccessEffect
import Network.AWS.WorkMail.GetMobileDeviceAccessOverride
import Network.AWS.WorkMail.ListAccessControlRules
import Network.AWS.WorkMail.ListAliases
import Network.AWS.WorkMail.ListGroupMembers
import Network.AWS.WorkMail.ListGroups
import Network.AWS.WorkMail.ListMailDomains
import Network.AWS.WorkMail.ListMailboxExportJobs
import Network.AWS.WorkMail.ListMailboxPermissions
import Network.AWS.WorkMail.ListMobileDeviceAccessOverrides
import Network.AWS.WorkMail.ListMobileDeviceAccessRules
import Network.AWS.WorkMail.ListOrganizations
import Network.AWS.WorkMail.ListResourceDelegates
import Network.AWS.WorkMail.ListResources
import Network.AWS.WorkMail.ListTagsForResource
import Network.AWS.WorkMail.ListUsers
import Network.AWS.WorkMail.PutAccessControlRule
import Network.AWS.WorkMail.PutInboundDmarcSettings
import Network.AWS.WorkMail.PutMailboxPermissions
import Network.AWS.WorkMail.PutMobileDeviceAccessOverride
import Network.AWS.WorkMail.PutRetentionPolicy
import Network.AWS.WorkMail.RegisterMailDomain
import Network.AWS.WorkMail.RegisterToWorkMail
import Network.AWS.WorkMail.ResetPassword
import Network.AWS.WorkMail.StartMailboxExportJob
import Network.AWS.WorkMail.TagResource
import Network.AWS.WorkMail.Types.AccessControlRule
import Network.AWS.WorkMail.Types.BookingOptions
import Network.AWS.WorkMail.Types.Delegate
import Network.AWS.WorkMail.Types.DnsRecord
import Network.AWS.WorkMail.Types.Domain
import Network.AWS.WorkMail.Types.FolderConfiguration
import Network.AWS.WorkMail.Types.Group
import Network.AWS.WorkMail.Types.MailDomainSummary
import Network.AWS.WorkMail.Types.MailboxExportJob
import Network.AWS.WorkMail.Types.Member
import Network.AWS.WorkMail.Types.MobileDeviceAccessMatchedRule
import Network.AWS.WorkMail.Types.MobileDeviceAccessOverride
import Network.AWS.WorkMail.Types.MobileDeviceAccessRule
import Network.AWS.WorkMail.Types.OrganizationSummary
import Network.AWS.WorkMail.Types.Permission
import Network.AWS.WorkMail.Types.Resource
import Network.AWS.WorkMail.Types.Tag
import Network.AWS.WorkMail.Types.User
import Network.AWS.WorkMail.UntagResource
import Network.AWS.WorkMail.UpdateDefaultMailDomain
import Network.AWS.WorkMail.UpdateMailboxQuota
import Network.AWS.WorkMail.UpdateMobileDeviceAccessRule
import Network.AWS.WorkMail.UpdatePrimaryEmailAddress
import Network.AWS.WorkMail.UpdateResource
