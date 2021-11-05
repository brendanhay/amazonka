{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WorkMail.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Lens
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

import Amazonka.WorkMail.AssociateDelegateToResource
import Amazonka.WorkMail.AssociateMemberToGroup
import Amazonka.WorkMail.CancelMailboxExportJob
import Amazonka.WorkMail.CreateAlias
import Amazonka.WorkMail.CreateGroup
import Amazonka.WorkMail.CreateMobileDeviceAccessRule
import Amazonka.WorkMail.CreateOrganization
import Amazonka.WorkMail.CreateResource
import Amazonka.WorkMail.CreateUser
import Amazonka.WorkMail.DeleteAccessControlRule
import Amazonka.WorkMail.DeleteAlias
import Amazonka.WorkMail.DeleteGroup
import Amazonka.WorkMail.DeleteMailboxPermissions
import Amazonka.WorkMail.DeleteMobileDeviceAccessOverride
import Amazonka.WorkMail.DeleteMobileDeviceAccessRule
import Amazonka.WorkMail.DeleteOrganization
import Amazonka.WorkMail.DeleteResource
import Amazonka.WorkMail.DeleteRetentionPolicy
import Amazonka.WorkMail.DeleteUser
import Amazonka.WorkMail.DeregisterFromWorkMail
import Amazonka.WorkMail.DeregisterMailDomain
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
import Amazonka.WorkMail.GetMailDomain
import Amazonka.WorkMail.GetMailboxDetails
import Amazonka.WorkMail.GetMobileDeviceAccessEffect
import Amazonka.WorkMail.GetMobileDeviceAccessOverride
import Amazonka.WorkMail.ListAccessControlRules
import Amazonka.WorkMail.ListAliases
import Amazonka.WorkMail.ListGroupMembers
import Amazonka.WorkMail.ListGroups
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
import Amazonka.WorkMail.PutInboundDmarcSettings
import Amazonka.WorkMail.PutMailboxPermissions
import Amazonka.WorkMail.PutMobileDeviceAccessOverride
import Amazonka.WorkMail.PutRetentionPolicy
import Amazonka.WorkMail.RegisterMailDomain
import Amazonka.WorkMail.RegisterToWorkMail
import Amazonka.WorkMail.ResetPassword
import Amazonka.WorkMail.StartMailboxExportJob
import Amazonka.WorkMail.TagResource
import Amazonka.WorkMail.Types.AccessControlRule
import Amazonka.WorkMail.Types.BookingOptions
import Amazonka.WorkMail.Types.Delegate
import Amazonka.WorkMail.Types.DnsRecord
import Amazonka.WorkMail.Types.Domain
import Amazonka.WorkMail.Types.FolderConfiguration
import Amazonka.WorkMail.Types.Group
import Amazonka.WorkMail.Types.MailDomainSummary
import Amazonka.WorkMail.Types.MailboxExportJob
import Amazonka.WorkMail.Types.Member
import Amazonka.WorkMail.Types.MobileDeviceAccessMatchedRule
import Amazonka.WorkMail.Types.MobileDeviceAccessOverride
import Amazonka.WorkMail.Types.MobileDeviceAccessRule
import Amazonka.WorkMail.Types.OrganizationSummary
import Amazonka.WorkMail.Types.Permission
import Amazonka.WorkMail.Types.Resource
import Amazonka.WorkMail.Types.Tag
import Amazonka.WorkMail.Types.User
import Amazonka.WorkMail.UntagResource
import Amazonka.WorkMail.UpdateDefaultMailDomain
import Amazonka.WorkMail.UpdateMailboxQuota
import Amazonka.WorkMail.UpdateMobileDeviceAccessRule
import Amazonka.WorkMail.UpdatePrimaryEmailAddress
import Amazonka.WorkMail.UpdateResource
