{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WorkMail.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Lens
  ( -- * Operations

    -- ** AssociateDelegateToResource
    associateDelegateToResource_organizationId,
    associateDelegateToResource_resourceId,
    associateDelegateToResource_entityId,
    associateDelegateToResourceResponse_httpStatus,

    -- ** AssociateMemberToGroup
    associateMemberToGroup_organizationId,
    associateMemberToGroup_groupId,
    associateMemberToGroup_memberId,
    associateMemberToGroupResponse_httpStatus,

    -- ** AssumeImpersonationRole
    assumeImpersonationRole_organizationId,
    assumeImpersonationRole_impersonationRoleId,
    assumeImpersonationRoleResponse_expiresIn,
    assumeImpersonationRoleResponse_token,
    assumeImpersonationRoleResponse_httpStatus,

    -- ** CancelMailboxExportJob
    cancelMailboxExportJob_clientToken,
    cancelMailboxExportJob_jobId,
    cancelMailboxExportJob_organizationId,
    cancelMailboxExportJobResponse_httpStatus,

    -- ** CreateAlias
    createAlias_organizationId,
    createAlias_entityId,
    createAlias_alias,
    createAliasResponse_httpStatus,

    -- ** CreateAvailabilityConfiguration
    createAvailabilityConfiguration_clientToken,
    createAvailabilityConfiguration_ewsProvider,
    createAvailabilityConfiguration_lambdaProvider,
    createAvailabilityConfiguration_organizationId,
    createAvailabilityConfiguration_domainName,
    createAvailabilityConfigurationResponse_httpStatus,

    -- ** CreateGroup
    createGroup_organizationId,
    createGroup_name,
    createGroupResponse_groupId,
    createGroupResponse_httpStatus,

    -- ** CreateImpersonationRole
    createImpersonationRole_clientToken,
    createImpersonationRole_description,
    createImpersonationRole_organizationId,
    createImpersonationRole_name,
    createImpersonationRole_type,
    createImpersonationRole_rules,
    createImpersonationRoleResponse_impersonationRoleId,
    createImpersonationRoleResponse_httpStatus,

    -- ** CreateMobileDeviceAccessRule
    createMobileDeviceAccessRule_clientToken,
    createMobileDeviceAccessRule_description,
    createMobileDeviceAccessRule_deviceModels,
    createMobileDeviceAccessRule_deviceOperatingSystems,
    createMobileDeviceAccessRule_deviceTypes,
    createMobileDeviceAccessRule_deviceUserAgents,
    createMobileDeviceAccessRule_notDeviceModels,
    createMobileDeviceAccessRule_notDeviceOperatingSystems,
    createMobileDeviceAccessRule_notDeviceTypes,
    createMobileDeviceAccessRule_notDeviceUserAgents,
    createMobileDeviceAccessRule_organizationId,
    createMobileDeviceAccessRule_name,
    createMobileDeviceAccessRule_effect,
    createMobileDeviceAccessRuleResponse_mobileDeviceAccessRuleId,
    createMobileDeviceAccessRuleResponse_httpStatus,

    -- ** CreateOrganization
    createOrganization_clientToken,
    createOrganization_directoryId,
    createOrganization_domains,
    createOrganization_enableInteroperability,
    createOrganization_kmsKeyArn,
    createOrganization_alias,
    createOrganizationResponse_organizationId,
    createOrganizationResponse_httpStatus,

    -- ** CreateResource
    createResource_organizationId,
    createResource_name,
    createResource_type,
    createResourceResponse_resourceId,
    createResourceResponse_httpStatus,

    -- ** CreateUser
    createUser_organizationId,
    createUser_name,
    createUser_displayName,
    createUser_password,
    createUserResponse_userId,
    createUserResponse_httpStatus,

    -- ** DeleteAccessControlRule
    deleteAccessControlRule_organizationId,
    deleteAccessControlRule_name,
    deleteAccessControlRuleResponse_httpStatus,

    -- ** DeleteAlias
    deleteAlias_organizationId,
    deleteAlias_entityId,
    deleteAlias_alias,
    deleteAliasResponse_httpStatus,

    -- ** DeleteAvailabilityConfiguration
    deleteAvailabilityConfiguration_organizationId,
    deleteAvailabilityConfiguration_domainName,
    deleteAvailabilityConfigurationResponse_httpStatus,

    -- ** DeleteEmailMonitoringConfiguration
    deleteEmailMonitoringConfiguration_organizationId,
    deleteEmailMonitoringConfigurationResponse_httpStatus,

    -- ** DeleteGroup
    deleteGroup_organizationId,
    deleteGroup_groupId,
    deleteGroupResponse_httpStatus,

    -- ** DeleteImpersonationRole
    deleteImpersonationRole_organizationId,
    deleteImpersonationRole_impersonationRoleId,
    deleteImpersonationRoleResponse_httpStatus,

    -- ** DeleteMailboxPermissions
    deleteMailboxPermissions_organizationId,
    deleteMailboxPermissions_entityId,
    deleteMailboxPermissions_granteeId,
    deleteMailboxPermissionsResponse_httpStatus,

    -- ** DeleteMobileDeviceAccessOverride
    deleteMobileDeviceAccessOverride_organizationId,
    deleteMobileDeviceAccessOverride_userId,
    deleteMobileDeviceAccessOverride_deviceId,
    deleteMobileDeviceAccessOverrideResponse_httpStatus,

    -- ** DeleteMobileDeviceAccessRule
    deleteMobileDeviceAccessRule_organizationId,
    deleteMobileDeviceAccessRule_mobileDeviceAccessRuleId,
    deleteMobileDeviceAccessRuleResponse_httpStatus,

    -- ** DeleteOrganization
    deleteOrganization_clientToken,
    deleteOrganization_organizationId,
    deleteOrganization_deleteDirectory,
    deleteOrganizationResponse_organizationId,
    deleteOrganizationResponse_state,
    deleteOrganizationResponse_httpStatus,

    -- ** DeleteResource
    deleteResource_organizationId,
    deleteResource_resourceId,
    deleteResourceResponse_httpStatus,

    -- ** DeleteRetentionPolicy
    deleteRetentionPolicy_organizationId,
    deleteRetentionPolicy_id,
    deleteRetentionPolicyResponse_httpStatus,

    -- ** DeleteUser
    deleteUser_organizationId,
    deleteUser_userId,
    deleteUserResponse_httpStatus,

    -- ** DeregisterFromWorkMail
    deregisterFromWorkMail_organizationId,
    deregisterFromWorkMail_entityId,
    deregisterFromWorkMailResponse_httpStatus,

    -- ** DeregisterMailDomain
    deregisterMailDomain_organizationId,
    deregisterMailDomain_domainName,
    deregisterMailDomainResponse_httpStatus,

    -- ** DescribeEmailMonitoringConfiguration
    describeEmailMonitoringConfiguration_organizationId,
    describeEmailMonitoringConfigurationResponse_logGroupArn,
    describeEmailMonitoringConfigurationResponse_roleArn,
    describeEmailMonitoringConfigurationResponse_httpStatus,

    -- ** DescribeGroup
    describeGroup_organizationId,
    describeGroup_groupId,
    describeGroupResponse_disabledDate,
    describeGroupResponse_email,
    describeGroupResponse_enabledDate,
    describeGroupResponse_groupId,
    describeGroupResponse_name,
    describeGroupResponse_state,
    describeGroupResponse_httpStatus,

    -- ** DescribeInboundDmarcSettings
    describeInboundDmarcSettings_organizationId,
    describeInboundDmarcSettingsResponse_enforced,
    describeInboundDmarcSettingsResponse_httpStatus,

    -- ** DescribeMailboxExportJob
    describeMailboxExportJob_jobId,
    describeMailboxExportJob_organizationId,
    describeMailboxExportJobResponse_description,
    describeMailboxExportJobResponse_endTime,
    describeMailboxExportJobResponse_entityId,
    describeMailboxExportJobResponse_errorInfo,
    describeMailboxExportJobResponse_estimatedProgress,
    describeMailboxExportJobResponse_kmsKeyArn,
    describeMailboxExportJobResponse_roleArn,
    describeMailboxExportJobResponse_s3BucketName,
    describeMailboxExportJobResponse_s3Path,
    describeMailboxExportJobResponse_s3Prefix,
    describeMailboxExportJobResponse_startTime,
    describeMailboxExportJobResponse_state,
    describeMailboxExportJobResponse_httpStatus,

    -- ** DescribeOrganization
    describeOrganization_organizationId,
    describeOrganizationResponse_arn,
    describeOrganizationResponse_alias,
    describeOrganizationResponse_completedDate,
    describeOrganizationResponse_defaultMailDomain,
    describeOrganizationResponse_directoryId,
    describeOrganizationResponse_directoryType,
    describeOrganizationResponse_errorMessage,
    describeOrganizationResponse_organizationId,
    describeOrganizationResponse_state,
    describeOrganizationResponse_httpStatus,

    -- ** DescribeResource
    describeResource_organizationId,
    describeResource_resourceId,
    describeResourceResponse_bookingOptions,
    describeResourceResponse_disabledDate,
    describeResourceResponse_email,
    describeResourceResponse_enabledDate,
    describeResourceResponse_name,
    describeResourceResponse_resourceId,
    describeResourceResponse_state,
    describeResourceResponse_type,
    describeResourceResponse_httpStatus,

    -- ** DescribeUser
    describeUser_organizationId,
    describeUser_userId,
    describeUserResponse_disabledDate,
    describeUserResponse_displayName,
    describeUserResponse_email,
    describeUserResponse_enabledDate,
    describeUserResponse_name,
    describeUserResponse_state,
    describeUserResponse_userId,
    describeUserResponse_userRole,
    describeUserResponse_httpStatus,

    -- ** DisassociateDelegateFromResource
    disassociateDelegateFromResource_organizationId,
    disassociateDelegateFromResource_resourceId,
    disassociateDelegateFromResource_entityId,
    disassociateDelegateFromResourceResponse_httpStatus,

    -- ** DisassociateMemberFromGroup
    disassociateMemberFromGroup_organizationId,
    disassociateMemberFromGroup_groupId,
    disassociateMemberFromGroup_memberId,
    disassociateMemberFromGroupResponse_httpStatus,

    -- ** GetAccessControlEffect
    getAccessControlEffect_impersonationRoleId,
    getAccessControlEffect_userId,
    getAccessControlEffect_organizationId,
    getAccessControlEffect_ipAddress,
    getAccessControlEffect_action,
    getAccessControlEffectResponse_effect,
    getAccessControlEffectResponse_matchedRules,
    getAccessControlEffectResponse_httpStatus,

    -- ** GetDefaultRetentionPolicy
    getDefaultRetentionPolicy_organizationId,
    getDefaultRetentionPolicyResponse_description,
    getDefaultRetentionPolicyResponse_folderConfigurations,
    getDefaultRetentionPolicyResponse_id,
    getDefaultRetentionPolicyResponse_name,
    getDefaultRetentionPolicyResponse_httpStatus,

    -- ** GetImpersonationRole
    getImpersonationRole_organizationId,
    getImpersonationRole_impersonationRoleId,
    getImpersonationRoleResponse_dateCreated,
    getImpersonationRoleResponse_dateModified,
    getImpersonationRoleResponse_description,
    getImpersonationRoleResponse_impersonationRoleId,
    getImpersonationRoleResponse_name,
    getImpersonationRoleResponse_rules,
    getImpersonationRoleResponse_type,
    getImpersonationRoleResponse_httpStatus,

    -- ** GetImpersonationRoleEffect
    getImpersonationRoleEffect_organizationId,
    getImpersonationRoleEffect_impersonationRoleId,
    getImpersonationRoleEffect_targetUser,
    getImpersonationRoleEffectResponse_effect,
    getImpersonationRoleEffectResponse_matchedRules,
    getImpersonationRoleEffectResponse_type,
    getImpersonationRoleEffectResponse_httpStatus,

    -- ** GetMailDomain
    getMailDomain_organizationId,
    getMailDomain_domainName,
    getMailDomainResponse_dkimVerificationStatus,
    getMailDomainResponse_isDefault,
    getMailDomainResponse_isTestDomain,
    getMailDomainResponse_ownershipVerificationStatus,
    getMailDomainResponse_records,
    getMailDomainResponse_httpStatus,

    -- ** GetMailboxDetails
    getMailboxDetails_organizationId,
    getMailboxDetails_userId,
    getMailboxDetailsResponse_mailboxQuota,
    getMailboxDetailsResponse_mailboxSize,
    getMailboxDetailsResponse_httpStatus,

    -- ** GetMobileDeviceAccessEffect
    getMobileDeviceAccessEffect_deviceModel,
    getMobileDeviceAccessEffect_deviceOperatingSystem,
    getMobileDeviceAccessEffect_deviceType,
    getMobileDeviceAccessEffect_deviceUserAgent,
    getMobileDeviceAccessEffect_organizationId,
    getMobileDeviceAccessEffectResponse_effect,
    getMobileDeviceAccessEffectResponse_matchedRules,
    getMobileDeviceAccessEffectResponse_httpStatus,

    -- ** GetMobileDeviceAccessOverride
    getMobileDeviceAccessOverride_organizationId,
    getMobileDeviceAccessOverride_userId,
    getMobileDeviceAccessOverride_deviceId,
    getMobileDeviceAccessOverrideResponse_dateCreated,
    getMobileDeviceAccessOverrideResponse_dateModified,
    getMobileDeviceAccessOverrideResponse_description,
    getMobileDeviceAccessOverrideResponse_deviceId,
    getMobileDeviceAccessOverrideResponse_effect,
    getMobileDeviceAccessOverrideResponse_userId,
    getMobileDeviceAccessOverrideResponse_httpStatus,

    -- ** ListAccessControlRules
    listAccessControlRules_organizationId,
    listAccessControlRulesResponse_rules,
    listAccessControlRulesResponse_httpStatus,

    -- ** ListAliases
    listAliases_maxResults,
    listAliases_nextToken,
    listAliases_organizationId,
    listAliases_entityId,
    listAliasesResponse_aliases,
    listAliasesResponse_nextToken,
    listAliasesResponse_httpStatus,

    -- ** ListAvailabilityConfigurations
    listAvailabilityConfigurations_maxResults,
    listAvailabilityConfigurations_nextToken,
    listAvailabilityConfigurations_organizationId,
    listAvailabilityConfigurationsResponse_availabilityConfigurations,
    listAvailabilityConfigurationsResponse_nextToken,
    listAvailabilityConfigurationsResponse_httpStatus,

    -- ** ListGroupMembers
    listGroupMembers_maxResults,
    listGroupMembers_nextToken,
    listGroupMembers_organizationId,
    listGroupMembers_groupId,
    listGroupMembersResponse_members,
    listGroupMembersResponse_nextToken,
    listGroupMembersResponse_httpStatus,

    -- ** ListGroups
    listGroups_maxResults,
    listGroups_nextToken,
    listGroups_organizationId,
    listGroupsResponse_groups,
    listGroupsResponse_nextToken,
    listGroupsResponse_httpStatus,

    -- ** ListImpersonationRoles
    listImpersonationRoles_maxResults,
    listImpersonationRoles_nextToken,
    listImpersonationRoles_organizationId,
    listImpersonationRolesResponse_nextToken,
    listImpersonationRolesResponse_roles,
    listImpersonationRolesResponse_httpStatus,

    -- ** ListMailDomains
    listMailDomains_maxResults,
    listMailDomains_nextToken,
    listMailDomains_organizationId,
    listMailDomainsResponse_mailDomains,
    listMailDomainsResponse_nextToken,
    listMailDomainsResponse_httpStatus,

    -- ** ListMailboxExportJobs
    listMailboxExportJobs_maxResults,
    listMailboxExportJobs_nextToken,
    listMailboxExportJobs_organizationId,
    listMailboxExportJobsResponse_jobs,
    listMailboxExportJobsResponse_nextToken,
    listMailboxExportJobsResponse_httpStatus,

    -- ** ListMailboxPermissions
    listMailboxPermissions_maxResults,
    listMailboxPermissions_nextToken,
    listMailboxPermissions_organizationId,
    listMailboxPermissions_entityId,
    listMailboxPermissionsResponse_nextToken,
    listMailboxPermissionsResponse_permissions,
    listMailboxPermissionsResponse_httpStatus,

    -- ** ListMobileDeviceAccessOverrides
    listMobileDeviceAccessOverrides_deviceId,
    listMobileDeviceAccessOverrides_maxResults,
    listMobileDeviceAccessOverrides_nextToken,
    listMobileDeviceAccessOverrides_userId,
    listMobileDeviceAccessOverrides_organizationId,
    listMobileDeviceAccessOverridesResponse_nextToken,
    listMobileDeviceAccessOverridesResponse_overrides,
    listMobileDeviceAccessOverridesResponse_httpStatus,

    -- ** ListMobileDeviceAccessRules
    listMobileDeviceAccessRules_organizationId,
    listMobileDeviceAccessRulesResponse_rules,
    listMobileDeviceAccessRulesResponse_httpStatus,

    -- ** ListOrganizations
    listOrganizations_maxResults,
    listOrganizations_nextToken,
    listOrganizationsResponse_nextToken,
    listOrganizationsResponse_organizationSummaries,
    listOrganizationsResponse_httpStatus,

    -- ** ListResourceDelegates
    listResourceDelegates_maxResults,
    listResourceDelegates_nextToken,
    listResourceDelegates_organizationId,
    listResourceDelegates_resourceId,
    listResourceDelegatesResponse_delegates,
    listResourceDelegatesResponse_nextToken,
    listResourceDelegatesResponse_httpStatus,

    -- ** ListResources
    listResources_maxResults,
    listResources_nextToken,
    listResources_organizationId,
    listResourcesResponse_nextToken,
    listResourcesResponse_resources,
    listResourcesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListUsers
    listUsers_maxResults,
    listUsers_nextToken,
    listUsers_organizationId,
    listUsersResponse_nextToken,
    listUsersResponse_users,
    listUsersResponse_httpStatus,

    -- ** PutAccessControlRule
    putAccessControlRule_actions,
    putAccessControlRule_impersonationRoleIds,
    putAccessControlRule_ipRanges,
    putAccessControlRule_notActions,
    putAccessControlRule_notImpersonationRoleIds,
    putAccessControlRule_notIpRanges,
    putAccessControlRule_notUserIds,
    putAccessControlRule_userIds,
    putAccessControlRule_name,
    putAccessControlRule_effect,
    putAccessControlRule_description,
    putAccessControlRule_organizationId,
    putAccessControlRuleResponse_httpStatus,

    -- ** PutEmailMonitoringConfiguration
    putEmailMonitoringConfiguration_organizationId,
    putEmailMonitoringConfiguration_roleArn,
    putEmailMonitoringConfiguration_logGroupArn,
    putEmailMonitoringConfigurationResponse_httpStatus,

    -- ** PutInboundDmarcSettings
    putInboundDmarcSettings_organizationId,
    putInboundDmarcSettings_enforced,
    putInboundDmarcSettingsResponse_httpStatus,

    -- ** PutMailboxPermissions
    putMailboxPermissions_organizationId,
    putMailboxPermissions_entityId,
    putMailboxPermissions_granteeId,
    putMailboxPermissions_permissionValues,
    putMailboxPermissionsResponse_httpStatus,

    -- ** PutMobileDeviceAccessOverride
    putMobileDeviceAccessOverride_description,
    putMobileDeviceAccessOverride_organizationId,
    putMobileDeviceAccessOverride_userId,
    putMobileDeviceAccessOverride_deviceId,
    putMobileDeviceAccessOverride_effect,
    putMobileDeviceAccessOverrideResponse_httpStatus,

    -- ** PutRetentionPolicy
    putRetentionPolicy_description,
    putRetentionPolicy_id,
    putRetentionPolicy_organizationId,
    putRetentionPolicy_name,
    putRetentionPolicy_folderConfigurations,
    putRetentionPolicyResponse_httpStatus,

    -- ** RegisterMailDomain
    registerMailDomain_clientToken,
    registerMailDomain_organizationId,
    registerMailDomain_domainName,
    registerMailDomainResponse_httpStatus,

    -- ** RegisterToWorkMail
    registerToWorkMail_organizationId,
    registerToWorkMail_entityId,
    registerToWorkMail_email,
    registerToWorkMailResponse_httpStatus,

    -- ** ResetPassword
    resetPassword_organizationId,
    resetPassword_userId,
    resetPassword_password,
    resetPasswordResponse_httpStatus,

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

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** TestAvailabilityConfiguration
    testAvailabilityConfiguration_domainName,
    testAvailabilityConfiguration_ewsProvider,
    testAvailabilityConfiguration_lambdaProvider,
    testAvailabilityConfiguration_organizationId,
    testAvailabilityConfigurationResponse_failureReason,
    testAvailabilityConfigurationResponse_testPassed,
    testAvailabilityConfigurationResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateAvailabilityConfiguration
    updateAvailabilityConfiguration_ewsProvider,
    updateAvailabilityConfiguration_lambdaProvider,
    updateAvailabilityConfiguration_organizationId,
    updateAvailabilityConfiguration_domainName,
    updateAvailabilityConfigurationResponse_httpStatus,

    -- ** UpdateDefaultMailDomain
    updateDefaultMailDomain_organizationId,
    updateDefaultMailDomain_domainName,
    updateDefaultMailDomainResponse_httpStatus,

    -- ** UpdateImpersonationRole
    updateImpersonationRole_description,
    updateImpersonationRole_organizationId,
    updateImpersonationRole_impersonationRoleId,
    updateImpersonationRole_name,
    updateImpersonationRole_type,
    updateImpersonationRole_rules,
    updateImpersonationRoleResponse_httpStatus,

    -- ** UpdateMailboxQuota
    updateMailboxQuota_organizationId,
    updateMailboxQuota_userId,
    updateMailboxQuota_mailboxQuota,
    updateMailboxQuotaResponse_httpStatus,

    -- ** UpdateMobileDeviceAccessRule
    updateMobileDeviceAccessRule_description,
    updateMobileDeviceAccessRule_deviceModels,
    updateMobileDeviceAccessRule_deviceOperatingSystems,
    updateMobileDeviceAccessRule_deviceTypes,
    updateMobileDeviceAccessRule_deviceUserAgents,
    updateMobileDeviceAccessRule_notDeviceModels,
    updateMobileDeviceAccessRule_notDeviceOperatingSystems,
    updateMobileDeviceAccessRule_notDeviceTypes,
    updateMobileDeviceAccessRule_notDeviceUserAgents,
    updateMobileDeviceAccessRule_organizationId,
    updateMobileDeviceAccessRule_mobileDeviceAccessRuleId,
    updateMobileDeviceAccessRule_name,
    updateMobileDeviceAccessRule_effect,
    updateMobileDeviceAccessRuleResponse_httpStatus,

    -- ** UpdatePrimaryEmailAddress
    updatePrimaryEmailAddress_organizationId,
    updatePrimaryEmailAddress_entityId,
    updatePrimaryEmailAddress_email,
    updatePrimaryEmailAddressResponse_httpStatus,

    -- ** UpdateResource
    updateResource_bookingOptions,
    updateResource_name,
    updateResource_organizationId,
    updateResource_resourceId,
    updateResourceResponse_httpStatus,

    -- * Types

    -- ** AccessControlRule
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

    -- ** AvailabilityConfiguration
    availabilityConfiguration_dateCreated,
    availabilityConfiguration_dateModified,
    availabilityConfiguration_domainName,
    availabilityConfiguration_ewsProvider,
    availabilityConfiguration_lambdaProvider,
    availabilityConfiguration_providerType,

    -- ** BookingOptions
    bookingOptions_autoAcceptRequests,
    bookingOptions_autoDeclineConflictingRequests,
    bookingOptions_autoDeclineRecurringRequests,

    -- ** Delegate
    delegate_id,
    delegate_type,

    -- ** DnsRecord
    dnsRecord_hostname,
    dnsRecord_type,
    dnsRecord_value,

    -- ** Domain
    domain_domainName,
    domain_hostedZoneId,

    -- ** EwsAvailabilityProvider
    ewsAvailabilityProvider_ewsEndpoint,
    ewsAvailabilityProvider_ewsUsername,
    ewsAvailabilityProvider_ewsPassword,

    -- ** FolderConfiguration
    folderConfiguration_period,
    folderConfiguration_name,
    folderConfiguration_action,

    -- ** Group
    group_disabledDate,
    group_email,
    group_enabledDate,
    group_id,
    group_name,
    group_state,

    -- ** ImpersonationMatchedRule
    impersonationMatchedRule_impersonationRuleId,
    impersonationMatchedRule_name,

    -- ** ImpersonationRole
    impersonationRole_dateCreated,
    impersonationRole_dateModified,
    impersonationRole_impersonationRoleId,
    impersonationRole_name,
    impersonationRole_type,

    -- ** ImpersonationRule
    impersonationRule_description,
    impersonationRule_name,
    impersonationRule_notTargetUsers,
    impersonationRule_targetUsers,
    impersonationRule_impersonationRuleId,
    impersonationRule_effect,

    -- ** LambdaAvailabilityProvider
    lambdaAvailabilityProvider_lambdaArn,

    -- ** MailDomainSummary
    mailDomainSummary_defaultDomain,
    mailDomainSummary_domainName,

    -- ** MailboxExportJob
    mailboxExportJob_description,
    mailboxExportJob_endTime,
    mailboxExportJob_entityId,
    mailboxExportJob_estimatedProgress,
    mailboxExportJob_jobId,
    mailboxExportJob_s3BucketName,
    mailboxExportJob_s3Path,
    mailboxExportJob_startTime,
    mailboxExportJob_state,

    -- ** Member
    member_disabledDate,
    member_enabledDate,
    member_id,
    member_name,
    member_state,
    member_type,

    -- ** MobileDeviceAccessMatchedRule
    mobileDeviceAccessMatchedRule_mobileDeviceAccessRuleId,
    mobileDeviceAccessMatchedRule_name,

    -- ** MobileDeviceAccessOverride
    mobileDeviceAccessOverride_dateCreated,
    mobileDeviceAccessOverride_dateModified,
    mobileDeviceAccessOverride_description,
    mobileDeviceAccessOverride_deviceId,
    mobileDeviceAccessOverride_effect,
    mobileDeviceAccessOverride_userId,

    -- ** MobileDeviceAccessRule
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

    -- ** OrganizationSummary
    organizationSummary_alias,
    organizationSummary_defaultMailDomain,
    organizationSummary_errorMessage,
    organizationSummary_organizationId,
    organizationSummary_state,

    -- ** Permission
    permission_granteeId,
    permission_granteeType,
    permission_permissionValues,

    -- ** RedactedEwsAvailabilityProvider
    redactedEwsAvailabilityProvider_ewsEndpoint,
    redactedEwsAvailabilityProvider_ewsUsername,

    -- ** Resource
    resource_disabledDate,
    resource_email,
    resource_enabledDate,
    resource_id,
    resource_name,
    resource_state,
    resource_type,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** User
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
import Amazonka.WorkMail.Types.AccessControlRule
import Amazonka.WorkMail.Types.AvailabilityConfiguration
import Amazonka.WorkMail.Types.BookingOptions
import Amazonka.WorkMail.Types.Delegate
import Amazonka.WorkMail.Types.DnsRecord
import Amazonka.WorkMail.Types.Domain
import Amazonka.WorkMail.Types.EwsAvailabilityProvider
import Amazonka.WorkMail.Types.FolderConfiguration
import Amazonka.WorkMail.Types.Group
import Amazonka.WorkMail.Types.ImpersonationMatchedRule
import Amazonka.WorkMail.Types.ImpersonationRole
import Amazonka.WorkMail.Types.ImpersonationRule
import Amazonka.WorkMail.Types.LambdaAvailabilityProvider
import Amazonka.WorkMail.Types.MailDomainSummary
import Amazonka.WorkMail.Types.MailboxExportJob
import Amazonka.WorkMail.Types.Member
import Amazonka.WorkMail.Types.MobileDeviceAccessMatchedRule
import Amazonka.WorkMail.Types.MobileDeviceAccessOverride
import Amazonka.WorkMail.Types.MobileDeviceAccessRule
import Amazonka.WorkMail.Types.OrganizationSummary
import Amazonka.WorkMail.Types.Permission
import Amazonka.WorkMail.Types.RedactedEwsAvailabilityProvider
import Amazonka.WorkMail.Types.Resource
import Amazonka.WorkMail.Types.Tag
import Amazonka.WorkMail.Types.User
import Amazonka.WorkMail.UntagResource
import Amazonka.WorkMail.UpdateAvailabilityConfiguration
import Amazonka.WorkMail.UpdateDefaultMailDomain
import Amazonka.WorkMail.UpdateImpersonationRole
import Amazonka.WorkMail.UpdateMailboxQuota
import Amazonka.WorkMail.UpdateMobileDeviceAccessRule
import Amazonka.WorkMail.UpdatePrimaryEmailAddress
import Amazonka.WorkMail.UpdateResource
