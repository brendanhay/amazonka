{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Lens
  ( -- * Operations

    -- ** GetContextKeysForPrincipalPolicy
    getContextKeysForPrincipalPolicy_policyInputList,
    getContextKeysForPrincipalPolicy_policySourceArn,
    getContextKeysForPolicyResponse_contextKeyNames,

    -- ** ListPolicies
    listPolicies_pathPrefix,
    listPolicies_onlyAttached,
    listPolicies_marker,
    listPolicies_scope,
    listPolicies_maxItems,
    listPolicies_policyUsageFilter,
    listPoliciesResponse_marker,
    listPoliciesResponse_isTruncated,
    listPoliciesResponse_policies,
    listPoliciesResponse_httpStatus,

    -- ** CreatePolicy
    createPolicy_path,
    createPolicy_description,
    createPolicy_tags,
    createPolicy_policyName,
    createPolicy_policyDocument,
    createPolicyResponse_policy,
    createPolicyResponse_httpStatus,

    -- ** ListInstanceProfilesForRole
    listInstanceProfilesForRole_marker,
    listInstanceProfilesForRole_maxItems,
    listInstanceProfilesForRole_roleName,
    listInstanceProfilesForRoleResponse_marker,
    listInstanceProfilesForRoleResponse_isTruncated,
    listInstanceProfilesForRoleResponse_httpStatus,
    listInstanceProfilesForRoleResponse_instanceProfiles,

    -- ** AttachGroupPolicy
    attachGroupPolicy_groupName,
    attachGroupPolicy_policyArn,

    -- ** CreateAccessKey
    createAccessKey_userName,
    createAccessKeyResponse_httpStatus,
    createAccessKeyResponse_accessKey,

    -- ** ListRoleTags
    listRoleTags_marker,
    listRoleTags_maxItems,
    listRoleTags_roleName,
    listRoleTagsResponse_marker,
    listRoleTagsResponse_isTruncated,
    listRoleTagsResponse_httpStatus,
    listRoleTagsResponse_tags,

    -- ** ListSSHPublicKeys
    listSSHPublicKeys_userName,
    listSSHPublicKeys_marker,
    listSSHPublicKeys_maxItems,
    listSSHPublicKeysResponse_sSHPublicKeys,
    listSSHPublicKeysResponse_marker,
    listSSHPublicKeysResponse_isTruncated,
    listSSHPublicKeysResponse_httpStatus,

    -- ** UntagOpenIDConnectProvider
    untagOpenIDConnectProvider_openIDConnectProviderArn,
    untagOpenIDConnectProvider_tagKeys,

    -- ** ListOpenIDConnectProviders
    listOpenIDConnectProvidersResponse_openIDConnectProviderList,
    listOpenIDConnectProvidersResponse_httpStatus,

    -- ** CreateVirtualMFADevice
    createVirtualMFADevice_path,
    createVirtualMFADevice_tags,
    createVirtualMFADevice_virtualMFADeviceName,
    createVirtualMFADeviceResponse_httpStatus,
    createVirtualMFADeviceResponse_virtualMFADevice,

    -- ** DeleteAccountPasswordPolicy

    -- ** UpdateAccountPasswordPolicy
    updateAccountPasswordPolicy_minimumPasswordLength,
    updateAccountPasswordPolicy_requireNumbers,
    updateAccountPasswordPolicy_passwordReusePrevention,
    updateAccountPasswordPolicy_requireLowercaseCharacters,
    updateAccountPasswordPolicy_maxPasswordAge,
    updateAccountPasswordPolicy_hardExpiry,
    updateAccountPasswordPolicy_requireSymbols,
    updateAccountPasswordPolicy_requireUppercaseCharacters,
    updateAccountPasswordPolicy_allowUsersToChangePassword,

    -- ** AttachRolePolicy
    attachRolePolicy_roleName,
    attachRolePolicy_policyArn,

    -- ** UpdateSSHPublicKey
    updateSSHPublicKey_userName,
    updateSSHPublicKey_sSHPublicKeyId,
    updateSSHPublicKey_status,

    -- ** DeleteSSHPublicKey
    deleteSSHPublicKey_userName,
    deleteSSHPublicKey_sSHPublicKeyId,

    -- ** GetUserPolicy
    getUserPolicy_userName,
    getUserPolicy_policyName,
    getUserPolicyResponse_httpStatus,
    getUserPolicyResponse_userName,
    getUserPolicyResponse_policyName,
    getUserPolicyResponse_policyDocument,

    -- ** UpdateServiceSpecificCredential
    updateServiceSpecificCredential_userName,
    updateServiceSpecificCredential_serviceSpecificCredentialId,
    updateServiceSpecificCredential_status,

    -- ** DeleteServiceSpecificCredential
    deleteServiceSpecificCredential_userName,
    deleteServiceSpecificCredential_serviceSpecificCredentialId,

    -- ** ListAttachedRolePolicies
    listAttachedRolePolicies_pathPrefix,
    listAttachedRolePolicies_marker,
    listAttachedRolePolicies_maxItems,
    listAttachedRolePolicies_roleName,
    listAttachedRolePoliciesResponse_attachedPolicies,
    listAttachedRolePoliciesResponse_marker,
    listAttachedRolePoliciesResponse_isTruncated,
    listAttachedRolePoliciesResponse_httpStatus,

    -- ** GetRole
    getRole_roleName,
    getRoleResponse_httpStatus,
    getRoleResponse_role,

    -- ** DeactivateMFADevice
    deactivateMFADevice_userName,
    deactivateMFADevice_serialNumber,

    -- ** CreateOpenIDConnectProvider
    createOpenIDConnectProvider_clientIDList,
    createOpenIDConnectProvider_tags,
    createOpenIDConnectProvider_url,
    createOpenIDConnectProvider_thumbprintList,
    createOpenIDConnectProviderResponse_openIDConnectProviderArn,
    createOpenIDConnectProviderResponse_tags,
    createOpenIDConnectProviderResponse_httpStatus,

    -- ** DeleteVirtualMFADevice
    deleteVirtualMFADevice_serialNumber,

    -- ** ListRoles
    listRoles_pathPrefix,
    listRoles_marker,
    listRoles_maxItems,
    listRolesResponse_marker,
    listRolesResponse_isTruncated,
    listRolesResponse_httpStatus,
    listRolesResponse_roles,

    -- ** ListUserPolicies
    listUserPolicies_marker,
    listUserPolicies_maxItems,
    listUserPolicies_userName,
    listUserPoliciesResponse_marker,
    listUserPoliciesResponse_isTruncated,
    listUserPoliciesResponse_httpStatus,
    listUserPoliciesResponse_policyNames,

    -- ** ListOpenIDConnectProviderTags
    listOpenIDConnectProviderTags_marker,
    listOpenIDConnectProviderTags_maxItems,
    listOpenIDConnectProviderTags_openIDConnectProviderArn,
    listOpenIDConnectProviderTagsResponse_marker,
    listOpenIDConnectProviderTagsResponse_isTruncated,
    listOpenIDConnectProviderTagsResponse_httpStatus,
    listOpenIDConnectProviderTagsResponse_tags,

    -- ** PutRolePermissionsBoundary
    putRolePermissionsBoundary_roleName,
    putRolePermissionsBoundary_permissionsBoundary,

    -- ** UploadSSHPublicKey
    uploadSSHPublicKey_userName,
    uploadSSHPublicKey_sSHPublicKeyBody,
    uploadSSHPublicKeyResponse_sSHPublicKey,
    uploadSSHPublicKeyResponse_httpStatus,

    -- ** DeleteRolePermissionsBoundary
    deleteRolePermissionsBoundary_roleName,

    -- ** SimulateCustomPolicy
    simulateCustomPolicy_resourcePolicy,
    simulateCustomPolicy_callerArn,
    simulateCustomPolicy_resourceHandlingOption,
    simulateCustomPolicy_resourceArns,
    simulateCustomPolicy_permissionsBoundaryPolicyInputList,
    simulateCustomPolicy_marker,
    simulateCustomPolicy_maxItems,
    simulateCustomPolicy_contextEntries,
    simulateCustomPolicy_resourceOwner,
    simulateCustomPolicy_policyInputList,
    simulateCustomPolicy_actionNames,
    simulatePolicyResponse_evaluationResults,
    simulatePolicyResponse_marker,
    simulatePolicyResponse_isTruncated,

    -- ** UpdateRole
    updateRole_maxSessionDuration,
    updateRole_description,
    updateRole_roleName,
    updateRoleResponse_httpStatus,

    -- ** DeleteRole
    deleteRole_roleName,

    -- ** ListUsers
    listUsers_pathPrefix,
    listUsers_marker,
    listUsers_maxItems,
    listUsersResponse_marker,
    listUsersResponse_isTruncated,
    listUsersResponse_httpStatus,
    listUsersResponse_users,

    -- ** UpdateOpenIDConnectProviderThumbprint
    updateOpenIDConnectProviderThumbprint_openIDConnectProviderArn,
    updateOpenIDConnectProviderThumbprint_thumbprintList,

    -- ** PutUserPolicy
    putUserPolicy_userName,
    putUserPolicy_policyName,
    putUserPolicy_policyDocument,

    -- ** TagMFADevice
    tagMFADevice_serialNumber,
    tagMFADevice_tags,

    -- ** GetSSHPublicKey
    getSSHPublicKey_userName,
    getSSHPublicKey_sSHPublicKeyId,
    getSSHPublicKey_encoding,
    getSSHPublicKeyResponse_sSHPublicKey,
    getSSHPublicKeyResponse_httpStatus,

    -- ** UntagUser
    untagUser_userName,
    untagUser_tagKeys,

    -- ** DetachGroupPolicy
    detachGroupPolicy_groupName,
    detachGroupPolicy_policyArn,

    -- ** TagInstanceProfile
    tagInstanceProfile_instanceProfileName,
    tagInstanceProfile_tags,

    -- ** GetOpenIDConnectProvider
    getOpenIDConnectProvider_openIDConnectProviderArn,
    getOpenIDConnectProviderResponse_createDate,
    getOpenIDConnectProviderResponse_url,
    getOpenIDConnectProviderResponse_thumbprintList,
    getOpenIDConnectProviderResponse_clientIDList,
    getOpenIDConnectProviderResponse_tags,
    getOpenIDConnectProviderResponse_httpStatus,

    -- ** PutUserPermissionsBoundary
    putUserPermissionsBoundary_userName,
    putUserPermissionsBoundary_permissionsBoundary,

    -- ** DeleteUserPolicy
    deleteUserPolicy_userName,
    deleteUserPolicy_policyName,

    -- ** TagSAMLProvider
    tagSAMLProvider_sAMLProviderArn,
    tagSAMLProvider_tags,

    -- ** DeleteUserPermissionsBoundary
    deleteUserPermissionsBoundary_userName,

    -- ** CreateRole
    createRole_maxSessionDuration,
    createRole_path,
    createRole_permissionsBoundary,
    createRole_description,
    createRole_tags,
    createRole_roleName,
    createRole_assumeRolePolicyDocument,
    createRoleResponse_httpStatus,
    createRoleResponse_role,

    -- ** ResetServiceSpecificCredential
    resetServiceSpecificCredential_userName,
    resetServiceSpecificCredential_serviceSpecificCredentialId,
    resetServiceSpecificCredentialResponse_serviceSpecificCredential,
    resetServiceSpecificCredentialResponse_httpStatus,

    -- ** UntagSAMLProvider
    untagSAMLProvider_sAMLProviderArn,
    untagSAMLProvider_tagKeys,

    -- ** GetCredentialReport
    getCredentialReportResponse_content,
    getCredentialReportResponse_generatedTime,
    getCredentialReportResponse_reportFormat,
    getCredentialReportResponse_httpStatus,

    -- ** ListServerCertificateTags
    listServerCertificateTags_marker,
    listServerCertificateTags_maxItems,
    listServerCertificateTags_serverCertificateName,
    listServerCertificateTagsResponse_marker,
    listServerCertificateTagsResponse_isTruncated,
    listServerCertificateTagsResponse_httpStatus,
    listServerCertificateTagsResponse_tags,

    -- ** GetAccountSummary
    getAccountSummaryResponse_summaryMap,
    getAccountSummaryResponse_httpStatus,

    -- ** GenerateServiceLastAccessedDetails
    generateServiceLastAccessedDetails_granularity,
    generateServiceLastAccessedDetails_arn,
    generateServiceLastAccessedDetailsResponse_jobId,
    generateServiceLastAccessedDetailsResponse_httpStatus,

    -- ** ListPolicyTags
    listPolicyTags_marker,
    listPolicyTags_maxItems,
    listPolicyTags_policyArn,
    listPolicyTagsResponse_marker,
    listPolicyTagsResponse_isTruncated,
    listPolicyTagsResponse_httpStatus,
    listPolicyTagsResponse_tags,

    -- ** ListGroupPolicies
    listGroupPolicies_marker,
    listGroupPolicies_maxItems,
    listGroupPolicies_groupName,
    listGroupPoliciesResponse_marker,
    listGroupPoliciesResponse_isTruncated,
    listGroupPoliciesResponse_httpStatus,
    listGroupPoliciesResponse_policyNames,

    -- ** DeletePolicyVersion
    deletePolicyVersion_policyArn,
    deletePolicyVersion_versionId,

    -- ** TagUser
    tagUser_userName,
    tagUser_tags,

    -- ** DeleteInstanceProfile
    deleteInstanceProfile_instanceProfileName,

    -- ** DetachRolePolicy
    detachRolePolicy_roleName,
    detachRolePolicy_policyArn,

    -- ** RemoveRoleFromInstanceProfile
    removeRoleFromInstanceProfile_instanceProfileName,
    removeRoleFromInstanceProfile_roleName,

    -- ** CreatePolicyVersion
    createPolicyVersion_setAsDefault,
    createPolicyVersion_policyArn,
    createPolicyVersion_policyDocument,
    createPolicyVersionResponse_policyVersion,
    createPolicyVersionResponse_httpStatus,

    -- ** CreateInstanceProfile
    createInstanceProfile_path,
    createInstanceProfile_tags,
    createInstanceProfile_instanceProfileName,
    createInstanceProfileResponse_httpStatus,
    createInstanceProfileResponse_instanceProfile,

    -- ** CreateSAMLProvider
    createSAMLProvider_tags,
    createSAMLProvider_sAMLMetadataDocument,
    createSAMLProvider_name,
    createSAMLProviderResponse_sAMLProviderArn,
    createSAMLProviderResponse_tags,
    createSAMLProviderResponse_httpStatus,

    -- ** GetAccountAuthorizationDetails
    getAccountAuthorizationDetails_marker,
    getAccountAuthorizationDetails_maxItems,
    getAccountAuthorizationDetails_filter,
    getAccountAuthorizationDetailsResponse_roleDetailList,
    getAccountAuthorizationDetailsResponse_groupDetailList,
    getAccountAuthorizationDetailsResponse_userDetailList,
    getAccountAuthorizationDetailsResponse_marker,
    getAccountAuthorizationDetailsResponse_isTruncated,
    getAccountAuthorizationDetailsResponse_policies,
    getAccountAuthorizationDetailsResponse_httpStatus,

    -- ** GetServiceLinkedRoleDeletionStatus
    getServiceLinkedRoleDeletionStatus_deletionTaskId,
    getServiceLinkedRoleDeletionStatusResponse_reason,
    getServiceLinkedRoleDeletionStatusResponse_httpStatus,
    getServiceLinkedRoleDeletionStatusResponse_status,

    -- ** DeleteAccountAlias
    deleteAccountAlias_accountAlias,

    -- ** DetachUserPolicy
    detachUserPolicy_userName,
    detachUserPolicy_policyArn,

    -- ** RemoveUserFromGroup
    removeUserFromGroup_groupName,
    removeUserFromGroup_userName,

    -- ** DeleteGroupPolicy
    deleteGroupPolicy_groupName,
    deleteGroupPolicy_policyName,

    -- ** TagRole
    tagRole_roleName,
    tagRole_tags,

    -- ** PutGroupPolicy
    putGroupPolicy_groupName,
    putGroupPolicy_policyName,
    putGroupPolicy_policyDocument,

    -- ** GetLoginProfile
    getLoginProfile_userName,
    getLoginProfileResponse_httpStatus,
    getLoginProfileResponse_loginProfile,

    -- ** GetGroupPolicy
    getGroupPolicy_groupName,
    getGroupPolicy_policyName,
    getGroupPolicyResponse_httpStatus,
    getGroupPolicyResponse_groupName,
    getGroupPolicyResponse_policyName,
    getGroupPolicyResponse_policyDocument,

    -- ** GenerateOrganizationsAccessReport
    generateOrganizationsAccessReport_organizationsPolicyId,
    generateOrganizationsAccessReport_entityPath,
    generateOrganizationsAccessReportResponse_jobId,
    generateOrganizationsAccessReportResponse_httpStatus,

    -- ** ChangePassword
    changePassword_oldPassword,
    changePassword_newPassword,

    -- ** ListServerCertificates
    listServerCertificates_pathPrefix,
    listServerCertificates_marker,
    listServerCertificates_maxItems,
    listServerCertificatesResponse_marker,
    listServerCertificatesResponse_isTruncated,
    listServerCertificatesResponse_httpStatus,
    listServerCertificatesResponse_serverCertificateMetadataList,

    -- ** DeleteServiceLinkedRole
    deleteServiceLinkedRole_roleName,
    deleteServiceLinkedRoleResponse_httpStatus,
    deleteServiceLinkedRoleResponse_deletionTaskId,

    -- ** DeletePolicy
    deletePolicy_policyArn,

    -- ** UpdateAssumeRolePolicy
    updateAssumeRolePolicy_roleName,
    updateAssumeRolePolicy_policyDocument,

    -- ** GetServiceLastAccessedDetailsWithEntities
    getServiceLastAccessedDetailsWithEntities_marker,
    getServiceLastAccessedDetailsWithEntities_maxItems,
    getServiceLastAccessedDetailsWithEntities_jobId,
    getServiceLastAccessedDetailsWithEntities_serviceNamespace,
    getServiceLastAccessedDetailsWithEntitiesResponse_error,
    getServiceLastAccessedDetailsWithEntitiesResponse_marker,
    getServiceLastAccessedDetailsWithEntitiesResponse_isTruncated,
    getServiceLastAccessedDetailsWithEntitiesResponse_httpStatus,
    getServiceLastAccessedDetailsWithEntitiesResponse_jobStatus,
    getServiceLastAccessedDetailsWithEntitiesResponse_jobCreationDate,
    getServiceLastAccessedDetailsWithEntitiesResponse_jobCompletionDate,
    getServiceLastAccessedDetailsWithEntitiesResponse_entityDetailsList,

    -- ** UntagServerCertificate
    untagServerCertificate_serverCertificateName,
    untagServerCertificate_tagKeys,

    -- ** GetInstanceProfile
    getInstanceProfile_instanceProfileName,
    getInstanceProfileResponse_httpStatus,
    getInstanceProfileResponse_instanceProfile,

    -- ** CreateLoginProfile
    createLoginProfile_passwordResetRequired,
    createLoginProfile_userName,
    createLoginProfile_password,
    createLoginProfileResponse_httpStatus,
    createLoginProfileResponse_loginProfile,

    -- ** GetSAMLProvider
    getSAMLProvider_sAMLProviderArn,
    getSAMLProviderResponse_createDate,
    getSAMLProviderResponse_validUntil,
    getSAMLProviderResponse_tags,
    getSAMLProviderResponse_sAMLMetadataDocument,
    getSAMLProviderResponse_httpStatus,

    -- ** AddRoleToInstanceProfile
    addRoleToInstanceProfile_instanceProfileName,
    addRoleToInstanceProfile_roleName,

    -- ** ListGroupsForUser
    listGroupsForUser_marker,
    listGroupsForUser_maxItems,
    listGroupsForUser_userName,
    listGroupsForUserResponse_marker,
    listGroupsForUserResponse_isTruncated,
    listGroupsForUserResponse_httpStatus,
    listGroupsForUserResponse_groups,

    -- ** ListEntitiesForPolicy
    listEntitiesForPolicy_pathPrefix,
    listEntitiesForPolicy_entityFilter,
    listEntitiesForPolicy_marker,
    listEntitiesForPolicy_maxItems,
    listEntitiesForPolicy_policyUsageFilter,
    listEntitiesForPolicy_policyArn,
    listEntitiesForPolicyResponse_policyGroups,
    listEntitiesForPolicyResponse_policyRoles,
    listEntitiesForPolicyResponse_marker,
    listEntitiesForPolicyResponse_policyUsers,
    listEntitiesForPolicyResponse_isTruncated,
    listEntitiesForPolicyResponse_httpStatus,

    -- ** AddUserToGroup
    addUserToGroup_groupName,
    addUserToGroup_userName,

    -- ** TagOpenIDConnectProvider
    tagOpenIDConnectProvider_openIDConnectProviderArn,
    tagOpenIDConnectProvider_tags,

    -- ** SimulatePrincipalPolicy
    simulatePrincipalPolicy_policyInputList,
    simulatePrincipalPolicy_resourcePolicy,
    simulatePrincipalPolicy_callerArn,
    simulatePrincipalPolicy_resourceHandlingOption,
    simulatePrincipalPolicy_resourceArns,
    simulatePrincipalPolicy_permissionsBoundaryPolicyInputList,
    simulatePrincipalPolicy_marker,
    simulatePrincipalPolicy_maxItems,
    simulatePrincipalPolicy_contextEntries,
    simulatePrincipalPolicy_resourceOwner,
    simulatePrincipalPolicy_policySourceArn,
    simulatePrincipalPolicy_actionNames,
    simulatePolicyResponse_evaluationResults,
    simulatePolicyResponse_marker,
    simulatePolicyResponse_isTruncated,

    -- ** GetOrganizationsAccessReport
    getOrganizationsAccessReport_sortKey,
    getOrganizationsAccessReport_marker,
    getOrganizationsAccessReport_maxItems,
    getOrganizationsAccessReport_jobId,
    getOrganizationsAccessReportResponse_numberOfServicesNotAccessed,
    getOrganizationsAccessReportResponse_jobCompletionDate,
    getOrganizationsAccessReportResponse_accessDetails,
    getOrganizationsAccessReportResponse_numberOfServicesAccessible,
    getOrganizationsAccessReportResponse_marker,
    getOrganizationsAccessReportResponse_errorDetails,
    getOrganizationsAccessReportResponse_isTruncated,
    getOrganizationsAccessReportResponse_httpStatus,
    getOrganizationsAccessReportResponse_jobStatus,
    getOrganizationsAccessReportResponse_jobCreationDate,

    -- ** GetPolicyVersion
    getPolicyVersion_policyArn,
    getPolicyVersion_versionId,
    getPolicyVersionResponse_policyVersion,
    getPolicyVersionResponse_httpStatus,

    -- ** CreateServiceLinkedRole
    createServiceLinkedRole_customSuffix,
    createServiceLinkedRole_description,
    createServiceLinkedRole_aWSServiceName,
    createServiceLinkedRoleResponse_role,
    createServiceLinkedRoleResponse_httpStatus,

    -- ** ListServiceSpecificCredentials
    listServiceSpecificCredentials_userName,
    listServiceSpecificCredentials_serviceName,
    listServiceSpecificCredentialsResponse_serviceSpecificCredentials,
    listServiceSpecificCredentialsResponse_httpStatus,

    -- ** DeleteOpenIDConnectProvider
    deleteOpenIDConnectProvider_openIDConnectProviderArn,

    -- ** GetUser
    getUser_userName,
    getUserResponse_httpStatus,
    getUserResponse_user,

    -- ** ListSigningCertificates
    listSigningCertificates_userName,
    listSigningCertificates_marker,
    listSigningCertificates_maxItems,
    listSigningCertificatesResponse_marker,
    listSigningCertificatesResponse_isTruncated,
    listSigningCertificatesResponse_httpStatus,
    listSigningCertificatesResponse_certificates,

    -- ** DeleteSigningCertificate
    deleteSigningCertificate_userName,
    deleteSigningCertificate_certificateId,

    -- ** UpdateSigningCertificate
    updateSigningCertificate_userName,
    updateSigningCertificate_certificateId,
    updateSigningCertificate_status,

    -- ** ListAttachedUserPolicies
    listAttachedUserPolicies_pathPrefix,
    listAttachedUserPolicies_marker,
    listAttachedUserPolicies_maxItems,
    listAttachedUserPolicies_userName,
    listAttachedUserPoliciesResponse_attachedPolicies,
    listAttachedUserPoliciesResponse_marker,
    listAttachedUserPoliciesResponse_isTruncated,
    listAttachedUserPoliciesResponse_httpStatus,

    -- ** RemoveClientIDFromOpenIDConnectProvider
    removeClientIDFromOpenIDConnectProvider_openIDConnectProviderArn,
    removeClientIDFromOpenIDConnectProvider_clientID,

    -- ** AttachUserPolicy
    attachUserPolicy_userName,
    attachUserPolicy_policyArn,

    -- ** TagPolicy
    tagPolicy_policyArn,
    tagPolicy_tags,

    -- ** CreateServiceSpecificCredential
    createServiceSpecificCredential_userName,
    createServiceSpecificCredential_serviceName,
    createServiceSpecificCredentialResponse_serviceSpecificCredential,
    createServiceSpecificCredentialResponse_httpStatus,

    -- ** ListVirtualMFADevices
    listVirtualMFADevices_assignmentStatus,
    listVirtualMFADevices_marker,
    listVirtualMFADevices_maxItems,
    listVirtualMFADevicesResponse_marker,
    listVirtualMFADevicesResponse_isTruncated,
    listVirtualMFADevicesResponse_httpStatus,
    listVirtualMFADevicesResponse_virtualMFADevices,

    -- ** ResyncMFADevice
    resyncMFADevice_userName,
    resyncMFADevice_serialNumber,
    resyncMFADevice_authenticationCode1,
    resyncMFADevice_authenticationCode2,

    -- ** TagServerCertificate
    tagServerCertificate_serverCertificateName,
    tagServerCertificate_tags,

    -- ** DeleteAccessKey
    deleteAccessKey_userName,
    deleteAccessKey_accessKeyId,

    -- ** UpdateAccessKey
    updateAccessKey_userName,
    updateAccessKey_accessKeyId,
    updateAccessKey_status,

    -- ** ListUserTags
    listUserTags_marker,
    listUserTags_maxItems,
    listUserTags_userName,
    listUserTagsResponse_marker,
    listUserTagsResponse_isTruncated,
    listUserTagsResponse_httpStatus,
    listUserTagsResponse_tags,

    -- ** ListAccessKeys
    listAccessKeys_userName,
    listAccessKeys_marker,
    listAccessKeys_maxItems,
    listAccessKeysResponse_marker,
    listAccessKeysResponse_isTruncated,
    listAccessKeysResponse_httpStatus,
    listAccessKeysResponse_accessKeyMetadata,

    -- ** GetRolePolicy
    getRolePolicy_roleName,
    getRolePolicy_policyName,
    getRolePolicyResponse_httpStatus,
    getRolePolicyResponse_roleName,
    getRolePolicyResponse_policyName,
    getRolePolicyResponse_policyDocument,

    -- ** SetSecurityTokenServicePreferences
    setSecurityTokenServicePreferences_globalEndpointTokenVersion,

    -- ** UntagRole
    untagRole_roleName,
    untagRole_tagKeys,

    -- ** CreateUser
    createUser_path,
    createUser_permissionsBoundary,
    createUser_tags,
    createUser_userName,
    createUserResponse_user,
    createUserResponse_httpStatus,

    -- ** PutRolePolicy
    putRolePolicy_roleName,
    putRolePolicy_policyName,
    putRolePolicy_policyDocument,

    -- ** GetContextKeysForCustomPolicy
    getContextKeysForCustomPolicy_policyInputList,
    getContextKeysForPolicyResponse_contextKeyNames,

    -- ** UploadSigningCertificate
    uploadSigningCertificate_userName,
    uploadSigningCertificate_certificateBody,
    uploadSigningCertificateResponse_httpStatus,
    uploadSigningCertificateResponse_certificate,

    -- ** DeleteRolePolicy
    deleteRolePolicy_roleName,
    deleteRolePolicy_policyName,

    -- ** GetAccountPasswordPolicy
    getAccountPasswordPolicyResponse_httpStatus,
    getAccountPasswordPolicyResponse_passwordPolicy,

    -- ** GetAccessKeyLastUsed
    getAccessKeyLastUsed_accessKeyId,
    getAccessKeyLastUsedResponse_userName,
    getAccessKeyLastUsedResponse_accessKeyLastUsed,
    getAccessKeyLastUsedResponse_httpStatus,

    -- ** UpdateUser
    updateUser_newUserName,
    updateUser_newPath,
    updateUser_userName,

    -- ** DeleteUser
    deleteUser_userName,

    -- ** AddClientIDToOpenIDConnectProvider
    addClientIDToOpenIDConnectProvider_openIDConnectProviderArn,
    addClientIDToOpenIDConnectProvider_clientID,

    -- ** ListRolePolicies
    listRolePolicies_marker,
    listRolePolicies_maxItems,
    listRolePolicies_roleName,
    listRolePoliciesResponse_marker,
    listRolePoliciesResponse_isTruncated,
    listRolePoliciesResponse_httpStatus,
    listRolePoliciesResponse_policyNames,

    -- ** CreateAccountAlias
    createAccountAlias_accountAlias,

    -- ** ListPoliciesGrantingServiceAccess
    listPoliciesGrantingServiceAccess_marker,
    listPoliciesGrantingServiceAccess_arn,
    listPoliciesGrantingServiceAccess_serviceNamespaces,
    listPoliciesGrantingServiceAccessResponse_marker,
    listPoliciesGrantingServiceAccessResponse_isTruncated,
    listPoliciesGrantingServiceAccessResponse_httpStatus,
    listPoliciesGrantingServiceAccessResponse_policiesGrantingServiceAccess,

    -- ** ListInstanceProfiles
    listInstanceProfiles_pathPrefix,
    listInstanceProfiles_marker,
    listInstanceProfiles_maxItems,
    listInstanceProfilesResponse_marker,
    listInstanceProfilesResponse_isTruncated,
    listInstanceProfilesResponse_httpStatus,
    listInstanceProfilesResponse_instanceProfiles,

    -- ** EnableMFADevice
    enableMFADevice_userName,
    enableMFADevice_serialNumber,
    enableMFADevice_authenticationCode1,
    enableMFADevice_authenticationCode2,

    -- ** ListAccountAliases
    listAccountAliases_marker,
    listAccountAliases_maxItems,
    listAccountAliasesResponse_marker,
    listAccountAliasesResponse_isTruncated,
    listAccountAliasesResponse_httpStatus,
    listAccountAliasesResponse_accountAliases,

    -- ** DeleteSAMLProvider
    deleteSAMLProvider_sAMLProviderArn,

    -- ** UpdateSAMLProvider
    updateSAMLProvider_sAMLMetadataDocument,
    updateSAMLProvider_sAMLProviderArn,
    updateSAMLProviderResponse_sAMLProviderArn,
    updateSAMLProviderResponse_httpStatus,

    -- ** UntagMFADevice
    untagMFADevice_serialNumber,
    untagMFADevice_tagKeys,

    -- ** CreateGroup
    createGroup_path,
    createGroup_groupName,
    createGroupResponse_httpStatus,
    createGroupResponse_group,

    -- ** ListMFADevices
    listMFADevices_userName,
    listMFADevices_marker,
    listMFADevices_maxItems,
    listMFADevicesResponse_marker,
    listMFADevicesResponse_isTruncated,
    listMFADevicesResponse_httpStatus,
    listMFADevicesResponse_mfaDevices,

    -- ** UntagInstanceProfile
    untagInstanceProfile_instanceProfileName,
    untagInstanceProfile_tagKeys,

    -- ** UploadServerCertificate
    uploadServerCertificate_path,
    uploadServerCertificate_certificateChain,
    uploadServerCertificate_tags,
    uploadServerCertificate_serverCertificateName,
    uploadServerCertificate_certificateBody,
    uploadServerCertificate_privateKey,
    uploadServerCertificateResponse_serverCertificateMetadata,
    uploadServerCertificateResponse_tags,
    uploadServerCertificateResponse_httpStatus,

    -- ** SetDefaultPolicyVersion
    setDefaultPolicyVersion_policyArn,
    setDefaultPolicyVersion_versionId,

    -- ** ListPolicyVersions
    listPolicyVersions_marker,
    listPolicyVersions_maxItems,
    listPolicyVersions_policyArn,
    listPolicyVersionsResponse_versions,
    listPolicyVersionsResponse_marker,
    listPolicyVersionsResponse_isTruncated,
    listPolicyVersionsResponse_httpStatus,

    -- ** UpdateRoleDescription
    updateRoleDescription_roleName,
    updateRoleDescription_description,
    updateRoleDescriptionResponse_role,
    updateRoleDescriptionResponse_httpStatus,

    -- ** ListSAMLProviders
    listSAMLProvidersResponse_sAMLProviderList,
    listSAMLProvidersResponse_httpStatus,

    -- ** GetServiceLastAccessedDetails
    getServiceLastAccessedDetails_marker,
    getServiceLastAccessedDetails_maxItems,
    getServiceLastAccessedDetails_jobId,
    getServiceLastAccessedDetailsResponse_jobType,
    getServiceLastAccessedDetailsResponse_error,
    getServiceLastAccessedDetailsResponse_marker,
    getServiceLastAccessedDetailsResponse_isTruncated,
    getServiceLastAccessedDetailsResponse_httpStatus,
    getServiceLastAccessedDetailsResponse_jobStatus,
    getServiceLastAccessedDetailsResponse_jobCreationDate,
    getServiceLastAccessedDetailsResponse_servicesLastAccessed,
    getServiceLastAccessedDetailsResponse_jobCompletionDate,

    -- ** GetServerCertificate
    getServerCertificate_serverCertificateName,
    getServerCertificateResponse_httpStatus,
    getServerCertificateResponse_serverCertificate,

    -- ** DeleteGroup
    deleteGroup_groupName,

    -- ** UpdateGroup
    updateGroup_newGroupName,
    updateGroup_newPath,
    updateGroup_groupName,

    -- ** ListGroups
    listGroups_pathPrefix,
    listGroups_marker,
    listGroups_maxItems,
    listGroupsResponse_marker,
    listGroupsResponse_isTruncated,
    listGroupsResponse_httpStatus,
    listGroupsResponse_groups,

    -- ** GenerateCredentialReport
    generateCredentialReportResponse_state,
    generateCredentialReportResponse_description,
    generateCredentialReportResponse_httpStatus,

    -- ** GetPolicy
    getPolicy_policyArn,
    getPolicyResponse_policy,
    getPolicyResponse_httpStatus,

    -- ** ListInstanceProfileTags
    listInstanceProfileTags_marker,
    listInstanceProfileTags_maxItems,
    listInstanceProfileTags_instanceProfileName,
    listInstanceProfileTagsResponse_marker,
    listInstanceProfileTagsResponse_isTruncated,
    listInstanceProfileTagsResponse_httpStatus,
    listInstanceProfileTagsResponse_tags,

    -- ** UpdateLoginProfile
    updateLoginProfile_password,
    updateLoginProfile_passwordResetRequired,
    updateLoginProfile_userName,

    -- ** DeleteLoginProfile
    deleteLoginProfile_userName,

    -- ** ListSAMLProviderTags
    listSAMLProviderTags_marker,
    listSAMLProviderTags_maxItems,
    listSAMLProviderTags_sAMLProviderArn,
    listSAMLProviderTagsResponse_marker,
    listSAMLProviderTagsResponse_isTruncated,
    listSAMLProviderTagsResponse_httpStatus,
    listSAMLProviderTagsResponse_tags,

    -- ** GetGroup
    getGroup_marker,
    getGroup_maxItems,
    getGroup_groupName,
    getGroupResponse_marker,
    getGroupResponse_isTruncated,
    getGroupResponse_httpStatus,
    getGroupResponse_group,
    getGroupResponse_users,

    -- ** UntagPolicy
    untagPolicy_policyArn,
    untagPolicy_tagKeys,

    -- ** DeleteServerCertificate
    deleteServerCertificate_serverCertificateName,

    -- ** UpdateServerCertificate
    updateServerCertificate_newServerCertificateName,
    updateServerCertificate_newPath,
    updateServerCertificate_serverCertificateName,

    -- ** ListAttachedGroupPolicies
    listAttachedGroupPolicies_pathPrefix,
    listAttachedGroupPolicies_marker,
    listAttachedGroupPolicies_maxItems,
    listAttachedGroupPolicies_groupName,
    listAttachedGroupPoliciesResponse_attachedPolicies,
    listAttachedGroupPoliciesResponse_marker,
    listAttachedGroupPoliciesResponse_isTruncated,
    listAttachedGroupPoliciesResponse_httpStatus,

    -- ** ListMFADeviceTags
    listMFADeviceTags_marker,
    listMFADeviceTags_maxItems,
    listMFADeviceTags_serialNumber,
    listMFADeviceTagsResponse_marker,
    listMFADeviceTagsResponse_isTruncated,
    listMFADeviceTagsResponse_httpStatus,
    listMFADeviceTagsResponse_tags,

    -- * Types

    -- ** AccessDetail
    accessDetail_entityPath,
    accessDetail_region,
    accessDetail_lastAuthenticatedTime,
    accessDetail_totalAuthenticatedEntities,
    accessDetail_serviceName,
    accessDetail_serviceNamespace,

    -- ** AccessKeyInfo
    accessKeyInfo_createDate,
    accessKeyInfo_userName,
    accessKeyInfo_accessKeyId,
    accessKeyInfo_status,
    accessKeyInfo_secretAccessKey,

    -- ** AccessKeyLastUsed
    accessKeyLastUsed_lastUsedDate,
    accessKeyLastUsed_serviceName,
    accessKeyLastUsed_region,

    -- ** AccessKeyMetadata
    accessKeyMetadata_status,
    accessKeyMetadata_createDate,
    accessKeyMetadata_userName,
    accessKeyMetadata_accessKeyId,

    -- ** AttachedPermissionsBoundary
    attachedPermissionsBoundary_permissionsBoundaryType,
    attachedPermissionsBoundary_permissionsBoundaryArn,

    -- ** AttachedPolicy
    attachedPolicy_policyName,
    attachedPolicy_policyArn,

    -- ** ContextEntry
    contextEntry_contextKeyValues,
    contextEntry_contextKeyName,
    contextEntry_contextKeyType,

    -- ** DeletionTaskFailureReasonType
    deletionTaskFailureReasonType_roleUsageList,
    deletionTaskFailureReasonType_reason,

    -- ** EntityDetails
    entityDetails_lastAuthenticated,
    entityDetails_entityInfo,

    -- ** EntityInfo
    entityInfo_path,
    entityInfo_arn,
    entityInfo_name,
    entityInfo_type,
    entityInfo_id,

    -- ** ErrorDetails
    errorDetails_message,
    errorDetails_code,

    -- ** EvaluationResult
    evaluationResult_matchedStatements,
    evaluationResult_evalDecisionDetails,
    evaluationResult_resourceSpecificResults,
    evaluationResult_evalResourceName,
    evaluationResult_missingContextValues,
    evaluationResult_permissionsBoundaryDecisionDetail,
    evaluationResult_organizationsDecisionDetail,
    evaluationResult_evalActionName,
    evaluationResult_evalDecision,

    -- ** GetContextKeysForPolicyResponse
    getContextKeysForPolicyResponse_contextKeyNames,

    -- ** Group
    group_path,
    group_groupName,
    group_groupId,
    group_arn,
    group_createDate,

    -- ** GroupDetail
    groupDetail_arn,
    groupDetail_path,
    groupDetail_createDate,
    groupDetail_groupId,
    groupDetail_groupPolicyList,
    groupDetail_groupName,
    groupDetail_attachedManagedPolicies,

    -- ** InstanceProfile
    instanceProfile_tags,
    instanceProfile_path,
    instanceProfile_instanceProfileName,
    instanceProfile_instanceProfileId,
    instanceProfile_arn,
    instanceProfile_createDate,
    instanceProfile_roles,

    -- ** ListPoliciesGrantingServiceAccessEntry
    listPoliciesGrantingServiceAccessEntry_serviceNamespace,
    listPoliciesGrantingServiceAccessEntry_policies,

    -- ** LoginProfile
    loginProfile_passwordResetRequired,
    loginProfile_userName,
    loginProfile_createDate,

    -- ** MFADevice
    mfaDevice_userName,
    mfaDevice_serialNumber,
    mfaDevice_enableDate,

    -- ** ManagedPolicyDetail
    managedPolicyDetail_policyName,
    managedPolicyDetail_arn,
    managedPolicyDetail_updateDate,
    managedPolicyDetail_policyId,
    managedPolicyDetail_path,
    managedPolicyDetail_policyVersionList,
    managedPolicyDetail_createDate,
    managedPolicyDetail_isAttachable,
    managedPolicyDetail_permissionsBoundaryUsageCount,
    managedPolicyDetail_defaultVersionId,
    managedPolicyDetail_attachmentCount,
    managedPolicyDetail_description,

    -- ** OpenIDConnectProviderListEntry
    openIDConnectProviderListEntry_arn,

    -- ** OrganizationsDecisionDetail
    organizationsDecisionDetail_allowedByOrganizations,

    -- ** PasswordPolicy
    passwordPolicy_expirePasswords,
    passwordPolicy_minimumPasswordLength,
    passwordPolicy_requireNumbers,
    passwordPolicy_passwordReusePrevention,
    passwordPolicy_requireLowercaseCharacters,
    passwordPolicy_maxPasswordAge,
    passwordPolicy_hardExpiry,
    passwordPolicy_requireSymbols,
    passwordPolicy_requireUppercaseCharacters,
    passwordPolicy_allowUsersToChangePassword,

    -- ** PermissionsBoundaryDecisionDetail
    permissionsBoundaryDecisionDetail_allowedByPermissionsBoundary,

    -- ** Policy
    policy_policyName,
    policy_arn,
    policy_updateDate,
    policy_policyId,
    policy_path,
    policy_createDate,
    policy_isAttachable,
    policy_permissionsBoundaryUsageCount,
    policy_defaultVersionId,
    policy_attachmentCount,
    policy_description,
    policy_tags,

    -- ** PolicyDetail
    policyDetail_policyDocument,
    policyDetail_policyName,

    -- ** PolicyGrantingServiceAccess
    policyGrantingServiceAccess_entityName,
    policyGrantingServiceAccess_entityType,
    policyGrantingServiceAccess_policyArn,
    policyGrantingServiceAccess_policyName,
    policyGrantingServiceAccess_policyType,

    -- ** PolicyGroup
    policyGroup_groupId,
    policyGroup_groupName,

    -- ** PolicyRole
    policyRole_roleName,
    policyRole_roleId,

    -- ** PolicyUser
    policyUser_userName,
    policyUser_userId,

    -- ** PolicyVersion
    policyVersion_versionId,
    policyVersion_createDate,
    policyVersion_document,
    policyVersion_isDefaultVersion,

    -- ** Position
    position_line,
    position_column,

    -- ** ResourceSpecificResult
    resourceSpecificResult_matchedStatements,
    resourceSpecificResult_evalDecisionDetails,
    resourceSpecificResult_missingContextValues,
    resourceSpecificResult_permissionsBoundaryDecisionDetail,
    resourceSpecificResult_evalResourceName,
    resourceSpecificResult_evalResourceDecision,

    -- ** Role
    role_maxSessionDuration,
    role_assumeRolePolicyDocument,
    role_roleLastUsed,
    role_permissionsBoundary,
    role_description,
    role_tags,
    role_path,
    role_roleName,
    role_roleId,
    role_arn,
    role_createDate,

    -- ** RoleDetail
    roleDetail_assumeRolePolicyDocument,
    roleDetail_arn,
    roleDetail_path,
    roleDetail_instanceProfileList,
    roleDetail_createDate,
    roleDetail_roleName,
    roleDetail_roleId,
    roleDetail_roleLastUsed,
    roleDetail_permissionsBoundary,
    roleDetail_rolePolicyList,
    roleDetail_tags,
    roleDetail_attachedManagedPolicies,

    -- ** RoleLastUsed
    roleLastUsed_lastUsedDate,
    roleLastUsed_region,

    -- ** RoleUsageType
    roleUsageType_resources,
    roleUsageType_region,

    -- ** SAMLProviderListEntry
    sAMLProviderListEntry_arn,
    sAMLProviderListEntry_createDate,
    sAMLProviderListEntry_validUntil,

    -- ** SSHPublicKey
    sSHPublicKey_uploadDate,
    sSHPublicKey_userName,
    sSHPublicKey_sSHPublicKeyId,
    sSHPublicKey_fingerprint,
    sSHPublicKey_sSHPublicKeyBody,
    sSHPublicKey_status,

    -- ** SSHPublicKeyMetadata
    sSHPublicKeyMetadata_userName,
    sSHPublicKeyMetadata_sSHPublicKeyId,
    sSHPublicKeyMetadata_status,
    sSHPublicKeyMetadata_uploadDate,

    -- ** ServerCertificate
    serverCertificate_certificateChain,
    serverCertificate_tags,
    serverCertificate_serverCertificateMetadata,
    serverCertificate_certificateBody,

    -- ** ServerCertificateMetadata
    serverCertificateMetadata_uploadDate,
    serverCertificateMetadata_expiration,
    serverCertificateMetadata_path,
    serverCertificateMetadata_serverCertificateName,
    serverCertificateMetadata_serverCertificateId,
    serverCertificateMetadata_arn,

    -- ** ServiceLastAccessed
    serviceLastAccessed_lastAuthenticated,
    serviceLastAccessed_trackedActionsLastAccessed,
    serviceLastAccessed_lastAuthenticatedEntity,
    serviceLastAccessed_lastAuthenticatedRegion,
    serviceLastAccessed_totalAuthenticatedEntities,
    serviceLastAccessed_serviceName,
    serviceLastAccessed_serviceNamespace,

    -- ** ServiceSpecificCredential
    serviceSpecificCredential_createDate,
    serviceSpecificCredential_serviceName,
    serviceSpecificCredential_serviceUserName,
    serviceSpecificCredential_servicePassword,
    serviceSpecificCredential_serviceSpecificCredentialId,
    serviceSpecificCredential_userName,
    serviceSpecificCredential_status,

    -- ** ServiceSpecificCredentialMetadata
    serviceSpecificCredentialMetadata_userName,
    serviceSpecificCredentialMetadata_status,
    serviceSpecificCredentialMetadata_serviceUserName,
    serviceSpecificCredentialMetadata_createDate,
    serviceSpecificCredentialMetadata_serviceSpecificCredentialId,
    serviceSpecificCredentialMetadata_serviceName,

    -- ** SigningCertificate
    signingCertificate_uploadDate,
    signingCertificate_userName,
    signingCertificate_certificateId,
    signingCertificate_certificateBody,
    signingCertificate_status,

    -- ** SimulatePolicyResponse
    simulatePolicyResponse_evaluationResults,
    simulatePolicyResponse_marker,
    simulatePolicyResponse_isTruncated,

    -- ** Statement
    statement_sourcePolicyType,
    statement_sourcePolicyId,
    statement_endPosition,
    statement_startPosition,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TrackedActionLastAccessed
    trackedActionLastAccessed_lastAccessedTime,
    trackedActionLastAccessed_actionName,
    trackedActionLastAccessed_lastAccessedEntity,
    trackedActionLastAccessed_lastAccessedRegion,

    -- ** User
    user_passwordLastUsed,
    user_path,
    user_permissionsBoundary,
    user_tags,
    user_userName,
    user_userId,
    user_arn,
    user_createDate,

    -- ** UserDetail
    userDetail_groupList,
    userDetail_arn,
    userDetail_path,
    userDetail_createDate,
    userDetail_userName,
    userDetail_userId,
    userDetail_permissionsBoundary,
    userDetail_userPolicyList,
    userDetail_tags,
    userDetail_attachedManagedPolicies,

    -- ** VirtualMFADevice
    virtualMFADevice_qRCodePNG,
    virtualMFADevice_base32StringSeed,
    virtualMFADevice_user,
    virtualMFADevice_enableDate,
    virtualMFADevice_tags,
    virtualMFADevice_serialNumber,
  )
where

import Network.AWS.IAM.AddClientIDToOpenIDConnectProvider
import Network.AWS.IAM.AddRoleToInstanceProfile
import Network.AWS.IAM.AddUserToGroup
import Network.AWS.IAM.AttachGroupPolicy
import Network.AWS.IAM.AttachRolePolicy
import Network.AWS.IAM.AttachUserPolicy
import Network.AWS.IAM.ChangePassword
import Network.AWS.IAM.CreateAccessKey
import Network.AWS.IAM.CreateAccountAlias
import Network.AWS.IAM.CreateGroup
import Network.AWS.IAM.CreateInstanceProfile
import Network.AWS.IAM.CreateLoginProfile
import Network.AWS.IAM.CreateOpenIDConnectProvider
import Network.AWS.IAM.CreatePolicy
import Network.AWS.IAM.CreatePolicyVersion
import Network.AWS.IAM.CreateRole
import Network.AWS.IAM.CreateSAMLProvider
import Network.AWS.IAM.CreateServiceLinkedRole
import Network.AWS.IAM.CreateServiceSpecificCredential
import Network.AWS.IAM.CreateUser
import Network.AWS.IAM.CreateVirtualMFADevice
import Network.AWS.IAM.DeactivateMFADevice
import Network.AWS.IAM.DeleteAccessKey
import Network.AWS.IAM.DeleteAccountAlias
import Network.AWS.IAM.DeleteAccountPasswordPolicy
import Network.AWS.IAM.DeleteGroup
import Network.AWS.IAM.DeleteGroupPolicy
import Network.AWS.IAM.DeleteInstanceProfile
import Network.AWS.IAM.DeleteLoginProfile
import Network.AWS.IAM.DeleteOpenIDConnectProvider
import Network.AWS.IAM.DeletePolicy
import Network.AWS.IAM.DeletePolicyVersion
import Network.AWS.IAM.DeleteRole
import Network.AWS.IAM.DeleteRolePermissionsBoundary
import Network.AWS.IAM.DeleteRolePolicy
import Network.AWS.IAM.DeleteSAMLProvider
import Network.AWS.IAM.DeleteSSHPublicKey
import Network.AWS.IAM.DeleteServerCertificate
import Network.AWS.IAM.DeleteServiceLinkedRole
import Network.AWS.IAM.DeleteServiceSpecificCredential
import Network.AWS.IAM.DeleteSigningCertificate
import Network.AWS.IAM.DeleteUser
import Network.AWS.IAM.DeleteUserPermissionsBoundary
import Network.AWS.IAM.DeleteUserPolicy
import Network.AWS.IAM.DeleteVirtualMFADevice
import Network.AWS.IAM.DetachGroupPolicy
import Network.AWS.IAM.DetachRolePolicy
import Network.AWS.IAM.DetachUserPolicy
import Network.AWS.IAM.EnableMFADevice
import Network.AWS.IAM.GenerateCredentialReport
import Network.AWS.IAM.GenerateOrganizationsAccessReport
import Network.AWS.IAM.GenerateServiceLastAccessedDetails
import Network.AWS.IAM.GetAccessKeyLastUsed
import Network.AWS.IAM.GetAccountAuthorizationDetails
import Network.AWS.IAM.GetAccountPasswordPolicy
import Network.AWS.IAM.GetAccountSummary
import Network.AWS.IAM.GetContextKeysForCustomPolicy
import Network.AWS.IAM.GetContextKeysForPrincipalPolicy
import Network.AWS.IAM.GetCredentialReport
import Network.AWS.IAM.GetGroup
import Network.AWS.IAM.GetGroupPolicy
import Network.AWS.IAM.GetInstanceProfile
import Network.AWS.IAM.GetLoginProfile
import Network.AWS.IAM.GetOpenIDConnectProvider
import Network.AWS.IAM.GetOrganizationsAccessReport
import Network.AWS.IAM.GetPolicy
import Network.AWS.IAM.GetPolicyVersion
import Network.AWS.IAM.GetRole
import Network.AWS.IAM.GetRolePolicy
import Network.AWS.IAM.GetSAMLProvider
import Network.AWS.IAM.GetSSHPublicKey
import Network.AWS.IAM.GetServerCertificate
import Network.AWS.IAM.GetServiceLastAccessedDetails
import Network.AWS.IAM.GetServiceLastAccessedDetailsWithEntities
import Network.AWS.IAM.GetServiceLinkedRoleDeletionStatus
import Network.AWS.IAM.GetUser
import Network.AWS.IAM.GetUserPolicy
import Network.AWS.IAM.ListAccessKeys
import Network.AWS.IAM.ListAccountAliases
import Network.AWS.IAM.ListAttachedGroupPolicies
import Network.AWS.IAM.ListAttachedRolePolicies
import Network.AWS.IAM.ListAttachedUserPolicies
import Network.AWS.IAM.ListEntitiesForPolicy
import Network.AWS.IAM.ListGroupPolicies
import Network.AWS.IAM.ListGroups
import Network.AWS.IAM.ListGroupsForUser
import Network.AWS.IAM.ListInstanceProfileTags
import Network.AWS.IAM.ListInstanceProfiles
import Network.AWS.IAM.ListInstanceProfilesForRole
import Network.AWS.IAM.ListMFADeviceTags
import Network.AWS.IAM.ListMFADevices
import Network.AWS.IAM.ListOpenIDConnectProviderTags
import Network.AWS.IAM.ListOpenIDConnectProviders
import Network.AWS.IAM.ListPolicies
import Network.AWS.IAM.ListPoliciesGrantingServiceAccess
import Network.AWS.IAM.ListPolicyTags
import Network.AWS.IAM.ListPolicyVersions
import Network.AWS.IAM.ListRolePolicies
import Network.AWS.IAM.ListRoleTags
import Network.AWS.IAM.ListRoles
import Network.AWS.IAM.ListSAMLProviderTags
import Network.AWS.IAM.ListSAMLProviders
import Network.AWS.IAM.ListSSHPublicKeys
import Network.AWS.IAM.ListServerCertificateTags
import Network.AWS.IAM.ListServerCertificates
import Network.AWS.IAM.ListServiceSpecificCredentials
import Network.AWS.IAM.ListSigningCertificates
import Network.AWS.IAM.ListUserPolicies
import Network.AWS.IAM.ListUserTags
import Network.AWS.IAM.ListUsers
import Network.AWS.IAM.ListVirtualMFADevices
import Network.AWS.IAM.PutGroupPolicy
import Network.AWS.IAM.PutRolePermissionsBoundary
import Network.AWS.IAM.PutRolePolicy
import Network.AWS.IAM.PutUserPermissionsBoundary
import Network.AWS.IAM.PutUserPolicy
import Network.AWS.IAM.RemoveClientIDFromOpenIDConnectProvider
import Network.AWS.IAM.RemoveRoleFromInstanceProfile
import Network.AWS.IAM.RemoveUserFromGroup
import Network.AWS.IAM.ResetServiceSpecificCredential
import Network.AWS.IAM.ResyncMFADevice
import Network.AWS.IAM.SetDefaultPolicyVersion
import Network.AWS.IAM.SetSecurityTokenServicePreferences
import Network.AWS.IAM.SimulateCustomPolicy
import Network.AWS.IAM.SimulatePrincipalPolicy
import Network.AWS.IAM.TagInstanceProfile
import Network.AWS.IAM.TagMFADevice
import Network.AWS.IAM.TagOpenIDConnectProvider
import Network.AWS.IAM.TagPolicy
import Network.AWS.IAM.TagRole
import Network.AWS.IAM.TagSAMLProvider
import Network.AWS.IAM.TagServerCertificate
import Network.AWS.IAM.TagUser
import Network.AWS.IAM.Types.AccessDetail
import Network.AWS.IAM.Types.AccessKeyInfo
import Network.AWS.IAM.Types.AccessKeyLastUsed
import Network.AWS.IAM.Types.AccessKeyMetadata
import Network.AWS.IAM.Types.AttachedPermissionsBoundary
import Network.AWS.IAM.Types.AttachedPolicy
import Network.AWS.IAM.Types.ContextEntry
import Network.AWS.IAM.Types.DeletionTaskFailureReasonType
import Network.AWS.IAM.Types.EntityDetails
import Network.AWS.IAM.Types.EntityInfo
import Network.AWS.IAM.Types.ErrorDetails
import Network.AWS.IAM.Types.EvaluationResult
import Network.AWS.IAM.Types.GetContextKeysForPolicyResponse
import Network.AWS.IAM.Types.Group
import Network.AWS.IAM.Types.GroupDetail
import Network.AWS.IAM.Types.InstanceProfile
import Network.AWS.IAM.Types.ListPoliciesGrantingServiceAccessEntry
import Network.AWS.IAM.Types.LoginProfile
import Network.AWS.IAM.Types.MFADevice
import Network.AWS.IAM.Types.ManagedPolicyDetail
import Network.AWS.IAM.Types.OpenIDConnectProviderListEntry
import Network.AWS.IAM.Types.OrganizationsDecisionDetail
import Network.AWS.IAM.Types.PasswordPolicy
import Network.AWS.IAM.Types.PermissionsBoundaryDecisionDetail
import Network.AWS.IAM.Types.Policy
import Network.AWS.IAM.Types.PolicyDetail
import Network.AWS.IAM.Types.PolicyGrantingServiceAccess
import Network.AWS.IAM.Types.PolicyGroup
import Network.AWS.IAM.Types.PolicyRole
import Network.AWS.IAM.Types.PolicyUser
import Network.AWS.IAM.Types.PolicyVersion
import Network.AWS.IAM.Types.Position
import Network.AWS.IAM.Types.ResourceSpecificResult
import Network.AWS.IAM.Types.Role
import Network.AWS.IAM.Types.RoleDetail
import Network.AWS.IAM.Types.RoleLastUsed
import Network.AWS.IAM.Types.RoleUsageType
import Network.AWS.IAM.Types.SAMLProviderListEntry
import Network.AWS.IAM.Types.SSHPublicKey
import Network.AWS.IAM.Types.SSHPublicKeyMetadata
import Network.AWS.IAM.Types.ServerCertificate
import Network.AWS.IAM.Types.ServerCertificateMetadata
import Network.AWS.IAM.Types.ServiceLastAccessed
import Network.AWS.IAM.Types.ServiceSpecificCredential
import Network.AWS.IAM.Types.ServiceSpecificCredentialMetadata
import Network.AWS.IAM.Types.SigningCertificate
import Network.AWS.IAM.Types.SimulatePolicyResponse
import Network.AWS.IAM.Types.Statement
import Network.AWS.IAM.Types.Tag
import Network.AWS.IAM.Types.TrackedActionLastAccessed
import Network.AWS.IAM.Types.User
import Network.AWS.IAM.Types.UserDetail
import Network.AWS.IAM.Types.VirtualMFADevice
import Network.AWS.IAM.UntagInstanceProfile
import Network.AWS.IAM.UntagMFADevice
import Network.AWS.IAM.UntagOpenIDConnectProvider
import Network.AWS.IAM.UntagPolicy
import Network.AWS.IAM.UntagRole
import Network.AWS.IAM.UntagSAMLProvider
import Network.AWS.IAM.UntagServerCertificate
import Network.AWS.IAM.UntagUser
import Network.AWS.IAM.UpdateAccessKey
import Network.AWS.IAM.UpdateAccountPasswordPolicy
import Network.AWS.IAM.UpdateAssumeRolePolicy
import Network.AWS.IAM.UpdateGroup
import Network.AWS.IAM.UpdateLoginProfile
import Network.AWS.IAM.UpdateOpenIDConnectProviderThumbprint
import Network.AWS.IAM.UpdateRole
import Network.AWS.IAM.UpdateRoleDescription
import Network.AWS.IAM.UpdateSAMLProvider
import Network.AWS.IAM.UpdateSSHPublicKey
import Network.AWS.IAM.UpdateServerCertificate
import Network.AWS.IAM.UpdateServiceSpecificCredential
import Network.AWS.IAM.UpdateSigningCertificate
import Network.AWS.IAM.UpdateUser
import Network.AWS.IAM.UploadSSHPublicKey
import Network.AWS.IAM.UploadServerCertificate
import Network.AWS.IAM.UploadSigningCertificate
