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

    -- ** CreateVirtualMFADevice
    createVirtualMFADevice_tags,
    createVirtualMFADevice_path,
    createVirtualMFADevice_virtualMFADeviceName,
    createVirtualMFADeviceResponse_httpStatus,
    createVirtualMFADeviceResponse_virtualMFADevice,

    -- ** AttachRolePolicy
    attachRolePolicy_roleName,
    attachRolePolicy_policyArn,

    -- ** DeleteSSHPublicKey
    deleteSSHPublicKey_userName,
    deleteSSHPublicKey_sSHPublicKeyId,

    -- ** GetUser
    getUser_userName,
    getUserResponse_httpStatus,
    getUserResponse_user,

    -- ** UpdateSSHPublicKey
    updateSSHPublicKey_userName,
    updateSSHPublicKey_sSHPublicKeyId,
    updateSSHPublicKey_status,

    -- ** UntagOpenIDConnectProvider
    untagOpenIDConnectProvider_openIDConnectProviderArn,
    untagOpenIDConnectProvider_tagKeys,

    -- ** ListSigningCertificates
    listSigningCertificates_userName,
    listSigningCertificates_maxItems,
    listSigningCertificates_marker,
    listSigningCertificatesResponse_isTruncated,
    listSigningCertificatesResponse_marker,
    listSigningCertificatesResponse_httpStatus,
    listSigningCertificatesResponse_certificates,

    -- ** DeleteOpenIDConnectProvider
    deleteOpenIDConnectProvider_openIDConnectProviderArn,

    -- ** ListRoleTags
    listRoleTags_maxItems,
    listRoleTags_marker,
    listRoleTags_roleName,
    listRoleTagsResponse_isTruncated,
    listRoleTagsResponse_marker,
    listRoleTagsResponse_httpStatus,
    listRoleTagsResponse_tags,

    -- ** ListOpenIDConnectProviders
    listOpenIDConnectProvidersResponse_openIDConnectProviderList,
    listOpenIDConnectProvidersResponse_httpStatus,

    -- ** CreatePolicy
    createPolicy_tags,
    createPolicy_description,
    createPolicy_path,
    createPolicy_policyName,
    createPolicy_policyDocument,
    createPolicyResponse_policy,
    createPolicyResponse_httpStatus,

    -- ** GetSAMLProvider
    getSAMLProvider_sAMLProviderArn,
    getSAMLProviderResponse_createDate,
    getSAMLProviderResponse_validUntil,
    getSAMLProviderResponse_tags,
    getSAMLProviderResponse_sAMLMetadataDocument,
    getSAMLProviderResponse_httpStatus,

    -- ** GetContextKeysForPrincipalPolicy
    getContextKeysForPrincipalPolicy_policyInputList,
    getContextKeysForPrincipalPolicy_policySourceArn,
    getContextKeysForPolicyResponse_contextKeyNames,

    -- ** ListEntitiesForPolicy
    listEntitiesForPolicy_entityFilter,
    listEntitiesForPolicy_policyUsageFilter,
    listEntitiesForPolicy_pathPrefix,
    listEntitiesForPolicy_maxItems,
    listEntitiesForPolicy_marker,
    listEntitiesForPolicy_policyArn,
    listEntitiesForPolicyResponse_policyRoles,
    listEntitiesForPolicyResponse_isTruncated,
    listEntitiesForPolicyResponse_policyUsers,
    listEntitiesForPolicyResponse_policyGroups,
    listEntitiesForPolicyResponse_marker,
    listEntitiesForPolicyResponse_httpStatus,

    -- ** ListGroupsForUser
    listGroupsForUser_maxItems,
    listGroupsForUser_marker,
    listGroupsForUser_userName,
    listGroupsForUserResponse_isTruncated,
    listGroupsForUserResponse_marker,
    listGroupsForUserResponse_httpStatus,
    listGroupsForUserResponse_groups,

    -- ** SimulatePrincipalPolicy
    simulatePrincipalPolicy_resourceOwner,
    simulatePrincipalPolicy_contextEntries,
    simulatePrincipalPolicy_resourcePolicy,
    simulatePrincipalPolicy_resourceArns,
    simulatePrincipalPolicy_permissionsBoundaryPolicyInputList,
    simulatePrincipalPolicy_policyInputList,
    simulatePrincipalPolicy_resourceHandlingOption,
    simulatePrincipalPolicy_callerArn,
    simulatePrincipalPolicy_maxItems,
    simulatePrincipalPolicy_marker,
    simulatePrincipalPolicy_policySourceArn,
    simulatePrincipalPolicy_actionNames,
    simulatePolicyResponse_isTruncated,
    simulatePolicyResponse_evaluationResults,
    simulatePolicyResponse_marker,

    -- ** ListPolicies
    listPolicies_scope,
    listPolicies_onlyAttached,
    listPolicies_policyUsageFilter,
    listPolicies_pathPrefix,
    listPolicies_maxItems,
    listPolicies_marker,
    listPoliciesResponse_policies,
    listPoliciesResponse_isTruncated,
    listPoliciesResponse_marker,
    listPoliciesResponse_httpStatus,

    -- ** CreateServiceLinkedRole
    createServiceLinkedRole_customSuffix,
    createServiceLinkedRole_description,
    createServiceLinkedRole_aWSServiceName,
    createServiceLinkedRoleResponse_role,
    createServiceLinkedRoleResponse_httpStatus,

    -- ** UntagPolicy
    untagPolicy_policyArn,
    untagPolicy_tagKeys,

    -- ** DeletePolicy
    deletePolicy_policyArn,

    -- ** DeleteServerCertificate
    deleteServerCertificate_serverCertificateName,

    -- ** ListAttachedGroupPolicies
    listAttachedGroupPolicies_pathPrefix,
    listAttachedGroupPolicies_maxItems,
    listAttachedGroupPolicies_marker,
    listAttachedGroupPolicies_groupName,
    listAttachedGroupPoliciesResponse_isTruncated,
    listAttachedGroupPoliciesResponse_attachedPolicies,
    listAttachedGroupPoliciesResponse_marker,
    listAttachedGroupPoliciesResponse_httpStatus,

    -- ** ChangePassword
    changePassword_oldPassword,
    changePassword_newPassword,

    -- ** ListMFADeviceTags
    listMFADeviceTags_maxItems,
    listMFADeviceTags_marker,
    listMFADeviceTags_serialNumber,
    listMFADeviceTagsResponse_isTruncated,
    listMFADeviceTagsResponse_marker,
    listMFADeviceTagsResponse_httpStatus,
    listMFADeviceTagsResponse_tags,

    -- ** UntagServerCertificate
    untagServerCertificate_serverCertificateName,
    untagServerCertificate_tagKeys,

    -- ** UpdateAssumeRolePolicy
    updateAssumeRolePolicy_roleName,
    updateAssumeRolePolicy_policyDocument,

    -- ** GetGroupPolicy
    getGroupPolicy_groupName,
    getGroupPolicy_policyName,
    getGroupPolicyResponse_httpStatus,
    getGroupPolicyResponse_groupName,
    getGroupPolicyResponse_policyName,
    getGroupPolicyResponse_policyDocument,

    -- ** UpdateServerCertificate
    updateServerCertificate_newPath,
    updateServerCertificate_newServerCertificateName,
    updateServerCertificate_serverCertificateName,

    -- ** ListServerCertificates
    listServerCertificates_pathPrefix,
    listServerCertificates_maxItems,
    listServerCertificates_marker,
    listServerCertificatesResponse_isTruncated,
    listServerCertificatesResponse_marker,
    listServerCertificatesResponse_httpStatus,
    listServerCertificatesResponse_serverCertificateMetadataList,

    -- ** ListInstanceProfileTags
    listInstanceProfileTags_maxItems,
    listInstanceProfileTags_marker,
    listInstanceProfileTags_instanceProfileName,
    listInstanceProfileTagsResponse_isTruncated,
    listInstanceProfileTagsResponse_marker,
    listInstanceProfileTagsResponse_httpStatus,
    listInstanceProfileTagsResponse_tags,

    -- ** DeleteGroupPolicy
    deleteGroupPolicy_groupName,
    deleteGroupPolicy_policyName,

    -- ** CreateInstanceProfile
    createInstanceProfile_tags,
    createInstanceProfile_path,
    createInstanceProfile_instanceProfileName,
    createInstanceProfileResponse_httpStatus,
    createInstanceProfileResponse_instanceProfile,

    -- ** ListGroups
    listGroups_pathPrefix,
    listGroups_maxItems,
    listGroups_marker,
    listGroupsResponse_isTruncated,
    listGroupsResponse_marker,
    listGroupsResponse_httpStatus,
    listGroupsResponse_groups,

    -- ** GetLoginProfile
    getLoginProfile_userName,
    getLoginProfileResponse_httpStatus,
    getLoginProfileResponse_loginProfile,

    -- ** TagRole
    tagRole_roleName,
    tagRole_tags,

    -- ** RemoveRoleFromInstanceProfile
    removeRoleFromInstanceProfile_instanceProfileName,
    removeRoleFromInstanceProfile_roleName,

    -- ** GenerateCredentialReport
    generateCredentialReportResponse_state,
    generateCredentialReportResponse_description,
    generateCredentialReportResponse_httpStatus,

    -- ** CreatePolicyVersion
    createPolicyVersion_setAsDefault,
    createPolicyVersion_policyArn,
    createPolicyVersion_policyDocument,
    createPolicyVersionResponse_policyVersion,
    createPolicyVersionResponse_httpStatus,

    -- ** GetServerCertificate
    getServerCertificate_serverCertificateName,
    getServerCertificateResponse_httpStatus,
    getServerCertificateResponse_serverCertificate,

    -- ** RemoveUserFromGroup
    removeUserFromGroup_groupName,
    removeUserFromGroup_userName,

    -- ** SetDefaultPolicyVersion
    setDefaultPolicyVersion_policyArn,
    setDefaultPolicyVersion_versionId,

    -- ** ResetServiceSpecificCredential
    resetServiceSpecificCredential_userName,
    resetServiceSpecificCredential_serviceSpecificCredentialId,
    resetServiceSpecificCredentialResponse_serviceSpecificCredential,
    resetServiceSpecificCredentialResponse_httpStatus,

    -- ** GenerateServiceLastAccessedDetails
    generateServiceLastAccessedDetails_granularity,
    generateServiceLastAccessedDetails_arn,
    generateServiceLastAccessedDetailsResponse_jobId,
    generateServiceLastAccessedDetailsResponse_httpStatus,

    -- ** ListPoliciesGrantingServiceAccess
    listPoliciesGrantingServiceAccess_marker,
    listPoliciesGrantingServiceAccess_arn,
    listPoliciesGrantingServiceAccess_serviceNamespaces,
    listPoliciesGrantingServiceAccessResponse_isTruncated,
    listPoliciesGrantingServiceAccessResponse_marker,
    listPoliciesGrantingServiceAccessResponse_httpStatus,
    listPoliciesGrantingServiceAccessResponse_policiesGrantingServiceAccess,

    -- ** UpdateRoleDescription
    updateRoleDescription_roleName,
    updateRoleDescription_description,
    updateRoleDescriptionResponse_role,
    updateRoleDescriptionResponse_httpStatus,

    -- ** UploadServerCertificate
    uploadServerCertificate_tags,
    uploadServerCertificate_certificateChain,
    uploadServerCertificate_path,
    uploadServerCertificate_serverCertificateName,
    uploadServerCertificate_certificateBody,
    uploadServerCertificate_privateKey,
    uploadServerCertificateResponse_serverCertificateMetadata,
    uploadServerCertificateResponse_tags,
    uploadServerCertificateResponse_httpStatus,

    -- ** DetachRolePolicy
    detachRolePolicy_roleName,
    detachRolePolicy_policyArn,

    -- ** EnableMFADevice
    enableMFADevice_userName,
    enableMFADevice_serialNumber,
    enableMFADevice_authenticationCode1,
    enableMFADevice_authenticationCode2,

    -- ** ListSAMLProviders
    listSAMLProvidersResponse_sAMLProviderList,
    listSAMLProvidersResponse_httpStatus,

    -- ** ListPolicyTags
    listPolicyTags_maxItems,
    listPolicyTags_marker,
    listPolicyTags_policyArn,
    listPolicyTagsResponse_isTruncated,
    listPolicyTagsResponse_marker,
    listPolicyTagsResponse_httpStatus,
    listPolicyTagsResponse_tags,

    -- ** CreateGroup
    createGroup_path,
    createGroup_groupName,
    createGroupResponse_httpStatus,
    createGroupResponse_group,

    -- ** TagMFADevice
    tagMFADevice_serialNumber,
    tagMFADevice_tags,

    -- ** TagInstanceProfile
    tagInstanceProfile_instanceProfileName,
    tagInstanceProfile_tags,

    -- ** GetOpenIDConnectProvider
    getOpenIDConnectProvider_openIDConnectProviderArn,
    getOpenIDConnectProviderResponse_clientIDList,
    getOpenIDConnectProviderResponse_createDate,
    getOpenIDConnectProviderResponse_thumbprintList,
    getOpenIDConnectProviderResponse_tags,
    getOpenIDConnectProviderResponse_url,
    getOpenIDConnectProviderResponse_httpStatus,

    -- ** CreateRole
    createRole_maxSessionDuration,
    createRole_permissionsBoundary,
    createRole_tags,
    createRole_description,
    createRole_path,
    createRole_roleName,
    createRole_assumeRolePolicyDocument,
    createRoleResponse_httpStatus,
    createRoleResponse_role,

    -- ** PutUserPermissionsBoundary
    putUserPermissionsBoundary_userName,
    putUserPermissionsBoundary_permissionsBoundary,

    -- ** DeleteUserPolicy
    deleteUserPolicy_userName,
    deleteUserPolicy_policyName,

    -- ** DeleteRolePermissionsBoundary
    deleteRolePermissionsBoundary_roleName,

    -- ** CreateUser
    createUser_permissionsBoundary,
    createUser_tags,
    createUser_path,
    createUser_userName,
    createUserResponse_user,
    createUserResponse_httpStatus,

    -- ** ListOpenIDConnectProviderTags
    listOpenIDConnectProviderTags_maxItems,
    listOpenIDConnectProviderTags_marker,
    listOpenIDConnectProviderTags_openIDConnectProviderArn,
    listOpenIDConnectProviderTagsResponse_isTruncated,
    listOpenIDConnectProviderTagsResponse_marker,
    listOpenIDConnectProviderTagsResponse_httpStatus,
    listOpenIDConnectProviderTagsResponse_tags,

    -- ** ListRoles
    listRoles_pathPrefix,
    listRoles_maxItems,
    listRoles_marker,
    listRolesResponse_isTruncated,
    listRolesResponse_marker,
    listRolesResponse_httpStatus,
    listRolesResponse_roles,

    -- ** UploadSigningCertificate
    uploadSigningCertificate_userName,
    uploadSigningCertificate_certificateBody,
    uploadSigningCertificateResponse_httpStatus,
    uploadSigningCertificateResponse_certificate,

    -- ** DeleteRolePolicy
    deleteRolePolicy_roleName,
    deleteRolePolicy_policyName,

    -- ** ListAttachedRolePolicies
    listAttachedRolePolicies_pathPrefix,
    listAttachedRolePolicies_maxItems,
    listAttachedRolePolicies_marker,
    listAttachedRolePolicies_roleName,
    listAttachedRolePoliciesResponse_isTruncated,
    listAttachedRolePoliciesResponse_attachedPolicies,
    listAttachedRolePoliciesResponse_marker,
    listAttachedRolePoliciesResponse_httpStatus,

    -- ** GetRolePolicy
    getRolePolicy_roleName,
    getRolePolicy_policyName,
    getRolePolicyResponse_httpStatus,
    getRolePolicyResponse_roleName,
    getRolePolicyResponse_policyName,
    getRolePolicyResponse_policyDocument,

    -- ** DeleteAccessKey
    deleteAccessKey_userName,
    deleteAccessKey_accessKeyId,

    -- ** ListVirtualMFADevices
    listVirtualMFADevices_assignmentStatus,
    listVirtualMFADevices_maxItems,
    listVirtualMFADevices_marker,
    listVirtualMFADevicesResponse_isTruncated,
    listVirtualMFADevicesResponse_marker,
    listVirtualMFADevicesResponse_httpStatus,
    listVirtualMFADevicesResponse_virtualMFADevices,

    -- ** TagPolicy
    tagPolicy_policyArn,
    tagPolicy_tags,

    -- ** RemoveClientIDFromOpenIDConnectProvider
    removeClientIDFromOpenIDConnectProvider_openIDConnectProviderArn,
    removeClientIDFromOpenIDConnectProvider_clientID,

    -- ** DeleteVirtualMFADevice
    deleteVirtualMFADevice_serialNumber,

    -- ** UpdateAccessKey
    updateAccessKey_userName,
    updateAccessKey_accessKeyId,
    updateAccessKey_status,

    -- ** CreateServiceSpecificCredential
    createServiceSpecificCredential_userName,
    createServiceSpecificCredential_serviceName,
    createServiceSpecificCredentialResponse_serviceSpecificCredential,
    createServiceSpecificCredentialResponse_httpStatus,

    -- ** ResyncMFADevice
    resyncMFADevice_userName,
    resyncMFADevice_serialNumber,
    resyncMFADevice_authenticationCode1,
    resyncMFADevice_authenticationCode2,

    -- ** UpdateServiceSpecificCredential
    updateServiceSpecificCredential_userName,
    updateServiceSpecificCredential_serviceSpecificCredentialId,
    updateServiceSpecificCredential_status,

    -- ** GetUserPolicy
    getUserPolicy_userName,
    getUserPolicy_policyName,
    getUserPolicyResponse_httpStatus,
    getUserPolicyResponse_userName,
    getUserPolicyResponse_policyName,
    getUserPolicyResponse_policyDocument,

    -- ** UpdateAccountPasswordPolicy
    updateAccountPasswordPolicy_maxPasswordAge,
    updateAccountPasswordPolicy_requireLowercaseCharacters,
    updateAccountPasswordPolicy_minimumPasswordLength,
    updateAccountPasswordPolicy_passwordReusePrevention,
    updateAccountPasswordPolicy_requireUppercaseCharacters,
    updateAccountPasswordPolicy_allowUsersToChangePassword,
    updateAccountPasswordPolicy_hardExpiry,
    updateAccountPasswordPolicy_requireSymbols,
    updateAccountPasswordPolicy_requireNumbers,

    -- ** ListServiceSpecificCredentials
    listServiceSpecificCredentials_serviceName,
    listServiceSpecificCredentials_userName,
    listServiceSpecificCredentialsResponse_serviceSpecificCredentials,
    listServiceSpecificCredentialsResponse_httpStatus,

    -- ** DeleteSigningCertificate
    deleteSigningCertificate_userName,
    deleteSigningCertificate_certificateId,

    -- ** ListAttachedUserPolicies
    listAttachedUserPolicies_pathPrefix,
    listAttachedUserPolicies_maxItems,
    listAttachedUserPolicies_marker,
    listAttachedUserPolicies_userName,
    listAttachedUserPoliciesResponse_isTruncated,
    listAttachedUserPoliciesResponse_attachedPolicies,
    listAttachedUserPoliciesResponse_marker,
    listAttachedUserPoliciesResponse_httpStatus,

    -- ** UpdateSigningCertificate
    updateSigningCertificate_userName,
    updateSigningCertificate_certificateId,
    updateSigningCertificate_status,

    -- ** ListSSHPublicKeys
    listSSHPublicKeys_userName,
    listSSHPublicKeys_maxItems,
    listSSHPublicKeys_marker,
    listSSHPublicKeysResponse_isTruncated,
    listSSHPublicKeysResponse_sSHPublicKeys,
    listSSHPublicKeysResponse_marker,
    listSSHPublicKeysResponse_httpStatus,

    -- ** DeleteServiceSpecificCredential
    deleteServiceSpecificCredential_userName,
    deleteServiceSpecificCredential_serviceSpecificCredentialId,

    -- ** CreateAccessKey
    createAccessKey_userName,
    createAccessKeyResponse_httpStatus,
    createAccessKeyResponse_accessKey,

    -- ** DeleteAccountPasswordPolicy

    -- ** GetOrganizationsAccessReport
    getOrganizationsAccessReport_sortKey,
    getOrganizationsAccessReport_maxItems,
    getOrganizationsAccessReport_marker,
    getOrganizationsAccessReport_jobId,
    getOrganizationsAccessReportResponse_accessDetails,
    getOrganizationsAccessReportResponse_isTruncated,
    getOrganizationsAccessReportResponse_jobCompletionDate,
    getOrganizationsAccessReportResponse_numberOfServicesNotAccessed,
    getOrganizationsAccessReportResponse_numberOfServicesAccessible,
    getOrganizationsAccessReportResponse_marker,
    getOrganizationsAccessReportResponse_errorDetails,
    getOrganizationsAccessReportResponse_httpStatus,
    getOrganizationsAccessReportResponse_jobStatus,
    getOrganizationsAccessReportResponse_jobCreationDate,

    -- ** ListInstanceProfilesForRole
    listInstanceProfilesForRole_maxItems,
    listInstanceProfilesForRole_marker,
    listInstanceProfilesForRole_roleName,
    listInstanceProfilesForRoleResponse_isTruncated,
    listInstanceProfilesForRoleResponse_marker,
    listInstanceProfilesForRoleResponse_httpStatus,
    listInstanceProfilesForRoleResponse_instanceProfiles,

    -- ** GetPolicyVersion
    getPolicyVersion_policyArn,
    getPolicyVersion_versionId,
    getPolicyVersionResponse_policyVersion,
    getPolicyVersionResponse_httpStatus,

    -- ** CreateLoginProfile
    createLoginProfile_passwordResetRequired,
    createLoginProfile_userName,
    createLoginProfile_password,
    createLoginProfileResponse_httpStatus,
    createLoginProfileResponse_loginProfile,

    -- ** AddRoleToInstanceProfile
    addRoleToInstanceProfile_instanceProfileName,
    addRoleToInstanceProfile_roleName,

    -- ** GetInstanceProfile
    getInstanceProfile_instanceProfileName,
    getInstanceProfileResponse_httpStatus,
    getInstanceProfileResponse_instanceProfile,

    -- ** TagOpenIDConnectProvider
    tagOpenIDConnectProvider_openIDConnectProviderArn,
    tagOpenIDConnectProvider_tags,

    -- ** AddUserToGroup
    addUserToGroup_groupName,
    addUserToGroup_userName,

    -- ** AttachGroupPolicy
    attachGroupPolicy_groupName,
    attachGroupPolicy_policyArn,

    -- ** UpdateLoginProfile
    updateLoginProfile_passwordResetRequired,
    updateLoginProfile_password,
    updateLoginProfile_userName,

    -- ** ListSAMLProviderTags
    listSAMLProviderTags_maxItems,
    listSAMLProviderTags_marker,
    listSAMLProviderTags_sAMLProviderArn,
    listSAMLProviderTagsResponse_isTruncated,
    listSAMLProviderTagsResponse_marker,
    listSAMLProviderTagsResponse_httpStatus,
    listSAMLProviderTagsResponse_tags,

    -- ** GetGroup
    getGroup_maxItems,
    getGroup_marker,
    getGroup_groupName,
    getGroupResponse_isTruncated,
    getGroupResponse_marker,
    getGroupResponse_httpStatus,
    getGroupResponse_group,
    getGroupResponse_users,

    -- ** DeleteLoginProfile
    deleteLoginProfile_userName,

    -- ** DeleteServiceLinkedRole
    deleteServiceLinkedRole_roleName,
    deleteServiceLinkedRoleResponse_httpStatus,
    deleteServiceLinkedRoleResponse_deletionTaskId,

    -- ** GenerateOrganizationsAccessReport
    generateOrganizationsAccessReport_organizationsPolicyId,
    generateOrganizationsAccessReport_entityPath,
    generateOrganizationsAccessReportResponse_jobId,
    generateOrganizationsAccessReportResponse_httpStatus,

    -- ** GetServiceLastAccessedDetailsWithEntities
    getServiceLastAccessedDetailsWithEntities_maxItems,
    getServiceLastAccessedDetailsWithEntities_marker,
    getServiceLastAccessedDetailsWithEntities_jobId,
    getServiceLastAccessedDetailsWithEntities_serviceNamespace,
    getServiceLastAccessedDetailsWithEntitiesResponse_isTruncated,
    getServiceLastAccessedDetailsWithEntitiesResponse_error,
    getServiceLastAccessedDetailsWithEntitiesResponse_marker,
    getServiceLastAccessedDetailsWithEntitiesResponse_httpStatus,
    getServiceLastAccessedDetailsWithEntitiesResponse_jobStatus,
    getServiceLastAccessedDetailsWithEntitiesResponse_jobCreationDate,
    getServiceLastAccessedDetailsWithEntitiesResponse_jobCompletionDate,
    getServiceLastAccessedDetailsWithEntitiesResponse_entityDetailsList,

    -- ** PutGroupPolicy
    putGroupPolicy_groupName,
    putGroupPolicy_policyName,
    putGroupPolicy_policyDocument,

    -- ** GetServiceLastAccessedDetails
    getServiceLastAccessedDetails_maxItems,
    getServiceLastAccessedDetails_marker,
    getServiceLastAccessedDetails_jobId,
    getServiceLastAccessedDetailsResponse_isTruncated,
    getServiceLastAccessedDetailsResponse_jobType,
    getServiceLastAccessedDetailsResponse_error,
    getServiceLastAccessedDetailsResponse_marker,
    getServiceLastAccessedDetailsResponse_httpStatus,
    getServiceLastAccessedDetailsResponse_jobStatus,
    getServiceLastAccessedDetailsResponse_jobCreationDate,
    getServiceLastAccessedDetailsResponse_servicesLastAccessed,
    getServiceLastAccessedDetailsResponse_jobCompletionDate,

    -- ** DeleteAccountAlias
    deleteAccountAlias_accountAlias,

    -- ** CreateSAMLProvider
    createSAMLProvider_tags,
    createSAMLProvider_sAMLMetadataDocument,
    createSAMLProvider_name,
    createSAMLProviderResponse_sAMLProviderArn,
    createSAMLProviderResponse_tags,
    createSAMLProviderResponse_httpStatus,

    -- ** GetPolicy
    getPolicy_policyArn,
    getPolicyResponse_policy,
    getPolicyResponse_httpStatus,

    -- ** DetachUserPolicy
    detachUserPolicy_userName,
    detachUserPolicy_policyArn,

    -- ** UpdateGroup
    updateGroup_newGroupName,
    updateGroup_newPath,
    updateGroup_groupName,

    -- ** DeleteGroup
    deleteGroup_groupName,

    -- ** GetServiceLinkedRoleDeletionStatus
    getServiceLinkedRoleDeletionStatus_deletionTaskId,
    getServiceLinkedRoleDeletionStatusResponse_reason,
    getServiceLinkedRoleDeletionStatusResponse_httpStatus,
    getServiceLinkedRoleDeletionStatusResponse_status,

    -- ** GetAccountAuthorizationDetails
    getAccountAuthorizationDetails_filter,
    getAccountAuthorizationDetails_maxItems,
    getAccountAuthorizationDetails_marker,
    getAccountAuthorizationDetailsResponse_roleDetailList,
    getAccountAuthorizationDetailsResponse_groupDetailList,
    getAccountAuthorizationDetailsResponse_policies,
    getAccountAuthorizationDetailsResponse_isTruncated,
    getAccountAuthorizationDetailsResponse_userDetailList,
    getAccountAuthorizationDetailsResponse_marker,
    getAccountAuthorizationDetailsResponse_httpStatus,

    -- ** ListGroupPolicies
    listGroupPolicies_maxItems,
    listGroupPolicies_marker,
    listGroupPolicies_groupName,
    listGroupPoliciesResponse_isTruncated,
    listGroupPoliciesResponse_marker,
    listGroupPoliciesResponse_httpStatus,
    listGroupPoliciesResponse_policyNames,

    -- ** DeletePolicyVersion
    deletePolicyVersion_policyArn,
    deletePolicyVersion_versionId,

    -- ** DeleteSAMLProvider
    deleteSAMLProvider_sAMLProviderArn,

    -- ** TagUser
    tagUser_userName,
    tagUser_tags,

    -- ** ListInstanceProfiles
    listInstanceProfiles_pathPrefix,
    listInstanceProfiles_maxItems,
    listInstanceProfiles_marker,
    listInstanceProfilesResponse_isTruncated,
    listInstanceProfilesResponse_marker,
    listInstanceProfilesResponse_httpStatus,
    listInstanceProfilesResponse_instanceProfiles,

    -- ** GetCredentialReport
    getCredentialReportResponse_reportFormat,
    getCredentialReportResponse_generatedTime,
    getCredentialReportResponse_content,
    getCredentialReportResponse_httpStatus,

    -- ** ListMFADevices
    listMFADevices_userName,
    listMFADevices_maxItems,
    listMFADevices_marker,
    listMFADevicesResponse_isTruncated,
    listMFADevicesResponse_marker,
    listMFADevicesResponse_httpStatus,
    listMFADevicesResponse_mfaDevices,

    -- ** UpdateSAMLProvider
    updateSAMLProvider_sAMLMetadataDocument,
    updateSAMLProvider_sAMLProviderArn,
    updateSAMLProviderResponse_sAMLProviderArn,
    updateSAMLProviderResponse_httpStatus,

    -- ** UntagInstanceProfile
    untagInstanceProfile_instanceProfileName,
    untagInstanceProfile_tagKeys,

    -- ** CreateAccountAlias
    createAccountAlias_accountAlias,

    -- ** UntagMFADevice
    untagMFADevice_serialNumber,
    untagMFADevice_tagKeys,

    -- ** UntagSAMLProvider
    untagSAMLProvider_sAMLProviderArn,
    untagSAMLProvider_tagKeys,

    -- ** ListAccountAliases
    listAccountAliases_maxItems,
    listAccountAliases_marker,
    listAccountAliasesResponse_isTruncated,
    listAccountAliasesResponse_marker,
    listAccountAliasesResponse_httpStatus,
    listAccountAliasesResponse_accountAliases,

    -- ** ListPolicyVersions
    listPolicyVersions_maxItems,
    listPolicyVersions_marker,
    listPolicyVersions_policyArn,
    listPolicyVersionsResponse_versions,
    listPolicyVersionsResponse_isTruncated,
    listPolicyVersionsResponse_marker,
    listPolicyVersionsResponse_httpStatus,

    -- ** DeleteInstanceProfile
    deleteInstanceProfile_instanceProfileName,

    -- ** GetAccountSummary
    getAccountSummaryResponse_summaryMap,
    getAccountSummaryResponse_httpStatus,

    -- ** ListServerCertificateTags
    listServerCertificateTags_maxItems,
    listServerCertificateTags_marker,
    listServerCertificateTags_serverCertificateName,
    listServerCertificateTagsResponse_isTruncated,
    listServerCertificateTagsResponse_marker,
    listServerCertificateTagsResponse_httpStatus,
    listServerCertificateTagsResponse_tags,

    -- ** GetSSHPublicKey
    getSSHPublicKey_userName,
    getSSHPublicKey_sSHPublicKeyId,
    getSSHPublicKey_encoding,
    getSSHPublicKeyResponse_sSHPublicKey,
    getSSHPublicKeyResponse_httpStatus,

    -- ** UpdateOpenIDConnectProviderThumbprint
    updateOpenIDConnectProviderThumbprint_openIDConnectProviderArn,
    updateOpenIDConnectProviderThumbprint_thumbprintList,

    -- ** GetAccessKeyLastUsed
    getAccessKeyLastUsed_accessKeyId,
    getAccessKeyLastUsedResponse_userName,
    getAccessKeyLastUsedResponse_accessKeyLastUsed,
    getAccessKeyLastUsedResponse_httpStatus,

    -- ** TagSAMLProvider
    tagSAMLProvider_sAMLProviderArn,
    tagSAMLProvider_tags,

    -- ** GetAccountPasswordPolicy
    getAccountPasswordPolicyResponse_httpStatus,
    getAccountPasswordPolicyResponse_passwordPolicy,

    -- ** DeleteUser
    deleteUser_userName,

    -- ** ListUsers
    listUsers_pathPrefix,
    listUsers_maxItems,
    listUsers_marker,
    listUsersResponse_isTruncated,
    listUsersResponse_marker,
    listUsersResponse_httpStatus,
    listUsersResponse_users,

    -- ** UpdateUser
    updateUser_newPath,
    updateUser_newUserName,
    updateUser_userName,

    -- ** ListRolePolicies
    listRolePolicies_maxItems,
    listRolePolicies_marker,
    listRolePolicies_roleName,
    listRolePoliciesResponse_isTruncated,
    listRolePoliciesResponse_marker,
    listRolePoliciesResponse_httpStatus,
    listRolePoliciesResponse_policyNames,

    -- ** AddClientIDToOpenIDConnectProvider
    addClientIDToOpenIDConnectProvider_openIDConnectProviderArn,
    addClientIDToOpenIDConnectProvider_clientID,

    -- ** DeleteUserPermissionsBoundary
    deleteUserPermissionsBoundary_userName,

    -- ** PutUserPolicy
    putUserPolicy_userName,
    putUserPolicy_policyName,
    putUserPolicy_policyDocument,

    -- ** DetachGroupPolicy
    detachGroupPolicy_groupName,
    detachGroupPolicy_policyArn,

    -- ** UntagUser
    untagUser_userName,
    untagUser_tagKeys,

    -- ** GetContextKeysForCustomPolicy
    getContextKeysForCustomPolicy_policyInputList,
    getContextKeysForPolicyResponse_contextKeyNames,

    -- ** PutRolePermissionsBoundary
    putRolePermissionsBoundary_roleName,
    putRolePermissionsBoundary_permissionsBoundary,

    -- ** UntagRole
    untagRole_roleName,
    untagRole_tagKeys,

    -- ** SimulateCustomPolicy
    simulateCustomPolicy_resourceOwner,
    simulateCustomPolicy_contextEntries,
    simulateCustomPolicy_resourcePolicy,
    simulateCustomPolicy_resourceArns,
    simulateCustomPolicy_permissionsBoundaryPolicyInputList,
    simulateCustomPolicy_resourceHandlingOption,
    simulateCustomPolicy_callerArn,
    simulateCustomPolicy_maxItems,
    simulateCustomPolicy_marker,
    simulateCustomPolicy_policyInputList,
    simulateCustomPolicy_actionNames,
    simulatePolicyResponse_isTruncated,
    simulatePolicyResponse_evaluationResults,
    simulatePolicyResponse_marker,

    -- ** UploadSSHPublicKey
    uploadSSHPublicKey_userName,
    uploadSSHPublicKey_sSHPublicKeyBody,
    uploadSSHPublicKeyResponse_sSHPublicKey,
    uploadSSHPublicKeyResponse_httpStatus,

    -- ** DeleteRole
    deleteRole_roleName,

    -- ** ListUserPolicies
    listUserPolicies_maxItems,
    listUserPolicies_marker,
    listUserPolicies_userName,
    listUserPoliciesResponse_isTruncated,
    listUserPoliciesResponse_marker,
    listUserPoliciesResponse_httpStatus,
    listUserPoliciesResponse_policyNames,

    -- ** PutRolePolicy
    putRolePolicy_roleName,
    putRolePolicy_policyName,
    putRolePolicy_policyDocument,

    -- ** UpdateRole
    updateRole_maxSessionDuration,
    updateRole_description,
    updateRole_roleName,
    updateRoleResponse_httpStatus,

    -- ** SetSecurityTokenServicePreferences
    setSecurityTokenServicePreferences_globalEndpointTokenVersion,

    -- ** AttachUserPolicy
    attachUserPolicy_userName,
    attachUserPolicy_policyArn,

    -- ** TagServerCertificate
    tagServerCertificate_serverCertificateName,
    tagServerCertificate_tags,

    -- ** ListAccessKeys
    listAccessKeys_userName,
    listAccessKeys_maxItems,
    listAccessKeys_marker,
    listAccessKeysResponse_isTruncated,
    listAccessKeysResponse_marker,
    listAccessKeysResponse_httpStatus,
    listAccessKeysResponse_accessKeyMetadata,

    -- ** CreateOpenIDConnectProvider
    createOpenIDConnectProvider_clientIDList,
    createOpenIDConnectProvider_tags,
    createOpenIDConnectProvider_url,
    createOpenIDConnectProvider_thumbprintList,
    createOpenIDConnectProviderResponse_tags,
    createOpenIDConnectProviderResponse_openIDConnectProviderArn,
    createOpenIDConnectProviderResponse_httpStatus,

    -- ** DeactivateMFADevice
    deactivateMFADevice_userName,
    deactivateMFADevice_serialNumber,

    -- ** ListUserTags
    listUserTags_maxItems,
    listUserTags_marker,
    listUserTags_userName,
    listUserTagsResponse_isTruncated,
    listUserTagsResponse_marker,
    listUserTagsResponse_httpStatus,
    listUserTagsResponse_tags,

    -- ** GetRole
    getRole_roleName,
    getRoleResponse_httpStatus,
    getRoleResponse_role,

    -- * Types

    -- ** AccessDetail
    accessDetail_totalAuthenticatedEntities,
    accessDetail_entityPath,
    accessDetail_lastAuthenticatedTime,
    accessDetail_region,
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
    accessKeyMetadata_accessKeyId,
    accessKeyMetadata_userName,

    -- ** AttachedPermissionsBoundary
    attachedPermissionsBoundary_permissionsBoundaryArn,
    attachedPermissionsBoundary_permissionsBoundaryType,

    -- ** AttachedPolicy
    attachedPolicy_policyName,
    attachedPolicy_policyArn,

    -- ** ContextEntry
    contextEntry_contextKeyValues,
    contextEntry_contextKeyName,
    contextEntry_contextKeyType,

    -- ** DeletionTaskFailureReasonType
    deletionTaskFailureReasonType_reason,
    deletionTaskFailureReasonType_roleUsageList,

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
    evaluationResult_evalDecisionDetails,
    evaluationResult_permissionsBoundaryDecisionDetail,
    evaluationResult_organizationsDecisionDetail,
    evaluationResult_resourceSpecificResults,
    evaluationResult_matchedStatements,
    evaluationResult_evalResourceName,
    evaluationResult_missingContextValues,
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
    groupDetail_attachedManagedPolicies,
    groupDetail_groupName,
    groupDetail_createDate,
    groupDetail_arn,
    groupDetail_groupId,
    groupDetail_groupPolicyList,
    groupDetail_path,

    -- ** InstanceProfile
    instanceProfile_tags,
    instanceProfile_path,
    instanceProfile_instanceProfileName,
    instanceProfile_instanceProfileId,
    instanceProfile_arn,
    instanceProfile_createDate,
    instanceProfile_roles,

    -- ** ListPoliciesGrantingServiceAccessEntry
    listPoliciesGrantingServiceAccessEntry_policies,
    listPoliciesGrantingServiceAccessEntry_serviceNamespace,

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
    managedPolicyDetail_permissionsBoundaryUsageCount,
    managedPolicyDetail_isAttachable,
    managedPolicyDetail_createDate,
    managedPolicyDetail_arn,
    managedPolicyDetail_attachmentCount,
    managedPolicyDetail_defaultVersionId,
    managedPolicyDetail_description,
    managedPolicyDetail_policyVersionList,
    managedPolicyDetail_path,
    managedPolicyDetail_policyId,
    managedPolicyDetail_updateDate,

    -- ** OpenIDConnectProviderListEntry
    openIDConnectProviderListEntry_arn,

    -- ** OrganizationsDecisionDetail
    organizationsDecisionDetail_allowedByOrganizations,

    -- ** PasswordPolicy
    passwordPolicy_maxPasswordAge,
    passwordPolicy_requireLowercaseCharacters,
    passwordPolicy_minimumPasswordLength,
    passwordPolicy_passwordReusePrevention,
    passwordPolicy_expirePasswords,
    passwordPolicy_requireUppercaseCharacters,
    passwordPolicy_allowUsersToChangePassword,
    passwordPolicy_hardExpiry,
    passwordPolicy_requireSymbols,
    passwordPolicy_requireNumbers,

    -- ** PermissionsBoundaryDecisionDetail
    permissionsBoundaryDecisionDetail_allowedByPermissionsBoundary,

    -- ** Policy
    policy_policyName,
    policy_permissionsBoundaryUsageCount,
    policy_isAttachable,
    policy_createDate,
    policy_arn,
    policy_attachmentCount,
    policy_defaultVersionId,
    policy_tags,
    policy_description,
    policy_path,
    policy_policyId,
    policy_updateDate,

    -- ** PolicyDetail
    policyDetail_policyName,
    policyDetail_policyDocument,

    -- ** PolicyGrantingServiceAccess
    policyGrantingServiceAccess_entityName,
    policyGrantingServiceAccess_entityType,
    policyGrantingServiceAccess_policyArn,
    policyGrantingServiceAccess_policyName,
    policyGrantingServiceAccess_policyType,

    -- ** PolicyGroup
    policyGroup_groupName,
    policyGroup_groupId,

    -- ** PolicyRole
    policyRole_roleId,
    policyRole_roleName,

    -- ** PolicyUser
    policyUser_userId,
    policyUser_userName,

    -- ** PolicyVersion
    policyVersion_createDate,
    policyVersion_versionId,
    policyVersion_document,
    policyVersion_isDefaultVersion,

    -- ** Position
    position_column,
    position_line,

    -- ** ResourceSpecificResult
    resourceSpecificResult_evalDecisionDetails,
    resourceSpecificResult_permissionsBoundaryDecisionDetail,
    resourceSpecificResult_matchedStatements,
    resourceSpecificResult_missingContextValues,
    resourceSpecificResult_evalResourceName,
    resourceSpecificResult_evalResourceDecision,

    -- ** Role
    role_assumeRolePolicyDocument,
    role_maxSessionDuration,
    role_roleLastUsed,
    role_permissionsBoundary,
    role_tags,
    role_description,
    role_path,
    role_roleName,
    role_roleId,
    role_arn,
    role_createDate,

    -- ** RoleDetail
    roleDetail_assumeRolePolicyDocument,
    roleDetail_roleId,
    roleDetail_roleLastUsed,
    roleDetail_attachedManagedPolicies,
    roleDetail_rolePolicyList,
    roleDetail_roleName,
    roleDetail_permissionsBoundary,
    roleDetail_createDate,
    roleDetail_arn,
    roleDetail_tags,
    roleDetail_instanceProfileList,
    roleDetail_path,

    -- ** RoleLastUsed
    roleLastUsed_lastUsedDate,
    roleLastUsed_region,

    -- ** RoleUsageType
    roleUsageType_resources,
    roleUsageType_region,

    -- ** SAMLProviderListEntry
    sAMLProviderListEntry_createDate,
    sAMLProviderListEntry_arn,
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
    serverCertificate_tags,
    serverCertificate_certificateChain,
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
    serviceLastAccessed_totalAuthenticatedEntities,
    serviceLastAccessed_lastAuthenticatedRegion,
    serviceLastAccessed_lastAuthenticatedEntity,
    serviceLastAccessed_trackedActionsLastAccessed,
    serviceLastAccessed_lastAuthenticated,
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
    simulatePolicyResponse_isTruncated,
    simulatePolicyResponse_evaluationResults,
    simulatePolicyResponse_marker,

    -- ** Statement
    statement_startPosition,
    statement_sourcePolicyType,
    statement_endPosition,
    statement_sourcePolicyId,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TrackedActionLastAccessed
    trackedActionLastAccessed_actionName,
    trackedActionLastAccessed_lastAccessedTime,
    trackedActionLastAccessed_lastAccessedRegion,
    trackedActionLastAccessed_lastAccessedEntity,

    -- ** User
    user_permissionsBoundary,
    user_passwordLastUsed,
    user_tags,
    user_path,
    user_userName,
    user_userId,
    user_arn,
    user_createDate,

    -- ** UserDetail
    userDetail_attachedManagedPolicies,
    userDetail_permissionsBoundary,
    userDetail_createDate,
    userDetail_arn,
    userDetail_groupList,
    userDetail_userId,
    userDetail_tags,
    userDetail_userName,
    userDetail_userPolicyList,
    userDetail_path,

    -- ** VirtualMFADevice
    virtualMFADevice_user,
    virtualMFADevice_enableDate,
    virtualMFADevice_qRCodePNG,
    virtualMFADevice_tags,
    virtualMFADevice_base32StringSeed,
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
