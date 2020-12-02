{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Identity and Access Management__
--
-- AWS Identity and Access Management (IAM) is a web service for securely controlling access to AWS services. With IAM, you can centrally manage users, security credentials such as access keys, and permissions that control which AWS resources users and applications can access. For more information about IAM, see <http://aws.amazon.com/iam/ AWS Identity and Access Management (IAM)> and the <https://docs.aws.amazon.com/IAM/latest/UserGuide/ AWS Identity and Access Management User Guide> .
module Network.AWS.IAM
  ( -- * Service Configuration
    iam,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- ** InstanceProfileExists
    instanceProfileExists,

    -- ** UserExists
    userExists,

    -- ** RoleExists
    roleExists,

    -- ** PolicyExists
    policyExists,

    -- * Operations
    -- $operations

    -- ** GetContextKeysForPrincipalPolicy
    module Network.AWS.IAM.GetContextKeysForPrincipalPolicy,

    -- ** ListPolicies (Paginated)
    module Network.AWS.IAM.ListPolicies,

    -- ** CreatePolicy
    module Network.AWS.IAM.CreatePolicy,

    -- ** ListInstanceProfilesForRole (Paginated)
    module Network.AWS.IAM.ListInstanceProfilesForRole,

    -- ** AttachGroupPolicy
    module Network.AWS.IAM.AttachGroupPolicy,

    -- ** CreateAccessKey
    module Network.AWS.IAM.CreateAccessKey,

    -- ** ListRoleTags
    module Network.AWS.IAM.ListRoleTags,

    -- ** ListSSHPublicKeys (Paginated)
    module Network.AWS.IAM.ListSSHPublicKeys,

    -- ** ListOpenIdConnectProviders
    module Network.AWS.IAM.ListOpenIdConnectProviders,

    -- ** CreateVirtualMFADevice
    module Network.AWS.IAM.CreateVirtualMFADevice,

    -- ** DeleteAccountPasswordPolicy
    module Network.AWS.IAM.DeleteAccountPasswordPolicy,

    -- ** UpdateAccountPasswordPolicy
    module Network.AWS.IAM.UpdateAccountPasswordPolicy,

    -- ** AttachRolePolicy
    module Network.AWS.IAM.AttachRolePolicy,

    -- ** UpdateSSHPublicKey
    module Network.AWS.IAM.UpdateSSHPublicKey,

    -- ** DeleteSSHPublicKey
    module Network.AWS.IAM.DeleteSSHPublicKey,

    -- ** GetUserPolicy
    module Network.AWS.IAM.GetUserPolicy,

    -- ** UpdateServiceSpecificCredential
    module Network.AWS.IAM.UpdateServiceSpecificCredential,

    -- ** DeleteServiceSpecificCredential
    module Network.AWS.IAM.DeleteServiceSpecificCredential,

    -- ** ListAttachedRolePolicies (Paginated)
    module Network.AWS.IAM.ListAttachedRolePolicies,

    -- ** GetRole
    module Network.AWS.IAM.GetRole,

    -- ** DeactivateMFADevice
    module Network.AWS.IAM.DeactivateMFADevice,

    -- ** CreateOpenIdConnectProvider
    module Network.AWS.IAM.CreateOpenIdConnectProvider,

    -- ** DeleteVirtualMFADevice
    module Network.AWS.IAM.DeleteVirtualMFADevice,

    -- ** ListRoles (Paginated)
    module Network.AWS.IAM.ListRoles,

    -- ** ListUserPolicies (Paginated)
    module Network.AWS.IAM.ListUserPolicies,

    -- ** PutRolePermissionsBoundary
    module Network.AWS.IAM.PutRolePermissionsBoundary,

    -- ** UploadSSHPublicKey
    module Network.AWS.IAM.UploadSSHPublicKey,

    -- ** DeleteRolePermissionsBoundary
    module Network.AWS.IAM.DeleteRolePermissionsBoundary,

    -- ** SimulateCustomPolicy (Paginated)
    module Network.AWS.IAM.SimulateCustomPolicy,

    -- ** UpdateRole
    module Network.AWS.IAM.UpdateRole,

    -- ** DeleteRole
    module Network.AWS.IAM.DeleteRole,

    -- ** ListUsers (Paginated)
    module Network.AWS.IAM.ListUsers,

    -- ** UpdateOpenIdConnectProviderThumbprint
    module Network.AWS.IAM.UpdateOpenIdConnectProviderThumbprint,

    -- ** PutUserPolicy
    module Network.AWS.IAM.PutUserPolicy,

    -- ** GetSSHPublicKey
    module Network.AWS.IAM.GetSSHPublicKey,

    -- ** UntagUser
    module Network.AWS.IAM.UntagUser,

    -- ** DetachGroupPolicy
    module Network.AWS.IAM.DetachGroupPolicy,

    -- ** GetOpenIdConnectProvider
    module Network.AWS.IAM.GetOpenIdConnectProvider,

    -- ** PutUserPermissionsBoundary
    module Network.AWS.IAM.PutUserPermissionsBoundary,

    -- ** DeleteUserPolicy
    module Network.AWS.IAM.DeleteUserPolicy,

    -- ** DeleteUserPermissionsBoundary
    module Network.AWS.IAM.DeleteUserPermissionsBoundary,

    -- ** CreateRole
    module Network.AWS.IAM.CreateRole,

    -- ** ResetServiceSpecificCredential
    module Network.AWS.IAM.ResetServiceSpecificCredential,

    -- ** GetCredentialReport
    module Network.AWS.IAM.GetCredentialReport,

    -- ** GetAccountSummary
    module Network.AWS.IAM.GetAccountSummary,

    -- ** GenerateServiceLastAccessedDetails
    module Network.AWS.IAM.GenerateServiceLastAccessedDetails,

    -- ** ListGroupPolicies (Paginated)
    module Network.AWS.IAM.ListGroupPolicies,

    -- ** DeletePolicyVersion
    module Network.AWS.IAM.DeletePolicyVersion,

    -- ** TagUser
    module Network.AWS.IAM.TagUser,

    -- ** DeleteInstanceProfile
    module Network.AWS.IAM.DeleteInstanceProfile,

    -- ** DetachRolePolicy
    module Network.AWS.IAM.DetachRolePolicy,

    -- ** RemoveRoleFromInstanceProfile
    module Network.AWS.IAM.RemoveRoleFromInstanceProfile,

    -- ** CreatePolicyVersion
    module Network.AWS.IAM.CreatePolicyVersion,

    -- ** CreateInstanceProfile
    module Network.AWS.IAM.CreateInstanceProfile,

    -- ** CreateSAMLProvider
    module Network.AWS.IAM.CreateSAMLProvider,

    -- ** GetAccountAuthorizationDetails (Paginated)
    module Network.AWS.IAM.GetAccountAuthorizationDetails,

    -- ** GetServiceLinkedRoleDeletionStatus
    module Network.AWS.IAM.GetServiceLinkedRoleDeletionStatus,

    -- ** DeleteAccountAlias
    module Network.AWS.IAM.DeleteAccountAlias,

    -- ** DetachUserPolicy
    module Network.AWS.IAM.DetachUserPolicy,

    -- ** RemoveUserFromGroup
    module Network.AWS.IAM.RemoveUserFromGroup,

    -- ** DeleteGroupPolicy
    module Network.AWS.IAM.DeleteGroupPolicy,

    -- ** TagRole
    module Network.AWS.IAM.TagRole,

    -- ** PutGroupPolicy
    module Network.AWS.IAM.PutGroupPolicy,

    -- ** GetLoginProfile
    module Network.AWS.IAM.GetLoginProfile,

    -- ** GetGroupPolicy
    module Network.AWS.IAM.GetGroupPolicy,

    -- ** GenerateOrganizationsAccessReport
    module Network.AWS.IAM.GenerateOrganizationsAccessReport,

    -- ** ChangePassword
    module Network.AWS.IAM.ChangePassword,

    -- ** ListServerCertificates (Paginated)
    module Network.AWS.IAM.ListServerCertificates,

    -- ** DeleteServiceLinkedRole
    module Network.AWS.IAM.DeleteServiceLinkedRole,

    -- ** DeletePolicy
    module Network.AWS.IAM.DeletePolicy,

    -- ** UpdateAssumeRolePolicy
    module Network.AWS.IAM.UpdateAssumeRolePolicy,

    -- ** GetServiceLastAccessedDetailsWithEntities
    module Network.AWS.IAM.GetServiceLastAccessedDetailsWithEntities,

    -- ** GetInstanceProfile
    module Network.AWS.IAM.GetInstanceProfile,

    -- ** CreateLoginProfile
    module Network.AWS.IAM.CreateLoginProfile,

    -- ** GetSAMLProvider
    module Network.AWS.IAM.GetSAMLProvider,

    -- ** AddRoleToInstanceProfile
    module Network.AWS.IAM.AddRoleToInstanceProfile,

    -- ** ListGroupsForUser (Paginated)
    module Network.AWS.IAM.ListGroupsForUser,

    -- ** ListEntitiesForPolicy (Paginated)
    module Network.AWS.IAM.ListEntitiesForPolicy,

    -- ** AddUserToGroup
    module Network.AWS.IAM.AddUserToGroup,

    -- ** SimulatePrincipalPolicy (Paginated)
    module Network.AWS.IAM.SimulatePrincipalPolicy,

    -- ** GetOrganizationsAccessReport
    module Network.AWS.IAM.GetOrganizationsAccessReport,

    -- ** GetPolicyVersion
    module Network.AWS.IAM.GetPolicyVersion,

    -- ** CreateServiceLinkedRole
    module Network.AWS.IAM.CreateServiceLinkedRole,

    -- ** ListServiceSpecificCredentials
    module Network.AWS.IAM.ListServiceSpecificCredentials,

    -- ** DeleteOpenIdConnectProvider
    module Network.AWS.IAM.DeleteOpenIdConnectProvider,

    -- ** GetUser
    module Network.AWS.IAM.GetUser,

    -- ** ListSigningCertificates (Paginated)
    module Network.AWS.IAM.ListSigningCertificates,

    -- ** DeleteSigningCertificate
    module Network.AWS.IAM.DeleteSigningCertificate,

    -- ** UpdateSigningCertificate
    module Network.AWS.IAM.UpdateSigningCertificate,

    -- ** ListAttachedUserPolicies (Paginated)
    module Network.AWS.IAM.ListAttachedUserPolicies,

    -- ** RemoveClientIdFromOpenIdConnectProvider
    module Network.AWS.IAM.RemoveClientIdFromOpenIdConnectProvider,

    -- ** AttachUserPolicy
    module Network.AWS.IAM.AttachUserPolicy,

    -- ** CreateServiceSpecificCredential
    module Network.AWS.IAM.CreateServiceSpecificCredential,

    -- ** ListVirtualMFADevices (Paginated)
    module Network.AWS.IAM.ListVirtualMFADevices,

    -- ** ResyncMFADevice
    module Network.AWS.IAM.ResyncMFADevice,

    -- ** DeleteAccessKey
    module Network.AWS.IAM.DeleteAccessKey,

    -- ** UpdateAccessKey
    module Network.AWS.IAM.UpdateAccessKey,

    -- ** ListUserTags
    module Network.AWS.IAM.ListUserTags,

    -- ** ListAccessKeys (Paginated)
    module Network.AWS.IAM.ListAccessKeys,

    -- ** GetRolePolicy
    module Network.AWS.IAM.GetRolePolicy,

    -- ** SetSecurityTokenServicePreferences
    module Network.AWS.IAM.SetSecurityTokenServicePreferences,

    -- ** UntagRole
    module Network.AWS.IAM.UntagRole,

    -- ** CreateUser
    module Network.AWS.IAM.CreateUser,

    -- ** PutRolePolicy
    module Network.AWS.IAM.PutRolePolicy,

    -- ** GetContextKeysForCustomPolicy
    module Network.AWS.IAM.GetContextKeysForCustomPolicy,

    -- ** UploadSigningCertificate
    module Network.AWS.IAM.UploadSigningCertificate,

    -- ** DeleteRolePolicy
    module Network.AWS.IAM.DeleteRolePolicy,

    -- ** GetAccountPasswordPolicy
    module Network.AWS.IAM.GetAccountPasswordPolicy,

    -- ** GetAccessKeyLastUsed
    module Network.AWS.IAM.GetAccessKeyLastUsed,

    -- ** UpdateUser
    module Network.AWS.IAM.UpdateUser,

    -- ** DeleteUser
    module Network.AWS.IAM.DeleteUser,

    -- ** AddClientIdToOpenIdConnectProvider
    module Network.AWS.IAM.AddClientIdToOpenIdConnectProvider,

    -- ** ListRolePolicies (Paginated)
    module Network.AWS.IAM.ListRolePolicies,

    -- ** CreateAccountAlias
    module Network.AWS.IAM.CreateAccountAlias,

    -- ** ListPoliciesGrantingServiceAccess
    module Network.AWS.IAM.ListPoliciesGrantingServiceAccess,

    -- ** ListInstanceProfiles (Paginated)
    module Network.AWS.IAM.ListInstanceProfiles,

    -- ** EnableMFADevice
    module Network.AWS.IAM.EnableMFADevice,

    -- ** ListAccountAliases (Paginated)
    module Network.AWS.IAM.ListAccountAliases,

    -- ** DeleteSAMLProvider
    module Network.AWS.IAM.DeleteSAMLProvider,

    -- ** UpdateSAMLProvider
    module Network.AWS.IAM.UpdateSAMLProvider,

    -- ** CreateGroup
    module Network.AWS.IAM.CreateGroup,

    -- ** ListMFADevices (Paginated)
    module Network.AWS.IAM.ListMFADevices,

    -- ** UploadServerCertificate
    module Network.AWS.IAM.UploadServerCertificate,

    -- ** SetDefaultPolicyVersion
    module Network.AWS.IAM.SetDefaultPolicyVersion,

    -- ** ListPolicyVersions (Paginated)
    module Network.AWS.IAM.ListPolicyVersions,

    -- ** UpdateRoleDescription
    module Network.AWS.IAM.UpdateRoleDescription,

    -- ** ListSAMLProviders
    module Network.AWS.IAM.ListSAMLProviders,

    -- ** GetServiceLastAccessedDetails
    module Network.AWS.IAM.GetServiceLastAccessedDetails,

    -- ** GetServerCertificate
    module Network.AWS.IAM.GetServerCertificate,

    -- ** DeleteGroup
    module Network.AWS.IAM.DeleteGroup,

    -- ** UpdateGroup
    module Network.AWS.IAM.UpdateGroup,

    -- ** ListGroups (Paginated)
    module Network.AWS.IAM.ListGroups,

    -- ** GenerateCredentialReport
    module Network.AWS.IAM.GenerateCredentialReport,

    -- ** GetPolicy
    module Network.AWS.IAM.GetPolicy,

    -- ** UpdateLoginProfile
    module Network.AWS.IAM.UpdateLoginProfile,

    -- ** DeleteLoginProfile
    module Network.AWS.IAM.DeleteLoginProfile,

    -- ** GetGroup (Paginated)
    module Network.AWS.IAM.GetGroup,

    -- ** DeleteServerCertificate
    module Network.AWS.IAM.DeleteServerCertificate,

    -- ** UpdateServerCertificate
    module Network.AWS.IAM.UpdateServerCertificate,

    -- ** ListAttachedGroupPolicies (Paginated)
    module Network.AWS.IAM.ListAttachedGroupPolicies,

    -- * Types

    -- ** AccessAdvisorUsageGranularityType
    AccessAdvisorUsageGranularityType (..),

    -- ** AssignmentStatusType
    AssignmentStatusType (..),

    -- ** ContextKeyTypeEnum
    ContextKeyTypeEnum (..),

    -- ** DeletionTaskStatusType
    DeletionTaskStatusType (..),

    -- ** EncodingType
    EncodingType (..),

    -- ** EntityType
    EntityType (..),

    -- ** GlobalEndpointTokenVersion
    GlobalEndpointTokenVersion (..),

    -- ** JobStatusType
    JobStatusType (..),

    -- ** PermissionsBoundaryAttachmentType
    PermissionsBoundaryAttachmentType (..),

    -- ** PolicyEvaluationDecisionType
    PolicyEvaluationDecisionType (..),

    -- ** PolicyOwnerEntityType
    PolicyOwnerEntityType (..),

    -- ** PolicyScopeType
    PolicyScopeType (..),

    -- ** PolicySourceType
    PolicySourceType (..),

    -- ** PolicyType
    PolicyType (..),

    -- ** PolicyUsageType
    PolicyUsageType (..),

    -- ** ReportFormatType
    ReportFormatType (..),

    -- ** ReportStateType
    ReportStateType (..),

    -- ** SortKeyType
    SortKeyType (..),

    -- ** StatusType
    StatusType (..),

    -- ** SummaryKeyType
    SummaryKeyType (..),

    -- ** AccessDetail
    AccessDetail,
    accessDetail,
    adEntityPath,
    adRegion,
    adLastAuthenticatedTime,
    adTotalAuthenticatedEntities,
    adServiceName,
    adServiceNamespace,

    -- ** AccessKeyInfo
    AccessKeyInfo,
    accessKeyInfo,
    akiCreateDate,
    akiUserName,
    akiAccessKeyId,
    akiStatus,
    akiSecretAccessKey,

    -- ** AccessKeyLastUsed
    AccessKeyLastUsed,
    accessKeyLastUsed,
    akluLastUsedDate,
    akluServiceName,
    akluRegion,

    -- ** AccessKeyMetadata
    AccessKeyMetadata,
    accessKeyMetadata,
    akmStatus,
    akmCreateDate,
    akmUserName,
    akmAccessKeyId,

    -- ** AttachedPermissionsBoundary
    AttachedPermissionsBoundary,
    attachedPermissionsBoundary,
    apbPermissionsBoundaryType,
    apbPermissionsBoundaryARN,

    -- ** AttachedPolicy
    AttachedPolicy,
    attachedPolicy,
    apPolicyName,
    apPolicyARN,

    -- ** ContextEntry
    ContextEntry,
    contextEntry,
    ceContextKeyValues,
    ceContextKeyName,
    ceContextKeyType,

    -- ** DeletionTaskFailureReasonType
    DeletionTaskFailureReasonType,
    deletionTaskFailureReasonType,
    dtfrtRoleUsageList,
    dtfrtReason,

    -- ** EntityDetails
    EntityDetails,
    entityDetails,
    edLastAuthenticated,
    edEntityInfo,

    -- ** EntityInfo
    EntityInfo,
    entityInfo,
    eiPath,
    eiARN,
    eiName,
    eiType,
    eiId,

    -- ** ErrorDetails
    ErrorDetails,
    errorDetails,
    edMessage,
    edCode,

    -- ** EvaluationResult
    EvaluationResult,
    evaluationResult,
    erMatchedStatements,
    erEvalDecisionDetails,
    erResourceSpecificResults,
    erEvalResourceName,
    erMissingContextValues,
    erPermissionsBoundaryDecisionDetail,
    erOrganizationsDecisionDetail,
    erEvalActionName,
    erEvalDecision,

    -- ** GetContextKeysForPolicyResponse
    GetContextKeysForPolicyResponse,
    getContextKeysForPolicyResponse,
    gckfpContextKeyNames,

    -- ** Group
    Group,
    group',
    gPath,
    gGroupName,
    gGroupId,
    gARN,
    gCreateDate,

    -- ** GroupDetail
    GroupDetail,
    groupDetail,
    gdARN,
    gdPath,
    gdCreateDate,
    gdGroupId,
    gdGroupPolicyList,
    gdGroupName,
    gdAttachedManagedPolicies,

    -- ** InstanceProfile
    InstanceProfile,
    instanceProfile,
    ipPath,
    ipInstanceProfileName,
    ipInstanceProfileId,
    ipARN,
    ipCreateDate,
    ipRoles,

    -- ** ListPoliciesGrantingServiceAccessEntry
    ListPoliciesGrantingServiceAccessEntry,
    listPoliciesGrantingServiceAccessEntry,
    lpgsaeServiceNamespace,
    lpgsaePolicies,

    -- ** LoginProfile
    LoginProfile,
    loginProfile,
    lpPasswordResetRequired,
    lpUserName,
    lpCreateDate,

    -- ** MFADevice
    MFADevice,
    mfaDevice,
    mdUserName,
    mdSerialNumber,
    mdEnableDate,

    -- ** ManagedPolicyDetail
    ManagedPolicyDetail,
    managedPolicyDetail,
    mpdPolicyName,
    mpdARN,
    mpdUpdateDate,
    mpdPolicyId,
    mpdPath,
    mpdPolicyVersionList,
    mpdCreateDate,
    mpdIsAttachable,
    mpdPermissionsBoundaryUsageCount,
    mpdDefaultVersionId,
    mpdAttachmentCount,
    mpdDescription,

    -- ** OpenIdConnectProviderListEntry
    OpenIdConnectProviderListEntry,
    openIdConnectProviderListEntry,
    oicpleARN,

    -- ** OrganizationsDecisionDetail
    OrganizationsDecisionDetail,
    organizationsDecisionDetail,
    oddAllowedByOrganizations,

    -- ** PasswordPolicy
    PasswordPolicy,
    passwordPolicy,
    ppExpirePasswords,
    ppMinimumPasswordLength,
    ppRequireNumbers,
    ppPasswordReusePrevention,
    ppRequireLowercaseCharacters,
    ppMaxPasswordAge,
    ppHardExpiry,
    ppRequireSymbols,
    ppRequireUppercaseCharacters,
    ppAllowUsersToChangePassword,

    -- ** PermissionsBoundaryDecisionDetail
    PermissionsBoundaryDecisionDetail,
    permissionsBoundaryDecisionDetail,
    pbddAllowedByPermissionsBoundary,

    -- ** Policy
    Policy,
    policy,
    pPolicyName,
    pARN,
    pUpdateDate,
    pPolicyId,
    pPath,
    pCreateDate,
    pIsAttachable,
    pPermissionsBoundaryUsageCount,
    pDefaultVersionId,
    pAttachmentCount,
    pDescription,

    -- ** PolicyDetail
    PolicyDetail,
    policyDetail,
    pdPolicyDocument,
    pdPolicyName,

    -- ** PolicyGrantingServiceAccess
    PolicyGrantingServiceAccess,
    policyGrantingServiceAccess,
    pgsaEntityName,
    pgsaEntityType,
    pgsaPolicyARN,
    pgsaPolicyName,
    pgsaPolicyType,

    -- ** PolicyGroup
    PolicyGroup,
    policyGroup,
    pgGroupId,
    pgGroupName,

    -- ** PolicyRole
    PolicyRole,
    policyRole,
    prRoleName,
    prRoleId,

    -- ** PolicyUser
    PolicyUser,
    policyUser,
    puUserName,
    puUserId,

    -- ** PolicyVersion
    PolicyVersion,
    policyVersion,
    pvVersionId,
    pvCreateDate,
    pvDocument,
    pvIsDefaultVersion,

    -- ** Position
    Position,
    position,
    pLine,
    pColumn,

    -- ** ResourceSpecificResult
    ResourceSpecificResult,
    resourceSpecificResult,
    rsrMatchedStatements,
    rsrEvalDecisionDetails,
    rsrMissingContextValues,
    rsrPermissionsBoundaryDecisionDetail,
    rsrEvalResourceName,
    rsrEvalResourceDecision,

    -- ** Role
    Role,
    role',
    rMaxSessionDuration,
    rAssumeRolePolicyDocument,
    rRoleLastUsed,
    rPermissionsBoundary,
    rDescription,
    rTags,
    rPath,
    rRoleName,
    rRoleId,
    rARN,
    rCreateDate,

    -- ** RoleDetail
    RoleDetail,
    roleDetail,
    rdAssumeRolePolicyDocument,
    rdARN,
    rdPath,
    rdInstanceProfileList,
    rdCreateDate,
    rdRoleName,
    rdRoleId,
    rdRoleLastUsed,
    rdPermissionsBoundary,
    rdRolePolicyList,
    rdTags,
    rdAttachedManagedPolicies,

    -- ** RoleLastUsed
    RoleLastUsed,
    roleLastUsed,
    rluLastUsedDate,
    rluRegion,

    -- ** RoleUsageType
    RoleUsageType,
    roleUsageType,
    rutResources,
    rutRegion,

    -- ** SAMLProviderListEntry
    SAMLProviderListEntry,
    sAMLProviderListEntry,
    samlpleARN,
    samlpleCreateDate,
    samlpleValidUntil,

    -- ** SSHPublicKey
    SSHPublicKey,
    sshPublicKey,
    spkUploadDate,
    spkUserName,
    spkSSHPublicKeyId,
    spkFingerprint,
    spkSSHPublicKeyBody,
    spkStatus,

    -- ** SSHPublicKeyMetadata
    SSHPublicKeyMetadata,
    sshPublicKeyMetadata,
    spkmUserName,
    spkmSSHPublicKeyId,
    spkmStatus,
    spkmUploadDate,

    -- ** ServerCertificate
    ServerCertificate,
    serverCertificate,
    sCertificateChain,
    sServerCertificateMetadata,
    sCertificateBody,

    -- ** ServerCertificateMetadata
    ServerCertificateMetadata,
    serverCertificateMetadata,
    scmUploadDate,
    scmExpiration,
    scmPath,
    scmServerCertificateName,
    scmServerCertificateId,
    scmARN,

    -- ** ServiceLastAccessed
    ServiceLastAccessed,
    serviceLastAccessed,
    slaLastAuthenticated,
    slaTrackedActionsLastAccessed,
    slaLastAuthenticatedEntity,
    slaLastAuthenticatedRegion,
    slaTotalAuthenticatedEntities,
    slaServiceName,
    slaServiceNamespace,

    -- ** ServiceSpecificCredential
    ServiceSpecificCredential,
    serviceSpecificCredential,
    sscCreateDate,
    sscServiceName,
    sscServiceUserName,
    sscServicePassword,
    sscServiceSpecificCredentialId,
    sscUserName,
    sscStatus,

    -- ** ServiceSpecificCredentialMetadata
    ServiceSpecificCredentialMetadata,
    serviceSpecificCredentialMetadata,
    sscmUserName,
    sscmStatus,
    sscmServiceUserName,
    sscmCreateDate,
    sscmServiceSpecificCredentialId,
    sscmServiceName,

    -- ** SigningCertificate
    SigningCertificate,
    signingCertificate,
    scUploadDate,
    scUserName,
    scCertificateId,
    scCertificateBody,
    scStatus,

    -- ** SimulatePolicyResponse
    SimulatePolicyResponse,
    simulatePolicyResponse,
    spEvaluationResults,
    spMarker,
    spIsTruncated,

    -- ** Statement
    Statement,
    statement,
    sSourcePolicyType,
    sSourcePolicyId,
    sEndPosition,
    sStartPosition,

    -- ** Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- ** TrackedActionLastAccessed
    TrackedActionLastAccessed,
    trackedActionLastAccessed,
    talaLastAccessedTime,
    talaActionName,
    talaLastAccessedEntity,
    talaLastAccessedRegion,

    -- ** User
    User,
    user,
    uPasswordLastUsed,
    uPermissionsBoundary,
    uTags,
    uPath,
    uUserName,
    uUserId,
    uARN,
    uCreateDate,

    -- ** UserDetail
    UserDetail,
    userDetail,
    udGroupList,
    udARN,
    udPath,
    udCreateDate,
    udUserName,
    udUserId,
    udPermissionsBoundary,
    udUserPolicyList,
    udTags,
    udAttachedManagedPolicies,

    -- ** VirtualMFADevice
    VirtualMFADevice,
    virtualMFADevice,
    vmdQRCodePNG,
    vmdBase32StringSeed,
    vmdUser,
    vmdEnableDate,
    vmdSerialNumber,
  )
where

import Network.AWS.IAM.AddClientIdToOpenIdConnectProvider
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
import Network.AWS.IAM.CreateOpenIdConnectProvider
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
import Network.AWS.IAM.DeleteOpenIdConnectProvider
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
import Network.AWS.IAM.GetOpenIdConnectProvider
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
import Network.AWS.IAM.ListInstanceProfiles
import Network.AWS.IAM.ListInstanceProfilesForRole
import Network.AWS.IAM.ListMFADevices
import Network.AWS.IAM.ListOpenIdConnectProviders
import Network.AWS.IAM.ListPolicies
import Network.AWS.IAM.ListPoliciesGrantingServiceAccess
import Network.AWS.IAM.ListPolicyVersions
import Network.AWS.IAM.ListRolePolicies
import Network.AWS.IAM.ListRoleTags
import Network.AWS.IAM.ListRoles
import Network.AWS.IAM.ListSAMLProviders
import Network.AWS.IAM.ListSSHPublicKeys
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
import Network.AWS.IAM.RemoveClientIdFromOpenIdConnectProvider
import Network.AWS.IAM.RemoveRoleFromInstanceProfile
import Network.AWS.IAM.RemoveUserFromGroup
import Network.AWS.IAM.ResetServiceSpecificCredential
import Network.AWS.IAM.ResyncMFADevice
import Network.AWS.IAM.SetDefaultPolicyVersion
import Network.AWS.IAM.SetSecurityTokenServicePreferences
import Network.AWS.IAM.SimulateCustomPolicy
import Network.AWS.IAM.SimulatePrincipalPolicy
import Network.AWS.IAM.TagRole
import Network.AWS.IAM.TagUser
import Network.AWS.IAM.Types
import Network.AWS.IAM.UntagRole
import Network.AWS.IAM.UntagUser
import Network.AWS.IAM.UpdateAccessKey
import Network.AWS.IAM.UpdateAccountPasswordPolicy
import Network.AWS.IAM.UpdateAssumeRolePolicy
import Network.AWS.IAM.UpdateGroup
import Network.AWS.IAM.UpdateLoginProfile
import Network.AWS.IAM.UpdateOpenIdConnectProviderThumbprint
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
import Network.AWS.IAM.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IAM'.

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
