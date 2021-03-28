-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _CredentialReportNotPresentException
    , _CredentialReportNotReadyException
    , _MalformedPolicyDocumentException
    , _EntityAlreadyExistsException
    , _MalformedCertificateException
    , _CredentialReportExpiredException
    , _UnmodifiableEntityException
    , _DuplicateCertificateException
    , _DeleteConflictException
    , _NoSuchEntityException
    , _InvalidCertificateException
    , _PolicyNotAttachableException
    , _ServiceNotSupportedException
    , _UnrecognizedPublicKeyEncodingException
    , _ReportGenerationLimitExceededException
    , _InvalidUserTypeException
    , _ServiceFailureException
    , _ConcurrentModificationException
    , _InvalidInputException
    , _InvalidPublicKeyException
    , _InvalidAuthenticationCodeException
    , _EntityTemporarilyUnmodifiableException
    , _DuplicateSSHPublicKeyException
    , _KeyPairMismatchException
    , _PolicyEvaluationException
    , _PasswordPolicyViolationException
    , _LimitExceededException

    -- * ManagedPolicyDetail
    , ManagedPolicyDetail (..)
    , mkManagedPolicyDetail
    , mpdArn
    , mpdAttachmentCount
    , mpdCreateDate
    , mpdDefaultVersionId
    , mpdDescription
    , mpdIsAttachable
    , mpdPath
    , mpdPermissionsBoundaryUsageCount
    , mpdPolicyId
    , mpdPolicyName
    , mpdPolicyVersionList
    , mpdUpdateDate

    -- * VirtualMFADeviceName
    , VirtualMFADeviceName (..)

    -- * ReasonType
    , ReasonType (..)

    -- * PolicyRole
    , PolicyRole (..)
    , mkPolicyRole
    , prRoleId
    , prRoleName

    -- * AssignmentStatusType
    , AssignmentStatusType (..)

    -- * PasswordPolicy
    , PasswordPolicy (..)
    , mkPasswordPolicy
    , ppAllowUsersToChangePassword
    , ppExpirePasswords
    , ppHardExpiry
    , ppMaxPasswordAge
    , ppMinimumPasswordLength
    , ppPasswordReusePrevention
    , ppRequireLowercaseCharacters
    , ppRequireNumbers
    , ppRequireSymbols
    , ppRequireUppercaseCharacters

    -- * Group
    , Group (..)
    , mkGroup
    , gPath
    , gGroupName
    , gGroupId
    , gArn
    , gCreateDate

    -- * ThumbprintType
    , ThumbprintType (..)

    -- * PasswordType
    , PasswordType (..)

    -- * ContextKeyNameType
    , ContextKeyNameType (..)

    -- * PolicyVersionIdType
    , PolicyVersionIdType (..)

    -- * EvaluationResult
    , EvaluationResult (..)
    , mkEvaluationResult
    , erEvalActionName
    , erEvalDecision
    , erEvalDecisionDetails
    , erEvalResourceName
    , erMatchedStatements
    , erMissingContextValues
    , erOrganizationsDecisionDetail
    , erPermissionsBoundaryDecisionDetail
    , erResourceSpecificResults

    -- * TrackedActionLastAccessed
    , TrackedActionLastAccessed (..)
    , mkTrackedActionLastAccessed
    , talaActionName
    , talaLastAccessedEntity
    , talaLastAccessedRegion
    , talaLastAccessedTime

    -- * PolicyGrantingServiceAccess
    , PolicyGrantingServiceAccess (..)
    , mkPolicyGrantingServiceAccess
    , pgsaPolicyName
    , pgsaPolicyType
    , pgsaEntityName
    , pgsaEntityType
    , pgsaPolicyArn

    -- * MarkerType
    , MarkerType (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * AttachedPolicy
    , AttachedPolicy (..)
    , mkAttachedPolicy
    , apPolicyArn
    , apPolicyName

    -- * ServiceLastAccessed
    , ServiceLastAccessed (..)
    , mkServiceLastAccessed
    , slaServiceName
    , slaServiceNamespace
    , slaLastAuthenticated
    , slaLastAuthenticatedEntity
    , slaLastAuthenticatedRegion
    , slaTotalAuthenticatedEntities
    , slaTrackedActionsLastAccessed

    -- * MFADevice
    , MFADevice (..)
    , mkMFADevice
    , mfadUserName
    , mfadSerialNumber
    , mfadEnableDate

    -- * PolicyVersion
    , PolicyVersion (..)
    , mkPolicyVersion
    , pvCreateDate
    , pvDocument
    , pvIsDefaultVersion
    , pvVersionId

    -- * ServiceNameType
    , ServiceNameType (..)

    -- * AttachedPermissionsBoundary
    , AttachedPermissionsBoundary (..)
    , mkAttachedPermissionsBoundary
    , apbPermissionsBoundaryArn
    , apbPermissionsBoundaryType

    -- * InstanceProfile
    , InstanceProfile (..)
    , mkInstanceProfile
    , ipPath
    , ipInstanceProfileName
    , ipInstanceProfileId
    , ipArn
    , ipCreateDate
    , ipRoles

    -- * RoleDescriptionType
    , RoleDescriptionType (..)

    -- * PublicKeyMaterialType
    , PublicKeyMaterialType (..)

    -- * RoleDetail
    , RoleDetail (..)
    , mkRoleDetail
    , rdArn
    , rdAssumeRolePolicyDocument
    , rdAttachedManagedPolicies
    , rdCreateDate
    , rdInstanceProfileList
    , rdPath
    , rdPermissionsBoundary
    , rdRoleId
    , rdRoleLastUsed
    , rdRoleName
    , rdRolePolicyList
    , rdTags

    -- * SimulatePolicyResponse
    , SimulatePolicyResponse (..)
    , mkSimulatePolicyResponse
    , sprEvaluationResults
    , sprIsTruncated
    , sprMarker

    -- * ReportFormatType
    , ReportFormatType (..)

    -- * Statement
    , Statement (..)
    , mkStatement
    , sEndPosition
    , sSourcePolicyId
    , sSourcePolicyType
    , sStartPosition

    -- * InstanceProfileNameType
    , InstanceProfileNameType (..)

    -- * TagKeyType
    , TagKeyType (..)

    -- * AuthenticationCodeType
    , AuthenticationCodeType (..)

    -- * IdType
    , IdType (..)

    -- * OrganizationsPolicyIdType
    , OrganizationsPolicyIdType (..)

    -- * OpenIDConnectProviderUrlType
    , OpenIDConnectProviderUrlType (..)

    -- * CertificateBodyType
    , CertificateBodyType (..)

    -- * ServerCertificateMetadata
    , ServerCertificateMetadata (..)
    , mkServerCertificateMetadata
    , scmPath
    , scmServerCertificateName
    , scmServerCertificateId
    , scmArn
    , scmExpiration
    , scmUploadDate

    -- * DeletionTaskStatusType
    , DeletionTaskStatusType (..)

    -- * PolicyPathType
    , PolicyPathType (..)

    -- * GroupNameType
    , GroupNameType (..)

    -- * CertificateChainType
    , CertificateChainType (..)

    -- * PolicyType
    , PolicyType (..)

    -- * OpenIDConnectProviderListEntry
    , OpenIDConnectProviderListEntry (..)
    , mkOpenIDConnectProviderListEntry
    , oidcpleArn

    -- * LoginProfile
    , LoginProfile (..)
    , mkLoginProfile
    , lpUserName
    , lpCreateDate
    , lpPasswordResetRequired

    -- * EncodingType
    , EncodingType (..)

    -- * SerialNumberType
    , SerialNumberType (..)

    -- * EntityType
    , EntityType (..)

    -- * SSHPublicKeyMetadata
    , SSHPublicKeyMetadata (..)
    , mkSSHPublicKeyMetadata
    , sshpkmUserName
    , sshpkmSSHPublicKeyId
    , sshpkmStatus
    , sshpkmUploadDate

    -- * SummaryKeyType
    , SummaryKeyType (..)

    -- * ContextEntry
    , ContextEntry (..)
    , mkContextEntry
    , ceContextKeyName
    , ceContextKeyType
    , ceContextKeyValues

    -- * EvalDecisionSourceType
    , EvalDecisionSourceType (..)

    -- * JobStatusType
    , JobStatusType (..)

    -- * ContextKeyTypeEnum
    , ContextKeyTypeEnum (..)

    -- * OrganizationsEntityPathType
    , OrganizationsEntityPathType (..)

    -- * SSHPublicKey
    , SSHPublicKey (..)
    , mkSSHPublicKey
    , sshpkUserName
    , sshpkSSHPublicKeyId
    , sshpkFingerprint
    , sshpkSSHPublicKeyBody
    , sshpkStatus
    , sshpkUploadDate

    -- * PolicyEvaluationDecisionType
    , PolicyEvaluationDecisionType (..)

    -- * GroupDetail
    , GroupDetail (..)
    , mkGroupDetail
    , gdArn
    , gdAttachedManagedPolicies
    , gdCreateDate
    , gdGroupId
    , gdGroupName
    , gdGroupPolicyList
    , gdPath

    -- * ReportStateType
    , ReportStateType (..)

    -- * PermissionsBoundaryAttachmentType
    , PermissionsBoundaryAttachmentType (..)

    -- * ResponseMarkerType
    , ResponseMarkerType (..)

    -- * User
    , User (..)
    , mkUser
    , uPath
    , uUserName
    , uUserId
    , uArn
    , uCreateDate
    , uPasswordLastUsed
    , uPermissionsBoundary
    , uTags

    -- * RoleLastUsed
    , RoleLastUsed (..)
    , mkRoleLastUsed
    , rluLastUsedDate
    , rluRegion

    -- * PolicyDetail
    , PolicyDetail (..)
    , mkPolicyDetail
    , pdPolicyDocument
    , pdPolicyName

    -- * SAMLMetadataDocumentType
    , SAMLMetadataDocumentType (..)

    -- * GlobalEndpointTokenVersion
    , GlobalEndpointTokenVersion (..)

    -- * StatusType
    , StatusType (..)

    -- * SAMLProviderListEntry
    , SAMLProviderListEntry (..)
    , mkSAMLProviderListEntry
    , samlpleArn
    , samlpleCreateDate
    , samlpleValidUntil

    -- * ClientIDType
    , ClientIDType (..)

    -- * Role
    , Role (..)
    , mkRole
    , rPath
    , rRoleName
    , rRoleId
    , rArn
    , rCreateDate
    , rAssumeRolePolicyDocument
    , rDescription
    , rMaxSessionDuration
    , rPermissionsBoundary
    , rRoleLastUsed
    , rTags

    -- * PolicyNameType
    , PolicyNameType (..)

    -- * PolicyDocumentType
    , PolicyDocumentType (..)

    -- * AccountAliasType
    , AccountAliasType (..)

    -- * PolicyGroup
    , PolicyGroup (..)
    , mkPolicyGroup
    , pgGroupId
    , pgGroupName

    -- * ServerCertificateNameType
    , ServerCertificateNameType (..)

    -- * RegionNameType
    , RegionNameType (..)

    -- * ListPoliciesGrantingServiceAccessEntry
    , ListPoliciesGrantingServiceAccessEntry (..)
    , mkListPoliciesGrantingServiceAccessEntry
    , lpgsaePolicies
    , lpgsaeServiceNamespace

    -- * PolicyOwnerEntityType
    , PolicyOwnerEntityType (..)

    -- * PolicyScopeType
    , PolicyScopeType (..)

    -- * PrivateKeyType
    , PrivateKeyType (..)

    -- * ErrorDetails
    , ErrorDetails (..)
    , mkErrorDetails
    , edMessage
    , edCode

    -- * ServiceName
    , ServiceName (..)

    -- * ServicePassword
    , ServicePassword (..)

    -- * StringType
    , StringType (..)

    -- * ExistingUserNameType
    , ExistingUserNameType (..)

    -- * GetContextKeysForPolicyResponse
    , GetContextKeysForPolicyResponse (..)
    , mkGetContextKeysForPolicyResponse
    , gckfprContextKeyNames

    -- * ResourceNameType
    , ResourceNameType (..)

    -- * AccessDetail
    , AccessDetail (..)
    , mkAccessDetail
    , adServiceName
    , adServiceNamespace
    , adEntityPath
    , adLastAuthenticatedTime
    , adRegion
    , adTotalAuthenticatedEntities

    -- * PolicySourceType
    , PolicySourceType (..)

    -- * ArnType
    , ArnType (..)

    -- * PathType
    , PathType (..)

    -- * SortKeyType
    , SortKeyType (..)

    -- * EntityInfo
    , EntityInfo (..)
    , mkEntityInfo
    , eiArn
    , eiName
    , eiType
    , eiId
    , eiPath

    -- * DeletionTaskFailureReasonType
    , DeletionTaskFailureReasonType (..)
    , mkDeletionTaskFailureReasonType
    , dtfrtReason
    , dtfrtRoleUsageList

    -- * AccessKeySecretType
    , AccessKeySecretType (..)

    -- * PolicyUsageType
    , PolicyUsageType (..)

    -- * PathPrefixType
    , PathPrefixType (..)

    -- * UserDetail
    , UserDetail (..)
    , mkUserDetail
    , udArn
    , udAttachedManagedPolicies
    , udCreateDate
    , udGroupList
    , udPath
    , udPermissionsBoundary
    , udTags
    , udUserId
    , udUserName
    , udUserPolicyList

    -- * Policy
    , Policy (..)
    , mkPolicy
    , pArn
    , pAttachmentCount
    , pCreateDate
    , pDefaultVersionId
    , pDescription
    , pIsAttachable
    , pPath
    , pPermissionsBoundaryUsageCount
    , pPolicyId
    , pPolicyName
    , pUpdateDate

    -- * CertificateIdType
    , CertificateIdType (..)

    -- * ServerCertificate
    , ServerCertificate (..)
    , mkServerCertificate
    , sServerCertificateMetadata
    , sCertificateBody
    , sCertificateChain

    -- * ContextKeyValueType
    , ContextKeyValueType (..)

    -- * PublicKeyIdType
    , PublicKeyIdType (..)

    -- * ResourceSpecificResult
    , ResourceSpecificResult (..)
    , mkResourceSpecificResult
    , rsrEvalResourceName
    , rsrEvalResourceDecision
    , rsrEvalDecisionDetails
    , rsrMatchedStatements
    , rsrMissingContextValues
    , rsrPermissionsBoundaryDecisionDetail

    -- * ServiceSpecificCredentialMetadata
    , ServiceSpecificCredentialMetadata (..)
    , mkServiceSpecificCredentialMetadata
    , sscmUserName
    , sscmStatus
    , sscmServiceUserName
    , sscmCreateDate
    , sscmServiceSpecificCredentialId
    , sscmServiceName

    -- * AccessKeyInfo
    , AccessKeyInfo (..)
    , mkAccessKeyInfo
    , akiUserName
    , akiAccessKeyId
    , akiStatus
    , akiSecretAccessKey
    , akiCreateDate

    -- * ServiceSpecificCredentialId
    , ServiceSpecificCredentialId (..)

    -- * ServiceUserName
    , ServiceUserName (..)

    -- * RoleNameType
    , RoleNameType (..)

    -- * VirtualMFADevice
    , VirtualMFADevice (..)
    , mkVirtualMFADevice
    , vmfadSerialNumber
    , vmfadBase32StringSeed
    , vmfadEnableDate
    , vmfadQRCodePNG
    , vmfadUser

    -- * SigningCertificate
    , SigningCertificate (..)
    , mkSigningCertificate
    , scUserName
    , scCertificateId
    , scCertificateBody
    , scStatus
    , scUploadDate

    -- * UserNameType
    , UserNameType (..)

    -- * ServiceNamespaceType
    , ServiceNamespaceType (..)

    -- * ServiceSpecificCredential
    , ServiceSpecificCredential (..)
    , mkServiceSpecificCredential
    , sscCreateDate
    , sscServiceName
    , sscServiceUserName
    , sscServicePassword
    , sscServiceSpecificCredentialId
    , sscUserName
    , sscStatus

    -- * AccessKeyLastUsed
    , AccessKeyLastUsed (..)
    , mkAccessKeyLastUsed
    , akluLastUsedDate
    , akluServiceName
    , akluRegion

    -- * AccessKeyMetadata
    , AccessKeyMetadata (..)
    , mkAccessKeyMetadata
    , akmAccessKeyId
    , akmCreateDate
    , akmStatus
    , akmUserName

    -- * AccessAdvisorUsageGranularityType
    , AccessAdvisorUsageGranularityType (..)

    -- * PolicyUser
    , PolicyUser (..)
    , mkPolicyUser
    , puUserId
    , puUserName

    -- * RoleUsageType
    , RoleUsageType (..)
    , mkRoleUsageType
    , rutRegion
    , rutResources

    -- * PermissionsBoundaryDecisionDetail
    , PermissionsBoundaryDecisionDetail (..)
    , mkPermissionsBoundaryDecisionDetail
    , pbddAllowedByPermissionsBoundary

    -- * OrganizationsDecisionDetail
    , OrganizationsDecisionDetail (..)
    , mkOrganizationsDecisionDetail
    , oddAllowedByOrganizations

    -- * Position
    , Position (..)
    , mkPosition
    , pColumn
    , pLine

    -- * EntityDetails
    , EntityDetails (..)
    , mkEntityDetails
    , edEntityInfo
    , edLastAuthenticated

    -- * ActionNameType
    , ActionNameType (..)

    -- * Arn
    , Arn (..)

    -- * DefaultVersionId
    , DefaultVersionId (..)

    -- * Description
    , Description (..)

    -- * Path
    , Path (..)

    -- * PolicyId
    , PolicyId (..)

    -- * PolicyName
    , PolicyName (..)

    -- * UserName
    , UserName (..)

    -- * PolicyDocument
    , PolicyDocument (..)

    -- * Marker
    , Marker (..)

    -- * PathPrefix
    , PathPrefix (..)

    -- * RoleName
    , RoleName (..)

    -- * Message
    , Message (..)

    -- * RoleId
    , RoleId (..)

    -- * CallerArn
    , CallerArn (..)

    -- * ResourceHandlingOption
    , ResourceHandlingOption (..)

    -- * ResourceOwner
    , ResourceOwner (..)

    -- * ResourcePolicy
    , ResourcePolicy (..)

    -- * SSHPublicKeyBody
    , SSHPublicKeyBody (..)

    -- * PermissionsBoundary
    , PermissionsBoundary (..)

    -- * InstanceProfileName
    , InstanceProfileName (..)

    -- * GroupName
    , GroupName (..)

    -- * GroupId
    , GroupId (..)

    -- * PolicyArn
    , PolicyArn (..)

    -- * EvalActionName
    , EvalActionName (..)

    -- * EvalResourceName
    , EvalResourceName (..)

    -- * AccountAlias
    , AccountAlias (..)

    -- * DeletionTaskId
    , DeletionTaskId (..)

    -- * ActionName
    , ActionName (..)

    -- * LastAccessedEntity
    , LastAccessedEntity (..)

    -- * LastAccessedRegion
    , LastAccessedRegion (..)

    -- * SAMLMetadataDocument
    , SAMLMetadataDocument (..)

    -- * Name
    , Name (..)

    -- * EntityName
    , EntityName (..)

    -- * ServiceSpecificCredentialId
    , ServiceSpecificCredentialId (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * ServiceName
    , ServiceName (..)

    -- * ServiceNamespace
    , ServiceNamespace (..)

    -- * LastAuthenticatedEntity
    , LastAuthenticatedEntity (..)

    -- * LastAuthenticatedRegion
    , LastAuthenticatedRegion (..)

    -- * SerialNumber
    , SerialNumber (..)

    -- * Document
    , Document (..)

    -- * PermissionsBoundaryArn
    , PermissionsBoundaryArn (..)

    -- * InstanceProfileId
    , InstanceProfileId (..)

    -- * JobId
    , JobId (..)

    -- * SAMLProviderArn
    , SAMLProviderArn (..)

    -- * PolicySourceArn
    , PolicySourceArn (..)

    -- * AssumeRolePolicyDocument
    , AssumeRolePolicyDocument (..)

    -- * SourcePolicyId
    , SourcePolicyId (..)

    -- * ServerCertificateName
    , ServerCertificateName (..)

    -- * AWSServiceName
    , AWSServiceName (..)

    -- * CustomSuffix
    , CustomSuffix (..)

    -- * EntityPath
    , EntityPath (..)

    -- * AccessKeyId
    , AccessKeyId (..)

    -- * SSHPublicKeyId
    , SSHPublicKeyId (..)

    -- * OpenIDConnectProviderArn
    , OpenIDConnectProviderArn (..)

    -- * ClientID
    , ClientID (..)

    -- * Fingerprint
    , Fingerprint (..)

    -- * CertificateId
    , CertificateId (..)

    -- * Region
    , Region (..)

    -- * NewPath
    , NewPath (..)

    -- * NewUserName
    , NewUserName (..)

    -- * Code
    , Code (..)

    -- * ServiceUserName
    , ServiceUserName (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.IAM.Types.ManagedPolicyDetail
  
import Network.AWS.IAM.Types.VirtualMFADeviceName
  
import Network.AWS.IAM.Types.ReasonType
  
  
import Network.AWS.IAM.Types.PolicyRole
  
  
  
import Network.AWS.IAM.Types.AssignmentStatusType
  
import Network.AWS.IAM.Types.PasswordPolicy
  
import Network.AWS.IAM.Types.Group
  
import Network.AWS.IAM.Types.ThumbprintType
  
import Network.AWS.IAM.Types.PasswordType
  
import Network.AWS.IAM.Types.ContextKeyNameType
  
import Network.AWS.IAM.Types.PolicyVersionIdType
  
import Network.AWS.IAM.Types.EvaluationResult
  
import Network.AWS.IAM.Types.TrackedActionLastAccessed
  
import Network.AWS.IAM.Types.PolicyGrantingServiceAccess
  
import Network.AWS.IAM.Types.MarkerType
  
  
import Network.AWS.IAM.Types.Tag
  
import Network.AWS.IAM.Types.AttachedPolicy
  
import Network.AWS.IAM.Types.ServiceLastAccessed
  
import Network.AWS.IAM.Types.MFADevice
  
import Network.AWS.IAM.Types.PolicyVersion
  
import Network.AWS.IAM.Types.ServiceNameType
  
import Network.AWS.IAM.Types.AttachedPermissionsBoundary
  
import Network.AWS.IAM.Types.InstanceProfile
  
import Network.AWS.IAM.Types.RoleDescriptionType
  
import Network.AWS.IAM.Types.PublicKeyMaterialType
  
  
import Network.AWS.IAM.Types.RoleDetail
  
  
import Network.AWS.IAM.Types.SimulatePolicyResponse
  
import Network.AWS.IAM.Types.ReportFormatType
  
  
import Network.AWS.IAM.Types.Statement
  
import Network.AWS.IAM.Types.InstanceProfileNameType
  
  
import Network.AWS.IAM.Types.TagKeyType
  
import Network.AWS.IAM.Types.AuthenticationCodeType
  
import Network.AWS.IAM.Types.IdType
  
import Network.AWS.IAM.Types.OrganizationsPolicyIdType
  
import Network.AWS.IAM.Types.OpenIDConnectProviderUrlType
  
import Network.AWS.IAM.Types.CertificateBodyType
  
import Network.AWS.IAM.Types.ServerCertificateMetadata
  
import Network.AWS.IAM.Types.DeletionTaskStatusType
  
import Network.AWS.IAM.Types.PolicyPathType
  
import Network.AWS.IAM.Types.GroupNameType
  
import Network.AWS.IAM.Types.CertificateChainType
  
import Network.AWS.IAM.Types.PolicyType
  
import Network.AWS.IAM.Types.OpenIDConnectProviderListEntry
  
import Network.AWS.IAM.Types.LoginProfile
  
  
  
  
import Network.AWS.IAM.Types.EncodingType
  
import Network.AWS.IAM.Types.SerialNumberType
  
import Network.AWS.IAM.Types.EntityType
  
  
import Network.AWS.IAM.Types.SSHPublicKeyMetadata
  
import Network.AWS.IAM.Types.SummaryKeyType
  
import Network.AWS.IAM.Types.ContextEntry
  
import Network.AWS.IAM.Types.EvalDecisionSourceType
  
import Network.AWS.IAM.Types.JobStatusType
  
import Network.AWS.IAM.Types.ContextKeyTypeEnum
  
import Network.AWS.IAM.Types.OrganizationsEntityPathType
  
import Network.AWS.IAM.Types.SSHPublicKey
  
import Network.AWS.IAM.Types.PolicyEvaluationDecisionType
  
import Network.AWS.IAM.Types.GroupDetail
  
import Network.AWS.IAM.Types.ReportStateType
  
import Network.AWS.IAM.Types.PermissionsBoundaryAttachmentType
  
  
  
import Network.AWS.IAM.Types.ResponseMarkerType
  
import Network.AWS.IAM.Types.User
  
import Network.AWS.IAM.Types.RoleLastUsed
  
  
import Network.AWS.IAM.Types.PolicyDetail
  
import Network.AWS.IAM.Types.SAMLMetadataDocumentType
  
import Network.AWS.IAM.Types.GlobalEndpointTokenVersion
  
import Network.AWS.IAM.Types.StatusType
  
import Network.AWS.IAM.Types.SAMLProviderListEntry
  
import Network.AWS.IAM.Types.ClientIDType
  
import Network.AWS.IAM.Types.Role
  
import Network.AWS.IAM.Types.PolicyNameType
  
import Network.AWS.IAM.Types.PolicyDocumentType
  
import Network.AWS.IAM.Types.AccountAliasType
  
import Network.AWS.IAM.Types.PolicyGroup
  
  
import Network.AWS.IAM.Types.ServerCertificateNameType
  
import Network.AWS.IAM.Types.RegionNameType
  
import Network.AWS.IAM.Types.ListPoliciesGrantingServiceAccessEntry
  
  
import Network.AWS.IAM.Types.PolicyOwnerEntityType
  
import Network.AWS.IAM.Types.PolicyScopeType
  
  
import Network.AWS.IAM.Types.PrivateKeyType
  
import Network.AWS.IAM.Types.ErrorDetails
  
import Network.AWS.IAM.Types.ServiceName
  
import Network.AWS.IAM.Types.ServicePassword
  
import Network.AWS.IAM.Types.StringType
  
import Network.AWS.IAM.Types.ExistingUserNameType
  
import Network.AWS.IAM.Types.GetContextKeysForPolicyResponse
  
import Network.AWS.IAM.Types.ResourceNameType
  
import Network.AWS.IAM.Types.AccessDetail
  
import Network.AWS.IAM.Types.PolicySourceType
  
import Network.AWS.IAM.Types.ArnType
  
import Network.AWS.IAM.Types.PathType
  
import Network.AWS.IAM.Types.SortKeyType
  
import Network.AWS.IAM.Types.EntityInfo
  
import Network.AWS.IAM.Types.DeletionTaskFailureReasonType
  
import Network.AWS.IAM.Types.AccessKeySecretType
  
import Network.AWS.IAM.Types.PolicyUsageType
  
import Network.AWS.IAM.Types.PathPrefixType
  
import Network.AWS.IAM.Types.UserDetail
  
import Network.AWS.IAM.Types.Policy
  
  
  
import Network.AWS.IAM.Types.CertificateIdType
  
import Network.AWS.IAM.Types.ServerCertificate
  
import Network.AWS.IAM.Types.ContextKeyValueType
  
import Network.AWS.IAM.Types.PublicKeyIdType
  
  
import Network.AWS.IAM.Types.ResourceSpecificResult
  
  
import Network.AWS.IAM.Types.ServiceSpecificCredentialMetadata
  
import Network.AWS.IAM.Types.AccessKeyInfo
  
import Network.AWS.IAM.Types.ServiceSpecificCredentialId
  
import Network.AWS.IAM.Types.ServiceUserName
  
import Network.AWS.IAM.Types.RoleNameType
  
import Network.AWS.IAM.Types.VirtualMFADevice
  
import Network.AWS.IAM.Types.SigningCertificate
  
import Network.AWS.IAM.Types.UserNameType
  
import Network.AWS.IAM.Types.ServiceNamespaceType
  
  
import Network.AWS.IAM.Types.ServiceSpecificCredential
  
  
import Network.AWS.IAM.Types.AccessKeyLastUsed
  
import Network.AWS.IAM.Types.AccessKeyMetadata
  
import Network.AWS.IAM.Types.AccessAdvisorUsageGranularityType
  
  
import Network.AWS.IAM.Types.PolicyUser
  
import Network.AWS.IAM.Types.RoleUsageType
  
import Network.AWS.IAM.Types.PermissionsBoundaryDecisionDetail
  
import Network.AWS.IAM.Types.OrganizationsDecisionDetail
  
  
import Network.AWS.IAM.Types.Position
  
  
import Network.AWS.IAM.Types.EntityDetails
  
import Network.AWS.IAM.Types.ActionNameType
  
import Network.AWS.IAM.Types.Arn
  
import Network.AWS.IAM.Types.DefaultVersionId
  
import Network.AWS.IAM.Types.Description
  
import Network.AWS.IAM.Types.Path
  
import Network.AWS.IAM.Types.PolicyId
  
import Network.AWS.IAM.Types.PolicyName
  
import Network.AWS.IAM.Types.UserName
  
import Network.AWS.IAM.Types.PolicyDocument
  
import Network.AWS.IAM.Types.Marker
  
import Network.AWS.IAM.Types.PathPrefix
  
import Network.AWS.IAM.Types.RoleName
  
import Network.AWS.IAM.Types.Message
  
import Network.AWS.IAM.Types.RoleId
  
import Network.AWS.IAM.Types.CallerArn
  
import Network.AWS.IAM.Types.ResourceHandlingOption
  
import Network.AWS.IAM.Types.ResourceOwner
  
import Network.AWS.IAM.Types.ResourcePolicy
  
import Network.AWS.IAM.Types.SSHPublicKeyBody
  
import Network.AWS.IAM.Types.PermissionsBoundary
  
import Network.AWS.IAM.Types.InstanceProfileName
  
import Network.AWS.IAM.Types.GroupName
  
import Network.AWS.IAM.Types.GroupId
  
import Network.AWS.IAM.Types.PolicyArn
  
import Network.AWS.IAM.Types.EvalActionName
  
import Network.AWS.IAM.Types.EvalResourceName
  
import Network.AWS.IAM.Types.AccountAlias
  
import Network.AWS.IAM.Types.DeletionTaskId
  
import Network.AWS.IAM.Types.ActionName
  
import Network.AWS.IAM.Types.LastAccessedEntity
  
import Network.AWS.IAM.Types.LastAccessedRegion
  
import Network.AWS.IAM.Types.SAMLMetadataDocument
  
import Network.AWS.IAM.Types.Name
  
import Network.AWS.IAM.Types.EntityName
  
import Network.AWS.IAM.Types.ServiceSpecificCredentialId
  
import Network.AWS.IAM.Types.Key
  
import Network.AWS.IAM.Types.Value
  
import Network.AWS.IAM.Types.ServiceName
  
import Network.AWS.IAM.Types.ServiceNamespace
  
import Network.AWS.IAM.Types.LastAuthenticatedEntity
  
import Network.AWS.IAM.Types.LastAuthenticatedRegion
  
import Network.AWS.IAM.Types.SerialNumber
  
import Network.AWS.IAM.Types.Document
  
import Network.AWS.IAM.Types.PermissionsBoundaryArn
  
import Network.AWS.IAM.Types.InstanceProfileId
  
import Network.AWS.IAM.Types.JobId
  
import Network.AWS.IAM.Types.SAMLProviderArn
  
import Network.AWS.IAM.Types.PolicySourceArn
  
import Network.AWS.IAM.Types.AssumeRolePolicyDocument
  
import Network.AWS.IAM.Types.SourcePolicyId
  
import Network.AWS.IAM.Types.ServerCertificateName
  
import Network.AWS.IAM.Types.AWSServiceName
  
import Network.AWS.IAM.Types.CustomSuffix
  
import Network.AWS.IAM.Types.EntityPath
  
import Network.AWS.IAM.Types.AccessKeyId
  
import Network.AWS.IAM.Types.SSHPublicKeyId
  
import Network.AWS.IAM.Types.OpenIDConnectProviderArn
  
import Network.AWS.IAM.Types.ClientID
  
import Network.AWS.IAM.Types.Fingerprint
  
import Network.AWS.IAM.Types.CertificateId
  
import Network.AWS.IAM.Types.Region
  
import Network.AWS.IAM.Types.NewPath
  
import Network.AWS.IAM.Types.NewUserName
  
import Network.AWS.IAM.Types.Code
  
import Network.AWS.IAM.Types.ServiceUserName
  

-- | API version @2010-05-08@ of the Amazon Identity and Access Management SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "IAM", Core._svcSigner = Sign.v4,
                 Core._svcPrefix = "iam", Core._svcVersion = "2010-05-08",
                 Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseXMLError "IAM",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The request was rejected because the credential report does not exist. To generate a credential report, use 'GenerateCredentialReport' .
_CredentialReportNotPresentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CredentialReportNotPresentException
  = Core._MatchServiceError mkServiceConfig "ReportNotPresent" Core..
      Core.hasStatues 410
{-# INLINEABLE _CredentialReportNotPresentException #-}
{-# DEPRECATED _CredentialReportNotPresentException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because the credential report is still being generated.
_CredentialReportNotReadyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CredentialReportNotReadyException
  = Core._MatchServiceError mkServiceConfig "ReportInProgress" Core..
      Core.hasStatues 404
{-# INLINEABLE _CredentialReportNotReadyException #-}
{-# DEPRECATED _CredentialReportNotReadyException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because the policy document was malformed. The error message describes the specific error.
_MalformedPolicyDocumentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MalformedPolicyDocumentException
  = Core._MatchServiceError mkServiceConfig "MalformedPolicyDocument"
      Core.. Core.hasStatues 400
{-# INLINEABLE _MalformedPolicyDocumentException #-}
{-# DEPRECATED _MalformedPolicyDocumentException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because it attempted to create a resource that already exists.
_EntityAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EntityAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig "EntityAlreadyExists"
      Core.. Core.hasStatues 409
{-# INLINEABLE _EntityAlreadyExistsException #-}
{-# DEPRECATED _EntityAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because the certificate was malformed or expired. The error message describes the specific error.
_MalformedCertificateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MalformedCertificateException
  = Core._MatchServiceError mkServiceConfig "MalformedCertificate"
      Core.. Core.hasStatues 400
{-# INLINEABLE _MalformedCertificateException #-}
{-# DEPRECATED _MalformedCertificateException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because the most recent credential report has expired. To generate a new credential report, use 'GenerateCredentialReport' . For more information about credential report expiration, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Getting Credential Reports> in the /IAM User Guide/ .
_CredentialReportExpiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CredentialReportExpiredException
  = Core._MatchServiceError mkServiceConfig "ReportExpired" Core..
      Core.hasStatues 410
{-# INLINEABLE _CredentialReportExpiredException #-}
{-# DEPRECATED _CredentialReportExpiredException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because only the service that depends on the service-linked role can modify or delete the role on your behalf. The error message includes the name of the service that depends on this service-linked role. You must request the change through that service.
_UnmodifiableEntityException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnmodifiableEntityException
  = Core._MatchServiceError mkServiceConfig "UnmodifiableEntity"
      Core.. Core.hasStatues 400
{-# INLINEABLE _UnmodifiableEntityException #-}
{-# DEPRECATED _UnmodifiableEntityException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because the same certificate is associated with an IAM user in the account.
_DuplicateCertificateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateCertificateException
  = Core._MatchServiceError mkServiceConfig "DuplicateCertificate"
      Core.. Core.hasStatues 409
{-# INLINEABLE _DuplicateCertificateException #-}
{-# DEPRECATED _DuplicateCertificateException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because it attempted to delete a resource that has attached subordinate entities. The error message describes these entities.
_DeleteConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeleteConflictException
  = Core._MatchServiceError mkServiceConfig "DeleteConflict" Core..
      Core.hasStatues 409
{-# INLINEABLE _DeleteConflictException #-}
{-# DEPRECATED _DeleteConflictException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because it referenced a resource entity that does not exist. The error message describes the resource.
_NoSuchEntityException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchEntityException
  = Core._MatchServiceError mkServiceConfig "NoSuchEntity" Core..
      Core.hasStatues 404
{-# INLINEABLE _NoSuchEntityException #-}
{-# DEPRECATED _NoSuchEntityException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because the certificate is invalid.
_InvalidCertificateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCertificateException
  = Core._MatchServiceError mkServiceConfig "InvalidCertificate"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidCertificateException #-}
{-# DEPRECATED _InvalidCertificateException "Use generic-lens or generic-optics instead"  #-}

-- | The request failed because AWS service role policies can only be attached to the service-linked role for that service.
_PolicyNotAttachableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyNotAttachableException
  = Core._MatchServiceError mkServiceConfig "PolicyNotAttachable"
      Core.. Core.hasStatues 400
{-# INLINEABLE _PolicyNotAttachableException #-}
{-# DEPRECATED _PolicyNotAttachableException "Use generic-lens or generic-optics instead"  #-}

-- | The specified service does not support service-specific credentials.
_ServiceNotSupportedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceNotSupportedException
  = Core._MatchServiceError mkServiceConfig "NotSupportedService"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ServiceNotSupportedException #-}
{-# DEPRECATED _ServiceNotSupportedException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because the public key encoding format is unsupported or unrecognized.
_UnrecognizedPublicKeyEncodingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnrecognizedPublicKeyEncodingException
  = Core._MatchServiceError mkServiceConfig
      "UnrecognizedPublicKeyEncoding"
      Core.. Core.hasStatues 400
{-# INLINEABLE _UnrecognizedPublicKeyEncodingException #-}
{-# DEPRECATED _UnrecognizedPublicKeyEncodingException "Use generic-lens or generic-optics instead"  #-}

-- | The request failed because the maximum number of concurrent requests for this account are already running.
_ReportGenerationLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReportGenerationLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "ReportGenerationLimitExceeded"
      Core.. Core.hasStatues 409
{-# INLINEABLE _ReportGenerationLimitExceededException #-}
{-# DEPRECATED _ReportGenerationLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because the type of user for the transaction was incorrect.
_InvalidUserTypeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidUserTypeException
  = Core._MatchServiceError mkServiceConfig "InvalidUserType" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidUserTypeException #-}
{-# DEPRECATED _InvalidUserTypeException "Use generic-lens or generic-optics instead"  #-}

-- | The request processing has failed because of an unknown error, exception or failure.
_ServiceFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceFailureException
  = Core._MatchServiceError mkServiceConfig "ServiceFailure" Core..
      Core.hasStatues 500
{-# INLINEABLE _ServiceFailureException #-}
{-# DEPRECATED _ServiceFailureException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because multiple requests to change this object were submitted simultaneously. Wait a few minutes and submit your request again.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException
  = Core._MatchServiceError mkServiceConfig "ConcurrentModification"
      Core.. Core.hasStatues 409
{-# INLINEABLE _ConcurrentModificationException #-}
{-# DEPRECATED _ConcurrentModificationException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because an invalid or out-of-range value was supplied for an input parameter.
_InvalidInputException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInputException
  = Core._MatchServiceError mkServiceConfig "InvalidInput" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidInputException #-}
{-# DEPRECATED _InvalidInputException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because the public key is malformed or otherwise invalid.
_InvalidPublicKeyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPublicKeyException
  = Core._MatchServiceError mkServiceConfig "InvalidPublicKey" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidPublicKeyException #-}
{-# DEPRECATED _InvalidPublicKeyException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because the authentication code was not recognized. The error message describes the specific error.
_InvalidAuthenticationCodeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidAuthenticationCodeException
  = Core._MatchServiceError mkServiceConfig
      "InvalidAuthenticationCode"
      Core.. Core.hasStatues 403
{-# INLINEABLE _InvalidAuthenticationCodeException #-}
{-# DEPRECATED _InvalidAuthenticationCodeException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because it referenced an entity that is temporarily unmodifiable, such as a user name that was deleted and then recreated. The error indicates that the request is likely to succeed if you try again after waiting several minutes. The error message describes the entity.
_EntityTemporarilyUnmodifiableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EntityTemporarilyUnmodifiableException
  = Core._MatchServiceError mkServiceConfig
      "EntityTemporarilyUnmodifiable"
      Core.. Core.hasStatues 409
{-# INLINEABLE _EntityTemporarilyUnmodifiableException #-}
{-# DEPRECATED _EntityTemporarilyUnmodifiableException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because the SSH public key is already associated with the specified IAM user.
_DuplicateSSHPublicKeyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateSSHPublicKeyException
  = Core._MatchServiceError mkServiceConfig "DuplicateSSHPublicKey"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DuplicateSSHPublicKeyException #-}
{-# DEPRECATED _DuplicateSSHPublicKeyException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because the public key certificate and the private key do not match.
_KeyPairMismatchException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KeyPairMismatchException
  = Core._MatchServiceError mkServiceConfig "KeyPairMismatch" Core..
      Core.hasStatues 400
{-# INLINEABLE _KeyPairMismatchException #-}
{-# DEPRECATED _KeyPairMismatchException "Use generic-lens or generic-optics instead"  #-}

-- | The request failed because a provided policy could not be successfully evaluated. An additional detailed message indicates the source of the failure.
_PolicyEvaluationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyEvaluationException
  = Core._MatchServiceError mkServiceConfig "PolicyEvaluation" Core..
      Core.hasStatues 500
{-# INLINEABLE _PolicyEvaluationException #-}
{-# DEPRECATED _PolicyEvaluationException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because the provided password did not meet the requirements imposed by the account password policy.
_PasswordPolicyViolationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PasswordPolicyViolationException
  = Core._MatchServiceError mkServiceConfig "PasswordPolicyViolation"
      Core.. Core.hasStatues 400
{-# INLINEABLE _PasswordPolicyViolationException #-}
{-# DEPRECATED _PasswordPolicyViolationException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because it attempted to create resources beyond the current AWS account limitations. The error message describes the limit exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceeded" Core..
      Core.hasStatues 409
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}
