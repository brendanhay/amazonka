{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Identity and Access Management
--
-- AWS Identity and Access Management (IAM) is a web service that you can
-- use to manage users and user permissions under your AWS account. This
-- guide provides descriptions of IAM actions that you can call
-- programmatically. For general information about IAM, see
-- <http://aws.amazon.com/iam/ AWS Identity and Access Management (IAM)>.
-- For the user guide for IAM, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/ Using IAM>.
--
-- AWS provides SDKs that consist of libraries and sample code for various
-- programming languages and platforms (Java, Ruby, .NET, iOS, Android,
-- etc.). The SDKs provide a convenient way to create programmatic access
-- to IAM and AWS. For example, the SDKs take care of tasks such as
-- cryptographically signing requests (see below), managing errors, and
-- retrying requests automatically. For information about the AWS SDKs,
-- including how to download and install them, see the
-- <http://aws.amazon.com/tools/ Tools for Amazon Web Services> page.
--
-- We recommend that you use the AWS SDKs to make programmatic API calls to
-- IAM. However, you can also use the IAM Query API to make direct calls to
-- the IAM web service. To learn more about the IAM Query API, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html Making Query Requests>
-- in the /Using IAM/ guide. IAM supports GET and POST requests for all
-- actions. That is, the API does not require you to use GET for some
-- actions and POST for others. However, GET requests are subject to the
-- limitation size of a URL. Therefore, for operations that require larger
-- sizes, use a POST request.
--
-- __Signing Requests__
--
-- Requests must be signed using an access key ID and a secret access key.
-- We strongly recommend that you do not use your AWS account access key ID
-- and secret access key for everyday work with IAM. You can use the access
-- key ID and secret access key for an IAM user or you can use the AWS
-- Security Token Service to generate temporary security credentials and
-- use those to sign requests.
--
-- To sign requests, we recommend that you use
-- <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4>.
-- If you have an existing application that uses Signature Version 2, you
-- do not have to update it to use Signature Version 4. However, some
-- operations now require Signature Version 4. The documentation for
-- operations that require version 4 indicate this requirement.
--
-- __Additional Resources__
--
-- For more information, see the following:
--
-- -   <http://docs.aws.amazon.com/general/latest/gr/aws-security-credentials.html AWS Security Credentials>.
--     This topic provides general information about the types of
--     credentials used for accessing AWS.
-- -   <http://docs.aws.amazon.com/IAM/latest/UserGuide/IAMBestPractices.html IAM Best Practices>.
--     This topic presents a list of suggestions for using the IAM service
--     to help secure your AWS resources.
-- -   <http://docs.aws.amazon.com/STS/latest/UsingSTS/ AWS Security Token Service>.
--     This guide describes how to create and use temporary security
--     credentials.
-- -   <http://docs.aws.amazon.com/general/latest/gr/signing_aws_api_requests.html Signing AWS API Requests>.
--     This set of topics walk you through the process of signing a request
--     using an access key ID and secret access key.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/Welcome.html AWS API Reference>
module Network.AWS.IAM
    (
    -- * Service
      IAM

    -- * Errors
    -- $errors

    -- ** CredentialReportNotPresentException
    , _CredentialReportNotPresentException

    -- ** CredentialReportNotReadyException
    , _CredentialReportNotReadyException

    -- ** MalformedPolicyDocumentException
    , _MalformedPolicyDocumentException

    -- ** EntityAlreadyExistsException
    , _EntityAlreadyExistsException

    -- ** MalformedCertificateException
    , _MalformedCertificateException

    -- ** DuplicateCertificateException
    , _DuplicateCertificateException

    -- ** CredentialReportExpiredException
    , _CredentialReportExpiredException

    -- ** NoSuchEntityException
    , _NoSuchEntityException

    -- ** DeleteConflictException
    , _DeleteConflictException

    -- ** InvalidCertificateException
    , _InvalidCertificateException

    -- ** UnrecognizedPublicKeyEncodingException
    , _UnrecognizedPublicKeyEncodingException

    -- ** InvalidUserTypeException
    , _InvalidUserTypeException

    -- ** ServiceFailureException
    , _ServiceFailureException

    -- ** InvalidInputException
    , _InvalidInputException

    -- ** InvalidPublicKeyException
    , _InvalidPublicKeyException

    -- ** InvalidAuthenticationCodeException
    , _InvalidAuthenticationCodeException

    -- ** EntityTemporarilyUnmodifiableException
    , _EntityTemporarilyUnmodifiableException

    -- ** DuplicateSSHPublicKeyException
    , _DuplicateSSHPublicKeyException

    -- ** KeyPairMismatchException
    , _KeyPairMismatchException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** PasswordPolicyViolationException
    , _PasswordPolicyViolationException

    -- * Waiters
    -- $waiters

    -- ** InstanceProfileExists
    , instanceProfileExists

    -- ** UserExists
    , userExists

    -- * Operations
    -- $operations

    -- ** AttachGroupPolicy
    , module Network.AWS.IAM.AttachGroupPolicy

    -- ** ListInstanceProfilesForRole (Paginated)
    , module Network.AWS.IAM.ListInstanceProfilesForRole

    -- ** CreatePolicy
    , module Network.AWS.IAM.CreatePolicy

    -- ** ListPolicies (Paginated)
    , module Network.AWS.IAM.ListPolicies

    -- ** AttachRolePolicy
    , module Network.AWS.IAM.AttachRolePolicy

    -- ** ListSSHPublicKeys
    , module Network.AWS.IAM.ListSSHPublicKeys

    -- ** DeleteSSHPublicKey
    , module Network.AWS.IAM.DeleteSSHPublicKey

    -- ** UpdateSSHPublicKey
    , module Network.AWS.IAM.UpdateSSHPublicKey

    -- ** ListOpenIdConnectProviders
    , module Network.AWS.IAM.ListOpenIdConnectProviders

    -- ** DeleteAccountPasswordPolicy
    , module Network.AWS.IAM.DeleteAccountPasswordPolicy

    -- ** UpdateAccountPasswordPolicy
    , module Network.AWS.IAM.UpdateAccountPasswordPolicy

    -- ** CreateAccessKey
    , module Network.AWS.IAM.CreateAccessKey

    -- ** GetUserPolicy
    , module Network.AWS.IAM.GetUserPolicy

    -- ** CreateVirtualMFADevice
    , module Network.AWS.IAM.CreateVirtualMFADevice

    -- ** CreateOpenIdConnectProvider
    , module Network.AWS.IAM.CreateOpenIdConnectProvider

    -- ** ListAttachedRolePolicies
    , module Network.AWS.IAM.ListAttachedRolePolicies

    -- ** DeleteVirtualMFADevice
    , module Network.AWS.IAM.DeleteVirtualMFADevice

    -- ** GetRole
    , module Network.AWS.IAM.GetRole

    -- ** DeactivateMFADevice
    , module Network.AWS.IAM.DeactivateMFADevice

    -- ** ListRoles (Paginated)
    , module Network.AWS.IAM.ListRoles

    -- ** DeleteRole
    , module Network.AWS.IAM.DeleteRole

    -- ** ListUserPolicies (Paginated)
    , module Network.AWS.IAM.ListUserPolicies

    -- ** UploadSSHPublicKey
    , module Network.AWS.IAM.UploadSSHPublicKey

    -- ** ListUsers (Paginated)
    , module Network.AWS.IAM.ListUsers

    -- ** UpdateOpenIdConnectProviderThumbprint
    , module Network.AWS.IAM.UpdateOpenIdConnectProviderThumbprint

    -- ** GetSSHPublicKey
    , module Network.AWS.IAM.GetSSHPublicKey

    -- ** PutUserPolicy
    , module Network.AWS.IAM.PutUserPolicy

    -- ** CreateRole
    , module Network.AWS.IAM.CreateRole

    -- ** DeleteUserPolicy
    , module Network.AWS.IAM.DeleteUserPolicy

    -- ** GetOpenIdConnectProvider
    , module Network.AWS.IAM.GetOpenIdConnectProvider

    -- ** DetachGroupPolicy
    , module Network.AWS.IAM.DetachGroupPolicy

    -- ** GetCredentialReport
    , module Network.AWS.IAM.GetCredentialReport

    -- ** DeletePolicyVersion
    , module Network.AWS.IAM.DeletePolicyVersion

    -- ** DetachRolePolicy
    , module Network.AWS.IAM.DetachRolePolicy

    -- ** DeleteInstanceProfile
    , module Network.AWS.IAM.DeleteInstanceProfile

    -- ** ListGroupPolicies (Paginated)
    , module Network.AWS.IAM.ListGroupPolicies

    -- ** GetAccountSummary
    , module Network.AWS.IAM.GetAccountSummary

    -- ** CreateInstanceProfile
    , module Network.AWS.IAM.CreateInstanceProfile

    -- ** PutGroupPolicy
    , module Network.AWS.IAM.PutGroupPolicy

    -- ** DeleteGroupPolicy
    , module Network.AWS.IAM.DeleteGroupPolicy

    -- ** GetAccountAuthorizationDetails
    , module Network.AWS.IAM.GetAccountAuthorizationDetails

    -- ** DeleteAccountAlias
    , module Network.AWS.IAM.DeleteAccountAlias

    -- ** RemoveRoleFromInstanceProfile
    , module Network.AWS.IAM.RemoveRoleFromInstanceProfile

    -- ** GetLoginProfile
    , module Network.AWS.IAM.GetLoginProfile

    -- ** RemoveUserFromGroup
    , module Network.AWS.IAM.RemoveUserFromGroup

    -- ** DetachUserPolicy
    , module Network.AWS.IAM.DetachUserPolicy

    -- ** CreateSAMLProvider
    , module Network.AWS.IAM.CreateSAMLProvider

    -- ** CreatePolicyVersion
    , module Network.AWS.IAM.CreatePolicyVersion

    -- ** GetGroupPolicy
    , module Network.AWS.IAM.GetGroupPolicy

    -- ** DeletePolicy
    , module Network.AWS.IAM.DeletePolicy

    -- ** ListServerCertificates (Paginated)
    , module Network.AWS.IAM.ListServerCertificates

    -- ** UpdateAssumeRolePolicy
    , module Network.AWS.IAM.UpdateAssumeRolePolicy

    -- ** ChangePassword
    , module Network.AWS.IAM.ChangePassword

    -- ** ListGroupsForUser (Paginated)
    , module Network.AWS.IAM.ListGroupsForUser

    -- ** GetPolicyVersion
    , module Network.AWS.IAM.GetPolicyVersion

    -- ** CreateLoginProfile
    , module Network.AWS.IAM.CreateLoginProfile

    -- ** GetInstanceProfile
    , module Network.AWS.IAM.GetInstanceProfile

    -- ** ListEntitiesForPolicy
    , module Network.AWS.IAM.ListEntitiesForPolicy

    -- ** GetSAMLProvider
    , module Network.AWS.IAM.GetSAMLProvider

    -- ** AddRoleToInstanceProfile
    , module Network.AWS.IAM.AddRoleToInstanceProfile

    -- ** AddUserToGroup
    , module Network.AWS.IAM.AddUserToGroup

    -- ** DeleteOpenIdConnectProvider
    , module Network.AWS.IAM.DeleteOpenIdConnectProvider

    -- ** GetUser
    , module Network.AWS.IAM.GetUser

    -- ** ListAttachedUserPolicies
    , module Network.AWS.IAM.ListAttachedUserPolicies

    -- ** DeleteSigningCertificate
    , module Network.AWS.IAM.DeleteSigningCertificate

    -- ** UpdateSigningCertificate
    , module Network.AWS.IAM.UpdateSigningCertificate

    -- ** ListSigningCertificates (Paginated)
    , module Network.AWS.IAM.ListSigningCertificates

    -- ** RemoveClientIdFromOpenIdConnectProvider
    , module Network.AWS.IAM.RemoveClientIdFromOpenIdConnectProvider

    -- ** ListAccessKeys (Paginated)
    , module Network.AWS.IAM.ListAccessKeys

    -- ** ListVirtualMFADevices (Paginated)
    , module Network.AWS.IAM.ListVirtualMFADevices

    -- ** DeleteAccessKey
    , module Network.AWS.IAM.DeleteAccessKey

    -- ** UpdateAccessKey
    , module Network.AWS.IAM.UpdateAccessKey

    -- ** GetRolePolicy
    , module Network.AWS.IAM.GetRolePolicy

    -- ** AttachUserPolicy
    , module Network.AWS.IAM.AttachUserPolicy

    -- ** ResyncMFADevice
    , module Network.AWS.IAM.ResyncMFADevice

    -- ** CreateUser
    , module Network.AWS.IAM.CreateUser

    -- ** UploadSigningCertificate
    , module Network.AWS.IAM.UploadSigningCertificate

    -- ** PutRolePolicy
    , module Network.AWS.IAM.PutRolePolicy

    -- ** DeleteRolePolicy
    , module Network.AWS.IAM.DeleteRolePolicy

    -- ** UpdateUser
    , module Network.AWS.IAM.UpdateUser

    -- ** DeleteUser
    , module Network.AWS.IAM.DeleteUser

    -- ** ListRolePolicies (Paginated)
    , module Network.AWS.IAM.ListRolePolicies

    -- ** AddClientIdToOpenIdConnectProvider
    , module Network.AWS.IAM.AddClientIdToOpenIdConnectProvider

    -- ** GetAccessKeyLastUsed
    , module Network.AWS.IAM.GetAccessKeyLastUsed

    -- ** GetAccountPasswordPolicy
    , module Network.AWS.IAM.GetAccountPasswordPolicy

    -- ** ListAccountAliases (Paginated)
    , module Network.AWS.IAM.ListAccountAliases

    -- ** CreateAccountAlias
    , module Network.AWS.IAM.CreateAccountAlias

    -- ** UploadServerCertificate
    , module Network.AWS.IAM.UploadServerCertificate

    -- ** ListMFADevices (Paginated)
    , module Network.AWS.IAM.ListMFADevices

    -- ** EnableMFADevice
    , module Network.AWS.IAM.EnableMFADevice

    -- ** ListPolicyVersions
    , module Network.AWS.IAM.ListPolicyVersions

    -- ** ListSAMLProviders
    , module Network.AWS.IAM.ListSAMLProviders

    -- ** UpdateSAMLProvider
    , module Network.AWS.IAM.UpdateSAMLProvider

    -- ** DeleteSAMLProvider
    , module Network.AWS.IAM.DeleteSAMLProvider

    -- ** CreateGroup
    , module Network.AWS.IAM.CreateGroup

    -- ** SetDefaultPolicyVersion
    , module Network.AWS.IAM.SetDefaultPolicyVersion

    -- ** ListInstanceProfiles (Paginated)
    , module Network.AWS.IAM.ListInstanceProfiles

    -- ** ListGroups (Paginated)
    , module Network.AWS.IAM.ListGroups

    -- ** DeleteGroup
    , module Network.AWS.IAM.DeleteGroup

    -- ** UpdateGroup
    , module Network.AWS.IAM.UpdateGroup

    -- ** GetServerCertificate
    , module Network.AWS.IAM.GetServerCertificate

    -- ** GetPolicy
    , module Network.AWS.IAM.GetPolicy

    -- ** GenerateCredentialReport
    , module Network.AWS.IAM.GenerateCredentialReport

    -- ** GetGroup (Paginated)
    , module Network.AWS.IAM.GetGroup

    -- ** DeleteServerCertificate
    , module Network.AWS.IAM.DeleteServerCertificate

    -- ** UpdateServerCertificate
    , module Network.AWS.IAM.UpdateServerCertificate

    -- ** DeleteLoginProfile
    , module Network.AWS.IAM.DeleteLoginProfile

    -- ** UpdateLoginProfile
    , module Network.AWS.IAM.UpdateLoginProfile

    -- ** ListAttachedGroupPolicies
    , module Network.AWS.IAM.ListAttachedGroupPolicies

    -- * Types

    -- ** AssignmentStatusType
    , AssignmentStatusType (..)

    -- ** EncodingType
    , EncodingType (..)

    -- ** EntityType
    , EntityType (..)

    -- ** PolicyScopeType
    , PolicyScopeType (..)

    -- ** ReportFormatType
    , ReportFormatType (..)

    -- ** ReportStateType
    , ReportStateType (..)

    -- ** StatusType
    , StatusType (..)

    -- ** SummaryKeyType
    , SummaryKeyType (..)

    -- ** AccessKey
    , AccessKey
    , accessKey
    , akCreateDate
    , akUserName
    , akAccessKeyId
    , akStatus
    , akSecretAccessKey

    -- ** AccessKeyLastUsed
    , AccessKeyLastUsed
    , accessKeyLastUsed
    , akluLastUsedDate
    , akluServiceName
    , akluRegion

    -- ** AccessKeyMetadata
    , AccessKeyMetadata
    , accessKeyMetadata
    , akmStatus
    , akmCreateDate
    , akmUserName
    , akmAccessKeyId

    -- ** AttachedPolicy
    , AttachedPolicy
    , attachedPolicy
    , apPolicyName
    , apPolicyARN

    -- ** Group
    , Group
    , group'
    , gPath
    , gGroupName
    , gGroupId
    , gARN
    , gCreateDate

    -- ** GroupDetail
    , GroupDetail
    , groupDetail
    , gdARN
    , gdPath
    , gdCreateDate
    , gdGroupId
    , gdGroupPolicyList
    , gdGroupName
    , gdAttachedManagedPolicies

    -- ** InstanceProfile
    , InstanceProfile
    , instanceProfile
    , ipPath
    , ipInstanceProfileName
    , ipInstanceProfileId
    , ipARN
    , ipCreateDate
    , ipRoles

    -- ** LoginProfile
    , LoginProfile
    , loginProfile
    , lpPasswordResetRequired
    , lpUserName
    , lpCreateDate

    -- ** MFADevice
    , MFADevice
    , mfaDevice
    , mdUserName
    , mdSerialNumber
    , mdEnableDate

    -- ** ManagedPolicyDetail
    , ManagedPolicyDetail
    , managedPolicyDetail
    , mpdPolicyName
    , mpdARN
    , mpdPath
    , mpdUpdateDate
    , mpdPolicyId
    , mpdCreateDate
    , mpdPolicyVersionList
    , mpdIsAttachable
    , mpdDefaultVersionId
    , mpdAttachmentCount
    , mpdDescription

    -- ** OpenIdConnectProviderListEntry
    , OpenIdConnectProviderListEntry
    , openIdConnectProviderListEntry
    , oicpleARN

    -- ** PasswordPolicy
    , PasswordPolicy
    , passwordPolicy
    , ppExpirePasswords
    , ppRequireNumbers
    , ppMinimumPasswordLength
    , ppPasswordReusePrevention
    , ppRequireLowercaseCharacters
    , ppMaxPasswordAge
    , ppHardExpiry
    , ppRequireSymbols
    , ppRequireUppercaseCharacters
    , ppAllowUsersToChangePassword

    -- ** Policy
    , Policy
    , policy
    , pPolicyName
    , pARN
    , pPath
    , pUpdateDate
    , pPolicyId
    , pCreateDate
    , pIsAttachable
    , pDefaultVersionId
    , pAttachmentCount
    , pDescription

    -- ** PolicyDetail
    , PolicyDetail
    , policyDetail
    , pdPolicyDocument
    , pdPolicyName

    -- ** PolicyGroup
    , PolicyGroup
    , policyGroup
    , pgGroupName

    -- ** PolicyRole
    , PolicyRole
    , policyRole
    , prRoleName

    -- ** PolicyUser
    , PolicyUser
    , policyUser
    , puUserName

    -- ** PolicyVersion
    , PolicyVersion
    , policyVersion
    , pvVersionId
    , pvCreateDate
    , pvDocument
    , pvIsDefaultVersion

    -- ** Role
    , Role
    , role
    , rAssumeRolePolicyDocument
    , rPath
    , rRoleName
    , rRoleId
    , rARN
    , rCreateDate

    -- ** RoleDetail
    , RoleDetail
    , roleDetail
    , rdAssumeRolePolicyDocument
    , rdARN
    , rdPath
    , rdInstanceProfileList
    , rdCreateDate
    , rdRoleName
    , rdRoleId
    , rdRolePolicyList
    , rdAttachedManagedPolicies

    -- ** SAMLProviderListEntry
    , SAMLProviderListEntry
    , sAMLProviderListEntry
    , samlpleARN
    , samlpleCreateDate
    , samlpleValidUntil

    -- ** SSHPublicKey
    , SSHPublicKey
    , sshPublicKey
    , spkUploadDate
    , spkUserName
    , spkSSHPublicKeyId
    , spkFingerprint
    , spkSSHPublicKeyBody
    , spkStatus

    -- ** SSHPublicKeyMetadata
    , SSHPublicKeyMetadata
    , sshPublicKeyMetadata
    , spkmUserName
    , spkmSSHPublicKeyId
    , spkmStatus
    , spkmUploadDate

    -- ** ServerCertificate
    , ServerCertificate
    , serverCertificate
    , sCertificateChain
    , sServerCertificateMetadata
    , sCertificateBody

    -- ** ServerCertificateMetadata
    , ServerCertificateMetadata
    , serverCertificateMetadata
    , scmUploadDate
    , scmExpiration
    , scmPath
    , scmServerCertificateName
    , scmServerCertificateId
    , scmARN

    -- ** SigningCertificate
    , SigningCertificate
    , signingCertificate
    , scUploadDate
    , scUserName
    , scCertificateId
    , scCertificateBody
    , scStatus

    -- ** User
    , User
    , user
    , uPasswordLastUsed
    , uPath
    , uUserName
    , uUserId
    , uARN
    , uCreateDate

    -- ** UserDetail
    , UserDetail
    , userDetail
    , udARN
    , udPath
    , udGroupList
    , udCreateDate
    , udUserName
    , udUserId
    , udUserPolicyList
    , udAttachedManagedPolicies

    -- ** VirtualMFADevice
    , VirtualMFADevice
    , virtualMFADevice
    , vmdQRCodePNG
    , vmdBase32StringSeed
    , vmdUser
    , vmdEnableDate
    , vmdSerialNumber
    ) where

import           Network.AWS.IAM.AddClientIdToOpenIdConnectProvider
import           Network.AWS.IAM.AddRoleToInstanceProfile
import           Network.AWS.IAM.AddUserToGroup
import           Network.AWS.IAM.AttachGroupPolicy
import           Network.AWS.IAM.AttachRolePolicy
import           Network.AWS.IAM.AttachUserPolicy
import           Network.AWS.IAM.ChangePassword
import           Network.AWS.IAM.CreateAccessKey
import           Network.AWS.IAM.CreateAccountAlias
import           Network.AWS.IAM.CreateGroup
import           Network.AWS.IAM.CreateInstanceProfile
import           Network.AWS.IAM.CreateLoginProfile
import           Network.AWS.IAM.CreateOpenIdConnectProvider
import           Network.AWS.IAM.CreatePolicy
import           Network.AWS.IAM.CreatePolicyVersion
import           Network.AWS.IAM.CreateRole
import           Network.AWS.IAM.CreateSAMLProvider
import           Network.AWS.IAM.CreateUser
import           Network.AWS.IAM.CreateVirtualMFADevice
import           Network.AWS.IAM.DeactivateMFADevice
import           Network.AWS.IAM.DeleteAccessKey
import           Network.AWS.IAM.DeleteAccountAlias
import           Network.AWS.IAM.DeleteAccountPasswordPolicy
import           Network.AWS.IAM.DeleteGroup
import           Network.AWS.IAM.DeleteGroupPolicy
import           Network.AWS.IAM.DeleteInstanceProfile
import           Network.AWS.IAM.DeleteLoginProfile
import           Network.AWS.IAM.DeleteOpenIdConnectProvider
import           Network.AWS.IAM.DeletePolicy
import           Network.AWS.IAM.DeletePolicyVersion
import           Network.AWS.IAM.DeleteRole
import           Network.AWS.IAM.DeleteRolePolicy
import           Network.AWS.IAM.DeleteSAMLProvider
import           Network.AWS.IAM.DeleteServerCertificate
import           Network.AWS.IAM.DeleteSigningCertificate
import           Network.AWS.IAM.DeleteSSHPublicKey
import           Network.AWS.IAM.DeleteUser
import           Network.AWS.IAM.DeleteUserPolicy
import           Network.AWS.IAM.DeleteVirtualMFADevice
import           Network.AWS.IAM.DetachGroupPolicy
import           Network.AWS.IAM.DetachRolePolicy
import           Network.AWS.IAM.DetachUserPolicy
import           Network.AWS.IAM.EnableMFADevice
import           Network.AWS.IAM.GenerateCredentialReport
import           Network.AWS.IAM.GetAccessKeyLastUsed
import           Network.AWS.IAM.GetAccountAuthorizationDetails
import           Network.AWS.IAM.GetAccountPasswordPolicy
import           Network.AWS.IAM.GetAccountSummary
import           Network.AWS.IAM.GetCredentialReport
import           Network.AWS.IAM.GetGroup
import           Network.AWS.IAM.GetGroupPolicy
import           Network.AWS.IAM.GetInstanceProfile
import           Network.AWS.IAM.GetLoginProfile
import           Network.AWS.IAM.GetOpenIdConnectProvider
import           Network.AWS.IAM.GetPolicy
import           Network.AWS.IAM.GetPolicyVersion
import           Network.AWS.IAM.GetRole
import           Network.AWS.IAM.GetRolePolicy
import           Network.AWS.IAM.GetSAMLProvider
import           Network.AWS.IAM.GetServerCertificate
import           Network.AWS.IAM.GetSSHPublicKey
import           Network.AWS.IAM.GetUser
import           Network.AWS.IAM.GetUserPolicy
import           Network.AWS.IAM.ListAccessKeys
import           Network.AWS.IAM.ListAccountAliases
import           Network.AWS.IAM.ListAttachedGroupPolicies
import           Network.AWS.IAM.ListAttachedRolePolicies
import           Network.AWS.IAM.ListAttachedUserPolicies
import           Network.AWS.IAM.ListEntitiesForPolicy
import           Network.AWS.IAM.ListGroupPolicies
import           Network.AWS.IAM.ListGroups
import           Network.AWS.IAM.ListGroupsForUser
import           Network.AWS.IAM.ListInstanceProfiles
import           Network.AWS.IAM.ListInstanceProfilesForRole
import           Network.AWS.IAM.ListMFADevices
import           Network.AWS.IAM.ListOpenIdConnectProviders
import           Network.AWS.IAM.ListPolicies
import           Network.AWS.IAM.ListPolicyVersions
import           Network.AWS.IAM.ListRolePolicies
import           Network.AWS.IAM.ListRoles
import           Network.AWS.IAM.ListSAMLProviders
import           Network.AWS.IAM.ListServerCertificates
import           Network.AWS.IAM.ListSigningCertificates
import           Network.AWS.IAM.ListSSHPublicKeys
import           Network.AWS.IAM.ListUserPolicies
import           Network.AWS.IAM.ListUsers
import           Network.AWS.IAM.ListVirtualMFADevices
import           Network.AWS.IAM.PutGroupPolicy
import           Network.AWS.IAM.PutRolePolicy
import           Network.AWS.IAM.PutUserPolicy
import           Network.AWS.IAM.RemoveClientIdFromOpenIdConnectProvider
import           Network.AWS.IAM.RemoveRoleFromInstanceProfile
import           Network.AWS.IAM.RemoveUserFromGroup
import           Network.AWS.IAM.ResyncMFADevice
import           Network.AWS.IAM.SetDefaultPolicyVersion
import           Network.AWS.IAM.Types
import           Network.AWS.IAM.UpdateAccessKey
import           Network.AWS.IAM.UpdateAccountPasswordPolicy
import           Network.AWS.IAM.UpdateAssumeRolePolicy
import           Network.AWS.IAM.UpdateGroup
import           Network.AWS.IAM.UpdateLoginProfile
import           Network.AWS.IAM.UpdateOpenIdConnectProviderThumbprint
import           Network.AWS.IAM.UpdateSAMLProvider
import           Network.AWS.IAM.UpdateServerCertificate
import           Network.AWS.IAM.UpdateSigningCertificate
import           Network.AWS.IAM.UpdateSSHPublicKey
import           Network.AWS.IAM.UpdateUser
import           Network.AWS.IAM.UploadServerCertificate
import           Network.AWS.IAM.UploadSigningCertificate
import           Network.AWS.IAM.UploadSSHPublicKey
import           Network.AWS.IAM.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'IAM'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
