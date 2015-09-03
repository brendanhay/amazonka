{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types
    (
    -- * Service Configuration
      iAM

    -- * Errors
    , _CredentialReportNotPresentException
    , _CredentialReportNotReadyException
    , _MalformedPolicyDocumentException
    , _EntityAlreadyExistsException
    , _MalformedCertificateException
    , _CredentialReportExpiredException
    , _DuplicateCertificateException
    , _DeleteConflictException
    , _NoSuchEntityException
    , _InvalidCertificateException
    , _UnrecognizedPublicKeyEncodingException
    , _InvalidUserTypeException
    , _ServiceFailureException
    , _InvalidInputException
    , _InvalidPublicKeyException
    , _InvalidAuthenticationCodeException
    , _EntityTemporarilyUnmodifiableException
    , _DuplicateSSHPublicKeyException
    , _KeyPairMismatchException
    , _PasswordPolicyViolationException
    , _LimitExceededException

    -- * AssignmentStatusType
    , AssignmentStatusType (..)

    -- * EncodingType
    , EncodingType (..)

    -- * EntityType
    , EntityType (..)

    -- * PolicyScopeType
    , PolicyScopeType (..)

    -- * ReportFormatType
    , ReportFormatType (..)

    -- * ReportStateType
    , ReportStateType (..)

    -- * StatusType
    , StatusType (..)

    -- * SummaryKeyType
    , SummaryKeyType (..)

    -- * AccessKey
    , AccessKey
    , accessKey
    , akCreateDate
    , akUserName
    , akAccessKeyId
    , akStatus
    , akSecretAccessKey

    -- * AccessKeyLastUsed
    , AccessKeyLastUsed
    , accessKeyLastUsed
    , akluLastUsedDate
    , akluServiceName
    , akluRegion

    -- * AccessKeyMetadata
    , AccessKeyMetadata
    , accessKeyMetadata
    , akmStatus
    , akmCreateDate
    , akmUserName
    , akmAccessKeyId

    -- * AttachedPolicy
    , AttachedPolicy
    , attachedPolicy
    , apPolicyName
    , apPolicyARN

    -- * Group
    , Group
    , group'
    , gPath
    , gGroupName
    , gGroupId
    , gARN
    , gCreateDate

    -- * GroupDetail
    , GroupDetail
    , groupDetail
    , gdARN
    , gdPath
    , gdCreateDate
    , gdGroupId
    , gdGroupPolicyList
    , gdGroupName
    , gdAttachedManagedPolicies

    -- * InstanceProfile
    , InstanceProfile
    , instanceProfile
    , ipPath
    , ipInstanceProfileName
    , ipInstanceProfileId
    , ipARN
    , ipCreateDate
    , ipRoles

    -- * LoginProfile
    , LoginProfile
    , loginProfile
    , lpPasswordResetRequired
    , lpUserName
    , lpCreateDate

    -- * MFADevice
    , MFADevice
    , mfaDevice
    , mdUserName
    , mdSerialNumber
    , mdEnableDate

    -- * ManagedPolicyDetail
    , ManagedPolicyDetail
    , managedPolicyDetail
    , mpdPolicyName
    , mpdARN
    , mpdUpdateDate
    , mpdPolicyId
    , mpdPath
    , mpdPolicyVersionList
    , mpdCreateDate
    , mpdIsAttachable
    , mpdDefaultVersionId
    , mpdAttachmentCount
    , mpdDescription

    -- * OpenIdConnectProviderListEntry
    , OpenIdConnectProviderListEntry
    , openIdConnectProviderListEntry
    , oicpleARN

    -- * PasswordPolicy
    , PasswordPolicy
    , passwordPolicy
    , ppExpirePasswords
    , ppMinimumPasswordLength
    , ppRequireNumbers
    , ppPasswordReusePrevention
    , ppRequireLowercaseCharacters
    , ppMaxPasswordAge
    , ppHardExpiry
    , ppRequireSymbols
    , ppRequireUppercaseCharacters
    , ppAllowUsersToChangePassword

    -- * Policy
    , Policy
    , policy
    , pPolicyName
    , pARN
    , pUpdateDate
    , pPolicyId
    , pPath
    , pCreateDate
    , pIsAttachable
    , pDefaultVersionId
    , pAttachmentCount
    , pDescription

    -- * PolicyDetail
    , PolicyDetail
    , policyDetail
    , pdPolicyDocument
    , pdPolicyName

    -- * PolicyGroup
    , PolicyGroup
    , policyGroup
    , pgGroupName

    -- * PolicyRole
    , PolicyRole
    , policyRole
    , prRoleName

    -- * PolicyUser
    , PolicyUser
    , policyUser
    , puUserName

    -- * PolicyVersion
    , PolicyVersion
    , policyVersion
    , pvVersionId
    , pvCreateDate
    , pvDocument
    , pvIsDefaultVersion

    -- * Role
    , Role
    , role
    , rAssumeRolePolicyDocument
    , rPath
    , rRoleName
    , rRoleId
    , rARN
    , rCreateDate

    -- * RoleDetail
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

    -- * SAMLProviderListEntry
    , SAMLProviderListEntry
    , sAMLProviderListEntry
    , samlpleARN
    , samlpleCreateDate
    , samlpleValidUntil

    -- * SSHPublicKey
    , SSHPublicKey
    , sshPublicKey
    , spkUploadDate
    , spkUserName
    , spkSSHPublicKeyId
    , spkFingerprint
    , spkSSHPublicKeyBody
    , spkStatus

    -- * SSHPublicKeyMetadata
    , SSHPublicKeyMetadata
    , sshPublicKeyMetadata
    , spkmUserName
    , spkmSSHPublicKeyId
    , spkmStatus
    , spkmUploadDate

    -- * ServerCertificate
    , ServerCertificate
    , serverCertificate
    , sCertificateChain
    , sServerCertificateMetadata
    , sCertificateBody

    -- * ServerCertificateMetadata
    , ServerCertificateMetadata
    , serverCertificateMetadata
    , scmUploadDate
    , scmExpiration
    , scmPath
    , scmServerCertificateName
    , scmServerCertificateId
    , scmARN

    -- * SigningCertificate
    , SigningCertificate
    , signingCertificate
    , scUploadDate
    , scUserName
    , scCertificateId
    , scCertificateBody
    , scStatus

    -- * User
    , User
    , user
    , uPasswordLastUsed
    , uPath
    , uUserName
    , uUserId
    , uARN
    , uCreateDate

    -- * UserDetail
    , UserDetail
    , userDetail
    , udGroupList
    , udARN
    , udPath
    , udCreateDate
    , udUserName
    , udUserId
    , udUserPolicyList
    , udAttachedManagedPolicies

    -- * VirtualMFADevice
    , VirtualMFADevice
    , virtualMFADevice
    , vmdQRCodePNG
    , vmdBase32StringSeed
    , vmdUser
    , vmdEnableDate
    , vmdSerialNumber
    ) where

import           Network.AWS.IAM.Types.Product
import           Network.AWS.IAM.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2010-05-08' of the Amazon Identity and Access Management SDK configuration.
iAM :: Service
iAM =
    Service
    { _svcAbbrev = "IAM"
    , _svcSigner = v4
    , _svcPrefix = "iam"
    , _svcVersion = "2010-05-08"
    , _svcEndpoint = defaultEndpoint iAM
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError
    , _svcRetry = retry
    }
  where
    retry =
        Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | The request was rejected because the credential report does not exist.
-- To generate a credential report, use GenerateCredentialReport.
_CredentialReportNotPresentException :: AsError a => Getting (First ServiceError) a ServiceError
_CredentialReportNotPresentException =
    _ServiceError . hasStatus 410 . hasCode "ReportNotPresent"

-- | The request was rejected because the credential report is still being
-- generated.
_CredentialReportNotReadyException :: AsError a => Getting (First ServiceError) a ServiceError
_CredentialReportNotReadyException =
    _ServiceError . hasStatus 404 . hasCode "ReportInProgress"

-- | The request was rejected because the policy document was malformed. The
-- error message describes the specific error.
_MalformedPolicyDocumentException :: AsError a => Getting (First ServiceError) a ServiceError
_MalformedPolicyDocumentException =
    _ServiceError . hasStatus 400 . hasCode "MalformedPolicyDocument"

-- | The request was rejected because it attempted to create a resource that
-- already exists.
_EntityAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_EntityAlreadyExistsException =
    _ServiceError . hasStatus 409 . hasCode "EntityAlreadyExists"

-- | The request was rejected because the certificate was malformed or
-- expired. The error message describes the specific error.
_MalformedCertificateException :: AsError a => Getting (First ServiceError) a ServiceError
_MalformedCertificateException =
    _ServiceError . hasStatus 400 . hasCode "MalformedCertificate"

-- | The request was rejected because the most recent credential report has
-- expired. To generate a new credential report, use
-- GenerateCredentialReport. For more information about credential report
-- expiration, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Getting Credential Reports>
-- in the /Using IAM/ guide.
_CredentialReportExpiredException :: AsError a => Getting (First ServiceError) a ServiceError
_CredentialReportExpiredException =
    _ServiceError . hasStatus 410 . hasCode "ReportExpired"

-- | The request was rejected because the same certificate is associated with
-- an IAM user in the account.
_DuplicateCertificateException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateCertificateException =
    _ServiceError . hasStatus 409 . hasCode "DuplicateCertificate"

-- | The request was rejected because it attempted to delete a resource that
-- has attached subordinate entities. The error message describes these
-- entities.
_DeleteConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_DeleteConflictException =
    _ServiceError . hasStatus 409 . hasCode "DeleteConflict"

-- | The request was rejected because it referenced an entity that does not
-- exist. The error message describes the entity.
_NoSuchEntityException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchEntityException = _ServiceError . hasStatus 404 . hasCode "NoSuchEntity"

-- | The request was rejected because the certificate is invalid.
_InvalidCertificateException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCertificateException =
    _ServiceError . hasStatus 400 . hasCode "InvalidCertificate"

-- | The request was rejected because the public key encoding format is
-- unsupported or unrecognized.
_UnrecognizedPublicKeyEncodingException :: AsError a => Getting (First ServiceError) a ServiceError
_UnrecognizedPublicKeyEncodingException =
    _ServiceError . hasStatus 400 . hasCode "UnrecognizedPublicKeyEncoding"

-- | The request was rejected because the type of user for the transaction
-- was incorrect.
_InvalidUserTypeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidUserTypeException =
    _ServiceError . hasStatus 400 . hasCode "InvalidUserType"

-- | The request processing has failed because of an unknown error, exception
-- or failure.
_ServiceFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceFailureException =
    _ServiceError . hasStatus 500 . hasCode "ServiceFailure"

-- | The request was rejected because an invalid or out-of-range value was
-- supplied for an input parameter.
_InvalidInputException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInputException = _ServiceError . hasStatus 400 . hasCode "InvalidInput"

-- | The request was rejected because the public key is malformed or
-- otherwise invalid.
_InvalidPublicKeyException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidPublicKeyException =
    _ServiceError . hasStatus 400 . hasCode "InvalidPublicKey"

-- | The request was rejected because the authentication code was not
-- recognized. The error message describes the specific error.
_InvalidAuthenticationCodeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAuthenticationCodeException =
    _ServiceError . hasStatus 403 . hasCode "InvalidAuthenticationCode"

-- | The request was rejected because it referenced an entity that is
-- temporarily unmodifiable, such as a user name that was deleted and then
-- recreated. The error indicates that the request is likely to succeed if
-- you try again after waiting several minutes. The error message describes
-- the entity.
_EntityTemporarilyUnmodifiableException :: AsError a => Getting (First ServiceError) a ServiceError
_EntityTemporarilyUnmodifiableException =
    _ServiceError . hasStatus 409 . hasCode "EntityTemporarilyUnmodifiable"

-- | The request was rejected because the SSH public key is already
-- associated with the specified IAM user.
_DuplicateSSHPublicKeyException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateSSHPublicKeyException =
    _ServiceError . hasStatus 400 . hasCode "DuplicateSSHPublicKey"

-- | The request was rejected because the public key certificate and the
-- private key do not match.
_KeyPairMismatchException :: AsError a => Getting (First ServiceError) a ServiceError
_KeyPairMismatchException =
    _ServiceError . hasStatus 400 . hasCode "KeyPairMismatch"

-- | The request was rejected because the provided password did not meet the
-- requirements imposed by the account password policy.
_PasswordPolicyViolationException :: AsError a => Getting (First ServiceError) a ServiceError
_PasswordPolicyViolationException =
    _ServiceError . hasStatus 400 . hasCode "PasswordPolicyViolation"

-- | The request was rejected because it attempted to create resources beyond
-- the current AWS account limits. The error message describes the limit
-- exceeded.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _ServiceError . hasStatus 409 . hasCode "LimitExceeded"
