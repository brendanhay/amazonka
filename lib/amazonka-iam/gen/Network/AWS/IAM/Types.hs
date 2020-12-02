{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types
    (
    -- * Service Configuration
      iam

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
    , _InvalidUserTypeException
    , _ServiceFailureException
    , _InvalidInputException
    , _InvalidPublicKeyException
    , _InvalidAuthenticationCodeException
    , _EntityTemporarilyUnmodifiableException
    , _DuplicateSSHPublicKeyException
    , _KeyPairMismatchException
    , _PolicyEvaluationException
    , _PasswordPolicyViolationException
    , _LimitExceededException

    -- * AssignmentStatusType
    , AssignmentStatusType (..)

    -- * ContextKeyTypeEnum
    , ContextKeyTypeEnum (..)

    -- * DeletionTaskStatusType
    , DeletionTaskStatusType (..)

    -- * EncodingType
    , EncodingType (..)

    -- * EntityType
    , EntityType (..)

    -- * PolicyEvaluationDecisionType
    , PolicyEvaluationDecisionType (..)

    -- * PolicyScopeType
    , PolicyScopeType (..)

    -- * PolicySourceType
    , PolicySourceType (..)

    -- * ReportFormatType
    , ReportFormatType (..)

    -- * ReportStateType
    , ReportStateType (..)

    -- * StatusType
    , StatusType (..)

    -- * SummaryKeyType
    , SummaryKeyType (..)

    -- * AccessKeyInfo
    , AccessKeyInfo
    , accessKeyInfo
    , akiCreateDate
    , akiUserName
    , akiAccessKeyId
    , akiStatus
    , akiSecretAccessKey

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

    -- * ContextEntry
    , ContextEntry
    , contextEntry
    , ceContextKeyValues
    , ceContextKeyName
    , ceContextKeyType

    -- * DeletionTaskFailureReasonType
    , DeletionTaskFailureReasonType
    , deletionTaskFailureReasonType
    , dtfrtRoleUsageList
    , dtfrtReason

    -- * EvaluationResult
    , EvaluationResult
    , evaluationResult
    , erMatchedStatements
    , erEvalDecisionDetails
    , erResourceSpecificResults
    , erEvalResourceName
    , erMissingContextValues
    , erOrganizationsDecisionDetail
    , erEvalActionName
    , erEvalDecision

    -- * GetContextKeysForPolicyResponse
    , GetContextKeysForPolicyResponse
    , getContextKeysForPolicyResponse
    , gckfpContextKeyNames

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

    -- * OrganizationsDecisionDetail
    , OrganizationsDecisionDetail
    , organizationsDecisionDetail
    , oddAllowedByOrganizations

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
    , pgGroupId
    , pgGroupName

    -- * PolicyRole
    , PolicyRole
    , policyRole
    , prRoleName
    , prRoleId

    -- * PolicyUser
    , PolicyUser
    , policyUser
    , puUserName
    , puUserId

    -- * PolicyVersion
    , PolicyVersion
    , policyVersion
    , pvVersionId
    , pvCreateDate
    , pvDocument
    , pvIsDefaultVersion

    -- * Position
    , Position
    , position
    , pLine
    , pColumn

    -- * ResourceSpecificResult
    , ResourceSpecificResult
    , resourceSpecificResult
    , rsrMatchedStatements
    , rsrEvalDecisionDetails
    , rsrMissingContextValues
    , rsrEvalResourceName
    , rsrEvalResourceDecision

    -- * Role
    , Role
    , role'
    , rMaxSessionDuration
    , rAssumeRolePolicyDocument
    , rDescription
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

    -- * RoleUsageType
    , RoleUsageType
    , roleUsageType
    , rutResources
    , rutRegion

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

    -- * ServiceSpecificCredential
    , ServiceSpecificCredential
    , serviceSpecificCredential
    , sscCreateDate
    , sscServiceName
    , sscServiceUserName
    , sscServicePassword
    , sscServiceSpecificCredentialId
    , sscUserName
    , sscStatus

    -- * ServiceSpecificCredentialMetadata
    , ServiceSpecificCredentialMetadata
    , serviceSpecificCredentialMetadata
    , sscmUserName
    , sscmStatus
    , sscmServiceUserName
    , sscmCreateDate
    , sscmServiceSpecificCredentialId
    , sscmServiceName

    -- * SigningCertificate
    , SigningCertificate
    , signingCertificate
    , scUploadDate
    , scUserName
    , scCertificateId
    , scCertificateBody
    , scStatus

    -- * SimulatePolicyResponse
    , SimulatePolicyResponse
    , simulatePolicyResponse
    , spEvaluationResults
    , spMarker
    , spIsTruncated

    -- * Statement
    , Statement
    , statement
    , sSourcePolicyType
    , sSourcePolicyId
    , sEndPosition
    , sStartPosition

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

import Network.AWS.IAM.Types.Product
import Network.AWS.IAM.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2010-05-08@ of the Amazon Identity and Access Management SDK configuration.
iam :: Service
iam =
  Service
    { _svcAbbrev = "IAM"
    , _svcSigner = v4
    , _svcPrefix = "iam"
    , _svcVersion = "2010-05-08"
    , _svcEndpoint = defaultEndpoint iam
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError "IAM"
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
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The request was rejected because the credential report does not exist. To generate a credential report, use 'GenerateCredentialReport' .
--
--
_CredentialReportNotPresentException :: AsError a => Getting (First ServiceError) a ServiceError
_CredentialReportNotPresentException =
  _MatchServiceError iam "ReportNotPresent" . hasStatus 410


-- | The request was rejected because the credential report is still being generated.
--
--
_CredentialReportNotReadyException :: AsError a => Getting (First ServiceError) a ServiceError
_CredentialReportNotReadyException =
  _MatchServiceError iam "ReportInProgress" . hasStatus 404


-- | The request was rejected because the policy document was malformed. The error message describes the specific error.
--
--
_MalformedPolicyDocumentException :: AsError a => Getting (First ServiceError) a ServiceError
_MalformedPolicyDocumentException =
  _MatchServiceError iam "MalformedPolicyDocument" . hasStatus 400


-- | The request was rejected because it attempted to create a resource that already exists.
--
--
_EntityAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_EntityAlreadyExistsException =
  _MatchServiceError iam "EntityAlreadyExists" . hasStatus 409


-- | The request was rejected because the certificate was malformed or expired. The error message describes the specific error.
--
--
_MalformedCertificateException :: AsError a => Getting (First ServiceError) a ServiceError
_MalformedCertificateException =
  _MatchServiceError iam "MalformedCertificate" . hasStatus 400


-- | The request was rejected because the most recent credential report has expired. To generate a new credential report, use 'GenerateCredentialReport' . For more information about credential report expiration, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Getting Credential Reports> in the /IAM User Guide/ .
--
--
_CredentialReportExpiredException :: AsError a => Getting (First ServiceError) a ServiceError
_CredentialReportExpiredException =
  _MatchServiceError iam "ReportExpired" . hasStatus 410


-- | The request was rejected because only the service that depends on the service-linked role can modify or delete the role on your behalf. The error message includes the name of the service that depends on this service-linked role. You must request the change through that service.
--
--
_UnmodifiableEntityException :: AsError a => Getting (First ServiceError) a ServiceError
_UnmodifiableEntityException =
  _MatchServiceError iam "UnmodifiableEntity" . hasStatus 400


-- | The request was rejected because the same certificate is associated with an IAM user in the account.
--
--
_DuplicateCertificateException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateCertificateException =
  _MatchServiceError iam "DuplicateCertificate" . hasStatus 409


-- | The request was rejected because it attempted to delete a resource that has attached subordinate entities. The error message describes these entities.
--
--
_DeleteConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_DeleteConflictException =
  _MatchServiceError iam "DeleteConflict" . hasStatus 409


-- | The request was rejected because it referenced an entity that does not exist. The error message describes the entity.
--
--
_NoSuchEntityException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchEntityException = _MatchServiceError iam "NoSuchEntity" . hasStatus 404


-- | The request was rejected because the certificate is invalid.
--
--
_InvalidCertificateException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCertificateException =
  _MatchServiceError iam "InvalidCertificate" . hasStatus 400


-- | The request failed because AWS service role policies can only be attached to the service-linked role for that service.
--
--
_PolicyNotAttachableException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyNotAttachableException =
  _MatchServiceError iam "PolicyNotAttachable" . hasStatus 400


-- | The specified service does not support service-specific credentials.
--
--
_ServiceNotSupportedException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceNotSupportedException =
  _MatchServiceError iam "NotSupportedService" . hasStatus 404


-- | The request was rejected because the public key encoding format is unsupported or unrecognized.
--
--
_UnrecognizedPublicKeyEncodingException :: AsError a => Getting (First ServiceError) a ServiceError
_UnrecognizedPublicKeyEncodingException =
  _MatchServiceError iam "UnrecognizedPublicKeyEncoding" . hasStatus 400


-- | The request was rejected because the type of user for the transaction was incorrect.
--
--
_InvalidUserTypeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidUserTypeException =
  _MatchServiceError iam "InvalidUserType" . hasStatus 400


-- | The request processing has failed because of an unknown error, exception or failure.
--
--
_ServiceFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceFailureException =
  _MatchServiceError iam "ServiceFailure" . hasStatus 500


-- | The request was rejected because an invalid or out-of-range value was supplied for an input parameter.
--
--
_InvalidInputException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInputException = _MatchServiceError iam "InvalidInput" . hasStatus 400


-- | The request was rejected because the public key is malformed or otherwise invalid.
--
--
_InvalidPublicKeyException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidPublicKeyException =
  _MatchServiceError iam "InvalidPublicKey" . hasStatus 400


-- | The request was rejected because the authentication code was not recognized. The error message describes the specific error.
--
--
_InvalidAuthenticationCodeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAuthenticationCodeException =
  _MatchServiceError iam "InvalidAuthenticationCode" . hasStatus 403


-- | The request was rejected because it referenced an entity that is temporarily unmodifiable, such as a user name that was deleted and then recreated. The error indicates that the request is likely to succeed if you try again after waiting several minutes. The error message describes the entity.
--
--
_EntityTemporarilyUnmodifiableException :: AsError a => Getting (First ServiceError) a ServiceError
_EntityTemporarilyUnmodifiableException =
  _MatchServiceError iam "EntityTemporarilyUnmodifiable" . hasStatus 409


-- | The request was rejected because the SSH public key is already associated with the specified IAM user.
--
--
_DuplicateSSHPublicKeyException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateSSHPublicKeyException =
  _MatchServiceError iam "DuplicateSSHPublicKey" . hasStatus 400


-- | The request was rejected because the public key certificate and the private key do not match.
--
--
_KeyPairMismatchException :: AsError a => Getting (First ServiceError) a ServiceError
_KeyPairMismatchException =
  _MatchServiceError iam "KeyPairMismatch" . hasStatus 400


-- | The request failed because a provided policy could not be successfully evaluated. An additional detailed message indicates the source of the failure.
--
--
_PolicyEvaluationException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyEvaluationException =
  _MatchServiceError iam "PolicyEvaluation" . hasStatus 500


-- | The request was rejected because the provided password did not meet the requirements imposed by the account password policy.
--
--
_PasswordPolicyViolationException :: AsError a => Getting (First ServiceError) a ServiceError
_PasswordPolicyViolationException =
  _MatchServiceError iam "PasswordPolicyViolation" . hasStatus 400


-- | The request was rejected because it attempted to create resources beyond the current AWS account limits. The error message describes the limit exceeded.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _MatchServiceError iam "LimitExceeded" . hasStatus 409

