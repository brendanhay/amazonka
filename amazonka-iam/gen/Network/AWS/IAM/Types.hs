{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types
    (
    -- * Service
      IAM

    -- * Errors
    , _CredentialReportNotPresentException
    , _CredentialReportNotReadyException
    , _MalformedPolicyDocumentException
    , _EntityAlreadyExistsException
    , _MalformedCertificateException
    , _DuplicateCertificateException
    , _CredentialReportExpiredException
    , _NoSuchEntityException
    , _DeleteConflictException
    , _InvalidCertificateException
    , _InvalidUserTypeException
    , _ServiceFailureException
    , _InvalidInputException
    , _InvalidAuthenticationCodeException
    , _EntityTemporarilyUnmodifiableException
    , _KeyPairMismatchException
    , _LimitExceededException
    , _PasswordPolicyViolationException

    -- * AssignmentStatusType
    , AssignmentStatusType (..)

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
    , groPath
    , groGroupName
    , groGroupId
    , groARN
    , groCreateDate

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
    , mpdPath
    , mpdUpdateDate
    , mpdPolicyId
    , mpdCreateDate
    , mpdPolicyVersionList
    , mpdIsAttachable
    , mpdDefaultVersionId
    , mpdAttachmentCount
    , mpdDescription

    -- * OpenIDConnectProviderListEntry
    , OpenIDConnectProviderListEntry
    , openIDConnectProviderListEntry
    , oidcpleARN

    -- * PasswordPolicy
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

    -- * Policy
    , Policy
    , policy
    , polPolicyName
    , polARN
    , polPath
    , polUpdateDate
    , polPolicyId
    , polCreateDate
    , polIsAttachable
    , polDefaultVersionId
    , polAttachmentCount
    , polDescription

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
    , rolAssumeRolePolicyDocument
    , rolPath
    , rolRoleName
    , rolRoleId
    , rolARN
    , rolCreateDate

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

    -- * ServerCertificate
    , ServerCertificate
    , serverCertificate
    , serCertificateChain
    , serServerCertificateMetadata
    , serCertificateBody

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
    , usePasswordLastUsed
    , usePath
    , useUserName
    , useUserId
    , useARN
    , useCreateDate

    -- * UserDetail
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

    -- * VirtualMFADevice
    , VirtualMFADevice
    , virtualMFADevice
    , vmdQRCodePNG
    , vmdBase32StringSeed
    , vmdUser
    , vmdEnableDate
    , vmdSerialNumber
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2010-05-08@ of the Amazon Identity and Access Management SDK.
data IAM

instance AWSService IAM where
    type Sg IAM = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "IAM"
            , _svcPrefix = "iam"
            , _svcVersion = "2010-05-08"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseXMLError
            , _svcRetry = retry
            }
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
_CredentialReportNotPresentException :: AWSError a => Getting (First ServiceError) a ServiceError
_CredentialReportNotPresentException =
    _ServiceError . hasStatus 410 . hasCode "ReportNotPresent"

-- | The request was rejected because the credential report is still being
-- generated.
_CredentialReportNotReadyException :: AWSError a => Getting (First ServiceError) a ServiceError
_CredentialReportNotReadyException =
    _ServiceError . hasStatus 404 . hasCode "ReportInProgress"

-- | The request was rejected because the policy document was malformed. The
-- error message describes the specific error.
_MalformedPolicyDocumentException :: AWSError a => Getting (First ServiceError) a ServiceError
_MalformedPolicyDocumentException =
    _ServiceError . hasStatus 400 . hasCode "MalformedPolicyDocument"

-- | The request was rejected because it attempted to create a resource that
-- already exists.
_EntityAlreadyExistsException :: AWSError a => Getting (First ServiceError) a ServiceError
_EntityAlreadyExistsException =
    _ServiceError . hasStatus 409 . hasCode "EntityAlreadyExists"

-- | The request was rejected because the certificate was malformed or
-- expired. The error message describes the specific error.
_MalformedCertificateException :: AWSError a => Getting (First ServiceError) a ServiceError
_MalformedCertificateException =
    _ServiceError . hasStatus 400 . hasCode "MalformedCertificate"

-- | The request was rejected because the same certificate is associated to
-- another user under the account.
_DuplicateCertificateException :: AWSError a => Getting (First ServiceError) a ServiceError
_DuplicateCertificateException =
    _ServiceError . hasStatus 409 . hasCode "DuplicateCertificate"

-- | The request was rejected because the most recent credential report has
-- expired. To generate a new credential report, use
-- GenerateCredentialReport. For more information about credential report
-- expiration, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Getting Credential Reports>
-- in the /Using IAM/ guide.
_CredentialReportExpiredException :: AWSError a => Getting (First ServiceError) a ServiceError
_CredentialReportExpiredException =
    _ServiceError . hasStatus 410 . hasCode "ReportExpired"

-- | The request was rejected because it referenced an entity that does not
-- exist. The error message describes the entity.
_NoSuchEntityException :: AWSError a => Getting (First ServiceError) a ServiceError
_NoSuchEntityException = _ServiceError . hasStatus 404 . hasCode "NoSuchEntity"

-- | The request was rejected because it attempted to delete a resource that
-- has attached subordinate entities. The error message describes these
-- entities.
_DeleteConflictException :: AWSError a => Getting (First ServiceError) a ServiceError
_DeleteConflictException =
    _ServiceError . hasStatus 409 . hasCode "DeleteConflict"

-- | The request was rejected because the certificate is invalid.
_InvalidCertificateException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidCertificateException =
    _ServiceError . hasStatus 400 . hasCode "InvalidCertificate"

-- | The request was rejected because the type of user for the transaction
-- was incorrect.
_InvalidUserTypeException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidUserTypeException =
    _ServiceError . hasStatus 400 . hasCode "InvalidUserType"

-- | The request processing has failed because of an unknown error, exception
-- or failure.
_ServiceFailureException :: AWSError a => Getting (First ServiceError) a ServiceError
_ServiceFailureException =
    _ServiceError . hasStatus 500 . hasCode "ServiceFailure"

-- | The request was rejected because an invalid or out-of-range value was
-- supplied for an input parameter.
_InvalidInputException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidInputException = _ServiceError . hasStatus 400 . hasCode "InvalidInput"

-- | The request was rejected because the authentication code was not
-- recognized. The error message describes the specific error.
_InvalidAuthenticationCodeException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidAuthenticationCodeException =
    _ServiceError . hasStatus 403 . hasCode "InvalidAuthenticationCode"

-- | The request was rejected because it referenced an entity that is
-- temporarily unmodifiable, such as a user name that was deleted and then
-- recreated. The error indicates that the request is likely to succeed if
-- you try again after waiting several minutes. The error message describes
-- the entity.
_EntityTemporarilyUnmodifiableException :: AWSError a => Getting (First ServiceError) a ServiceError
_EntityTemporarilyUnmodifiableException =
    _ServiceError . hasStatus 409 . hasCode "EntityTemporarilyUnmodifiable"

-- | The request was rejected because the public key certificate and the
-- private key do not match.
_KeyPairMismatchException :: AWSError a => Getting (First ServiceError) a ServiceError
_KeyPairMismatchException =
    _ServiceError . hasStatus 400 . hasCode "KeyPairMismatch"

-- | The request was rejected because it attempted to create resources beyond
-- the current AWS account limits. The error message describes the limit
-- exceeded.
_LimitExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _ServiceError . hasStatus 409 . hasCode "LimitExceeded"

-- | The request was rejected because the provided password did not meet the
-- requirements imposed by the account password policy.
_PasswordPolicyViolationException :: AWSError a => Getting (First ServiceError) a ServiceError
_PasswordPolicyViolationException =
    _ServiceError . hasStatus 400 . hasCode "PasswordPolicyViolation"

data AssignmentStatusType
    = Assigned
    | Unassigned
    | Any
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText AssignmentStatusType where
    parser = takeLowerText >>= \case
        "any" -> pure Any
        "assigned" -> pure Assigned
        "unassigned" -> pure Unassigned
        e -> fromTextError $ "Failure parsing AssignmentStatusType from value: '" <> e
           <> "'. Accepted values: any, assigned, unassigned"

instance ToText AssignmentStatusType where
    toText = \case
        Any -> "any"
        Assigned -> "assigned"
        Unassigned -> "unassigned"

instance Hashable AssignmentStatusType
instance ToQuery AssignmentStatusType
instance ToHeader AssignmentStatusType

data EntityType
    = Group
    | LocalManagedPolicy
    | AWSManagedPolicy
    | User
    | Role
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText EntityType where
    parser = takeLowerText >>= \case
        "awsmanagedpolicy" -> pure AWSManagedPolicy
        "group" -> pure Group
        "localmanagedpolicy" -> pure LocalManagedPolicy
        "role" -> pure Role
        "user" -> pure User
        e -> fromTextError $ "Failure parsing EntityType from value: '" <> e
           <> "'. Accepted values: awsmanagedpolicy, group, localmanagedpolicy, role, user"

instance ToText EntityType where
    toText = \case
        AWSManagedPolicy -> "awsmanagedpolicy"
        Group -> "group"
        LocalManagedPolicy -> "localmanagedpolicy"
        Role -> "role"
        User -> "user"

instance Hashable EntityType
instance ToQuery EntityType
instance ToHeader EntityType

data PolicyScopeType
    = AWS
    | Local
    | All
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText PolicyScopeType where
    parser = takeLowerText >>= \case
        "aws" -> pure AWS
        "all" -> pure All
        "local" -> pure Local
        e -> fromTextError $ "Failure parsing PolicyScopeType from value: '" <> e
           <> "'. Accepted values: aws, all, local"

instance ToText PolicyScopeType where
    toText = \case
        AWS -> "aws"
        All -> "all"
        Local -> "local"

instance Hashable PolicyScopeType
instance ToQuery PolicyScopeType
instance ToHeader PolicyScopeType

data ReportFormatType =
    TextCSV
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ReportFormatType where
    parser = takeLowerText >>= \case
        "text/csv" -> pure TextCSV
        e -> fromTextError $ "Failure parsing ReportFormatType from value: '" <> e
           <> "'. Accepted values: text/csv"

instance ToText ReportFormatType where
    toText = \case
        TextCSV -> "text/csv"

instance Hashable ReportFormatType
instance ToQuery ReportFormatType
instance ToHeader ReportFormatType

instance FromXML ReportFormatType where
    parseXML = parseXMLText "ReportFormatType"

data ReportStateType
    = Inprogress
    | Started
    | Complete
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ReportStateType where
    parser = takeLowerText >>= \case
        "complete" -> pure Complete
        "inprogress" -> pure Inprogress
        "started" -> pure Started
        e -> fromTextError $ "Failure parsing ReportStateType from value: '" <> e
           <> "'. Accepted values: complete, inprogress, started"

instance ToText ReportStateType where
    toText = \case
        Complete -> "complete"
        Inprogress -> "inprogress"
        Started -> "started"

instance Hashable ReportStateType
instance ToQuery ReportStateType
instance ToHeader ReportStateType

instance FromXML ReportStateType where
    parseXML = parseXMLText "ReportStateType"

data StatusType
    = Inactive
    | Active
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StatusType where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "inactive" -> pure Inactive
        e -> fromTextError $ "Failure parsing StatusType from value: '" <> e
           <> "'. Accepted values: active, inactive"

instance ToText StatusType where
    toText = \case
        Active -> "active"
        Inactive -> "inactive"

instance Hashable StatusType
instance ToQuery StatusType
instance ToHeader StatusType

instance FromXML StatusType where
    parseXML = parseXMLText "StatusType"

data SummaryKeyType
    = AttachedPoliciesPerUserQuota
    | UsersQuota
    | Groups
    | GroupsQuota
    | Users
    | MFADevicesInUse
    | PolicyVersionsInUse
    | SigningCertificatesPerUserQuota
    | PoliciesQuota
    | AccessKeysPerUserQuota
    | PolicySizeQuota
    | ServerCertificates
    | AttachedPoliciesPerRoleQuota
    | GroupsPerUserQuota
    | GroupPolicySizeQuota
    | AccountSigningCertificatesPresent
    | UserPolicySizeQuota
    | AttachedPoliciesPerGroupQuota
    | AccountAccessKeysPresent
    | ServerCertificatesQuota
    | VersionsPerPolicyQuota
    | PolicyVersionsInUseQuota
    | Policies
    | AccountMFAEnabled
    | MFADevices
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText SummaryKeyType where
    parser = takeLowerText >>= \case
        "accesskeysperuserquota" -> pure AccessKeysPerUserQuota
        "accountaccesskeyspresent" -> pure AccountAccessKeysPresent
        "accountmfaenabled" -> pure AccountMFAEnabled
        "accountsigningcertificatespresent" -> pure AccountSigningCertificatesPresent
        "attachedpoliciespergroupquota" -> pure AttachedPoliciesPerGroupQuota
        "attachedpoliciesperrolequota" -> pure AttachedPoliciesPerRoleQuota
        "attachedpoliciesperuserquota" -> pure AttachedPoliciesPerUserQuota
        "grouppolicysizequota" -> pure GroupPolicySizeQuota
        "groups" -> pure Groups
        "groupsperuserquota" -> pure GroupsPerUserQuota
        "groupsquota" -> pure GroupsQuota
        "mfadevices" -> pure MFADevices
        "mfadevicesinuse" -> pure MFADevicesInUse
        "policies" -> pure Policies
        "policiesquota" -> pure PoliciesQuota
        "policysizequota" -> pure PolicySizeQuota
        "policyversionsinuse" -> pure PolicyVersionsInUse
        "policyversionsinusequota" -> pure PolicyVersionsInUseQuota
        "servercertificates" -> pure ServerCertificates
        "servercertificatesquota" -> pure ServerCertificatesQuota
        "signingcertificatesperuserquota" -> pure SigningCertificatesPerUserQuota
        "userpolicysizequota" -> pure UserPolicySizeQuota
        "users" -> pure Users
        "usersquota" -> pure UsersQuota
        "versionsperpolicyquota" -> pure VersionsPerPolicyQuota
        e -> fromTextError $ "Failure parsing SummaryKeyType from value: '" <> e
           <> "'. Accepted values: accesskeysperuserquota, accountaccesskeyspresent, accountmfaenabled, accountsigningcertificatespresent, attachedpoliciespergroupquota, attachedpoliciesperrolequota, attachedpoliciesperuserquota, grouppolicysizequota, groups, groupsperuserquota, groupsquota, mfadevices, mfadevicesinuse, policies, policiesquota, policysizequota, policyversionsinuse, policyversionsinusequota, servercertificates, servercertificatesquota, signingcertificatesperuserquota, userpolicysizequota, users, usersquota, versionsperpolicyquota"

instance ToText SummaryKeyType where
    toText = \case
        AccessKeysPerUserQuota -> "accesskeysperuserquota"
        AccountAccessKeysPresent -> "accountaccesskeyspresent"
        AccountMFAEnabled -> "accountmfaenabled"
        AccountSigningCertificatesPresent -> "accountsigningcertificatespresent"
        AttachedPoliciesPerGroupQuota -> "attachedpoliciespergroupquota"
        AttachedPoliciesPerRoleQuota -> "attachedpoliciesperrolequota"
        AttachedPoliciesPerUserQuota -> "attachedpoliciesperuserquota"
        GroupPolicySizeQuota -> "grouppolicysizequota"
        Groups -> "groups"
        GroupsPerUserQuota -> "groupsperuserquota"
        GroupsQuota -> "groupsquota"
        MFADevices -> "mfadevices"
        MFADevicesInUse -> "mfadevicesinuse"
        Policies -> "policies"
        PoliciesQuota -> "policiesquota"
        PolicySizeQuota -> "policysizequota"
        PolicyVersionsInUse -> "policyversionsinuse"
        PolicyVersionsInUseQuota -> "policyversionsinusequota"
        ServerCertificates -> "servercertificates"
        ServerCertificatesQuota -> "servercertificatesquota"
        SigningCertificatesPerUserQuota -> "signingcertificatesperuserquota"
        UserPolicySizeQuota -> "userpolicysizequota"
        Users -> "users"
        UsersQuota -> "usersquota"
        VersionsPerPolicyQuota -> "versionsperpolicyquota"

instance Hashable SummaryKeyType
instance ToQuery SummaryKeyType
instance ToHeader SummaryKeyType

instance FromXML SummaryKeyType where
    parseXML = parseXMLText "SummaryKeyType"

-- | Contains information about an AWS access key.
--
-- This data type is used as a response element in the CreateAccessKey and
-- ListAccessKeys actions.
--
-- The @SecretAccessKey@ value is returned only in response to
-- CreateAccessKey. You can get a secret access key only when you first
-- create an access key; you cannot recover the secret access key later. If
-- you lose a secret access key, you must create a new access key.
--
-- /See:/ 'accessKey' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'akCreateDate'
--
-- * 'akUserName'
--
-- * 'akAccessKeyId'
--
-- * 'akStatus'
--
-- * 'akSecretAccessKey'
data AccessKey = AccessKey'
    { _akCreateDate      :: !(Maybe ISO8601)
    , _akUserName        :: !Text
    , _akAccessKeyId     :: !Text
    , _akStatus          :: !StatusType
    , _akSecretAccessKey :: !(Sensitive Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AccessKey' smart constructor.
accessKey :: Text -> Text -> StatusType -> Text -> AccessKey
accessKey pUserName pAccessKeyId pStatus pSecretAccessKey =
    AccessKey'
    { _akCreateDate = Nothing
    , _akUserName = pUserName
    , _akAccessKeyId = pAccessKeyId
    , _akStatus = pStatus
    , _akSecretAccessKey = _Sensitive # pSecretAccessKey
    }

-- | The date when the access key was created.
akCreateDate :: Lens' AccessKey (Maybe UTCTime)
akCreateDate = lens _akCreateDate (\ s a -> s{_akCreateDate = a}) . mapping _Time;

-- | The name of the IAM user that the access key is associated with.
akUserName :: Lens' AccessKey Text
akUserName = lens _akUserName (\ s a -> s{_akUserName = a});

-- | The ID for this access key.
akAccessKeyId :: Lens' AccessKey Text
akAccessKeyId = lens _akAccessKeyId (\ s a -> s{_akAccessKeyId = a});

-- | The status of the access key. @Active@ means the key is valid for API
-- calls, while @Inactive@ means it is not.
akStatus :: Lens' AccessKey StatusType
akStatus = lens _akStatus (\ s a -> s{_akStatus = a});

-- | The secret key used to sign requests.
akSecretAccessKey :: Lens' AccessKey Text
akSecretAccessKey = lens _akSecretAccessKey (\ s a -> s{_akSecretAccessKey = a}) . _Sensitive;

instance FromXML AccessKey where
        parseXML x
          = AccessKey' <$>
              (x .@? "CreateDate") <*> (x .@ "UserName") <*>
                (x .@ "AccessKeyId")
                <*> (x .@ "Status")
                <*> (x .@ "SecretAccessKey")

-- | Contains information about the last time an AWS access key was used.
--
-- This data type is used as a response element in the GetAccessKeyLastUsed
-- action.
--
-- /See:/ 'accessKeyLastUsed' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'akluLastUsedDate'
--
-- * 'akluServiceName'
--
-- * 'akluRegion'
data AccessKeyLastUsed = AccessKeyLastUsed'
    { _akluLastUsedDate :: !ISO8601
    , _akluServiceName  :: !Text
    , _akluRegion       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AccessKeyLastUsed' smart constructor.
accessKeyLastUsed :: UTCTime -> Text -> Text -> AccessKeyLastUsed
accessKeyLastUsed pLastUsedDate pServiceName pRegion =
    AccessKeyLastUsed'
    { _akluLastUsedDate = _Time # pLastUsedDate
    , _akluServiceName = pServiceName
    , _akluRegion = pRegion
    }

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- access key was most recently used. This field is null when:
--
-- -   The user does not have an access key.
--
-- -   An access key exists but has never been used, at least not since IAM
--     started tracking this information on April 22nd, 2015.
--
-- -   There is no sign-in data associated with the user
--
akluLastUsedDate :: Lens' AccessKeyLastUsed UTCTime
akluLastUsedDate = lens _akluLastUsedDate (\ s a -> s{_akluLastUsedDate = a}) . _Time;

-- | The name of the AWS service with which this access key was most recently
-- used. This field is null when:
--
-- -   The user does not have an access key.
--
-- -   An access key exists but has never been used, at least not since IAM
--     started tracking this information on April 22nd, 2015.
--
-- -   There is no sign-in data associated with the user
--
akluServiceName :: Lens' AccessKeyLastUsed Text
akluServiceName = lens _akluServiceName (\ s a -> s{_akluServiceName = a});

-- | The AWS region where this access key was most recently used. This field
-- is null when:
--
-- -   The user does not have an access key.
--
-- -   An access key exists but has never been used, at least not since IAM
--     started tracking this information on April 22nd, 2015.
--
-- -   There is no sign-in data associated with the user
--
-- For more information about AWS regions, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>
-- in the Amazon Web Services General Reference.
akluRegion :: Lens' AccessKeyLastUsed Text
akluRegion = lens _akluRegion (\ s a -> s{_akluRegion = a});

instance FromXML AccessKeyLastUsed where
        parseXML x
          = AccessKeyLastUsed' <$>
              (x .@ "LastUsedDate") <*> (x .@ "ServiceName") <*>
                (x .@ "Region")

-- | Contains information about an AWS access key, without its secret key.
--
-- This data type is used as a response element in the ListAccessKeys
-- action.
--
-- /See:/ 'accessKeyMetadata' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'akmStatus'
--
-- * 'akmCreateDate'
--
-- * 'akmUserName'
--
-- * 'akmAccessKeyId'
data AccessKeyMetadata = AccessKeyMetadata'
    { _akmStatus      :: !(Maybe StatusType)
    , _akmCreateDate  :: !(Maybe ISO8601)
    , _akmUserName    :: !(Maybe Text)
    , _akmAccessKeyId :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AccessKeyMetadata' smart constructor.
accessKeyMetadata :: AccessKeyMetadata
accessKeyMetadata =
    AccessKeyMetadata'
    { _akmStatus = Nothing
    , _akmCreateDate = Nothing
    , _akmUserName = Nothing
    , _akmAccessKeyId = Nothing
    }

-- | The status of the access key. @Active@ means the key is valid for API
-- calls; @Inactive@ means it is not.
akmStatus :: Lens' AccessKeyMetadata (Maybe StatusType)
akmStatus = lens _akmStatus (\ s a -> s{_akmStatus = a});

-- | The date when the access key was created.
akmCreateDate :: Lens' AccessKeyMetadata (Maybe UTCTime)
akmCreateDate = lens _akmCreateDate (\ s a -> s{_akmCreateDate = a}) . mapping _Time;

-- | The name of the IAM user that the key is associated with.
akmUserName :: Lens' AccessKeyMetadata (Maybe Text)
akmUserName = lens _akmUserName (\ s a -> s{_akmUserName = a});

-- | The ID for this access key.
akmAccessKeyId :: Lens' AccessKeyMetadata (Maybe Text)
akmAccessKeyId = lens _akmAccessKeyId (\ s a -> s{_akmAccessKeyId = a});

instance FromXML AccessKeyMetadata where
        parseXML x
          = AccessKeyMetadata' <$>
              (x .@? "Status") <*> (x .@? "CreateDate") <*>
                (x .@? "UserName")
                <*> (x .@? "AccessKeyId")

-- | Contains information about an attached policy.
--
-- An attached policy is a managed policy that has been attached to a user,
-- group, or role. This data type is used as a response element in the
-- ListAttachedGroupPolicies, ListAttachedRolePolicies,
-- ListAttachedUserPolicies, and GetAccountAuthorizationDetails actions.
--
-- For more information about managed policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- /See:/ 'attachedPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apPolicyName'
--
-- * 'apPolicyARN'
data AttachedPolicy = AttachedPolicy'
    { _apPolicyName :: !(Maybe Text)
    , _apPolicyARN  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachedPolicy' smart constructor.
attachedPolicy :: AttachedPolicy
attachedPolicy =
    AttachedPolicy'
    { _apPolicyName = Nothing
    , _apPolicyARN = Nothing
    }

-- | The friendly name of the attached policy.
apPolicyName :: Lens' AttachedPolicy (Maybe Text)
apPolicyName = lens _apPolicyName (\ s a -> s{_apPolicyName = a});

-- | FIXME: Undocumented member.
apPolicyARN :: Lens' AttachedPolicy (Maybe Text)
apPolicyARN = lens _apPolicyARN (\ s a -> s{_apPolicyARN = a});

instance FromXML AttachedPolicy where
        parseXML x
          = AttachedPolicy' <$>
              (x .@? "PolicyName") <*> (x .@? "PolicyArn")

-- | Contains information about an IAM group entity.
--
-- This data type is used as a response element in the following actions:
--
-- -   CreateGroup
-- -   GetGroup
-- -   ListGroups
--
-- /See:/ 'group'' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'groPath'
--
-- * 'groGroupName'
--
-- * 'groGroupId'
--
-- * 'groARN'
--
-- * 'groCreateDate'
data Group = Group'
    { _groPath       :: !Text
    , _groGroupName  :: !Text
    , _groGroupId    :: !Text
    , _groARN        :: !Text
    , _groCreateDate :: !ISO8601
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Group' smart constructor.
group' :: Text -> Text -> Text -> Text -> UTCTime -> Group
group' pPath pGroupName pGroupId pARN pCreateDate =
    Group'
    { _groPath = pPath
    , _groGroupName = pGroupName
    , _groGroupId = pGroupId
    , _groARN = pARN
    , _groCreateDate = _Time # pCreateDate
    }

-- | The path to the group. For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
groPath :: Lens' Group Text
groPath = lens _groPath (\ s a -> s{_groPath = a});

-- | The friendly name that identifies the group.
groGroupName :: Lens' Group Text
groGroupName = lens _groGroupName (\ s a -> s{_groGroupName = a});

-- | The stable and unique string identifying the group. For more information
-- about IDs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
groGroupId :: Lens' Group Text
groGroupId = lens _groGroupId (\ s a -> s{_groGroupId = a});

-- | The Amazon Resource Name (ARN) specifying the group. For more
-- information about ARNs and how to use them in policies, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
groARN :: Lens' Group Text
groARN = lens _groARN (\ s a -> s{_groARN = a});

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- group was created.
groCreateDate :: Lens' Group UTCTime
groCreateDate = lens _groCreateDate (\ s a -> s{_groCreateDate = a}) . _Time;

instance FromXML Group where
        parseXML x
          = Group' <$>
              (x .@ "Path") <*> (x .@ "GroupName") <*>
                (x .@ "GroupId")
                <*> (x .@ "Arn")
                <*> (x .@ "CreateDate")

-- | Contains information about an IAM group, including all of the group\'s
-- policies.
--
-- This data type is used as a response element in the
-- GetAccountAuthorizationDetails action.
--
-- /See:/ 'groupDetail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdARN'
--
-- * 'gdPath'
--
-- * 'gdCreateDate'
--
-- * 'gdGroupId'
--
-- * 'gdGroupPolicyList'
--
-- * 'gdGroupName'
--
-- * 'gdAttachedManagedPolicies'
data GroupDetail = GroupDetail'
    { _gdARN                     :: !(Maybe Text)
    , _gdPath                    :: !(Maybe Text)
    , _gdCreateDate              :: !(Maybe ISO8601)
    , _gdGroupId                 :: !(Maybe Text)
    , _gdGroupPolicyList         :: !(Maybe [PolicyDetail])
    , _gdGroupName               :: !(Maybe Text)
    , _gdAttachedManagedPolicies :: !(Maybe [AttachedPolicy])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GroupDetail' smart constructor.
groupDetail :: GroupDetail
groupDetail =
    GroupDetail'
    { _gdARN = Nothing
    , _gdPath = Nothing
    , _gdCreateDate = Nothing
    , _gdGroupId = Nothing
    , _gdGroupPolicyList = Nothing
    , _gdGroupName = Nothing
    , _gdAttachedManagedPolicies = Nothing
    }

-- | FIXME: Undocumented member.
gdARN :: Lens' GroupDetail (Maybe Text)
gdARN = lens _gdARN (\ s a -> s{_gdARN = a});

-- | The path to the group. For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
gdPath :: Lens' GroupDetail (Maybe Text)
gdPath = lens _gdPath (\ s a -> s{_gdPath = a});

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- group was created.
gdCreateDate :: Lens' GroupDetail (Maybe UTCTime)
gdCreateDate = lens _gdCreateDate (\ s a -> s{_gdCreateDate = a}) . mapping _Time;

-- | The stable and unique string identifying the group. For more information
-- about IDs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
gdGroupId :: Lens' GroupDetail (Maybe Text)
gdGroupId = lens _gdGroupId (\ s a -> s{_gdGroupId = a});

-- | A list of the inline policies embedded in the group.
gdGroupPolicyList :: Lens' GroupDetail [PolicyDetail]
gdGroupPolicyList = lens _gdGroupPolicyList (\ s a -> s{_gdGroupPolicyList = a}) . _Default;

-- | The friendly name that identifies the group.
gdGroupName :: Lens' GroupDetail (Maybe Text)
gdGroupName = lens _gdGroupName (\ s a -> s{_gdGroupName = a});

-- | A list of the managed policies attached to the group.
gdAttachedManagedPolicies :: Lens' GroupDetail [AttachedPolicy]
gdAttachedManagedPolicies = lens _gdAttachedManagedPolicies (\ s a -> s{_gdAttachedManagedPolicies = a}) . _Default;

instance FromXML GroupDetail where
        parseXML x
          = GroupDetail' <$>
              (x .@? "Arn") <*> (x .@? "Path") <*>
                (x .@? "CreateDate")
                <*> (x .@? "GroupId")
                <*>
                (x .@? "GroupPolicyList" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "GroupName")
                <*>
                (x .@? "AttachedManagedPolicies" .!@ mempty >>=
                   may (parseXMLList "member"))

-- | Contains information about an instance profile.
--
-- This data type is used as a response element in the following actions:
--
-- -   CreateInstanceProfile
--
-- -   GetInstanceProfile
--
-- -   ListInstanceProfiles
--
-- -   ListInstanceProfilesForRole
--
--
-- /See:/ 'instanceProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ipPath'
--
-- * 'ipInstanceProfileName'
--
-- * 'ipInstanceProfileId'
--
-- * 'ipARN'
--
-- * 'ipCreateDate'
--
-- * 'ipRoles'
data InstanceProfile = InstanceProfile'
    { _ipPath                :: !Text
    , _ipInstanceProfileName :: !Text
    , _ipInstanceProfileId   :: !Text
    , _ipARN                 :: !Text
    , _ipCreateDate          :: !ISO8601
    , _ipRoles               :: ![Role]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'InstanceProfile' smart constructor.
instanceProfile :: Text -> Text -> Text -> Text -> UTCTime -> InstanceProfile
instanceProfile pPath pInstanceProfileName pInstanceProfileId pARN pCreateDate =
    InstanceProfile'
    { _ipPath = pPath
    , _ipInstanceProfileName = pInstanceProfileName
    , _ipInstanceProfileId = pInstanceProfileId
    , _ipARN = pARN
    , _ipCreateDate = _Time # pCreateDate
    , _ipRoles = mempty
    }

-- | The path to the instance profile. For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
ipPath :: Lens' InstanceProfile Text
ipPath = lens _ipPath (\ s a -> s{_ipPath = a});

-- | The name identifying the instance profile.
ipInstanceProfileName :: Lens' InstanceProfile Text
ipInstanceProfileName = lens _ipInstanceProfileName (\ s a -> s{_ipInstanceProfileName = a});

-- | The stable and unique string identifying the instance profile. For more
-- information about IDs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
ipInstanceProfileId :: Lens' InstanceProfile Text
ipInstanceProfileId = lens _ipInstanceProfileId (\ s a -> s{_ipInstanceProfileId = a});

-- | The Amazon Resource Name (ARN) specifying the instance profile. For more
-- information about ARNs and how to use them in policies, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
ipARN :: Lens' InstanceProfile Text
ipARN = lens _ipARN (\ s a -> s{_ipARN = a});

-- | The date when the instance profile was created.
ipCreateDate :: Lens' InstanceProfile UTCTime
ipCreateDate = lens _ipCreateDate (\ s a -> s{_ipCreateDate = a}) . _Time;

-- | The role associated with the instance profile.
ipRoles :: Lens' InstanceProfile [Role]
ipRoles = lens _ipRoles (\ s a -> s{_ipRoles = a});

instance FromXML InstanceProfile where
        parseXML x
          = InstanceProfile' <$>
              (x .@ "Path") <*> (x .@ "InstanceProfileName") <*>
                (x .@ "InstanceProfileId")
                <*> (x .@ "Arn")
                <*> (x .@ "CreateDate")
                <*>
                (x .@? "Roles" .!@ mempty >>= parseXMLList "member")

-- | Contains the user name and password create date for a user.
--
-- This data type is used as a response element in the CreateLoginProfile
-- and GetLoginProfile actions.
--
-- /See:/ 'loginProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpPasswordResetRequired'
--
-- * 'lpUserName'
--
-- * 'lpCreateDate'
data LoginProfile = LoginProfile'
    { _lpPasswordResetRequired :: !(Maybe Bool)
    , _lpUserName              :: !Text
    , _lpCreateDate            :: !ISO8601
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'LoginProfile' smart constructor.
loginProfile :: Text -> UTCTime -> LoginProfile
loginProfile pUserName pCreateDate =
    LoginProfile'
    { _lpPasswordResetRequired = Nothing
    , _lpUserName = pUserName
    , _lpCreateDate = _Time # pCreateDate
    }

-- | Specifies whether the user is required to set a new password on next
-- sign-in.
lpPasswordResetRequired :: Lens' LoginProfile (Maybe Bool)
lpPasswordResetRequired = lens _lpPasswordResetRequired (\ s a -> s{_lpPasswordResetRequired = a});

-- | The name of the user, which can be used for signing in to the AWS
-- Management Console.
lpUserName :: Lens' LoginProfile Text
lpUserName = lens _lpUserName (\ s a -> s{_lpUserName = a});

-- | The date when the password for the user was created.
lpCreateDate :: Lens' LoginProfile UTCTime
lpCreateDate = lens _lpCreateDate (\ s a -> s{_lpCreateDate = a}) . _Time;

instance FromXML LoginProfile where
        parseXML x
          = LoginProfile' <$>
              (x .@? "PasswordResetRequired") <*> (x .@ "UserName")
                <*> (x .@ "CreateDate")

-- | Contains information about an MFA device.
--
-- This data type is used as a response element in the ListMFADevices
-- action.
--
-- /See:/ 'mfaDevice' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdUserName'
--
-- * 'mdSerialNumber'
--
-- * 'mdEnableDate'
data MFADevice = MFADevice'
    { _mdUserName     :: !Text
    , _mdSerialNumber :: !Text
    , _mdEnableDate   :: !ISO8601
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'MFADevice' smart constructor.
mfaDevice :: Text -> Text -> UTCTime -> MFADevice
mfaDevice pUserName pSerialNumber pEnableDate =
    MFADevice'
    { _mdUserName = pUserName
    , _mdSerialNumber = pSerialNumber
    , _mdEnableDate = _Time # pEnableDate
    }

-- | The user with whom the MFA device is associated.
mdUserName :: Lens' MFADevice Text
mdUserName = lens _mdUserName (\ s a -> s{_mdUserName = a});

-- | The serial number that uniquely identifies the MFA device. For virtual
-- MFA devices, the serial number is the device ARN.
mdSerialNumber :: Lens' MFADevice Text
mdSerialNumber = lens _mdSerialNumber (\ s a -> s{_mdSerialNumber = a});

-- | The date when the MFA device was enabled for the user.
mdEnableDate :: Lens' MFADevice UTCTime
mdEnableDate = lens _mdEnableDate (\ s a -> s{_mdEnableDate = a}) . _Time;

instance FromXML MFADevice where
        parseXML x
          = MFADevice' <$>
              (x .@ "UserName") <*> (x .@ "SerialNumber") <*>
                (x .@ "EnableDate")

-- | Contains information about a managed policy, including the policy\'s
-- ARN, versions, and the number of principal entities (users, groups, and
-- roles) that the policy is attached to.
--
-- This data type is used as a response element in the
-- GetAccountAuthorizationDetails action.
--
-- For more information about managed policies, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- /See:/ 'managedPolicyDetail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mpdPolicyName'
--
-- * 'mpdARN'
--
-- * 'mpdPath'
--
-- * 'mpdUpdateDate'
--
-- * 'mpdPolicyId'
--
-- * 'mpdCreateDate'
--
-- * 'mpdPolicyVersionList'
--
-- * 'mpdIsAttachable'
--
-- * 'mpdDefaultVersionId'
--
-- * 'mpdAttachmentCount'
--
-- * 'mpdDescription'
data ManagedPolicyDetail = ManagedPolicyDetail'
    { _mpdPolicyName        :: !(Maybe Text)
    , _mpdARN               :: !(Maybe Text)
    , _mpdPath              :: !(Maybe Text)
    , _mpdUpdateDate        :: !(Maybe ISO8601)
    , _mpdPolicyId          :: !(Maybe Text)
    , _mpdCreateDate        :: !(Maybe ISO8601)
    , _mpdPolicyVersionList :: !(Maybe [PolicyVersion])
    , _mpdIsAttachable      :: !(Maybe Bool)
    , _mpdDefaultVersionId  :: !(Maybe Text)
    , _mpdAttachmentCount   :: !(Maybe Int)
    , _mpdDescription       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ManagedPolicyDetail' smart constructor.
managedPolicyDetail :: ManagedPolicyDetail
managedPolicyDetail =
    ManagedPolicyDetail'
    { _mpdPolicyName = Nothing
    , _mpdARN = Nothing
    , _mpdPath = Nothing
    , _mpdUpdateDate = Nothing
    , _mpdPolicyId = Nothing
    , _mpdCreateDate = Nothing
    , _mpdPolicyVersionList = Nothing
    , _mpdIsAttachable = Nothing
    , _mpdDefaultVersionId = Nothing
    , _mpdAttachmentCount = Nothing
    , _mpdDescription = Nothing
    }

-- | The friendly name (not ARN) identifying the policy.
mpdPolicyName :: Lens' ManagedPolicyDetail (Maybe Text)
mpdPolicyName = lens _mpdPolicyName (\ s a -> s{_mpdPolicyName = a});

-- | FIXME: Undocumented member.
mpdARN :: Lens' ManagedPolicyDetail (Maybe Text)
mpdARN = lens _mpdARN (\ s a -> s{_mpdARN = a});

-- | The path to the policy.
--
-- For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
mpdPath :: Lens' ManagedPolicyDetail (Maybe Text)
mpdPath = lens _mpdPath (\ s a -> s{_mpdPath = a});

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy was last updated.
--
-- When a policy has only one version, this field contains the date and
-- time when the policy was created. When a policy has more than one
-- version, this field contains the date and time when the most recent
-- policy version was created.
mpdUpdateDate :: Lens' ManagedPolicyDetail (Maybe UTCTime)
mpdUpdateDate = lens _mpdUpdateDate (\ s a -> s{_mpdUpdateDate = a}) . mapping _Time;

-- | The stable and unique string identifying the policy.
--
-- For more information about IDs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
mpdPolicyId :: Lens' ManagedPolicyDetail (Maybe Text)
mpdPolicyId = lens _mpdPolicyId (\ s a -> s{_mpdPolicyId = a});

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy was created.
mpdCreateDate :: Lens' ManagedPolicyDetail (Maybe UTCTime)
mpdCreateDate = lens _mpdCreateDate (\ s a -> s{_mpdCreateDate = a}) . mapping _Time;

-- | A list containing information about the versions of the policy.
mpdPolicyVersionList :: Lens' ManagedPolicyDetail [PolicyVersion]
mpdPolicyVersionList = lens _mpdPolicyVersionList (\ s a -> s{_mpdPolicyVersionList = a}) . _Default;

-- | Specifies whether the policy can be attached to an IAM user, group, or
-- role.
mpdIsAttachable :: Lens' ManagedPolicyDetail (Maybe Bool)
mpdIsAttachable = lens _mpdIsAttachable (\ s a -> s{_mpdIsAttachable = a});

-- | The identifier for the version of the policy that is set as the default
-- (operative) version.
--
-- For more information about policy versions, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies>
-- in the /Using IAM/ guide.
mpdDefaultVersionId :: Lens' ManagedPolicyDetail (Maybe Text)
mpdDefaultVersionId = lens _mpdDefaultVersionId (\ s a -> s{_mpdDefaultVersionId = a});

-- | The number of principal entities (users, groups, and roles) that the
-- policy is attached to.
mpdAttachmentCount :: Lens' ManagedPolicyDetail (Maybe Int)
mpdAttachmentCount = lens _mpdAttachmentCount (\ s a -> s{_mpdAttachmentCount = a});

-- | A friendly description of the policy.
mpdDescription :: Lens' ManagedPolicyDetail (Maybe Text)
mpdDescription = lens _mpdDescription (\ s a -> s{_mpdDescription = a});

instance FromXML ManagedPolicyDetail where
        parseXML x
          = ManagedPolicyDetail' <$>
              (x .@? "PolicyName") <*> (x .@? "Arn") <*>
                (x .@? "Path")
                <*> (x .@? "UpdateDate")
                <*> (x .@? "PolicyId")
                <*> (x .@? "CreateDate")
                <*>
                (x .@? "PolicyVersionList" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "IsAttachable")
                <*> (x .@? "DefaultVersionId")
                <*> (x .@? "AttachmentCount")
                <*> (x .@? "Description")

-- | Contains the Amazon Resource Name (ARN) for an IAM OpenID Connect
-- provider.
--
-- /See:/ 'openIDConnectProviderListEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'oidcpleARN'
newtype OpenIDConnectProviderListEntry = OpenIDConnectProviderListEntry'
    { _oidcpleARN :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'OpenIDConnectProviderListEntry' smart constructor.
openIDConnectProviderListEntry :: OpenIDConnectProviderListEntry
openIDConnectProviderListEntry =
    OpenIDConnectProviderListEntry'
    { _oidcpleARN = Nothing
    }

-- | FIXME: Undocumented member.
oidcpleARN :: Lens' OpenIDConnectProviderListEntry (Maybe Text)
oidcpleARN = lens _oidcpleARN (\ s a -> s{_oidcpleARN = a});

instance FromXML OpenIDConnectProviderListEntry where
        parseXML x
          = OpenIDConnectProviderListEntry' <$> (x .@? "Arn")

-- | Contains information about the account password policy.
--
-- This data type is used as a response element in the
-- GetAccountPasswordPolicy action.
--
-- /See:/ 'passwordPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ppExpirePasswords'
--
-- * 'ppRequireNumbers'
--
-- * 'ppMinimumPasswordLength'
--
-- * 'ppPasswordReusePrevention'
--
-- * 'ppRequireLowercaseCharacters'
--
-- * 'ppMaxPasswordAge'
--
-- * 'ppHardExpiry'
--
-- * 'ppRequireSymbols'
--
-- * 'ppRequireUppercaseCharacters'
--
-- * 'ppAllowUsersToChangePassword'
data PasswordPolicy = PasswordPolicy'
    { _ppExpirePasswords            :: !(Maybe Bool)
    , _ppRequireNumbers             :: !(Maybe Bool)
    , _ppMinimumPasswordLength      :: !(Maybe Nat)
    , _ppPasswordReusePrevention    :: !(Maybe Nat)
    , _ppRequireLowercaseCharacters :: !(Maybe Bool)
    , _ppMaxPasswordAge             :: !(Maybe Nat)
    , _ppHardExpiry                 :: !(Maybe Bool)
    , _ppRequireSymbols             :: !(Maybe Bool)
    , _ppRequireUppercaseCharacters :: !(Maybe Bool)
    , _ppAllowUsersToChangePassword :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PasswordPolicy' smart constructor.
passwordPolicy :: PasswordPolicy
passwordPolicy =
    PasswordPolicy'
    { _ppExpirePasswords = Nothing
    , _ppRequireNumbers = Nothing
    , _ppMinimumPasswordLength = Nothing
    , _ppPasswordReusePrevention = Nothing
    , _ppRequireLowercaseCharacters = Nothing
    , _ppMaxPasswordAge = Nothing
    , _ppHardExpiry = Nothing
    , _ppRequireSymbols = Nothing
    , _ppRequireUppercaseCharacters = Nothing
    , _ppAllowUsersToChangePassword = Nothing
    }

-- | Specifies whether IAM users are required to change their password after
-- a specified number of days.
ppExpirePasswords :: Lens' PasswordPolicy (Maybe Bool)
ppExpirePasswords = lens _ppExpirePasswords (\ s a -> s{_ppExpirePasswords = a});

-- | Specifies whether to require numbers for IAM user passwords.
ppRequireNumbers :: Lens' PasswordPolicy (Maybe Bool)
ppRequireNumbers = lens _ppRequireNumbers (\ s a -> s{_ppRequireNumbers = a});

-- | Minimum length to require for IAM user passwords.
ppMinimumPasswordLength :: Lens' PasswordPolicy (Maybe Natural)
ppMinimumPasswordLength = lens _ppMinimumPasswordLength (\ s a -> s{_ppMinimumPasswordLength = a}) . mapping _Nat;

-- | Specifies the number of previous passwords that IAM users are prevented
-- from reusing.
ppPasswordReusePrevention :: Lens' PasswordPolicy (Maybe Natural)
ppPasswordReusePrevention = lens _ppPasswordReusePrevention (\ s a -> s{_ppPasswordReusePrevention = a}) . mapping _Nat;

-- | Specifies whether to require lowercase characters for IAM user
-- passwords.
ppRequireLowercaseCharacters :: Lens' PasswordPolicy (Maybe Bool)
ppRequireLowercaseCharacters = lens _ppRequireLowercaseCharacters (\ s a -> s{_ppRequireLowercaseCharacters = a});

-- | The number of days that an IAM user password is valid.
ppMaxPasswordAge :: Lens' PasswordPolicy (Maybe Natural)
ppMaxPasswordAge = lens _ppMaxPasswordAge (\ s a -> s{_ppMaxPasswordAge = a}) . mapping _Nat;

-- | Specifies whether IAM users are prevented from setting a new password
-- after their password has expired.
ppHardExpiry :: Lens' PasswordPolicy (Maybe Bool)
ppHardExpiry = lens _ppHardExpiry (\ s a -> s{_ppHardExpiry = a});

-- | Specifies whether to require symbols for IAM user passwords.
ppRequireSymbols :: Lens' PasswordPolicy (Maybe Bool)
ppRequireSymbols = lens _ppRequireSymbols (\ s a -> s{_ppRequireSymbols = a});

-- | Specifies whether to require uppercase characters for IAM user
-- passwords.
ppRequireUppercaseCharacters :: Lens' PasswordPolicy (Maybe Bool)
ppRequireUppercaseCharacters = lens _ppRequireUppercaseCharacters (\ s a -> s{_ppRequireUppercaseCharacters = a});

-- | Specifies whether IAM users are allowed to change their own password.
ppAllowUsersToChangePassword :: Lens' PasswordPolicy (Maybe Bool)
ppAllowUsersToChangePassword = lens _ppAllowUsersToChangePassword (\ s a -> s{_ppAllowUsersToChangePassword = a});

instance FromXML PasswordPolicy where
        parseXML x
          = PasswordPolicy' <$>
              (x .@? "ExpirePasswords") <*>
                (x .@? "RequireNumbers")
                <*> (x .@? "MinimumPasswordLength")
                <*> (x .@? "PasswordReusePrevention")
                <*> (x .@? "RequireLowercaseCharacters")
                <*> (x .@? "MaxPasswordAge")
                <*> (x .@? "HardExpiry")
                <*> (x .@? "RequireSymbols")
                <*> (x .@? "RequireUppercaseCharacters")
                <*> (x .@? "AllowUsersToChangePassword")

-- | Contains information about a managed policy.
--
-- This data type is used as a response element in the CreatePolicy,
-- GetPolicy, and ListPolicies actions.
--
-- For more information about managed policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- /See:/ 'policy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'polPolicyName'
--
-- * 'polARN'
--
-- * 'polPath'
--
-- * 'polUpdateDate'
--
-- * 'polPolicyId'
--
-- * 'polCreateDate'
--
-- * 'polIsAttachable'
--
-- * 'polDefaultVersionId'
--
-- * 'polAttachmentCount'
--
-- * 'polDescription'
data Policy = Policy'
    { _polPolicyName       :: !(Maybe Text)
    , _polARN              :: !(Maybe Text)
    , _polPath             :: !(Maybe Text)
    , _polUpdateDate       :: !(Maybe ISO8601)
    , _polPolicyId         :: !(Maybe Text)
    , _polCreateDate       :: !(Maybe ISO8601)
    , _polIsAttachable     :: !(Maybe Bool)
    , _polDefaultVersionId :: !(Maybe Text)
    , _polAttachmentCount  :: !(Maybe Int)
    , _polDescription      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Policy' smart constructor.
policy :: Policy
policy =
    Policy'
    { _polPolicyName = Nothing
    , _polARN = Nothing
    , _polPath = Nothing
    , _polUpdateDate = Nothing
    , _polPolicyId = Nothing
    , _polCreateDate = Nothing
    , _polIsAttachable = Nothing
    , _polDefaultVersionId = Nothing
    , _polAttachmentCount = Nothing
    , _polDescription = Nothing
    }

-- | The friendly name (not ARN) identifying the policy.
polPolicyName :: Lens' Policy (Maybe Text)
polPolicyName = lens _polPolicyName (\ s a -> s{_polPolicyName = a});

-- | FIXME: Undocumented member.
polARN :: Lens' Policy (Maybe Text)
polARN = lens _polARN (\ s a -> s{_polARN = a});

-- | The path to the policy.
--
-- For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
polPath :: Lens' Policy (Maybe Text)
polPath = lens _polPath (\ s a -> s{_polPath = a});

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy was last updated.
--
-- When a policy has only one version, this field contains the date and
-- time when the policy was created. When a policy has more than one
-- version, this field contains the date and time when the most recent
-- policy version was created.
polUpdateDate :: Lens' Policy (Maybe UTCTime)
polUpdateDate = lens _polUpdateDate (\ s a -> s{_polUpdateDate = a}) . mapping _Time;

-- | The stable and unique string identifying the policy.
--
-- For more information about IDs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
polPolicyId :: Lens' Policy (Maybe Text)
polPolicyId = lens _polPolicyId (\ s a -> s{_polPolicyId = a});

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy was created.
polCreateDate :: Lens' Policy (Maybe UTCTime)
polCreateDate = lens _polCreateDate (\ s a -> s{_polCreateDate = a}) . mapping _Time;

-- | Specifies whether the policy can be attached to an IAM user, group, or
-- role.
polIsAttachable :: Lens' Policy (Maybe Bool)
polIsAttachable = lens _polIsAttachable (\ s a -> s{_polIsAttachable = a});

-- | The identifier for the version of the policy that is set as the default
-- version.
polDefaultVersionId :: Lens' Policy (Maybe Text)
polDefaultVersionId = lens _polDefaultVersionId (\ s a -> s{_polDefaultVersionId = a});

-- | The number of entities (users, groups, and roles) that the policy is
-- attached to.
polAttachmentCount :: Lens' Policy (Maybe Int)
polAttachmentCount = lens _polAttachmentCount (\ s a -> s{_polAttachmentCount = a});

-- | A friendly description of the policy.
--
-- This element is included in the response to the GetPolicy operation. It
-- is not included in the response to the ListPolicies operation.
polDescription :: Lens' Policy (Maybe Text)
polDescription = lens _polDescription (\ s a -> s{_polDescription = a});

instance FromXML Policy where
        parseXML x
          = Policy' <$>
              (x .@? "PolicyName") <*> (x .@? "Arn") <*>
                (x .@? "Path")
                <*> (x .@? "UpdateDate")
                <*> (x .@? "PolicyId")
                <*> (x .@? "CreateDate")
                <*> (x .@? "IsAttachable")
                <*> (x .@? "DefaultVersionId")
                <*> (x .@? "AttachmentCount")
                <*> (x .@? "Description")

-- | Contains information about an IAM policy, including the policy document.
--
-- This data type is used as a response element in the
-- GetAccountAuthorizationDetails action.
--
-- /See:/ 'policyDetail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pdPolicyDocument'
--
-- * 'pdPolicyName'
data PolicyDetail = PolicyDetail'
    { _pdPolicyDocument :: !(Maybe Text)
    , _pdPolicyName     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PolicyDetail' smart constructor.
policyDetail :: PolicyDetail
policyDetail =
    PolicyDetail'
    { _pdPolicyDocument = Nothing
    , _pdPolicyName = Nothing
    }

-- | The policy document.
pdPolicyDocument :: Lens' PolicyDetail (Maybe Text)
pdPolicyDocument = lens _pdPolicyDocument (\ s a -> s{_pdPolicyDocument = a});

-- | The name of the policy.
pdPolicyName :: Lens' PolicyDetail (Maybe Text)
pdPolicyName = lens _pdPolicyName (\ s a -> s{_pdPolicyName = a});

instance FromXML PolicyDetail where
        parseXML x
          = PolicyDetail' <$>
              (x .@? "PolicyDocument") <*> (x .@? "PolicyName")

-- | Contains information about a group that a managed policy is attached to.
--
-- This data type is used as a response element in the
-- ListEntitiesForPolicy action.
--
-- For more information about managed policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- /See:/ 'policyGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pgGroupName'
newtype PolicyGroup = PolicyGroup'
    { _pgGroupName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PolicyGroup' smart constructor.
policyGroup :: PolicyGroup
policyGroup =
    PolicyGroup'
    { _pgGroupName = Nothing
    }

-- | The name (friendly name, not ARN) identifying the group.
pgGroupName :: Lens' PolicyGroup (Maybe Text)
pgGroupName = lens _pgGroupName (\ s a -> s{_pgGroupName = a});

instance FromXML PolicyGroup where
        parseXML x = PolicyGroup' <$> (x .@? "GroupName")

-- | Contains information about a role that a managed policy is attached to.
--
-- This data type is used as a response element in the
-- ListEntitiesForPolicy action.
--
-- For more information about managed policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- /See:/ 'policyRole' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prRoleName'
newtype PolicyRole = PolicyRole'
    { _prRoleName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PolicyRole' smart constructor.
policyRole :: PolicyRole
policyRole =
    PolicyRole'
    { _prRoleName = Nothing
    }

-- | The name (friendly name, not ARN) identifying the role.
prRoleName :: Lens' PolicyRole (Maybe Text)
prRoleName = lens _prRoleName (\ s a -> s{_prRoleName = a});

instance FromXML PolicyRole where
        parseXML x = PolicyRole' <$> (x .@? "RoleName")

-- | Contains information about a user that a managed policy is attached to.
--
-- This data type is used as a response element in the
-- ListEntitiesForPolicy action.
--
-- For more information about managed policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- /See:/ 'policyUser' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'puUserName'
newtype PolicyUser = PolicyUser'
    { _puUserName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PolicyUser' smart constructor.
policyUser :: PolicyUser
policyUser =
    PolicyUser'
    { _puUserName = Nothing
    }

-- | The name (friendly name, not ARN) identifying the user.
puUserName :: Lens' PolicyUser (Maybe Text)
puUserName = lens _puUserName (\ s a -> s{_puUserName = a});

instance FromXML PolicyUser where
        parseXML x = PolicyUser' <$> (x .@? "UserName")

-- | Contains information about a version of a managed policy.
--
-- This data type is used as a response element in the CreatePolicyVersion,
-- GetPolicyVersion, ListPolicyVersions, and GetAccountAuthorizationDetails
-- actions.
--
-- For more information about managed policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- /See:/ 'policyVersion' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pvVersionId'
--
-- * 'pvCreateDate'
--
-- * 'pvDocument'
--
-- * 'pvIsDefaultVersion'
data PolicyVersion = PolicyVersion'
    { _pvVersionId        :: !(Maybe Text)
    , _pvCreateDate       :: !(Maybe ISO8601)
    , _pvDocument         :: !(Maybe Text)
    , _pvIsDefaultVersion :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PolicyVersion' smart constructor.
policyVersion :: PolicyVersion
policyVersion =
    PolicyVersion'
    { _pvVersionId = Nothing
    , _pvCreateDate = Nothing
    , _pvDocument = Nothing
    , _pvIsDefaultVersion = Nothing
    }

-- | The identifier for the policy version.
--
-- Policy version identifiers always begin with @v@ (always lowercase).
-- When a policy is created, the first policy version is @v1@.
pvVersionId :: Lens' PolicyVersion (Maybe Text)
pvVersionId = lens _pvVersionId (\ s a -> s{_pvVersionId = a});

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy version was created.
pvCreateDate :: Lens' PolicyVersion (Maybe UTCTime)
pvCreateDate = lens _pvCreateDate (\ s a -> s{_pvCreateDate = a}) . mapping _Time;

-- | The policy document.
--
-- The policy document is returned in the response to the GetPolicyVersion
-- and GetAccountAuthorizationDetails operations. It is not returned in the
-- response to the CreatePolicyVersion or ListPolicyVersions operations.
pvDocument :: Lens' PolicyVersion (Maybe Text)
pvDocument = lens _pvDocument (\ s a -> s{_pvDocument = a});

-- | Specifies whether the policy version is set as the policy\'s default
-- version.
pvIsDefaultVersion :: Lens' PolicyVersion (Maybe Bool)
pvIsDefaultVersion = lens _pvIsDefaultVersion (\ s a -> s{_pvIsDefaultVersion = a});

instance FromXML PolicyVersion where
        parseXML x
          = PolicyVersion' <$>
              (x .@? "VersionId") <*> (x .@? "CreateDate") <*>
                (x .@? "Document")
                <*> (x .@? "IsDefaultVersion")

-- | Contains information about an IAM role.
--
-- This data type is used as a response element in the following actions:
--
-- -   CreateRole
--
-- -   GetRole
--
-- -   ListRoles
--
--
-- /See:/ 'role' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rolAssumeRolePolicyDocument'
--
-- * 'rolPath'
--
-- * 'rolRoleName'
--
-- * 'rolRoleId'
--
-- * 'rolARN'
--
-- * 'rolCreateDate'
data Role = Role'
    { _rolAssumeRolePolicyDocument :: !(Maybe Text)
    , _rolPath                     :: !Text
    , _rolRoleName                 :: !Text
    , _rolRoleId                   :: !Text
    , _rolARN                      :: !Text
    , _rolCreateDate               :: !ISO8601
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Role' smart constructor.
role :: Text -> Text -> Text -> Text -> UTCTime -> Role
role pPath pRoleName pRoleId pARN pCreateDate =
    Role'
    { _rolAssumeRolePolicyDocument = Nothing
    , _rolPath = pPath
    , _rolRoleName = pRoleName
    , _rolRoleId = pRoleId
    , _rolARN = pARN
    , _rolCreateDate = _Time # pCreateDate
    }

-- | The policy that grants an entity permission to assume the role.
rolAssumeRolePolicyDocument :: Lens' Role (Maybe Text)
rolAssumeRolePolicyDocument = lens _rolAssumeRolePolicyDocument (\ s a -> s{_rolAssumeRolePolicyDocument = a});

-- | The path to the role. For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
rolPath :: Lens' Role Text
rolPath = lens _rolPath (\ s a -> s{_rolPath = a});

-- | The friendly name that identifies the role.
rolRoleName :: Lens' Role Text
rolRoleName = lens _rolRoleName (\ s a -> s{_rolRoleName = a});

-- | The stable and unique string identifying the role. For more information
-- about IDs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
rolRoleId :: Lens' Role Text
rolRoleId = lens _rolRoleId (\ s a -> s{_rolRoleId = a});

-- | The Amazon Resource Name (ARN) specifying the role. For more information
-- about ARNs and how to use them in policies, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
rolARN :: Lens' Role Text
rolARN = lens _rolARN (\ s a -> s{_rolARN = a});

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- role was created.
rolCreateDate :: Lens' Role UTCTime
rolCreateDate = lens _rolCreateDate (\ s a -> s{_rolCreateDate = a}) . _Time;

instance FromXML Role where
        parseXML x
          = Role' <$>
              (x .@? "AssumeRolePolicyDocument") <*> (x .@ "Path")
                <*> (x .@ "RoleName")
                <*> (x .@ "RoleId")
                <*> (x .@ "Arn")
                <*> (x .@ "CreateDate")

-- | Contains information about an IAM role, including all of the role\'s
-- policies.
--
-- This data type is used as a response element in the
-- GetAccountAuthorizationDetails action.
--
-- /See:/ 'roleDetail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdAssumeRolePolicyDocument'
--
-- * 'rdARN'
--
-- * 'rdPath'
--
-- * 'rdInstanceProfileList'
--
-- * 'rdCreateDate'
--
-- * 'rdRoleName'
--
-- * 'rdRoleId'
--
-- * 'rdRolePolicyList'
--
-- * 'rdAttachedManagedPolicies'
data RoleDetail = RoleDetail'
    { _rdAssumeRolePolicyDocument :: !(Maybe Text)
    , _rdARN                      :: !(Maybe Text)
    , _rdPath                     :: !(Maybe Text)
    , _rdInstanceProfileList      :: !(Maybe [InstanceProfile])
    , _rdCreateDate               :: !(Maybe ISO8601)
    , _rdRoleName                 :: !(Maybe Text)
    , _rdRoleId                   :: !(Maybe Text)
    , _rdRolePolicyList           :: !(Maybe [PolicyDetail])
    , _rdAttachedManagedPolicies  :: !(Maybe [AttachedPolicy])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RoleDetail' smart constructor.
roleDetail :: RoleDetail
roleDetail =
    RoleDetail'
    { _rdAssumeRolePolicyDocument = Nothing
    , _rdARN = Nothing
    , _rdPath = Nothing
    , _rdInstanceProfileList = Nothing
    , _rdCreateDate = Nothing
    , _rdRoleName = Nothing
    , _rdRoleId = Nothing
    , _rdRolePolicyList = Nothing
    , _rdAttachedManagedPolicies = Nothing
    }

-- | The trust policy that grants permission to assume the role.
rdAssumeRolePolicyDocument :: Lens' RoleDetail (Maybe Text)
rdAssumeRolePolicyDocument = lens _rdAssumeRolePolicyDocument (\ s a -> s{_rdAssumeRolePolicyDocument = a});

-- | FIXME: Undocumented member.
rdARN :: Lens' RoleDetail (Maybe Text)
rdARN = lens _rdARN (\ s a -> s{_rdARN = a});

-- | The path to the role. For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
rdPath :: Lens' RoleDetail (Maybe Text)
rdPath = lens _rdPath (\ s a -> s{_rdPath = a});

-- | FIXME: Undocumented member.
rdInstanceProfileList :: Lens' RoleDetail [InstanceProfile]
rdInstanceProfileList = lens _rdInstanceProfileList (\ s a -> s{_rdInstanceProfileList = a}) . _Default;

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- role was created.
rdCreateDate :: Lens' RoleDetail (Maybe UTCTime)
rdCreateDate = lens _rdCreateDate (\ s a -> s{_rdCreateDate = a}) . mapping _Time;

-- | The friendly name that identifies the role.
rdRoleName :: Lens' RoleDetail (Maybe Text)
rdRoleName = lens _rdRoleName (\ s a -> s{_rdRoleName = a});

-- | The stable and unique string identifying the role. For more information
-- about IDs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
rdRoleId :: Lens' RoleDetail (Maybe Text)
rdRoleId = lens _rdRoleId (\ s a -> s{_rdRoleId = a});

-- | A list of inline policies embedded in the role. These policies are the
-- role\'s access (permissions) policies.
rdRolePolicyList :: Lens' RoleDetail [PolicyDetail]
rdRolePolicyList = lens _rdRolePolicyList (\ s a -> s{_rdRolePolicyList = a}) . _Default;

-- | A list of managed policies attached to the role. These policies are the
-- role\'s access (permissions) policies.
rdAttachedManagedPolicies :: Lens' RoleDetail [AttachedPolicy]
rdAttachedManagedPolicies = lens _rdAttachedManagedPolicies (\ s a -> s{_rdAttachedManagedPolicies = a}) . _Default;

instance FromXML RoleDetail where
        parseXML x
          = RoleDetail' <$>
              (x .@? "AssumeRolePolicyDocument") <*> (x .@? "Arn")
                <*> (x .@? "Path")
                <*>
                (x .@? "InstanceProfileList" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "CreateDate")
                <*> (x .@? "RoleName")
                <*> (x .@? "RoleId")
                <*>
                (x .@? "RolePolicyList" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "AttachedManagedPolicies" .!@ mempty >>=
                   may (parseXMLList "member"))

-- | Contains the list of SAML providers for this account.
--
-- /See:/ 'sAMLProviderListEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'samlpleARN'
--
-- * 'samlpleCreateDate'
--
-- * 'samlpleValidUntil'
data SAMLProviderListEntry = SAMLProviderListEntry'
    { _samlpleARN        :: !(Maybe Text)
    , _samlpleCreateDate :: !(Maybe ISO8601)
    , _samlpleValidUntil :: !(Maybe ISO8601)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SAMLProviderListEntry' smart constructor.
sAMLProviderListEntry :: SAMLProviderListEntry
sAMLProviderListEntry =
    SAMLProviderListEntry'
    { _samlpleARN = Nothing
    , _samlpleCreateDate = Nothing
    , _samlpleValidUntil = Nothing
    }

-- | The Amazon Resource Name (ARN) of the SAML provider.
samlpleARN :: Lens' SAMLProviderListEntry (Maybe Text)
samlpleARN = lens _samlpleARN (\ s a -> s{_samlpleARN = a});

-- | The date and time when the SAML provider was created.
samlpleCreateDate :: Lens' SAMLProviderListEntry (Maybe UTCTime)
samlpleCreateDate = lens _samlpleCreateDate (\ s a -> s{_samlpleCreateDate = a}) . mapping _Time;

-- | The expiration date and time for the SAML provider.
samlpleValidUntil :: Lens' SAMLProviderListEntry (Maybe UTCTime)
samlpleValidUntil = lens _samlpleValidUntil (\ s a -> s{_samlpleValidUntil = a}) . mapping _Time;

instance FromXML SAMLProviderListEntry where
        parseXML x
          = SAMLProviderListEntry' <$>
              (x .@? "Arn") <*> (x .@? "CreateDate") <*>
                (x .@? "ValidUntil")

-- | Contains information about a server certificate.
--
-- This data type is used as a response element in the GetServerCertificate
-- action.
--
-- /See:/ 'serverCertificate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'serCertificateChain'
--
-- * 'serServerCertificateMetadata'
--
-- * 'serCertificateBody'
data ServerCertificate = ServerCertificate'
    { _serCertificateChain          :: !(Maybe Text)
    , _serServerCertificateMetadata :: !ServerCertificateMetadata
    , _serCertificateBody           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ServerCertificate' smart constructor.
serverCertificate :: ServerCertificateMetadata -> Text -> ServerCertificate
serverCertificate pServerCertificateMetadata pCertificateBody =
    ServerCertificate'
    { _serCertificateChain = Nothing
    , _serServerCertificateMetadata = pServerCertificateMetadata
    , _serCertificateBody = pCertificateBody
    }

-- | The contents of the public key certificate chain.
serCertificateChain :: Lens' ServerCertificate (Maybe Text)
serCertificateChain = lens _serCertificateChain (\ s a -> s{_serCertificateChain = a});

-- | The meta information of the server certificate, such as its name, path,
-- ID, and ARN.
serServerCertificateMetadata :: Lens' ServerCertificate ServerCertificateMetadata
serServerCertificateMetadata = lens _serServerCertificateMetadata (\ s a -> s{_serServerCertificateMetadata = a});

-- | The contents of the public key certificate.
serCertificateBody :: Lens' ServerCertificate Text
serCertificateBody = lens _serCertificateBody (\ s a -> s{_serCertificateBody = a});

instance FromXML ServerCertificate where
        parseXML x
          = ServerCertificate' <$>
              (x .@? "CertificateChain") <*>
                (x .@ "ServerCertificateMetadata")
                <*> (x .@ "CertificateBody")

-- | Contains information about a server certificate without its certificate
-- body, certificate chain, and private key.
--
-- This data type is used as a response element in the
-- UploadServerCertificate and ListServerCertificates actions.
--
-- /See:/ 'serverCertificateMetadata' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scmUploadDate'
--
-- * 'scmExpiration'
--
-- * 'scmPath'
--
-- * 'scmServerCertificateName'
--
-- * 'scmServerCertificateId'
--
-- * 'scmARN'
data ServerCertificateMetadata = ServerCertificateMetadata'
    { _scmUploadDate            :: !(Maybe ISO8601)
    , _scmExpiration            :: !(Maybe ISO8601)
    , _scmPath                  :: !Text
    , _scmServerCertificateName :: !Text
    , _scmServerCertificateId   :: !Text
    , _scmARN                   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ServerCertificateMetadata' smart constructor.
serverCertificateMetadata :: Text -> Text -> Text -> Text -> ServerCertificateMetadata
serverCertificateMetadata pPath pServerCertificateName pServerCertificateId pARN =
    ServerCertificateMetadata'
    { _scmUploadDate = Nothing
    , _scmExpiration = Nothing
    , _scmPath = pPath
    , _scmServerCertificateName = pServerCertificateName
    , _scmServerCertificateId = pServerCertificateId
    , _scmARN = pARN
    }

-- | The date when the server certificate was uploaded.
scmUploadDate :: Lens' ServerCertificateMetadata (Maybe UTCTime)
scmUploadDate = lens _scmUploadDate (\ s a -> s{_scmUploadDate = a}) . mapping _Time;

-- | The date on which the certificate is set to expire.
scmExpiration :: Lens' ServerCertificateMetadata (Maybe UTCTime)
scmExpiration = lens _scmExpiration (\ s a -> s{_scmExpiration = a}) . mapping _Time;

-- | The path to the server certificate. For more information about paths,
-- see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
scmPath :: Lens' ServerCertificateMetadata Text
scmPath = lens _scmPath (\ s a -> s{_scmPath = a});

-- | The name that identifies the server certificate.
scmServerCertificateName :: Lens' ServerCertificateMetadata Text
scmServerCertificateName = lens _scmServerCertificateName (\ s a -> s{_scmServerCertificateName = a});

-- | The stable and unique string identifying the server certificate. For
-- more information about IDs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
scmServerCertificateId :: Lens' ServerCertificateMetadata Text
scmServerCertificateId = lens _scmServerCertificateId (\ s a -> s{_scmServerCertificateId = a});

-- | The Amazon Resource Name (ARN) specifying the server certificate. For
-- more information about ARNs and how to use them in policies, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
scmARN :: Lens' ServerCertificateMetadata Text
scmARN = lens _scmARN (\ s a -> s{_scmARN = a});

instance FromXML ServerCertificateMetadata where
        parseXML x
          = ServerCertificateMetadata' <$>
              (x .@? "UploadDate") <*> (x .@? "Expiration") <*>
                (x .@ "Path")
                <*> (x .@ "ServerCertificateName")
                <*> (x .@ "ServerCertificateId")
                <*> (x .@ "Arn")

-- | Contains information about an X.509 signing certificate.
--
-- This data type is used as a response element in the
-- UploadSigningCertificate and ListSigningCertificates actions.
--
-- /See:/ 'signingCertificate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scUploadDate'
--
-- * 'scUserName'
--
-- * 'scCertificateId'
--
-- * 'scCertificateBody'
--
-- * 'scStatus'
data SigningCertificate = SigningCertificate'
    { _scUploadDate      :: !(Maybe ISO8601)
    , _scUserName        :: !Text
    , _scCertificateId   :: !Text
    , _scCertificateBody :: !Text
    , _scStatus          :: !StatusType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SigningCertificate' smart constructor.
signingCertificate :: Text -> Text -> Text -> StatusType -> SigningCertificate
signingCertificate pUserName pCertificateId pCertificateBody pStatus =
    SigningCertificate'
    { _scUploadDate = Nothing
    , _scUserName = pUserName
    , _scCertificateId = pCertificateId
    , _scCertificateBody = pCertificateBody
    , _scStatus = pStatus
    }

-- | The date when the signing certificate was uploaded.
scUploadDate :: Lens' SigningCertificate (Maybe UTCTime)
scUploadDate = lens _scUploadDate (\ s a -> s{_scUploadDate = a}) . mapping _Time;

-- | The name of the user the signing certificate is associated with.
scUserName :: Lens' SigningCertificate Text
scUserName = lens _scUserName (\ s a -> s{_scUserName = a});

-- | The ID for the signing certificate.
scCertificateId :: Lens' SigningCertificate Text
scCertificateId = lens _scCertificateId (\ s a -> s{_scCertificateId = a});

-- | The contents of the signing certificate.
scCertificateBody :: Lens' SigningCertificate Text
scCertificateBody = lens _scCertificateBody (\ s a -> s{_scCertificateBody = a});

-- | The status of the signing certificate. @Active@ means the key is valid
-- for API calls, while @Inactive@ means it is not.
scStatus :: Lens' SigningCertificate StatusType
scStatus = lens _scStatus (\ s a -> s{_scStatus = a});

instance FromXML SigningCertificate where
        parseXML x
          = SigningCertificate' <$>
              (x .@? "UploadDate") <*> (x .@ "UserName") <*>
                (x .@ "CertificateId")
                <*> (x .@ "CertificateBody")
                <*> (x .@ "Status")

-- | Contains information about an IAM user entity.
--
-- This data type is used as a response element in the following actions:
--
-- -   CreateUser
--
-- -   GetUser
--
-- -   ListUsers
--
--
-- /See:/ 'user' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usePasswordLastUsed'
--
-- * 'usePath'
--
-- * 'useUserName'
--
-- * 'useUserId'
--
-- * 'useARN'
--
-- * 'useCreateDate'
data User = User'
    { _usePasswordLastUsed :: !(Maybe ISO8601)
    , _usePath             :: !Text
    , _useUserName         :: !Text
    , _useUserId           :: !Text
    , _useARN              :: !Text
    , _useCreateDate       :: !ISO8601
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'User' smart constructor.
user :: Text -> Text -> Text -> Text -> UTCTime -> User
user pPath pUserName pUserId pARN pCreateDate =
    User'
    { _usePasswordLastUsed = Nothing
    , _usePath = pPath
    , _useUserName = pUserName
    , _useUserId = pUserId
    , _useARN = pARN
    , _useCreateDate = _Time # pCreateDate
    }

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- user\'s password was last used to sign in to an AWS website. For a list
-- of AWS websites that capture a user\'s last sign-in time, see the
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Credential Reports>
-- topic in the /Using IAM/ guide. If a password is used more than once in
-- a five-minute span, only the first use is returned in this field. This
-- field is null (not present) when:
--
-- -   The user does not have a password
--
-- -   The password exists but has never been used (at least not since IAM
--     started tracking this information on October 20th, 2014
--
-- -   there is no sign-in data associated with the user
--
-- This value is returned only in the GetUser and ListUsers actions.
usePasswordLastUsed :: Lens' User (Maybe UTCTime)
usePasswordLastUsed = lens _usePasswordLastUsed (\ s a -> s{_usePasswordLastUsed = a}) . mapping _Time;

-- | The path to the user. For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
usePath :: Lens' User Text
usePath = lens _usePath (\ s a -> s{_usePath = a});

-- | The friendly name identifying the user.
useUserName :: Lens' User Text
useUserName = lens _useUserName (\ s a -> s{_useUserName = a});

-- | The stable and unique string identifying the user. For more information
-- about IDs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
useUserId :: Lens' User Text
useUserId = lens _useUserId (\ s a -> s{_useUserId = a});

-- | The Amazon Resource Name (ARN) that identifies the user. For more
-- information about ARNs and how to use ARNs in policies, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
useARN :: Lens' User Text
useARN = lens _useARN (\ s a -> s{_useARN = a});

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- user was created.
useCreateDate :: Lens' User UTCTime
useCreateDate = lens _useCreateDate (\ s a -> s{_useCreateDate = a}) . _Time;

instance FromXML User where
        parseXML x
          = User' <$>
              (x .@? "PasswordLastUsed") <*> (x .@ "Path") <*>
                (x .@ "UserName")
                <*> (x .@ "UserId")
                <*> (x .@ "Arn")
                <*> (x .@ "CreateDate")

-- | Contains information about an IAM user, including all the user\'s
-- policies and all the IAM groups the user is in.
--
-- This data type is used as a response element in the
-- GetAccountAuthorizationDetails action.
--
-- /See:/ 'userDetail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udARN'
--
-- * 'udPath'
--
-- * 'udGroupList'
--
-- * 'udCreateDate'
--
-- * 'udUserName'
--
-- * 'udUserId'
--
-- * 'udUserPolicyList'
--
-- * 'udAttachedManagedPolicies'
data UserDetail = UserDetail'
    { _udARN                     :: !(Maybe Text)
    , _udPath                    :: !(Maybe Text)
    , _udGroupList               :: !(Maybe [Text])
    , _udCreateDate              :: !(Maybe ISO8601)
    , _udUserName                :: !(Maybe Text)
    , _udUserId                  :: !(Maybe Text)
    , _udUserPolicyList          :: !(Maybe [PolicyDetail])
    , _udAttachedManagedPolicies :: !(Maybe [AttachedPolicy])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UserDetail' smart constructor.
userDetail :: UserDetail
userDetail =
    UserDetail'
    { _udARN = Nothing
    , _udPath = Nothing
    , _udGroupList = Nothing
    , _udCreateDate = Nothing
    , _udUserName = Nothing
    , _udUserId = Nothing
    , _udUserPolicyList = Nothing
    , _udAttachedManagedPolicies = Nothing
    }

-- | FIXME: Undocumented member.
udARN :: Lens' UserDetail (Maybe Text)
udARN = lens _udARN (\ s a -> s{_udARN = a});

-- | The path to the user. For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
udPath :: Lens' UserDetail (Maybe Text)
udPath = lens _udPath (\ s a -> s{_udPath = a});

-- | A list of IAM groups that the user is in.
udGroupList :: Lens' UserDetail [Text]
udGroupList = lens _udGroupList (\ s a -> s{_udGroupList = a}) . _Default;

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- user was created.
udCreateDate :: Lens' UserDetail (Maybe UTCTime)
udCreateDate = lens _udCreateDate (\ s a -> s{_udCreateDate = a}) . mapping _Time;

-- | The friendly name identifying the user.
udUserName :: Lens' UserDetail (Maybe Text)
udUserName = lens _udUserName (\ s a -> s{_udUserName = a});

-- | The stable and unique string identifying the user. For more information
-- about IDs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
udUserId :: Lens' UserDetail (Maybe Text)
udUserId = lens _udUserId (\ s a -> s{_udUserId = a});

-- | A list of the inline policies embedded in the user.
udUserPolicyList :: Lens' UserDetail [PolicyDetail]
udUserPolicyList = lens _udUserPolicyList (\ s a -> s{_udUserPolicyList = a}) . _Default;

-- | A list of the managed policies attached to the user.
udAttachedManagedPolicies :: Lens' UserDetail [AttachedPolicy]
udAttachedManagedPolicies = lens _udAttachedManagedPolicies (\ s a -> s{_udAttachedManagedPolicies = a}) . _Default;

instance FromXML UserDetail where
        parseXML x
          = UserDetail' <$>
              (x .@? "Arn") <*> (x .@? "Path") <*>
                (x .@? "GroupList" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "CreateDate")
                <*> (x .@? "UserName")
                <*> (x .@? "UserId")
                <*>
                (x .@? "UserPolicyList" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "AttachedManagedPolicies" .!@ mempty >>=
                   may (parseXMLList "member"))

-- | Contains information about a virtual MFA device.
--
-- /See:/ 'virtualMFADevice' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vmdQRCodePNG'
--
-- * 'vmdBase32StringSeed'
--
-- * 'vmdUser'
--
-- * 'vmdEnableDate'
--
-- * 'vmdSerialNumber'
data VirtualMFADevice = VirtualMFADevice'
    { _vmdQRCodePNG        :: !(Maybe (Sensitive Base64))
    , _vmdBase32StringSeed :: !(Maybe (Sensitive Base64))
    , _vmdUser             :: !(Maybe User)
    , _vmdEnableDate       :: !(Maybe ISO8601)
    , _vmdSerialNumber     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'VirtualMFADevice' smart constructor.
virtualMFADevice :: Text -> VirtualMFADevice
virtualMFADevice pSerialNumber =
    VirtualMFADevice'
    { _vmdQRCodePNG = Nothing
    , _vmdBase32StringSeed = Nothing
    , _vmdUser = Nothing
    , _vmdEnableDate = Nothing
    , _vmdSerialNumber = pSerialNumber
    }

-- | A QR code PNG image that encodes
-- @otpauth:\/\/totp\/$virtualMFADeviceName\@$AccountName?secret=$Base32String@
-- where @$virtualMFADeviceName@ is one of the create call arguments,
-- @AccountName@ is the user name if set (otherwise, the account ID
-- otherwise), and @Base32String@ is the seed in Base32 format. The
-- @Base32String@ value is Base64-encoded.
vmdQRCodePNG :: Lens' VirtualMFADevice (Maybe Base64)
vmdQRCodePNG = lens _vmdQRCodePNG (\ s a -> s{_vmdQRCodePNG = a}) . mapping _Sensitive;

-- | The Base32 seed defined as specified in
-- <http://www.ietf.org/rfc/rfc3548.txt RFC3548>. The @Base32StringSeed@ is
-- Base64-encoded.
vmdBase32StringSeed :: Lens' VirtualMFADevice (Maybe Base64)
vmdBase32StringSeed = lens _vmdBase32StringSeed (\ s a -> s{_vmdBase32StringSeed = a}) . mapping _Sensitive;

-- | FIXME: Undocumented member.
vmdUser :: Lens' VirtualMFADevice (Maybe User)
vmdUser = lens _vmdUser (\ s a -> s{_vmdUser = a});

-- | The date and time on which the virtual MFA device was enabled.
vmdEnableDate :: Lens' VirtualMFADevice (Maybe UTCTime)
vmdEnableDate = lens _vmdEnableDate (\ s a -> s{_vmdEnableDate = a}) . mapping _Time;

-- | The serial number associated with @VirtualMFADevice@.
vmdSerialNumber :: Lens' VirtualMFADevice Text
vmdSerialNumber = lens _vmdSerialNumber (\ s a -> s{_vmdSerialNumber = a});

instance FromXML VirtualMFADevice where
        parseXML x
          = VirtualMFADevice' <$>
              (x .@? "QRCodePNG") <*> (x .@? "Base32StringSeed")
                <*> (x .@? "User")
                <*> (x .@? "EnableDate")
                <*> (x .@ "SerialNumber")
