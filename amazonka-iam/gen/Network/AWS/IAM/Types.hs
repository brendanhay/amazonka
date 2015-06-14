{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.IAM.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.IAM.Types
    (
    -- * Service
      IAM
    -- ** Errors
    , RESTError

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

    -- * AssignmentStatusType
    , AssignmentStatusType (..)

    -- * AttachedPolicy
    , AttachedPolicy
    , attachedPolicy
    , apPolicyName
    , apPolicyARN

    -- * EntityType
    , EntityType (..)

    -- * Group
    , Group
    , group
    , groPath
    , groGroupName
    , groGroupId
    , groARN
    , groCreateDate

    -- * GroupDetail
    , GroupDetail
    , groupDetail
    , gdCreateDate
    , gdGroupPolicyList
    , gdAttachedManagedPolicies
    , gdARN
    , gdPath
    , gdGroupId
    , gdGroupName

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
    , mpdPath
    , mpdUpdateDate
    , mpdCreateDate
    , mpdPolicyVersionList
    , mpdIsAttachable
    , mpdDefaultVersionId
    , mpdAttachmentCount
    , mpdDescription
    , mpdPolicyName
    , mpdARN
    , mpdPolicyId

    -- * OpenIDConnectProviderListEntry
    , OpenIDConnectProviderListEntry
    , openIDConnectProviderListEntry
    , oidcpleARN

    -- * PasswordPolicy
    , PasswordPolicy
    , passwordPolicy
    , ppExpirePasswords
    , ppRequireNumbers
    , ppRequireLowercaseCharacters
    , ppHardExpiry
    , ppRequireSymbols
    , ppRequireUppercaseCharacters
    , ppAllowUsersToChangePassword
    , ppMinimumPasswordLength
    , ppPasswordReusePrevention
    , ppMaxPasswordAge

    -- * Policy
    , Policy
    , policy
    , polPath
    , polUpdateDate
    , polCreateDate
    , polIsAttachable
    , polDefaultVersionId
    , polAttachmentCount
    , polDescription
    , polPolicyName
    , polARN
    , polPolicyId

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

    -- * PolicyScopeType
    , PolicyScopeType (..)

    -- * PolicyUser
    , PolicyUser
    , policyUser
    , puUserName

    -- * PolicyVersion
    , PolicyVersion
    , policyVersion
    , pvVersionId
    , pvCreateDate
    , pvIsDefaultVersion
    , pvDocument

    -- * ReportFormatType
    , ReportFormatType (..)

    -- * ReportStateType
    , ReportStateType (..)

    -- * Role
    , Role
    , role
    , rolPath
    , rolRoleName
    , rolRoleId
    , rolARN
    , rolCreateDate
    , rolAssumeRolePolicyDocument

    -- * RoleDetail
    , RoleDetail
    , roleDetail
    , rdInstanceProfileList
    , rdCreateDate
    , rdRolePolicyList
    , rdAttachedManagedPolicies
    , rdAssumeRolePolicyDocument
    , rdARN
    , rdPath
    , rdRoleName
    , rdRoleId

    -- * SAMLProviderListEntry
    , SAMLProviderListEntry
    , sAMLProviderListEntry
    , samlpleCreateDate
    , samlpleValidUntil
    , samlpleARN

    -- * ServerCertificate
    , ServerCertificate
    , serverCertificate
    , serServerCertificateMetadata
    , serCertificateBody
    , serCertificateChain

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

    -- * StatusType
    , StatusType (..)

    -- * SummaryKeyType
    , SummaryKeyType (..)

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
    , udGroupList
    , udCreateDate
    , udUserPolicyList
    , udAttachedManagedPolicies
    , udARN
    , udPath
    , udUserName
    , udUserId

    -- * VirtualMFADevice
    , VirtualMFADevice
    , virtualMFADevice
    , vmdQRCodePNG
    , vmdBase32StringSeed
    , vmdUser
    , vmdEnableDate
    , vmdSerialNumber
    ) where

import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | Version @2010-05-08@ of the Amazon Identity and Access Management SDK.
data IAM

instance AWSService IAM where
    type Sg IAM = V4
    type Er IAM = RESTError

    service = service'
      where
        service' :: Service IAM
        service' = Service
            { _svcAbbrev  = "IAM"
            , _svcPrefix  = "iam"
            , _svcVersion = "2010-05-08"
            , _svcHandle  = handle
            , _svcRetry   = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError RESTError)
        handle = restError statusSuccess service'

        retry :: Retry IAM
        retry = undefined

        check :: Status
              -> RESTError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e) = undefined

-- | /See:/ 'accessKey' smart constructor.
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
data AccessKey = AccessKey'{_akCreateDate :: Maybe ISO8601, _akUserName :: Text, _akAccessKeyId :: Text, _akStatus :: StatusType, _akSecretAccessKey :: Sensitive Text} deriving (Eq, Read, Show)

-- | 'AccessKey' smart constructor.
accessKey :: Text -> Text -> StatusType -> Text -> AccessKey
accessKey pUserName pAccessKeyId pStatus pSecretAccessKey = AccessKey'{_akCreateDate = Nothing, _akUserName = pUserName, _akAccessKeyId = pAccessKeyId, _akStatus = pStatus, _akSecretAccessKey = _Sensitive # pSecretAccessKey};

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
              x .@? "CreateDate" <*> x .@ "UserName" <*>
                x .@ "AccessKeyId"
                <*> x .@ "Status"
                <*> x .@ "SecretAccessKey"

-- | /See:/ 'accessKeyLastUsed' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'akluLastUsedDate'
--
-- * 'akluServiceName'
--
-- * 'akluRegion'
data AccessKeyLastUsed = AccessKeyLastUsed'{_akluLastUsedDate :: ISO8601, _akluServiceName :: Text, _akluRegion :: Text} deriving (Eq, Read, Show)

-- | 'AccessKeyLastUsed' smart constructor.
accessKeyLastUsed :: UTCTime -> Text -> Text -> AccessKeyLastUsed
accessKeyLastUsed pLastUsedDate pServiceName pRegion = AccessKeyLastUsed'{_akluLastUsedDate = _Time # pLastUsedDate, _akluServiceName = pServiceName, _akluRegion = pRegion};

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- access key was most recently used.
akluLastUsedDate :: Lens' AccessKeyLastUsed UTCTime
akluLastUsedDate = lens _akluLastUsedDate (\ s a -> s{_akluLastUsedDate = a}) . _Time;

-- | The name of the AWS service with which this access key was most recently
-- used.
akluServiceName :: Lens' AccessKeyLastUsed Text
akluServiceName = lens _akluServiceName (\ s a -> s{_akluServiceName = a});

-- | The AWS region where this access key was most recently used.
--
-- For more information about AWS regions, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>
-- in the Amazon Web Services General Reference.
akluRegion :: Lens' AccessKeyLastUsed Text
akluRegion = lens _akluRegion (\ s a -> s{_akluRegion = a});

instance FromXML AccessKeyLastUsed where
        parseXML x
          = AccessKeyLastUsed' <$>
              x .@ "LastUsedDate" <*> x .@ "ServiceName" <*>
                x .@ "Region"

-- | /See:/ 'accessKeyMetadata' smart constructor.
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
data AccessKeyMetadata = AccessKeyMetadata'{_akmStatus :: Maybe StatusType, _akmCreateDate :: Maybe ISO8601, _akmUserName :: Text, _akmAccessKeyId :: Text} deriving (Eq, Read, Show)

-- | 'AccessKeyMetadata' smart constructor.
accessKeyMetadata :: Text -> Text -> AccessKeyMetadata
accessKeyMetadata pUserName pAccessKeyId = AccessKeyMetadata'{_akmStatus = Nothing, _akmCreateDate = Nothing, _akmUserName = pUserName, _akmAccessKeyId = pAccessKeyId};

-- | The status of the access key. @Active@ means the key is valid for API
-- calls; @Inactive@ means it is not.
akmStatus :: Lens' AccessKeyMetadata (Maybe StatusType)
akmStatus = lens _akmStatus (\ s a -> s{_akmStatus = a});

-- | The date when the access key was created.
akmCreateDate :: Lens' AccessKeyMetadata (Maybe UTCTime)
akmCreateDate = lens _akmCreateDate (\ s a -> s{_akmCreateDate = a}) . mapping _Time;

-- | The name of the IAM user that the key is associated with.
akmUserName :: Lens' AccessKeyMetadata Text
akmUserName = lens _akmUserName (\ s a -> s{_akmUserName = a});

-- | The ID for this access key.
akmAccessKeyId :: Lens' AccessKeyMetadata Text
akmAccessKeyId = lens _akmAccessKeyId (\ s a -> s{_akmAccessKeyId = a});

instance FromXML AccessKeyMetadata where
        parseXML x
          = AccessKeyMetadata' <$>
              x .@? "Status" <*> x .@? "CreateDate" <*>
                x .@ "UserName"
                <*> x .@ "AccessKeyId"

data AssignmentStatusType = Assigned | Unassigned | Any deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText AssignmentStatusType where
    parser = takeLowerText >>= \case
        "Any" -> pure Any
        "Assigned" -> pure Assigned
        "Unassigned" -> pure Unassigned
        e -> fail ("Failure parsing AssignmentStatusType from " ++ show e)

instance ToText AssignmentStatusType where
    toText = \case
        Any -> "Any"
        Assigned -> "Assigned"
        Unassigned -> "Unassigned"

instance Hashable AssignmentStatusType
instance ToQuery AssignmentStatusType
instance ToHeader AssignmentStatusType

-- | /See:/ 'attachedPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apPolicyName'
--
-- * 'apPolicyARN'
data AttachedPolicy = AttachedPolicy'{_apPolicyName :: Text, _apPolicyARN :: Text} deriving (Eq, Read, Show)

-- | 'AttachedPolicy' smart constructor.
attachedPolicy :: Text -> Text -> AttachedPolicy
attachedPolicy pPolicyName pPolicyARN = AttachedPolicy'{_apPolicyName = pPolicyName, _apPolicyARN = pPolicyARN};

-- | The friendly name of the attached policy.
apPolicyName :: Lens' AttachedPolicy Text
apPolicyName = lens _apPolicyName (\ s a -> s{_apPolicyName = a});

-- | FIXME: Undocumented member.
apPolicyARN :: Lens' AttachedPolicy Text
apPolicyARN = lens _apPolicyARN (\ s a -> s{_apPolicyARN = a});

instance FromXML AttachedPolicy where
        parseXML x
          = AttachedPolicy' <$>
              x .@ "PolicyName" <*> x .@ "PolicyArn"

data EntityType = Group | LocalManagedPolicy | AWSManagedPolicy | User | Role deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText EntityType where
    parser = takeLowerText >>= \case
        "AWSManagedPolicy" -> pure AWSManagedPolicy
        "Group" -> pure Group
        "LocalManagedPolicy" -> pure LocalManagedPolicy
        "Role" -> pure Role
        "User" -> pure User
        e -> fail ("Failure parsing EntityType from " ++ show e)

instance ToText EntityType where
    toText = \case
        AWSManagedPolicy -> "AWSManagedPolicy"
        Group -> "Group"
        LocalManagedPolicy -> "LocalManagedPolicy"
        Role -> "Role"
        User -> "User"

instance Hashable EntityType
instance ToQuery EntityType
instance ToHeader EntityType

-- | /See:/ 'group' smart constructor.
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
data Group = Group'{_groPath :: Text, _groGroupName :: Text, _groGroupId :: Text, _groARN :: Text, _groCreateDate :: ISO8601} deriving (Eq, Read, Show)

-- | 'Group' smart constructor.
group :: Text -> Text -> Text -> Text -> UTCTime -> Group
group pPath pGroupName pGroupId pARN pCreateDate = Group'{_groPath = pPath, _groGroupName = pGroupName, _groGroupId = pGroupId, _groARN = pARN, _groCreateDate = _Time # pCreateDate};

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
              x .@ "Path" <*> x .@ "GroupName" <*> x .@ "GroupId"
                <*> x .@ "Arn"
                <*> x .@ "CreateDate"

-- | /See:/ 'groupDetail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdCreateDate'
--
-- * 'gdGroupPolicyList'
--
-- * 'gdAttachedManagedPolicies'
--
-- * 'gdARN'
--
-- * 'gdPath'
--
-- * 'gdGroupId'
--
-- * 'gdGroupName'
data GroupDetail = GroupDetail'{_gdCreateDate :: Maybe ISO8601, _gdGroupPolicyList :: [PolicyDetail], _gdAttachedManagedPolicies :: [AttachedPolicy], _gdARN :: Text, _gdPath :: Text, _gdGroupId :: Text, _gdGroupName :: Text} deriving (Eq, Read, Show)

-- | 'GroupDetail' smart constructor.
groupDetail :: Text -> Text -> Text -> Text -> GroupDetail
groupDetail pARN pPath pGroupId pGroupName = GroupDetail'{_gdCreateDate = Nothing, _gdGroupPolicyList = mempty, _gdAttachedManagedPolicies = mempty, _gdARN = pARN, _gdPath = pPath, _gdGroupId = pGroupId, _gdGroupName = pGroupName};

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- group was created.
gdCreateDate :: Lens' GroupDetail (Maybe UTCTime)
gdCreateDate = lens _gdCreateDate (\ s a -> s{_gdCreateDate = a}) . mapping _Time;

-- | A list of the inline policies embedded in the group.
gdGroupPolicyList :: Lens' GroupDetail [PolicyDetail]
gdGroupPolicyList = lens _gdGroupPolicyList (\ s a -> s{_gdGroupPolicyList = a});

-- | A list of the managed policies attached to the group.
gdAttachedManagedPolicies :: Lens' GroupDetail [AttachedPolicy]
gdAttachedManagedPolicies = lens _gdAttachedManagedPolicies (\ s a -> s{_gdAttachedManagedPolicies = a});

-- | FIXME: Undocumented member.
gdARN :: Lens' GroupDetail Text
gdARN = lens _gdARN (\ s a -> s{_gdARN = a});

-- | The path to the group. For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
gdPath :: Lens' GroupDetail Text
gdPath = lens _gdPath (\ s a -> s{_gdPath = a});

-- | The stable and unique string identifying the group. For more information
-- about IDs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
gdGroupId :: Lens' GroupDetail Text
gdGroupId = lens _gdGroupId (\ s a -> s{_gdGroupId = a});

-- | The friendly name that identifies the group.
gdGroupName :: Lens' GroupDetail Text
gdGroupName = lens _gdGroupName (\ s a -> s{_gdGroupName = a});

instance FromXML GroupDetail where
        parseXML x
          = GroupDetail' <$>
              x .@? "CreateDate" <*>
                (x .@? "GroupPolicyList" .!@ mempty >>=
                   parseXMLList "member")
                <*>
                (x .@? "AttachedManagedPolicies" .!@ mempty >>=
                   parseXMLList "member")
                <*> x .@ "Arn"
                <*> x .@ "Path"
                <*> x .@ "GroupId"
                <*> x .@ "GroupName"

-- | /See:/ 'instanceProfile' smart constructor.
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
data InstanceProfile = InstanceProfile'{_ipPath :: Text, _ipInstanceProfileName :: Text, _ipInstanceProfileId :: Text, _ipARN :: Text, _ipCreateDate :: ISO8601, _ipRoles :: [Role]} deriving (Eq, Read, Show)

-- | 'InstanceProfile' smart constructor.
instanceProfile :: Text -> Text -> Text -> Text -> UTCTime -> [Role] -> InstanceProfile
instanceProfile pPath pInstanceProfileName pInstanceProfileId pARN pCreateDate pRoles = InstanceProfile'{_ipPath = pPath, _ipInstanceProfileName = pInstanceProfileName, _ipInstanceProfileId = pInstanceProfileId, _ipARN = pARN, _ipCreateDate = _Time # pCreateDate, _ipRoles = pRoles};

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
              x .@ "Path" <*> x .@ "InstanceProfileName" <*>
                x .@ "InstanceProfileId"
                <*> x .@ "Arn"
                <*> x .@ "CreateDate"
                <*>
                (x .@? "Roles" .!@ mempty >>= parseXMLList "member")

-- | /See:/ 'loginProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpPasswordResetRequired'
--
-- * 'lpUserName'
--
-- * 'lpCreateDate'
data LoginProfile = LoginProfile'{_lpPasswordResetRequired :: Maybe Bool, _lpUserName :: Text, _lpCreateDate :: ISO8601} deriving (Eq, Read, Show)

-- | 'LoginProfile' smart constructor.
loginProfile :: Text -> UTCTime -> LoginProfile
loginProfile pUserName pCreateDate = LoginProfile'{_lpPasswordResetRequired = Nothing, _lpUserName = pUserName, _lpCreateDate = _Time # pCreateDate};

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
              x .@? "PasswordResetRequired" <*> x .@ "UserName" <*>
                x .@ "CreateDate"

-- | /See:/ 'mfaDevice' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdUserName'
--
-- * 'mdSerialNumber'
--
-- * 'mdEnableDate'
data MFADevice = MFADevice'{_mdUserName :: Text, _mdSerialNumber :: Text, _mdEnableDate :: ISO8601} deriving (Eq, Read, Show)

-- | 'MFADevice' smart constructor.
mfaDevice :: Text -> Text -> UTCTime -> MFADevice
mfaDevice pUserName pSerialNumber pEnableDate = MFADevice'{_mdUserName = pUserName, _mdSerialNumber = pSerialNumber, _mdEnableDate = _Time # pEnableDate};

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
              x .@ "UserName" <*> x .@ "SerialNumber" <*>
                x .@ "EnableDate"

-- | /See:/ 'managedPolicyDetail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mpdPath'
--
-- * 'mpdUpdateDate'
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
--
-- * 'mpdPolicyName'
--
-- * 'mpdARN'
--
-- * 'mpdPolicyId'
data ManagedPolicyDetail = ManagedPolicyDetail'{_mpdPath :: Maybe Text, _mpdUpdateDate :: Maybe ISO8601, _mpdCreateDate :: Maybe ISO8601, _mpdPolicyVersionList :: [PolicyVersion], _mpdIsAttachable :: Maybe Bool, _mpdDefaultVersionId :: Maybe Text, _mpdAttachmentCount :: Maybe Int, _mpdDescription :: Maybe Text, _mpdPolicyName :: Text, _mpdARN :: Text, _mpdPolicyId :: Text} deriving (Eq, Read, Show)

-- | 'ManagedPolicyDetail' smart constructor.
managedPolicyDetail :: Text -> Text -> Text -> ManagedPolicyDetail
managedPolicyDetail pPolicyName pARN pPolicyId = ManagedPolicyDetail'{_mpdPath = Nothing, _mpdUpdateDate = Nothing, _mpdCreateDate = Nothing, _mpdPolicyVersionList = mempty, _mpdIsAttachable = Nothing, _mpdDefaultVersionId = Nothing, _mpdAttachmentCount = Nothing, _mpdDescription = Nothing, _mpdPolicyName = pPolicyName, _mpdARN = pARN, _mpdPolicyId = pPolicyId};

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

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy was created.
mpdCreateDate :: Lens' ManagedPolicyDetail (Maybe UTCTime)
mpdCreateDate = lens _mpdCreateDate (\ s a -> s{_mpdCreateDate = a}) . mapping _Time;

-- | A list containing information about the versions of the policy.
mpdPolicyVersionList :: Lens' ManagedPolicyDetail [PolicyVersion]
mpdPolicyVersionList = lens _mpdPolicyVersionList (\ s a -> s{_mpdPolicyVersionList = a});

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

-- | The friendly name (not ARN) identifying the policy.
mpdPolicyName :: Lens' ManagedPolicyDetail Text
mpdPolicyName = lens _mpdPolicyName (\ s a -> s{_mpdPolicyName = a});

-- | FIXME: Undocumented member.
mpdARN :: Lens' ManagedPolicyDetail Text
mpdARN = lens _mpdARN (\ s a -> s{_mpdARN = a});

-- | The stable and unique string identifying the policy.
--
-- For more information about IDs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
mpdPolicyId :: Lens' ManagedPolicyDetail Text
mpdPolicyId = lens _mpdPolicyId (\ s a -> s{_mpdPolicyId = a});

instance FromXML ManagedPolicyDetail where
        parseXML x
          = ManagedPolicyDetail' <$>
              x .@? "Path" <*> x .@? "UpdateDate" <*>
                x .@? "CreateDate"
                <*>
                (x .@? "PolicyVersionList" .!@ mempty >>=
                   parseXMLList "member")
                <*> x .@? "IsAttachable"
                <*> x .@? "DefaultVersionId"
                <*> x .@? "AttachmentCount"
                <*> x .@? "Description"
                <*> x .@ "PolicyName"
                <*> x .@ "Arn"
                <*> x .@ "PolicyId"

-- | /See:/ 'openIDConnectProviderListEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'oidcpleARN'
newtype OpenIDConnectProviderListEntry = OpenIDConnectProviderListEntry'{_oidcpleARN :: Text} deriving (Eq, Read, Show)

-- | 'OpenIDConnectProviderListEntry' smart constructor.
openIDConnectProviderListEntry :: Text -> OpenIDConnectProviderListEntry
openIDConnectProviderListEntry pARN = OpenIDConnectProviderListEntry'{_oidcpleARN = pARN};

-- | FIXME: Undocumented member.
oidcpleARN :: Lens' OpenIDConnectProviderListEntry Text
oidcpleARN = lens _oidcpleARN (\ s a -> s{_oidcpleARN = a});

instance FromXML OpenIDConnectProviderListEntry where
        parseXML x
          = OpenIDConnectProviderListEntry' <$> x .@ "Arn"

-- | /See:/ 'passwordPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ppExpirePasswords'
--
-- * 'ppRequireNumbers'
--
-- * 'ppRequireLowercaseCharacters'
--
-- * 'ppHardExpiry'
--
-- * 'ppRequireSymbols'
--
-- * 'ppRequireUppercaseCharacters'
--
-- * 'ppAllowUsersToChangePassword'
--
-- * 'ppMinimumPasswordLength'
--
-- * 'ppPasswordReusePrevention'
--
-- * 'ppMaxPasswordAge'
data PasswordPolicy = PasswordPolicy'{_ppExpirePasswords :: Maybe Bool, _ppRequireNumbers :: Maybe Bool, _ppRequireLowercaseCharacters :: Maybe Bool, _ppHardExpiry :: Maybe Bool, _ppRequireSymbols :: Maybe Bool, _ppRequireUppercaseCharacters :: Maybe Bool, _ppAllowUsersToChangePassword :: Maybe Bool, _ppMinimumPasswordLength :: Nat, _ppPasswordReusePrevention :: Nat, _ppMaxPasswordAge :: Nat} deriving (Eq, Read, Show)

-- | 'PasswordPolicy' smart constructor.
passwordPolicy :: Natural -> Natural -> Natural -> PasswordPolicy
passwordPolicy pMinimumPasswordLength pPasswordReusePrevention pMaxPasswordAge = PasswordPolicy'{_ppExpirePasswords = Nothing, _ppRequireNumbers = Nothing, _ppRequireLowercaseCharacters = Nothing, _ppHardExpiry = Nothing, _ppRequireSymbols = Nothing, _ppRequireUppercaseCharacters = Nothing, _ppAllowUsersToChangePassword = Nothing, _ppMinimumPasswordLength = _Nat # pMinimumPasswordLength, _ppPasswordReusePrevention = _Nat # pPasswordReusePrevention, _ppMaxPasswordAge = _Nat # pMaxPasswordAge};

-- | Specifies whether IAM users are required to change their password after
-- a specified number of days.
ppExpirePasswords :: Lens' PasswordPolicy (Maybe Bool)
ppExpirePasswords = lens _ppExpirePasswords (\ s a -> s{_ppExpirePasswords = a});

-- | Specifies whether to require numbers for IAM user passwords.
ppRequireNumbers :: Lens' PasswordPolicy (Maybe Bool)
ppRequireNumbers = lens _ppRequireNumbers (\ s a -> s{_ppRequireNumbers = a});

-- | Specifies whether to require lowercase characters for IAM user
-- passwords.
ppRequireLowercaseCharacters :: Lens' PasswordPolicy (Maybe Bool)
ppRequireLowercaseCharacters = lens _ppRequireLowercaseCharacters (\ s a -> s{_ppRequireLowercaseCharacters = a});

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

-- | Minimum length to require for IAM user passwords.
ppMinimumPasswordLength :: Lens' PasswordPolicy Natural
ppMinimumPasswordLength = lens _ppMinimumPasswordLength (\ s a -> s{_ppMinimumPasswordLength = a}) . _Nat;

-- | Specifies the number of previous passwords that IAM users are prevented
-- from reusing.
ppPasswordReusePrevention :: Lens' PasswordPolicy Natural
ppPasswordReusePrevention = lens _ppPasswordReusePrevention (\ s a -> s{_ppPasswordReusePrevention = a}) . _Nat;

-- | The number of days that an IAM user password is valid.
ppMaxPasswordAge :: Lens' PasswordPolicy Natural
ppMaxPasswordAge = lens _ppMaxPasswordAge (\ s a -> s{_ppMaxPasswordAge = a}) . _Nat;

instance FromXML PasswordPolicy where
        parseXML x
          = PasswordPolicy' <$>
              x .@? "ExpirePasswords" <*> x .@? "RequireNumbers"
                <*> x .@? "RequireLowercaseCharacters"
                <*> x .@? "HardExpiry"
                <*> x .@? "RequireSymbols"
                <*> x .@? "RequireUppercaseCharacters"
                <*> x .@? "AllowUsersToChangePassword"
                <*> x .@ "MinimumPasswordLength"
                <*> x .@ "PasswordReusePrevention"
                <*> x .@ "MaxPasswordAge"

-- | /See:/ 'policy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'polPath'
--
-- * 'polUpdateDate'
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
--
-- * 'polPolicyName'
--
-- * 'polARN'
--
-- * 'polPolicyId'
data Policy = Policy'{_polPath :: Maybe Text, _polUpdateDate :: Maybe ISO8601, _polCreateDate :: Maybe ISO8601, _polIsAttachable :: Maybe Bool, _polDefaultVersionId :: Maybe Text, _polAttachmentCount :: Maybe Int, _polDescription :: Maybe Text, _polPolicyName :: Text, _polARN :: Text, _polPolicyId :: Text} deriving (Eq, Read, Show)

-- | 'Policy' smart constructor.
policy :: Text -> Text -> Text -> Policy
policy pPolicyName pARN pPolicyId = Policy'{_polPath = Nothing, _polUpdateDate = Nothing, _polCreateDate = Nothing, _polIsAttachable = Nothing, _polDefaultVersionId = Nothing, _polAttachmentCount = Nothing, _polDescription = Nothing, _polPolicyName = pPolicyName, _polARN = pARN, _polPolicyId = pPolicyId};

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

-- | The friendly name (not ARN) identifying the policy.
polPolicyName :: Lens' Policy Text
polPolicyName = lens _polPolicyName (\ s a -> s{_polPolicyName = a});

-- | FIXME: Undocumented member.
polARN :: Lens' Policy Text
polARN = lens _polARN (\ s a -> s{_polARN = a});

-- | The stable and unique string identifying the policy.
--
-- For more information about IDs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
polPolicyId :: Lens' Policy Text
polPolicyId = lens _polPolicyId (\ s a -> s{_polPolicyId = a});

instance FromXML Policy where
        parseXML x
          = Policy' <$>
              x .@? "Path" <*> x .@? "UpdateDate" <*>
                x .@? "CreateDate"
                <*> x .@? "IsAttachable"
                <*> x .@? "DefaultVersionId"
                <*> x .@? "AttachmentCount"
                <*> x .@? "Description"
                <*> x .@ "PolicyName"
                <*> x .@ "Arn"
                <*> x .@ "PolicyId"

-- | /See:/ 'policyDetail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pdPolicyDocument'
--
-- * 'pdPolicyName'
data PolicyDetail = PolicyDetail'{_pdPolicyDocument :: Text, _pdPolicyName :: Text} deriving (Eq, Read, Show)

-- | 'PolicyDetail' smart constructor.
policyDetail :: Text -> Text -> PolicyDetail
policyDetail pPolicyDocument pPolicyName = PolicyDetail'{_pdPolicyDocument = pPolicyDocument, _pdPolicyName = pPolicyName};

-- | The policy document.
pdPolicyDocument :: Lens' PolicyDetail Text
pdPolicyDocument = lens _pdPolicyDocument (\ s a -> s{_pdPolicyDocument = a});

-- | The name of the policy.
pdPolicyName :: Lens' PolicyDetail Text
pdPolicyName = lens _pdPolicyName (\ s a -> s{_pdPolicyName = a});

instance FromXML PolicyDetail where
        parseXML x
          = PolicyDetail' <$>
              x .@ "PolicyDocument" <*> x .@ "PolicyName"

-- | /See:/ 'policyGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pgGroupName'
newtype PolicyGroup = PolicyGroup'{_pgGroupName :: Text} deriving (Eq, Read, Show)

-- | 'PolicyGroup' smart constructor.
policyGroup :: Text -> PolicyGroup
policyGroup pGroupName = PolicyGroup'{_pgGroupName = pGroupName};

-- | The name (friendly name, not ARN) identifying the group.
pgGroupName :: Lens' PolicyGroup Text
pgGroupName = lens _pgGroupName (\ s a -> s{_pgGroupName = a});

instance FromXML PolicyGroup where
        parseXML x = PolicyGroup' <$> x .@ "GroupName"

-- | /See:/ 'policyRole' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prRoleName'
newtype PolicyRole = PolicyRole'{_prRoleName :: Text} deriving (Eq, Read, Show)

-- | 'PolicyRole' smart constructor.
policyRole :: Text -> PolicyRole
policyRole pRoleName = PolicyRole'{_prRoleName = pRoleName};

-- | The name (friendly name, not ARN) identifying the role.
prRoleName :: Lens' PolicyRole Text
prRoleName = lens _prRoleName (\ s a -> s{_prRoleName = a});

instance FromXML PolicyRole where
        parseXML x = PolicyRole' <$> x .@ "RoleName"

data PolicyScopeType = AWS | Local | All deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText PolicyScopeType where
    parser = takeLowerText >>= \case
        "AWS" -> pure AWS
        "All" -> pure All
        "Local" -> pure Local
        e -> fail ("Failure parsing PolicyScopeType from " ++ show e)

instance ToText PolicyScopeType where
    toText = \case
        AWS -> "AWS"
        All -> "All"
        Local -> "Local"

instance Hashable PolicyScopeType
instance ToQuery PolicyScopeType
instance ToHeader PolicyScopeType

-- | /See:/ 'policyUser' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'puUserName'
newtype PolicyUser = PolicyUser'{_puUserName :: Text} deriving (Eq, Read, Show)

-- | 'PolicyUser' smart constructor.
policyUser :: Text -> PolicyUser
policyUser pUserName = PolicyUser'{_puUserName = pUserName};

-- | The name (friendly name, not ARN) identifying the user.
puUserName :: Lens' PolicyUser Text
puUserName = lens _puUserName (\ s a -> s{_puUserName = a});

instance FromXML PolicyUser where
        parseXML x = PolicyUser' <$> x .@ "UserName"

-- | /See:/ 'policyVersion' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pvVersionId'
--
-- * 'pvCreateDate'
--
-- * 'pvIsDefaultVersion'
--
-- * 'pvDocument'
data PolicyVersion = PolicyVersion'{_pvVersionId :: Maybe Text, _pvCreateDate :: Maybe ISO8601, _pvIsDefaultVersion :: Maybe Bool, _pvDocument :: Text} deriving (Eq, Read, Show)

-- | 'PolicyVersion' smart constructor.
policyVersion :: Text -> PolicyVersion
policyVersion pDocument = PolicyVersion'{_pvVersionId = Nothing, _pvCreateDate = Nothing, _pvIsDefaultVersion = Nothing, _pvDocument = pDocument};

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

-- | Specifies whether the policy version is set as the policy\'s default
-- version.
pvIsDefaultVersion :: Lens' PolicyVersion (Maybe Bool)
pvIsDefaultVersion = lens _pvIsDefaultVersion (\ s a -> s{_pvIsDefaultVersion = a});

-- | The policy document.
--
-- The policy document is returned in the response to the GetPolicyVersion
-- and GetAccountAuthorizationDetails operations. It is not returned in the
-- response to the CreatePolicyVersion or ListPolicyVersions operations.
pvDocument :: Lens' PolicyVersion Text
pvDocument = lens _pvDocument (\ s a -> s{_pvDocument = a});

instance FromXML PolicyVersion where
        parseXML x
          = PolicyVersion' <$>
              x .@? "VersionId" <*> x .@? "CreateDate" <*>
                x .@? "IsDefaultVersion"
                <*> x .@ "Document"

data ReportFormatType = TextCSV deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ReportFormatType where
    parser = takeLowerText >>= \case
        "text/csv" -> pure TextCSV
        e -> fail ("Failure parsing ReportFormatType from " ++ show e)

instance ToText ReportFormatType where
    toText = \case
        TextCSV -> "text/csv"

instance Hashable ReportFormatType
instance ToQuery ReportFormatType
instance ToHeader ReportFormatType

instance FromXML ReportFormatType where
    parseXML = parseXMLText "ReportFormatType"

data ReportStateType = Inprogress | Started | Complete deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ReportStateType where
    parser = takeLowerText >>= \case
        "COMPLETE" -> pure Complete
        "INPROGRESS" -> pure Inprogress
        "STARTED" -> pure Started
        e -> fail ("Failure parsing ReportStateType from " ++ show e)

instance ToText ReportStateType where
    toText = \case
        Complete -> "COMPLETE"
        Inprogress -> "INPROGRESS"
        Started -> "STARTED"

instance Hashable ReportStateType
instance ToQuery ReportStateType
instance ToHeader ReportStateType

instance FromXML ReportStateType where
    parseXML = parseXMLText "ReportStateType"

-- | /See:/ 'role' smart constructor.
--
-- The fields accessible through corresponding lenses are:
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
--
-- * 'rolAssumeRolePolicyDocument'
data Role = Role'{_rolPath :: Text, _rolRoleName :: Text, _rolRoleId :: Text, _rolARN :: Text, _rolCreateDate :: ISO8601, _rolAssumeRolePolicyDocument :: Text} deriving (Eq, Read, Show)

-- | 'Role' smart constructor.
role :: Text -> Text -> Text -> Text -> UTCTime -> Text -> Role
role pPath pRoleName pRoleId pARN pCreateDate pAssumeRolePolicyDocument = Role'{_rolPath = pPath, _rolRoleName = pRoleName, _rolRoleId = pRoleId, _rolARN = pARN, _rolCreateDate = _Time # pCreateDate, _rolAssumeRolePolicyDocument = pAssumeRolePolicyDocument};

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

-- | The policy that grants an entity permission to assume the role.
rolAssumeRolePolicyDocument :: Lens' Role Text
rolAssumeRolePolicyDocument = lens _rolAssumeRolePolicyDocument (\ s a -> s{_rolAssumeRolePolicyDocument = a});

instance FromXML Role where
        parseXML x
          = Role' <$>
              x .@ "Path" <*> x .@ "RoleName" <*> x .@ "RoleId" <*>
                x .@ "Arn"
                <*> x .@ "CreateDate"
                <*> x .@ "AssumeRolePolicyDocument"

-- | /See:/ 'roleDetail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdInstanceProfileList'
--
-- * 'rdCreateDate'
--
-- * 'rdRolePolicyList'
--
-- * 'rdAttachedManagedPolicies'
--
-- * 'rdAssumeRolePolicyDocument'
--
-- * 'rdARN'
--
-- * 'rdPath'
--
-- * 'rdRoleName'
--
-- * 'rdRoleId'
data RoleDetail = RoleDetail'{_rdInstanceProfileList :: [InstanceProfile], _rdCreateDate :: Maybe ISO8601, _rdRolePolicyList :: [PolicyDetail], _rdAttachedManagedPolicies :: [AttachedPolicy], _rdAssumeRolePolicyDocument :: Text, _rdARN :: Text, _rdPath :: Text, _rdRoleName :: Text, _rdRoleId :: Text} deriving (Eq, Read, Show)

-- | 'RoleDetail' smart constructor.
roleDetail :: Text -> Text -> Text -> Text -> Text -> RoleDetail
roleDetail pAssumeRolePolicyDocument pARN pPath pRoleName pRoleId = RoleDetail'{_rdInstanceProfileList = mempty, _rdCreateDate = Nothing, _rdRolePolicyList = mempty, _rdAttachedManagedPolicies = mempty, _rdAssumeRolePolicyDocument = pAssumeRolePolicyDocument, _rdARN = pARN, _rdPath = pPath, _rdRoleName = pRoleName, _rdRoleId = pRoleId};

-- | FIXME: Undocumented member.
rdInstanceProfileList :: Lens' RoleDetail [InstanceProfile]
rdInstanceProfileList = lens _rdInstanceProfileList (\ s a -> s{_rdInstanceProfileList = a});

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- role was created.
rdCreateDate :: Lens' RoleDetail (Maybe UTCTime)
rdCreateDate = lens _rdCreateDate (\ s a -> s{_rdCreateDate = a}) . mapping _Time;

-- | A list of inline policies embedded in the role. These policies are the
-- role\'s access (permissions) policies.
rdRolePolicyList :: Lens' RoleDetail [PolicyDetail]
rdRolePolicyList = lens _rdRolePolicyList (\ s a -> s{_rdRolePolicyList = a});

-- | A list of managed policies attached to the role. These policies are the
-- role\'s access (permissions) policies.
rdAttachedManagedPolicies :: Lens' RoleDetail [AttachedPolicy]
rdAttachedManagedPolicies = lens _rdAttachedManagedPolicies (\ s a -> s{_rdAttachedManagedPolicies = a});

-- | The trust policy that grants permission to assume the role.
rdAssumeRolePolicyDocument :: Lens' RoleDetail Text
rdAssumeRolePolicyDocument = lens _rdAssumeRolePolicyDocument (\ s a -> s{_rdAssumeRolePolicyDocument = a});

-- | FIXME: Undocumented member.
rdARN :: Lens' RoleDetail Text
rdARN = lens _rdARN (\ s a -> s{_rdARN = a});

-- | The path to the role. For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
rdPath :: Lens' RoleDetail Text
rdPath = lens _rdPath (\ s a -> s{_rdPath = a});

-- | The friendly name that identifies the role.
rdRoleName :: Lens' RoleDetail Text
rdRoleName = lens _rdRoleName (\ s a -> s{_rdRoleName = a});

-- | The stable and unique string identifying the role. For more information
-- about IDs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
rdRoleId :: Lens' RoleDetail Text
rdRoleId = lens _rdRoleId (\ s a -> s{_rdRoleId = a});

instance FromXML RoleDetail where
        parseXML x
          = RoleDetail' <$>
              (x .@? "InstanceProfileList" .!@ mempty >>=
                 parseXMLList "member")
                <*> x .@? "CreateDate"
                <*>
                (x .@? "RolePolicyList" .!@ mempty >>=
                   parseXMLList "member")
                <*>
                (x .@? "AttachedManagedPolicies" .!@ mempty >>=
                   parseXMLList "member")
                <*> x .@ "AssumeRolePolicyDocument"
                <*> x .@ "Arn"
                <*> x .@ "Path"
                <*> x .@ "RoleName"
                <*> x .@ "RoleId"

-- | /See:/ 'sAMLProviderListEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'samlpleCreateDate'
--
-- * 'samlpleValidUntil'
--
-- * 'samlpleARN'
data SAMLProviderListEntry = SAMLProviderListEntry'{_samlpleCreateDate :: Maybe ISO8601, _samlpleValidUntil :: Maybe ISO8601, _samlpleARN :: Text} deriving (Eq, Read, Show)

-- | 'SAMLProviderListEntry' smart constructor.
sAMLProviderListEntry :: Text -> SAMLProviderListEntry
sAMLProviderListEntry pARN = SAMLProviderListEntry'{_samlpleCreateDate = Nothing, _samlpleValidUntil = Nothing, _samlpleARN = pARN};

-- | The date and time when the SAML provider was created.
samlpleCreateDate :: Lens' SAMLProviderListEntry (Maybe UTCTime)
samlpleCreateDate = lens _samlpleCreateDate (\ s a -> s{_samlpleCreateDate = a}) . mapping _Time;

-- | The expiration date and time for the SAML provider.
samlpleValidUntil :: Lens' SAMLProviderListEntry (Maybe UTCTime)
samlpleValidUntil = lens _samlpleValidUntil (\ s a -> s{_samlpleValidUntil = a}) . mapping _Time;

-- | The Amazon Resource Name (ARN) of the SAML provider.
samlpleARN :: Lens' SAMLProviderListEntry Text
samlpleARN = lens _samlpleARN (\ s a -> s{_samlpleARN = a});

instance FromXML SAMLProviderListEntry where
        parseXML x
          = SAMLProviderListEntry' <$>
              x .@? "CreateDate" <*> x .@? "ValidUntil" <*>
                x .@ "Arn"

-- | /See:/ 'serverCertificate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'serServerCertificateMetadata'
--
-- * 'serCertificateBody'
--
-- * 'serCertificateChain'
data ServerCertificate = ServerCertificate'{_serServerCertificateMetadata :: ServerCertificateMetadata, _serCertificateBody :: Text, _serCertificateChain :: Text} deriving (Eq, Read, Show)

-- | 'ServerCertificate' smart constructor.
serverCertificate :: ServerCertificateMetadata -> Text -> Text -> ServerCertificate
serverCertificate pServerCertificateMetadata pCertificateBody pCertificateChain = ServerCertificate'{_serServerCertificateMetadata = pServerCertificateMetadata, _serCertificateBody = pCertificateBody, _serCertificateChain = pCertificateChain};

-- | The meta information of the server certificate, such as its name, path,
-- ID, and ARN.
serServerCertificateMetadata :: Lens' ServerCertificate ServerCertificateMetadata
serServerCertificateMetadata = lens _serServerCertificateMetadata (\ s a -> s{_serServerCertificateMetadata = a});

-- | The contents of the public key certificate.
serCertificateBody :: Lens' ServerCertificate Text
serCertificateBody = lens _serCertificateBody (\ s a -> s{_serCertificateBody = a});

-- | The contents of the public key certificate chain.
serCertificateChain :: Lens' ServerCertificate Text
serCertificateChain = lens _serCertificateChain (\ s a -> s{_serCertificateChain = a});

instance FromXML ServerCertificate where
        parseXML x
          = ServerCertificate' <$>
              x .@ "ServerCertificateMetadata" <*>
                x .@ "CertificateBody"
                <*> x .@ "CertificateChain"

-- | /See:/ 'serverCertificateMetadata' smart constructor.
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
data ServerCertificateMetadata = ServerCertificateMetadata'{_scmUploadDate :: Maybe ISO8601, _scmExpiration :: Maybe ISO8601, _scmPath :: Text, _scmServerCertificateName :: Text, _scmServerCertificateId :: Text, _scmARN :: Text} deriving (Eq, Read, Show)

-- | 'ServerCertificateMetadata' smart constructor.
serverCertificateMetadata :: Text -> Text -> Text -> Text -> ServerCertificateMetadata
serverCertificateMetadata pPath pServerCertificateName pServerCertificateId pARN = ServerCertificateMetadata'{_scmUploadDate = Nothing, _scmExpiration = Nothing, _scmPath = pPath, _scmServerCertificateName = pServerCertificateName, _scmServerCertificateId = pServerCertificateId, _scmARN = pARN};

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
              x .@? "UploadDate" <*> x .@? "Expiration" <*>
                x .@ "Path"
                <*> x .@ "ServerCertificateName"
                <*> x .@ "ServerCertificateId"
                <*> x .@ "Arn"

-- | /See:/ 'signingCertificate' smart constructor.
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
data SigningCertificate = SigningCertificate'{_scUploadDate :: Maybe ISO8601, _scUserName :: Text, _scCertificateId :: Text, _scCertificateBody :: Text, _scStatus :: StatusType} deriving (Eq, Read, Show)

-- | 'SigningCertificate' smart constructor.
signingCertificate :: Text -> Text -> Text -> StatusType -> SigningCertificate
signingCertificate pUserName pCertificateId pCertificateBody pStatus = SigningCertificate'{_scUploadDate = Nothing, _scUserName = pUserName, _scCertificateId = pCertificateId, _scCertificateBody = pCertificateBody, _scStatus = pStatus};

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
              x .@? "UploadDate" <*> x .@ "UserName" <*>
                x .@ "CertificateId"
                <*> x .@ "CertificateBody"
                <*> x .@ "Status"

data StatusType = Inactive | Active deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText StatusType where
    parser = takeLowerText >>= \case
        "Active" -> pure Active
        "Inactive" -> pure Inactive
        e -> fail ("Failure parsing StatusType from " ++ show e)

instance ToText StatusType where
    toText = \case
        Active -> "Active"
        Inactive -> "Inactive"

instance Hashable StatusType
instance ToQuery StatusType
instance ToHeader StatusType

instance FromXML StatusType where
    parseXML = parseXMLText "StatusType"

data SummaryKeyType = AttachedPoliciesPerUserQuota | UsersQuota | Groups | GroupsQuota | Users | MFADevicesInUse | PolicyVersionsInUse | SigningCertificatesPerUserQuota | PoliciesQuota | AccessKeysPerUserQuota | PolicySizeQuota | ServerCertificates | AttachedPoliciesPerRoleQuota | GroupsPerUserQuota | GroupPolicySizeQuota | AccountSigningCertificatesPresent | UserPolicySizeQuota | AttachedPoliciesPerGroupQuota | AccountAccessKeysPresent | ServerCertificatesQuota | VersionsPerPolicyQuota | PolicyVersionsInUseQuota | Policies | AccountMFAEnabled | MFADevices deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText SummaryKeyType where
    parser = takeLowerText >>= \case
        "AccessKeysPerUserQuota" -> pure AccessKeysPerUserQuota
        "AccountAccessKeysPresent" -> pure AccountAccessKeysPresent
        "AccountMFAEnabled" -> pure AccountMFAEnabled
        "AccountSigningCertificatesPresent" -> pure AccountSigningCertificatesPresent
        "AttachedPoliciesPerGroupQuota" -> pure AttachedPoliciesPerGroupQuota
        "AttachedPoliciesPerRoleQuota" -> pure AttachedPoliciesPerRoleQuota
        "AttachedPoliciesPerUserQuota" -> pure AttachedPoliciesPerUserQuota
        "GroupPolicySizeQuota" -> pure GroupPolicySizeQuota
        "Groups" -> pure Groups
        "GroupsPerUserQuota" -> pure GroupsPerUserQuota
        "GroupsQuota" -> pure GroupsQuota
        "MFADevices" -> pure MFADevices
        "MFADevicesInUse" -> pure MFADevicesInUse
        "Policies" -> pure Policies
        "PoliciesQuota" -> pure PoliciesQuota
        "PolicySizeQuota" -> pure PolicySizeQuota
        "PolicyVersionsInUse" -> pure PolicyVersionsInUse
        "PolicyVersionsInUseQuota" -> pure PolicyVersionsInUseQuota
        "ServerCertificates" -> pure ServerCertificates
        "ServerCertificatesQuota" -> pure ServerCertificatesQuota
        "SigningCertificatesPerUserQuota" -> pure SigningCertificatesPerUserQuota
        "UserPolicySizeQuota" -> pure UserPolicySizeQuota
        "Users" -> pure Users
        "UsersQuota" -> pure UsersQuota
        "VersionsPerPolicyQuota" -> pure VersionsPerPolicyQuota
        e -> fail ("Failure parsing SummaryKeyType from " ++ show e)

instance ToText SummaryKeyType where
    toText = \case
        AccessKeysPerUserQuota -> "AccessKeysPerUserQuota"
        AccountAccessKeysPresent -> "AccountAccessKeysPresent"
        AccountMFAEnabled -> "AccountMFAEnabled"
        AccountSigningCertificatesPresent -> "AccountSigningCertificatesPresent"
        AttachedPoliciesPerGroupQuota -> "AttachedPoliciesPerGroupQuota"
        AttachedPoliciesPerRoleQuota -> "AttachedPoliciesPerRoleQuota"
        AttachedPoliciesPerUserQuota -> "AttachedPoliciesPerUserQuota"
        GroupPolicySizeQuota -> "GroupPolicySizeQuota"
        Groups -> "Groups"
        GroupsPerUserQuota -> "GroupsPerUserQuota"
        GroupsQuota -> "GroupsQuota"
        MFADevices -> "MFADevices"
        MFADevicesInUse -> "MFADevicesInUse"
        Policies -> "Policies"
        PoliciesQuota -> "PoliciesQuota"
        PolicySizeQuota -> "PolicySizeQuota"
        PolicyVersionsInUse -> "PolicyVersionsInUse"
        PolicyVersionsInUseQuota -> "PolicyVersionsInUseQuota"
        ServerCertificates -> "ServerCertificates"
        ServerCertificatesQuota -> "ServerCertificatesQuota"
        SigningCertificatesPerUserQuota -> "SigningCertificatesPerUserQuota"
        UserPolicySizeQuota -> "UserPolicySizeQuota"
        Users -> "Users"
        UsersQuota -> "UsersQuota"
        VersionsPerPolicyQuota -> "VersionsPerPolicyQuota"

instance Hashable SummaryKeyType
instance ToQuery SummaryKeyType
instance ToHeader SummaryKeyType

instance FromXML SummaryKeyType where
    parseXML = parseXMLText "SummaryKeyType"

-- | /See:/ 'user' smart constructor.
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
data User = User'{_usePasswordLastUsed :: Maybe ISO8601, _usePath :: Text, _useUserName :: Text, _useUserId :: Text, _useARN :: Text, _useCreateDate :: ISO8601} deriving (Eq, Read, Show)

-- | 'User' smart constructor.
user :: Text -> Text -> Text -> Text -> UTCTime -> User
user pPath pUserName pUserId pARN pCreateDate = User'{_usePasswordLastUsed = Nothing, _usePath = pPath, _useUserName = pUserName, _useUserId = pUserId, _useARN = pARN, _useCreateDate = _Time # pCreateDate};

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- user\'s password was last used to sign in to an AWS website. For a list
-- of AWS websites that capture a user\'s last sign-in time, see the
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Credential Reports>
-- topic in the /Using IAM/ guide. If a password is used more than once in
-- a five-minute span, only the first use is returned in this field. When
-- the user does not have a password, this field is null (not present).
-- When a user\'s password exists but has never been used, or when there is
-- no sign-in data associated with the user, this field is null (not
-- present).
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
              x .@? "PasswordLastUsed" <*> x .@ "Path" <*>
                x .@ "UserName"
                <*> x .@ "UserId"
                <*> x .@ "Arn"
                <*> x .@ "CreateDate"

-- | /See:/ 'userDetail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udGroupList'
--
-- * 'udCreateDate'
--
-- * 'udUserPolicyList'
--
-- * 'udAttachedManagedPolicies'
--
-- * 'udARN'
--
-- * 'udPath'
--
-- * 'udUserName'
--
-- * 'udUserId'
data UserDetail = UserDetail'{_udGroupList :: [Text], _udCreateDate :: Maybe ISO8601, _udUserPolicyList :: [PolicyDetail], _udAttachedManagedPolicies :: [AttachedPolicy], _udARN :: Text, _udPath :: Text, _udUserName :: Text, _udUserId :: Text} deriving (Eq, Read, Show)

-- | 'UserDetail' smart constructor.
userDetail :: Text -> Text -> Text -> Text -> UserDetail
userDetail pARN pPath pUserName pUserId = UserDetail'{_udGroupList = mempty, _udCreateDate = Nothing, _udUserPolicyList = mempty, _udAttachedManagedPolicies = mempty, _udARN = pARN, _udPath = pPath, _udUserName = pUserName, _udUserId = pUserId};

-- | A list of IAM groups that the user is in.
udGroupList :: Lens' UserDetail [Text]
udGroupList = lens _udGroupList (\ s a -> s{_udGroupList = a});

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- user was created.
udCreateDate :: Lens' UserDetail (Maybe UTCTime)
udCreateDate = lens _udCreateDate (\ s a -> s{_udCreateDate = a}) . mapping _Time;

-- | A list of the inline policies embedded in the user.
udUserPolicyList :: Lens' UserDetail [PolicyDetail]
udUserPolicyList = lens _udUserPolicyList (\ s a -> s{_udUserPolicyList = a});

-- | A list of the managed policies attached to the user.
udAttachedManagedPolicies :: Lens' UserDetail [AttachedPolicy]
udAttachedManagedPolicies = lens _udAttachedManagedPolicies (\ s a -> s{_udAttachedManagedPolicies = a});

-- | FIXME: Undocumented member.
udARN :: Lens' UserDetail Text
udARN = lens _udARN (\ s a -> s{_udARN = a});

-- | The path to the user. For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
udPath :: Lens' UserDetail Text
udPath = lens _udPath (\ s a -> s{_udPath = a});

-- | The friendly name identifying the user.
udUserName :: Lens' UserDetail Text
udUserName = lens _udUserName (\ s a -> s{_udUserName = a});

-- | The stable and unique string identifying the user. For more information
-- about IDs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
udUserId :: Lens' UserDetail Text
udUserId = lens _udUserId (\ s a -> s{_udUserId = a});

instance FromXML UserDetail where
        parseXML x
          = UserDetail' <$>
              (x .@? "GroupList" .!@ mempty >>=
                 parseXMLList "member")
                <*> x .@? "CreateDate"
                <*>
                (x .@? "UserPolicyList" .!@ mempty >>=
                   parseXMLList "member")
                <*>
                (x .@? "AttachedManagedPolicies" .!@ mempty >>=
                   parseXMLList "member")
                <*> x .@ "Arn"
                <*> x .@ "Path"
                <*> x .@ "UserName"
                <*> x .@ "UserId"

-- | /See:/ 'virtualMFADevice' smart constructor.
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
data VirtualMFADevice = VirtualMFADevice'{_vmdQRCodePNG :: Maybe (Sensitive Base64), _vmdBase32StringSeed :: Maybe (Sensitive Base64), _vmdUser :: Maybe User, _vmdEnableDate :: Maybe ISO8601, _vmdSerialNumber :: Text} deriving (Eq, Read, Show)

-- | 'VirtualMFADevice' smart constructor.
virtualMFADevice :: Text -> VirtualMFADevice
virtualMFADevice pSerialNumber = VirtualMFADevice'{_vmdQRCodePNG = Nothing, _vmdBase32StringSeed = Nothing, _vmdUser = Nothing, _vmdEnableDate = Nothing, _vmdSerialNumber = pSerialNumber};

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
              x .@? "QRCodePNG" <*> x .@? "Base32StringSeed" <*>
                x .@? "User"
                <*> x .@? "EnableDate"
                <*> x .@ "SerialNumber"
