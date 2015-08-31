{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.Product where

import           Network.AWS.IAM.Types.Sum
import           Network.AWS.Prelude

-- | Contains information about an AWS access key.
--
-- This data type is used as a response element in the CreateAccessKey and
-- ListAccessKeys actions.
--
-- The 'SecretAccessKey' value is returned only in response to
-- CreateAccessKey. You can get a secret access key only when you first
-- create an access key; you cannot recover the secret access key later. If
-- you lose a secret access key, you must create a new access key.
--
-- /See:/ 'accessKey' smart constructor.
data AccessKey = AccessKey'
    { _akCreateDate      :: !(Maybe ISO8601)
    , _akUserName        :: !Text
    , _akAccessKeyId     :: !Text
    , _akStatus          :: !StatusType
    , _akSecretAccessKey :: !(Sensitive Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccessKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
accessKey
    :: Text -- ^ 'akUserName'
    -> Text -- ^ 'akAccessKeyId'
    -> StatusType -- ^ 'akStatus'
    -> Text -- ^ 'akSecretAccessKey'
    -> AccessKey
accessKey pUserName_ pAccessKeyId_ pStatus_ pSecretAccessKey_ =
    AccessKey'
    { _akCreateDate = Nothing
    , _akUserName = pUserName_
    , _akAccessKeyId = pAccessKeyId_
    , _akStatus = pStatus_
    , _akSecretAccessKey = _Sensitive # pSecretAccessKey_
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

-- | The status of the access key. 'Active' means the key is valid for API
-- calls, while 'Inactive' means it is not.
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
data AccessKeyLastUsed = AccessKeyLastUsed'
    { _akluLastUsedDate :: !ISO8601
    , _akluServiceName  :: !Text
    , _akluRegion       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccessKeyLastUsed' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'akluLastUsedDate'
--
-- * 'akluServiceName'
--
-- * 'akluRegion'
accessKeyLastUsed
    :: UTCTime -- ^ 'akluLastUsedDate'
    -> Text -- ^ 'akluServiceName'
    -> Text -- ^ 'akluRegion'
    -> AccessKeyLastUsed
accessKeyLastUsed pLastUsedDate_ pServiceName_ pRegion_ =
    AccessKeyLastUsed'
    { _akluLastUsedDate = _Time # pLastUsedDate_
    , _akluServiceName = pServiceName_
    , _akluRegion = pRegion_
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
data AccessKeyMetadata = AccessKeyMetadata'
    { _akmStatus      :: !(Maybe StatusType)
    , _akmCreateDate  :: !(Maybe ISO8601)
    , _akmUserName    :: !(Maybe Text)
    , _akmAccessKeyId :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccessKeyMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'akmStatus'
--
-- * 'akmCreateDate'
--
-- * 'akmUserName'
--
-- * 'akmAccessKeyId'
accessKeyMetadata
    :: AccessKeyMetadata
accessKeyMetadata =
    AccessKeyMetadata'
    { _akmStatus = Nothing
    , _akmCreateDate = Nothing
    , _akmUserName = Nothing
    , _akmAccessKeyId = Nothing
    }

-- | The status of the access key. 'Active' means the key is valid for API
-- calls; 'Inactive' means it is not.
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
data AttachedPolicy = AttachedPolicy'
    { _apPolicyName :: !(Maybe Text)
    , _apPolicyARN  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AttachedPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apPolicyName'
--
-- * 'apPolicyARN'
attachedPolicy
    :: AttachedPolicy
attachedPolicy =
    AttachedPolicy'
    { _apPolicyName = Nothing
    , _apPolicyARN = Nothing
    }

-- | The friendly name of the attached policy.
apPolicyName :: Lens' AttachedPolicy (Maybe Text)
apPolicyName = lens _apPolicyName (\ s a -> s{_apPolicyName = a});

-- | Undocumented member.
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
data Group = Group'
    { _gPath       :: !Text
    , _gGroupName  :: !Text
    , _gGroupId    :: !Text
    , _gARN        :: !Text
    , _gCreateDate :: !ISO8601
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Group' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gPath'
--
-- * 'gGroupName'
--
-- * 'gGroupId'
--
-- * 'gARN'
--
-- * 'gCreateDate'
group'
    :: Text -- ^ 'gPath'
    -> Text -- ^ 'gGroupName'
    -> Text -- ^ 'gGroupId'
    -> Text -- ^ 'gARN'
    -> UTCTime -- ^ 'gCreateDate'
    -> Group
group' pPath_ pGroupName_ pGroupId_ pARN_ pCreateDate_ =
    Group'
    { _gPath = pPath_
    , _gGroupName = pGroupName_
    , _gGroupId = pGroupId_
    , _gARN = pARN_
    , _gCreateDate = _Time # pCreateDate_
    }

-- | The path to the group. For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
gPath :: Lens' Group Text
gPath = lens _gPath (\ s a -> s{_gPath = a});

-- | The friendly name that identifies the group.
gGroupName :: Lens' Group Text
gGroupName = lens _gGroupName (\ s a -> s{_gGroupName = a});

-- | The stable and unique string identifying the group. For more information
-- about IDs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
gGroupId :: Lens' Group Text
gGroupId = lens _gGroupId (\ s a -> s{_gGroupId = a});

-- | The Amazon Resource Name (ARN) specifying the group. For more
-- information about ARNs and how to use them in policies, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
gARN :: Lens' Group Text
gARN = lens _gARN (\ s a -> s{_gARN = a});

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- group was created.
gCreateDate :: Lens' Group UTCTime
gCreateDate = lens _gCreateDate (\ s a -> s{_gCreateDate = a}) . _Time;

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
data GroupDetail = GroupDetail'
    { _gdARN                     :: !(Maybe Text)
    , _gdPath                    :: !(Maybe Text)
    , _gdCreateDate              :: !(Maybe ISO8601)
    , _gdGroupId                 :: !(Maybe Text)
    , _gdGroupPolicyList         :: !(Maybe [PolicyDetail])
    , _gdGroupName               :: !(Maybe Text)
    , _gdAttachedManagedPolicies :: !(Maybe [AttachedPolicy])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GroupDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
groupDetail
    :: GroupDetail
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

-- | Undocumented member.
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
gdGroupPolicyList = lens _gdGroupPolicyList (\ s a -> s{_gdGroupPolicyList = a}) . _Default . _Coerce;

-- | The friendly name that identifies the group.
gdGroupName :: Lens' GroupDetail (Maybe Text)
gdGroupName = lens _gdGroupName (\ s a -> s{_gdGroupName = a});

-- | A list of the managed policies attached to the group.
gdAttachedManagedPolicies :: Lens' GroupDetail [AttachedPolicy]
gdAttachedManagedPolicies = lens _gdAttachedManagedPolicies (\ s a -> s{_gdAttachedManagedPolicies = a}) . _Default . _Coerce;

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
data InstanceProfile = InstanceProfile'
    { _ipPath                :: !Text
    , _ipInstanceProfileName :: !Text
    , _ipInstanceProfileId   :: !Text
    , _ipARN                 :: !Text
    , _ipCreateDate          :: !ISO8601
    , _ipRoles               :: ![Role]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
instanceProfile
    :: Text -- ^ 'ipPath'
    -> Text -- ^ 'ipInstanceProfileName'
    -> Text -- ^ 'ipInstanceProfileId'
    -> Text -- ^ 'ipARN'
    -> UTCTime -- ^ 'ipCreateDate'
    -> InstanceProfile
instanceProfile pPath_ pInstanceProfileName_ pInstanceProfileId_ pARN_ pCreateDate_ =
    InstanceProfile'
    { _ipPath = pPath_
    , _ipInstanceProfileName = pInstanceProfileName_
    , _ipInstanceProfileId = pInstanceProfileId_
    , _ipARN = pARN_
    , _ipCreateDate = _Time # pCreateDate_
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
ipRoles = lens _ipRoles (\ s a -> s{_ipRoles = a}) . _Coerce;

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
data LoginProfile = LoginProfile'
    { _lpPasswordResetRequired :: !(Maybe Bool)
    , _lpUserName              :: !Text
    , _lpCreateDate            :: !ISO8601
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LoginProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpPasswordResetRequired'
--
-- * 'lpUserName'
--
-- * 'lpCreateDate'
loginProfile
    :: Text -- ^ 'lpUserName'
    -> UTCTime -- ^ 'lpCreateDate'
    -> LoginProfile
loginProfile pUserName_ pCreateDate_ =
    LoginProfile'
    { _lpPasswordResetRequired = Nothing
    , _lpUserName = pUserName_
    , _lpCreateDate = _Time # pCreateDate_
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
data MFADevice = MFADevice'
    { _mdUserName     :: !Text
    , _mdSerialNumber :: !Text
    , _mdEnableDate   :: !ISO8601
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MFADevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdUserName'
--
-- * 'mdSerialNumber'
--
-- * 'mdEnableDate'
mfaDevice
    :: Text -- ^ 'mdUserName'
    -> Text -- ^ 'mdSerialNumber'
    -> UTCTime -- ^ 'mdEnableDate'
    -> MFADevice
mfaDevice pUserName_ pSerialNumber_ pEnableDate_ =
    MFADevice'
    { _mdUserName = pUserName_
    , _mdSerialNumber = pSerialNumber_
    , _mdEnableDate = _Time # pEnableDate_
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
data ManagedPolicyDetail = ManagedPolicyDetail'
    { _mpdPolicyName        :: !(Maybe Text)
    , _mpdARN               :: !(Maybe Text)
    , _mpdUpdateDate        :: !(Maybe ISO8601)
    , _mpdPolicyId          :: !(Maybe Text)
    , _mpdPath              :: !(Maybe Text)
    , _mpdPolicyVersionList :: !(Maybe [PolicyVersion])
    , _mpdCreateDate        :: !(Maybe ISO8601)
    , _mpdIsAttachable      :: !(Maybe Bool)
    , _mpdDefaultVersionId  :: !(Maybe Text)
    , _mpdAttachmentCount   :: !(Maybe Int)
    , _mpdDescription       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ManagedPolicyDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpdPolicyName'
--
-- * 'mpdARN'
--
-- * 'mpdUpdateDate'
--
-- * 'mpdPolicyId'
--
-- * 'mpdPath'
--
-- * 'mpdPolicyVersionList'
--
-- * 'mpdCreateDate'
--
-- * 'mpdIsAttachable'
--
-- * 'mpdDefaultVersionId'
--
-- * 'mpdAttachmentCount'
--
-- * 'mpdDescription'
managedPolicyDetail
    :: ManagedPolicyDetail
managedPolicyDetail =
    ManagedPolicyDetail'
    { _mpdPolicyName = Nothing
    , _mpdARN = Nothing
    , _mpdUpdateDate = Nothing
    , _mpdPolicyId = Nothing
    , _mpdPath = Nothing
    , _mpdPolicyVersionList = Nothing
    , _mpdCreateDate = Nothing
    , _mpdIsAttachable = Nothing
    , _mpdDefaultVersionId = Nothing
    , _mpdAttachmentCount = Nothing
    , _mpdDescription = Nothing
    }

-- | The friendly name (not ARN) identifying the policy.
mpdPolicyName :: Lens' ManagedPolicyDetail (Maybe Text)
mpdPolicyName = lens _mpdPolicyName (\ s a -> s{_mpdPolicyName = a});

-- | Undocumented member.
mpdARN :: Lens' ManagedPolicyDetail (Maybe Text)
mpdARN = lens _mpdARN (\ s a -> s{_mpdARN = a});

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

-- | The path to the policy.
--
-- For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
mpdPath :: Lens' ManagedPolicyDetail (Maybe Text)
mpdPath = lens _mpdPath (\ s a -> s{_mpdPath = a});

-- | A list containing information about the versions of the policy.
mpdPolicyVersionList :: Lens' ManagedPolicyDetail [PolicyVersion]
mpdPolicyVersionList = lens _mpdPolicyVersionList (\ s a -> s{_mpdPolicyVersionList = a}) . _Default . _Coerce;

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy was created.
mpdCreateDate :: Lens' ManagedPolicyDetail (Maybe UTCTime)
mpdCreateDate = lens _mpdCreateDate (\ s a -> s{_mpdCreateDate = a}) . mapping _Time;

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
                (x .@? "UpdateDate")
                <*> (x .@? "PolicyId")
                <*> (x .@? "Path")
                <*>
                (x .@? "PolicyVersionList" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "CreateDate")
                <*> (x .@? "IsAttachable")
                <*> (x .@? "DefaultVersionId")
                <*> (x .@? "AttachmentCount")
                <*> (x .@? "Description")

-- | Contains the Amazon Resource Name (ARN) for an IAM OpenID Connect
-- provider.
--
-- /See:/ 'openIdConnectProviderListEntry' smart constructor.
newtype OpenIdConnectProviderListEntry = OpenIdConnectProviderListEntry'
    { _oicpleARN :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'OpenIdConnectProviderListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oicpleARN'
openIdConnectProviderListEntry
    :: OpenIdConnectProviderListEntry
openIdConnectProviderListEntry =
    OpenIdConnectProviderListEntry'
    { _oicpleARN = Nothing
    }

-- | Undocumented member.
oicpleARN :: Lens' OpenIdConnectProviderListEntry (Maybe Text)
oicpleARN = lens _oicpleARN (\ s a -> s{_oicpleARN = a});

instance FromXML OpenIdConnectProviderListEntry where
        parseXML x
          = OpenIdConnectProviderListEntry' <$> (x .@? "Arn")

-- | Contains information about the account password policy.
--
-- This data type is used as a response element in the
-- GetAccountPasswordPolicy action.
--
-- /See:/ 'passwordPolicy' smart constructor.
data PasswordPolicy = PasswordPolicy'
    { _ppExpirePasswords            :: !(Maybe Bool)
    , _ppMinimumPasswordLength      :: !(Maybe Nat)
    , _ppRequireNumbers             :: !(Maybe Bool)
    , _ppPasswordReusePrevention    :: !(Maybe Nat)
    , _ppRequireLowercaseCharacters :: !(Maybe Bool)
    , _ppMaxPasswordAge             :: !(Maybe Nat)
    , _ppHardExpiry                 :: !(Maybe Bool)
    , _ppRequireSymbols             :: !(Maybe Bool)
    , _ppRequireUppercaseCharacters :: !(Maybe Bool)
    , _ppAllowUsersToChangePassword :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PasswordPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppExpirePasswords'
--
-- * 'ppMinimumPasswordLength'
--
-- * 'ppRequireNumbers'
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
passwordPolicy
    :: PasswordPolicy
passwordPolicy =
    PasswordPolicy'
    { _ppExpirePasswords = Nothing
    , _ppMinimumPasswordLength = Nothing
    , _ppRequireNumbers = Nothing
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

-- | Minimum length to require for IAM user passwords.
ppMinimumPasswordLength :: Lens' PasswordPolicy (Maybe Natural)
ppMinimumPasswordLength = lens _ppMinimumPasswordLength (\ s a -> s{_ppMinimumPasswordLength = a}) . mapping _Nat;

-- | Specifies whether to require numbers for IAM user passwords.
ppRequireNumbers :: Lens' PasswordPolicy (Maybe Bool)
ppRequireNumbers = lens _ppRequireNumbers (\ s a -> s{_ppRequireNumbers = a});

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
                (x .@? "MinimumPasswordLength")
                <*> (x .@? "RequireNumbers")
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
data Policy = Policy'
    { _pPolicyName       :: !(Maybe Text)
    , _pARN              :: !(Maybe Text)
    , _pUpdateDate       :: !(Maybe ISO8601)
    , _pPolicyId         :: !(Maybe Text)
    , _pPath             :: !(Maybe Text)
    , _pCreateDate       :: !(Maybe ISO8601)
    , _pIsAttachable     :: !(Maybe Bool)
    , _pDefaultVersionId :: !(Maybe Text)
    , _pAttachmentCount  :: !(Maybe Int)
    , _pDescription      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Policy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pPolicyName'
--
-- * 'pARN'
--
-- * 'pUpdateDate'
--
-- * 'pPolicyId'
--
-- * 'pPath'
--
-- * 'pCreateDate'
--
-- * 'pIsAttachable'
--
-- * 'pDefaultVersionId'
--
-- * 'pAttachmentCount'
--
-- * 'pDescription'
policy
    :: Policy
policy =
    Policy'
    { _pPolicyName = Nothing
    , _pARN = Nothing
    , _pUpdateDate = Nothing
    , _pPolicyId = Nothing
    , _pPath = Nothing
    , _pCreateDate = Nothing
    , _pIsAttachable = Nothing
    , _pDefaultVersionId = Nothing
    , _pAttachmentCount = Nothing
    , _pDescription = Nothing
    }

-- | The friendly name (not ARN) identifying the policy.
pPolicyName :: Lens' Policy (Maybe Text)
pPolicyName = lens _pPolicyName (\ s a -> s{_pPolicyName = a});

-- | Undocumented member.
pARN :: Lens' Policy (Maybe Text)
pARN = lens _pARN (\ s a -> s{_pARN = a});

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy was last updated.
--
-- When a policy has only one version, this field contains the date and
-- time when the policy was created. When a policy has more than one
-- version, this field contains the date and time when the most recent
-- policy version was created.
pUpdateDate :: Lens' Policy (Maybe UTCTime)
pUpdateDate = lens _pUpdateDate (\ s a -> s{_pUpdateDate = a}) . mapping _Time;

-- | The stable and unique string identifying the policy.
--
-- For more information about IDs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
pPolicyId :: Lens' Policy (Maybe Text)
pPolicyId = lens _pPolicyId (\ s a -> s{_pPolicyId = a});

-- | The path to the policy.
--
-- For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
pPath :: Lens' Policy (Maybe Text)
pPath = lens _pPath (\ s a -> s{_pPath = a});

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy was created.
pCreateDate :: Lens' Policy (Maybe UTCTime)
pCreateDate = lens _pCreateDate (\ s a -> s{_pCreateDate = a}) . mapping _Time;

-- | Specifies whether the policy can be attached to an IAM user, group, or
-- role.
pIsAttachable :: Lens' Policy (Maybe Bool)
pIsAttachable = lens _pIsAttachable (\ s a -> s{_pIsAttachable = a});

-- | The identifier for the version of the policy that is set as the default
-- version.
pDefaultVersionId :: Lens' Policy (Maybe Text)
pDefaultVersionId = lens _pDefaultVersionId (\ s a -> s{_pDefaultVersionId = a});

-- | The number of entities (users, groups, and roles) that the policy is
-- attached to.
pAttachmentCount :: Lens' Policy (Maybe Int)
pAttachmentCount = lens _pAttachmentCount (\ s a -> s{_pAttachmentCount = a});

-- | A friendly description of the policy.
--
-- This element is included in the response to the GetPolicy operation. It
-- is not included in the response to the ListPolicies operation.
pDescription :: Lens' Policy (Maybe Text)
pDescription = lens _pDescription (\ s a -> s{_pDescription = a});

instance FromXML Policy where
        parseXML x
          = Policy' <$>
              (x .@? "PolicyName") <*> (x .@? "Arn") <*>
                (x .@? "UpdateDate")
                <*> (x .@? "PolicyId")
                <*> (x .@? "Path")
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
data PolicyDetail = PolicyDetail'
    { _pdPolicyDocument :: !(Maybe Text)
    , _pdPolicyName     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PolicyDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdPolicyDocument'
--
-- * 'pdPolicyName'
policyDetail
    :: PolicyDetail
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
newtype PolicyGroup = PolicyGroup'
    { _pgGroupName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PolicyGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgGroupName'
policyGroup
    :: PolicyGroup
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
newtype PolicyRole = PolicyRole'
    { _prRoleName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PolicyRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prRoleName'
policyRole
    :: PolicyRole
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
newtype PolicyUser = PolicyUser'
    { _puUserName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PolicyUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'puUserName'
policyUser
    :: PolicyUser
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
data PolicyVersion = PolicyVersion'
    { _pvVersionId        :: !(Maybe Text)
    , _pvCreateDate       :: !(Maybe ISO8601)
    , _pvDocument         :: !(Maybe Text)
    , _pvIsDefaultVersion :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PolicyVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvVersionId'
--
-- * 'pvCreateDate'
--
-- * 'pvDocument'
--
-- * 'pvIsDefaultVersion'
policyVersion
    :: PolicyVersion
policyVersion =
    PolicyVersion'
    { _pvVersionId = Nothing
    , _pvCreateDate = Nothing
    , _pvDocument = Nothing
    , _pvIsDefaultVersion = Nothing
    }

-- | The identifier for the policy version.
--
-- Policy version identifiers always begin with 'v' (always lowercase).
-- When a policy is created, the first policy version is 'v1'.
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
data Role = Role'
    { _rAssumeRolePolicyDocument :: !(Maybe Text)
    , _rPath                     :: !Text
    , _rRoleName                 :: !Text
    , _rRoleId                   :: !Text
    , _rARN                      :: !Text
    , _rCreateDate               :: !ISO8601
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Role' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rAssumeRolePolicyDocument'
--
-- * 'rPath'
--
-- * 'rRoleName'
--
-- * 'rRoleId'
--
-- * 'rARN'
--
-- * 'rCreateDate'
role
    :: Text -- ^ 'rPath'
    -> Text -- ^ 'rRoleName'
    -> Text -- ^ 'rRoleId'
    -> Text -- ^ 'rARN'
    -> UTCTime -- ^ 'rCreateDate'
    -> Role
role pPath_ pRoleName_ pRoleId_ pARN_ pCreateDate_ =
    Role'
    { _rAssumeRolePolicyDocument = Nothing
    , _rPath = pPath_
    , _rRoleName = pRoleName_
    , _rRoleId = pRoleId_
    , _rARN = pARN_
    , _rCreateDate = _Time # pCreateDate_
    }

-- | The policy that grants an entity permission to assume the role.
rAssumeRolePolicyDocument :: Lens' Role (Maybe Text)
rAssumeRolePolicyDocument = lens _rAssumeRolePolicyDocument (\ s a -> s{_rAssumeRolePolicyDocument = a});

-- | The path to the role. For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
rPath :: Lens' Role Text
rPath = lens _rPath (\ s a -> s{_rPath = a});

-- | The friendly name that identifies the role.
rRoleName :: Lens' Role Text
rRoleName = lens _rRoleName (\ s a -> s{_rRoleName = a});

-- | The stable and unique string identifying the role. For more information
-- about IDs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
rRoleId :: Lens' Role Text
rRoleId = lens _rRoleId (\ s a -> s{_rRoleId = a});

-- | The Amazon Resource Name (ARN) specifying the role. For more information
-- about ARNs and how to use them in policies, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
rARN :: Lens' Role Text
rARN = lens _rARN (\ s a -> s{_rARN = a});

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- role was created.
rCreateDate :: Lens' Role UTCTime
rCreateDate = lens _rCreateDate (\ s a -> s{_rCreateDate = a}) . _Time;

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

-- | Creates a value of 'RoleDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
roleDetail
    :: RoleDetail
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

-- | Undocumented member.
rdARN :: Lens' RoleDetail (Maybe Text)
rdARN = lens _rdARN (\ s a -> s{_rdARN = a});

-- | The path to the role. For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
rdPath :: Lens' RoleDetail (Maybe Text)
rdPath = lens _rdPath (\ s a -> s{_rdPath = a});

-- | Undocumented member.
rdInstanceProfileList :: Lens' RoleDetail [InstanceProfile]
rdInstanceProfileList = lens _rdInstanceProfileList (\ s a -> s{_rdInstanceProfileList = a}) . _Default . _Coerce;

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
rdRolePolicyList = lens _rdRolePolicyList (\ s a -> s{_rdRolePolicyList = a}) . _Default . _Coerce;

-- | A list of managed policies attached to the role. These policies are the
-- role\'s access (permissions) policies.
rdAttachedManagedPolicies :: Lens' RoleDetail [AttachedPolicy]
rdAttachedManagedPolicies = lens _rdAttachedManagedPolicies (\ s a -> s{_rdAttachedManagedPolicies = a}) . _Default . _Coerce;

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
data SAMLProviderListEntry = SAMLProviderListEntry'
    { _samlpleARN        :: !(Maybe Text)
    , _samlpleCreateDate :: !(Maybe ISO8601)
    , _samlpleValidUntil :: !(Maybe ISO8601)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SAMLProviderListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'samlpleARN'
--
-- * 'samlpleCreateDate'
--
-- * 'samlpleValidUntil'
sAMLProviderListEntry
    :: SAMLProviderListEntry
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

-- | Contains information about an SSH public key.
--
-- This data type is used as a response element in the GetSSHPublicKey and
-- UploadSSHPublicKey actions.
--
-- /See:/ 'sshPublicKey' smart constructor.
data SSHPublicKey = SSHPublicKey'
    { _spkUploadDate       :: !(Maybe ISO8601)
    , _spkUserName         :: !Text
    , _spkSSHPublicKeyId   :: !Text
    , _spkFingerprint      :: !Text
    , _spkSSHPublicKeyBody :: !Text
    , _spkStatus           :: !StatusType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SSHPublicKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spkUploadDate'
--
-- * 'spkUserName'
--
-- * 'spkSSHPublicKeyId'
--
-- * 'spkFingerprint'
--
-- * 'spkSSHPublicKeyBody'
--
-- * 'spkStatus'
sshPublicKey
    :: Text -- ^ 'spkUserName'
    -> Text -- ^ 'spkSSHPublicKeyId'
    -> Text -- ^ 'spkFingerprint'
    -> Text -- ^ 'spkSSHPublicKeyBody'
    -> StatusType -- ^ 'spkStatus'
    -> SSHPublicKey
sshPublicKey pUserName_ pSSHPublicKeyId_ pFingerprint_ pSSHPublicKeyBody_ pStatus_ =
    SSHPublicKey'
    { _spkUploadDate = Nothing
    , _spkUserName = pUserName_
    , _spkSSHPublicKeyId = pSSHPublicKeyId_
    , _spkFingerprint = pFingerprint_
    , _spkSSHPublicKeyBody = pSSHPublicKeyBody_
    , _spkStatus = pStatus_
    }

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the SSH
-- public key was uploaded.
spkUploadDate :: Lens' SSHPublicKey (Maybe UTCTime)
spkUploadDate = lens _spkUploadDate (\ s a -> s{_spkUploadDate = a}) . mapping _Time;

-- | The name of the IAM user associated with the SSH public key.
spkUserName :: Lens' SSHPublicKey Text
spkUserName = lens _spkUserName (\ s a -> s{_spkUserName = a});

-- | The unique identifier for the SSH public key.
spkSSHPublicKeyId :: Lens' SSHPublicKey Text
spkSSHPublicKeyId = lens _spkSSHPublicKeyId (\ s a -> s{_spkSSHPublicKeyId = a});

-- | The MD5 message digest of the SSH public key.
spkFingerprint :: Lens' SSHPublicKey Text
spkFingerprint = lens _spkFingerprint (\ s a -> s{_spkFingerprint = a});

-- | The SSH public key.
spkSSHPublicKeyBody :: Lens' SSHPublicKey Text
spkSSHPublicKeyBody = lens _spkSSHPublicKeyBody (\ s a -> s{_spkSSHPublicKeyBody = a});

-- | The status of the SSH public key. 'Active' means the key can be used for
-- authentication with an AWS CodeCommit repository. 'Inactive' means the
-- key cannot be used.
spkStatus :: Lens' SSHPublicKey StatusType
spkStatus = lens _spkStatus (\ s a -> s{_spkStatus = a});

instance FromXML SSHPublicKey where
        parseXML x
          = SSHPublicKey' <$>
              (x .@? "UploadDate") <*> (x .@ "UserName") <*>
                (x .@ "SSHPublicKeyId")
                <*> (x .@ "Fingerprint")
                <*> (x .@ "SSHPublicKeyBody")
                <*> (x .@ "Status")

-- | Contains information about an SSH public key, without the key\'s body or
-- fingerprint.
--
-- This data type is used as a response element in the ListSSHPublicKeys
-- action.
--
-- /See:/ 'sshPublicKeyMetadata' smart constructor.
data SSHPublicKeyMetadata = SSHPublicKeyMetadata'
    { _spkmUserName       :: !Text
    , _spkmSSHPublicKeyId :: !Text
    , _spkmStatus         :: !StatusType
    , _spkmUploadDate     :: !ISO8601
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SSHPublicKeyMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spkmUserName'
--
-- * 'spkmSSHPublicKeyId'
--
-- * 'spkmStatus'
--
-- * 'spkmUploadDate'
sshPublicKeyMetadata
    :: Text -- ^ 'spkmUserName'
    -> Text -- ^ 'spkmSSHPublicKeyId'
    -> StatusType -- ^ 'spkmStatus'
    -> UTCTime -- ^ 'spkmUploadDate'
    -> SSHPublicKeyMetadata
sshPublicKeyMetadata pUserName_ pSSHPublicKeyId_ pStatus_ pUploadDate_ =
    SSHPublicKeyMetadata'
    { _spkmUserName = pUserName_
    , _spkmSSHPublicKeyId = pSSHPublicKeyId_
    , _spkmStatus = pStatus_
    , _spkmUploadDate = _Time # pUploadDate_
    }

-- | The name of the IAM user associated with the SSH public key.
spkmUserName :: Lens' SSHPublicKeyMetadata Text
spkmUserName = lens _spkmUserName (\ s a -> s{_spkmUserName = a});

-- | The unique identifier for the SSH public key.
spkmSSHPublicKeyId :: Lens' SSHPublicKeyMetadata Text
spkmSSHPublicKeyId = lens _spkmSSHPublicKeyId (\ s a -> s{_spkmSSHPublicKeyId = a});

-- | The status of the SSH public key. 'Active' means the key can be used for
-- authentication with an AWS CodeCommit repository. 'Inactive' means the
-- key cannot be used.
spkmStatus :: Lens' SSHPublicKeyMetadata StatusType
spkmStatus = lens _spkmStatus (\ s a -> s{_spkmStatus = a});

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the SSH
-- public key was uploaded.
spkmUploadDate :: Lens' SSHPublicKeyMetadata UTCTime
spkmUploadDate = lens _spkmUploadDate (\ s a -> s{_spkmUploadDate = a}) . _Time;

instance FromXML SSHPublicKeyMetadata where
        parseXML x
          = SSHPublicKeyMetadata' <$>
              (x .@ "UserName") <*> (x .@ "SSHPublicKeyId") <*>
                (x .@ "Status")
                <*> (x .@ "UploadDate")

-- | Contains information about a server certificate.
--
-- This data type is used as a response element in the GetServerCertificate
-- action.
--
-- /See:/ 'serverCertificate' smart constructor.
data ServerCertificate = ServerCertificate'
    { _sCertificateChain          :: !(Maybe Text)
    , _sServerCertificateMetadata :: !ServerCertificateMetadata
    , _sCertificateBody           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ServerCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sCertificateChain'
--
-- * 'sServerCertificateMetadata'
--
-- * 'sCertificateBody'
serverCertificate
    :: ServerCertificateMetadata -- ^ 'sServerCertificateMetadata'
    -> Text -- ^ 'sCertificateBody'
    -> ServerCertificate
serverCertificate pServerCertificateMetadata_ pCertificateBody_ =
    ServerCertificate'
    { _sCertificateChain = Nothing
    , _sServerCertificateMetadata = pServerCertificateMetadata_
    , _sCertificateBody = pCertificateBody_
    }

-- | The contents of the public key certificate chain.
sCertificateChain :: Lens' ServerCertificate (Maybe Text)
sCertificateChain = lens _sCertificateChain (\ s a -> s{_sCertificateChain = a});

-- | The meta information of the server certificate, such as its name, path,
-- ID, and ARN.
sServerCertificateMetadata :: Lens' ServerCertificate ServerCertificateMetadata
sServerCertificateMetadata = lens _sServerCertificateMetadata (\ s a -> s{_sServerCertificateMetadata = a});

-- | The contents of the public key certificate.
sCertificateBody :: Lens' ServerCertificate Text
sCertificateBody = lens _sCertificateBody (\ s a -> s{_sCertificateBody = a});

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
data ServerCertificateMetadata = ServerCertificateMetadata'
    { _scmUploadDate            :: !(Maybe ISO8601)
    , _scmExpiration            :: !(Maybe ISO8601)
    , _scmPath                  :: !Text
    , _scmServerCertificateName :: !Text
    , _scmServerCertificateId   :: !Text
    , _scmARN                   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ServerCertificateMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
serverCertificateMetadata
    :: Text -- ^ 'scmPath'
    -> Text -- ^ 'scmServerCertificateName'
    -> Text -- ^ 'scmServerCertificateId'
    -> Text -- ^ 'scmARN'
    -> ServerCertificateMetadata
serverCertificateMetadata pPath_ pServerCertificateName_ pServerCertificateId_ pARN_ =
    ServerCertificateMetadata'
    { _scmUploadDate = Nothing
    , _scmExpiration = Nothing
    , _scmPath = pPath_
    , _scmServerCertificateName = pServerCertificateName_
    , _scmServerCertificateId = pServerCertificateId_
    , _scmARN = pARN_
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
data SigningCertificate = SigningCertificate'
    { _scUploadDate      :: !(Maybe ISO8601)
    , _scUserName        :: !Text
    , _scCertificateId   :: !Text
    , _scCertificateBody :: !Text
    , _scStatus          :: !StatusType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SigningCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
signingCertificate
    :: Text -- ^ 'scUserName'
    -> Text -- ^ 'scCertificateId'
    -> Text -- ^ 'scCertificateBody'
    -> StatusType -- ^ 'scStatus'
    -> SigningCertificate
signingCertificate pUserName_ pCertificateId_ pCertificateBody_ pStatus_ =
    SigningCertificate'
    { _scUploadDate = Nothing
    , _scUserName = pUserName_
    , _scCertificateId = pCertificateId_
    , _scCertificateBody = pCertificateBody_
    , _scStatus = pStatus_
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

-- | The status of the signing certificate. 'Active' means the key is valid
-- for API calls, while 'Inactive' means it is not.
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
data User = User'
    { _uPasswordLastUsed :: !(Maybe ISO8601)
    , _uPath             :: !Text
    , _uUserName         :: !Text
    , _uUserId           :: !Text
    , _uARN              :: !Text
    , _uCreateDate       :: !ISO8601
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uPasswordLastUsed'
--
-- * 'uPath'
--
-- * 'uUserName'
--
-- * 'uUserId'
--
-- * 'uARN'
--
-- * 'uCreateDate'
user
    :: Text -- ^ 'uPath'
    -> Text -- ^ 'uUserName'
    -> Text -- ^ 'uUserId'
    -> Text -- ^ 'uARN'
    -> UTCTime -- ^ 'uCreateDate'
    -> User
user pPath_ pUserName_ pUserId_ pARN_ pCreateDate_ =
    User'
    { _uPasswordLastUsed = Nothing
    , _uPath = pPath_
    , _uUserName = pUserName_
    , _uUserId = pUserId_
    , _uARN = pARN_
    , _uCreateDate = _Time # pCreateDate_
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
uPasswordLastUsed :: Lens' User (Maybe UTCTime)
uPasswordLastUsed = lens _uPasswordLastUsed (\ s a -> s{_uPasswordLastUsed = a}) . mapping _Time;

-- | The path to the user. For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
uPath :: Lens' User Text
uPath = lens _uPath (\ s a -> s{_uPath = a});

-- | The friendly name identifying the user.
uUserName :: Lens' User Text
uUserName = lens _uUserName (\ s a -> s{_uUserName = a});

-- | The stable and unique string identifying the user. For more information
-- about IDs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
uUserId :: Lens' User Text
uUserId = lens _uUserId (\ s a -> s{_uUserId = a});

-- | The Amazon Resource Name (ARN) that identifies the user. For more
-- information about ARNs and how to use ARNs in policies, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
uARN :: Lens' User Text
uARN = lens _uARN (\ s a -> s{_uARN = a});

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- user was created.
uCreateDate :: Lens' User UTCTime
uCreateDate = lens _uCreateDate (\ s a -> s{_uCreateDate = a}) . _Time;

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
data UserDetail = UserDetail'
    { _udGroupList               :: !(Maybe [Text])
    , _udARN                     :: !(Maybe Text)
    , _udPath                    :: !(Maybe Text)
    , _udCreateDate              :: !(Maybe ISO8601)
    , _udUserName                :: !(Maybe Text)
    , _udUserId                  :: !(Maybe Text)
    , _udUserPolicyList          :: !(Maybe [PolicyDetail])
    , _udAttachedManagedPolicies :: !(Maybe [AttachedPolicy])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UserDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udGroupList'
--
-- * 'udARN'
--
-- * 'udPath'
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
userDetail
    :: UserDetail
userDetail =
    UserDetail'
    { _udGroupList = Nothing
    , _udARN = Nothing
    , _udPath = Nothing
    , _udCreateDate = Nothing
    , _udUserName = Nothing
    , _udUserId = Nothing
    , _udUserPolicyList = Nothing
    , _udAttachedManagedPolicies = Nothing
    }

-- | A list of IAM groups that the user is in.
udGroupList :: Lens' UserDetail [Text]
udGroupList = lens _udGroupList (\ s a -> s{_udGroupList = a}) . _Default . _Coerce;

-- | Undocumented member.
udARN :: Lens' UserDetail (Maybe Text)
udARN = lens _udARN (\ s a -> s{_udARN = a});

-- | The path to the user. For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
udPath :: Lens' UserDetail (Maybe Text)
udPath = lens _udPath (\ s a -> s{_udPath = a});

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
udUserPolicyList = lens _udUserPolicyList (\ s a -> s{_udUserPolicyList = a}) . _Default . _Coerce;

-- | A list of the managed policies attached to the user.
udAttachedManagedPolicies :: Lens' UserDetail [AttachedPolicy]
udAttachedManagedPolicies = lens _udAttachedManagedPolicies (\ s a -> s{_udAttachedManagedPolicies = a}) . _Default . _Coerce;

instance FromXML UserDetail where
        parseXML x
          = UserDetail' <$>
              (x .@? "GroupList" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@? "Arn")
                <*> (x .@? "Path")
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
data VirtualMFADevice = VirtualMFADevice'
    { _vmdQRCodePNG        :: !(Maybe (Sensitive Base64))
    , _vmdBase32StringSeed :: !(Maybe (Sensitive Base64))
    , _vmdUser             :: !(Maybe User)
    , _vmdEnableDate       :: !(Maybe ISO8601)
    , _vmdSerialNumber     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'VirtualMFADevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
virtualMFADevice
    :: Text -- ^ 'vmdSerialNumber'
    -> VirtualMFADevice
virtualMFADevice pSerialNumber_ =
    VirtualMFADevice'
    { _vmdQRCodePNG = Nothing
    , _vmdBase32StringSeed = Nothing
    , _vmdUser = Nothing
    , _vmdEnableDate = Nothing
    , _vmdSerialNumber = pSerialNumber_
    }

-- | A QR code PNG image that encodes
-- 'otpauth:\/\/totp\/$virtualMFADeviceName\'$AccountName?secret=$Base32String'
-- where '$virtualMFADeviceName' is one of the create call arguments,
-- 'AccountName' is the user name if set (otherwise, the account ID
-- otherwise), and 'Base32String' is the seed in Base32 format. The
-- 'Base32String' value is Base64-encoded.
--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data,
-- despite what the AWS documentation might say.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
vmdQRCodePNG :: Lens' VirtualMFADevice (Maybe ByteString)
vmdQRCodePNG = lens _vmdQRCodePNG (\ s a -> s{_vmdQRCodePNG = a}) . mapping (_Sensitive . _Base64);

-- | The Base32 seed defined as specified in
-- <http://www.ietf.org/rfc/rfc3548.txt RFC3548>. The 'Base32StringSeed' is
-- Base64-encoded.
--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data,
-- despite what the AWS documentation might say.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
vmdBase32StringSeed :: Lens' VirtualMFADevice (Maybe ByteString)
vmdBase32StringSeed = lens _vmdBase32StringSeed (\ s a -> s{_vmdBase32StringSeed = a}) . mapping (_Sensitive . _Base64);

-- | Undocumented member.
vmdUser :: Lens' VirtualMFADevice (Maybe User)
vmdUser = lens _vmdUser (\ s a -> s{_vmdUser = a});

-- | The date and time on which the virtual MFA device was enabled.
vmdEnableDate :: Lens' VirtualMFADevice (Maybe UTCTime)
vmdEnableDate = lens _vmdEnableDate (\ s a -> s{_vmdEnableDate = a}) . mapping _Time;

-- | The serial number associated with 'VirtualMFADevice'.
vmdSerialNumber :: Lens' VirtualMFADevice Text
vmdSerialNumber = lens _vmdSerialNumber (\ s a -> s{_vmdSerialNumber = a});

instance FromXML VirtualMFADevice where
        parseXML x
          = VirtualMFADevice' <$>
              (x .@? "QRCodePNG") <*> (x .@? "Base32StringSeed")
                <*> (x .@? "User")
                <*> (x .@? "EnableDate")
                <*> (x .@ "SerialNumber")
