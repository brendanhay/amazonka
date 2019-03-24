{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.Product where

import Network.AWS.IAM.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an AWS access key.
--
--
-- This data type is used as a response element in the 'CreateAccessKey' and 'ListAccessKeys' operations.
--
--
-- /See:/ 'accessKeyInfo' smart constructor.
data AccessKeyInfo = AccessKeyInfo'
  { _akiCreateDate      :: !(Maybe ISO8601)
  , _akiUserName        :: !Text
  , _akiAccessKeyId     :: !AccessKey
  , _akiStatus          :: !StatusType
  , _akiSecretAccessKey :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccessKeyInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'akiCreateDate' - The date when the access key was created.
--
-- * 'akiUserName' - The name of the IAM user that the access key is associated with.
--
-- * 'akiAccessKeyId' - The ID for this access key.
--
-- * 'akiStatus' - The status of the access key. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not.
--
-- * 'akiSecretAccessKey' - The secret key used to sign requests.
accessKeyInfo
    :: Text -- ^ 'akiUserName'
    -> AccessKey -- ^ 'akiAccessKeyId'
    -> StatusType -- ^ 'akiStatus'
    -> Text -- ^ 'akiSecretAccessKey'
    -> AccessKeyInfo
accessKeyInfo pUserName_ pAccessKeyId_ pStatus_ pSecretAccessKey_ =
  AccessKeyInfo'
    { _akiCreateDate = Nothing
    , _akiUserName = pUserName_
    , _akiAccessKeyId = pAccessKeyId_
    , _akiStatus = pStatus_
    , _akiSecretAccessKey = _Sensitive # pSecretAccessKey_
    }


-- | The date when the access key was created.
akiCreateDate :: Lens' AccessKeyInfo (Maybe UTCTime)
akiCreateDate = lens _akiCreateDate (\ s a -> s{_akiCreateDate = a}) . mapping _Time

-- | The name of the IAM user that the access key is associated with.
akiUserName :: Lens' AccessKeyInfo Text
akiUserName = lens _akiUserName (\ s a -> s{_akiUserName = a})

-- | The ID for this access key.
akiAccessKeyId :: Lens' AccessKeyInfo AccessKey
akiAccessKeyId = lens _akiAccessKeyId (\ s a -> s{_akiAccessKeyId = a})

-- | The status of the access key. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not.
akiStatus :: Lens' AccessKeyInfo StatusType
akiStatus = lens _akiStatus (\ s a -> s{_akiStatus = a})

-- | The secret key used to sign requests.
akiSecretAccessKey :: Lens' AccessKeyInfo Text
akiSecretAccessKey = lens _akiSecretAccessKey (\ s a -> s{_akiSecretAccessKey = a}) . _Sensitive

instance FromXML AccessKeyInfo where
        parseXML x
          = AccessKeyInfo' <$>
              (x .@? "CreateDate") <*> (x .@ "UserName") <*>
                (x .@ "AccessKeyId")
                <*> (x .@ "Status")
                <*> (x .@ "SecretAccessKey")

instance Hashable AccessKeyInfo where

instance NFData AccessKeyInfo where

-- | Contains information about the last time an AWS access key was used since IAM began tracking this information on April 22, 2015.
--
--
-- This data type is used as a response element in the 'GetAccessKeyLastUsed' operation.
--
--
-- /See:/ 'accessKeyLastUsed' smart constructor.
data AccessKeyLastUsed = AccessKeyLastUsed'
  { _akluLastUsedDate :: !ISO8601
  , _akluServiceName  :: !Text
  , _akluRegion       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccessKeyLastUsed' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'akluLastUsedDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the access key was most recently used. This field is null in the following situations:     * The user does not have an access key.     * An access key exists but has not been used since IAM began tracking this information.     * There is no sign-in data associated with the user
--
-- * 'akluServiceName' - The name of the AWS service with which this access key was most recently used. The value of this field is "N/A" in the following situations:     * The user does not have an access key.     * An access key exists but has not been used since IAM started tracking this information.     * There is no sign-in data associated with the user
--
-- * 'akluRegion' - The AWS region where this access key was most recently used. The value for this field is "N/A" in the following situations:     * The user does not have an access key.     * An access key exists but has not been used since IAM began tracking this information.     * There is no sign-in data associated with the user For more information about AWS regions, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> in the Amazon Web Services General Reference.
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


-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the access key was most recently used. This field is null in the following situations:     * The user does not have an access key.     * An access key exists but has not been used since IAM began tracking this information.     * There is no sign-in data associated with the user
akluLastUsedDate :: Lens' AccessKeyLastUsed UTCTime
akluLastUsedDate = lens _akluLastUsedDate (\ s a -> s{_akluLastUsedDate = a}) . _Time

-- | The name of the AWS service with which this access key was most recently used. The value of this field is "N/A" in the following situations:     * The user does not have an access key.     * An access key exists but has not been used since IAM started tracking this information.     * There is no sign-in data associated with the user
akluServiceName :: Lens' AccessKeyLastUsed Text
akluServiceName = lens _akluServiceName (\ s a -> s{_akluServiceName = a})

-- | The AWS region where this access key was most recently used. The value for this field is "N/A" in the following situations:     * The user does not have an access key.     * An access key exists but has not been used since IAM began tracking this information.     * There is no sign-in data associated with the user For more information about AWS regions, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> in the Amazon Web Services General Reference.
akluRegion :: Lens' AccessKeyLastUsed Text
akluRegion = lens _akluRegion (\ s a -> s{_akluRegion = a})

instance FromXML AccessKeyLastUsed where
        parseXML x
          = AccessKeyLastUsed' <$>
              (x .@ "LastUsedDate") <*> (x .@ "ServiceName") <*>
                (x .@ "Region")

instance Hashable AccessKeyLastUsed where

instance NFData AccessKeyLastUsed where

-- | Contains information about an AWS access key, without its secret key.
--
--
-- This data type is used as a response element in the 'ListAccessKeys' operation.
--
--
-- /See:/ 'accessKeyMetadata' smart constructor.
data AccessKeyMetadata = AccessKeyMetadata'
  { _akmStatus      :: !(Maybe StatusType)
  , _akmCreateDate  :: !(Maybe ISO8601)
  , _akmUserName    :: !(Maybe Text)
  , _akmAccessKeyId :: !(Maybe AccessKey)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccessKeyMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'akmStatus' - The status of the access key. @Active@ means that the key is valid for API calls; @Inactive@ means it is not.
--
-- * 'akmCreateDate' - The date when the access key was created.
--
-- * 'akmUserName' - The name of the IAM user that the key is associated with.
--
-- * 'akmAccessKeyId' - The ID for this access key.
accessKeyMetadata
    :: AccessKeyMetadata
accessKeyMetadata =
  AccessKeyMetadata'
    { _akmStatus = Nothing
    , _akmCreateDate = Nothing
    , _akmUserName = Nothing
    , _akmAccessKeyId = Nothing
    }


-- | The status of the access key. @Active@ means that the key is valid for API calls; @Inactive@ means it is not.
akmStatus :: Lens' AccessKeyMetadata (Maybe StatusType)
akmStatus = lens _akmStatus (\ s a -> s{_akmStatus = a})

-- | The date when the access key was created.
akmCreateDate :: Lens' AccessKeyMetadata (Maybe UTCTime)
akmCreateDate = lens _akmCreateDate (\ s a -> s{_akmCreateDate = a}) . mapping _Time

-- | The name of the IAM user that the key is associated with.
akmUserName :: Lens' AccessKeyMetadata (Maybe Text)
akmUserName = lens _akmUserName (\ s a -> s{_akmUserName = a})

-- | The ID for this access key.
akmAccessKeyId :: Lens' AccessKeyMetadata (Maybe AccessKey)
akmAccessKeyId = lens _akmAccessKeyId (\ s a -> s{_akmAccessKeyId = a})

instance FromXML AccessKeyMetadata where
        parseXML x
          = AccessKeyMetadata' <$>
              (x .@? "Status") <*> (x .@? "CreateDate") <*>
                (x .@? "UserName")
                <*> (x .@? "AccessKeyId")

instance Hashable AccessKeyMetadata where

instance NFData AccessKeyMetadata where

-- | Contains information about an attached permissions boundary.
--
--
-- An attached permissions boundary is a managed policy that has been attached to a user or role to set the permissions boundary.
--
-- For more information about permissions boundaries, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
--
--
-- /See:/ 'attachedPermissionsBoundary' smart constructor.
data AttachedPermissionsBoundary = AttachedPermissionsBoundary'
  { _apbPermissionsBoundaryType :: !(Maybe PermissionsBoundaryAttachmentType)
  , _apbPermissionsBoundaryARN  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachedPermissionsBoundary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apbPermissionsBoundaryType' - The permissions boundary usage type that indicates what type of IAM resource is used as the permissions boundary for an entity. This data type can only have a value of @Policy@ .
--
-- * 'apbPermissionsBoundaryARN' - The ARN of the policy used to set the permissions boundary for the user or role.
attachedPermissionsBoundary
    :: AttachedPermissionsBoundary
attachedPermissionsBoundary =
  AttachedPermissionsBoundary'
    { _apbPermissionsBoundaryType = Nothing
    , _apbPermissionsBoundaryARN = Nothing
    }


-- | The permissions boundary usage type that indicates what type of IAM resource is used as the permissions boundary for an entity. This data type can only have a value of @Policy@ .
apbPermissionsBoundaryType :: Lens' AttachedPermissionsBoundary (Maybe PermissionsBoundaryAttachmentType)
apbPermissionsBoundaryType = lens _apbPermissionsBoundaryType (\ s a -> s{_apbPermissionsBoundaryType = a})

-- | The ARN of the policy used to set the permissions boundary for the user or role.
apbPermissionsBoundaryARN :: Lens' AttachedPermissionsBoundary (Maybe Text)
apbPermissionsBoundaryARN = lens _apbPermissionsBoundaryARN (\ s a -> s{_apbPermissionsBoundaryARN = a})

instance FromXML AttachedPermissionsBoundary where
        parseXML x
          = AttachedPermissionsBoundary' <$>
              (x .@? "PermissionsBoundaryType") <*>
                (x .@? "PermissionsBoundaryArn")

instance Hashable AttachedPermissionsBoundary where

instance NFData AttachedPermissionsBoundary where

-- | Contains information about an attached policy.
--
--
-- An attached policy is a managed policy that has been attached to a user, group, or role. This data type is used as a response element in the 'ListAttachedGroupPolicies' , 'ListAttachedRolePolicies' , 'ListAttachedUserPolicies' , and 'GetAccountAuthorizationDetails' operations.
--
-- For more information about managed policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /Using IAM/ guide.
--
--
-- /See:/ 'attachedPolicy' smart constructor.
data AttachedPolicy = AttachedPolicy'
  { _apPolicyName :: !(Maybe Text)
  , _apPolicyARN  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachedPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apPolicyName' - The friendly name of the attached policy.
--
-- * 'apPolicyARN' - Undocumented member.
attachedPolicy
    :: AttachedPolicy
attachedPolicy =
  AttachedPolicy' {_apPolicyName = Nothing, _apPolicyARN = Nothing}


-- | The friendly name of the attached policy.
apPolicyName :: Lens' AttachedPolicy (Maybe Text)
apPolicyName = lens _apPolicyName (\ s a -> s{_apPolicyName = a})

-- | Undocumented member.
apPolicyARN :: Lens' AttachedPolicy (Maybe Text)
apPolicyARN = lens _apPolicyARN (\ s a -> s{_apPolicyARN = a})

instance FromXML AttachedPolicy where
        parseXML x
          = AttachedPolicy' <$>
              (x .@? "PolicyName") <*> (x .@? "PolicyArn")

instance Hashable AttachedPolicy where

instance NFData AttachedPolicy where

-- | Contains information about a condition context key. It includes the name of the key and specifies the value (or values, if the context key supports multiple values) to use in the simulation. This information is used when evaluating the @Condition@ elements of the input policies.
--
--
-- This data type is used as an input parameter to @'SimulateCustomPolicy' @ and @'SimulateCustomPolicy' @ .
--
--
-- /See:/ 'contextEntry' smart constructor.
data ContextEntry = ContextEntry'
  { _ceContextKeyValues :: !(Maybe [Text])
  , _ceContextKeyName   :: !(Maybe Text)
  , _ceContextKeyType   :: !(Maybe ContextKeyTypeEnum)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContextEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceContextKeyValues' - The value (or values, if the condition context key supports multiple values) to provide to the simulation when the key is referenced by a @Condition@ element in an input policy.
--
-- * 'ceContextKeyName' - The full name of a condition context key, including the service prefix. For example, @aws:SourceIp@ or @s3:VersionId@ .
--
-- * 'ceContextKeyType' - The data type of the value (or values) specified in the @ContextKeyValues@ parameter.
contextEntry
    :: ContextEntry
contextEntry =
  ContextEntry'
    { _ceContextKeyValues = Nothing
    , _ceContextKeyName = Nothing
    , _ceContextKeyType = Nothing
    }


-- | The value (or values, if the condition context key supports multiple values) to provide to the simulation when the key is referenced by a @Condition@ element in an input policy.
ceContextKeyValues :: Lens' ContextEntry [Text]
ceContextKeyValues = lens _ceContextKeyValues (\ s a -> s{_ceContextKeyValues = a}) . _Default . _Coerce

-- | The full name of a condition context key, including the service prefix. For example, @aws:SourceIp@ or @s3:VersionId@ .
ceContextKeyName :: Lens' ContextEntry (Maybe Text)
ceContextKeyName = lens _ceContextKeyName (\ s a -> s{_ceContextKeyName = a})

-- | The data type of the value (or values) specified in the @ContextKeyValues@ parameter.
ceContextKeyType :: Lens' ContextEntry (Maybe ContextKeyTypeEnum)
ceContextKeyType = lens _ceContextKeyType (\ s a -> s{_ceContextKeyType = a})

instance Hashable ContextEntry where

instance NFData ContextEntry where

instance ToQuery ContextEntry where
        toQuery ContextEntry'{..}
          = mconcat
              ["ContextKeyValues" =:
                 toQuery
                   (toQueryList "member" <$> _ceContextKeyValues),
               "ContextKeyName" =: _ceContextKeyName,
               "ContextKeyType" =: _ceContextKeyType]

-- | The reason that the service-linked role deletion failed.
--
--
-- This data type is used as a response element in the 'GetServiceLinkedRoleDeletionStatus' operation.
--
--
-- /See:/ 'deletionTaskFailureReasonType' smart constructor.
data DeletionTaskFailureReasonType = DeletionTaskFailureReasonType'
  { _dtfrtRoleUsageList :: !(Maybe [RoleUsageType])
  , _dtfrtReason        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletionTaskFailureReasonType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtfrtRoleUsageList' - A list of objects that contains details about the service-linked role deletion failure, if that information is returned by the service. If the service-linked role has active sessions or if any resources that were used by the role have not been deleted from the linked service, the role can't be deleted. This parameter includes a list of the resources that are associated with the role and the region in which the resources are being used.
--
-- * 'dtfrtReason' - A short description of the reason that the service-linked role deletion failed.
deletionTaskFailureReasonType
    :: DeletionTaskFailureReasonType
deletionTaskFailureReasonType =
  DeletionTaskFailureReasonType'
    {_dtfrtRoleUsageList = Nothing, _dtfrtReason = Nothing}


-- | A list of objects that contains details about the service-linked role deletion failure, if that information is returned by the service. If the service-linked role has active sessions or if any resources that were used by the role have not been deleted from the linked service, the role can't be deleted. This parameter includes a list of the resources that are associated with the role and the region in which the resources are being used.
dtfrtRoleUsageList :: Lens' DeletionTaskFailureReasonType [RoleUsageType]
dtfrtRoleUsageList = lens _dtfrtRoleUsageList (\ s a -> s{_dtfrtRoleUsageList = a}) . _Default . _Coerce

-- | A short description of the reason that the service-linked role deletion failed.
dtfrtReason :: Lens' DeletionTaskFailureReasonType (Maybe Text)
dtfrtReason = lens _dtfrtReason (\ s a -> s{_dtfrtReason = a})

instance FromXML DeletionTaskFailureReasonType where
        parseXML x
          = DeletionTaskFailureReasonType' <$>
              (x .@? "RoleUsageList" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@? "Reason")

instance Hashable DeletionTaskFailureReasonType where

instance NFData DeletionTaskFailureReasonType where

-- | An object that contains details about when the IAM entities (users or roles) were last used in an attempt to access the specified AWS service.
--
--
-- This data type is a response element in the 'GetServiceLastAccessedDetailsWithEntities' operation.
--
--
-- /See:/ 'entityDetails' smart constructor.
data EntityDetails = EntityDetails'
  { _edLastAuthenticated :: !(Maybe ISO8601)
  , _edEntityInfo        :: !EntityInfo
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EntityDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edLastAuthenticated' - The date and time, in
