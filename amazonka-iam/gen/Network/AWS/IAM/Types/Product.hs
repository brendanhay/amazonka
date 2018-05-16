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

-- | Contains information about the last time an AWS access key was used.
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
-- * 'akluLastUsedDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the access key was most recently used. This field is null in the following situations:     * The user does not have an access key.     * An access key exists but has never been used, at least not since IAM started tracking this information on April 22nd, 2015.     * There is no sign-in data associated with the user
--
-- * 'akluServiceName' - The name of the AWS service with which this access key was most recently used. This field displays "N/A" in the following situations:     * The user does not have an access key.     * An access key exists but has never been used, at least not since IAM started tracking this information on April 22nd, 2015.     * There is no sign-in data associated with the user
--
-- * 'akluRegion' - The AWS region where this access key was most recently used. This field is displays "N/A" in the following situations:     * The user does not have an access key.     * An access key exists but has never been used, at least not since IAM started tracking this information on April 22nd, 2015.     * There is no sign-in data associated with the user For more information about AWS regions, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> in the Amazon Web Services General Reference.
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


-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the access key was most recently used. This field is null in the following situations:     * The user does not have an access key.     * An access key exists but has never been used, at least not since IAM started tracking this information on April 22nd, 2015.     * There is no sign-in data associated with the user
akluLastUsedDate :: Lens' AccessKeyLastUsed UTCTime
akluLastUsedDate = lens _akluLastUsedDate (\ s a -> s{_akluLastUsedDate = a}) . _Time

-- | The name of the AWS service with which this access key was most recently used. This field displays "N/A" in the following situations:     * The user does not have an access key.     * An access key exists but has never been used, at least not since IAM started tracking this information on April 22nd, 2015.     * There is no sign-in data associated with the user
akluServiceName :: Lens' AccessKeyLastUsed Text
akluServiceName = lens _akluServiceName (\ s a -> s{_akluServiceName = a})

-- | The AWS region where this access key was most recently used. This field is displays "N/A" in the following situations:     * The user does not have an access key.     * An access key exists but has never been used, at least not since IAM started tracking this information on April 22nd, 2015.     * There is no sign-in data associated with the user For more information about AWS regions, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> in the Amazon Web Services General Reference.
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
-- * 'akmStatus' - The status of the access key. @Active@ means the key is valid for API calls; @Inactive@ means it is not.
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


-- | The status of the access key. @Active@ means the key is valid for API calls; @Inactive@ means it is not.
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

-- | Contains the results of a simulation.
--
--
-- This data type is used by the return parameter of @'SimulateCustomPolicy' @ and @'SimulatePrincipalPolicy' @ .
--
--
-- /See:/ 'evaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
  { _erMatchedStatements :: !(Maybe [Statement])
  , _erEvalDecisionDetails :: !(Maybe (Map Text PolicyEvaluationDecisionType))
  , _erResourceSpecificResults :: !(Maybe [ResourceSpecificResult])
  , _erEvalResourceName :: !(Maybe Text)
  , _erMissingContextValues :: !(Maybe [Text])
  , _erOrganizationsDecisionDetail :: !(Maybe OrganizationsDecisionDetail)
  , _erEvalActionName :: !Text
  , _erEvalDecision :: !PolicyEvaluationDecisionType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EvaluationResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erMatchedStatements' - A list of the statements in the input policies that determine the result for this scenario. Remember that even if multiple statements allow the operation on the resource, if only one statement denies that operation, then the explicit deny overrides any allow, and the deny statement is the only entry included in the result.
--
-- * 'erEvalDecisionDetails' - Additional details about the results of the evaluation decision. When there are both IAM policies and resource policies, this parameter explains how each set of policies contributes to the final evaluation decision. When simulating cross-account access to a resource, both the resource-based policy and the caller's IAM policy must grant access. See <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_compare-resource-policies.html How IAM Roles Differ from Resource-based Policies>
--
-- * 'erResourceSpecificResults' - The individual results of the simulation of the API operation specified in EvalActionName on each resource.
--
-- * 'erEvalResourceName' - The ARN of the resource that the indicated API operation was tested on.
--
-- * 'erMissingContextValues' - A list of context keys that are required by the included input policies but that were not provided by one of the input parameters. This list is used when the resource in a simulation is "*", either explicitly, or when the @ResourceArns@ parameter blank. If you include a list of resources, then any missing context values are instead included under the @ResourceSpecificResults@ section. To discover the context keys used by a set of policies, you can call 'GetContextKeysForCustomPolicy' or 'GetContextKeysForPrincipalPolicy' .
--
-- * 'erOrganizationsDecisionDetail' - A structure that details how AWS Organizations and its service control policies affect the results of the simulation. Only applies if the simulated user's account is part of an organization.
--
-- * 'erEvalActionName' - The name of the API operation tested on the indicated resource.
--
-- * 'erEvalDecision' - The result of the simulation.
evaluationResult
    :: Text -- ^ 'erEvalActionName'
    -> PolicyEvaluationDecisionType -- ^ 'erEvalDecision'
    -> EvaluationResult
evaluationResult pEvalActionName_ pEvalDecision_ =
  EvaluationResult'
    { _erMatchedStatements = Nothing
    , _erEvalDecisionDetails = Nothing
    , _erResourceSpecificResults = Nothing
    , _erEvalResourceName = Nothing
    , _erMissingContextValues = Nothing
    , _erOrganizationsDecisionDetail = Nothing
    , _erEvalActionName = pEvalActionName_
    , _erEvalDecision = pEvalDecision_
    }


-- | A list of the statements in the input policies that determine the result for this scenario. Remember that even if multiple statements allow the operation on the resource, if only one statement denies that operation, then the explicit deny overrides any allow, and the deny statement is the only entry included in the result.
erMatchedStatements :: Lens' EvaluationResult [Statement]
erMatchedStatements = lens _erMatchedStatements (\ s a -> s{_erMatchedStatements = a}) . _Default . _Coerce

-- | Additional details about the results of the evaluation decision. When there are both IAM policies and resource policies, this parameter explains how each set of policies contributes to the final evaluation decision. When simulating cross-account access to a resource, both the resource-based policy and the caller's IAM policy must grant access. See <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_compare-resource-policies.html How IAM Roles Differ from Resource-based Policies>
erEvalDecisionDetails :: Lens' EvaluationResult (HashMap Text PolicyEvaluationDecisionType)
erEvalDecisionDetails = lens _erEvalDecisionDetails (\ s a -> s{_erEvalDecisionDetails = a}) . _Default . _Map

-- | The individual results of the simulation of the API operation specified in EvalActionName on each resource.
erResourceSpecificResults :: Lens' EvaluationResult [ResourceSpecificResult]
erResourceSpecificResults = lens _erResourceSpecificResults (\ s a -> s{_erResourceSpecificResults = a}) . _Default . _Coerce

-- | The ARN of the resource that the indicated API operation was tested on.
erEvalResourceName :: Lens' EvaluationResult (Maybe Text)
erEvalResourceName = lens _erEvalResourceName (\ s a -> s{_erEvalResourceName = a})

-- | A list of context keys that are required by the included input policies but that were not provided by one of the input parameters. This list is used when the resource in a simulation is "*", either explicitly, or when the @ResourceArns@ parameter blank. If you include a list of resources, then any missing context values are instead included under the @ResourceSpecificResults@ section. To discover the context keys used by a set of policies, you can call 'GetContextKeysForCustomPolicy' or 'GetContextKeysForPrincipalPolicy' .
erMissingContextValues :: Lens' EvaluationResult [Text]
erMissingContextValues = lens _erMissingContextValues (\ s a -> s{_erMissingContextValues = a}) . _Default . _Coerce

-- | A structure that details how AWS Organizations and its service control policies affect the results of the simulation. Only applies if the simulated user's account is part of an organization.
erOrganizationsDecisionDetail :: Lens' EvaluationResult (Maybe OrganizationsDecisionDetail)
erOrganizationsDecisionDetail = lens _erOrganizationsDecisionDetail (\ s a -> s{_erOrganizationsDecisionDetail = a})

-- | The name of the API operation tested on the indicated resource.
erEvalActionName :: Lens' EvaluationResult Text
erEvalActionName = lens _erEvalActionName (\ s a -> s{_erEvalActionName = a})

-- | The result of the simulation.
erEvalDecision :: Lens' EvaluationResult PolicyEvaluationDecisionType
erEvalDecision = lens _erEvalDecision (\ s a -> s{_erEvalDecision = a})

instance FromXML EvaluationResult where
        parseXML x
          = EvaluationResult' <$>
              (x .@? "MatchedStatements" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*>
                (x .@? "EvalDecisionDetails" .!@ mempty >>=
                   may (parseXMLMap "entry" "key" "value"))
                <*>
                (x .@? "ResourceSpecificResults" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "EvalResourceName")
                <*>
                (x .@? "MissingContextValues" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "OrganizationsDecisionDetail")
                <*> (x .@ "EvalActionName")
                <*> (x .@ "EvalDecision")

instance Hashable EvaluationResult where

instance NFData EvaluationResult where

-- | Contains the response to a successful 'GetContextKeysForPrincipalPolicy' or 'GetContextKeysForCustomPolicy' request.
--
--
--
-- /See:/ 'getContextKeysForPolicyResponse' smart constructor.
newtype GetContextKeysForPolicyResponse = GetContextKeysForPolicyResponse'
  { _gckfpContextKeyNames :: Maybe [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetContextKeysForPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gckfpContextKeyNames' - The list of context keys that are referenced in the input policies.
getContextKeysForPolicyResponse
    :: GetContextKeysForPolicyResponse
getContextKeysForPolicyResponse =
  GetContextKeysForPolicyResponse' {_gckfpContextKeyNames = Nothing}


-- | The list of context keys that are referenced in the input policies.
gckfpContextKeyNames :: Lens' GetContextKeysForPolicyResponse [Text]
gckfpContextKeyNames = lens _gckfpContextKeyNames (\ s a -> s{_gckfpContextKeyNames = a}) . _Default . _Coerce

instance FromXML GetContextKeysForPolicyResponse
         where
        parseXML x
          = GetContextKeysForPolicyResponse' <$>
              (x .@? "ContextKeyNames" .!@ mempty >>=
                 may (parseXMLList "member"))

instance Hashable GetContextKeysForPolicyResponse
         where

instance NFData GetContextKeysForPolicyResponse where

-- | Contains information about an IAM group entity.
--
--
-- This data type is used as a response element in the following operations:
--
--     * 'CreateGroup'
--
--     * 'GetGroup'
--
--     * 'ListGroups'
--
--
--
--
-- /See:/ 'group'' smart constructor.
data Group = Group'
  { _gPath       :: !Text
  , _gGroupName  :: !Text
  , _gGroupId    :: !Text
  , _gARN        :: !Text
  , _gCreateDate :: !ISO8601
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Group' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gPath' - The path to the group. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- * 'gGroupName' - The friendly name that identifies the group.
--
-- * 'gGroupId' - The stable and unique string identifying the group. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- * 'gARN' - The Amazon Resource Name (ARN) specifying the group. For more information about ARNs and how to use them in policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- * 'gCreateDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the group was created.
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


-- | The path to the group. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
gPath :: Lens' Group Text
gPath = lens _gPath (\ s a -> s{_gPath = a})

-- | The friendly name that identifies the group.
gGroupName :: Lens' Group Text
gGroupName = lens _gGroupName (\ s a -> s{_gGroupName = a})

-- | The stable and unique string identifying the group. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
gGroupId :: Lens' Group Text
gGroupId = lens _gGroupId (\ s a -> s{_gGroupId = a})

-- | The Amazon Resource Name (ARN) specifying the group. For more information about ARNs and how to use them in policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
gARN :: Lens' Group Text
gARN = lens _gARN (\ s a -> s{_gARN = a})

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the group was created.
gCreateDate :: Lens' Group UTCTime
gCreateDate = lens _gCreateDate (\ s a -> s{_gCreateDate = a}) . _Time

instance FromXML Group where
        parseXML x
          = Group' <$>
              (x .@ "Path") <*> (x .@ "GroupName") <*>
                (x .@ "GroupId")
                <*> (x .@ "Arn")
                <*> (x .@ "CreateDate")

instance Hashable Group where

instance NFData Group where

-- | Contains information about an IAM group, including all of the group's policies.
--
--
-- This data type is used as a response element in the 'GetAccountAuthorizationDetails' operation.
--
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GroupDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdARN' - Undocumented member.
--
-- * 'gdPath' - The path to the group. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- * 'gdCreateDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the group was created.
--
-- * 'gdGroupId' - The stable and unique string identifying the group. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- * 'gdGroupPolicyList' - A list of the inline policies embedded in the group.
--
-- * 'gdGroupName' - The friendly name that identifies the group.
--
-- * 'gdAttachedManagedPolicies' - A list of the managed policies attached to the group.
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
gdARN = lens _gdARN (\ s a -> s{_gdARN = a})

-- | The path to the group. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
gdPath :: Lens' GroupDetail (Maybe Text)
gdPath = lens _gdPath (\ s a -> s{_gdPath = a})

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the group was created.
gdCreateDate :: Lens' GroupDetail (Maybe UTCTime)
gdCreateDate = lens _gdCreateDate (\ s a -> s{_gdCreateDate = a}) . mapping _Time

-- | The stable and unique string identifying the group. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
gdGroupId :: Lens' GroupDetail (Maybe Text)
gdGroupId = lens _gdGroupId (\ s a -> s{_gdGroupId = a})

-- | A list of the inline policies embedded in the group.
gdGroupPolicyList :: Lens' GroupDetail [PolicyDetail]
gdGroupPolicyList = lens _gdGroupPolicyList (\ s a -> s{_gdGroupPolicyList = a}) . _Default . _Coerce

-- | The friendly name that identifies the group.
gdGroupName :: Lens' GroupDetail (Maybe Text)
gdGroupName = lens _gdGroupName (\ s a -> s{_gdGroupName = a})

-- | A list of the managed policies attached to the group.
gdAttachedManagedPolicies :: Lens' GroupDetail [AttachedPolicy]
gdAttachedManagedPolicies = lens _gdAttachedManagedPolicies (\ s a -> s{_gdAttachedManagedPolicies = a}) . _Default . _Coerce

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

instance Hashable GroupDetail where

instance NFData GroupDetail where

-- | Contains information about an instance profile.
--
--
-- This data type is used as a response element in the following operations:
--
--     * 'CreateInstanceProfile'
--
--     * 'GetInstanceProfile'
--
--     * 'ListInstanceProfiles'
--
--     * 'ListInstanceProfilesForRole'
--
--
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipPath' - The path to the instance profile. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- * 'ipInstanceProfileName' - The name identifying the instance profile.
--
-- * 'ipInstanceProfileId' - The stable and unique string identifying the instance profile. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- * 'ipARN' - The Amazon Resource Name (ARN) specifying the instance profile. For more information about ARNs and how to use them in policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- * 'ipCreateDate' - The date when the instance profile was created.
--
-- * 'ipRoles' - The role associated with the instance profile.
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


-- | The path to the instance profile. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
ipPath :: Lens' InstanceProfile Text
ipPath = lens _ipPath (\ s a -> s{_ipPath = a})

-- | The name identifying the instance profile.
ipInstanceProfileName :: Lens' InstanceProfile Text
ipInstanceProfileName = lens _ipInstanceProfileName (\ s a -> s{_ipInstanceProfileName = a})

-- | The stable and unique string identifying the instance profile. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
ipInstanceProfileId :: Lens' InstanceProfile Text
ipInstanceProfileId = lens _ipInstanceProfileId (\ s a -> s{_ipInstanceProfileId = a})

-- | The Amazon Resource Name (ARN) specifying the instance profile. For more information about ARNs and how to use them in policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
ipARN :: Lens' InstanceProfile Text
ipARN = lens _ipARN (\ s a -> s{_ipARN = a})

-- | The date when the instance profile was created.
ipCreateDate :: Lens' InstanceProfile UTCTime
ipCreateDate = lens _ipCreateDate (\ s a -> s{_ipCreateDate = a}) . _Time

-- | The role associated with the instance profile.
ipRoles :: Lens' InstanceProfile [Role]
ipRoles = lens _ipRoles (\ s a -> s{_ipRoles = a}) . _Coerce

instance FromXML InstanceProfile where
        parseXML x
          = InstanceProfile' <$>
              (x .@ "Path") <*> (x .@ "InstanceProfileName") <*>
                (x .@ "InstanceProfileId")
                <*> (x .@ "Arn")
                <*> (x .@ "CreateDate")
                <*>
                (x .@? "Roles" .!@ mempty >>= parseXMLList "member")

instance Hashable InstanceProfile where

instance NFData InstanceProfile where

-- | Contains the user name and password create date for a user.
--
--
-- This data type is used as a response element in the 'CreateLoginProfile' and 'GetLoginProfile' operations.
--
--
-- /See:/ 'loginProfile' smart constructor.
data LoginProfile = LoginProfile'
  { _lpPasswordResetRequired :: !(Maybe Bool)
  , _lpUserName              :: !Text
  , _lpCreateDate            :: !ISO8601
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoginProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpPasswordResetRequired' - Specifies whether the user is required to set a new password on next sign-in.
--
-- * 'lpUserName' - The name of the user, which can be used for signing in to the AWS Management Console.
--
-- * 'lpCreateDate' - The date when the password for the user was created.
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


-- | Specifies whether the user is required to set a new password on next sign-in.
lpPasswordResetRequired :: Lens' LoginProfile (Maybe Bool)
lpPasswordResetRequired = lens _lpPasswordResetRequired (\ s a -> s{_lpPasswordResetRequired = a})

-- | The name of the user, which can be used for signing in to the AWS Management Console.
lpUserName :: Lens' LoginProfile Text
lpUserName = lens _lpUserName (\ s a -> s{_lpUserName = a})

-- | The date when the password for the user was created.
lpCreateDate :: Lens' LoginProfile UTCTime
lpCreateDate = lens _lpCreateDate (\ s a -> s{_lpCreateDate = a}) . _Time

instance FromXML LoginProfile where
        parseXML x
          = LoginProfile' <$>
              (x .@? "PasswordResetRequired") <*> (x .@ "UserName")
                <*> (x .@ "CreateDate")

instance Hashable LoginProfile where

instance NFData LoginProfile where

-- | Contains information about an MFA device.
--
--
-- This data type is used as a response element in the 'ListMFADevices' operation.
--
--
-- /See:/ 'mfaDevice' smart constructor.
data MFADevice = MFADevice'
  { _mdUserName     :: !Text
  , _mdSerialNumber :: !Text
  , _mdEnableDate   :: !ISO8601
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MFADevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdUserName' - The user with whom the MFA device is associated.
--
-- * 'mdSerialNumber' - The serial number that uniquely identifies the MFA device. For virtual MFA devices, the serial number is the device ARN.
--
-- * 'mdEnableDate' - The date when the MFA device was enabled for the user.
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
mdUserName = lens _mdUserName (\ s a -> s{_mdUserName = a})

-- | The serial number that uniquely identifies the MFA device. For virtual MFA devices, the serial number is the device ARN.
mdSerialNumber :: Lens' MFADevice Text
mdSerialNumber = lens _mdSerialNumber (\ s a -> s{_mdSerialNumber = a})

-- | The date when the MFA device was enabled for the user.
mdEnableDate :: Lens' MFADevice UTCTime
mdEnableDate = lens _mdEnableDate (\ s a -> s{_mdEnableDate = a}) . _Time

instance FromXML MFADevice where
        parseXML x
          = MFADevice' <$>
              (x .@ "UserName") <*> (x .@ "SerialNumber") <*>
                (x .@ "EnableDate")

instance Hashable MFADevice where

instance NFData MFADevice where

-- | Contains information about a managed policy, including the policy's ARN, versions, and the number of principal entities (users, groups, and roles) that the policy is attached to.
--
--
-- This data type is used as a response element in the 'GetAccountAuthorizationDetails' operation.
--
-- For more information about managed policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /Using IAM/ guide.
--
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ManagedPolicyDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpdPolicyName' - The friendly name (not ARN) identifying the policy.
--
-- * 'mpdARN' - Undocumented member.
--
-- * 'mpdUpdateDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was last updated. When a policy has only one version, this field contains the date and time when the policy was created. When a policy has more than one version, this field contains the date and time when the most recent policy version was created.
--
-- * 'mpdPolicyId' - The stable and unique string identifying the policy. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- * 'mpdPath' - The path to the policy. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- * 'mpdPolicyVersionList' - A list containing information about the versions of the policy.
--
-- * 'mpdCreateDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was created.
--
-- * 'mpdIsAttachable' - Specifies whether the policy can be attached to an IAM user, group, or role.
--
-- * 'mpdDefaultVersionId' - The identifier for the version of the policy that is set as the default (operative) version. For more information about policy versions, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /Using IAM/ guide.
--
-- * 'mpdAttachmentCount' - The number of principal entities (users, groups, and roles) that the policy is attached to.
--
-- * 'mpdDescription' - A friendly description of the policy.
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
mpdPolicyName = lens _mpdPolicyName (\ s a -> s{_mpdPolicyName = a})

-- | Undocumented member.
mpdARN :: Lens' ManagedPolicyDetail (Maybe Text)
mpdARN = lens _mpdARN (\ s a -> s{_mpdARN = a})

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was last updated. When a policy has only one version, this field contains the date and time when the policy was created. When a policy has more than one version, this field contains the date and time when the most recent policy version was created.
mpdUpdateDate :: Lens' ManagedPolicyDetail (Maybe UTCTime)
mpdUpdateDate = lens _mpdUpdateDate (\ s a -> s{_mpdUpdateDate = a}) . mapping _Time

-- | The stable and unique string identifying the policy. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
mpdPolicyId :: Lens' ManagedPolicyDetail (Maybe Text)
mpdPolicyId = lens _mpdPolicyId (\ s a -> s{_mpdPolicyId = a})

-- | The path to the policy. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
mpdPath :: Lens' ManagedPolicyDetail (Maybe Text)
mpdPath = lens _mpdPath (\ s a -> s{_mpdPath = a})

-- | A list containing information about the versions of the policy.
mpdPolicyVersionList :: Lens' ManagedPolicyDetail [PolicyVersion]
mpdPolicyVersionList = lens _mpdPolicyVersionList (\ s a -> s{_mpdPolicyVersionList = a}) . _Default . _Coerce

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was created.
mpdCreateDate :: Lens' ManagedPolicyDetail (Maybe UTCTime)
mpdCreateDate = lens _mpdCreateDate (\ s a -> s{_mpdCreateDate = a}) . mapping _Time

-- | Specifies whether the policy can be attached to an IAM user, group, or role.
mpdIsAttachable :: Lens' ManagedPolicyDetail (Maybe Bool)
mpdIsAttachable = lens _mpdIsAttachable (\ s a -> s{_mpdIsAttachable = a})

-- | The identifier for the version of the policy that is set as the default (operative) version. For more information about policy versions, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /Using IAM/ guide.
mpdDefaultVersionId :: Lens' ManagedPolicyDetail (Maybe Text)
mpdDefaultVersionId = lens _mpdDefaultVersionId (\ s a -> s{_mpdDefaultVersionId = a})

-- | The number of principal entities (users, groups, and roles) that the policy is attached to.
mpdAttachmentCount :: Lens' ManagedPolicyDetail (Maybe Int)
mpdAttachmentCount = lens _mpdAttachmentCount (\ s a -> s{_mpdAttachmentCount = a})

-- | A friendly description of the policy.
mpdDescription :: Lens' ManagedPolicyDetail (Maybe Text)
mpdDescription = lens _mpdDescription (\ s a -> s{_mpdDescription = a})

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

instance Hashable ManagedPolicyDetail where

instance NFData ManagedPolicyDetail where

-- | Contains the Amazon Resource Name (ARN) for an IAM OpenID Connect provider.
--
--
--
-- /See:/ 'openIdConnectProviderListEntry' smart constructor.
newtype OpenIdConnectProviderListEntry = OpenIdConnectProviderListEntry'
  { _oicpleARN :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OpenIdConnectProviderListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oicpleARN' - Undocumented member.
openIdConnectProviderListEntry
    :: OpenIdConnectProviderListEntry
openIdConnectProviderListEntry =
  OpenIdConnectProviderListEntry' {_oicpleARN = Nothing}


-- | Undocumented member.
oicpleARN :: Lens' OpenIdConnectProviderListEntry (Maybe Text)
oicpleARN = lens _oicpleARN (\ s a -> s{_oicpleARN = a})

instance FromXML OpenIdConnectProviderListEntry where
        parseXML x
          = OpenIdConnectProviderListEntry' <$> (x .@? "Arn")

instance Hashable OpenIdConnectProviderListEntry
         where

instance NFData OpenIdConnectProviderListEntry where

-- | Contains information about AWS Organizations's effect on a policy simulation.
--
--
--
-- /See:/ 'organizationsDecisionDetail' smart constructor.
newtype OrganizationsDecisionDetail = OrganizationsDecisionDetail'
  { _oddAllowedByOrganizations :: Maybe Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OrganizationsDecisionDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oddAllowedByOrganizations' - Specifies whether the simulated operation is allowed by the AWS Organizations service control policies that impact the simulated user's account.
organizationsDecisionDetail
    :: OrganizationsDecisionDetail
organizationsDecisionDetail =
  OrganizationsDecisionDetail' {_oddAllowedByOrganizations = Nothing}


-- | Specifies whether the simulated operation is allowed by the AWS Organizations service control policies that impact the simulated user's account.
oddAllowedByOrganizations :: Lens' OrganizationsDecisionDetail (Maybe Bool)
oddAllowedByOrganizations = lens _oddAllowedByOrganizations (\ s a -> s{_oddAllowedByOrganizations = a})

instance FromXML OrganizationsDecisionDetail where
        parseXML x
          = OrganizationsDecisionDetail' <$>
              (x .@? "AllowedByOrganizations")

instance Hashable OrganizationsDecisionDetail where

instance NFData OrganizationsDecisionDetail where

-- | Contains information about the account password policy.
--
--
-- This data type is used as a response element in the 'GetAccountPasswordPolicy' operation.
--
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PasswordPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppExpirePasswords' - Indicates whether passwords in the account expire. Returns true if @MaxPasswordAge@ contains a value greater than 0. Returns false if MaxPasswordAge is 0 or not present.
--
-- * 'ppMinimumPasswordLength' - Minimum length to require for IAM user passwords.
--
-- * 'ppRequireNumbers' - Specifies whether to require numbers for IAM user passwords.
--
-- * 'ppPasswordReusePrevention' - Specifies the number of previous passwords that IAM users are prevented from reusing.
--
-- * 'ppRequireLowercaseCharacters' - Specifies whether to require lowercase characters for IAM user passwords.
--
-- * 'ppMaxPasswordAge' - The number of days that an IAM user password is valid.
--
-- * 'ppHardExpiry' - Specifies whether IAM users are prevented from setting a new password after their password has expired.
--
-- * 'ppRequireSymbols' - Specifies whether to require symbols for IAM user passwords.
--
-- * 'ppRequireUppercaseCharacters' - Specifies whether to require uppercase characters for IAM user passwords.
--
-- * 'ppAllowUsersToChangePassword' - Specifies whether IAM users are allowed to change their own password.
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


-- | Indicates whether passwords in the account expire. Returns true if @MaxPasswordAge@ contains a value greater than 0. Returns false if MaxPasswordAge is 0 or not present.
ppExpirePasswords :: Lens' PasswordPolicy (Maybe Bool)
ppExpirePasswords = lens _ppExpirePasswords (\ s a -> s{_ppExpirePasswords = a})

-- | Minimum length to require for IAM user passwords.
ppMinimumPasswordLength :: Lens' PasswordPolicy (Maybe Natural)
ppMinimumPasswordLength = lens _ppMinimumPasswordLength (\ s a -> s{_ppMinimumPasswordLength = a}) . mapping _Nat

-- | Specifies whether to require numbers for IAM user passwords.
ppRequireNumbers :: Lens' PasswordPolicy (Maybe Bool)
ppRequireNumbers = lens _ppRequireNumbers (\ s a -> s{_ppRequireNumbers = a})

-- | Specifies the number of previous passwords that IAM users are prevented from reusing.
ppPasswordReusePrevention :: Lens' PasswordPolicy (Maybe Natural)
ppPasswordReusePrevention = lens _ppPasswordReusePrevention (\ s a -> s{_ppPasswordReusePrevention = a}) . mapping _Nat

-- | Specifies whether to require lowercase characters for IAM user passwords.
ppRequireLowercaseCharacters :: Lens' PasswordPolicy (Maybe Bool)
ppRequireLowercaseCharacters = lens _ppRequireLowercaseCharacters (\ s a -> s{_ppRequireLowercaseCharacters = a})

-- | The number of days that an IAM user password is valid.
ppMaxPasswordAge :: Lens' PasswordPolicy (Maybe Natural)
ppMaxPasswordAge = lens _ppMaxPasswordAge (\ s a -> s{_ppMaxPasswordAge = a}) . mapping _Nat

-- | Specifies whether IAM users are prevented from setting a new password after their password has expired.
ppHardExpiry :: Lens' PasswordPolicy (Maybe Bool)
ppHardExpiry = lens _ppHardExpiry (\ s a -> s{_ppHardExpiry = a})

-- | Specifies whether to require symbols for IAM user passwords.
ppRequireSymbols :: Lens' PasswordPolicy (Maybe Bool)
ppRequireSymbols = lens _ppRequireSymbols (\ s a -> s{_ppRequireSymbols = a})

-- | Specifies whether to require uppercase characters for IAM user passwords.
ppRequireUppercaseCharacters :: Lens' PasswordPolicy (Maybe Bool)
ppRequireUppercaseCharacters = lens _ppRequireUppercaseCharacters (\ s a -> s{_ppRequireUppercaseCharacters = a})

-- | Specifies whether IAM users are allowed to change their own password.
ppAllowUsersToChangePassword :: Lens' PasswordPolicy (Maybe Bool)
ppAllowUsersToChangePassword = lens _ppAllowUsersToChangePassword (\ s a -> s{_ppAllowUsersToChangePassword = a})

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

instance Hashable PasswordPolicy where

instance NFData PasswordPolicy where

-- | Contains information about a managed policy.
--
--
-- This data type is used as a response element in the 'CreatePolicy' , 'GetPolicy' , and 'ListPolicies' operations.
--
-- For more information about managed policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /Using IAM/ guide.
--
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Policy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pPolicyName' - The friendly name (not ARN) identifying the policy.
--
-- * 'pARN' - Undocumented member.
--
-- * 'pUpdateDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was last updated. When a policy has only one version, this field contains the date and time when the policy was created. When a policy has more than one version, this field contains the date and time when the most recent policy version was created.
--
-- * 'pPolicyId' - The stable and unique string identifying the policy. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- * 'pPath' - The path to the policy. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- * 'pCreateDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was created.
--
-- * 'pIsAttachable' - Specifies whether the policy can be attached to an IAM user, group, or role.
--
-- * 'pDefaultVersionId' - The identifier for the version of the policy that is set as the default version.
--
-- * 'pAttachmentCount' - The number of entities (users, groups, and roles) that the policy is attached to.
--
-- * 'pDescription' - A friendly description of the policy. This element is included in the response to the 'GetPolicy' operation. It is not included in the response to the 'ListPolicies' operation.
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
pPolicyName = lens _pPolicyName (\ s a -> s{_pPolicyName = a})

-- | Undocumented member.
pARN :: Lens' Policy (Maybe Text)
pARN = lens _pARN (\ s a -> s{_pARN = a})

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was last updated. When a policy has only one version, this field contains the date and time when the policy was created. When a policy has more than one version, this field contains the date and time when the most recent policy version was created.
pUpdateDate :: Lens' Policy (Maybe UTCTime)
pUpdateDate = lens _pUpdateDate (\ s a -> s{_pUpdateDate = a}) . mapping _Time

-- | The stable and unique string identifying the policy. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
pPolicyId :: Lens' Policy (Maybe Text)
pPolicyId = lens _pPolicyId (\ s a -> s{_pPolicyId = a})

-- | The path to the policy. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
pPath :: Lens' Policy (Maybe Text)
pPath = lens _pPath (\ s a -> s{_pPath = a})

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was created.
pCreateDate :: Lens' Policy (Maybe UTCTime)
pCreateDate = lens _pCreateDate (\ s a -> s{_pCreateDate = a}) . mapping _Time

-- | Specifies whether the policy can be attached to an IAM user, group, or role.
pIsAttachable :: Lens' Policy (Maybe Bool)
pIsAttachable = lens _pIsAttachable (\ s a -> s{_pIsAttachable = a})

-- | The identifier for the version of the policy that is set as the default version.
pDefaultVersionId :: Lens' Policy (Maybe Text)
pDefaultVersionId = lens _pDefaultVersionId (\ s a -> s{_pDefaultVersionId = a})

-- | The number of entities (users, groups, and roles) that the policy is attached to.
pAttachmentCount :: Lens' Policy (Maybe Int)
pAttachmentCount = lens _pAttachmentCount (\ s a -> s{_pAttachmentCount = a})

-- | A friendly description of the policy. This element is included in the response to the 'GetPolicy' operation. It is not included in the response to the 'ListPolicies' operation.
pDescription :: Lens' Policy (Maybe Text)
pDescription = lens _pDescription (\ s a -> s{_pDescription = a})

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

instance Hashable Policy where

instance NFData Policy where

-- | Contains information about an IAM policy, including the policy document.
--
--
-- This data type is used as a response element in the 'GetAccountAuthorizationDetails' operation.
--
--
-- /See:/ 'policyDetail' smart constructor.
data PolicyDetail = PolicyDetail'
  { _pdPolicyDocument :: !(Maybe Text)
  , _pdPolicyName     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PolicyDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdPolicyDocument' - The policy document.
--
-- * 'pdPolicyName' - The name of the policy.
policyDetail
    :: PolicyDetail
policyDetail =
  PolicyDetail' {_pdPolicyDocument = Nothing, _pdPolicyName = Nothing}


-- | The policy document.
pdPolicyDocument :: Lens' PolicyDetail (Maybe Text)
pdPolicyDocument = lens _pdPolicyDocument (\ s a -> s{_pdPolicyDocument = a})

-- | The name of the policy.
pdPolicyName :: Lens' PolicyDetail (Maybe Text)
pdPolicyName = lens _pdPolicyName (\ s a -> s{_pdPolicyName = a})

instance FromXML PolicyDetail where
        parseXML x
          = PolicyDetail' <$>
              (x .@? "PolicyDocument") <*> (x .@? "PolicyName")

instance Hashable PolicyDetail where

instance NFData PolicyDetail where

-- | Contains information about a group that a managed policy is attached to.
--
--
-- This data type is used as a response element in the 'ListEntitiesForPolicy' operation.
--
-- For more information about managed policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /Using IAM/ guide.
--
--
-- /See:/ 'policyGroup' smart constructor.
data PolicyGroup = PolicyGroup'
  { _pgGroupId   :: !(Maybe Text)
  , _pgGroupName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PolicyGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgGroupId' - The stable and unique string identifying the group. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- * 'pgGroupName' - The name (friendly name, not ARN) identifying the group.
policyGroup
    :: PolicyGroup
policyGroup = PolicyGroup' {_pgGroupId = Nothing, _pgGroupName = Nothing}


-- | The stable and unique string identifying the group. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
pgGroupId :: Lens' PolicyGroup (Maybe Text)
pgGroupId = lens _pgGroupId (\ s a -> s{_pgGroupId = a})

-- | The name (friendly name, not ARN) identifying the group.
pgGroupName :: Lens' PolicyGroup (Maybe Text)
pgGroupName = lens _pgGroupName (\ s a -> s{_pgGroupName = a})

instance FromXML PolicyGroup where
        parseXML x
          = PolicyGroup' <$>
              (x .@? "GroupId") <*> (x .@? "GroupName")

instance Hashable PolicyGroup where

instance NFData PolicyGroup where

-- | Contains information about a role that a managed policy is attached to.
--
--
-- This data type is used as a response element in the 'ListEntitiesForPolicy' operation.
--
-- For more information about managed policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /Using IAM/ guide.
--
--
-- /See:/ 'policyRole' smart constructor.
data PolicyRole = PolicyRole'
  { _prRoleName :: !(Maybe Text)
  , _prRoleId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PolicyRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prRoleName' - The name (friendly name, not ARN) identifying the role.
--
-- * 'prRoleId' - The stable and unique string identifying the role. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
policyRole
    :: PolicyRole
policyRole = PolicyRole' {_prRoleName = Nothing, _prRoleId = Nothing}


-- | The name (friendly name, not ARN) identifying the role.
prRoleName :: Lens' PolicyRole (Maybe Text)
prRoleName = lens _prRoleName (\ s a -> s{_prRoleName = a})

-- | The stable and unique string identifying the role. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
prRoleId :: Lens' PolicyRole (Maybe Text)
prRoleId = lens _prRoleId (\ s a -> s{_prRoleId = a})

instance FromXML PolicyRole where
        parseXML x
          = PolicyRole' <$>
              (x .@? "RoleName") <*> (x .@? "RoleId")

instance Hashable PolicyRole where

instance NFData PolicyRole where

-- | Contains information about a user that a managed policy is attached to.
--
--
-- This data type is used as a response element in the 'ListEntitiesForPolicy' operation.
--
-- For more information about managed policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /Using IAM/ guide.
--
--
-- /See:/ 'policyUser' smart constructor.
data PolicyUser = PolicyUser'
  { _puUserName :: !(Maybe Text)
  , _puUserId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PolicyUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'puUserName' - The name (friendly name, not ARN) identifying the user.
--
-- * 'puUserId' - The stable and unique string identifying the user. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
policyUser
    :: PolicyUser
policyUser = PolicyUser' {_puUserName = Nothing, _puUserId = Nothing}


-- | The name (friendly name, not ARN) identifying the user.
puUserName :: Lens' PolicyUser (Maybe Text)
puUserName = lens _puUserName (\ s a -> s{_puUserName = a})

-- | The stable and unique string identifying the user. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
puUserId :: Lens' PolicyUser (Maybe Text)
puUserId = lens _puUserId (\ s a -> s{_puUserId = a})

instance FromXML PolicyUser where
        parseXML x
          = PolicyUser' <$>
              (x .@? "UserName") <*> (x .@? "UserId")

instance Hashable PolicyUser where

instance NFData PolicyUser where

-- | Contains information about a version of a managed policy.
--
--
-- This data type is used as a response element in the 'CreatePolicyVersion' , 'GetPolicyVersion' , 'ListPolicyVersions' , and 'GetAccountAuthorizationDetails' operations.
--
-- For more information about managed policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /Using IAM/ guide.
--
--
-- /See:/ 'policyVersion' smart constructor.
data PolicyVersion = PolicyVersion'
  { _pvVersionId        :: !(Maybe Text)
  , _pvCreateDate       :: !(Maybe ISO8601)
  , _pvDocument         :: !(Maybe Text)
  , _pvIsDefaultVersion :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PolicyVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvVersionId' - The identifier for the policy version. Policy version identifiers always begin with @v@ (always lowercase). When a policy is created, the first policy version is @v1@ .
--
-- * 'pvCreateDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy version was created.
--
-- * 'pvDocument' - The policy document. The policy document is returned in the response to the 'GetPolicyVersion' and 'GetAccountAuthorizationDetails' operations. It is not returned in the response to the 'CreatePolicyVersion' or 'ListPolicyVersions' operations.  The policy document returned in this structure is URL-encoded compliant with <https://tools.ietf.org/html/rfc3986 RFC 3986> . You can use a URL decoding method to convert the policy back to plain JSON text. For example, if you use Java, you can use the @decode@ method of the @java.net.URLDecoder@ utility class in the Java SDK. Other languages and SDKs provide similar functionality.
--
-- * 'pvIsDefaultVersion' - Specifies whether the policy version is set as the policy's default version.
policyVersion
    :: PolicyVersion
policyVersion =
  PolicyVersion'
    { _pvVersionId = Nothing
    , _pvCreateDate = Nothing
    , _pvDocument = Nothing
    , _pvIsDefaultVersion = Nothing
    }


-- | The identifier for the policy version. Policy version identifiers always begin with @v@ (always lowercase). When a policy is created, the first policy version is @v1@ .
pvVersionId :: Lens' PolicyVersion (Maybe Text)
pvVersionId = lens _pvVersionId (\ s a -> s{_pvVersionId = a})

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy version was created.
pvCreateDate :: Lens' PolicyVersion (Maybe UTCTime)
pvCreateDate = lens _pvCreateDate (\ s a -> s{_pvCreateDate = a}) . mapping _Time

-- | The policy document. The policy document is returned in the response to the 'GetPolicyVersion' and 'GetAccountAuthorizationDetails' operations. It is not returned in the response to the 'CreatePolicyVersion' or 'ListPolicyVersions' operations.  The policy document returned in this structure is URL-encoded compliant with <https://tools.ietf.org/html/rfc3986 RFC 3986> . You can use a URL decoding method to convert the policy back to plain JSON text. For example, if you use Java, you can use the @decode@ method of the @java.net.URLDecoder@ utility class in the Java SDK. Other languages and SDKs provide similar functionality.
pvDocument :: Lens' PolicyVersion (Maybe Text)
pvDocument = lens _pvDocument (\ s a -> s{_pvDocument = a})

-- | Specifies whether the policy version is set as the policy's default version.
pvIsDefaultVersion :: Lens' PolicyVersion (Maybe Bool)
pvIsDefaultVersion = lens _pvIsDefaultVersion (\ s a -> s{_pvIsDefaultVersion = a})

instance FromXML PolicyVersion where
        parseXML x
          = PolicyVersion' <$>
              (x .@? "VersionId") <*> (x .@? "CreateDate") <*>
                (x .@? "Document")
                <*> (x .@? "IsDefaultVersion")

instance Hashable PolicyVersion where

instance NFData PolicyVersion where

-- | Contains the row and column of a location of a @Statement@ element in a policy document.
--
--
-- This data type is used as a member of the @'Statement' @ type.
--
--
-- /See:/ 'position' smart constructor.
data Position = Position'
  { _pLine   :: !(Maybe Int)
  , _pColumn :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Position' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pLine' - The line containing the specified position in the document.
--
-- * 'pColumn' - The column in the line containing the specified position in the document.
position
    :: Position
position = Position' {_pLine = Nothing, _pColumn = Nothing}


-- | The line containing the specified position in the document.
pLine :: Lens' Position (Maybe Int)
pLine = lens _pLine (\ s a -> s{_pLine = a})

-- | The column in the line containing the specified position in the document.
pColumn :: Lens' Position (Maybe Int)
pColumn = lens _pColumn (\ s a -> s{_pColumn = a})

instance FromXML Position where
        parseXML x
          = Position' <$> (x .@? "Line") <*> (x .@? "Column")

instance Hashable Position where

instance NFData Position where

-- | Contains the result of the simulation of a single API operation call on a single resource.
--
--
-- This data type is used by a member of the 'EvaluationResult' data type.
--
--
-- /See:/ 'resourceSpecificResult' smart constructor.
data ResourceSpecificResult = ResourceSpecificResult'
  { _rsrMatchedStatements    :: !(Maybe [Statement])
  , _rsrEvalDecisionDetails  :: !(Maybe (Map Text PolicyEvaluationDecisionType))
  , _rsrMissingContextValues :: !(Maybe [Text])
  , _rsrEvalResourceName     :: !Text
  , _rsrEvalResourceDecision :: !PolicyEvaluationDecisionType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceSpecificResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsrMatchedStatements' - A list of the statements in the input policies that determine the result for this part of the simulation. Remember that even if multiple statements allow the operation on the resource, if /any/ statement denies that operation, then the explicit deny overrides any allow, and the deny statement is the only entry included in the result.
--
-- * 'rsrEvalDecisionDetails' - Additional details about the results of the evaluation decision. When there are both IAM policies and resource policies, this parameter explains how each set of policies contributes to the final evaluation decision. When simulating cross-account access to a resource, both the resource-based policy and the caller's IAM policy must grant access.
--
-- * 'rsrMissingContextValues' - A list of context keys that are required by the included input policies but that were not provided by one of the input parameters. This list is used when a list of ARNs is included in the @ResourceArns@ parameter instead of "*". If you do not specify individual resources, by setting @ResourceArns@ to "*" or by not including the @ResourceArns@ parameter, then any missing context values are instead included under the @EvaluationResults@ section. To discover the context keys used by a set of policies, you can call 'GetContextKeysForCustomPolicy' or 'GetContextKeysForPrincipalPolicy' .
--
-- * 'rsrEvalResourceName' - The name of the simulated resource, in Amazon Resource Name (ARN) format.
--
-- * 'rsrEvalResourceDecision' - The result of the simulation of the simulated API operation on the resource specified in @EvalResourceName@ .
resourceSpecificResult
    :: Text -- ^ 'rsrEvalResourceName'
    -> PolicyEvaluationDecisionType -- ^ 'rsrEvalResourceDecision'
    -> ResourceSpecificResult
resourceSpecificResult pEvalResourceName_ pEvalResourceDecision_ =
  ResourceSpecificResult'
    { _rsrMatchedStatements = Nothing
    , _rsrEvalDecisionDetails = Nothing
    , _rsrMissingContextValues = Nothing
    , _rsrEvalResourceName = pEvalResourceName_
    , _rsrEvalResourceDecision = pEvalResourceDecision_
    }


-- | A list of the statements in the input policies that determine the result for this part of the simulation. Remember that even if multiple statements allow the operation on the resource, if /any/ statement denies that operation, then the explicit deny overrides any allow, and the deny statement is the only entry included in the result.
rsrMatchedStatements :: Lens' ResourceSpecificResult [Statement]
rsrMatchedStatements = lens _rsrMatchedStatements (\ s a -> s{_rsrMatchedStatements = a}) . _Default . _Coerce

-- | Additional details about the results of the evaluation decision. When there are both IAM policies and resource policies, this parameter explains how each set of policies contributes to the final evaluation decision. When simulating cross-account access to a resource, both the resource-based policy and the caller's IAM policy must grant access.
rsrEvalDecisionDetails :: Lens' ResourceSpecificResult (HashMap Text PolicyEvaluationDecisionType)
rsrEvalDecisionDetails = lens _rsrEvalDecisionDetails (\ s a -> s{_rsrEvalDecisionDetails = a}) . _Default . _Map

-- | A list of context keys that are required by the included input policies but that were not provided by one of the input parameters. This list is used when a list of ARNs is included in the @ResourceArns@ parameter instead of "*". If you do not specify individual resources, by setting @ResourceArns@ to "*" or by not including the @ResourceArns@ parameter, then any missing context values are instead included under the @EvaluationResults@ section. To discover the context keys used by a set of policies, you can call 'GetContextKeysForCustomPolicy' or 'GetContextKeysForPrincipalPolicy' .
rsrMissingContextValues :: Lens' ResourceSpecificResult [Text]
rsrMissingContextValues = lens _rsrMissingContextValues (\ s a -> s{_rsrMissingContextValues = a}) . _Default . _Coerce

-- | The name of the simulated resource, in Amazon Resource Name (ARN) format.
rsrEvalResourceName :: Lens' ResourceSpecificResult Text
rsrEvalResourceName = lens _rsrEvalResourceName (\ s a -> s{_rsrEvalResourceName = a})

-- | The result of the simulation of the simulated API operation on the resource specified in @EvalResourceName@ .
rsrEvalResourceDecision :: Lens' ResourceSpecificResult PolicyEvaluationDecisionType
rsrEvalResourceDecision = lens _rsrEvalResourceDecision (\ s a -> s{_rsrEvalResourceDecision = a})

instance FromXML ResourceSpecificResult where
        parseXML x
          = ResourceSpecificResult' <$>
              (x .@? "MatchedStatements" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*>
                (x .@? "EvalDecisionDetails" .!@ mempty >>=
                   may (parseXMLMap "entry" "key" "value"))
                <*>
                (x .@? "MissingContextValues" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@ "EvalResourceName")
                <*> (x .@ "EvalResourceDecision")

instance Hashable ResourceSpecificResult where

instance NFData ResourceSpecificResult where

-- | Contains information about an IAM role. This structure is returned as a response element in several API operations that interact with roles.
--
--
--
-- /See:/ 'role'' smart constructor.
data Role = Role'
  { _rMaxSessionDuration       :: !(Maybe Nat)
  , _rAssumeRolePolicyDocument :: !(Maybe Text)
  , _rDescription              :: !(Maybe Text)
  , _rPath                     :: !Text
  , _rRoleName                 :: !Text
  , _rRoleId                   :: !Text
  , _rARN                      :: !Text
  , _rCreateDate               :: !ISO8601
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Role' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rMaxSessionDuration' - The maximum session duration (in seconds) for the specified role. Anyone who uses the AWS CLI or API to assume the role can specify the duration using the optional @DurationSeconds@ API parameter or @duration-seconds@ CLI parameter.
--
-- * 'rAssumeRolePolicyDocument' - The policy that grants an entity permission to assume the role.
--
-- * 'rDescription' - A description of the role that you provide.
--
-- * 'rPath' - The path to the role. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- * 'rRoleName' - The friendly name that identifies the role.
--
-- * 'rRoleId' - The stable and unique string identifying the role. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- * 'rARN' - The Amazon Resource Name (ARN) specifying the role. For more information about ARNs and how to use them in policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ guide.
--
-- * 'rCreateDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the role was created.
role'
    :: Text -- ^ 'rPath'
    -> Text -- ^ 'rRoleName'
    -> Text -- ^ 'rRoleId'
    -> Text -- ^ 'rARN'
    -> UTCTime -- ^ 'rCreateDate'
    -> Role
role' pPath_ pRoleName_ pRoleId_ pARN_ pCreateDate_ =
  Role'
    { _rMaxSessionDuration = Nothing
    , _rAssumeRolePolicyDocument = Nothing
    , _rDescription = Nothing
    , _rPath = pPath_
    , _rRoleName = pRoleName_
    , _rRoleId = pRoleId_
    , _rARN = pARN_
    , _rCreateDate = _Time # pCreateDate_
    }


-- | The maximum session duration (in seconds) for the specified role. Anyone who uses the AWS CLI or API to assume the role can specify the duration using the optional @DurationSeconds@ API parameter or @duration-seconds@ CLI parameter.
rMaxSessionDuration :: Lens' Role (Maybe Natural)
rMaxSessionDuration = lens _rMaxSessionDuration (\ s a -> s{_rMaxSessionDuration = a}) . mapping _Nat

-- | The policy that grants an entity permission to assume the role.
rAssumeRolePolicyDocument :: Lens' Role (Maybe Text)
rAssumeRolePolicyDocument = lens _rAssumeRolePolicyDocument (\ s a -> s{_rAssumeRolePolicyDocument = a})

-- | A description of the role that you provide.
rDescription :: Lens' Role (Maybe Text)
rDescription = lens _rDescription (\ s a -> s{_rDescription = a})

-- | The path to the role. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
rPath :: Lens' Role Text
rPath = lens _rPath (\ s a -> s{_rPath = a})

-- | The friendly name that identifies the role.
rRoleName :: Lens' Role Text
rRoleName = lens _rRoleName (\ s a -> s{_rRoleName = a})

-- | The stable and unique string identifying the role. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
rRoleId :: Lens' Role Text
rRoleId = lens _rRoleId (\ s a -> s{_rRoleId = a})

-- | The Amazon Resource Name (ARN) specifying the role. For more information about ARNs and how to use them in policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ guide.
rARN :: Lens' Role Text
rARN = lens _rARN (\ s a -> s{_rARN = a})

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the role was created.
rCreateDate :: Lens' Role UTCTime
rCreateDate = lens _rCreateDate (\ s a -> s{_rCreateDate = a}) . _Time

instance FromXML Role where
        parseXML x
          = Role' <$>
              (x .@? "MaxSessionDuration") <*>
                (x .@? "AssumeRolePolicyDocument")
                <*> (x .@? "Description")
                <*> (x .@ "Path")
                <*> (x .@ "RoleName")
                <*> (x .@ "RoleId")
                <*> (x .@ "Arn")
                <*> (x .@ "CreateDate")

instance Hashable Role where

instance NFData Role where

-- | Contains information about an IAM role, including all of the role's policies.
--
--
-- This data type is used as a response element in the 'GetAccountAuthorizationDetails' operation.
--
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RoleDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdAssumeRolePolicyDocument' - The trust policy that grants permission to assume the role.
--
-- * 'rdARN' - Undocumented member.
--
-- * 'rdPath' - The path to the role. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- * 'rdInstanceProfileList' - A list of instance profiles that contain this role.
--
-- * 'rdCreateDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the role was created.
--
-- * 'rdRoleName' - The friendly name that identifies the role.
--
-- * 'rdRoleId' - The stable and unique string identifying the role. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- * 'rdRolePolicyList' - A list of inline policies embedded in the role. These policies are the role's access (permissions) policies.
--
-- * 'rdAttachedManagedPolicies' - A list of managed policies attached to the role. These policies are the role's access (permissions) policies.
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
rdAssumeRolePolicyDocument = lens _rdAssumeRolePolicyDocument (\ s a -> s{_rdAssumeRolePolicyDocument = a})

-- | Undocumented member.
rdARN :: Lens' RoleDetail (Maybe Text)
rdARN = lens _rdARN (\ s a -> s{_rdARN = a})

-- | The path to the role. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
rdPath :: Lens' RoleDetail (Maybe Text)
rdPath = lens _rdPath (\ s a -> s{_rdPath = a})

-- | A list of instance profiles that contain this role.
rdInstanceProfileList :: Lens' RoleDetail [InstanceProfile]
rdInstanceProfileList = lens _rdInstanceProfileList (\ s a -> s{_rdInstanceProfileList = a}) . _Default . _Coerce

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the role was created.
rdCreateDate :: Lens' RoleDetail (Maybe UTCTime)
rdCreateDate = lens _rdCreateDate (\ s a -> s{_rdCreateDate = a}) . mapping _Time

-- | The friendly name that identifies the role.
rdRoleName :: Lens' RoleDetail (Maybe Text)
rdRoleName = lens _rdRoleName (\ s a -> s{_rdRoleName = a})

-- | The stable and unique string identifying the role. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
rdRoleId :: Lens' RoleDetail (Maybe Text)
rdRoleId = lens _rdRoleId (\ s a -> s{_rdRoleId = a})

-- | A list of inline policies embedded in the role. These policies are the role's access (permissions) policies.
rdRolePolicyList :: Lens' RoleDetail [PolicyDetail]
rdRolePolicyList = lens _rdRolePolicyList (\ s a -> s{_rdRolePolicyList = a}) . _Default . _Coerce

-- | A list of managed policies attached to the role. These policies are the role's access (permissions) policies.
rdAttachedManagedPolicies :: Lens' RoleDetail [AttachedPolicy]
rdAttachedManagedPolicies = lens _rdAttachedManagedPolicies (\ s a -> s{_rdAttachedManagedPolicies = a}) . _Default . _Coerce

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

instance Hashable RoleDetail where

instance NFData RoleDetail where

-- | An object that contains details about how a service-linked role is used, if that information is returned by the service.
--
--
-- This data type is used as a response element in the 'GetServiceLinkedRoleDeletionStatus' operation.
--
--
-- /See:/ 'roleUsageType' smart constructor.
data RoleUsageType = RoleUsageType'
  { _rutResources :: !(Maybe [Text])
  , _rutRegion    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RoleUsageType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rutResources' - The name of the resource that is using the service-linked role.
--
-- * 'rutRegion' - The name of the region where the service-linked role is being used.
roleUsageType
    :: RoleUsageType
roleUsageType = RoleUsageType' {_rutResources = Nothing, _rutRegion = Nothing}


-- | The name of the resource that is using the service-linked role.
rutResources :: Lens' RoleUsageType [Text]
rutResources = lens _rutResources (\ s a -> s{_rutResources = a}) . _Default . _Coerce

-- | The name of the region where the service-linked role is being used.
rutRegion :: Lens' RoleUsageType (Maybe Text)
rutRegion = lens _rutRegion (\ s a -> s{_rutRegion = a})

instance FromXML RoleUsageType where
        parseXML x
          = RoleUsageType' <$>
              (x .@? "Resources" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@? "Region")

instance Hashable RoleUsageType where

instance NFData RoleUsageType where

-- | Contains the list of SAML providers for this account.
--
--
--
-- /See:/ 'sAMLProviderListEntry' smart constructor.
data SAMLProviderListEntry = SAMLProviderListEntry'
  { _samlpleARN        :: !(Maybe Text)
  , _samlpleCreateDate :: !(Maybe ISO8601)
  , _samlpleValidUntil :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SAMLProviderListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'samlpleARN' - The Amazon Resource Name (ARN) of the SAML provider.
--
-- * 'samlpleCreateDate' - The date and time when the SAML provider was created.
--
-- * 'samlpleValidUntil' - The expiration date and time for the SAML provider.
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
samlpleARN = lens _samlpleARN (\ s a -> s{_samlpleARN = a})

-- | The date and time when the SAML provider was created.
samlpleCreateDate :: Lens' SAMLProviderListEntry (Maybe UTCTime)
samlpleCreateDate = lens _samlpleCreateDate (\ s a -> s{_samlpleCreateDate = a}) . mapping _Time

-- | The expiration date and time for the SAML provider.
samlpleValidUntil :: Lens' SAMLProviderListEntry (Maybe UTCTime)
samlpleValidUntil = lens _samlpleValidUntil (\ s a -> s{_samlpleValidUntil = a}) . mapping _Time

instance FromXML SAMLProviderListEntry where
        parseXML x
          = SAMLProviderListEntry' <$>
              (x .@? "Arn") <*> (x .@? "CreateDate") <*>
                (x .@? "ValidUntil")

instance Hashable SAMLProviderListEntry where

instance NFData SAMLProviderListEntry where

-- | Contains information about an SSH public key.
--
--
-- This data type is used as a response element in the 'GetSSHPublicKey' and 'UploadSSHPublicKey' operations.
--
--
-- /See:/ 'sshPublicKey' smart constructor.
data SSHPublicKey = SSHPublicKey'
  { _spkUploadDate       :: !(Maybe ISO8601)
  , _spkUserName         :: !Text
  , _spkSSHPublicKeyId   :: !Text
  , _spkFingerprint      :: !Text
  , _spkSSHPublicKeyBody :: !Text
  , _spkStatus           :: !StatusType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SSHPublicKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spkUploadDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the SSH public key was uploaded.
--
-- * 'spkUserName' - The name of the IAM user associated with the SSH public key.
--
-- * 'spkSSHPublicKeyId' - The unique identifier for the SSH public key.
--
-- * 'spkFingerprint' - The MD5 message digest of the SSH public key.
--
-- * 'spkSSHPublicKeyBody' - The SSH public key.
--
-- * 'spkStatus' - The status of the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
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


-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the SSH public key was uploaded.
spkUploadDate :: Lens' SSHPublicKey (Maybe UTCTime)
spkUploadDate = lens _spkUploadDate (\ s a -> s{_spkUploadDate = a}) . mapping _Time

-- | The name of the IAM user associated with the SSH public key.
spkUserName :: Lens' SSHPublicKey Text
spkUserName = lens _spkUserName (\ s a -> s{_spkUserName = a})

-- | The unique identifier for the SSH public key.
spkSSHPublicKeyId :: Lens' SSHPublicKey Text
spkSSHPublicKeyId = lens _spkSSHPublicKeyId (\ s a -> s{_spkSSHPublicKeyId = a})

-- | The MD5 message digest of the SSH public key.
spkFingerprint :: Lens' SSHPublicKey Text
spkFingerprint = lens _spkFingerprint (\ s a -> s{_spkFingerprint = a})

-- | The SSH public key.
spkSSHPublicKeyBody :: Lens' SSHPublicKey Text
spkSSHPublicKeyBody = lens _spkSSHPublicKeyBody (\ s a -> s{_spkSSHPublicKeyBody = a})

-- | The status of the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
spkStatus :: Lens' SSHPublicKey StatusType
spkStatus = lens _spkStatus (\ s a -> s{_spkStatus = a})

instance FromXML SSHPublicKey where
        parseXML x
          = SSHPublicKey' <$>
              (x .@? "UploadDate") <*> (x .@ "UserName") <*>
                (x .@ "SSHPublicKeyId")
                <*> (x .@ "Fingerprint")
                <*> (x .@ "SSHPublicKeyBody")
                <*> (x .@ "Status")

instance Hashable SSHPublicKey where

instance NFData SSHPublicKey where

-- | Contains information about an SSH public key, without the key's body or fingerprint.
--
--
-- This data type is used as a response element in the 'ListSSHPublicKeys' operation.
--
--
-- /See:/ 'sshPublicKeyMetadata' smart constructor.
data SSHPublicKeyMetadata = SSHPublicKeyMetadata'
  { _spkmUserName       :: !Text
  , _spkmSSHPublicKeyId :: !Text
  , _spkmStatus         :: !StatusType
  , _spkmUploadDate     :: !ISO8601
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SSHPublicKeyMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spkmUserName' - The name of the IAM user associated with the SSH public key.
--
-- * 'spkmSSHPublicKeyId' - The unique identifier for the SSH public key.
--
-- * 'spkmStatus' - The status of the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
--
-- * 'spkmUploadDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the SSH public key was uploaded.
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
spkmUserName = lens _spkmUserName (\ s a -> s{_spkmUserName = a})

-- | The unique identifier for the SSH public key.
spkmSSHPublicKeyId :: Lens' SSHPublicKeyMetadata Text
spkmSSHPublicKeyId = lens _spkmSSHPublicKeyId (\ s a -> s{_spkmSSHPublicKeyId = a})

-- | The status of the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
spkmStatus :: Lens' SSHPublicKeyMetadata StatusType
spkmStatus = lens _spkmStatus (\ s a -> s{_spkmStatus = a})

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the SSH public key was uploaded.
spkmUploadDate :: Lens' SSHPublicKeyMetadata UTCTime
spkmUploadDate = lens _spkmUploadDate (\ s a -> s{_spkmUploadDate = a}) . _Time

instance FromXML SSHPublicKeyMetadata where
        parseXML x
          = SSHPublicKeyMetadata' <$>
              (x .@ "UserName") <*> (x .@ "SSHPublicKeyId") <*>
                (x .@ "Status")
                <*> (x .@ "UploadDate")

instance Hashable SSHPublicKeyMetadata where

instance NFData SSHPublicKeyMetadata where

-- | Contains information about a server certificate.
--
--
-- This data type is used as a response element in the 'GetServerCertificate' operation.
--
--
-- /See:/ 'serverCertificate' smart constructor.
data ServerCertificate = ServerCertificate'
  { _sCertificateChain          :: !(Maybe Text)
  , _sServerCertificateMetadata :: !ServerCertificateMetadata
  , _sCertificateBody           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServerCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sCertificateChain' - The contents of the public key certificate chain.
--
-- * 'sServerCertificateMetadata' - The meta information of the server certificate, such as its name, path, ID, and ARN.
--
-- * 'sCertificateBody' - The contents of the public key certificate.
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
sCertificateChain = lens _sCertificateChain (\ s a -> s{_sCertificateChain = a})

-- | The meta information of the server certificate, such as its name, path, ID, and ARN.
sServerCertificateMetadata :: Lens' ServerCertificate ServerCertificateMetadata
sServerCertificateMetadata = lens _sServerCertificateMetadata (\ s a -> s{_sServerCertificateMetadata = a})

-- | The contents of the public key certificate.
sCertificateBody :: Lens' ServerCertificate Text
sCertificateBody = lens _sCertificateBody (\ s a -> s{_sCertificateBody = a})

instance FromXML ServerCertificate where
        parseXML x
          = ServerCertificate' <$>
              (x .@? "CertificateChain") <*>
                (x .@ "ServerCertificateMetadata")
                <*> (x .@ "CertificateBody")

instance Hashable ServerCertificate where

instance NFData ServerCertificate where

-- | Contains information about a server certificate without its certificate body, certificate chain, and private key.
--
--
-- This data type is used as a response element in the 'UploadServerCertificate' and 'ListServerCertificates' operations.
--
--
-- /See:/ 'serverCertificateMetadata' smart constructor.
data ServerCertificateMetadata = ServerCertificateMetadata'
  { _scmUploadDate            :: !(Maybe ISO8601)
  , _scmExpiration            :: !(Maybe ISO8601)
  , _scmPath                  :: !Text
  , _scmServerCertificateName :: !Text
  , _scmServerCertificateId   :: !Text
  , _scmARN                   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServerCertificateMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scmUploadDate' - The date when the server certificate was uploaded.
--
-- * 'scmExpiration' - The date on which the certificate is set to expire.
--
-- * 'scmPath' - The path to the server certificate. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- * 'scmServerCertificateName' - The name that identifies the server certificate.
--
-- * 'scmServerCertificateId' - The stable and unique string identifying the server certificate. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- * 'scmARN' - The Amazon Resource Name (ARN) specifying the server certificate. For more information about ARNs and how to use them in policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
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
scmUploadDate = lens _scmUploadDate (\ s a -> s{_scmUploadDate = a}) . mapping _Time

-- | The date on which the certificate is set to expire.
scmExpiration :: Lens' ServerCertificateMetadata (Maybe UTCTime)
scmExpiration = lens _scmExpiration (\ s a -> s{_scmExpiration = a}) . mapping _Time

-- | The path to the server certificate. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
scmPath :: Lens' ServerCertificateMetadata Text
scmPath = lens _scmPath (\ s a -> s{_scmPath = a})

-- | The name that identifies the server certificate.
scmServerCertificateName :: Lens' ServerCertificateMetadata Text
scmServerCertificateName = lens _scmServerCertificateName (\ s a -> s{_scmServerCertificateName = a})

-- | The stable and unique string identifying the server certificate. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
scmServerCertificateId :: Lens' ServerCertificateMetadata Text
scmServerCertificateId = lens _scmServerCertificateId (\ s a -> s{_scmServerCertificateId = a})

-- | The Amazon Resource Name (ARN) specifying the server certificate. For more information about ARNs and how to use them in policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
scmARN :: Lens' ServerCertificateMetadata Text
scmARN = lens _scmARN (\ s a -> s{_scmARN = a})

instance FromXML ServerCertificateMetadata where
        parseXML x
          = ServerCertificateMetadata' <$>
              (x .@? "UploadDate") <*> (x .@? "Expiration") <*>
                (x .@ "Path")
                <*> (x .@ "ServerCertificateName")
                <*> (x .@ "ServerCertificateId")
                <*> (x .@ "Arn")

instance Hashable ServerCertificateMetadata where

instance NFData ServerCertificateMetadata where

-- | Contains the details of a service-specific credential.
--
--
--
-- /See:/ 'serviceSpecificCredential' smart constructor.
data ServiceSpecificCredential = ServiceSpecificCredential'
  { _sscCreateDate                  :: !ISO8601
  , _sscServiceName                 :: !Text
  , _sscServiceUserName             :: !Text
  , _sscServicePassword             :: !(Sensitive Text)
  , _sscServiceSpecificCredentialId :: !Text
  , _sscUserName                    :: !Text
  , _sscStatus                      :: !StatusType
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServiceSpecificCredential' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sscCreateDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the service-specific credential were created.
--
-- * 'sscServiceName' - The name of the service associated with the service-specific credential.
--
-- * 'sscServiceUserName' - The generated user name for the service-specific credential. This value is generated by combining the IAM user's name combined with the ID number of the AWS account, as in @jane-at-123456789012@ , for example. This value cannot be configured by the user.
--
-- * 'sscServicePassword' - The generated password for the service-specific credential.
--
-- * 'sscServiceSpecificCredentialId' - The unique identifier for the service-specific credential.
--
-- * 'sscUserName' - The name of the IAM user associated with the service-specific credential.
--
-- * 'sscStatus' - The status of the service-specific credential. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not.
serviceSpecificCredential
    :: UTCTime -- ^ 'sscCreateDate'
    -> Text -- ^ 'sscServiceName'
    -> Text -- ^ 'sscServiceUserName'
    -> Text -- ^ 'sscServicePassword'
    -> Text -- ^ 'sscServiceSpecificCredentialId'
    -> Text -- ^ 'sscUserName'
    -> StatusType -- ^ 'sscStatus'
    -> ServiceSpecificCredential
serviceSpecificCredential pCreateDate_ pServiceName_ pServiceUserName_ pServicePassword_ pServiceSpecificCredentialId_ pUserName_ pStatus_ =
  ServiceSpecificCredential'
    { _sscCreateDate = _Time # pCreateDate_
    , _sscServiceName = pServiceName_
    , _sscServiceUserName = pServiceUserName_
    , _sscServicePassword = _Sensitive # pServicePassword_
    , _sscServiceSpecificCredentialId = pServiceSpecificCredentialId_
    , _sscUserName = pUserName_
    , _sscStatus = pStatus_
    }


-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the service-specific credential were created.
sscCreateDate :: Lens' ServiceSpecificCredential UTCTime
sscCreateDate = lens _sscCreateDate (\ s a -> s{_sscCreateDate = a}) . _Time

-- | The name of the service associated with the service-specific credential.
sscServiceName :: Lens' ServiceSpecificCredential Text
sscServiceName = lens _sscServiceName (\ s a -> s{_sscServiceName = a})

-- | The generated user name for the service-specific credential. This value is generated by combining the IAM user's name combined with the ID number of the AWS account, as in @jane-at-123456789012@ , for example. This value cannot be configured by the user.
sscServiceUserName :: Lens' ServiceSpecificCredential Text
sscServiceUserName = lens _sscServiceUserName (\ s a -> s{_sscServiceUserName = a})

-- | The generated password for the service-specific credential.
sscServicePassword :: Lens' ServiceSpecificCredential Text
sscServicePassword = lens _sscServicePassword (\ s a -> s{_sscServicePassword = a}) . _Sensitive

-- | The unique identifier for the service-specific credential.
sscServiceSpecificCredentialId :: Lens' ServiceSpecificCredential Text
sscServiceSpecificCredentialId = lens _sscServiceSpecificCredentialId (\ s a -> s{_sscServiceSpecificCredentialId = a})

-- | The name of the IAM user associated with the service-specific credential.
sscUserName :: Lens' ServiceSpecificCredential Text
sscUserName = lens _sscUserName (\ s a -> s{_sscUserName = a})

-- | The status of the service-specific credential. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not.
sscStatus :: Lens' ServiceSpecificCredential StatusType
sscStatus = lens _sscStatus (\ s a -> s{_sscStatus = a})

instance FromXML ServiceSpecificCredential where
        parseXML x
          = ServiceSpecificCredential' <$>
              (x .@ "CreateDate") <*> (x .@ "ServiceName") <*>
                (x .@ "ServiceUserName")
                <*> (x .@ "ServicePassword")
                <*> (x .@ "ServiceSpecificCredentialId")
                <*> (x .@ "UserName")
                <*> (x .@ "Status")

instance Hashable ServiceSpecificCredential where

instance NFData ServiceSpecificCredential where

-- | Contains additional details about a service-specific credential.
--
--
--
-- /See:/ 'serviceSpecificCredentialMetadata' smart constructor.
data ServiceSpecificCredentialMetadata = ServiceSpecificCredentialMetadata'
  { _sscmUserName                    :: !Text
  , _sscmStatus                      :: !StatusType
  , _sscmServiceUserName             :: !Text
  , _sscmCreateDate                  :: !ISO8601
  , _sscmServiceSpecificCredentialId :: !Text
  , _sscmServiceName                 :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServiceSpecificCredentialMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sscmUserName' - The name of the IAM user associated with the service-specific credential.
--
-- * 'sscmStatus' - The status of the service-specific credential. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not.
--
-- * 'sscmServiceUserName' - The generated user name for the service-specific credential.
--
-- * 'sscmCreateDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the service-specific credential were created.
--
-- * 'sscmServiceSpecificCredentialId' - The unique identifier for the service-specific credential.
--
-- * 'sscmServiceName' - The name of the service associated with the service-specific credential.
serviceSpecificCredentialMetadata
    :: Text -- ^ 'sscmUserName'
    -> StatusType -- ^ 'sscmStatus'
    -> Text -- ^ 'sscmServiceUserName'
    -> UTCTime -- ^ 'sscmCreateDate'
    -> Text -- ^ 'sscmServiceSpecificCredentialId'
    -> Text -- ^ 'sscmServiceName'
    -> ServiceSpecificCredentialMetadata
serviceSpecificCredentialMetadata pUserName_ pStatus_ pServiceUserName_ pCreateDate_ pServiceSpecificCredentialId_ pServiceName_ =
  ServiceSpecificCredentialMetadata'
    { _sscmUserName = pUserName_
    , _sscmStatus = pStatus_
    , _sscmServiceUserName = pServiceUserName_
    , _sscmCreateDate = _Time # pCreateDate_
    , _sscmServiceSpecificCredentialId = pServiceSpecificCredentialId_
    , _sscmServiceName = pServiceName_
    }


-- | The name of the IAM user associated with the service-specific credential.
sscmUserName :: Lens' ServiceSpecificCredentialMetadata Text
sscmUserName = lens _sscmUserName (\ s a -> s{_sscmUserName = a})

-- | The status of the service-specific credential. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not.
sscmStatus :: Lens' ServiceSpecificCredentialMetadata StatusType
sscmStatus = lens _sscmStatus (\ s a -> s{_sscmStatus = a})

-- | The generated user name for the service-specific credential.
sscmServiceUserName :: Lens' ServiceSpecificCredentialMetadata Text
sscmServiceUserName = lens _sscmServiceUserName (\ s a -> s{_sscmServiceUserName = a})

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the service-specific credential were created.
sscmCreateDate :: Lens' ServiceSpecificCredentialMetadata UTCTime
sscmCreateDate = lens _sscmCreateDate (\ s a -> s{_sscmCreateDate = a}) . _Time

-- | The unique identifier for the service-specific credential.
sscmServiceSpecificCredentialId :: Lens' ServiceSpecificCredentialMetadata Text
sscmServiceSpecificCredentialId = lens _sscmServiceSpecificCredentialId (\ s a -> s{_sscmServiceSpecificCredentialId = a})

-- | The name of the service associated with the service-specific credential.
sscmServiceName :: Lens' ServiceSpecificCredentialMetadata Text
sscmServiceName = lens _sscmServiceName (\ s a -> s{_sscmServiceName = a})

instance FromXML ServiceSpecificCredentialMetadata
         where
        parseXML x
          = ServiceSpecificCredentialMetadata' <$>
              (x .@ "UserName") <*> (x .@ "Status") <*>
                (x .@ "ServiceUserName")
                <*> (x .@ "CreateDate")
                <*> (x .@ "ServiceSpecificCredentialId")
                <*> (x .@ "ServiceName")

instance Hashable ServiceSpecificCredentialMetadata
         where

instance NFData ServiceSpecificCredentialMetadata
         where

-- | Contains information about an X.509 signing certificate.
--
--
-- This data type is used as a response element in the 'UploadSigningCertificate' and 'ListSigningCertificates' operations.
--
--
-- /See:/ 'signingCertificate' smart constructor.
data SigningCertificate = SigningCertificate'
  { _scUploadDate      :: !(Maybe ISO8601)
  , _scUserName        :: !Text
  , _scCertificateId   :: !Text
  , _scCertificateBody :: !Text
  , _scStatus          :: !StatusType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SigningCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scUploadDate' - The date when the signing certificate was uploaded.
--
-- * 'scUserName' - The name of the user the signing certificate is associated with.
--
-- * 'scCertificateId' - The ID for the signing certificate.
--
-- * 'scCertificateBody' - The contents of the signing certificate.
--
-- * 'scStatus' - The status of the signing certificate. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not.
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
scUploadDate = lens _scUploadDate (\ s a -> s{_scUploadDate = a}) . mapping _Time

-- | The name of the user the signing certificate is associated with.
scUserName :: Lens' SigningCertificate Text
scUserName = lens _scUserName (\ s a -> s{_scUserName = a})

-- | The ID for the signing certificate.
scCertificateId :: Lens' SigningCertificate Text
scCertificateId = lens _scCertificateId (\ s a -> s{_scCertificateId = a})

-- | The contents of the signing certificate.
scCertificateBody :: Lens' SigningCertificate Text
scCertificateBody = lens _scCertificateBody (\ s a -> s{_scCertificateBody = a})

-- | The status of the signing certificate. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not.
scStatus :: Lens' SigningCertificate StatusType
scStatus = lens _scStatus (\ s a -> s{_scStatus = a})

instance FromXML SigningCertificate where
        parseXML x
          = SigningCertificate' <$>
              (x .@? "UploadDate") <*> (x .@ "UserName") <*>
                (x .@ "CertificateId")
                <*> (x .@ "CertificateBody")
                <*> (x .@ "Status")

instance Hashable SigningCertificate where

instance NFData SigningCertificate where

-- | Contains the response to a successful 'SimulatePrincipalPolicy' or 'SimulateCustomPolicy' request.
--
--
--
-- /See:/ 'simulatePolicyResponse' smart constructor.
data SimulatePolicyResponse = SimulatePolicyResponse'
  { _spEvaluationResults :: !(Maybe [EvaluationResult])
  , _spMarker            :: !(Maybe Text)
  , _spIsTruncated       :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SimulatePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spEvaluationResults' - The results of the simulation.
--
-- * 'spMarker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- * 'spIsTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
simulatePolicyResponse
    :: SimulatePolicyResponse
simulatePolicyResponse =
  SimulatePolicyResponse'
    { _spEvaluationResults = Nothing
    , _spMarker = Nothing
    , _spIsTruncated = Nothing
    }


-- | The results of the simulation.
spEvaluationResults :: Lens' SimulatePolicyResponse [EvaluationResult]
spEvaluationResults = lens _spEvaluationResults (\ s a -> s{_spEvaluationResults = a}) . _Default . _Coerce

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
spMarker :: Lens' SimulatePolicyResponse (Maybe Text)
spMarker = lens _spMarker (\ s a -> s{_spMarker = a})

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
spIsTruncated :: Lens' SimulatePolicyResponse (Maybe Bool)
spIsTruncated = lens _spIsTruncated (\ s a -> s{_spIsTruncated = a})

instance FromXML SimulatePolicyResponse where
        parseXML x
          = SimulatePolicyResponse' <$>
              (x .@? "EvaluationResults" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@? "Marker")
                <*> (x .@? "IsTruncated")

instance Hashable SimulatePolicyResponse where

instance NFData SimulatePolicyResponse where

-- | Contains a reference to a @Statement@ element in a policy document that determines the result of the simulation.
--
--
-- This data type is used by the @MatchedStatements@ member of the @'EvaluationResult' @ type.
--
--
-- /See:/ 'statement' smart constructor.
data Statement = Statement'
  { _sSourcePolicyType :: !(Maybe PolicySourceType)
  , _sSourcePolicyId   :: !(Maybe Text)
  , _sEndPosition      :: !(Maybe Position)
  , _sStartPosition    :: !(Maybe Position)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Statement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSourcePolicyType' - The type of the policy.
--
-- * 'sSourcePolicyId' - The identifier of the policy that was provided as an input.
--
-- * 'sEndPosition' - The row and column of the end of a @Statement@ in an IAM policy.
--
-- * 'sStartPosition' - The row and column of the beginning of the @Statement@ in an IAM policy.
statement
    :: Statement
statement =
  Statement'
    { _sSourcePolicyType = Nothing
    , _sSourcePolicyId = Nothing
    , _sEndPosition = Nothing
    , _sStartPosition = Nothing
    }


-- | The type of the policy.
sSourcePolicyType :: Lens' Statement (Maybe PolicySourceType)
sSourcePolicyType = lens _sSourcePolicyType (\ s a -> s{_sSourcePolicyType = a})

-- | The identifier of the policy that was provided as an input.
sSourcePolicyId :: Lens' Statement (Maybe Text)
sSourcePolicyId = lens _sSourcePolicyId (\ s a -> s{_sSourcePolicyId = a})

-- | The row and column of the end of a @Statement@ in an IAM policy.
sEndPosition :: Lens' Statement (Maybe Position)
sEndPosition = lens _sEndPosition (\ s a -> s{_sEndPosition = a})

-- | The row and column of the beginning of the @Statement@ in an IAM policy.
sStartPosition :: Lens' Statement (Maybe Position)
sStartPosition = lens _sStartPosition (\ s a -> s{_sStartPosition = a})

instance FromXML Statement where
        parseXML x
          = Statement' <$>
              (x .@? "SourcePolicyType") <*>
                (x .@? "SourcePolicyId")
                <*> (x .@? "EndPosition")
                <*> (x .@? "StartPosition")

instance Hashable Statement where

instance NFData Statement where

-- | Contains information about an IAM user entity.
--
--
-- This data type is used as a response element in the following operations:
--
--     * 'CreateUser'
--
--     * 'GetUser'
--
--     * 'ListUsers'
--
--
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uPasswordLastUsed' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the user's password was last used to sign in to an AWS website. For a list of AWS websites that capture a user's last sign-in time, see the <http://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Credential Reports> topic in the /Using IAM/ guide. If a password is used more than once in a five-minute span, only the first use is returned in this field. If the field is null (no value) then it indicates that they never signed in with a password. This can be because:     * The user never had a password.     * A password exists but has not been used since IAM started tracking this information on October 20th, 2014. A null does not mean that the user /never/ had a password. Also, if the user does not currently have a password, but had one in the past, then this field contains the date and time the most recent password was used. This value is returned only in the 'GetUser' and 'ListUsers' operations.
--
-- * 'uPath' - The path to the user. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- * 'uUserName' - The friendly name identifying the user.
--
-- * 'uUserId' - The stable and unique string identifying the user. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- * 'uARN' - The Amazon Resource Name (ARN) that identifies the user. For more information about ARNs and how to use ARNs in policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- * 'uCreateDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the user was created.
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


-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the user's password was last used to sign in to an AWS website. For a list of AWS websites that capture a user's last sign-in time, see the <http://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Credential Reports> topic in the /Using IAM/ guide. If a password is used more than once in a five-minute span, only the first use is returned in this field. If the field is null (no value) then it indicates that they never signed in with a password. This can be because:     * The user never had a password.     * A password exists but has not been used since IAM started tracking this information on October 20th, 2014. A null does not mean that the user /never/ had a password. Also, if the user does not currently have a password, but had one in the past, then this field contains the date and time the most recent password was used. This value is returned only in the 'GetUser' and 'ListUsers' operations.
uPasswordLastUsed :: Lens' User (Maybe UTCTime)
uPasswordLastUsed = lens _uPasswordLastUsed (\ s a -> s{_uPasswordLastUsed = a}) . mapping _Time

-- | The path to the user. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
uPath :: Lens' User Text
uPath = lens _uPath (\ s a -> s{_uPath = a})

-- | The friendly name identifying the user.
uUserName :: Lens' User Text
uUserName = lens _uUserName (\ s a -> s{_uUserName = a})

-- | The stable and unique string identifying the user. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
uUserId :: Lens' User Text
uUserId = lens _uUserId (\ s a -> s{_uUserId = a})

-- | The Amazon Resource Name (ARN) that identifies the user. For more information about ARNs and how to use ARNs in policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
uARN :: Lens' User Text
uARN = lens _uARN (\ s a -> s{_uARN = a})

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the user was created.
uCreateDate :: Lens' User UTCTime
uCreateDate = lens _uCreateDate (\ s a -> s{_uCreateDate = a}) . _Time

instance FromXML User where
        parseXML x
          = User' <$>
              (x .@? "PasswordLastUsed") <*> (x .@ "Path") <*>
                (x .@ "UserName")
                <*> (x .@ "UserId")
                <*> (x .@ "Arn")
                <*> (x .@ "CreateDate")

instance Hashable User where

instance NFData User where

-- | Contains information about an IAM user, including all the user's policies and all the IAM groups the user is in.
--
--
-- This data type is used as a response element in the 'GetAccountAuthorizationDetails' operation.
--
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udGroupList' - A list of IAM groups that the user is in.
--
-- * 'udARN' - Undocumented member.
--
-- * 'udPath' - The path to the user. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- * 'udCreateDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the user was created.
--
-- * 'udUserName' - The friendly name identifying the user.
--
-- * 'udUserId' - The stable and unique string identifying the user. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- * 'udUserPolicyList' - A list of the inline policies embedded in the user.
--
-- * 'udAttachedManagedPolicies' - A list of the managed policies attached to the user.
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
udGroupList = lens _udGroupList (\ s a -> s{_udGroupList = a}) . _Default . _Coerce

-- | Undocumented member.
udARN :: Lens' UserDetail (Maybe Text)
udARN = lens _udARN (\ s a -> s{_udARN = a})

-- | The path to the user. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
udPath :: Lens' UserDetail (Maybe Text)
udPath = lens _udPath (\ s a -> s{_udPath = a})

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the user was created.
udCreateDate :: Lens' UserDetail (Maybe UTCTime)
udCreateDate = lens _udCreateDate (\ s a -> s{_udCreateDate = a}) . mapping _Time

-- | The friendly name identifying the user.
udUserName :: Lens' UserDetail (Maybe Text)
udUserName = lens _udUserName (\ s a -> s{_udUserName = a})

-- | The stable and unique string identifying the user. For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
udUserId :: Lens' UserDetail (Maybe Text)
udUserId = lens _udUserId (\ s a -> s{_udUserId = a})

-- | A list of the inline policies embedded in the user.
udUserPolicyList :: Lens' UserDetail [PolicyDetail]
udUserPolicyList = lens _udUserPolicyList (\ s a -> s{_udUserPolicyList = a}) . _Default . _Coerce

-- | A list of the managed policies attached to the user.
udAttachedManagedPolicies :: Lens' UserDetail [AttachedPolicy]
udAttachedManagedPolicies = lens _udAttachedManagedPolicies (\ s a -> s{_udAttachedManagedPolicies = a}) . _Default . _Coerce

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

instance Hashable UserDetail where

instance NFData UserDetail where

-- | Contains information about a virtual MFA device.
--
--
--
-- /See:/ 'virtualMFADevice' smart constructor.
data VirtualMFADevice = VirtualMFADevice'
  { _vmdQRCodePNG        :: !(Maybe (Sensitive Base64))
  , _vmdBase32StringSeed :: !(Maybe (Sensitive Base64))
  , _vmdUser             :: !(Maybe User)
  , _vmdEnableDate       :: !(Maybe ISO8601)
  , _vmdSerialNumber     :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'VirtualMFADevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vmdQRCodePNG' - A QR code PNG image that encodes @otpauth://totp/$virtualMFADeviceName@$AccountName?secret=$Base32String@ where @> virtualMFADeviceName@ is one of the create call arguments, @AccountName@ is the user name if set (otherwise, the account ID otherwise), and @Base32String@ is the seed in Base32 format. The @Base32String@ value is Base64-encoded. -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'vmdBase32StringSeed' - The Base32 seed defined as specified in <https://tools.ietf.org/html/rfc3548.txt RFC3548> . The @Base32StringSeed@ is Base64-encoded. -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'vmdUser' - The IAM user associated with this virtual MFA device.
--
-- * 'vmdEnableDate' - The date and time on which the virtual MFA device was enabled.
--
-- * 'vmdSerialNumber' - The serial number associated with @VirtualMFADevice@ .
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


-- | A QR code PNG image that encodes @otpauth://totp/$virtualMFADeviceName@$AccountName?secret=$Base32String@ where @> virtualMFADeviceName@ is one of the create call arguments, @AccountName@ is the user name if set (otherwise, the account ID otherwise), and @Base32String@ is the seed in Base32 format. The @Base32String@ value is Base64-encoded. -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
vmdQRCodePNG :: Lens' VirtualMFADevice (Maybe ByteString)
vmdQRCodePNG = lens _vmdQRCodePNG (\ s a -> s{_vmdQRCodePNG = a}) . mapping (_Sensitive . _Base64)

-- | The Base32 seed defined as specified in <https://tools.ietf.org/html/rfc3548.txt RFC3548> . The @Base32StringSeed@ is Base64-encoded. -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
vmdBase32StringSeed :: Lens' VirtualMFADevice (Maybe ByteString)
vmdBase32StringSeed = lens _vmdBase32StringSeed (\ s a -> s{_vmdBase32StringSeed = a}) . mapping (_Sensitive . _Base64)

-- | The IAM user associated with this virtual MFA device.
vmdUser :: Lens' VirtualMFADevice (Maybe User)
vmdUser = lens _vmdUser (\ s a -> s{_vmdUser = a})

-- | The date and time on which the virtual MFA device was enabled.
vmdEnableDate :: Lens' VirtualMFADevice (Maybe UTCTime)
vmdEnableDate = lens _vmdEnableDate (\ s a -> s{_vmdEnableDate = a}) . mapping _Time

-- | The serial number associated with @VirtualMFADevice@ .
vmdSerialNumber :: Lens' VirtualMFADevice Text
vmdSerialNumber = lens _vmdSerialNumber (\ s a -> s{_vmdSerialNumber = a})

instance FromXML VirtualMFADevice where
        parseXML x
          = VirtualMFADevice' <$>
              (x .@? "QRCodePNG") <*> (x .@? "Base32StringSeed")
                <*> (x .@? "User")
                <*> (x .@? "EnableDate")
                <*> (x .@ "SerialNumber")

instance Hashable VirtualMFADevice where

instance NFData VirtualMFADevice where
