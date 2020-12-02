{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.Product where

import Network.AWS.FMS.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details of the resource that is not protected by the policy.
--
--
--
-- /See:/ 'complianceViolator' smart constructor.
data ComplianceViolator = ComplianceViolator'
  { _cvResourceId      :: !(Maybe Text)
  , _cvResourceType    :: !(Maybe Text)
  , _cvViolationReason :: !(Maybe ViolationReason)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ComplianceViolator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvResourceId' - The resource ID.
--
-- * 'cvResourceType' - The resource type. This is in the format shown in <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . Valid values are @AWS::ElasticLoadBalancingV2::LoadBalancer@ or @AWS::CloudFront::Distribution@ .
--
-- * 'cvViolationReason' - The reason that the resource is not protected by the policy.
complianceViolator
    :: ComplianceViolator
complianceViolator =
  ComplianceViolator'
    { _cvResourceId = Nothing
    , _cvResourceType = Nothing
    , _cvViolationReason = Nothing
    }


-- | The resource ID.
cvResourceId :: Lens' ComplianceViolator (Maybe Text)
cvResourceId = lens _cvResourceId (\ s a -> s{_cvResourceId = a})

-- | The resource type. This is in the format shown in <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . Valid values are @AWS::ElasticLoadBalancingV2::LoadBalancer@ or @AWS::CloudFront::Distribution@ .
cvResourceType :: Lens' ComplianceViolator (Maybe Text)
cvResourceType = lens _cvResourceType (\ s a -> s{_cvResourceType = a})

-- | The reason that the resource is not protected by the policy.
cvViolationReason :: Lens' ComplianceViolator (Maybe ViolationReason)
cvViolationReason = lens _cvViolationReason (\ s a -> s{_cvViolationReason = a})

instance FromJSON ComplianceViolator where
        parseJSON
          = withObject "ComplianceViolator"
              (\ x ->
                 ComplianceViolator' <$>
                   (x .:? "ResourceId") <*> (x .:? "ResourceType") <*>
                     (x .:? "ViolationReason"))

instance Hashable ComplianceViolator where

instance NFData ComplianceViolator where

-- | Describes the compliance status for the account. An account is considered non-compliant if it includes resources that are not protected by the specified policy.
--
--
--
-- /See:/ 'evaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
  { _erViolatorCount           :: !(Maybe Nat)
  , _erComplianceStatus        :: !(Maybe PolicyComplianceStatusType)
  , _erEvaluationLimitExceeded :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EvaluationResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erViolatorCount' - Number of resources that are non-compliant with the specified policy. A resource is considered non-compliant if it is not associated with the specified policy.
--
-- * 'erComplianceStatus' - Describes an AWS account's compliance with the AWS Firewall Manager policy.
--
-- * 'erEvaluationLimitExceeded' - Indicates that over 100 resources are non-compliant with the AWS Firewall Manager policy.
evaluationResult
    :: EvaluationResult
evaluationResult =
  EvaluationResult'
    { _erViolatorCount = Nothing
    , _erComplianceStatus = Nothing
    , _erEvaluationLimitExceeded = Nothing
    }


-- | Number of resources that are non-compliant with the specified policy. A resource is considered non-compliant if it is not associated with the specified policy.
erViolatorCount :: Lens' EvaluationResult (Maybe Natural)
erViolatorCount = lens _erViolatorCount (\ s a -> s{_erViolatorCount = a}) . mapping _Nat

-- | Describes an AWS account's compliance with the AWS Firewall Manager policy.
erComplianceStatus :: Lens' EvaluationResult (Maybe PolicyComplianceStatusType)
erComplianceStatus = lens _erComplianceStatus (\ s a -> s{_erComplianceStatus = a})

-- | Indicates that over 100 resources are non-compliant with the AWS Firewall Manager policy.
erEvaluationLimitExceeded :: Lens' EvaluationResult (Maybe Bool)
erEvaluationLimitExceeded = lens _erEvaluationLimitExceeded (\ s a -> s{_erEvaluationLimitExceeded = a})

instance FromJSON EvaluationResult where
        parseJSON
          = withObject "EvaluationResult"
              (\ x ->
                 EvaluationResult' <$>
                   (x .:? "ViolatorCount") <*>
                     (x .:? "ComplianceStatus")
                     <*> (x .:? "EvaluationLimitExceeded"))

instance Hashable EvaluationResult where

instance NFData EvaluationResult where

-- | An AWS Firewall Manager policy.
--
--
--
-- /See:/ 'policy' smart constructor.
data Policy = Policy'
  { _pPolicyId                  :: !(Maybe Text)
  , _pResourceTags              :: !(Maybe [ResourceTag])
  , _pPolicyUpdateToken         :: !(Maybe Text)
  , _pPolicyName                :: !Text
  , _pSecurityServicePolicyData :: !SecurityServicePolicyData
  , _pResourceType              :: !Text
  , _pExcludeResourceTags       :: !Bool
  , _pRemediationEnabled        :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Policy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pPolicyId' - The ID of the AWS Firewall Manager policy.
--
-- * 'pResourceTags' - An array of @ResourceTag@ objects.
--
-- * 'pPolicyUpdateToken' - A unique identifier for each update to the policy. When issuing a @PutPolicy@ request, the @PolicyUpdateToken@ in the request must match the @PolicyUpdateToken@ of the current policy version. To get the @PolicyUpdateToken@ of the current policy version, use a @GetPolicy@ request.
--
-- * 'pPolicyName' - The friendly name of the AWS Firewall Manager policy.
--
-- * 'pSecurityServicePolicyData' - Details about the security service that is being used to protect the resources.
--
-- * 'pResourceType' - The type of resource to protect with the policy, either an Application Load Balancer or a CloudFront distribution. This is in the format shown in <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . Valid values are @AWS::ElasticLoadBalancingV2::LoadBalancer@ or @AWS::CloudFront::Distribution@ .
--
-- * 'pExcludeResourceTags' - If set to @True@ , resources with the tags that are specified in the @ResourceTag@ array are not protected by the policy. If set to @False@ , and the @ResourceTag@ array is not null, only resources with the specified tags are associated with the policy.
--
-- * 'pRemediationEnabled' - Indicates if the policy should be automatically applied to new resources.
policy
    :: Text -- ^ 'pPolicyName'
    -> SecurityServicePolicyData -- ^ 'pSecurityServicePolicyData'
    -> Text -- ^ 'pResourceType'
    -> Bool -- ^ 'pExcludeResourceTags'
    -> Bool -- ^ 'pRemediationEnabled'
    -> Policy
policy pPolicyName_ pSecurityServicePolicyData_ pResourceType_ pExcludeResourceTags_ pRemediationEnabled_ =
  Policy'
    { _pPolicyId = Nothing
    , _pResourceTags = Nothing
    , _pPolicyUpdateToken = Nothing
    , _pPolicyName = pPolicyName_
    , _pSecurityServicePolicyData = pSecurityServicePolicyData_
    , _pResourceType = pResourceType_
    , _pExcludeResourceTags = pExcludeResourceTags_
    , _pRemediationEnabled = pRemediationEnabled_
    }


-- | The ID of the AWS Firewall Manager policy.
pPolicyId :: Lens' Policy (Maybe Text)
pPolicyId = lens _pPolicyId (\ s a -> s{_pPolicyId = a})

-- | An array of @ResourceTag@ objects.
pResourceTags :: Lens' Policy [ResourceTag]
pResourceTags = lens _pResourceTags (\ s a -> s{_pResourceTags = a}) . _Default . _Coerce

-- | A unique identifier for each update to the policy. When issuing a @PutPolicy@ request, the @PolicyUpdateToken@ in the request must match the @PolicyUpdateToken@ of the current policy version. To get the @PolicyUpdateToken@ of the current policy version, use a @GetPolicy@ request.
pPolicyUpdateToken :: Lens' Policy (Maybe Text)
pPolicyUpdateToken = lens _pPolicyUpdateToken (\ s a -> s{_pPolicyUpdateToken = a})

-- | The friendly name of the AWS Firewall Manager policy.
pPolicyName :: Lens' Policy Text
pPolicyName = lens _pPolicyName (\ s a -> s{_pPolicyName = a})

-- | Details about the security service that is being used to protect the resources.
pSecurityServicePolicyData :: Lens' Policy SecurityServicePolicyData
pSecurityServicePolicyData = lens _pSecurityServicePolicyData (\ s a -> s{_pSecurityServicePolicyData = a})

-- | The type of resource to protect with the policy, either an Application Load Balancer or a CloudFront distribution. This is in the format shown in <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . Valid values are @AWS::ElasticLoadBalancingV2::LoadBalancer@ or @AWS::CloudFront::Distribution@ .
pResourceType :: Lens' Policy Text
pResourceType = lens _pResourceType (\ s a -> s{_pResourceType = a})

-- | If set to @True@ , resources with the tags that are specified in the @ResourceTag@ array are not protected by the policy. If set to @False@ , and the @ResourceTag@ array is not null, only resources with the specified tags are associated with the policy.
pExcludeResourceTags :: Lens' Policy Bool
pExcludeResourceTags = lens _pExcludeResourceTags (\ s a -> s{_pExcludeResourceTags = a})

-- | Indicates if the policy should be automatically applied to new resources.
pRemediationEnabled :: Lens' Policy Bool
pRemediationEnabled = lens _pRemediationEnabled (\ s a -> s{_pRemediationEnabled = a})

instance FromJSON Policy where
        parseJSON
          = withObject "Policy"
              (\ x ->
                 Policy' <$>
                   (x .:? "PolicyId") <*>
                     (x .:? "ResourceTags" .!= mempty)
                     <*> (x .:? "PolicyUpdateToken")
                     <*> (x .: "PolicyName")
                     <*> (x .: "SecurityServicePolicyData")
                     <*> (x .: "ResourceType")
                     <*> (x .: "ExcludeResourceTags")
                     <*> (x .: "RemediationEnabled"))

instance Hashable Policy where

instance NFData Policy where

instance ToJSON Policy where
        toJSON Policy'{..}
          = object
              (catMaybes
                 [("PolicyId" .=) <$> _pPolicyId,
                  ("ResourceTags" .=) <$> _pResourceTags,
                  ("PolicyUpdateToken" .=) <$> _pPolicyUpdateToken,
                  Just ("PolicyName" .= _pPolicyName),
                  Just
                    ("SecurityServicePolicyData" .=
                       _pSecurityServicePolicyData),
                  Just ("ResourceType" .= _pResourceType),
                  Just
                    ("ExcludeResourceTags" .= _pExcludeResourceTags),
                  Just ("RemediationEnabled" .= _pRemediationEnabled)])

-- | Describes the non-compliant resources in a member account for a specific AWS Firewall Manager policy. A maximum of 100 entries are displayed. If more than 100 resources are non-compliant, @EvaluationLimitExceeded@ is set to @True@ .
--
--
--
-- /See:/ 'policyComplianceDetail' smart constructor.
data PolicyComplianceDetail = PolicyComplianceDetail'
  { _pcdExpiredAt               :: !(Maybe POSIX)
  , _pcdPolicyId                :: !(Maybe Text)
  , _pcdViolators               :: !(Maybe [ComplianceViolator])
  , _pcdEvaluationLimitExceeded :: !(Maybe Bool)
  , _pcdPolicyOwner             :: !(Maybe Text)
  , _pcdMemberAccount           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PolicyComplianceDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcdExpiredAt' - A time stamp that indicates when the returned information should be considered out-of-date.
--
-- * 'pcdPolicyId' - The ID of the AWS Firewall Manager policy.
--
-- * 'pcdViolators' - An array of resources that are not protected by the policy.
--
-- * 'pcdEvaluationLimitExceeded' - Indicates if over 100 resources are non-compliant with the AWS Firewall Manager policy.
--
-- * 'pcdPolicyOwner' - The AWS account that created the AWS Firewall Manager policy.
--
-- * 'pcdMemberAccount' - The AWS account ID.
policyComplianceDetail
    :: PolicyComplianceDetail
policyComplianceDetail =
  PolicyComplianceDetail'
    { _pcdExpiredAt = Nothing
    , _pcdPolicyId = Nothing
    , _pcdViolators = Nothing
    , _pcdEvaluationLimitExceeded = Nothing
    , _pcdPolicyOwner = Nothing
    , _pcdMemberAccount = Nothing
    }


-- | A time stamp that indicates when the returned information should be considered out-of-date.
pcdExpiredAt :: Lens' PolicyComplianceDetail (Maybe UTCTime)
pcdExpiredAt = lens _pcdExpiredAt (\ s a -> s{_pcdExpiredAt = a}) . mapping _Time

-- | The ID of the AWS Firewall Manager policy.
pcdPolicyId :: Lens' PolicyComplianceDetail (Maybe Text)
pcdPolicyId = lens _pcdPolicyId (\ s a -> s{_pcdPolicyId = a})

-- | An array of resources that are not protected by the policy.
pcdViolators :: Lens' PolicyComplianceDetail [ComplianceViolator]
pcdViolators = lens _pcdViolators (\ s a -> s{_pcdViolators = a}) . _Default . _Coerce

-- | Indicates if over 100 resources are non-compliant with the AWS Firewall Manager policy.
pcdEvaluationLimitExceeded :: Lens' PolicyComplianceDetail (Maybe Bool)
pcdEvaluationLimitExceeded = lens _pcdEvaluationLimitExceeded (\ s a -> s{_pcdEvaluationLimitExceeded = a})

-- | The AWS account that created the AWS Firewall Manager policy.
pcdPolicyOwner :: Lens' PolicyComplianceDetail (Maybe Text)
pcdPolicyOwner = lens _pcdPolicyOwner (\ s a -> s{_pcdPolicyOwner = a})

-- | The AWS account ID.
pcdMemberAccount :: Lens' PolicyComplianceDetail (Maybe Text)
pcdMemberAccount = lens _pcdMemberAccount (\ s a -> s{_pcdMemberAccount = a})

instance FromJSON PolicyComplianceDetail where
        parseJSON
          = withObject "PolicyComplianceDetail"
              (\ x ->
                 PolicyComplianceDetail' <$>
                   (x .:? "ExpiredAt") <*> (x .:? "PolicyId") <*>
                     (x .:? "Violators" .!= mempty)
                     <*> (x .:? "EvaluationLimitExceeded")
                     <*> (x .:? "PolicyOwner")
                     <*> (x .:? "MemberAccount"))

instance Hashable PolicyComplianceDetail where

instance NFData PolicyComplianceDetail where

-- | Indicates whether the account is compliant with the specified policy. An account is considered non-compliant if it includes resources that are not protected by the policy.
--
--
--
-- /See:/ 'policyComplianceStatus' smart constructor.
data PolicyComplianceStatus = PolicyComplianceStatus'
  { _pcsEvaluationResults :: !(Maybe [EvaluationResult])
  , _pcsLastUpdated       :: !(Maybe POSIX)
  , _pcsPolicyName        :: !(Maybe Text)
  , _pcsPolicyId          :: !(Maybe Text)
  , _pcsPolicyOwner       :: !(Maybe Text)
  , _pcsMemberAccount     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PolicyComplianceStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcsEvaluationResults' - An array of @EvaluationResult@ objects.
--
-- * 'pcsLastUpdated' - Time stamp of the last update to the @EvaluationResult@ objects.
--
-- * 'pcsPolicyName' - The friendly name of the AWS Firewall Manager policy.
--
-- * 'pcsPolicyId' - The ID of the AWS Firewall Manager policy.
--
-- * 'pcsPolicyOwner' - The AWS account that created the AWS Firewall Manager policy.
--
-- * 'pcsMemberAccount' - The member account ID.
policyComplianceStatus
    :: PolicyComplianceStatus
policyComplianceStatus =
  PolicyComplianceStatus'
    { _pcsEvaluationResults = Nothing
    , _pcsLastUpdated = Nothing
    , _pcsPolicyName = Nothing
    , _pcsPolicyId = Nothing
    , _pcsPolicyOwner = Nothing
    , _pcsMemberAccount = Nothing
    }


-- | An array of @EvaluationResult@ objects.
pcsEvaluationResults :: Lens' PolicyComplianceStatus [EvaluationResult]
pcsEvaluationResults = lens _pcsEvaluationResults (\ s a -> s{_pcsEvaluationResults = a}) . _Default . _Coerce

-- | Time stamp of the last update to the @EvaluationResult@ objects.
pcsLastUpdated :: Lens' PolicyComplianceStatus (Maybe UTCTime)
pcsLastUpdated = lens _pcsLastUpdated (\ s a -> s{_pcsLastUpdated = a}) . mapping _Time

-- | The friendly name of the AWS Firewall Manager policy.
pcsPolicyName :: Lens' PolicyComplianceStatus (Maybe Text)
pcsPolicyName = lens _pcsPolicyName (\ s a -> s{_pcsPolicyName = a})

-- | The ID of the AWS Firewall Manager policy.
pcsPolicyId :: Lens' PolicyComplianceStatus (Maybe Text)
pcsPolicyId = lens _pcsPolicyId (\ s a -> s{_pcsPolicyId = a})

-- | The AWS account that created the AWS Firewall Manager policy.
pcsPolicyOwner :: Lens' PolicyComplianceStatus (Maybe Text)
pcsPolicyOwner = lens _pcsPolicyOwner (\ s a -> s{_pcsPolicyOwner = a})

-- | The member account ID.
pcsMemberAccount :: Lens' PolicyComplianceStatus (Maybe Text)
pcsMemberAccount = lens _pcsMemberAccount (\ s a -> s{_pcsMemberAccount = a})

instance FromJSON PolicyComplianceStatus where
        parseJSON
          = withObject "PolicyComplianceStatus"
              (\ x ->
                 PolicyComplianceStatus' <$>
                   (x .:? "EvaluationResults" .!= mempty) <*>
                     (x .:? "LastUpdated")
                     <*> (x .:? "PolicyName")
                     <*> (x .:? "PolicyId")
                     <*> (x .:? "PolicyOwner")
                     <*> (x .:? "MemberAccount"))

instance Hashable PolicyComplianceStatus where

instance NFData PolicyComplianceStatus where

-- | Details of the AWS Firewall Manager policy.
--
--
--
-- /See:/ 'policySummary' smart constructor.
data PolicySummary = PolicySummary'
  { _psPolicyName          :: !(Maybe Text)
  , _psRemediationEnabled  :: !(Maybe Bool)
  , _psResourceType        :: !(Maybe Text)
  , _psPolicyId            :: !(Maybe Text)
  , _psPolicyARN           :: !(Maybe Text)
  , _psSecurityServiceType :: !(Maybe SecurityServiceType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PolicySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psPolicyName' - The friendly name of the specified policy.
--
-- * 'psRemediationEnabled' - Indicates if the policy should be automatically applied to new resources.
--
-- * 'psResourceType' - The type of resource to protect with the policy, either an Application Load Balancer or a CloudFront distribution. This is in the format shown in <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . Valid values are @AWS::ElasticLoadBalancingV2::LoadBalancer@ or @AWS::CloudFront::Distribution@ .
--
-- * 'psPolicyId' - The ID of the specified policy.
--
-- * 'psPolicyARN' - The Amazon Resource Name (ARN) of the specified policy.
--
-- * 'psSecurityServiceType' - The service that the policy is using to protect the resources. This value is @WAF@ .
policySummary
    :: PolicySummary
policySummary =
  PolicySummary'
    { _psPolicyName = Nothing
    , _psRemediationEnabled = Nothing
    , _psResourceType = Nothing
    , _psPolicyId = Nothing
    , _psPolicyARN = Nothing
    , _psSecurityServiceType = Nothing
    }


-- | The friendly name of the specified policy.
psPolicyName :: Lens' PolicySummary (Maybe Text)
psPolicyName = lens _psPolicyName (\ s a -> s{_psPolicyName = a})

-- | Indicates if the policy should be automatically applied to new resources.
psRemediationEnabled :: Lens' PolicySummary (Maybe Bool)
psRemediationEnabled = lens _psRemediationEnabled (\ s a -> s{_psRemediationEnabled = a})

-- | The type of resource to protect with the policy, either an Application Load Balancer or a CloudFront distribution. This is in the format shown in <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . Valid values are @AWS::ElasticLoadBalancingV2::LoadBalancer@ or @AWS::CloudFront::Distribution@ .
psResourceType :: Lens' PolicySummary (Maybe Text)
psResourceType = lens _psResourceType (\ s a -> s{_psResourceType = a})

-- | The ID of the specified policy.
psPolicyId :: Lens' PolicySummary (Maybe Text)
psPolicyId = lens _psPolicyId (\ s a -> s{_psPolicyId = a})

-- | The Amazon Resource Name (ARN) of the specified policy.
psPolicyARN :: Lens' PolicySummary (Maybe Text)
psPolicyARN = lens _psPolicyARN (\ s a -> s{_psPolicyARN = a})

-- | The service that the policy is using to protect the resources. This value is @WAF@ .
psSecurityServiceType :: Lens' PolicySummary (Maybe SecurityServiceType)
psSecurityServiceType = lens _psSecurityServiceType (\ s a -> s{_psSecurityServiceType = a})

instance FromJSON PolicySummary where
        parseJSON
          = withObject "PolicySummary"
              (\ x ->
                 PolicySummary' <$>
                   (x .:? "PolicyName") <*> (x .:? "RemediationEnabled")
                     <*> (x .:? "ResourceType")
                     <*> (x .:? "PolicyId")
                     <*> (x .:? "PolicyArn")
                     <*> (x .:? "SecurityServiceType"))

instance Hashable PolicySummary where

instance NFData PolicySummary where

-- | The resource tags that AWS Firewall Manager uses to determine if a particular resource should be included or excluded from protection by the AWS Firewall Manager policy. Tags enable you to categorize your AWS resources in different ways, for example, by purpose, owner, or environment. Each tag consists of a key and an optional value, both of which you define. Tags are combined with an "OR." That is, if you add more than one tag, if any of the tags matches, the resource is considered a match for the include or exclude. <https://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/tag-editor.html Working with Tag Editor> .
--
--
--
-- /See:/ 'resourceTag' smart constructor.
data ResourceTag = ResourceTag'
  { _rtValue :: !(Maybe Text)
  , _rtKey   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceTag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtValue' - The resource tag value.
--
-- * 'rtKey' - The resource tag key.
resourceTag
    :: Text -- ^ 'rtKey'
    -> ResourceTag
resourceTag pKey_ = ResourceTag' {_rtValue = Nothing, _rtKey = pKey_}


-- | The resource tag value.
rtValue :: Lens' ResourceTag (Maybe Text)
rtValue = lens _rtValue (\ s a -> s{_rtValue = a})

-- | The resource tag key.
rtKey :: Lens' ResourceTag Text
rtKey = lens _rtKey (\ s a -> s{_rtKey = a})

instance FromJSON ResourceTag where
        parseJSON
          = withObject "ResourceTag"
              (\ x ->
                 ResourceTag' <$> (x .:? "Value") <*> (x .: "Key"))

instance Hashable ResourceTag where

instance NFData ResourceTag where

instance ToJSON ResourceTag where
        toJSON ResourceTag'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _rtValue, Just ("Key" .= _rtKey)])

-- | Details about the security service that is being used to protect the resources.
--
--
--
-- /See:/ 'securityServicePolicyData' smart constructor.
data SecurityServicePolicyData = SecurityServicePolicyData'
  { _sspdManagedServiceData :: !(Maybe Text)
  , _sspdType               :: !SecurityServiceType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SecurityServicePolicyData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sspdManagedServiceData' - Details about the service. This contains @WAF@ data in JSON format, as shown in the following example: @ManagedServiceData": "{\"type\": \"WAF\", \"ruleGroups\": [{\"id\": \"12345678-1bcd-9012-efga-0987654321ab\", \"overrideAction\" : {\"type\": \"COUNT\"}}], \"defaultAction\": {\"type\": \"BLOCK\"}}@
--
-- * 'sspdType' - The service that the policy is using to protect the resources. This value is @WAF@ .
securityServicePolicyData
    :: SecurityServiceType -- ^ 'sspdType'
    -> SecurityServicePolicyData
securityServicePolicyData pType_ =
  SecurityServicePolicyData'
    {_sspdManagedServiceData = Nothing, _sspdType = pType_}


-- | Details about the service. This contains @WAF@ data in JSON format, as shown in the following example: @ManagedServiceData": "{\"type\": \"WAF\", \"ruleGroups\": [{\"id\": \"12345678-1bcd-9012-efga-0987654321ab\", \"overrideAction\" : {\"type\": \"COUNT\"}}], \"defaultAction\": {\"type\": \"BLOCK\"}}@
sspdManagedServiceData :: Lens' SecurityServicePolicyData (Maybe Text)
sspdManagedServiceData = lens _sspdManagedServiceData (\ s a -> s{_sspdManagedServiceData = a})

-- | The service that the policy is using to protect the resources. This value is @WAF@ .
sspdType :: Lens' SecurityServicePolicyData SecurityServiceType
sspdType = lens _sspdType (\ s a -> s{_sspdType = a})

instance FromJSON SecurityServicePolicyData where
        parseJSON
          = withObject "SecurityServicePolicyData"
              (\ x ->
                 SecurityServicePolicyData' <$>
                   (x .:? "ManagedServiceData") <*> (x .: "Type"))

instance Hashable SecurityServicePolicyData where

instance NFData SecurityServicePolicyData where

instance ToJSON SecurityServicePolicyData where
        toJSON SecurityServicePolicyData'{..}
          = object
              (catMaybes
                 [("ManagedServiceData" .=) <$>
                    _sspdManagedServiceData,
                  Just ("Type" .= _sspdType)])
