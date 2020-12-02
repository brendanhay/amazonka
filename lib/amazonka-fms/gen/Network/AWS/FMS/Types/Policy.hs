{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.Policy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.Policy where

import Network.AWS.FMS.Types.CustomerPolicyScopeIdType
import Network.AWS.FMS.Types.ResourceTag
import Network.AWS.FMS.Types.SecurityServicePolicyData
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An AWS Firewall Manager policy.
--
--
--
-- /See:/ 'policy' smart constructor.
data Policy = Policy'
  { _pPolicyId :: !(Maybe Text),
    _pResourceTypeList :: !(Maybe [Text]),
    _pResourceTags :: !(Maybe [ResourceTag]),
    _pPolicyUpdateToken :: !(Maybe Text),
    _pExcludeMap :: !(Maybe (Map CustomerPolicyScopeIdType ([Text]))),
    _pIncludeMap :: !(Maybe (Map CustomerPolicyScopeIdType ([Text]))),
    _pPolicyName :: !Text,
    _pSecurityServicePolicyData :: !SecurityServicePolicyData,
    _pResourceType :: !Text,
    _pExcludeResourceTags :: !Bool,
    _pRemediationEnabled :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Policy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pPolicyId' - The ID of the AWS Firewall Manager policy.
--
-- * 'pResourceTypeList' - An array of @ResourceType@ .
--
-- * 'pResourceTags' - An array of @ResourceTag@ objects.
--
-- * 'pPolicyUpdateToken' - A unique identifier for each update to the policy. When issuing a @PutPolicy@ request, the @PolicyUpdateToken@ in the request must match the @PolicyUpdateToken@ of the current policy version. To get the @PolicyUpdateToken@ of the current policy version, use a @GetPolicy@ request.
--
-- * 'pExcludeMap' - Specifies the AWS account IDs and AWS Organizations organizational units (OUs) to exclude from the policy. Specifying an OU is the equivalent of specifying all accounts in the OU and in any of its child OUs, including any child OUs and accounts that are added at a later time. You can specify inclusions or exclusions, but not both. If you specify an @IncludeMap@ , AWS Firewall Manager applies the policy to all accounts specified by the @IncludeMap@ , and does not evaluate any @ExcludeMap@ specifications. If you do not specify an @IncludeMap@ , then Firewall Manager applies the policy to all accounts except for those specified by the @ExcludeMap@ . You can specify account IDs, OUs, or a combination:      * Specify account IDs by setting the key to @ACCOUNT@ . For example, the following is a valid map: @{“ACCOUNT” : [“accountID1”, “accountID2”]}@ .     * Specify OUs by setting the key to @ORG_UNIT@ . For example, the following is a valid map: @{“ORG_UNIT” : [“ouid111”, “ouid112”]}@ .     * Specify accounts and OUs together in a single map, separated with a comma. For example, the following is a valid map: @{“ACCOUNT” : [“accountID1”, “accountID2”], “ORG_UNIT” : [“ouid111”, “ouid112”]}@ .
--
-- * 'pIncludeMap' - Specifies the AWS account IDs and AWS Organizations organizational units (OUs) to include in the policy. Specifying an OU is the equivalent of specifying all accounts in the OU and in any of its child OUs, including any child OUs and accounts that are added at a later time. You can specify inclusions or exclusions, but not both. If you specify an @IncludeMap@ , AWS Firewall Manager applies the policy to all accounts specified by the @IncludeMap@ , and does not evaluate any @ExcludeMap@ specifications. If you do not specify an @IncludeMap@ , then Firewall Manager applies the policy to all accounts except for those specified by the @ExcludeMap@ . You can specify account IDs, OUs, or a combination:      * Specify account IDs by setting the key to @ACCOUNT@ . For example, the following is a valid map: @{“ACCOUNT” : [“accountID1”, “accountID2”]}@ .     * Specify OUs by setting the key to @ORG_UNIT@ . For example, the following is a valid map: @{“ORG_UNIT” : [“ouid111”, “ouid112”]}@ .     * Specify accounts and OUs together in a single map, separated with a comma. For example, the following is a valid map: @{“ACCOUNT” : [“accountID1”, “accountID2”], “ORG_UNIT” : [“ouid111”, “ouid112”]}@ .
--
-- * 'pPolicyName' - The name of the AWS Firewall Manager policy.
--
-- * 'pSecurityServicePolicyData' - Details about the security service that is being used to protect the resources.
--
-- * 'pResourceType' - The type of resource protected by or in scope of the policy. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . For AWS WAF and Shield Advanced, examples include @AWS::ElasticLoadBalancingV2::LoadBalancer@ and @AWS::CloudFront::Distribution@ . For a security group common policy, valid values are @AWS::EC2::NetworkInterface@ and @AWS::EC2::Instance@ . For a security group content audit policy, valid values are @AWS::EC2::SecurityGroup@ , @AWS::EC2::NetworkInterface@ , and @AWS::EC2::Instance@ . For a security group usage audit policy, the value is @AWS::EC2::SecurityGroup@ . For an AWS Network Firewall policy, the value is @AWS::EC2::VPC@ .
--
-- * 'pExcludeResourceTags' - If set to @True@ , resources with the tags that are specified in the @ResourceTag@ array are not in scope of the policy. If set to @False@ , and the @ResourceTag@ array is not null, only resources with the specified tags are in scope of the policy.
--
-- * 'pRemediationEnabled' - Indicates if the policy should be automatically applied to new resources.
policy ::
  -- | 'pPolicyName'
  Text ->
  -- | 'pSecurityServicePolicyData'
  SecurityServicePolicyData ->
  -- | 'pResourceType'
  Text ->
  -- | 'pExcludeResourceTags'
  Bool ->
  -- | 'pRemediationEnabled'
  Bool ->
  Policy
policy
  pPolicyName_
  pSecurityServicePolicyData_
  pResourceType_
  pExcludeResourceTags_
  pRemediationEnabled_ =
    Policy'
      { _pPolicyId = Nothing,
        _pResourceTypeList = Nothing,
        _pResourceTags = Nothing,
        _pPolicyUpdateToken = Nothing,
        _pExcludeMap = Nothing,
        _pIncludeMap = Nothing,
        _pPolicyName = pPolicyName_,
        _pSecurityServicePolicyData = pSecurityServicePolicyData_,
        _pResourceType = pResourceType_,
        _pExcludeResourceTags = pExcludeResourceTags_,
        _pRemediationEnabled = pRemediationEnabled_
      }

-- | The ID of the AWS Firewall Manager policy.
pPolicyId :: Lens' Policy (Maybe Text)
pPolicyId = lens _pPolicyId (\s a -> s {_pPolicyId = a})

-- | An array of @ResourceType@ .
pResourceTypeList :: Lens' Policy [Text]
pResourceTypeList = lens _pResourceTypeList (\s a -> s {_pResourceTypeList = a}) . _Default . _Coerce

-- | An array of @ResourceTag@ objects.
pResourceTags :: Lens' Policy [ResourceTag]
pResourceTags = lens _pResourceTags (\s a -> s {_pResourceTags = a}) . _Default . _Coerce

-- | A unique identifier for each update to the policy. When issuing a @PutPolicy@ request, the @PolicyUpdateToken@ in the request must match the @PolicyUpdateToken@ of the current policy version. To get the @PolicyUpdateToken@ of the current policy version, use a @GetPolicy@ request.
pPolicyUpdateToken :: Lens' Policy (Maybe Text)
pPolicyUpdateToken = lens _pPolicyUpdateToken (\s a -> s {_pPolicyUpdateToken = a})

-- | Specifies the AWS account IDs and AWS Organizations organizational units (OUs) to exclude from the policy. Specifying an OU is the equivalent of specifying all accounts in the OU and in any of its child OUs, including any child OUs and accounts that are added at a later time. You can specify inclusions or exclusions, but not both. If you specify an @IncludeMap@ , AWS Firewall Manager applies the policy to all accounts specified by the @IncludeMap@ , and does not evaluate any @ExcludeMap@ specifications. If you do not specify an @IncludeMap@ , then Firewall Manager applies the policy to all accounts except for those specified by the @ExcludeMap@ . You can specify account IDs, OUs, or a combination:      * Specify account IDs by setting the key to @ACCOUNT@ . For example, the following is a valid map: @{“ACCOUNT” : [“accountID1”, “accountID2”]}@ .     * Specify OUs by setting the key to @ORG_UNIT@ . For example, the following is a valid map: @{“ORG_UNIT” : [“ouid111”, “ouid112”]}@ .     * Specify accounts and OUs together in a single map, separated with a comma. For example, the following is a valid map: @{“ACCOUNT” : [“accountID1”, “accountID2”], “ORG_UNIT” : [“ouid111”, “ouid112”]}@ .
pExcludeMap :: Lens' Policy (HashMap CustomerPolicyScopeIdType ([Text]))
pExcludeMap = lens _pExcludeMap (\s a -> s {_pExcludeMap = a}) . _Default . _Map

-- | Specifies the AWS account IDs and AWS Organizations organizational units (OUs) to include in the policy. Specifying an OU is the equivalent of specifying all accounts in the OU and in any of its child OUs, including any child OUs and accounts that are added at a later time. You can specify inclusions or exclusions, but not both. If you specify an @IncludeMap@ , AWS Firewall Manager applies the policy to all accounts specified by the @IncludeMap@ , and does not evaluate any @ExcludeMap@ specifications. If you do not specify an @IncludeMap@ , then Firewall Manager applies the policy to all accounts except for those specified by the @ExcludeMap@ . You can specify account IDs, OUs, or a combination:      * Specify account IDs by setting the key to @ACCOUNT@ . For example, the following is a valid map: @{“ACCOUNT” : [“accountID1”, “accountID2”]}@ .     * Specify OUs by setting the key to @ORG_UNIT@ . For example, the following is a valid map: @{“ORG_UNIT” : [“ouid111”, “ouid112”]}@ .     * Specify accounts and OUs together in a single map, separated with a comma. For example, the following is a valid map: @{“ACCOUNT” : [“accountID1”, “accountID2”], “ORG_UNIT” : [“ouid111”, “ouid112”]}@ .
pIncludeMap :: Lens' Policy (HashMap CustomerPolicyScopeIdType ([Text]))
pIncludeMap = lens _pIncludeMap (\s a -> s {_pIncludeMap = a}) . _Default . _Map

-- | The name of the AWS Firewall Manager policy.
pPolicyName :: Lens' Policy Text
pPolicyName = lens _pPolicyName (\s a -> s {_pPolicyName = a})

-- | Details about the security service that is being used to protect the resources.
pSecurityServicePolicyData :: Lens' Policy SecurityServicePolicyData
pSecurityServicePolicyData = lens _pSecurityServicePolicyData (\s a -> s {_pSecurityServicePolicyData = a})

-- | The type of resource protected by or in scope of the policy. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . For AWS WAF and Shield Advanced, examples include @AWS::ElasticLoadBalancingV2::LoadBalancer@ and @AWS::CloudFront::Distribution@ . For a security group common policy, valid values are @AWS::EC2::NetworkInterface@ and @AWS::EC2::Instance@ . For a security group content audit policy, valid values are @AWS::EC2::SecurityGroup@ , @AWS::EC2::NetworkInterface@ , and @AWS::EC2::Instance@ . For a security group usage audit policy, the value is @AWS::EC2::SecurityGroup@ . For an AWS Network Firewall policy, the value is @AWS::EC2::VPC@ .
pResourceType :: Lens' Policy Text
pResourceType = lens _pResourceType (\s a -> s {_pResourceType = a})

-- | If set to @True@ , resources with the tags that are specified in the @ResourceTag@ array are not in scope of the policy. If set to @False@ , and the @ResourceTag@ array is not null, only resources with the specified tags are in scope of the policy.
pExcludeResourceTags :: Lens' Policy Bool
pExcludeResourceTags = lens _pExcludeResourceTags (\s a -> s {_pExcludeResourceTags = a})

-- | Indicates if the policy should be automatically applied to new resources.
pRemediationEnabled :: Lens' Policy Bool
pRemediationEnabled = lens _pRemediationEnabled (\s a -> s {_pRemediationEnabled = a})

instance FromJSON Policy where
  parseJSON =
    withObject
      "Policy"
      ( \x ->
          Policy'
            <$> (x .:? "PolicyId")
            <*> (x .:? "ResourceTypeList" .!= mempty)
            <*> (x .:? "ResourceTags" .!= mempty)
            <*> (x .:? "PolicyUpdateToken")
            <*> (x .:? "ExcludeMap" .!= mempty)
            <*> (x .:? "IncludeMap" .!= mempty)
            <*> (x .: "PolicyName")
            <*> (x .: "SecurityServicePolicyData")
            <*> (x .: "ResourceType")
            <*> (x .: "ExcludeResourceTags")
            <*> (x .: "RemediationEnabled")
      )

instance Hashable Policy

instance NFData Policy

instance ToJSON Policy where
  toJSON Policy' {..} =
    object
      ( catMaybes
          [ ("PolicyId" .=) <$> _pPolicyId,
            ("ResourceTypeList" .=) <$> _pResourceTypeList,
            ("ResourceTags" .=) <$> _pResourceTags,
            ("PolicyUpdateToken" .=) <$> _pPolicyUpdateToken,
            ("ExcludeMap" .=) <$> _pExcludeMap,
            ("IncludeMap" .=) <$> _pIncludeMap,
            Just ("PolicyName" .= _pPolicyName),
            Just ("SecurityServicePolicyData" .= _pSecurityServicePolicyData),
            Just ("ResourceType" .= _pResourceType),
            Just ("ExcludeResourceTags" .= _pExcludeResourceTags),
            Just ("RemediationEnabled" .= _pRemediationEnabled)
          ]
      )
