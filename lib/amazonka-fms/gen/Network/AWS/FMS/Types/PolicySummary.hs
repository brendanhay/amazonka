{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.PolicySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.PolicySummary where

import Network.AWS.FMS.Types.SecurityServiceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details of the AWS Firewall Manager policy.
--
--
--
-- /See:/ 'policySummary' smart constructor.
data PolicySummary = PolicySummary'
  { _psPolicyName :: !(Maybe Text),
    _psRemediationEnabled :: !(Maybe Bool),
    _psResourceType :: !(Maybe Text),
    _psPolicyId :: !(Maybe Text),
    _psPolicyARN :: !(Maybe Text),
    _psSecurityServiceType :: !(Maybe SecurityServiceType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PolicySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psPolicyName' - The name of the specified policy.
--
-- * 'psRemediationEnabled' - Indicates if the policy should be automatically applied to new resources.
--
-- * 'psResourceType' - The type of resource protected by or in scope of the policy. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . For AWS WAF and Shield Advanced, examples include @AWS::ElasticLoadBalancingV2::LoadBalancer@ and @AWS::CloudFront::Distribution@ . For a security group common policy, valid values are @AWS::EC2::NetworkInterface@ and @AWS::EC2::Instance@ . For a security group content audit policy, valid values are @AWS::EC2::SecurityGroup@ , @AWS::EC2::NetworkInterface@ , and @AWS::EC2::Instance@ . For a security group usage audit policy, the value is @AWS::EC2::SecurityGroup@ . For an AWS Network Firewall policy, the value is @AWS::EC2::VPC@ .
--
-- * 'psPolicyId' - The ID of the specified policy.
--
-- * 'psPolicyARN' - The Amazon Resource Name (ARN) of the specified policy.
--
-- * 'psSecurityServiceType' - The service that the policy is using to protect the resources. This specifies the type of policy that is created, either an AWS WAF policy, a Shield Advanced policy, or a security group policy.
policySummary ::
  PolicySummary
policySummary =
  PolicySummary'
    { _psPolicyName = Nothing,
      _psRemediationEnabled = Nothing,
      _psResourceType = Nothing,
      _psPolicyId = Nothing,
      _psPolicyARN = Nothing,
      _psSecurityServiceType = Nothing
    }

-- | The name of the specified policy.
psPolicyName :: Lens' PolicySummary (Maybe Text)
psPolicyName = lens _psPolicyName (\s a -> s {_psPolicyName = a})

-- | Indicates if the policy should be automatically applied to new resources.
psRemediationEnabled :: Lens' PolicySummary (Maybe Bool)
psRemediationEnabled = lens _psRemediationEnabled (\s a -> s {_psRemediationEnabled = a})

-- | The type of resource protected by or in scope of the policy. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . For AWS WAF and Shield Advanced, examples include @AWS::ElasticLoadBalancingV2::LoadBalancer@ and @AWS::CloudFront::Distribution@ . For a security group common policy, valid values are @AWS::EC2::NetworkInterface@ and @AWS::EC2::Instance@ . For a security group content audit policy, valid values are @AWS::EC2::SecurityGroup@ , @AWS::EC2::NetworkInterface@ , and @AWS::EC2::Instance@ . For a security group usage audit policy, the value is @AWS::EC2::SecurityGroup@ . For an AWS Network Firewall policy, the value is @AWS::EC2::VPC@ .
psResourceType :: Lens' PolicySummary (Maybe Text)
psResourceType = lens _psResourceType (\s a -> s {_psResourceType = a})

-- | The ID of the specified policy.
psPolicyId :: Lens' PolicySummary (Maybe Text)
psPolicyId = lens _psPolicyId (\s a -> s {_psPolicyId = a})

-- | The Amazon Resource Name (ARN) of the specified policy.
psPolicyARN :: Lens' PolicySummary (Maybe Text)
psPolicyARN = lens _psPolicyARN (\s a -> s {_psPolicyARN = a})

-- | The service that the policy is using to protect the resources. This specifies the type of policy that is created, either an AWS WAF policy, a Shield Advanced policy, or a security group policy.
psSecurityServiceType :: Lens' PolicySummary (Maybe SecurityServiceType)
psSecurityServiceType = lens _psSecurityServiceType (\s a -> s {_psSecurityServiceType = a})

instance FromJSON PolicySummary where
  parseJSON =
    withObject
      "PolicySummary"
      ( \x ->
          PolicySummary'
            <$> (x .:? "PolicyName")
            <*> (x .:? "RemediationEnabled")
            <*> (x .:? "ResourceType")
            <*> (x .:? "PolicyId")
            <*> (x .:? "PolicyArn")
            <*> (x .:? "SecurityServiceType")
      )

instance Hashable PolicySummary

instance NFData PolicySummary
