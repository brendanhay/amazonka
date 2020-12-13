{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.PolicySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.PolicySummary
  ( PolicySummary (..),

    -- * Smart constructor
    mkPolicySummary,

    -- * Lenses
    psPolicyName,
    psRemediationEnabled,
    psResourceType,
    psPolicyId,
    psPolicyARN,
    psSecurityServiceType,
  )
where

import Network.AWS.FMS.Types.SecurityServiceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details of the AWS Firewall Manager policy.
--
-- /See:/ 'mkPolicySummary' smart constructor.
data PolicySummary = PolicySummary'
  { -- | The name of the specified policy.
    policyName :: Lude.Maybe Lude.Text,
    -- | Indicates if the policy should be automatically applied to new resources.
    remediationEnabled :: Lude.Maybe Lude.Bool,
    -- | The type of resource protected by or in scope of the policy. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . For AWS WAF and Shield Advanced, examples include @AWS::ElasticLoadBalancingV2::LoadBalancer@ and @AWS::CloudFront::Distribution@ . For a security group common policy, valid values are @AWS::EC2::NetworkInterface@ and @AWS::EC2::Instance@ . For a security group content audit policy, valid values are @AWS::EC2::SecurityGroup@ , @AWS::EC2::NetworkInterface@ , and @AWS::EC2::Instance@ . For a security group usage audit policy, the value is @AWS::EC2::SecurityGroup@ . For an AWS Network Firewall policy, the value is @AWS::EC2::VPC@ .
    resourceType :: Lude.Maybe Lude.Text,
    -- | The ID of the specified policy.
    policyId :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the specified policy.
    policyARN :: Lude.Maybe Lude.Text,
    -- | The service that the policy is using to protect the resources. This specifies the type of policy that is created, either an AWS WAF policy, a Shield Advanced policy, or a security group policy.
    securityServiceType :: Lude.Maybe SecurityServiceType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PolicySummary' with the minimum fields required to make a request.
--
-- * 'policyName' - The name of the specified policy.
-- * 'remediationEnabled' - Indicates if the policy should be automatically applied to new resources.
-- * 'resourceType' - The type of resource protected by or in scope of the policy. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . For AWS WAF and Shield Advanced, examples include @AWS::ElasticLoadBalancingV2::LoadBalancer@ and @AWS::CloudFront::Distribution@ . For a security group common policy, valid values are @AWS::EC2::NetworkInterface@ and @AWS::EC2::Instance@ . For a security group content audit policy, valid values are @AWS::EC2::SecurityGroup@ , @AWS::EC2::NetworkInterface@ , and @AWS::EC2::Instance@ . For a security group usage audit policy, the value is @AWS::EC2::SecurityGroup@ . For an AWS Network Firewall policy, the value is @AWS::EC2::VPC@ .
-- * 'policyId' - The ID of the specified policy.
-- * 'policyARN' - The Amazon Resource Name (ARN) of the specified policy.
-- * 'securityServiceType' - The service that the policy is using to protect the resources. This specifies the type of policy that is created, either an AWS WAF policy, a Shield Advanced policy, or a security group policy.
mkPolicySummary ::
  PolicySummary
mkPolicySummary =
  PolicySummary'
    { policyName = Lude.Nothing,
      remediationEnabled = Lude.Nothing,
      resourceType = Lude.Nothing,
      policyId = Lude.Nothing,
      policyARN = Lude.Nothing,
      securityServiceType = Lude.Nothing
    }

-- | The name of the specified policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPolicyName :: Lens.Lens' PolicySummary (Lude.Maybe Lude.Text)
psPolicyName = Lens.lens (policyName :: PolicySummary -> Lude.Maybe Lude.Text) (\s a -> s {policyName = a} :: PolicySummary)
{-# DEPRECATED psPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | Indicates if the policy should be automatically applied to new resources.
--
-- /Note:/ Consider using 'remediationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psRemediationEnabled :: Lens.Lens' PolicySummary (Lude.Maybe Lude.Bool)
psRemediationEnabled = Lens.lens (remediationEnabled :: PolicySummary -> Lude.Maybe Lude.Bool) (\s a -> s {remediationEnabled = a} :: PolicySummary)
{-# DEPRECATED psRemediationEnabled "Use generic-lens or generic-optics with 'remediationEnabled' instead." #-}

-- | The type of resource protected by or in scope of the policy. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . For AWS WAF and Shield Advanced, examples include @AWS::ElasticLoadBalancingV2::LoadBalancer@ and @AWS::CloudFront::Distribution@ . For a security group common policy, valid values are @AWS::EC2::NetworkInterface@ and @AWS::EC2::Instance@ . For a security group content audit policy, valid values are @AWS::EC2::SecurityGroup@ , @AWS::EC2::NetworkInterface@ , and @AWS::EC2::Instance@ . For a security group usage audit policy, the value is @AWS::EC2::SecurityGroup@ . For an AWS Network Firewall policy, the value is @AWS::EC2::VPC@ .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psResourceType :: Lens.Lens' PolicySummary (Lude.Maybe Lude.Text)
psResourceType = Lens.lens (resourceType :: PolicySummary -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: PolicySummary)
{-# DEPRECATED psResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The ID of the specified policy.
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPolicyId :: Lens.Lens' PolicySummary (Lude.Maybe Lude.Text)
psPolicyId = Lens.lens (policyId :: PolicySummary -> Lude.Maybe Lude.Text) (\s a -> s {policyId = a} :: PolicySummary)
{-# DEPRECATED psPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

-- | The Amazon Resource Name (ARN) of the specified policy.
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPolicyARN :: Lens.Lens' PolicySummary (Lude.Maybe Lude.Text)
psPolicyARN = Lens.lens (policyARN :: PolicySummary -> Lude.Maybe Lude.Text) (\s a -> s {policyARN = a} :: PolicySummary)
{-# DEPRECATED psPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

-- | The service that the policy is using to protect the resources. This specifies the type of policy that is created, either an AWS WAF policy, a Shield Advanced policy, or a security group policy.
--
-- /Note:/ Consider using 'securityServiceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psSecurityServiceType :: Lens.Lens' PolicySummary (Lude.Maybe SecurityServiceType)
psSecurityServiceType = Lens.lens (securityServiceType :: PolicySummary -> Lude.Maybe SecurityServiceType) (\s a -> s {securityServiceType = a} :: PolicySummary)
{-# DEPRECATED psSecurityServiceType "Use generic-lens or generic-optics with 'securityServiceType' instead." #-}

instance Lude.FromJSON PolicySummary where
  parseJSON =
    Lude.withObject
      "PolicySummary"
      ( \x ->
          PolicySummary'
            Lude.<$> (x Lude..:? "PolicyName")
            Lude.<*> (x Lude..:? "RemediationEnabled")
            Lude.<*> (x Lude..:? "ResourceType")
            Lude.<*> (x Lude..:? "PolicyId")
            Lude.<*> (x Lude..:? "PolicyArn")
            Lude.<*> (x Lude..:? "SecurityServiceType")
      )
