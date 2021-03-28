{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.PolicySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.PolicySummary
  ( PolicySummary (..)
  -- * Smart constructor
  , mkPolicySummary
  -- * Lenses
  , psPolicyArn
  , psPolicyId
  , psPolicyName
  , psRemediationEnabled
  , psResourceType
  , psSecurityServiceType
  ) where

import qualified Network.AWS.FMS.Types.PolicyArn as Types
import qualified Network.AWS.FMS.Types.PolicyId as Types
import qualified Network.AWS.FMS.Types.ResourceName as Types
import qualified Network.AWS.FMS.Types.ResourceType as Types
import qualified Network.AWS.FMS.Types.SecurityServiceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details of the AWS Firewall Manager policy. 
--
-- /See:/ 'mkPolicySummary' smart constructor.
data PolicySummary = PolicySummary'
  { policyArn :: Core.Maybe Types.PolicyArn
    -- ^ The Amazon Resource Name (ARN) of the specified policy.
  , policyId :: Core.Maybe Types.PolicyId
    -- ^ The ID of the specified policy.
  , policyName :: Core.Maybe Types.ResourceName
    -- ^ The name of the specified policy.
  , remediationEnabled :: Core.Maybe Core.Bool
    -- ^ Indicates if the policy should be automatically applied to new resources.
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The type of resource protected by or in scope of the policy. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . For AWS WAF and Shield Advanced, examples include @AWS::ElasticLoadBalancingV2::LoadBalancer@ and @AWS::CloudFront::Distribution@ . For a security group common policy, valid values are @AWS::EC2::NetworkInterface@ and @AWS::EC2::Instance@ . For a security group content audit policy, valid values are @AWS::EC2::SecurityGroup@ , @AWS::EC2::NetworkInterface@ , and @AWS::EC2::Instance@ . For a security group usage audit policy, the value is @AWS::EC2::SecurityGroup@ . For an AWS Network Firewall policy, the value is @AWS::EC2::VPC@ .
  , securityServiceType :: Core.Maybe Types.SecurityServiceType
    -- ^ The service that the policy is using to protect the resources. This specifies the type of policy that is created, either an AWS WAF policy, a Shield Advanced policy, or a security group policy.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PolicySummary' value with any optional fields omitted.
mkPolicySummary
    :: PolicySummary
mkPolicySummary
  = PolicySummary'{policyArn = Core.Nothing, policyId = Core.Nothing,
                   policyName = Core.Nothing, remediationEnabled = Core.Nothing,
                   resourceType = Core.Nothing, securityServiceType = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the specified policy.
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPolicyArn :: Lens.Lens' PolicySummary (Core.Maybe Types.PolicyArn)
psPolicyArn = Lens.field @"policyArn"
{-# INLINEABLE psPolicyArn #-}
{-# DEPRECATED policyArn "Use generic-lens or generic-optics with 'policyArn' instead"  #-}

-- | The ID of the specified policy.
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPolicyId :: Lens.Lens' PolicySummary (Core.Maybe Types.PolicyId)
psPolicyId = Lens.field @"policyId"
{-# INLINEABLE psPolicyId #-}
{-# DEPRECATED policyId "Use generic-lens or generic-optics with 'policyId' instead"  #-}

-- | The name of the specified policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPolicyName :: Lens.Lens' PolicySummary (Core.Maybe Types.ResourceName)
psPolicyName = Lens.field @"policyName"
{-# INLINEABLE psPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

-- | Indicates if the policy should be automatically applied to new resources.
--
-- /Note:/ Consider using 'remediationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psRemediationEnabled :: Lens.Lens' PolicySummary (Core.Maybe Core.Bool)
psRemediationEnabled = Lens.field @"remediationEnabled"
{-# INLINEABLE psRemediationEnabled #-}
{-# DEPRECATED remediationEnabled "Use generic-lens or generic-optics with 'remediationEnabled' instead"  #-}

-- | The type of resource protected by or in scope of the policy. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . For AWS WAF and Shield Advanced, examples include @AWS::ElasticLoadBalancingV2::LoadBalancer@ and @AWS::CloudFront::Distribution@ . For a security group common policy, valid values are @AWS::EC2::NetworkInterface@ and @AWS::EC2::Instance@ . For a security group content audit policy, valid values are @AWS::EC2::SecurityGroup@ , @AWS::EC2::NetworkInterface@ , and @AWS::EC2::Instance@ . For a security group usage audit policy, the value is @AWS::EC2::SecurityGroup@ . For an AWS Network Firewall policy, the value is @AWS::EC2::VPC@ .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psResourceType :: Lens.Lens' PolicySummary (Core.Maybe Types.ResourceType)
psResourceType = Lens.field @"resourceType"
{-# INLINEABLE psResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The service that the policy is using to protect the resources. This specifies the type of policy that is created, either an AWS WAF policy, a Shield Advanced policy, or a security group policy.
--
-- /Note:/ Consider using 'securityServiceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psSecurityServiceType :: Lens.Lens' PolicySummary (Core.Maybe Types.SecurityServiceType)
psSecurityServiceType = Lens.field @"securityServiceType"
{-# INLINEABLE psSecurityServiceType #-}
{-# DEPRECATED securityServiceType "Use generic-lens or generic-optics with 'securityServiceType' instead"  #-}

instance Core.FromJSON PolicySummary where
        parseJSON
          = Core.withObject "PolicySummary" Core.$
              \ x ->
                PolicySummary' Core.<$>
                  (x Core..:? "PolicyArn") Core.<*> x Core..:? "PolicyId" Core.<*>
                    x Core..:? "PolicyName"
                    Core.<*> x Core..:? "RemediationEnabled"
                    Core.<*> x Core..:? "ResourceType"
                    Core.<*> x Core..:? "SecurityServiceType"
