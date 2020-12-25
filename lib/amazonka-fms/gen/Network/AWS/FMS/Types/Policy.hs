{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.Policy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.Policy
  ( Policy (..),

    -- * Smart constructor
    mkPolicy,

    -- * Lenses
    pPolicyName,
    pSecurityServicePolicyData,
    pResourceType,
    pExcludeResourceTags,
    pRemediationEnabled,
    pExcludeMap,
    pIncludeMap,
    pPolicyId,
    pPolicyUpdateToken,
    pResourceTags,
    pResourceTypeList,
  )
where

import qualified Network.AWS.FMS.Types.CustomerPolicyScopeId as Types
import qualified Network.AWS.FMS.Types.CustomerPolicyScopeIdType as Types
import qualified Network.AWS.FMS.Types.PolicyId as Types
import qualified Network.AWS.FMS.Types.PolicyUpdateToken as Types
import qualified Network.AWS.FMS.Types.ResourceName as Types
import qualified Network.AWS.FMS.Types.ResourceTag as Types
import qualified Network.AWS.FMS.Types.ResourceType as Types
import qualified Network.AWS.FMS.Types.SecurityServicePolicyData as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An AWS Firewall Manager policy.
--
-- /See:/ 'mkPolicy' smart constructor.
data Policy = Policy'
  { -- | The name of the AWS Firewall Manager policy.
    policyName :: Types.ResourceName,
    -- | Details about the security service that is being used to protect the resources.
    securityServicePolicyData :: Types.SecurityServicePolicyData,
    -- | The type of resource protected by or in scope of the policy. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . For AWS WAF and Shield Advanced, examples include @AWS::ElasticLoadBalancingV2::LoadBalancer@ and @AWS::CloudFront::Distribution@ . For a security group common policy, valid values are @AWS::EC2::NetworkInterface@ and @AWS::EC2::Instance@ . For a security group content audit policy, valid values are @AWS::EC2::SecurityGroup@ , @AWS::EC2::NetworkInterface@ , and @AWS::EC2::Instance@ . For a security group usage audit policy, the value is @AWS::EC2::SecurityGroup@ . For an AWS Network Firewall policy, the value is @AWS::EC2::VPC@ .
    resourceType :: Types.ResourceType,
    -- | If set to @True@ , resources with the tags that are specified in the @ResourceTag@ array are not in scope of the policy. If set to @False@ , and the @ResourceTag@ array is not null, only resources with the specified tags are in scope of the policy.
    excludeResourceTags :: Core.Bool,
    -- | Indicates if the policy should be automatically applied to new resources.
    remediationEnabled :: Core.Bool,
    -- | Specifies the AWS account IDs and AWS Organizations organizational units (OUs) to exclude from the policy. Specifying an OU is the equivalent of specifying all accounts in the OU and in any of its child OUs, including any child OUs and accounts that are added at a later time.
    --
    -- You can specify inclusions or exclusions, but not both. If you specify an @IncludeMap@ , AWS Firewall Manager applies the policy to all accounts specified by the @IncludeMap@ , and does not evaluate any @ExcludeMap@ specifications. If you do not specify an @IncludeMap@ , then Firewall Manager applies the policy to all accounts except for those specified by the @ExcludeMap@ .
    -- You can specify account IDs, OUs, or a combination:
    --
    --     * Specify account IDs by setting the key to @ACCOUNT@ . For example, the following is a valid map: @{“ACCOUNT” : [“accountID1”, “accountID2”]}@ .
    --
    --
    --     * Specify OUs by setting the key to @ORG_UNIT@ . For example, the following is a valid map: @{“ORG_UNIT” : [“ouid111”, “ouid112”]}@ .
    --
    --
    --     * Specify accounts and OUs together in a single map, separated with a comma. For example, the following is a valid map: @{“ACCOUNT” : [“accountID1”, “accountID2”], “ORG_UNIT” : [“ouid111”, “ouid112”]}@ .
    excludeMap :: Core.Maybe (Core.HashMap Types.CustomerPolicyScopeIdType [Types.CustomerPolicyScopeId]),
    -- | Specifies the AWS account IDs and AWS Organizations organizational units (OUs) to include in the policy. Specifying an OU is the equivalent of specifying all accounts in the OU and in any of its child OUs, including any child OUs and accounts that are added at a later time.
    --
    -- You can specify inclusions or exclusions, but not both. If you specify an @IncludeMap@ , AWS Firewall Manager applies the policy to all accounts specified by the @IncludeMap@ , and does not evaluate any @ExcludeMap@ specifications. If you do not specify an @IncludeMap@ , then Firewall Manager applies the policy to all accounts except for those specified by the @ExcludeMap@ .
    -- You can specify account IDs, OUs, or a combination:
    --
    --     * Specify account IDs by setting the key to @ACCOUNT@ . For example, the following is a valid map: @{“ACCOUNT” : [“accountID1”, “accountID2”]}@ .
    --
    --
    --     * Specify OUs by setting the key to @ORG_UNIT@ . For example, the following is a valid map: @{“ORG_UNIT” : [“ouid111”, “ouid112”]}@ .
    --
    --
    --     * Specify accounts and OUs together in a single map, separated with a comma. For example, the following is a valid map: @{“ACCOUNT” : [“accountID1”, “accountID2”], “ORG_UNIT” : [“ouid111”, “ouid112”]}@ .
    includeMap :: Core.Maybe (Core.HashMap Types.CustomerPolicyScopeIdType [Types.CustomerPolicyScopeId]),
    -- | The ID of the AWS Firewall Manager policy.
    policyId :: Core.Maybe Types.PolicyId,
    -- | A unique identifier for each update to the policy. When issuing a @PutPolicy@ request, the @PolicyUpdateToken@ in the request must match the @PolicyUpdateToken@ of the current policy version. To get the @PolicyUpdateToken@ of the current policy version, use a @GetPolicy@ request.
    policyUpdateToken :: Core.Maybe Types.PolicyUpdateToken,
    -- | An array of @ResourceTag@ objects.
    resourceTags :: Core.Maybe [Types.ResourceTag],
    -- | An array of @ResourceType@ .
    resourceTypeList :: Core.Maybe [Types.ResourceType]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Policy' value with any optional fields omitted.
mkPolicy ::
  -- | 'policyName'
  Types.ResourceName ->
  -- | 'securityServicePolicyData'
  Types.SecurityServicePolicyData ->
  -- | 'resourceType'
  Types.ResourceType ->
  -- | 'excludeResourceTags'
  Core.Bool ->
  -- | 'remediationEnabled'
  Core.Bool ->
  Policy
mkPolicy
  policyName
  securityServicePolicyData
  resourceType
  excludeResourceTags
  remediationEnabled =
    Policy'
      { policyName,
        securityServicePolicyData,
        resourceType,
        excludeResourceTags,
        remediationEnabled,
        excludeMap = Core.Nothing,
        includeMap = Core.Nothing,
        policyId = Core.Nothing,
        policyUpdateToken = Core.Nothing,
        resourceTags = Core.Nothing,
        resourceTypeList = Core.Nothing
      }

-- | The name of the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPolicyName :: Lens.Lens' Policy Types.ResourceName
pPolicyName = Lens.field @"policyName"
{-# DEPRECATED pPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | Details about the security service that is being used to protect the resources.
--
-- /Note:/ Consider using 'securityServicePolicyData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSecurityServicePolicyData :: Lens.Lens' Policy Types.SecurityServicePolicyData
pSecurityServicePolicyData = Lens.field @"securityServicePolicyData"
{-# DEPRECATED pSecurityServicePolicyData "Use generic-lens or generic-optics with 'securityServicePolicyData' instead." #-}

-- | The type of resource protected by or in scope of the policy. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . For AWS WAF and Shield Advanced, examples include @AWS::ElasticLoadBalancingV2::LoadBalancer@ and @AWS::CloudFront::Distribution@ . For a security group common policy, valid values are @AWS::EC2::NetworkInterface@ and @AWS::EC2::Instance@ . For a security group content audit policy, valid values are @AWS::EC2::SecurityGroup@ , @AWS::EC2::NetworkInterface@ , and @AWS::EC2::Instance@ . For a security group usage audit policy, the value is @AWS::EC2::SecurityGroup@ . For an AWS Network Firewall policy, the value is @AWS::EC2::VPC@ .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pResourceType :: Lens.Lens' Policy Types.ResourceType
pResourceType = Lens.field @"resourceType"
{-# DEPRECATED pResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | If set to @True@ , resources with the tags that are specified in the @ResourceTag@ array are not in scope of the policy. If set to @False@ , and the @ResourceTag@ array is not null, only resources with the specified tags are in scope of the policy.
--
-- /Note:/ Consider using 'excludeResourceTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pExcludeResourceTags :: Lens.Lens' Policy Core.Bool
pExcludeResourceTags = Lens.field @"excludeResourceTags"
{-# DEPRECATED pExcludeResourceTags "Use generic-lens or generic-optics with 'excludeResourceTags' instead." #-}

-- | Indicates if the policy should be automatically applied to new resources.
--
-- /Note:/ Consider using 'remediationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pRemediationEnabled :: Lens.Lens' Policy Core.Bool
pRemediationEnabled = Lens.field @"remediationEnabled"
{-# DEPRECATED pRemediationEnabled "Use generic-lens or generic-optics with 'remediationEnabled' instead." #-}

-- | Specifies the AWS account IDs and AWS Organizations organizational units (OUs) to exclude from the policy. Specifying an OU is the equivalent of specifying all accounts in the OU and in any of its child OUs, including any child OUs and accounts that are added at a later time.
--
-- You can specify inclusions or exclusions, but not both. If you specify an @IncludeMap@ , AWS Firewall Manager applies the policy to all accounts specified by the @IncludeMap@ , and does not evaluate any @ExcludeMap@ specifications. If you do not specify an @IncludeMap@ , then Firewall Manager applies the policy to all accounts except for those specified by the @ExcludeMap@ .
-- You can specify account IDs, OUs, or a combination:
--
--     * Specify account IDs by setting the key to @ACCOUNT@ . For example, the following is a valid map: @{“ACCOUNT” : [“accountID1”, “accountID2”]}@ .
--
--
--     * Specify OUs by setting the key to @ORG_UNIT@ . For example, the following is a valid map: @{“ORG_UNIT” : [“ouid111”, “ouid112”]}@ .
--
--
--     * Specify accounts and OUs together in a single map, separated with a comma. For example, the following is a valid map: @{“ACCOUNT” : [“accountID1”, “accountID2”], “ORG_UNIT” : [“ouid111”, “ouid112”]}@ .
--
--
--
-- /Note:/ Consider using 'excludeMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pExcludeMap :: Lens.Lens' Policy (Core.Maybe (Core.HashMap Types.CustomerPolicyScopeIdType [Types.CustomerPolicyScopeId]))
pExcludeMap = Lens.field @"excludeMap"
{-# DEPRECATED pExcludeMap "Use generic-lens or generic-optics with 'excludeMap' instead." #-}

-- | Specifies the AWS account IDs and AWS Organizations organizational units (OUs) to include in the policy. Specifying an OU is the equivalent of specifying all accounts in the OU and in any of its child OUs, including any child OUs and accounts that are added at a later time.
--
-- You can specify inclusions or exclusions, but not both. If you specify an @IncludeMap@ , AWS Firewall Manager applies the policy to all accounts specified by the @IncludeMap@ , and does not evaluate any @ExcludeMap@ specifications. If you do not specify an @IncludeMap@ , then Firewall Manager applies the policy to all accounts except for those specified by the @ExcludeMap@ .
-- You can specify account IDs, OUs, or a combination:
--
--     * Specify account IDs by setting the key to @ACCOUNT@ . For example, the following is a valid map: @{“ACCOUNT” : [“accountID1”, “accountID2”]}@ .
--
--
--     * Specify OUs by setting the key to @ORG_UNIT@ . For example, the following is a valid map: @{“ORG_UNIT” : [“ouid111”, “ouid112”]}@ .
--
--
--     * Specify accounts and OUs together in a single map, separated with a comma. For example, the following is a valid map: @{“ACCOUNT” : [“accountID1”, “accountID2”], “ORG_UNIT” : [“ouid111”, “ouid112”]}@ .
--
--
--
-- /Note:/ Consider using 'includeMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pIncludeMap :: Lens.Lens' Policy (Core.Maybe (Core.HashMap Types.CustomerPolicyScopeIdType [Types.CustomerPolicyScopeId]))
pIncludeMap = Lens.field @"includeMap"
{-# DEPRECATED pIncludeMap "Use generic-lens or generic-optics with 'includeMap' instead." #-}

-- | The ID of the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPolicyId :: Lens.Lens' Policy (Core.Maybe Types.PolicyId)
pPolicyId = Lens.field @"policyId"
{-# DEPRECATED pPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

-- | A unique identifier for each update to the policy. When issuing a @PutPolicy@ request, the @PolicyUpdateToken@ in the request must match the @PolicyUpdateToken@ of the current policy version. To get the @PolicyUpdateToken@ of the current policy version, use a @GetPolicy@ request.
--
-- /Note:/ Consider using 'policyUpdateToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPolicyUpdateToken :: Lens.Lens' Policy (Core.Maybe Types.PolicyUpdateToken)
pPolicyUpdateToken = Lens.field @"policyUpdateToken"
{-# DEPRECATED pPolicyUpdateToken "Use generic-lens or generic-optics with 'policyUpdateToken' instead." #-}

-- | An array of @ResourceTag@ objects.
--
-- /Note:/ Consider using 'resourceTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pResourceTags :: Lens.Lens' Policy (Core.Maybe [Types.ResourceTag])
pResourceTags = Lens.field @"resourceTags"
{-# DEPRECATED pResourceTags "Use generic-lens or generic-optics with 'resourceTags' instead." #-}

-- | An array of @ResourceType@ .
--
-- /Note:/ Consider using 'resourceTypeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pResourceTypeList :: Lens.Lens' Policy (Core.Maybe [Types.ResourceType])
pResourceTypeList = Lens.field @"resourceTypeList"
{-# DEPRECATED pResourceTypeList "Use generic-lens or generic-optics with 'resourceTypeList' instead." #-}

instance Core.FromJSON Policy where
  toJSON Policy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PolicyName" Core..= policyName),
            Core.Just
              ("SecurityServicePolicyData" Core..= securityServicePolicyData),
            Core.Just ("ResourceType" Core..= resourceType),
            Core.Just ("ExcludeResourceTags" Core..= excludeResourceTags),
            Core.Just ("RemediationEnabled" Core..= remediationEnabled),
            ("ExcludeMap" Core..=) Core.<$> excludeMap,
            ("IncludeMap" Core..=) Core.<$> includeMap,
            ("PolicyId" Core..=) Core.<$> policyId,
            ("PolicyUpdateToken" Core..=) Core.<$> policyUpdateToken,
            ("ResourceTags" Core..=) Core.<$> resourceTags,
            ("ResourceTypeList" Core..=) Core.<$> resourceTypeList
          ]
      )

instance Core.FromJSON Policy where
  parseJSON =
    Core.withObject "Policy" Core.$
      \x ->
        Policy'
          Core.<$> (x Core..: "PolicyName")
          Core.<*> (x Core..: "SecurityServicePolicyData")
          Core.<*> (x Core..: "ResourceType")
          Core.<*> (x Core..: "ExcludeResourceTags")
          Core.<*> (x Core..: "RemediationEnabled")
          Core.<*> (x Core..:? "ExcludeMap")
          Core.<*> (x Core..:? "IncludeMap")
          Core.<*> (x Core..:? "PolicyId")
          Core.<*> (x Core..:? "PolicyUpdateToken")
          Core.<*> (x Core..:? "ResourceTags")
          Core.<*> (x Core..:? "ResourceTypeList")
