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
    pRemediationEnabled,
    pResourceType,
    pExcludeResourceTags,
    pPolicyId,
    pResourceTypeList,
    pResourceTags,
    pPolicyUpdateToken,
    pExcludeMap,
    pIncludeMap,
    pSecurityServicePolicyData,
  )
where

import Network.AWS.FMS.Types.CustomerPolicyScopeIdType
import Network.AWS.FMS.Types.ResourceTag
import Network.AWS.FMS.Types.SecurityServicePolicyData
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An AWS Firewall Manager policy.
--
-- /See:/ 'mkPolicy' smart constructor.
data Policy = Policy'
  { -- | The name of the AWS Firewall Manager policy.
    policyName :: Lude.Text,
    -- | Indicates if the policy should be automatically applied to new resources.
    remediationEnabled :: Lude.Bool,
    -- | The type of resource protected by or in scope of the policy. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . For AWS WAF and Shield Advanced, examples include @AWS::ElasticLoadBalancingV2::LoadBalancer@ and @AWS::CloudFront::Distribution@ . For a security group common policy, valid values are @AWS::EC2::NetworkInterface@ and @AWS::EC2::Instance@ . For a security group content audit policy, valid values are @AWS::EC2::SecurityGroup@ , @AWS::EC2::NetworkInterface@ , and @AWS::EC2::Instance@ . For a security group usage audit policy, the value is @AWS::EC2::SecurityGroup@ . For an AWS Network Firewall policy, the value is @AWS::EC2::VPC@ .
    resourceType :: Lude.Text,
    -- | If set to @True@ , resources with the tags that are specified in the @ResourceTag@ array are not in scope of the policy. If set to @False@ , and the @ResourceTag@ array is not null, only resources with the specified tags are in scope of the policy.
    excludeResourceTags :: Lude.Bool,
    -- | The ID of the AWS Firewall Manager policy.
    policyId :: Lude.Maybe Lude.Text,
    -- | An array of @ResourceType@ .
    resourceTypeList :: Lude.Maybe [Lude.Text],
    -- | An array of @ResourceTag@ objects.
    resourceTags :: Lude.Maybe [ResourceTag],
    -- | A unique identifier for each update to the policy. When issuing a @PutPolicy@ request, the @PolicyUpdateToken@ in the request must match the @PolicyUpdateToken@ of the current policy version. To get the @PolicyUpdateToken@ of the current policy version, use a @GetPolicy@ request.
    policyUpdateToken :: Lude.Maybe Lude.Text,
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
    excludeMap :: Lude.Maybe (Lude.HashMap CustomerPolicyScopeIdType ([Lude.Text])),
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
    includeMap :: Lude.Maybe (Lude.HashMap CustomerPolicyScopeIdType ([Lude.Text])),
    -- | Details about the security service that is being used to protect the resources.
    securityServicePolicyData :: SecurityServicePolicyData
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Policy' with the minimum fields required to make a request.
--
-- * 'policyName' - The name of the AWS Firewall Manager policy.
-- * 'remediationEnabled' - Indicates if the policy should be automatically applied to new resources.
-- * 'resourceType' - The type of resource protected by or in scope of the policy. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . For AWS WAF and Shield Advanced, examples include @AWS::ElasticLoadBalancingV2::LoadBalancer@ and @AWS::CloudFront::Distribution@ . For a security group common policy, valid values are @AWS::EC2::NetworkInterface@ and @AWS::EC2::Instance@ . For a security group content audit policy, valid values are @AWS::EC2::SecurityGroup@ , @AWS::EC2::NetworkInterface@ , and @AWS::EC2::Instance@ . For a security group usage audit policy, the value is @AWS::EC2::SecurityGroup@ . For an AWS Network Firewall policy, the value is @AWS::EC2::VPC@ .
-- * 'excludeResourceTags' - If set to @True@ , resources with the tags that are specified in the @ResourceTag@ array are not in scope of the policy. If set to @False@ , and the @ResourceTag@ array is not null, only resources with the specified tags are in scope of the policy.
-- * 'policyId' - The ID of the AWS Firewall Manager policy.
-- * 'resourceTypeList' - An array of @ResourceType@ .
-- * 'resourceTags' - An array of @ResourceTag@ objects.
-- * 'policyUpdateToken' - A unique identifier for each update to the policy. When issuing a @PutPolicy@ request, the @PolicyUpdateToken@ in the request must match the @PolicyUpdateToken@ of the current policy version. To get the @PolicyUpdateToken@ of the current policy version, use a @GetPolicy@ request.
-- * 'excludeMap' - Specifies the AWS account IDs and AWS Organizations organizational units (OUs) to exclude from the policy. Specifying an OU is the equivalent of specifying all accounts in the OU and in any of its child OUs, including any child OUs and accounts that are added at a later time.
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
-- * 'includeMap' - Specifies the AWS account IDs and AWS Organizations organizational units (OUs) to include in the policy. Specifying an OU is the equivalent of specifying all accounts in the OU and in any of its child OUs, including any child OUs and accounts that are added at a later time.
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
-- * 'securityServicePolicyData' - Details about the security service that is being used to protect the resources.
mkPolicy ::
  -- | 'policyName'
  Lude.Text ->
  -- | 'remediationEnabled'
  Lude.Bool ->
  -- | 'resourceType'
  Lude.Text ->
  -- | 'excludeResourceTags'
  Lude.Bool ->
  -- | 'securityServicePolicyData'
  SecurityServicePolicyData ->
  Policy
mkPolicy
  pPolicyName_
  pRemediationEnabled_
  pResourceType_
  pExcludeResourceTags_
  pSecurityServicePolicyData_ =
    Policy'
      { policyName = pPolicyName_,
        remediationEnabled = pRemediationEnabled_,
        resourceType = pResourceType_,
        excludeResourceTags = pExcludeResourceTags_,
        policyId = Lude.Nothing,
        resourceTypeList = Lude.Nothing,
        resourceTags = Lude.Nothing,
        policyUpdateToken = Lude.Nothing,
        excludeMap = Lude.Nothing,
        includeMap = Lude.Nothing,
        securityServicePolicyData = pSecurityServicePolicyData_
      }

-- | The name of the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPolicyName :: Lens.Lens' Policy Lude.Text
pPolicyName = Lens.lens (policyName :: Policy -> Lude.Text) (\s a -> s {policyName = a} :: Policy)
{-# DEPRECATED pPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | Indicates if the policy should be automatically applied to new resources.
--
-- /Note:/ Consider using 'remediationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pRemediationEnabled :: Lens.Lens' Policy Lude.Bool
pRemediationEnabled = Lens.lens (remediationEnabled :: Policy -> Lude.Bool) (\s a -> s {remediationEnabled = a} :: Policy)
{-# DEPRECATED pRemediationEnabled "Use generic-lens or generic-optics with 'remediationEnabled' instead." #-}

-- | The type of resource protected by or in scope of the policy. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . For AWS WAF and Shield Advanced, examples include @AWS::ElasticLoadBalancingV2::LoadBalancer@ and @AWS::CloudFront::Distribution@ . For a security group common policy, valid values are @AWS::EC2::NetworkInterface@ and @AWS::EC2::Instance@ . For a security group content audit policy, valid values are @AWS::EC2::SecurityGroup@ , @AWS::EC2::NetworkInterface@ , and @AWS::EC2::Instance@ . For a security group usage audit policy, the value is @AWS::EC2::SecurityGroup@ . For an AWS Network Firewall policy, the value is @AWS::EC2::VPC@ .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pResourceType :: Lens.Lens' Policy Lude.Text
pResourceType = Lens.lens (resourceType :: Policy -> Lude.Text) (\s a -> s {resourceType = a} :: Policy)
{-# DEPRECATED pResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | If set to @True@ , resources with the tags that are specified in the @ResourceTag@ array are not in scope of the policy. If set to @False@ , and the @ResourceTag@ array is not null, only resources with the specified tags are in scope of the policy.
--
-- /Note:/ Consider using 'excludeResourceTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pExcludeResourceTags :: Lens.Lens' Policy Lude.Bool
pExcludeResourceTags = Lens.lens (excludeResourceTags :: Policy -> Lude.Bool) (\s a -> s {excludeResourceTags = a} :: Policy)
{-# DEPRECATED pExcludeResourceTags "Use generic-lens or generic-optics with 'excludeResourceTags' instead." #-}

-- | The ID of the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPolicyId :: Lens.Lens' Policy (Lude.Maybe Lude.Text)
pPolicyId = Lens.lens (policyId :: Policy -> Lude.Maybe Lude.Text) (\s a -> s {policyId = a} :: Policy)
{-# DEPRECATED pPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

-- | An array of @ResourceType@ .
--
-- /Note:/ Consider using 'resourceTypeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pResourceTypeList :: Lens.Lens' Policy (Lude.Maybe [Lude.Text])
pResourceTypeList = Lens.lens (resourceTypeList :: Policy -> Lude.Maybe [Lude.Text]) (\s a -> s {resourceTypeList = a} :: Policy)
{-# DEPRECATED pResourceTypeList "Use generic-lens or generic-optics with 'resourceTypeList' instead." #-}

-- | An array of @ResourceTag@ objects.
--
-- /Note:/ Consider using 'resourceTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pResourceTags :: Lens.Lens' Policy (Lude.Maybe [ResourceTag])
pResourceTags = Lens.lens (resourceTags :: Policy -> Lude.Maybe [ResourceTag]) (\s a -> s {resourceTags = a} :: Policy)
{-# DEPRECATED pResourceTags "Use generic-lens or generic-optics with 'resourceTags' instead." #-}

-- | A unique identifier for each update to the policy. When issuing a @PutPolicy@ request, the @PolicyUpdateToken@ in the request must match the @PolicyUpdateToken@ of the current policy version. To get the @PolicyUpdateToken@ of the current policy version, use a @GetPolicy@ request.
--
-- /Note:/ Consider using 'policyUpdateToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPolicyUpdateToken :: Lens.Lens' Policy (Lude.Maybe Lude.Text)
pPolicyUpdateToken = Lens.lens (policyUpdateToken :: Policy -> Lude.Maybe Lude.Text) (\s a -> s {policyUpdateToken = a} :: Policy)
{-# DEPRECATED pPolicyUpdateToken "Use generic-lens or generic-optics with 'policyUpdateToken' instead." #-}

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
pExcludeMap :: Lens.Lens' Policy (Lude.Maybe (Lude.HashMap CustomerPolicyScopeIdType ([Lude.Text])))
pExcludeMap = Lens.lens (excludeMap :: Policy -> Lude.Maybe (Lude.HashMap CustomerPolicyScopeIdType ([Lude.Text]))) (\s a -> s {excludeMap = a} :: Policy)
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
pIncludeMap :: Lens.Lens' Policy (Lude.Maybe (Lude.HashMap CustomerPolicyScopeIdType ([Lude.Text])))
pIncludeMap = Lens.lens (includeMap :: Policy -> Lude.Maybe (Lude.HashMap CustomerPolicyScopeIdType ([Lude.Text]))) (\s a -> s {includeMap = a} :: Policy)
{-# DEPRECATED pIncludeMap "Use generic-lens or generic-optics with 'includeMap' instead." #-}

-- | Details about the security service that is being used to protect the resources.
--
-- /Note:/ Consider using 'securityServicePolicyData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSecurityServicePolicyData :: Lens.Lens' Policy SecurityServicePolicyData
pSecurityServicePolicyData = Lens.lens (securityServicePolicyData :: Policy -> SecurityServicePolicyData) (\s a -> s {securityServicePolicyData = a} :: Policy)
{-# DEPRECATED pSecurityServicePolicyData "Use generic-lens or generic-optics with 'securityServicePolicyData' instead." #-}

instance Lude.FromJSON Policy where
  parseJSON =
    Lude.withObject
      "Policy"
      ( \x ->
          Policy'
            Lude.<$> (x Lude..: "PolicyName")
            Lude.<*> (x Lude..: "RemediationEnabled")
            Lude.<*> (x Lude..: "ResourceType")
            Lude.<*> (x Lude..: "ExcludeResourceTags")
            Lude.<*> (x Lude..:? "PolicyId")
            Lude.<*> (x Lude..:? "ResourceTypeList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ResourceTags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "PolicyUpdateToken")
            Lude.<*> (x Lude..:? "ExcludeMap" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "IncludeMap" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "SecurityServicePolicyData")
      )

instance Lude.ToJSON Policy where
  toJSON Policy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("PolicyName" Lude..= policyName),
            Lude.Just ("RemediationEnabled" Lude..= remediationEnabled),
            Lude.Just ("ResourceType" Lude..= resourceType),
            Lude.Just ("ExcludeResourceTags" Lude..= excludeResourceTags),
            ("PolicyId" Lude..=) Lude.<$> policyId,
            ("ResourceTypeList" Lude..=) Lude.<$> resourceTypeList,
            ("ResourceTags" Lude..=) Lude.<$> resourceTags,
            ("PolicyUpdateToken" Lude..=) Lude.<$> policyUpdateToken,
            ("ExcludeMap" Lude..=) Lude.<$> excludeMap,
            ("IncludeMap" Lude..=) Lude.<$> includeMap,
            Lude.Just
              ("SecurityServicePolicyData" Lude..= securityServicePolicyData)
          ]
      )
